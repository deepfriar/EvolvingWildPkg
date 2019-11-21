

## --------------------- ##
##   Compile Functions   ##
## --------------------- ##

## Run All Functions to Scrape Game Data
sc.scrape_game <- function(game_id, season_id, scrape_type_, live_scrape_) {

  ## --------------- ##
  ##   Scrape Data   ##
  ## --------------- ##

  ## Scrape events - HTM
  events_HTM <- sc.scrape_events_HTM(
    game_id_fun = game_id,
    season_id_fun = season_id,
    attempts = 3
  )

  ## Scrape shifts - HTM
  shifts_data_scrape <- sc.scrape_shifts(
    game_id_fun = game_id,
    season_id_fun = season_id,
    attempts = 3
  )

  ## Scrape events - API
  if (scrape_type_ == "full") {
    events_API <- sc.scrape_events_API(
      game_id_fun = game_id,
      attempts = 3
    )

  }

  ## Scrape rosters
  rosters_HTM <- sc.scrape_rosters(game_id_fun = game_id, season_id_fun = season_id, attempts = 3)

  ## Scrape Event Summary
  if (scrape_type_ %in% c("full", "event_summary")) {
    event_summary_HTM <- sc.scrape_event_summary(
      game_id_fun = game_id,
      season_id_fun = season_id,
      attempts = 3
    )

  }


  ## ---------------------------- ##
  ##   Create Basic Data Frames   ##
  ## ---------------------------- ##

  ## Create game information data frame
  game_info_df <- sc.game_info(
    game_id_fun = game_id,
    season_id_fun = season_id,
    events_data = events_HTM,
    roster_data = rosters_HTM
  )


  ## Scrape API for shifts data if HTM source fails
  if (length(shifts_data_scrape$home_shifts_text) == 0 | length(shifts_data_scrape$away_shifts_text) == 0) {
    shifts_data_scrape <- sc.shifts_process_API(
      game_id_fun = game_id,
      game_info_data = game_info_df
    )

    HTM_shifts_okay <- FALSE

  } else {
    HTM_shifts_okay <- TRUE

  }


  ## Create rosters data frame
  rosters_list <- sc.roster_info(
    game_id_fun = game_id,
    season_id_fun = season_id,
    roster_data = rosters_HTM,
    game_info_data = game_info_df,
    shifts_list = shifts_data_scrape
  )

  ## Create event summary data frame
  if (scrape_type_ %in% c("full", "event_summary")) {
    event_summary_df <- sc.event_summary(
      game_id_fun = game_id,
      season_id_fun = season_id,
      event_summary_data = event_summary_HTM,
      roster_data = rosters_list$roster_df,
      game_info_data = game_info_df
    )

  }

  if (scrape_type_ == "full") {

    ## ----------------------- ##
    ##   Prepare Events Data   ##
    ## ----------------------- ##

    ## Prepare Events Data (HTM)
    prepare_events_HTM_df <- sc.prepare_events_HTM(
      game_id_fun = game_id,
      season_id_fun = season_id,
      events_data = events_HTM,
      game_info_data = game_info_df
    )

    ## Prepare Events Data (API)
    if (!is.null(events_API$liveData$plays$allPlays$coordinates)) {

      if (ncol(events_API$liveData$plays$allPlays$coordinates) > 0) {
        prepare_events_API_df <- sc.prepare_events_API(
          game_id_fun = game_id,
          events_data_API = events_API,
          game_info_data = game_info_df
        )

        coord_type <- "NHL_API"

      } else {
        prepare_events_API_df <- NULL

      }

    } else {
      prepare_events_API_df <- NULL

    }


    ## ----------------------- ##
    ##   Prepare Shifts Data   ##
    ## ----------------------- ##

    ## Parse Shifts & Period Sums Data
    if (HTM_shifts_okay == TRUE) {
      shifts_parsed_list <- sc.shifts_parse(
        game_id_fun = game_id,
        season_id_fun = season_id,
        shifts_list = shifts_data_scrape,
        roster_data = rosters_list$roster_df,
        game_info_data = game_info_df,
        fix_shifts = isFALSE(live_scrape_)  ## flip live_scrape input
      )

    } else {
      shifts_parsed_list <- sc.shifts_parse_API(
        game_id_fun = game_id,
        shifts_list = shifts_data_scrape,
        roster_data = rosters_list$roster_df,
        game_info_data = game_info_df
      )

    }


    ## Fix Goalie Shifts & Finalize
    shifts_final_df <- sc.shifts_finalize(
      game_id_fun = game_id,
      shifts_parse_data = shifts_parsed_list$shifts_parse,
      events_data_HTM = prepare_events_HTM_df,
      game_info_data = game_info_df,
      fix_shifts = isFALSE(live_scrape_)  ## flip live_scrape input
    )

    ## Create ON/OFF Event Types
    shifts_event_types_df <- sc.shifts_create_events(
      shifts_final_data = shifts_final_df
    )


    ## -------------------- ##
    ##   Join Coordinates   ##
    ## -------------------- ##

    if (!is.null(prepare_events_API_df)) {  ## NHL API SOURCE
      events_full_df <- sc.join_coordinates_API(
        events_data_API = prepare_events_API_df,
        events_data_HTM = prepare_events_HTM_df
      )

    } else {                              ## ESPN XML SOURCE
      prepare_events_ESPN_df <- sc.scrape_events_ESPN(
        game_id_fun = game_id,
        season_id_fun = season_id,
        game_info_data = game_info_df,
        attempts = 3
      )

      if (class(prepare_events_ESPN_df) == "data.frame") {

        if (nrow(prepare_events_ESPN_df) > 0) {
          events_full_df <- sc.join_coordinates_ESPN(
            season_id_fun = season_id,
            events_data_ESPN = prepare_events_ESPN_df,
            events_data_HTM = prepare_events_HTM_df,
            roster_data = rosters_list$roster_df,
            game_info_data = game_info_df
          )

          coord_type <- "ESPN"

        } else {  ## COORDINATES NOT AVAILABLE
          events_full_df <- prepare_events_HTM_df %>%
            dplyr::mutate(
              coords_x = NA,
              coords_y = NA,
              event_description_alt = NA
            )

          coord_type <- "NO_COORDS"

        }

      } else {    ## COORDINATES NOT AVAILABLE
        events_full_df <- prepare_events_HTM_df %>%
          dplyr::mutate(
            coords_x = NA,
            coords_y = NA,
            event_description_alt = NA
          )

        coord_type <- "NO_COORDS"

      }

    }


    ## -------------------- ##
    ##   Combine All Data   ##
    ## -------------------- ##

    ## Combine / Process Shifts and Events Data
    pbp_combine_list <- sc.pbp_combine(
      events_data = events_full_df,
      shifts_data = shifts_event_types_df,
      roster_data = rosters_list$roster_df,
      game_info_data = game_info_df
    )

    ## Finalize PBP Data
    pbp_finalize_list <- sc.pbp_finalize(
      pbp_data =       pbp_combine_list$pbp_final,
      on_data_home =   pbp_combine_list$is_on_df_home,
      on_data_away =   pbp_combine_list$is_on_df_away,
      roster_data =    rosters_list$roster_df,
      game_info_data = game_info_df,
      live_scrape = live_scrape_
    )

    ## Modify game_info_df to return
    game_info_df_return <- game_info_df %>%
      dplyr::mutate(
        coord_source =  coord_type,
        period_count =  max(na.omit(pbp_finalize_list$pbp_base$game_period)),
        game_end =
          case_when(
            period_count <= 3                  ~ "REG",
            period_count == 4 & session == "R" ~ "OT",
            period_count <= 5 & session == "R" ~ "SO",
            period_count >= 4 & session == "P" ~ "OT"
          )
      ) %>%
      select(game_id:away_score, game_end, home_coach:linesman_2, coord_source) %>%
      data.frame()


    ## Ensure database friendly column names
    colnames(pbp_finalize_list$pbp_base) <-            tolower(colnames(pbp_finalize_list$pbp_base))
    colnames(pbp_finalize_list$pbp_extras) <-          tolower(colnames(pbp_finalize_list$pbp_extras))
    colnames(shifts_final_df) <-                       tolower(colnames(shifts_final_df))
    colnames(shifts_parsed_list$player_period_sums) <- tolower(colnames(shifts_parsed_list$player_period_sums))
    colnames(rosters_list$roster_df_final) <-          tolower(colnames(rosters_list$roster_df_final))
    colnames(rosters_list$scratches_df) <-             tolower(colnames(rosters_list$scratches_df))
    colnames(game_info_df_return) <-                   tolower(colnames(game_info_df_return))
    colnames(event_summary_df) <-                      tolower(colnames(event_summary_df))


    ## Return all data as a list
    return_list <- list(
      pbp_base =         pbp_finalize_list$pbp_base,
      pbp_extras =       pbp_finalize_list$pbp_extras,
      player_shifts =    shifts_final_df,
      player_periods =   shifts_parsed_list$player_period_sums,
      roster_df =        rosters_list$roster_df_final,
      scratches_df =     rosters_list$scratches_df,
      game_info_df =     game_info_df_return,
      event_summary_df = event_summary_df
    )

  }

  ## Return data is not full scrape
  if (scrape_type_ == "event_summary") {

    ## Ensure database friendly column names
    colnames(rosters_list$roster_df_final) <- tolower(colnames(rosters_list$roster_df_final))
    colnames(rosters_list$scratches_df) <-    tolower(colnames(rosters_list$scratches_df))
    colnames(event_summary_df) <-             tolower(colnames(event_summary_df))

    ## Return as list
    return_list <- list(
      roster_df =        rosters_list$roster_df_final,
      scratches_df =     rosters_list$scratches_df,
      event_summary_df = event_summary_df
    )

  } else if (scrape_type_ == "rosters") {

    ## Ensure database friendly column names
    colnames(rosters_list$roster_df_final) <- tolower(colnames(rosters_list$roster_df_final))
    colnames(rosters_list$scratches_df) <-    tolower(colnames(rosters_list$scratches_df))

    ## Return as list
    return_list <- list(
      roster_df =    rosters_list$roster_df_final,
      scratches_df = rosters_list$scratches_df
    )

  }


  ## Return data
  return(return_list)

}

## Run sc.scrape_game function in loop for multiple games
sc.scrape_pbp <- function(games, scrape_type = "full", live_scrape = FALSE, verbose = TRUE, sleep = 0) {

  ## ----------------------- ##
  ##   Prepare and Message   ##
  ## ----------------------- ##

  ## Check that scrape_type is correct
  if (!scrape_type %in% c("full", "event_summary", "rosters")) stop("'scrape_type' Entered is Invalid", call. = FALSE)

  ## Evaluate game_ids
  games_vec <- games

  ## Check if dead games were provided
  if (TRUE %in% (dead_games %in% games_vec)) {
    games_vec <- games_vec[!games_vec %in% dead_games]
    dead_games_removed <- 1

    message(paste0("Unable to scrape game(s) ", games[games %in% dead_games], " due to NHL source deficiencies", "\n"))

  } else {
    dead_games_removed <- 0

  }


  ## Find unique games and sort
  games_vec <- sort(unique(games_vec))

  if (length(games_vec) == 0) stop("No Valid Games Provided", call. = FALSE)


  ## Label games to be scraped
  if (length(games_vec) > 1) {
    cat(paste0("Processing ", length(games_vec), " Games: ", min(sort(as.numeric(games_vec))), "-", max(sort(as.numeric(games_vec)))))

    switch (
      scrape_type,
      "full" =          cat(" // Full Scrape"),
      "event_summary" = cat(" // Event Summary and Rosters"),
      "rosters" =       cat(" // Rosters Only")
    )

    cat(paste0("\n", "-------------", "\n"))

  } else {
    cat(paste0("Processing Game... "))

    switch (
      scrape_type,
      "full" =          cat(" // Full Scrape"),
      "event_summary" = cat(" // Event Summary and Rosters"),
      "rosters" =       cat(" // Rosters Only")
    )

    cat(paste0("\n", "-------------", "\n"))

  }

  if (length(games) != length(games_vec) & dead_games_removed != 1) message("Duplicate game IDs provided - combined for processing", "\n")


  ## Create report data frame
  scrape_report_df <- data.frame(matrix(ncol = 10))


  ## ------------------------ ##
  ##   Loop to Scrape Games   ##
  ## ------------------------ ##

  for(i in 1:length(games_vec)) {

    cat(paste0(games_vec[i], "...  "))

    start_time <- Sys.time()

    ## Try to scrape game(s)
    tryCatch({

      pbp_list <- sc.scrape_game(
        game_id =     games_vec[i],
        season_id =   paste0(as.numeric(substr(games_vec[i], 1, 4)), as.numeric(substr(games_vec[i], 1, 4)) + 1),
        scrape_type_ = scrape_type,
        live_scrape_ = live_scrape
      )

      if (scrape_type == "full") {
        hold_pbp_base <-       pbp_list$pbp_base
        hold_pbp_extras <-     pbp_list$pbp_extras
        hold_player_shifts <-  pbp_list$player_shifts
        hold_player_periods <- pbp_list$player_periods
        hold_game_info_df <-   pbp_list$game_info_df
      }

      if (scrape_type %in% c("event_summary", "full")) {
        hold_event_summary_df <- pbp_list$event_summary_df
      }

      hold_roster_df <-    pbp_list$roster_df
      hold_scratches_df <- pbp_list$scratches_df


      ## Bind data
      if (i == 1 | !exists("new_roster_df")) {

        if (scrape_type == "full") {
          new_pbp_base <-       pbp_list$pbp_base
          new_pbp_extras <-     pbp_list$pbp_extras
          new_player_shifts <-  pbp_list$player_shifts
          new_player_periods <- pbp_list$player_periods
          new_game_info_df <-   pbp_list$game_info_df
        }

        if (scrape_type %in% c("event_summary", "full")) {
          new_event_summary_df <- pbp_list$event_summary_df
        }

        new_roster_df <-    pbp_list$roster_df
        new_scratches_df <- pbp_list$scratches_df

      } else if (i > 1) {

        if (scrape_type == "full") {
          new_pbp_base <-       bind_rows(hold_pbp_base, new_pbp_base)
          new_pbp_extras <-     bind_rows(hold_pbp_extras, new_pbp_extras)
          new_player_shifts <-  bind_rows(hold_player_shifts, new_player_shifts)
          new_player_periods <- bind_rows(hold_player_periods, new_player_periods)
          new_game_info_df <-   bind_rows(hold_game_info_df, new_game_info_df)
        }

        if (scrape_type %in% c("event_summary", "full")) {
          new_event_summary_df <- bind_rows(hold_event_summary_df, new_event_summary_df)
        }

        new_roster_df <-    bind_rows(hold_roster_df, new_roster_df)
        new_scratches_df <- bind_rows(hold_scratches_df, new_scratches_df)

      }

    },
    error = function(e) cat("ERROR :", conditionMessage(e), "... ")
    )

    ## Scrape report data frame
    if (exists("hold_roster_df")) {

      if (games_vec[i] == unique(hold_roster_df$game_id)) {
        scrape_report_df[i, 1] <-  games_vec[i]
        scrape_report_df[i, 2] <-  na_if_null(nrow(pbp_list$pbp_base))
        scrape_report_df[i, 3] <-  na_if_null(nrow(pbp_list$pbp_extras))
        scrape_report_df[i, 4] <-  na_if_null(nrow(pbp_list$player_shifts))
        scrape_report_df[i, 5] <-  na_if_null(nrow(pbp_list$player_periods))
        scrape_report_df[i, 6] <-  na_if_null(nrow(pbp_list$roster_df))
        scrape_report_df[i, 7] <-  na_if_null(nrow(pbp_list$scratches_df))
        scrape_report_df[i, 8] <-  na_if_null(nrow(pbp_list$event_summary_df))
        scrape_report_df[i, 9] <-  na_if_null(nrow(pbp_list$game_info_df))
        scrape_report_df[i, 10] <- as.numeric(round(Sys.time() - start_time, 2))

      } else {
        scrape_report_df[i, 1] <-       games_vec[i]
        scrape_report_df[i, c(2, 9)] <- NA
        scrape_report_df[i, 10] <-      as.numeric(round(Sys.time() - start_time, 2))

      }

    } else {
      scrape_report_df[i, 1] <-       games_vec[i]
      scrape_report_df[i, c(2, 9)] <- NA
      scrape_report_df[i, 10] <-      as.numeric(round(Sys.time() - start_time, 1))

    }

    ## Print times if verbose
    if (verbose == TRUE) cat(paste0(round(scrape_report_df[i, 10], 1), " sec"), "\n") else cat("\n")

    ## Garbage collect every 200 games
    if (isTRUE(all.equal(i / 200, as.integer(i / 200), check.attributes = FALSE))) {
      invisible(gc())
    }

  }


  ## Add Names
  names(scrape_report_df) <- c("game_id", "pbp_base", "pbp_extras", "player_shifts", "player_periods",
                               "roster_df", "scratches_df", "events_summary_df", "game_info_df", "time_elapsed")

  ## Print results
  cat("-------------", "\n",
      paste0(
        length(unique(new_roster_df$game_id)), " of ", length(games_vec),
        " games returned // Avg Time Per Game: ",
        round(mean(scrape_report_df$time_elapsed), 1), "\n"
      )
  )


  ## Return List
  if (scrape_type == "full") {
    return_list <- list(
      game_info_df =      new_game_info_df %>% arrange(game_id),
      pbp_base =          new_pbp_base %>% arrange(game_id),
      pbp_extras =        new_pbp_extras %>% arrange(game_id),
      player_shifts =     new_player_shifts %>% arrange(game_id),
      player_periods =    new_player_periods %>% arrange(game_id),
      roster_df =         new_roster_df %>% arrange(game_id),
      scratches_df =      new_scratches_df %>% arrange(game_id),
      events_summary_df = new_event_summary_df %>% arrange(game_id),
      report =            scrape_report_df %>% arrange(game_id)
    )

  } else if (scrape_type == "event_summary") {
    return_list <- list(
      roster_df =         new_roster_df %>% arrange(game_id),
      scratches_df =      new_scratches_df %>% arrange(game_id),
      events_summary_df = new_event_summary_df %>% arrange(game_id),
      report =            scrape_report_df %>% arrange(game_id)
    )

  } else {
    return_list <- list(
      roster_df =    new_roster_df %>% arrange(game_id),
      scratches_df = new_scratches_df %>% arrange(game_id),
      report =       scrape_report_df %>% arrange(game_id)
    )

  }

  ## Return data
  return(return_list)

}


