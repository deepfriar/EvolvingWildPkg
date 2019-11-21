########################

## --------------------- ##
##   Scraper Functions   ##
## --------------------- ##

###########################

## --------------- ##
##   Scrape Data   ##
## --------------- ##

## Scrape Schedule
sc.scrape_schedule <- function(start_date = Sys.Date(), end_date = Sys.Date(), print_sched = TRUE) {

  ## getURL for schedule data
  url_schedule <- NULL
  try_count <- 3

  while (!is.character(url_schedule) & try_count > 0) {

    url_schedule <- try(
      getURL(
        paste0(
          "https://statsapi.web.nhl.com/api/v1/schedule?startDate=",
          as.character(start_date),
          "&endDate=",
          as.character(end_date)
        )
      )
    )

    try_count <- try_count - 1

  }

  ## Parse JSON data
  if (is.character(url_schedule)) {
    schedule_list <- jsonlite::fromJSON(url_schedule)

  }

  ## Return from function if scrape returned no data
  if (length(schedule_list$dates) == 0) {

    warning("NHL Schedule API Returned No Data")

    ## Return empty data.frame in same format if error occurred
    return(
      data.frame(
        game_id = character(),
        game_date = character(),
        season = character(),
        session = character(),
        game_status = character(),
        away_team = character(),
        home_team = character(),
        game_venue = character(),
        game_datetime = character(),
        EST_time_convert = character(),
        EST_date = character(),
        stringsAsFactors = FALSE
      )
    )

  }


  ## Bind games from schedule list
  bind_schedule <- foreach(i = 1:length(schedule_list$dates$games), .combine = rbind) %do% {

    schedule_current <- data.frame(
      game_id =       as.character(schedule_list$dates$games[[i]]$gamePk),
      game_date =     as.Date(schedule_list$dates$games[[i]]$gameDate),
      season =        schedule_list$dates$games[[i]]$season,
      session =       schedule_list$dates$games[[i]]$gameType,
      game_status =   schedule_list$dates$games[[i]]$status$detailedState,
      away_team_id =  schedule_list$dates$games[[i]]$teams$away$team$id,
      home_team_id =  schedule_list$dates$games[[i]]$teams$home$team$id,
      game_venue =    schedule_list$dates$games[[i]]$venue$name,
      game_datetime = schedule_list$dates$games[[i]]$gameDate,

      stringsAsFactors = FALSE
    )

  }


  ## Modify bound schedule data
  schedule_current <- bind_schedule %>%
    arrange(game_id) %>%
    filter(session != "PR") %>%   ## filter out preseason games
    dplyr::mutate(
      home_team_id = Team_ID$Team[match(home_team_id, Team_ID$ID)],
      away_team_id = Team_ID$Team[match(away_team_id, Team_ID$ID)],

      EST_time_convert = format(
        as.POSIXct(gsub("T", " ", game_datetime) %>% gsub("Z", "", .),
                   tz = "UTC",
                   format = "%Y-%m-%d %H:%M:%S"),
        tz = "Canada/Eastern"
      ),

      EST_date = as.Date(
        ifelse(is.na(EST_time_convert), as.Date(game_datetime) - 1, EST_time_convert),
        origin = "1970-01-01"
      ),

      game_date = EST_date
    ) %>%
    arrange(game_id) %>%
    rename(home_team = home_team_id,
           away_team = away_team_id
    ) %>%
    data.frame()

  ## Arrange if playoff games
  if ("P" %in% unique(schedule_current$session)) {
    schedule_current <- arrange(schedule_current, game_date, EST_time_convert)

  }

  ## print schedule
  if (print_sched == TRUE) print(head(schedule_current, 20))

  ## return schedule
  return(schedule_current)

}

## Scrape Events (HTM)
sc.scrape_events_HTM <- function(game_id_fun, season_id_fun, attempts = 3) {

  url_events_HTM <- NULL
  try_count <-  attempts

  while (!is.character(url_events_HTM) & try_count > 0) {

    url_events_HTM <- try(
      getURL(
        paste0(
          "http://www.nhl.com/scores/htmlreports/",
          as.character(season_id_fun),
          "/PL0",
          as.character(substr(game_id_fun, 6, 10)),
          ".HTM"
        )
      )
    )

    try_count <- try_count - 1

  }

  ## Pull out events data
  events_body_text <- rvest::html_text(rvest::html_nodes(xml2::read_html(url_events_HTM), ".bborder"))

}

## Scrape Events (API)
sc.scrape_events_API <- function(game_id_fun, attempts = 3) {

  url_events_API <- NULL
  try_count <-  attempts

  while (!is.character(url_events_API) & try_count > 0) {

    url_events_API <- try(
      getURL(
        paste0(
          "https://statsapi.web.nhl.com/api/v1/game/",
          game_id_fun,
          "/feed/live?site=en_nhl"
        )
      )
    )

    try_count <- try_count - 1

  }


  ## Parse JSON // Error handling
  if (is.character(url_events_API)) {
    events_list_API <- try(jsonlite::fromJSON(url_events_API), silent = TRUE)
  } else {
    events_list_API <- list()
  }

  if (class(events_list_API) == "try-error") {
    events_list_API <- jsonlite::fromJSON(
      suppressWarnings(
        readLines(paste0("https://statsapi.web.nhl.com/api/v1/game/", game_id_fun, "/feed/live?site=en_nhl"))
      )
    )

  }

  if (class(events_list_API) != "list") {
    events_list_API <- list()

  }


  return(events_list_API)

}

## Scrape Events (ESPN)
sc.scrape_events_ESPN <- function(game_id_fun, season_id_fun, game_info_data, attempts = 3) {

  ## Scrape ESPN to locate game IDs for the specified date
  url_ESPN_page <- NULL
  try_count <- attempts

  while(class(url_ESPN_page) != "character" & try_count > 0) {

    url_ESPN_page <- try(
      getURL(
        .opts = curlOptions(
          referer = "www.espn.com",
          verbose = FALSE,
          followLocation = TRUE
        ),
        # url to scrape
        paste0(
          "http://www.espn.com/nhl/scoreboard?date=",
          gsub("-", "", as.character(game_info_data$game_date))
        )
      )
    )

    try_count <- try_count - 1

  }


  # ## Parse games from scraped ESPN day page
  # ESPN_game_ids <- as.character(unique(unlist(str_extract_all(url_ESPN_page, "gameId=[0-9]+")))) %>% gsub("gameId=", "", .)
  # ESPN_teams <-    toupper(gsub("team/_/name/|>|</div>", "", unique(unlist(str_extract_all(url_ESPN_page, "team/_/name/[a-zA-Z]+|>(Coyotes|Thrashers)</div>")))))


  ## Get game ids  *** New ESPN Scoreboard Page
  ESPN_game_ids <- as.character(unique(unlist(str_extract_all(url_ESPN_page, "boxscore/_/gameId/[0-9]+")))) %>% gsub("boxscore/_/gameId/", "", .)

  ## Get team ids
  ESPN_container <- rvest::html_text(rvest::html_node(read_html(url_ESPN_page), "#fittPageContainer"))
  ESPN_teams <- toupper(unlist(str_extract_all(ESPN_container, "[0-9A-Z]{4}[a-zA-Z\\s]+[(]+")) %>% gsub("123T|123OTT|123SOT", "", .) %>% gsub("[0-9]|[(]", "", .))

  ## Get partial team names data frame
  part_team_names <- full_team_names %>%
    group_by(Team, partTeam) %>%
    summarise() %>%
    data.frame()


  ## Check if ESPN URL returned game IDs
  if (length(ESPN_game_ids) > 0) {

    ESPN_games_df <- suppressWarnings(cbind(
      ESPN_game_ids,
      matrix(unique(ESPN_teams), byrow = TRUE, ncol = 2)
    )) %>%
      data.frame(stringsAsFactors = FALSE) %>%
      select(
        game_id =   1,
        away_team = V2,
        home_team = V3
      ) %>%
      mutate_at(
        vars(home_team, away_team),
        funs(toupper(.))
      ) %>%
      mutate_at(
        vars(home_team, away_team),
        funs(part_team_names$Team[match(., part_team_names$partTeam)])
      ) %>%
      ## ensure duplicate games are not created
      group_by(game_id) %>%
      dplyr::mutate(index = row_number()) %>%
      filter(index == 1) %>%
      select(-index) %>%
      data.frame()


    ## Update Teams for Thrashers (WPG -> ATL, 20102011 season and earlier)
    ESPN_games_df <- ESPN_games_df %>%
      mutate_at(
        vars(away_team, home_team),
        funs(ifelse(. == "WPG" & game_info_data$season <= 20102011, "ATL", .))
      )


    ## Scrape individual game
    ESPN_game_id_ <- filter(ESPN_games_df, away_team == game_info_data$away_team, home_team == game_info_data$home_team)$game_id

    url_ESPN_game <- NULL
    try_count <- 3

    while(class(url_ESPN_game) != "character" & try_count > 0) {

      url_ESPN_game <- try(
        getURL(
          .opts = curlOptions(referer = "www.espn.com",
                              verbose = FALSE,
                              followLocation = TRUE),
          ## url to scrape
          paste0("http://www.espn.com/nhl/gamecast/data/masterFeed?lang=en&isAll=true&rand=0&gameId=", ESPN_game_id_)
        )
      )

      try_count <- try_count - 1

    }


    ## Parse xml data
    xml_ESPN_events <- url_ESPN_game %>%
      gsub("\023", "", .) %>%   ## prevent xml parse from erroring
      read_xml %>%
      xml_nodes("Plays") %>%
      xml_children() %>%
      xml_text() %>%
      strsplit("~") %>%
      do.call(rbind, .) %>%
      data.frame(stringsAsFactors = FALSE)


    return(xml_ESPN_events)

  } else {

    return(return_char <- "ESPN_NULL")

  }

}

## Scrape Shifts (HTM)
sc.scrape_shifts <- function(game_id_fun, season_id_fun, attempts = 3) {

  url_home_shifts <- NULL
  try_count <-  attempts

  while (!is.character(url_home_shifts) & try_count > 0) {

    url_home_shifts <- try(
      getURL(
        paste0(
          "http://www.nhl.com/scores/htmlreports/",
          season_id_fun,
          "/TH0",
          as.character(substr(game_id_fun, 6, 10)),
          ".HTM"
        )
      )
    )

    try_count <- try_count - 1

  }

  url_away_shifts <- NULL
  try_count <- attempts

  while (!is.character(url_away_shifts) & try_count > 0) {

    url_away_shifts <- try(
      getURL(
        paste0(
          "http://www.nhl.com/scores/htmlreports/",
          season_id_fun,
          "/TV0",
          as.character(substr(game_id_fun, 6, 10)),
          ".HTM"
        )
      )
    )

    try_count <- try_count - 1

  }

  ## Pull out scraped shifts data
  home_shifts_titles <- rvest::html_text(rvest::html_nodes(xml2::read_html(url_home_shifts), ".border"))
  away_shifts_titles <- rvest::html_text(rvest::html_nodes(xml2::read_html(url_away_shifts), ".border"))

  home_shifts_text <- rvest::html_text(rvest::html_nodes(xml2::read_html(url_home_shifts), ".bborder"))
  away_shifts_text <- rvest::html_text(rvest::html_nodes(xml2::read_html(url_away_shifts), ".bborder"))

  ## Return data as list
  return_list  <- list(home_shifts_titles = home_shifts_titles,
                       away_shifts_titles = away_shifts_titles,
                       home_shifts_text =   home_shifts_text,
                       away_shifts_text =   away_shifts_text)

}

## Scrape Shifts (API)
sc.scrape_shifts_API <- function(game_id_fun, attempts = 3) {

  url_shifts <- NULL
  try_count <-  attempts

  while (!is.character(url_shifts) & try_count > 0) {

    url_shifts <- try(
      getURL(
        paste0(
          "http://www.nhl.com/stats/rest/shiftcharts?cayenneExp=gameId=",
          game_id_fun
        )
      )
    )

    try_count <- try_count - 1

  }

  if (is.character(url_shifts)) {
    shifts_list <- jsonlite::fromJSON(url_shifts)
  } else {
    shifts_list <- list()
  }

  return(shifts_list)

}

## Scrape Rosters
sc.scrape_rosters <- function(game_id_fun, season_id_fun, attempts = 3) {

  url_rosters <- NULL
  try_count <- attempts

  while (is.null(url_rosters) & try_count > 0) {

    url_rosters <- try(
      getURL(
        paste0(
          "http://www.nhl.com/scores/htmlreports/",
          as.character(season_id_fun),
          "/RO0",
          as.character(substr(game_id_fun, 6, 10)),
          ".HTM"
        )
      )
    )

    try_count <- try_count - 1

  }

  ## Pull out roster data
  rosters_text <- rvest::html_text(rvest::html_nodes(xml2::read_html(url_rosters), "td"))

}

## Scrape Event Summary
sc.scrape_event_summary <- function(game_id_fun, season_id_fun, attempts = 3) {

  url_event_summary <- NULL
  try_count <- attempts

  while (is.null(url_event_summary) & try_count > 0) {

    url_event_summary <- try(
      getURL(
        paste0(
          "http://www.nhl.com/scores/htmlreports/",
          as.character(season_id_fun),
          "/ES0",
          as.character(substr(game_id_fun, 6, 10)),
          ".HTM"
        )
      )
    )

    try_count <- try_count - 1

  }

  ## Pull out roster data
  event_summary_text <- rvest::html_text(rvest::html_nodes(xml2::read_html(url_event_summary), "td"))

}
