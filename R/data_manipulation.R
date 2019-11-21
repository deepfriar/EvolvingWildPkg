## ---------------------------- ##
##   Create Basic Data Frames   ##
## ---------------------------- ##

## Create Game Information data.frame
sc.game_info <- function(game_id_fun, season_id_fun, events_data, roster_data) {

  ## Find coaches
  coach_index <- which(roster_data %in% c("Head Coaches", "Entraîneurs chef / Head Coaches")) + 1


  ## Find referees (french format / standard format)
  if (roster_data[grep("^Referee|^Arbitre/Referee", roster_data)    ] %in% c("Referee", "Arbitre/Referee") &
      roster_data[grep("^Referee|^Arbitre/Referee", roster_data) + 1] %in% c("Linesman", "JL/Linesman")) {

    referee_index <- grep("^Linesman|^JL/Linesman|^Referee:\\s*", roster_data) + 2

    ## fix one-off formatting issue
    if (roster_data[referee_index] == "\r\n") {
      referee_index <- referee_index + 4

    }

    referee_1 <-     na_if_null(toupper(gsub("#[0-9]*\\s", "", roster_data[referee_index    ]) %>% gsub("\\s", ".", .)))
    referee_2 <-     na_if_null(toupper(gsub("#[0-9]*\\s", "", roster_data[referee_index + 1]) %>% gsub("\\s", ".", .)))
    linesman_1 <-    na_if_null(toupper(gsub("#[0-9]*\\s", "", roster_data[referee_index + 3]) %>% gsub("\\s", ".", .)))
    linesman_2 <-    na_if_null(toupper(gsub("#[0-9]*\\s", "", roster_data[referee_index + 4]) %>% gsub("\\s", ".", .)))

  } else {
    referee_index <- grep("^Referee:", roster_data)
    referee_1 <-     na_if_null(toupper(gsub("#[0-9]*\\s", "", roster_data[referee_index + 2]) %>% gsub("\\s", ".", .)))
    referee_2 <-     na_if_null(toupper(gsub("#[0-9]*\\s", "", roster_data[referee_index + 3]) %>% gsub("\\s", ".", .)))
    linesman_1 <-    na_if_null(toupper(gsub("#[0-9]*\\s", "", roster_data[referee_index + 6]) %>% gsub("\\s", ".", .)))
    linesman_2 <-    na_if_null(toupper(gsub("#[0-9]*\\s", "", roster_data[referee_index + 7]) %>% gsub("\\s", ".", .)))

  }


  ## Find venue & attendance (french format / standard format)
  if (sum(grep("Les Formations", roster_data)) == 0) {
    venue_vec <-      first(roster_data[grep("^Attendance\\s*", roster_data)]) %>% gsub(".*at\\s*", "", .)
    attendance_vec <- first(roster_data[grep("^Attendance*", roster_data)]) %>% str_extract(., "[0-9]+,[0-9]+") %>% gsub(",", "", .) %>% as.numeric()

  } else {
    venue_vec <-      first(roster_data[grep("^Ass./Att.\\s*", roster_data)]) %>% gsub(".*@\\s*", "", .)
    attendance_vec <- first(roster_data[grep("^Ass./Att.\\s*", roster_data)]) %>% str_extract(., "[0-9]+,[0-9]+") %>% gsub(",", "", .) %>% as.numeric()

  }


  ## Create game info data frame
  game_info_df <- data.frame(
    game_id =         game_id_fun,
    season =          season_id_fun,
    game_date =       first(roster_data[grep("^[a-zA-Z]*, ", roster_data)]) %>% gsub("^[a-zA-Z]*, ", "", .) %>% as.Date(., format = "%B %d, %Y") %>% as.character(),
    # session =         ifelse(as.character(substr(game_id_fun, 6, 10)) > 30000, "P", "R"),
    session =
      case_when(
        as.character(substr(game_id_fun, 6, 10)) > 20000 & as.character(substr(game_id_fun, 6, 10)) < 30000  ~ "R",
        as.character(substr(game_id_fun, 6, 10)) > 30000 ~ "P",
        as.character(substr(game_id_fun, 6, 10)) < 20000 ~ "PS"
      ),
    game_time_start = first(na.omit(str_extract(roster_data, "[sS]tart\\s*[0-9]+:[0-9]+\\s*[A-Z]+"))) %>% gsub("[sS]tart\\s*", "", .),
    game_time_end =   first(na.omit(str_extract(roster_data, "[eE]nd\\s*[0-9]+:[0-9]+\\s*[A-Z]+"))) %>% gsub("[eE]nd\\s*", "", .),
    venue =           venue_vec,
    attendance =      attendance_vec,
    home_team =       events_data[8] %>% gsub(" On Ice", "", .),
    away_team =       events_data[7] %>% gsub(" On Ice", "", .),
    home_score =      na_if_null(roster_data[grep("^HOME\\b", roster_data) + 1] %>% gsub("\r|\n", "", .) %>% as.numeric()),
    away_score =      na_if_null(roster_data[grep("^VISITOR\\b", roster_data) + 1] %>% gsub("\r|\n", "", .) %>% as.numeric()),
    home_coach =      na_if_null(toupper(gsub("\\\r|\\\n", "", roster_data[coach_index + 2]) %>% gsub("\\s", ".", .))),
    away_coach =      na_if_null(toupper(gsub("\\\r|\\\n", "", roster_data[coach_index    ]) %>% gsub("\\s", ".", .))),
    referee_1 =       ifelse(as.character(substr(game_id_fun, 6, 10)) < 30000, referee_1, NA),   ##  not adding referees for playoff games ...
    referee_2 =       ifelse(as.character(substr(game_id_fun, 6, 10)) < 30000, referee_2, NA),   ##  additional refs present
    linesman_1 =      ifelse(as.character(substr(game_id_fun, 6, 10)) < 30000, linesman_1, NA),
    linesman_2 =      ifelse(as.character(substr(game_id_fun, 6, 10)) < 30000, linesman_2, NA),

    stringsAsFactors = FALSE
  ) %>%
    dplyr::mutate(
      home_team = ifelse(home_team == "PHX", "ARI", home_team),
      away_team = ifelse(away_team == "PHX", "ARI", away_team),

      ## Fix wrong game date in source
      game_date = ifelse(game_id == "2007020003", "2007-10-03", game_date),

      ## Fix missing data issues
      game_time_start = ifelse(game_id == "2009020874", "7:38 MST", game_time_start),
      game_time_end =   ifelse(game_id == "2009020874", "10:02 MST", game_time_end),
      venue =           ifelse(game_id == "2009020874", "Jobing.com Arena", venue),
      attendance =      ifelse(game_id == "2009020874", 13421, attendance),
      home_score =      ifelse(game_id == "2009020874", 6, home_score),
      away_score =      ifelse(game_id == "2009020874", 1, away_score)
    )

}

## Fix Player Names - HTM
sc.update_names_HTM <- function(data, col_name) {

  ## Handle input column name
  hold_name <- as.name(col_name)

  data <- data %>%
    dplyr::mutate(player_name = !!hold_name)


  ## Find and modify incorrect player names
  fixed_names_df <- data %>%
    dplyr::mutate(
      player_name =
        ## Global name changes
        case_when(
          grepl("^ALEXANDER.|^ALEXANDRE.", player_name) ~ gsub("^ALEXANDER.|^ALEXANDRE.", "ALEX.", player_name),
          grepl("^CHRISTOPHER.", player_name) ~ gsub("^CHRISTOPHER.", "CHRIS.", player_name),
          TRUE ~ player_name
        ),
      player_name =
        ## Specific name changes
        case_when(
          player_name == "ANDREI.KASTSITSYN" ~ "ANDREI.KOSTITSYN",
          player_name == "AJ.GREER" ~ "A.J..GREER",
          player_name == "ANDREW.GREENE" ~ "ANDY.GREENE",
          player_name == "ANDREW.WOZNIEWSKI" ~ "ANDY.WOZNIEWSKI",
          player_name == "ANTHONY.DEANGELO" ~ "TONY.DEANGELO",
          player_name == "BATES (JON).BATTAGLIA" ~ "BATES.BATTAGLIA",
          player_name %in% c("BJ.CROMBEEN", "B.J.CROMBEEN", "BRANDON.CROMBEEN") ~ "B.J..CROMBEEN",
          player_name == "BRADLEY.MILLS" ~ "BRAD.MILLS",
          player_name == "CAMERON.BARKER" ~ "CAM.BARKER",
          player_name == "COLIN (JOHN).WHITE" ~ "COLIN.WHITE",
          player_name == "CRISTOVAL.NIEVES" ~ "BOO.NIEVES",
          player_name == "CHRIS.VANDE VELDE" ~ "CHRIS.VANDEVELDE",
          player_name == "DANNY.BRIERE" ~ "DANIEL.BRIERE",
          player_name %in% c("DAN.CLEARY", "DANNY.CLEARY") ~ "DANIEL.CLEARY",
          player_name == "DANIEL.GIRARDI" ~ "DAN.GIRARDI",
          player_name == "DANNY.O'REGAN" ~ "DANIEL.O'REGAN",
          player_name == "DANIEL.CARCILLO" ~ "DAN.CARCILLO",
          player_name == "DAVID JOHNNY.ODUYA" ~ "JOHNNY.ODUYA",
          player_name == "DAVID.BOLLAND" ~ "DAVE.BOLLAND",
          player_name == "DENIS JR..GAUTHIER" ~ "DENIS.GAUTHIER",
          player_name == "DWAYNE.KING" ~ "DJ.KING",
          player_name == "EDWARD.PURCELL" ~ "TEDDY.PURCELL",
          player_name == "EMMANUEL.FERNANDEZ" ~ "MANNY.FERNANDEZ",
          player_name == "EMMANUEL.LEGACE" ~ "MANNY.LEGACE",
          player_name == "EVGENII.DADONOV" ~ "EVGENY.DADONOV",
          player_name == "FREDDY.MODIN" ~ "FREDRIK.MODIN",
          player_name == "FREDERICK.MEYER IV" ~ "FREDDY.MEYER",
          player_name == "HARRISON.ZOLNIERCZYK" ~ "HARRY.ZOLNIERCZYK",
          player_name == "ILJA.BRYZGALOV" ~ "ILYA.BRYZGALOV",
          player_name == "JACOB.DOWELL" ~ "JAKE.DOWELL",
          player_name == "JAMES.HOWARD" ~ "JIMMY.HOWARD",
          player_name == "JAMES.VANDERMEER" ~ "JIM.VANDERMEER",
          player_name == "JAMES.WYMAN" ~ "JT.WYMAN",
          player_name == "JOHN.HILLEN III" ~ "JACK.HILLEN",
          player_name == "JOHN.ODUYA" ~ "JOHNNY.ODUYA",
          player_name == "JOHN.PEVERLEY" ~ "RICH.PEVERLEY",
          player_name == "JONATHAN.SIM" ~ "JON.SIM",
          player_name == "JONATHON.KALINSKI" ~ "JON.KALINSKI",
          player_name == "JONATHAN.AUDY-MARCHESSAULT" ~ "JONATHAN.MARCHESSAULT",
          player_name == "JOSEPH.CRABB" ~ "JOEY.CRABB",
          player_name == "JOSEPH.CORVO" ~ "JOE.CORVO",
          player_name == "JOSHUA.BAILEY" ~ "JOSH.BAILEY",
          player_name == "JOSHUA.HENNESSY" ~ "JOSH.HENNESSY",
          player_name == "JOSHUA.MORRISSEY" ~ "JOSH.MORRISSEY",
          player_name == "JEAN-FRANCOIS.JACQUES" ~ "J-F.JACQUES",
          player_name %in% c("J P.DUMONT", "JEAN-PIERRE.DUMONT") ~ "J-P.DUMONT",
          player_name == "JT.COMPHER" ~ "J.T..COMPHER",
          player_name == "KRISTOPHER.LETANG" ~ "KRIS.LETANG",
          player_name == "KRYSTOFER.BARCH" ~ "KRYS.BARCH",
          player_name == "KRYSTOFER.KOLANOS" ~ "KRYS.KOLANOS",
          player_name == "MARC.POULIOT" ~ "MARC-ANTOINE.POULIOT",
          player_name == "MARTIN.ST LOUIS" ~ "MARTIN.ST. LOUIS",
          player_name == "MARTIN.ST PIERRE" ~ "MARTIN.ST. PIERRE",
          player_name == "MARTY.HAVLAT" ~ "MARTIN.HAVLAT",
          player_name == "MATTHEW.CARLE" ~ "MATT.CARLE",
          player_name == "MATHEW.DUMBA" ~ "MATT.DUMBA",
          player_name == "MATTHEW.BENNING" ~ "MATT.BENNING",
          player_name == "MATTHEW.IRWIN" ~ "MATT.IRWIN",
          player_name == "MATTHEW.NIETO" ~ "MATT.NIETO",
          player_name == "MATTHEW.STAJAN" ~ "MATT.STAJAN",
          player_name == "MAXIM.MAYOROV" ~ "MAKSIM.MAYOROV",
          player_name == "MAXIME.TALBOT" ~ "MAX.TALBOT",
          player_name == "MAXWELL.REINHART" ~ "MAX.REINHART",
          player_name == "MICHAEL.BLUNDEN" ~ "MIKE.BLUNDEN",
          player_name == "MICHAËL.BOURNIVAL" ~ "MICHAEL.BOURNIVAL",
          player_name == "MICHAEL.CAMMALLERI" ~ "MIKE.CAMMALLERI",
          player_name == "MICHAEL.FERLAND" ~ "MICHEAL.FERLAND",
          player_name == "MICHAEL.GRIER" ~ "MIKE.GRIER",
          player_name == "MICHAEL.KNUBLE" ~ "MIKE.KNUBLE",
          player_name == "MICHAEL.KOMISAREK" ~ "MIKE.KOMISAREK",
          player_name == "MICHAEL.MATHESON" ~ "MIKE.MATHESON",
          player_name == "MICHAEL.MODANO" ~ "MIKE.MODANO",
          player_name == "MICHAEL.RUPP" ~ "MIKE.RUPP",
          player_name == "MICHAEL.SANTORELLI" ~ "MIKE.SANTORELLI",
          player_name == "MICHAEL.SILLINGER" ~ "MIKE.SILLINGER",
          player_name == "MITCHELL.MARNER" ~ "MITCH.MARNER",
          player_name == "NATHAN.GUENIN" ~ "NATE.GUENIN",
          player_name == "NICHOLAS.BOYNTON" ~ "NICK.BOYNTON",
          player_name == "NICHOLAS.DRAZENOVIC" ~ "NICK.DRAZENOVIC",
          player_name == "NICKLAS.BERGFORS" ~ "NICLAS.BERGFORS",
          player_name == "NICKLAS.GROSSMAN" ~ "NICKLAS.GROSSMANN",
          player_name == "NICOLAS.PETAN" ~ "NIC.PETAN",
          player_name == "NIKLAS.KRONVALL" ~ "NIKLAS.KRONWALL",
          player_name == "NIKOLAI.ANTROPOV" ~ "NIK.ANTROPOV",
          player_name == "NIKOLAI.KULEMIN" ~ "NIKOLAY.KULEMIN",
          player_name == "NIKOLAI.ZHERDEV" ~ "NIKOLAY.ZHERDEV",
          player_name == "OLIVIER.MAGNAN-GRENIER" ~ "OLIVIER.MAGNAN",
          player_name == "PAT.MAROON" ~ "PATRICK.MAROON",
          player_name %in% c("P. J..AXELSSON", "PER JOHAN.AXELSSON") ~ "P.J..AXELSSON",
          player_name %in% c("PK.SUBBAN", "P.K.SUBBAN") ~ "P.K..SUBBAN",
          player_name %in% c("PIERRE.PARENTEAU", "PIERRE-ALEX.PARENTEAU", "PIERRE-ALEXANDRE.PARENTEAU", "PA.PARENTEAU", "P.A.PARENTEAU", "P-A.PARENTEAU") ~ "P.A..PARENTEAU",
          player_name == "PHILIP.VARONE" ~ "PHIL.VARONE",
          player_name == "QUINTIN.HUGHES" ~ "QUINN.HUGHES",
          player_name == "RAYMOND.MACIAS" ~ "RAY.MACIAS",
          player_name == "RJ.UMBERGER" ~ "R.J..UMBERGER",
          player_name == "ROBERT.BLAKE" ~ "ROB.BLAKE",
          player_name == "ROBERT.EARL" ~ "ROBBIE.EARL",
          player_name == "ROBERT.HOLIK" ~ "BOBBY.HOLIK",
          player_name == "ROBERT.SCUDERI" ~ "ROB.SCUDERI",
          player_name == "RODNEY.PELLEY" ~ "ROD.PELLEY",
          player_name == "SIARHEI.KASTSITSYN" ~ "SERGEI.KOSTITSYN",
          player_name == "SIMEON.VARLAMOV" ~ "SEMYON.VARLAMOV",
          player_name == "STAFFAN.KRONVALL" ~ "STAFFAN.KRONWALL",
          player_name == "STEVEN.REINPRECHT" ~ "STEVE.REINPRECHT",
          player_name == "TJ.GALIARDI" ~ "T.J..GALIARDI",
          player_name == "TJ.HENSICK" ~ "T.J..HENSICK",
          player_name %in% c("TJ.OSHIE", "T.J.OSHIE") ~ "T.J..OSHIE",
          player_name == "TOBY.ENSTROM" ~ "TOBIAS.ENSTROM",
          player_name == "TOMMY.SESTITO" ~ "TOM.SESTITO",
          player_name == "VACLAV.PROSPAL" ~ "VINNY.PROSPAL",
          player_name == "VINCENT.HINOSTROZA" ~ "VINNIE.HINOSTROZA",
          player_name == "WILLIAM.THOMAS" ~ "BILL.THOMAS",
          player_name == "ZACHARY.ASTON-REESE" ~ "ZACH.ASTON-REESE",
          player_name == "ZACHARY.SANFORD" ~ "ZACH.SANFORD",
          player_name == "ZACHERY.STORTINI" ~ "ZACK.STORTINI",

          ## NEW CHANGES
          player_name == "MATTHEW.MURRAY" ~ "MATT.MURRAY",
          player_name == "J-SEBASTIEN.AUBIN" ~ "JEAN-SEBASTIEN.AUBIN",
          player_name %in% c("J.F..BERUBE", "JEAN-FRANCOIS.BERUBE") ~ "J-F.BERUBE",
          player_name == "JEFF.DROUIN-DESLAURIERS" ~ "JEFF.DESLAURIERS",
          player_name == "NICHOLAS.BAPTISTE" ~ "NICK.BAPTISTE",
          player_name == "OLAF.KOLZIG" ~ "OLIE.KOLZIG",
          player_name == "STEPHEN.VALIQUETTE" ~ "STEVE.VALIQUETTE",
          player_name == "THOMAS.MCCOLLUM" ~ "TOM.MCCOLLUM",
          player_name == "TIMOTHY JR..THOMAS" ~ "TIM.THOMAS",

          TRUE ~ player_name
        )
    ) %>%
    data.frame()

  ## Replace original column with newly created column
  fixed_names_df[[col_name]] <- fixed_names_df$player_name

  ## Return data
  return(
    fixed_names_df %>%
      select(-player_name)
  )

}

## Create Rosters data.frame
sc.roster_info <- function(game_id_fun, season_id_fun, roster_data, game_info_data, shifts_list) {

  ## Get counts of roster & scratched players
  roster_player_counts <- lapply(str_extract_all(roster_data[grep("^\r\n#\r\nPos\r\nName|^\r\n#\r\nPos\r\nNom/Name", roster_data)], "[0-9]+"), length)

  if (length(roster_player_counts) == 0) {  ## extract string for alternate formatting
    roster_player_counts <- lapply(str_extract_all(roster_data[grep("^\n#\nPos\nName", roster_data)], "[0-9]+"), length)

  }

  ## Find players in rosters html text
  roster_index <- which(roster_data  %in% c("Name", "Nom/Name")) + 1

  roster_players <- bind_rows(

    ## Away players
    bind_cols(
      player =   roster_data[seq(roster_index[1] + 2, roster_index[1] + ((roster_player_counts[[1]] - 1) * 3) + 2, by = 3)][1:(roster_player_counts[[1]])],
      number =   roster_data[seq(roster_index[1]    , roster_index[1] + ((roster_player_counts[[1]] - 1) * 3)    , by = 3)][1:(roster_player_counts[[1]])],
      position = roster_data[seq(roster_index[1] + 1, roster_index[1] + ((roster_player_counts[[1]] - 1) * 3) + 1, by = 3)][1:(roster_player_counts[[1]])]
    ) %>%
      dplyr::mutate(Team = game_info_data$away_team),

    ## Home players
    bind_cols(
      player =   roster_data[seq(roster_index[2] + 2, roster_index[2] + ((roster_player_counts[[2]] - 1) * 3) + 2, by = 3)][1:(roster_player_counts[[2]])],
      number =   roster_data[seq(roster_index[2]    , roster_index[2] + ((roster_player_counts[[2]] - 1) * 3)    , by = 3)][1:(roster_player_counts[[2]])],
      position = roster_data[seq(roster_index[2] + 1, roster_index[2] + ((roster_player_counts[[2]] - 1) * 3) + 1, by = 3)][1:(roster_player_counts[[2]])]
    ) %>%
      dplyr::mutate(Team = game_info_data$home_team)

  ) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    dplyr::mutate(
      player = gsub("\\s*\\([A-Z]\\)", "", player),
      player_team_num = paste0(Team, number)
    )


  ## Find scratches
  away_scratch_count <- try(roster_player_counts[[3]], silent = TRUE)  ## verify both teams provided scratches
  home_scratch_count <- try(roster_player_counts[[4]], silent = TRUE)  ## verify both teams provided scratches

  if (class(away_scratch_count) != "try-error" & class(home_scratch_count) != "try-error") {

    roster_scratches <- bind_rows(

      ## Away scratches
      bind_cols(
        player =   roster_data[seq(roster_index[3] + 2, roster_index[3] + ((away_scratch_count - 1) * 3) + 2, by = 3)][1:away_scratch_count],
        number =   roster_data[seq(roster_index[3]    , roster_index[3] + ((away_scratch_count - 1) * 3)    , by = 3)][1:away_scratch_count],
        position = roster_data[seq(roster_index[3] + 1, roster_index[3] + ((away_scratch_count - 1) * 3) + 1, by = 3)][1:away_scratch_count]
      ) %>%
        dplyr::mutate(Team = game_info_data$away_team,
                      venue = "Away"),

      ## Home scratches
      bind_cols(
        player =   roster_data[seq(roster_index[4] + 2, roster_index[4] + ((home_scratch_count - 1) * 3) + 2, by = 3)][1:home_scratch_count],
        number =   roster_data[seq(roster_index[4]    , roster_index[4] + ((home_scratch_count - 1) * 3)    , by = 3)][1:home_scratch_count],
        position = roster_data[seq(roster_index[4] + 1, roster_index[4] + ((home_scratch_count - 1) * 3) + 1, by = 3)][1:home_scratch_count]
      ) %>%
        dplyr::mutate(Team = game_info_data$home_team,
                      venue = "Home")

    ) %>%
      data.frame(stringsAsFactors = FALSE) %>%
      dplyr::mutate(
        player =          gsub("\\s*\\([A-Z]\\)", "", player),
        player =          gsub("\\s", ".", player),
        player_num =      as.numeric(number),
        player_team_num = paste0(Team, player_num),
        position_type =   ifelse(position == "G", "G",
                                 ifelse(position == "D", "D",
                                        "F")),
        game_id =         game_info_data$game_id,
        game_date =       game_info_data$game_date,
        season =          game_info_data$season,
        session =         game_info_data$session,
        Opponent =        ifelse(Team == game_info_data$home_team, game_info_data$away_team, game_info_data$home_team),
        is_home =         1 * (Team == game_info_data$home_team)
      ) %>%

      ## Name Corrections
      sc.update_names_HTM(., col_name = "player") %>%
      ## Manula Name Updates
      dplyr::mutate(
        player =
          case_when(
            player == "SEBASTIAN.AHO" & position == "D" ~ "SEBASTIAN.AHO2",  ## D, ID 8480222
            player == "ALEX.PICARD" & position == "L" ~ "ALEX.PICARD2",      ## L, ID 8471221
            player == "SEAN.COLLINS" & position == "C" ~ "SEAN.COLLINS2",    ## C, ID 8474744
            player == "COLIN.WHITE" & as.numeric(game_info_data$season) >= 20162017 ~ "COLIN.WHITE2",         ## C, ID 8478400
            player == "ERIK.GUSTAFSSON" & as.numeric(game_info_data$season) >= 20152016 ~ "ERIK.GUSTAFSSON2", ## D, ID 8476979 (CHI player)

            player == "ANDREW.MILLER" & season == "20072008" ~ "DREW.MILLER", ## DREW.MILLER 8470778 ID
            TRUE ~ player
          )
      ) %>%

      select(player, position, position_type, game_id, game_date, season, session, Team, Opponent, is_home, player_num, player_team_num) %>%
      arrange(is_home, player) %>%
      data.frame()

  } else {

    roster_scratches <-
      data.frame(
        player = character(),
        position = character(),
        position_type = character(),
        game_id = character(),
        game_date = character(),
        season = character(),
        session = character(),
        Team = character(),
        Opponent = character(),
        is_home = numeric(),
        player_num = numeric(),
        player_team_num = character(),
        stringsAsFactors = FALSE
      )

  }


  ## Prepare & construct roster data frame
  home_shifts_titles <- shifts_list$home_shifts_titles
  away_shifts_titles <- shifts_list$away_shifts_titles

  if (game_id_fun == 2007030174) {  ## Manually remove Ryan Smyth from opposing team (one-off fix)
    away_shifts_titles <- away_shifts_titles[which(!(away_shifts_titles %in% "94 SMYTH, RYAN"))]

  }

  roster_df <- bind_rows(
    data.frame(
      # team_name =      shifts_list$home_shifts_titles[1],
      team_name =      home_shifts_titles[1],
      Team =           game_info_data$home_team,
      venue =          "Home",
      # num_last_first = shifts_list$home_shifts_titles[-1],
      num_last_first = home_shifts_titles[-1],
      stringsAsFactors = FALSE
    ),
    data.frame(
      # team_name =      shifts_list$away_shifts_titles[1],
      team_name =      away_shifts_titles[1],
      Team =           game_info_data$away_team,
      venue =          "Away",
      # num_last_first = shifts_list$away_shifts_titles[-1],
      num_last_first = away_shifts_titles[-1],
      stringsAsFactors = FALSE
    )
  ) %>%
    data.frame() %>%
    filter(grepl("[A-Z0-9]", num_last_first)) %>%  ## ensure string starts with player number
    dplyr::mutate(
      game_date =       game_info_data$game_date,
      game_id =         as.character(game_id_fun),
      season =          as.character(season_id_fun),
      session =         game_info_data$session,
      player_num =      parse_number(num_last_first),
      player_team_num = paste0(Team, player_num)
    ) %>%
    group_by(player_team_num) %>%
    dplyr::mutate(
      firstName = strsplit(gsub("^[0-9]+ ", "", num_last_first), ", ")[[1]][2],
      lastName =  strsplit(gsub("^[0-9]+ ", "", num_last_first), ", ")[[1]][1],
      player =    paste0(firstName, ".", lastName)
    ) %>%
    ungroup() %>%
    left_join(
      roster_players %>% select(player_team_num, position),
      by = "player_team_num"
    ) %>%
    dplyr::mutate(
      position_type = ifelse(position == "G", "G", ifelse(position == "D", "D", "F"))
    ) %>%

    ## Name Corrections
    sc.update_names_HTM(., col_name = "player") %>%
    ## Manual Name Corrections
    dplyr::mutate(
      player =
        case_when(
          player == "SEBASTIAN.AHO" & position == "D" ~ "SEBASTIAN.AHO2",  ## D, ID 8480222
          player == "ALEX.PICARD" & position == "L" ~ "ALEX.PICARD2",      ## L, ID 8471221
          player == "SEAN.COLLINS" & position == "C" ~ "SEAN.COLLINS2",    ## C, ID 8474744
          player == "COLIN.WHITE" & as.numeric(game_info_data$season) >= 20162017 ~ "COLIN.WHITE2",         ## C, ID 8478400
          player == "ERIK.GUSTAFSSON" & as.numeric(game_info_data$season) >= 20152016 ~ "ERIK.GUSTAFSSON2", ## D, ID 8476979 (CHI player)

          player == "ANDREW.MILLER" & season == "20072008" ~ "DREW.MILLER", ## DREW.MILLER 8470778 ID
          TRUE ~ player
        )
    ) %>%
    select(
      player, player_num, Team, game_id, game_date, season, session, num_last_first,
      player_team_num, firstName, lastName, venue, position_type, position
    ) %>%
    arrange(venue, player) %>%
    data.frame()


  ## Try getting player positions from the scratches data frame if NAs were returned
  if (sum(is.na(roster_df$position_type) + is.na(roster_df$position)) > 0 & nrow(roster_scratches) > 0) {
    roster_df_hold_NAs <- roster_df %>%
      filter(is.na(position_type) | is.na(position)) %>%
      select(-c(position, position_type)) %>%
      left_join(
        roster_scratches %>% select(player_team_num, position, position_type),
        by = "player_team_num"
      ) %>%
      data.frame()

    if (nrow(roster_df_hold_NAs) > 0) {

      if (!is.na(roster_df_hold_NAs$position) & !is.na(roster_df_hold_NAs$position_type)) {
        roster_df_correct <- bind_rows(
          roster_df %>% filter(!is.na(position_type), !is.na(position)),
          roster_df_hold_NAs
        ) %>%
          arrange(venue, player) %>%
          data.frame()

        ## Replace original roster_df data frame
        roster_df <- roster_df_correct

      }

    }

  }


  ## Roster data to be returned from final compile function
  roster_df_return <- roster_df %>%
    dplyr::mutate(
      Opponent = ifelse(Team == game_info_data$home_team, game_info_data$away_team, game_info_data$home_team),
      is_home =  1 * (Team == game_info_data$home_team)
    ) %>%
    select(
      player, num_last_first, player_team_num, position, position_type, game_id, game_date, season, session, Team, Opponent, is_home,
      player_num, firstName, lastName
    ) %>%
    data.frame()


  ## Return data as list
  return_list <- list(
    roster_df =       roster_df,
    roster_df_final = roster_df_return,
    scratches_df =    roster_scratches
  )

}

## Create Event Summary Data Frame
sc.event_summary <- function(game_id_fun, season_id_fun, event_summary_data, roster_data, game_info_data) {

  ## Find players in scraped html
  index <-     grep("^([A-Z]+|[A-Z]+.+[A-Z+]),\\s*[A-Z]+", event_summary_data) - 2
  index_sep <- grep("^TEAM TOTALS", event_summary_data)

  away_index <- index[index > 0 & index < index_sep[1]]
  home_index <- index[index > index_sep[1] & index < index_sep[2]]


  ## Loop to parse html
  event_summary_all <- bind_rows(

    ## Away players
    foreach(i = 1:length(away_index), .combine = bind_rows) %do% {

      event_summary_data[away_index[i]:(away_index[i] + 24)] %>%
        matrix(ncol = 25, byrow = TRUE) %>%
        data.frame(stringsAsFactors = FALSE)

    } %>%
      mutate_all(funs(gsub("\\b\\s\\b", 0, .))) %>%
      dplyr::mutate(
        Team =     game_info_data$away_team,
        Opponent = game_info_data$home_team,
        is_home =  0
      ),

    ## Home players
    foreach(i = 1:length(home_index), .combine = bind_rows) %do% {

      event_summary_data[home_index[i]:(home_index[i] + 24)] %>%
        matrix(ncol = 25, byrow = TRUE) %>%
        data.frame(stringsAsFactors = FALSE)

    } %>%
      mutate_all(funs(gsub("\\b\\s\\b", 0, .))) %>%
      dplyr::mutate(
        Team =     game_info_data$home_team,
        Opponent = game_info_data$away_team,
        is_home =  1
      )

  )

  ## Set column names
  colnames(event_summary_all) <- c(
    "player_num", "position", "player", "G", "A", "P", "P_M", "PEN", "PIM",
    "TOI_all", "SHF", "AVG", "TOI_PP", "TOI_SH", "TOI_EV",
    "S", "A_B", "MS", "HT", "GV", "TK", "BS", "FW", "FL", "FO_perc",
    "Team", "Opponent", "is_home"
  )


  ## Finalize
  event_summary_all <- event_summary_all %>%
    mutate_at(
      vars(G:PIM, SHF, S:FO_perc),
      funs(suppressWarnings(as.numeric(.)))   ## Warning: In evalq(as.numeric(G), <environment>) : NAs introduced by coercion
    ) %>%
    dplyr::mutate(
      player_team_num = paste0(Team, player_num),
      player =          roster_data$player[match(player_team_num, roster_data$player_team_num)],
      position_type =   ifelse(position == "D", "D",
                               ifelse(position == "G", "G",
                                      "F")),
      game_id =         game_id_fun,
      game_date =       game_info_data$game_date,
      season =          season_id_fun,
      session =         game_info_data$session
    ) %>%
    filter(!is.na(player)) %>%
    mutate_at(
      vars(TOI_all, AVG, TOI_PP, TOI_SH, TOI_EV),
      funs(suppressWarnings(round(period_to_seconds(ms(.)) / 60, 2)))
    ) %>%
    mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
    select(
      player, position, position_type, game_id, game_date, season, session, Team, Opponent, is_home,
      TOI_all, TOI_EV, TOI_PP, TOI_SH, SHF, AVG,
      G, A, P, P_M, PEN, PIM, S, A_B, MS, HT, GV, TK, BS, FW, FL, FO_perc
    ) %>%
    arrange(is_home, player) %>%
    data.frame()

}


## ----------------------- ##
##   Prepare Events Data   ##
## ----------------------- ##

## Prepare Events Data (HTM)
sc.prepare_events_HTM <- function(game_id_fun, season_id_fun, events_data, game_info_data) {

  ## Convert raw html to data frame
  pbp_HTM_events <- events_data %>%
    matrix(
      byrow = TRUE,
      ncol = 8
    ) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    select(
      eventIdx =          X1,
      game_period =       X2,
      strength =          X3,
      times =             X4,
      event_type =        X5,
      event_description = X6,
      away_skaters =      X7,
      home_skaters =      X8
    ) %>%
    filter(game_period != "Per") %>%
    dplyr::mutate(
      eventIdx =     as.numeric(eventIdx),
      game_id =      as.character(game_id_fun),
      game_period =  as.numeric(game_period),
      ## Extract shot types, times, penalty type, and challenge types
      event_detail =
        case_when(
          event_type %in% c("SHOT", "MISS", "BLOCK", "GOAL") ~ str_extract(event_description, ",\\s*[a-zA-Z|-]+,") %>% gsub(",|\\s", "", .),
          event_type %in% c("PSTR", "PEND", "SOC", "GEND") ~   str_extract(event_description, "[0-9]+:[0-9]+\\s*[A-Z]+"),
          event_type == "PENL" ~ str_extract(event_description, "[(][0-9]+\\s[a-z]*[)]") %>% gsub("[(]|[)]|\\s*", "", .),
          event_type == "CHL" ~  str_extract(event_description, "-[a-zA-Z\\s]+-") %>% gsub("\\s*-|-\\s*", "", .)
        ),
      ## Fix period & game end recording errors (rare)
      times =
        case_when(
          event_type == "PEND" & game_period == 4 & max(game_period) > 4 & game_info_data$session == "R" & times == "-16:0-120:00" ~ "5:000:00",
          event_type %in% c("PEND", "GEND") & game_period == 4 & max(game_period) == 4 & game_info_data$session == "R" & times == "-16:0-120:00" ~ last(na.omit(times)),
          TRUE ~ times
        ),
      time_elapsed = str_extract(times, "[0-9]+:[0-9]{2}"),
      game_seconds = suppressWarnings(period_to_seconds(ms(time_elapsed))),
      game_seconds =
        case_when(
          game_period == 2 ~ game_seconds + 1200,
          game_period == 3 ~ game_seconds + 2400,
          game_period == 4 ~ game_seconds + 3600,
          game_period == 5 & game_info_data$session == "R" ~ game_seconds + 3900,
          game_period == 5 & game_info_data$session == "P" ~ game_seconds + 4800,
          game_period == 6 & game_info_data$session == "P" ~ game_seconds + 6000,
          game_period == 7 & game_info_data$session == "P" ~ game_seconds + 7200,
          TRUE ~ game_seconds
        ),
      home_team =    game_info_data$home_team,
      away_team =    game_info_data$away_team
    ) %>%
    group_by(eventIdx) %>%
    dplyr::mutate(
      event_description = gsub("\\bPHX\\b", "ARI", event_description),   ## change PHX to ARI in event description

      event_team = ifelse(event_type %in% c(sc.main_events, "CHL", "DELPEN"), str_extract(event_description, "^[A-Z]\\.[A-Z]|^[A-Z]+"), NA),
      event_team = ifelse(!event_team %in% Team_ID_vec, NA, event_team),  ## ensure event_team extacted is an actual team

      event_player_1 =
        case_when(
          event_type %in% c("GOAL", "HIT", "MISS", "BLOCK", "FAC") ~
            str_extract(event_description, "([A-Z]+\\.[A-Z]+|[A-Z]+)\\s*#[0-9]+"),

          event_type %in% c("SHOT", "GIVE", "TAKE") ~
            gsub("ONGOAL\\s*-\\s*|GIVEAWAY\\s*-\\s*|TAKEAWAY\\s*-\\s*", "", event_description) %>% str_extract(., "([A-Z]+\\.[A-Z]+|[A-Z]+)\\s*(#[0-9]+|[0-9]+)"),

          event_type == "PENL" & !grepl("TEAM", event_description) ~                                       ## normal penalites
            str_extract(event_description, "([A-Z]+\\.[A-Z]+|[A-Z]+)\\s*#[0-9]+"),

          event_type == "PENL" & grepl("TEAM", event_description) & grepl("#[0-9]+", event_description) ~  ## bench minors (delay of game, faceoff violation, etc.)
            paste(event_team, str_extract(event_description, "#[0-9]+"))
        ),
      event_player_2 =
        case_when(
          event_type %in% c("BLOCK", "FAC", "HIT", "PENL") & length(str_extract_all(event_description, "#[0-9]+")[[1]]) > 1 ~  ## ensure a player is present
            str_extract_all(event_description, "([A-Z]+\\.[A-Z]+|[A-Z]+)\\s*#[0-9]+")[[1]][2],

          event_type == "GOAL" & length(str_extract_all(event_description, "#[0-9]+")[[1]]) > 1 ~  ## ensure a player is present
            paste(event_team, str_extract_all(event_description, "#[0-9]+")[[1]][2])
        ),
      event_player_3 =
        case_when(
          event_type == "GOAL" & length(str_extract_all(event_description, "#[0-9]+")[[1]]) > 2 ~  ## ensure a player is present
            paste(event_team, str_extract_all(event_description, "#[0-9]+")[[1]][3])
        ),

      event_zone = str_extract(event_description, "[a-zA-Z]{3}\\.\\s*[zZ]one") %>% gsub("\\.\\s*[zZ]one", "", .)
    ) %>%
    ungroup() %>%
    mutate_at(
      vars(event_player_1:event_player_3),
      funs(gsub("#|\\s*", "", .))
    ) %>%
    mutate_at(
      vars(event_player_1:event_player_3),  ## remove event players for true team/bench penalties
      funs(ifelse(grepl("[tT]oo\\s*many\\s*men|[A-Z]+\\s*[A-Z]+\\s*[bB]ench[(]", event_description), NA, .))
    ) %>%
    select(-c(home_skaters, away_skaters)) %>%
    data.frame()

}

## Prepare Events Data (API)
sc.prepare_events_API <- function(game_id_fun, events_data_API, game_info_data) {

  ## Parse API events data
  events_join_API_raw <-
    bind_cols(
      events_data_API$liveData$plays$allPlays$about %>%
        select(-goals),
      events_data_API$liveData$plays$allPlays$about$goals %>%
        rename(
          away.goals = away,
          home.goals = home
        )
    ) %>%
    dplyr::mutate(eventIdx = eventIdx + 1) %>%
    left_join(
      events_data_API$liveData$plays$allPlays$result %>%  ## removed separate parsing of "strength" endpoint
        dplyr::mutate(eventIdx = row_number()),
      by = "eventIdx"
    ) %>%
    left_join(
      events_data_API$liveData$plays$allPlays$coordinates %>%
        dplyr::mutate(eventIdx = row_number()),
      by = "eventIdx"
    ) %>%
    left_join(
      events_data_API$liveData$plays$allPlays$team %>%
        dplyr::mutate(
          triCode =  Team_ID$Team[match(id, Team_ID$ID)], ## use HTM source team triCodes
          eventIdx = row_number()
        ) %>%
        rename(teamName = name),
      by = "eventIdx"
    ) %>%
    select(
      eventIdx,
      game_period = period,
      time_elapsed = periodTime,
      event_type = eventTypeId,
      event_team = triCode,
      event_description = description,
      coords_x = x,
      coords_y = y
    ) %>%
    dplyr::mutate(
      game_seconds = suppressWarnings(period_to_seconds(ms(time_elapsed))),
      game_seconds =
        case_when(
          game_period == 2 ~ game_seconds + 1200,
          game_period == 3 ~ game_seconds + 2400,
          game_period == 4 ~ game_seconds + 3600,
          game_period == 5 & game_info_data$session == "R" ~ game_seconds + 3900,
          game_period == 5 & game_info_data$session == "P" ~ game_seconds + 4800,
          game_period == 6 & game_info_data$session == "P" ~ game_seconds + 6000,
          game_period == 7 & game_info_data$session == "P" ~ game_seconds + 7200,
          TRUE ~ game_seconds
        ),
      event_type = toupper(gsub("gamecenter", "", event_type)),  ## remove excessive "gamecenter" text and ensure capitalization
      event_type =
        case_when(
          ## Update API event type names
          event_type == "BLOCKEDSHOT" ~ "BLOCK",
          event_type == "BLOCKED_SHOT" ~ "BLOCK",
          event_type == "MISSEDSHOT" ~ "MISS",
          event_type == "MISSED_SHOT" ~ "MISS",
          event_type == "FACEOFF" ~ "FAC",
          event_type == "PENALTY" ~ "PENL",
          event_type == "GIVEAWAY" ~ "GIVE",
          event_type == "TAKEAWAY" ~ "TAKE",
          TRUE ~ event_type
        )
    ) %>%
    filter(event_type %in% c("TAKE", "GIVE", "MISS", "HIT", "SHOT", "BLOCK", "GOAL", "PENL", "FAC")) %>%
    data.frame()


  ## Parse Roster Data
  roster_data_API <- bind_rows(

    ## Home players
    foreach(i = 1:length(events_data_API$liveData$boxscore$teams$home$players), .combine = bind_rows) %do% {

      data.frame(
        player_ID = na_if_null(events_data_API$liveData$boxscore$teams$home$players[[i]]$person$id),
        fullName =  na_if_null(events_data_API$liveData$boxscore$teams$home$players[[i]]$person$fullName),
        Team =      game_info_data$home_team,
        number =    na_if_null(events_data_API$liveData$boxscore$teams$home$players[[i]]$jerseyNumber),
        stringsAsFactors = FALSE
      )

    } %>%
      data.frame(row.names = NULL),

    ## Away players
    foreach(i = 1:length(events_data_API$liveData$boxscore$teams$away$players), .combine = bind_rows) %do% {

      data.frame(
        player_ID = na_if_null(events_data_API$liveData$boxscore$teams$away$players[[i]]$person$id),
        fullName =  na_if_null(events_data_API$liveData$boxscore$teams$away$players[[i]]$person$fullName),
        Team =      game_info_data$away_team,
        number =    na_if_null(events_data_API$liveData$boxscore$teams$away$players[[i]]$jerseyNumber),
        stringsAsFactors = FALSE
      )

    } %>%
      data.frame(row.names = NULL)
  ) %>%
    dplyr::mutate(player_team_num = paste0(Team, number))


  ## Parse event players data
  events_players_list_API <- events_data_API$liveData$plays$allPlays$players

  events_players_raw_API <- foreach(i = 1:length(events_players_list_API), .combine = bind_rows) %do% {

    if (length(events_players_list_API[[i]]$player$fullName) > 0) {

      data.frame(
        eventIdx =       i,
        event_player_1 = as.character(events_players_list_API[[i]]$player$id[1]),
        event_player_2 = as.character(events_players_list_API[[i]]$player$id[2]),
        stringsAsFactors = FALSE
      )

    } else {

      data.frame(
        eventIdx =       i,
        event_player_1 = NA,
        event_player_2 = NA,
        stringsAsFactors = FALSE
      )

    }

  } %>%
    dplyr::mutate(
      event_player_1 = roster_data_API$player_team_num[match(event_player_1, roster_data_API$player_ID)],
      event_player_2 = roster_data_API$player_team_num[match(event_player_2, roster_data_API$player_ID)]
    ) %>%
    data.frame()


  ## Final Join
  events_join_API <- events_join_API_raw %>%
    left_join(events_players_raw_API, by = "eventIdx") %>%
    ## Correct event players and event team for blocked shots and faceoffs
    dplyr::mutate(
      hold_block_1 = ifelse(event_type == "BLOCK", event_player_2, NA),
      hold_block_2 = ifelse(event_type == "BLOCK", event_player_1, NA),
      hold_face_1 =
        case_when(
          event_type == "FAC" & event_team == game_info_data$away_team ~ event_player_1,
          event_type == "FAC" & event_team == game_info_data$home_team ~ event_player_2
        ),
      hold_face_2 =
        case_when(
          event_type == "FAC" & event_team == game_info_data$away_team ~ event_player_2,
          event_type == "FAC" & event_team == game_info_data$home_team ~ event_player_1
        ),
      event_player_1 = ifelse(event_type == "BLOCK", hold_block_1, event_player_1),
      event_player_2 = ifelse(event_type == "BLOCK", hold_block_2, event_player_2),
      event_player_1 = ifelse(event_type == "FAC", hold_face_1, event_player_1),
      event_player_2 = ifelse(event_type == "FAC", hold_face_2, event_player_2),

      event_team =     ifelse(event_type == "BLOCK",
                              ifelse(event_team == game_info_data$home_team, game_info_data$away_team, game_info_data$home_team),
                              event_team
      )
    ) %>%
    select(game_period, game_seconds, event_type, event_team, event_description, coords_x, coords_y, event_player_1) %>%
    data.frame()

}


## ----------------------- ##
##   Prepare Shifts Data   ##
## ----------------------- ##

## Parse Shifts & Period Sums Data
sc.shifts_parse <- function(game_id_fun, season_id_fun, shifts_list, roster_data, game_info_data, fix_shifts) {

  ## --------------------- ##
  ##   Main Shifts Parse   ##
  ## --------------------- ##

  ## Create player data frames utilized in loops
  player_home_df <- filter(roster_data, venue == "Home")
  player_away_df <- filter(roster_data, venue == "Away")


  ## Combine home and away shifts
  shifts_parse_full <- bind_rows(

    ## Home Shifts
    foreach(i = 1:nrow(player_home_df), .combine = bind_rows) %do% {

      index <- which(shifts_list$home_shifts_titles[-1] == player_home_df$num_last_first[i])

      shifts_list$home_shifts_text[which(shifts_list$home_shifts_text %in% c("Shift #", "Présence #Shift #"))[index]:(which(shifts_list$home_shifts_text %in% c("SHF", "PR/SHF"))[index] - 3)] %>%
        matrix(ncol = 6,
               byrow = TRUE
        ) %>%
        data.frame(stringsAsFactors = FALSE) %>%
        dplyr::mutate(player =         player_home_df$player[i],
                      num_last_first = player_home_df$num_last_first[i],
                      event_team =     game_info_data$home_team
        ) %>%
        filter(X2 != "Per") %>%
        data.frame()

    } %>%
      mutate_all(funs(ifelse(. == "", NA, .))) %>%
      select(7:9, 1:5),

    ## Away Shifts
    foreach(i = 1:nrow(player_away_df), .combine = bind_rows) %do% {

      index <- which(shifts_list$away_shifts_titles[-1] == player_away_df$num_last_first[i])

      shifts_list$away_shifts_text[which(shifts_list$away_shifts_text %in% c("Shift #", "Présence #Shift #"))[index]:(which(shifts_list$away_shifts_text %in% c("SHF", "PR/SHF"))[index] - 3)] %>%
        matrix(ncol = 6,
               byrow = TRUE
        ) %>%
        data.frame(stringsAsFactors = FALSE) %>%
        dplyr::mutate(player =         player_away_df$player[i],
                      num_last_first = player_away_df$num_last_first[i],
                      event_team =     game_info_data$away_team
        ) %>%
        filter(X2 != "Per") %>%
        data.frame()

    } %>%
      mutate_all(funs(ifelse(. == "", NA, .))) %>%
      select(7:9, 1:5)

  ) %>%
    left_join(
      roster_data %>% select(num_last_first, player_team_num, position_type, position),
      by = "num_last_first"
    ) %>%
    select(
      player, num_last_first, player_team_num, position, position_type,
      everything()
    ) %>%
    data.frame()

  ## Rename
  colnames(shifts_parse_full)[7:11] <- c("shift_num", "game_period", "shift_start", "shift_end", "duration")


  ## ------------------------------ ##
  ##   Prep Shifts Data - Initial   ##
  ## ------------------------------ ##

  if (fix_shifts == TRUE) {
    shifts_parse_full <- shifts_parse_full %>%
      group_by(player, num_last_first, event_team, shift_num, game_period) %>%
      dplyr::mutate(
        shift_end =
          suppressWarnings(case_when(
            ## Force end of period when shift end is before shift start (and start is within 3 minutes of period end)
            position != "G" & as.numeric(str_extract(duration, "^[0-9]+")) > 30 & as.numeric(str_extract(shift_start, "^[0-9]+")) >= 17 &
              (as.numeric(game_period) <= 3 | game_info_data$session == "P") ~
              "20:00 / 0:00",

            TRUE ~ shift_end
          )),
        shift_mod =
          ## Indicate if shift was modified
          suppressWarnings(ifelse(
            position != "G" & as.numeric(str_extract(duration, "^[0-9]+")) > 30 & as.numeric(str_extract(shift_start, "^[0-9]+")) >= 17 &
              (as.numeric(game_period) <= 3 | game_info_data$session == "P"),
            1, 0
          ))
      ) %>%
      ## Remove "incorrect" shifts (recording error resulting in additional "phantom" shift being created)
      ungroup() %>%
      dplyr::mutate(filtered = ifelse(shift_start == "20:00 / 0:00" & lag(shift_end) == "20:00 / 0:00", 1, 0)) %>%
      filter(filtered == 0) %>%
      select(-filtered) %>%
      data.frame()

  } else {
    shifts_parse_full <- shifts_parse_full %>%
      dplyr::mutate(shift_mod = 0)

  }

  ## Add data / order
  shifts_parsed <- shifts_parse_full %>%
    dplyr::mutate(
      game_id =       as.character(game_id_fun),
      game_date =     game_info_data$game_date,
      season =        game_info_data$season,
      game_period =   as.numeric(ifelse(game_period == "OT", 4, game_period)),
      home_team =     game_info_data$home_team,
      away_team =     game_info_data$away_team,
      shift_num =     as.numeric(shift_num),
      shift_start =   gsub(" / .*", "", shift_start),
      seconds_start = suppressWarnings(period_to_seconds(ms(shift_start))),
      seconds_start =
        case_when(
          game_period == 2 ~ seconds_start + 1200,
          game_period == 3 ~ seconds_start + 2400,
          game_period == 4 ~ seconds_start + 3600,
          game_period == 5 & game_info_data$session == "R" ~ seconds_start + 3900,
          game_period == 5 & game_info_data$session == "P" ~ seconds_start + 4800,
          game_period == 6 & game_info_data$session == "P" ~ seconds_start + 6000,
          game_period == 7 & game_info_data$session == "P" ~ seconds_start + 7200,
          TRUE ~ seconds_start
        ),
      shift_end =     gsub(" / .*", "", shift_end),
      seconds_end =   suppressWarnings(period_to_seconds(ms(shift_end))),
      seconds_end =
        case_when(
          game_period == 2 ~ seconds_end + 1200,
          game_period == 3 ~ seconds_end + 2400,
          game_period == 4 ~ seconds_end + 3600,
          game_period == 5 & game_info_data$session == "R" ~ seconds_end + 3900,
          game_period == 5 & game_info_data$session == "P" ~ seconds_end + 4800,
          game_period == 6 & game_info_data$session == "P" ~ seconds_end + 6000,
          game_period == 7 & game_info_data$session == "P" ~ seconds_end + 7200,
          TRUE ~ seconds_end
        ),
      seconds_duration = seconds_end - seconds_start
    ) %>%
    select(
      player, num_last_first, player_team_num, position, position_type, game_id, game_date, season,
      event_team, game_period, shift_num, seconds_start, seconds_end, seconds_duration, shift_start, shift_end, duration,
      home_team, away_team, shift_mod
    ) %>%
    arrange(seconds_start, event_team) %>%
    data.frame()



  ## --------------------- ##
  ##   Period Sums Parse   ##
  ## --------------------- ##

  ## Duplicate shifts html text for parsing
  home_shifts_text_dup <- c(shifts_list$home_shifts_text, shifts_list$home_shifts_text)
  away_shifts_text_dup <- c(shifts_list$away_shifts_text, shifts_list$away_shifts_text)


  ## Combine home and away period sums
  period_sum_full <- bind_rows(

    ## Home Period Sums
    foreach(i = 1:nrow(player_home_df), .combine = bind_rows) %do% {

      index <- which(shifts_list$home_shifts_titles[-1] == player_home_df$num_last_first[i])

      home_shifts_text_dup[(which(home_shifts_text_dup %in% c("SHF", "PR/SHF"))[index] + 6):(which(home_shifts_text_dup %in% c("Shift #", "Présence #Shift #"))[index + 1] - 8)] %>%
        matrix(ncol = 7,
               byrow = TRUE
        ) %>%
        data.frame(stringsAsFactors = FALSE) %>%
        dplyr::mutate(player =         player_home_df$player[i],
                      num_last_first = player_home_df$num_last_first[i],
                      event_team =     game_info_data$home_team
        ) %>%
        data.frame()

    } %>%
      mutate_at(
        vars(X1:X7),
        funs(gsub("\\s", NA, .))  ## turn blanks into NAs
      ),

    ## Away Period Sums
    foreach(i = 1:nrow(player_away_df), .combine = bind_rows) %do% {

      index <- which(shifts_list$away_shifts_titles[-1] == player_away_df$num_last_first[i])

      away_shifts_text_dup[(which(away_shifts_text_dup %in% c("SHF", "PR/SHF"))[index] + 6):(which(away_shifts_text_dup %in% c("Shift #", "Présence #Shift #"))[index + 1] - 8)] %>%
        matrix(ncol = 7,
               byrow = TRUE
        ) %>%
        data.frame(stringsAsFactors = FALSE) %>%
        dplyr::mutate(player =         player_away_df$player[i],
                      num_last_first = player_away_df$num_last_first[i],
                      event_team =     game_info_data$away_team
        ) %>%
        data.frame()

    } %>%
      mutate_at(
        vars(X1:X7),
        funs(gsub("\\s", NA, .))  ## turn blanks into NAs
      )

  )

  colnames(period_sum_full)[1:7] <- c("Per", "SHF", "AVG", "TOI", "EV_TOT", "PP_TOT", "SH_TOT")

  ## Finalize period sum data
  period_sum_full <- period_sum_full %>%
    dplyr::mutate(
      game_id =   as.character(game_id_fun),
      game_date = game_info_data$game_date,
      Per =       as.numeric(ifelse(Per == "OT", 4, Per)),
      season =    game_info_data$season,
      session =   game_info_data$session,
      Team =      event_team,
      Opponent =  ifelse(Team == game_info_data$home_team, game_info_data$home_team, game_info_data$away_team),
      is_home =   1 * (Team == game_info_data$home_team)
    ) %>%
    left_join(
      roster_data %>% select(num_last_first, player_team_num, position_type, position),
      by = "num_last_first"
    ) %>%
    mutate_at(
      vars(Per, SHF),
      funs(suppressWarnings(as.numeric(.)))
    ) %>%
    mutate_at(
      vars(AVG, TOI, EV_TOT, PP_TOT, SH_TOT),
      funs(suppressWarnings(round(period_to_seconds(ms(.)) / 60, 2)))  ## convert "00:00" format to minutes
    ) %>%
    select(
      player, num_last_first, player_team_num, position, position_type,
      game_id, game_date, season, session, Team, Opponent, is_home,
      game_period = Per,
      SHF, AVG,
      TOI_all = TOI,
      TOI_EV =  EV_TOT,
      TOI_PP = PP_TOT,
      TOI_SH = SH_TOT
    ) %>%
    data.frame()


  ## Return data as list
  return_list <- list(
    shifts_parse =        shifts_parsed,
    player_period_sums =  period_sum_full
  )

}

## Shift Backup: Process API Source if HTM source fails
sc.shifts_process_API <- function(game_id_fun, game_info_data) {

  ## Scrape API shifts
  shifts_API_list <- sc.scrape_shifts_API(game_id_fun = game_id_fun, attempts = 3)

  ## Scrape API rosters
  roster_API_list <- sc.scrape_events_API(game_id_fun = game_id_fun, attempts = 3)


  ## Compile Roster Data from API source
  roster_data_API <- bind_rows(

    ## Home players
    foreach(i = 1:length(roster_API_list$liveData$boxscore$teams$home$players), .combine = bind_rows) %do% {

      data.frame(
        playerId = na_if_null(roster_API_list$liveData$boxscore$teams$home$players[[i]]$person$id),
        fullName =  na_if_null(roster_API_list$liveData$boxscore$teams$home$players[[i]]$person$fullName),
        Team =      game_info_data$home_team,
        number =    na_if_null(roster_API_list$liveData$boxscore$teams$home$players[[i]]$jerseyNumber),
        stringsAsFactors = FALSE
      )

    } %>%
      data.frame(row.names = NULL),

    ## Away players
    foreach(i = 1:length(roster_API_list$liveData$boxscore$teams$away$players), .combine = bind_rows) %do% {

      data.frame(
        playerId = na_if_null(roster_API_list$liveData$boxscore$teams$away$players[[i]]$person$id),
        fullName =  na_if_null(roster_API_list$liveData$boxscore$teams$away$players[[i]]$person$fullName),
        Team =      game_info_data$away_team,
        number =    na_if_null(roster_API_list$liveData$boxscore$teams$away$players[[i]]$jerseyNumber),
        stringsAsFactors = FALSE
      )

    } %>%
      data.frame(row.names = NULL)

  ) %>%
    dplyr::mutate(player_team_num = paste0(Team, number))


  ## Make shifts data & player name "titles"
  shifts_API_df <- shifts_API_list$data %>%
    filter(is.na(eventDescription)) %>%   ## remove goal shift events
    left_join(roster_data_API %>% select(playerId, player_num = number), by = "playerId") %>%
    dplyr::mutate(
      teamName = toupper(teamName),
      teamAbbrev =
        case_when(
          teamAbbrev == "SJS" ~ "S.J",
          TRUE ~ teamAbbrev
        ),
      teamName = ifelse(teamAbbrev == "MTL", "MONTREAL CANADIENS", teamName),
      player_num = as.numeric(player_num),
      player_team_num = paste0(teamAbbrev, player_num)
    ) %>%
    data.frame()

  shifts_API_titles <- shifts_API_df %>%
    group_by(playerId, player_num, firstName, lastName, teamName, teamAbbrev) %>%
    summarise() %>%
    ungroup() %>%
    dplyr::mutate(num_last_first = toupper(paste0(player_num, " ", lastName, ", ", firstName))) %>%
    arrange(player_num) %>%
    data.frame()


  ## Make shifts_titles vector to match HTM source
  home_shifts_titles <- c(
    unique(filter(shifts_API_df, teamAbbrev == game_info_data$home_team)$teamName),
    filter(shifts_API_titles, teamAbbrev == game_info_data$home_team)$num_last_first
  )

  away_shifts_titles <- c(
    unique(filter(shifts_API_df, teamAbbrev == game_info_data$away_team)$teamName),
    filter(shifts_API_titles, teamAbbrev == game_info_data$away_team)$num_last_first
  )


  return(
    list(
      shifts_data =        shifts_API_df,
      home_shifts_titles = home_shifts_titles,
      away_shifts_titles = away_shifts_titles
    )
  )

}

## Shift Backup: Parse API Source shifts data
sc.shifts_parse_API <- function(game_id_fun, shifts_list, roster_data, game_info_data) {

  ## Make "shifts_raw" equivalent data frame
  shifts_parse_API <- shifts_list$shifts_data %>%
    left_join(
      roster_data %>%
        select(player_team_num, num_last_first, player, position, position_type),
      by = "player_team_num"
    ) %>%
    dplyr::mutate(
      game_id = game_info_data$game_id,
      game_date = game_info_data$game_date,
      game_period = period,
      season = game_info_data$season,
      home_team = game_info_data$home_team,
      away_team = game_info_data$away_team,
      shift_mod = 0,
      seconds_start = suppressWarnings(period_to_seconds(ms(startTime))),
      seconds_start =
        case_when(
          game_period == 2 ~ seconds_start + 1200,
          game_period == 3 ~ seconds_start + 2400,
          game_period == 4 ~ seconds_start + 3600,
          game_period == 5 & game_info_data$session == "R" ~ seconds_start + 3900,
          game_period == 5 & game_info_data$session == "P" ~ seconds_start + 4800,
          game_period == 6 & game_info_data$session == "P" ~ seconds_start + 6000,
          game_period == 7 & game_info_data$session == "P" ~ seconds_start + 7200,
          TRUE ~ seconds_start
        ),
      seconds_end = suppressWarnings(period_to_seconds(ms(endTime))),
      seconds_end =
        case_when(
          game_period == 2 ~ seconds_end + 1200,
          game_period == 3 ~ seconds_end + 2400,
          game_period == 4 ~ seconds_end + 3600,
          game_period == 5 & game_info_data$session == "R" ~ seconds_end + 3900,
          game_period == 5 & game_info_data$session == "P" ~ seconds_end + 4800,
          game_period == 6 & game_info_data$session == "P" ~ seconds_end + 6000,
          game_period == 7 & game_info_data$session == "P" ~ seconds_end + 7200,
          TRUE ~ seconds_end
        ),
      seconds_duration = seconds_end - seconds_start
    ) %>%
    select(
      player, num_last_first, player_team_num, position, position_type, game_id, game_date, season,
      event_team = teamAbbrev,
      game_period,
      shift_num = shiftNumber,
      seconds_start, seconds_end, seconds_duration,
      shift_start = startTime,
      shift_end = endTime,
      duration,
      home_team, away_team, shift_mod
    ) %>%
    arrange(player, game_period, seconds_start) %>%
    data.frame()


  ## Return data as list
  return_list <- list(
    shifts_parse =        shifts_parse_API,
    player_period_sums =  data.frame()  ## not available from API source
  )

}

## Finalize Shifts Data (HTM or API source)
sc.shifts_finalize <- function(game_id_fun, shifts_parse_data, events_data_HTM, game_info_data, fix_shifts) {

  ## ------------------------------- ##
  ##   Correct Skater Shift Errors   ##
  ## ------------------------------- ##

  shifts_parsed <- shifts_parse_data

  ## Fix OT shift_end issue (shift_end recorded as "0:00")
  if (fix_shifts == TRUE & min(na.omit(shifts_parsed$seconds_duration)) < 0) {
    shifts_parsed <- shifts_parsed %>%
      dplyr::mutate(
        shift_mod_hold =   ifelse(seconds_duration < 0, 1, 0),
        seconds_end =      ifelse(seconds_duration < 0, seconds_start + suppressWarnings(period_to_seconds(ms(duration))), seconds_end),
        seconds_end =      ifelse(seconds_end > max(events_data_HTM$game_seconds) & shift_mod_hold == 1 & game_period > 3, max(events_data_HTM$game_seconds), seconds_end),
        seconds_duration = ifelse(seconds_duration < 0 & shift_mod_hold == 1, seconds_end - seconds_start, seconds_duration),
        shift_mod =        ifelse(shift_mod_hold == 1, 1, shift_mod)
      ) %>%
      select(-shift_mod_hold)

  }

  ## Fix shift_end missing issue (very rare)
  if (fix_shifts == TRUE & sum(is.na(shifts_parsed$seconds_end)) > 0 & sum(is.na(shifts_parsed$seconds_duration)) > 0) {
    shifts_parsed <- shifts_parsed %>%
      dplyr::mutate(
        shift_mod =        ifelse(is.na(seconds_end) & is.na(seconds_duration) & !is.na(duration), 1, shift_mod),
        seconds_end =      ifelse(is.na(seconds_end) & !is.na(duration), seconds_start + suppressWarnings(period_to_seconds(ms(duration))), seconds_end),
        seconds_duration = ifelse(is.na(seconds_duration) & !is.na(duration), seconds_end - seconds_start, seconds_duration)
      ) %>%
      data.frame()

  }

  ## Final seconds_end check (NAs and negative shift lengths): "zero" shift
  shifts_parsed <- shifts_parsed %>%
    dplyr::mutate(
      shift_mod =        ifelse(is.na(seconds_end) | seconds_end < 0, 1, shift_mod),
      seconds_duration = ifelse(is.na(seconds_end) | seconds_end < 0, 0, seconds_duration),
      seconds_end =      ifelse(is.na(seconds_end) | seconds_end < 0, seconds_start, seconds_end)
    ) %>%
    data.frame()


  ## ------------------------------- ##
  ##   Correct Goalie Shift Errors   ##
  ## ------------------------------- ##

  if (fix_shifts == TRUE) {

    ## Get goalie shifts
    hold_goalie_shifts <- shifts_parsed %>%
      filter(position == "G") %>%
      group_by(game_id, game_period) %>%
      dplyr::mutate(
        is_home = 1 * (event_team == home_team),
        check =   1 * (period_to_seconds(ms(duration)) == period_to_seconds(ms(shift_end)) - period_to_seconds(ms(shift_start)))
      ) %>%
      select(player, position, game_id, is_home, event_team, game_period, seconds_start:seconds_duration, check) %>%
      data.frame()

    ## Ensure each shift end is after the shift start
    if (sum(hold_goalie_shifts$check) != nrow(hold_goalie_shifts)) {
      duration_problem <- 1
    } else {
      duration_problem <- 0
    }


    ## Transform goalie shifts data to long form
    hold_home_goalies <- hold_goalie_shifts %>%
      filter(is_home == 1) %>%
      select(player, game_period, seconds_start, seconds_end) %>%
      group_by(player, game_period) %>%
      gather(key = column, value = seconds) %>%
      data.frame()

    hold_away_goalies <- hold_goalie_shifts %>%
      filter(is_home == 0) %>%
      select(player, game_period, seconds_start, seconds_end) %>%
      group_by(player, game_period) %>%
      gather(key = column, value = seconds) %>%
      data.frame()

    ## Add player and game_period columns to above hold objects
    hold_home_goalies[, 3] <- hold_home_goalies[which(hold_home_goalies$column == "game_period"), ][2]
    hold_home_goalies[, 4] <- hold_home_goalies[which(hold_home_goalies$column == "player"), ][2]
    hold_away_goalies[, 3] <- hold_away_goalies[which(hold_away_goalies$column == "game_period"), ][2]
    hold_away_goalies[, 4] <- hold_away_goalies[which(hold_away_goalies$column == "player"), ][2]


    ## Bind data frames / summarise
    hold_ALL_goalies <- bind_rows(
      hold_home_goalies <- hold_home_goalies %>%
        filter(column %in% c("seconds_start", "seconds_end")) %>%
        select(player = seconds.2, game_period = seconds.1, column, seconds) %>%
        dplyr::mutate(
          index =       1 * (column == "seconds_start"),
          seconds =     as.numeric(seconds),
          game_period = as.numeric(game_period),
          is_home =     hold_goalie_shifts$is_home[match(player, hold_goalie_shifts$player)]
        ) %>%
        arrange(seconds, index) %>%
        data.frame(),
      hold_away_goalies <- hold_away_goalies %>%
        filter(column %in% c("seconds_start", "seconds_end")) %>%
        select(player = seconds.2, game_period = seconds.1, column, seconds) %>%
        dplyr::mutate(
          index =       1 * (column == "seconds_start"),
          seconds =     as.numeric(seconds),
          game_period = as.numeric(game_period),
          is_home =     hold_goalie_shifts$is_home[match(player, hold_goalie_shifts$player)]
        ) %>%
        arrange(seconds, index) %>%
        data.frame()
    )

    ## Check if shift starts and ends line up
    hold_check_goalie_shifts <- hold_ALL_goalies %>%
      dplyr::mutate(
        check = ifelse(column == "seconds_end" & lag(column) == "seconds_start", 1, 0),
        check = ifelse(column == "seconds_start" & lead(column) == "seconds_end", 1, check),
        check = ifelse(is.na(check), 0, check)
      )

    if (sum(hold_check_goalie_shifts$check) != nrow(hold_check_goalie_shifts)) {
      order_problem <- 1
    } else {
      order_problem <- 0
    }


    ## Check if all periods have correct goalie shift starts
    hold_ALL_goalies <- bind_rows(
      hold_home_goalies,
      hold_away_goalies
    ) %>%
      group_by(game_period, is_home) %>%
      dplyr::mutate(
        start_all = first(seconds),
        end_all =   last(seconds)
      ) %>%
      summarise_at(vars(start_all, end_all), funs(first)) %>%
      group_by(game_period, is_home) %>%
      dplyr::mutate(check = 1 * ((((start_all + 1200) / 1200) == game_period) & start_all < end_all)) %>%  ## verify shift_start == period start / verify shift_start before shift_end
      data.frame()

    if ((length(unique(filter(hold_ALL_goalies, is_home == 1)$game_period)) != max(shifts_parsed$game_period) |
         length(unique(filter(hold_ALL_goalies, is_home == 0)$game_period)) != max(shifts_parsed$game_period)) |
        sum(hold_ALL_goalies$check) != nrow(hold_ALL_goalies)
    ) {
      period_problem <- 1
    } else {
      period_problem <- 0
    }


    ## Impute new goalie shifts if error identified
    if (duration_problem == 1 | order_problem == 1 | period_problem == 1) {

      ## Vector indicating periods in game
      period_vec <- seq(1:max(shifts_parsed$game_period))

      ## Find problem periods for each team
      problem_vec_home <- unique(c(
        unique(filter(hold_goalie_shifts, is_home == 1, check == 0)$game_period),                           ## Locate problem periods for shift duration problems
        unique(filter(hold_check_goalie_shifts, is_home == 1, check == 0)$game_period),                     ## Locate problem periods for shift start/end order problems
        unique(filter(hold_ALL_goalies, is_home == 1, check == 0)$game_period),                             ## Locate problem periods for period start problems
        which(!period_vec %in% hold_ALL_goalies$game_period[which(hold_ALL_goalies$is_home == 1)] == TRUE)  ## Locate problem periods for missing periods problems
      ))

      problem_vec_away <- unique(c(
        unique(filter(hold_goalie_shifts, is_home == 0, check == 0)$game_period),                           ## Locate problem periods for shift duration problems
        unique(filter(hold_check_goalie_shifts, is_home == 0, check == 0)$game_period),                     ## Locate problem periods for shift start/end order problems
        unique(filter(hold_ALL_goalies, is_home == 0, check == 0)$game_period),                             ## Locate problem periods for period start problems
        which(!period_vec %in% hold_ALL_goalies$game_period[which(hold_ALL_goalies$is_home == 0)] == TRUE)  ## Locate problem periods for missing periods problems
      ))


      ## Create new goalie shifts for periods with identified issues
      if (length(problem_vec_home) > 0 | length(problem_vec_away) > 0) {

        ## Create new HOME goalie shift(s)
        if (length(problem_vec_home) > 0) {

          ## Check if at least first period is present
          if (min(problem_vec_home) > 1) {
            new_goalie_shift_home <- shifts_parsed %>%
              filter(
                position == "G",
                event_team == game_info_data$home_team,
                game_period == min(problem_vec_home) - 1
              ) %>%
              filter(dplyr::row_number() == max(dplyr::row_number())) %>%  ## filter to last goalie shifts before problem period
              data.frame()

            ## Verify period 2 is fine if not - use this shift as template
          } else if (!2 %in% problem_vec_home) {
            new_goalie_shift_home <- shifts_parsed %>%
              filter(
                position == "G",
                event_team == game_info_data$home_team,
                game_period == 2
              ) %>%
              data.frame()

            ## Verify period 3 is fine if not - use this shift as template
          } else if (!3 %in% problem_vec_home) {
            new_goalie_shift_home <- shifts_parsed %>%
              filter(
                position == "G",
                event_team == game_info_data$home_team,
                game_period == 3
              ) %>%
              data.frame()

          }

          ## Create additional rows if more are needed
          if (length(problem_vec_home) == 2) {
            new_goalie_shift_home <- bind_rows(new_goalie_shift_home, new_goalie_shift_home)

          } else if (length(problem_vec_home) == 3) {
            new_goalie_shift_home <- bind_rows(new_goalie_shift_home, new_goalie_shift_home, new_goalie_shift_home)

          } else if (length(problem_vec_home) == 4) {
            new_goalie_shift_home <- bind_rows(new_goalie_shift_home, new_goalie_shift_home, new_goalie_shift_home, new_goalie_shift_home)

          }

          ## Create new shift(s)
          new_goalie_shift_home <- new_goalie_shift_home %>%
            dplyr::mutate(
              game_period =      problem_vec_home,
              shift_num =        NA,
              seconds_start =    1200 * game_period - 1200,
              seconds_end =
                case_when(
                  # Regular Season
                  game_period <= 3 & game_info_data$session == "R" ~ 1200 * game_period,
                  game_period == 4 & game_info_data$session == "R" ~ max(na.omit(events_data_HTM$game_seconds)),
                  # Playoffs
                  game_period <  max(na.omit(events_data_HTM$game_period)) & game_info_data$session == "P" ~ 1200 * game_period,
                  game_period == max(na.omit(events_data_HTM$game_period)) & game_info_data$session == "P" ~ max(na.omit(events_data_HTM$game_seconds))
                ),
              seconds_duration = seconds_end - seconds_start,
              shift_start =      NA,
              shift_end =        NA,
              duration =         NA,
              home_team =        game_info_data$home_team,
              away_team =        game_info_data$away_team,
              shift_mod =        1
            ) %>%
            select(colnames(shifts_parsed)) %>%
            data.frame()

        } else {
          new_goalie_shift_home <- data.frame(matrix(ncol = ncol(shifts_parsed), nrow = 0))
          colnames(new_goalie_shift_home) <- colnames(shifts_parsed)

        }


        ## Create new AWAY goalie shift(s)
        if (length(problem_vec_away) > 0) {

          ## Check if at least first period is present
          if (min(problem_vec_away) > 1) {
            new_goalie_shift_away <- shifts_parsed %>%
              filter(
                position == "G",
                event_team == game_info_data$away_team,
                game_period == min(problem_vec_away) - 1
              ) %>%
              filter(dplyr::row_number() == max(dplyr::row_number())) %>%  ## filter to last goalie shifts before problem period
              data.frame()

            ## Verify period 2 is fine if not
          } else if (!2 %in% problem_vec_away) {
            new_goalie_shift_away <- shifts_parsed %>%
              filter(
                position == "G",
                event_team == game_info_data$away_team,
                game_period == 2
              ) %>%
              data.frame()

            ## Verify period 3 is fine if not
          } else if (!3 %in% problem_vec_away) {
            new_goalie_shift_away <- shifts_parsed %>%
              filter(
                position == "G",
                event_team == game_info_data$away_team,
                game_period == 3
              ) %>%
              data.frame()

          }

          ## Create additional rows if more are needed
          if (length(problem_vec_away) == 2) {
            new_goalie_shift_away <- bind_rows(new_goalie_shift_away, new_goalie_shift_away)

          } else if (length(problem_vec_away) == 3) {
            new_goalie_shift_away <- bind_rows(new_goalie_shift_away, new_goalie_shift_away, new_goalie_shift_away)

          } else if (length(problem_vec_away) == 4) {
            new_goalie_shift_away <- bind_rows(new_goalie_shift_away, new_goalie_shift_away, new_goalie_shift_away, new_goalie_shift_away)

          }

          ## Create new shift(s)
          new_goalie_shift_away <- new_goalie_shift_away %>%
            dplyr::mutate(
              game_period =      problem_vec_away,
              shift_num =        NA,
              seconds_start =    1200 * game_period - 1200,
              seconds_end =
                case_when(
                  ## Regular Season
                  game_period <= 3 & game_info_data$session == "R" ~ 1200 * game_period,
                  game_period == 4 & game_info_data$session == "R" ~ max(na.omit(events_data_HTM$game_seconds)),
                  ## Playoffs
                  game_period <  max(na.omit(events_data_HTM$game_period)) & game_info_data$session == "P" ~ 1200 * game_period,
                  game_period == max(na.omit(events_data_HTM$game_period)) & game_info_data$session == "P" ~ max(na.omit(events_data_HTM$game_seconds))
                ),
              seconds_duration = seconds_end - seconds_start,
              shift_start =      NA,
              shift_end =        NA,
              duration =         NA,
              home_team =        game_info_data$home_team,
              away_team =        game_info_data$away_team,
              shift_mod =        1
            ) %>%
            select(colnames(shifts_parsed)) %>%
            data.frame()

        } else {
          new_goalie_shift_away <- data.frame(matrix(ncol = ncol(shifts_parsed), nrow = 0))
          colnames(new_goalie_shift_away) <- colnames(shifts_parsed)

        }


        ## Bind new goalie shifts with original data
        shifts_raw_goalie_fix <- bind_rows(
          ## Home shifts
          shifts_parsed %>%
            filter(
              position == "G",
              event_team == game_info_data$home_team,
              !game_period %in% problem_vec_home
            ),
          new_goalie_shift_home,
          ## Away shifts
          shifts_parsed %>%
            filter(
              position == "G",
              event_team == game_info_data$away_team,
              !game_period %in% problem_vec_away
            ),
          new_goalie_shift_away
        ) %>%
          arrange(seconds_start, event_team) %>%
          data.frame()


        ## Combine with shifts_raw data
        shifts_raw_hold <- bind_rows(
          shifts_parsed %>% filter(position != "G"),
          shifts_raw_goalie_fix
        ) %>%
          dplyr::mutate(shift_mod = ifelse(is.na(shift_mod), 0, shift_mod)) %>%
          arrange(seconds_start, event_team) %>%
          data.frame()

      }

    }

    ## Determine if goalies shifts were corrected
    if (exists("shifts_raw_hold")) {
      shifts_raw <- shifts_raw_hold
    } else {
      shifts_raw <- shifts_parsed
    }



  } else {
    shifts_raw <- shifts_parsed

  }


  ## ----------------------------------- ##
  ##   Manual Goalie Shift Corrections   ##
  ## ----------------------------------- ##

  if (game_id_fun == "2019020019") {

    ## Pull out problematic goalie shifts
    manual_goalie_shift <- shifts_raw %>%
      filter(player == "MICHAEL.HUTCHINSON")

    manual_goalie_shift <- bind_rows(manual_goalie_shift, manual_goalie_shift[4, ])

    manual_goalie_shift[, "game_period"] <-   c(1, 2, 3, 3, 4)
    manual_goalie_shift[, "seconds_start"] <- c(0,    1200, 2400, 3525, 3600) ## 2400 + as.numeric(seconds(ms("18:45")))
    manual_goalie_shift[, "seconds_end"] <-   c(1200, 2400, 3517, 3600, 3900) ## 2400 + as.numeric(seconds(ms("18:37")))

    ## Combine with all shifts
    shifts_raw <- bind_rows(
      shifts_raw %>%
        filter(player != "MICHAEL.HUTCHINSON"),
      manual_goalie_shift
    ) %>%
      arrange(seconds_start, event_team)

  }



  ## ---------------------- ##
  ##   Format Shifts Data   ##
  ## ---------------------- ##

  player_shifts_final <- shifts_raw %>%
    dplyr::mutate(
      session =  game_info_data$session,
      Team =     ifelse(event_team == home_team, home_team, away_team),
      Opponent = ifelse(event_team == home_team, away_team, home_team),
      is_home =  1 * (event_team == home_team)
    ) %>%
    select(
      player, num_last_first, player_team_num, position, position_type, game_id, game_date, season, session, Team, Opponent, is_home,
      game_period, shift_num, seconds_start, seconds_end, seconds_duration, shift_start, shift_end, duration, shift_mod
    ) %>%
    arrange(seconds_start, is_home) %>%
    data.frame()


  ## Return data
  return(player_shifts_final)

}

## Create ON/OFF event types (HTM or API source)
sc.shifts_create_events <- function(shifts_final_data) {

  ## Create "CHANGE" Event Type
  shifts_events <- full_join(
    ## ON Shift Changes
    shifts_final_data %>%
      dplyr::mutate(
        home_team = unique(filter(shifts_final_data, is_home == 1)$Team),
        away_team = unique(filter(shifts_final_data, is_home == 0)$Team)
      ) %>%
      select(player_team_num, event_team = Team, game_id, game_period, seconds_start, home_team, away_team) %>%
      group_by(event_team, home_team, away_team, game_id, game_period, seconds_start) %>%
      dplyr::mutate(
        num = as.numeric(n()),
        players = paste(unique(player_team_num), collapse = ", ")
      ) %>%
      summarise_at(
        vars(num, players),
        funs(first(.))
      ) %>%
      dplyr::mutate(event_type = "ON") %>%
      rename(game_seconds = seconds_start) %>%
      data.frame(),
    ## OFF Shift Changes
    shifts_final_data %>%
      dplyr::mutate(
        home_team = unique(filter(shifts_final_data, is_home == 1)$Team),
        away_team = unique(filter(shifts_final_data, is_home == 0)$Team)
      ) %>%
      select(player_team_num, event_team = Team, game_id, game_period, seconds_end, home_team, away_team) %>%
      group_by(event_team, home_team, away_team, game_id, game_period, seconds_end) %>%
      dplyr::mutate(
        num = as.numeric(n()),
        players = paste(unique(player_team_num), collapse = ", ")
      ) %>%
      summarise_at(
        vars(num, players),
        funs(first(.))
      ) %>%
      dplyr::mutate(event_type = "OFF") %>%
      rename(game_seconds = seconds_end) %>%
      data.frame(),
    by = c("event_team", "home_team", "away_team", "game_id", "game_period", "game_seconds"),
    suffix = c("_on", "_off")
  ) %>%
    dplyr::mutate(
      num_on = ifelse(is.na(num_on), 0, num_on),
      num_off = ifelse(is.na(num_off), 0, num_off),
      is_home = 1 * (event_team == home_team),
      event_type = "CHANGE"
    ) %>%
    arrange(game_period, game_seconds, is_home) %>%
    select(game_id, game_seconds, game_period, event_team, event_type, num_on, num_off, players_on, players_off) %>%
    data.frame()


  ## Return data
  return(shifts_events)

}


## -------------------- ##
##   Join Coordinates   ##
## -------------------- ##

## Join coordinates (NHL API, main)
sc.join_coordinates_API <- function(events_data_API, events_data_HTM) {

  ## Base JSON events
  pbp_JSON_events <- events_data_API %>%
    group_by(game_period, game_seconds, event_type, event_team) %>%
    dplyr::mutate(group_count = n()) %>%
    data.frame()


  ## ----------------------------------- ##
  ##   Add Coords to HTM Events - Base   ##
  ## ----------------------------------- ##

  combine_events_okay_API <- events_data_HTM %>%
    select(eventIdx, game_period, game_seconds, event_type, event_team) %>%
    filter(event_type %in% unique(na.omit(pbp_JSON_events$event_type))) %>%
    left_join(
      pbp_JSON_events %>%
        select(-c(event_player_1)) %>%
        filter(group_count == 1),  ## filter out concurrent events
      by = c("game_period", "game_seconds", "event_type", "event_team")
    ) %>%
    filter(!is.na(group_count)) %>%
    select(-c(group_count)) %>%
    data.frame()



  ## ---------------------------------------- ##
  ##   Add Coords to HTM Events - Reconcile   ##
  ## ---------------------------------------- ##

  pbp_JSON_events_prob <- NULL

  ## Reconcile coordinates for concurrent events (same type, same second)
  if (nrow(filter(pbp_JSON_events, group_count > 1, event_type != "PENL")) > 0) {

    ## Prepare concurrent events data (JSON)
    pbp_JSON_events_prob <- pbp_JSON_events %>%
      filter(
        group_count > 1,           ## only act on problem rows
        event_type != "PENL"       ## not adding coordinates for concurrent penalties
      ) %>%
      dplyr::mutate(index = row_number()) %>%
      group_by(game_period, game_seconds, event_type, event_player_1) %>%
      dplyr::mutate(same_check = n()) %>% ## determine if/which event_player_1s are the same
      data.frame()


    ## Fix non-penalty events using pbp_JSON_events_prob object
    if (nrow(pbp_JSON_events_prob) > 0) {

      ## Initial join with HTM events
      hold_df_API <- events_data_HTM %>%
        select(eventIdx, game_period, game_seconds, event_type, event_team, event_player_1) %>%
        filter(event_type %in% unique(na.omit(pbp_JSON_events$event_type))) %>%
        left_join(
          pbp_JSON_events_prob,
          by = c("game_period", "game_seconds", "event_type", "event_team", "event_player_1")
        ) %>%
        filter(!is.na(group_count))

      ## Each event_player_1 is separate
      done_df_API <- hold_df_API %>%
        filter(same_check == 1) %>%
        select(eventIdx, game_period, game_seconds, event_type, event_team, event_description, coords_x, coords_y)

      ## 2 concurrent events - same event_player_1
      fix_df_API_2 <- hold_df_API %>%
        filter(same_check == 2) %>%
        ## Determine rows to keep
        group_by(game_period, game_seconds, event_type) %>%
        dplyr::mutate(
          row =      row_number(),
          filtered = suppressWarnings(ifelse(row_number() == max(row) | row_number() == min(row), 1, 0))
        ) %>%
        filter(filtered == 1) %>%
        ## Ensure new rows created from join are filtered out if something went wrong
        group_by(eventIdx) %>%
        dplyr::mutate(remove = row_number() - 1) %>%  ##  (any "created" row marked with a 1, 2, 3...)
        filter(remove == 0) %>%                ##  ("created" rows filtered out)
        ungroup() %>%
        select(eventIdx, game_period, game_seconds, event_type, event_team, event_description, coords_x, coords_y) %>%
        data.frame()

      ## 3 concurrent events - same event_player_1
      fix_df_API_3 <- hold_df_API %>%
        filter(same_check == 3) %>%
        ## Determine rows to keep
        group_by(game_period, game_seconds, event_type) %>%
        dplyr::mutate(
          row =      row_number(),
          filtered = suppressWarnings(ifelse(row_number() == max(row) | row_number() == min(row) | row_number() == min(row) + 4, 1, 0))
        ) %>%
        filter(filtered == 1) %>%
        ## Ensure new rows created from join are filtered out if something went wrong
        group_by(eventIdx) %>%
        dplyr::mutate(remove = row_number() - 1) %>%  ##  (any "created" row marked with a 1, 2, 3...)
        filter(remove == 0) %>%                ##  ("created" rows filtered out)
        ungroup() %>%
        select(eventIdx, game_period, game_seconds, event_type, event_team, event_description, coords_x, coords_y) %>%
        data.frame()

      ## 4 concurrent events - same event_player_1
      fix_df_API_4 <- hold_df_API %>%
        filter(same_check == 4) %>%
        ## Determine rows to keep
        group_by(game_period, game_seconds, event_type) %>%
        dplyr::mutate(
          row =      row_number(),
          filtered = suppressWarnings(ifelse(row_number() == max(row) | row_number() == min(row) | row_number() == min(row) + 5 | row_number() == min(row) + 10, 1, 0))
        ) %>%
        filter(filtered == 1) %>%
        ## Ensure new rows created from join are filtered out if something went wrong
        group_by(eventIdx) %>%
        dplyr::mutate(remove = row_number() - 1) %>%  ##  (any "created" row marked with a 1, 2, 3...)
        filter(remove == 0) %>%                ##  ("created" rows filtered out)
        ungroup() %>%
        select(eventIdx, game_period, game_seconds, event_type, event_team, event_description, coords_x, coords_y) %>%
        data.frame()

      ## 5 concurrent events - *** NO EXAMPLES TO CORRECT


      ## Combine to final object for joining
      combine_events_prob_API <- bind_rows(
        done_df_API,
        fix_df_API_2,
        fix_df_API_3,
        fix_df_API_4
      ) %>%
        arrange(eventIdx)

    }


    ## Join to data not effected by concurrent event issues
    if (exists("combine_events_prob_API")) {
      combine_events_reconcile_API <- bind_rows(
        combine_events_okay_API,
        combine_events_prob_API
      ) %>%
        arrange(eventIdx) %>%
        data.frame()

    } else {
      combine_events_reconcile_API <- combine_events_okay_API

    }

  }



  ## ---------------------------------- ##
  ##   Join JSON Coords w/ HTM Events   ##
  ## ---------------------------------- ##

  ## Determine object to join
  if (nrow(filter(pbp_JSON_events, group_count > 1, event_type != "PENL")) == 0) {
    combine_events_final_API <- combine_events_okay_API

  } else {
    combine_events_final_API <- combine_events_reconcile_API

  }


  ## Final Join
  pbp_events_full <- events_data_HTM %>%
    left_join(
      combine_events_final_API %>%
        select(
          eventIdx, game_period, game_seconds, event_type, coords_x, coords_y, event_team,
          event_description_alt = event_description
        ),
      by = c("eventIdx", "game_period", "game_seconds", "event_type", "event_team")
    ) %>%
    data.frame()


  ## Override reconciliation process if duplicate rows created
  if (nrow(events_data_HTM) != nrow(pbp_events_full)) {
    warning("additional events created in coordinate reconciliation, NAs created in lieu")

    pbp_events_full <- events_data_HTM %>%
      left_join(
        combine_events_okay %>%
          select(
            eventIdx, game_period, game_seconds, event_type, coords_x, coords_y, event_team,
            event_description_alt = event_description
          ),
        by = c("eventIdx", "game_period", "game_seconds", "event_type", "event_team")
      ) %>%
      data.frame()

  }


  return(pbp_events_full)

}

## Join coordinates (ESPN, backup)
sc.join_coordinates_ESPN <- function(season_id_fun, events_data_ESPN, events_data_HTM, roster_data, game_info_data) {

  ## Change WPG to ATL if applicable (same ESPN team ID)
  if (as.numeric(season_id_fun) <= 20102011) {
    ESPN_team_IDs_update <- ESPN_team_IDs %>%
      dplyr::mutate(Team = ifelse(Team == "WPG", "ATL", Team))

  } else {
    ESPN_team_IDs_update <- ESPN_team_IDs

  }


  ## --------------------------- ##
  ##   Prepare All ESPN Events   ##
  ## --------------------------- ##

  pbp_ESPN_events <- events_data_ESPN %>%
    select(
      ESPN_type =         X3,
      coords_x =          X1,
      coords_y =          X2,
      time =              X4,
      game_period =       X5,
      team_ID_ESPN =      X15,
      event_description = X9
    ) %>%
    mutate_at(
      vars(coords_x, coords_y, game_period),
      funs(as.numeric(.))
    ) %>%
    dplyr::mutate(
      game_seconds = suppressWarnings(period_to_seconds(ms(time))),
      game_seconds =
        case_when(
          game_period == 2 ~ game_seconds + 1200,
          game_period == 3 ~ game_seconds + 2400,
          game_period == 4 ~ game_seconds + 3600,
          game_period == 5 & game_info_data$session == "R" ~ game_seconds + 3900,
          game_period == 5 & game_info_data$session == "P" ~ game_seconds + 4800,
          game_period == 6 & game_info_data$session == "P" ~ game_seconds + 6000,
          game_period == 7 & game_info_data$session == "P" ~ game_seconds + 7200,
          TRUE ~ game_seconds
        ),
      ## Join in event types
      event_type = ESPN_codes$event[match(ESPN_type, ESPN_codes$code)],
      event_type =
        case_when(
          event_type == "GvTk" & grepl("Giveaway", event_description) ~ "GIVE",
          event_type == "GvTk" & grepl("Takeaway", event_description) ~ "TAKE",
          event_type == "SOut" & grepl("SAVE", event_description) ~ "SHOT",
          event_type == "SOut" & grepl("MISS", event_description) ~ "MISS",
          event_type == "SOut" & grepl("GOAL", event_description) ~ "GOAL",
          TRUE ~ event_type
        ),
      event_team = ESPN_team_IDs_update$Team[match(team_ID_ESPN, ESPN_team_IDs_update$team_ID)],
      ## Account for ESPN's strange method of recording coordinates
      coords_x =
        case_when(
          game_info_data$season %in% c("20072008", "20082009") ~ round((coords_x - 1.5) * (198 / 197), 1),  ## ESPN min/max was -97/100, center and expand to match NHL API scale
          as.numeric(game_info_data$season) >= 20162017 ~ coords_x + 1,  ## ESPN adjusts other way as of 20162017
          game_info_data$season == "20092010" & coords_x > 99 ~ 99,      ## handful of coordinates greater than 99
          TRUE ~ coords_x
        ),
      coords_y =
        case_when(
          game_info_data$season %in% c("20072008", "20082009") ~ round((coords_y + 1.5) * (84 / 83), 1),  ## ESPN min/max was -43/40, center and expand to match NHL API scale
          as.numeric(game_info_data$season) >= 20162017 ~ coords_y - 1,  ## ESPN adjusts other way as of 20162017
          game_info_data$season == "20092010" & coords_y < -42 ~ -42,      ## handful of coordinates less than -42
          TRUE ~ coords_y
        )
    ) %>%
    filter(event_type %in% c("TAKE", "GIVE", "MISS", "HIT", "SHOT", "BLOCK", "GOAL", "PENL", "FAC")) %>%
    ## Determine if simultaneous events are present
    group_by(game_period, game_seconds, event_type, event_team) %>%
    dplyr::mutate(group_count = n()) %>%
    select(game_period, game_seconds, event_type, event_team, event_description, coords_x, coords_y, group_count, team_ID_ESPN, ESPN_type) %>%
    data.frame()



  ## ----------------------------------- ##
  ##   Add Coords to HTM Events - Base   ##
  ## ----------------------------------- ##

  combine_events_okay <- events_data_HTM %>%
    select(eventIdx, game_period, game_seconds, event_type, event_team) %>%
    filter(event_type %in% unique(na.omit(pbp_ESPN_events$event_type))) %>%
    left_join(
      pbp_ESPN_events %>%
        filter(group_count == 1),  ## filter out concurrent events
      by = c("game_period", "game_seconds", "event_type", "event_team")
    ) %>%
    filter(!is.na(group_count)) %>%
    select(-c(group_count)) %>%
    data.frame()



  ## ---------------------------------------- ##
  ##   Add Coords to HTM Events - Reconcile   ##
  ## ---------------------------------------- ##

  pbp_ESPN_events_prob <- NULL

  ## Reconcile coordinates for concurrent events (same type, same second)
  if (nrow(filter(pbp_ESPN_events, group_count > 1, event_type != "PENL")) > 0) {

    ## Prepare concurrent events data (ESPN)
    pbp_ESPN_events_prob <- pbp_ESPN_events %>%
      filter(
        group_count > 1,           ## only act on problem rows
        event_type != "PENL"       ## not adding coordinates for concurrent penalties
      ) %>%
      dplyr::mutate(index = row_number()) %>%
      ## Extract the first event player
      dplyr::mutate(
        player_match =
          case_when(
            ## Normal Strengths
            event_type == "SHOT" & game_period < 5 ~  gsub("Shot\\s*on\\s*goal\\s*by\\s*", "", event_description) %>% gsub("\\s*saved by.+|\\s*[(]+.*", "", .),
            event_type == "MISS" & game_period < 5 ~  gsub("Shot\\s*missed\\s*by\\s*", "", event_description) %>% gsub("\\s*[(]+.*", "", .),
            event_type == "BLOCK" & game_period < 5 ~ gsub("\\s*shot\\s*blocked\\s*by.*", "", event_description), ## blocked shots do not have the shooter prior to 2010-2011

            event_type == "HIT" ~  gsub("\\s*credited\\s*.*", "", event_description),  ## ~ 160 games missing HIT events in the ESPN source for '07-08
            event_type == "GIVE" ~ gsub("Giveaway\\s*by\\s*", "", event_description) %>% gsub("\\s*\\bin\\b\\s*.*", "", .),

            ## NOTE: Faceoffs are missing the player who lost the faceoff after certain season (unclear at this point)
            event_type == "FAC" & event_team == game_info_data$away_team ~ gsub("\\s*won\\s*faceoff.*", "", event_description),
            event_type == "FAC" & event_team == game_info_data$home_team ~ gsub(".*won\\s*faceoff\\s*against\\s*", "", event_description),

            ## Shootouts
            event_type %in% c("SHOT", "MISS") & game_period == 5 & grepl("Shootout\\s*attempt\\s*by\\s*", event_description) ~ gsub("Shootout\\s*attempt\\s*by\\s*", "", event_description) %>% gsub("\\s*saved by.+", "", .),
            event_type %in% c("SHOT", "MISS") & game_period == 5 & grepl("shootout\\s*attempt\\s*against", event_description) ~ gsub("\\s*shootout\\s*attempt\\s*against.*", "", event_description),

            event_type == "GOAL" & game_period == 5 & grepl("Shootout\\s*GOAL\\s*scored\\s*by\\s*", event_description) ~ gsub("Shootout\\s*GOAL\\s*scored\\s*by\\s*", "", event_description) %>% gsub("\\s*\\bon\\b\\s*.*", "", .),
            event_type == "GOAL" & game_period == 5 & grepl("shootout\\s*attempt\\s*against", event_description) ~       gsub("\\s*shootout\\s*attempt\\s*against.*", "", event_description)
          )
      ) %>%
      group_by(index) %>%
      dplyr::mutate(
        last_name =      toupper(strsplit(player_match, "\\s")[[1]][2]),
        ## Match players accounting for faceoffs
        event_player_1 =
          case_when(
            event_type != "FAC" ~ roster_data$player_team_num[match(paste0(event_team, last_name), paste0(roster_data$Team, roster_data$lastName))],
            event_type == "FAC" ~ roster_data$player_team_num[match(paste0(game_info_data$away_team, last_name), paste0(roster_data$Team, roster_data$lastName))]
          ),
        ## Attempt to address two-string last names
        name_length =    max(as.numeric(lapply(strsplit(player_match, "\\s"), length))),
        event_player_1 = ifelse(is.na(event_player_1) & name_length > 2,
                                roster_data$player_team_num[match(
                                  paste0(event_team,
                                         toupper(strsplit(player_match, "\\s")[[1]][2]),
                                         toupper(strsplit(player_match, "\\s")[[1]][3])
                                  ),
                                  paste0(roster_data$Team,
                                         gsub("\\s*", "", roster_data$lastName))
                                )],
                                event_player_1
        )
      ) %>%
      select(-c(name_length)) %>%
      group_by(game_period, game_seconds, event_type, event_player_1) %>%
      dplyr::mutate(same_check = n()) %>%
      data.frame()


    ## Fix non-penalty events using pbp_ESPN_events_prob object
    if (nrow(pbp_ESPN_events_prob) > 0) {

      ## Initial join with HTM events
      hold_df_ESPN <- events_data_HTM %>%
        select(eventIdx, game_period, game_seconds, event_type, event_team, event_player_1) %>%
        filter(event_type %in% unique(na.omit(pbp_ESPN_events$event_type))) %>%
        left_join(
          pbp_ESPN_events_prob,
          by = c("game_period", "game_seconds", "event_type", "event_team", "event_player_1")
        ) %>%
        filter(!is.na(group_count))

      ## Each event_player_1 is separate
      done_df_ESPN <- hold_df_ESPN %>%
        filter(same_check == 1) %>%
        select(eventIdx, game_period, game_seconds, event_type, event_team, event_description, coords_x, coords_y)

      ## 2 concurrent events - same event_player_1
      fix_df_ESPN_2 <- hold_df_ESPN %>%
        filter(same_check == 2) %>%
        ## Determine rows to keep
        group_by(game_period, game_seconds, event_type) %>%
        dplyr::mutate(
          row =      row_number(),
          filtered = suppressWarnings(ifelse(row_number() == max(row) | row_number() == min(row), 1, 0))
        ) %>%
        filter(filtered == 1) %>%
        ## Ensure new rows created from join are filtered out if something went wrong
        group_by(eventIdx) %>%
        dplyr::mutate(remove = row_number() - 1) %>%  ##  (any "created" row marked with a 1, 2, 3...)
        filter(remove == 0) %>%                ##  ("created" rows filtered out)
        ungroup() %>%
        select(eventIdx, game_period, game_seconds, event_type, event_team, event_description, coords_x, coords_y) %>%
        data.frame()

      ## 3 concurrent events - same event_player_1
      fix_df_ESPN_3 <- hold_df_ESPN %>%
        filter(same_check == 3) %>%
        ## Determine rows to keep
        group_by(game_period, game_seconds, event_type) %>%
        dplyr::mutate(
          row =      row_number(),
          filtered = suppressWarnings(ifelse(row_number() == max(row) | row_number() == min(row) | row_number() == min(row) + 4, 1, 0))
        ) %>%
        filter(filtered == 1) %>%
        ## Ensure new rows created from join are filtered out if something went wrong
        group_by(eventIdx) %>%
        dplyr::mutate(remove = row_number() - 1) %>%  ##  (any "created" row marked with a 1, 2, 3...)
        filter(remove == 0) %>%                ##  ("created" rows filtered out)
        ungroup() %>%
        select(eventIdx, game_period, game_seconds, event_type, event_team, event_description, coords_x, coords_y) %>%
        data.frame()

      ## 4 concurrent events - same event_player_1
      fix_df_ESPN_4 <- hold_df_ESPN %>%
        filter(same_check == 4) %>%
        ## Determine rows to keep
        group_by(game_period, game_seconds, event_type) %>%
        dplyr::mutate(
          row =      row_number(),
          filtered = suppressWarnings(ifelse(row_number() == max(row) | row_number() == min(row) | row_number() == min(row) + 5 | row_number() == min(row) + 10, 1, 0))
        ) %>%
        filter(filtered == 1) %>%
        ## Ensure new rows created from join are filtered out if something went wrong
        group_by(eventIdx) %>%
        dplyr::mutate(remove = row_number() - 1) %>%  ##  (any "created" row marked with a 1, 2, 3...)
        filter(remove == 0) %>%                ##  ("created" rows filtered out)
        ungroup() %>%
        select(eventIdx, game_period, game_seconds, event_type, event_team, event_description, coords_x, coords_y) %>%
        data.frame()

      ## 5 concurrent events - *** NO EXAMPLES TO CORRECT


      ## Combine to final object for joining
      combine_events_prob <- bind_rows(
        done_df_ESPN,
        fix_df_ESPN_2,
        fix_df_ESPN_3,
        fix_df_ESPN_4
      ) %>%
        arrange(eventIdx)

    }


    ## Join to data not effected by concurrent event issues
    if (exists("combine_events_prob")) {
      combine_events_reconcile <- bind_rows(
        combine_events_okay,
        combine_events_prob
      ) %>%
        arrange(eventIdx) %>%
        data.frame()

    }
    else {
      combine_events_reconcile <- combine_events_okay

    }

  }



  ## ---------------------------------- ##
  ##   Join ESPN Coords w/ HTM Events   ##
  ## ---------------------------------- ##

  ## Determine object to join
  if (nrow(filter(pbp_ESPN_events, group_count > 1, event_type != "PENL")) == 0) {
    combine_events_final <- combine_events_okay

  } else {
    combine_events_final <- combine_events_reconcile

  }


  ## Final Join
  pbp_events_full <- events_data_HTM %>%
    left_join(
      combine_events_final %>%
        select(
          eventIdx, game_period, game_seconds, event_type, coords_x, coords_y, event_team,
          event_description_alt = event_description
        ),
      by = c("eventIdx", "game_period", "game_seconds", "event_type", "event_team")
    ) %>%
    data.frame()


  ## Override reconciliation process if duplicate rows created
  if (nrow(events_data_HTM) != nrow(pbp_events_full)) {
    warning("additional events created in coordinate reconciliation, NAs created in lieu")

    pbp_events_full <- events_data_HTM %>%
      left_join(
        combine_events_okay %>%
          select(
            eventIdx, game_period, game_seconds, event_type, coords_x, coords_y, event_team,
            event_description_alt = event_description
          ),
        by = c("eventIdx", "game_period", "game_seconds", "event_type", "event_team")
      ) %>%
      data.frame()

  }


  return(pbp_events_full)

}


## -------------------- ##
##   Combine All Data   ##
## -------------------- ##

## Combine / Process Shifts and Events Data
sc.pbp_combine <- function(events_data, shifts_data, roster_data, game_info_data) {

  ## Join / Arrange
  pbp_df <- bind_rows(
    events_data,
    shifts_data
  ) %>%
    dplyr::mutate(
      season =    game_info_data$season,
      game_date = game_info_data$game_date,
      session =   game_info_data$session,
      home_team = game_info_data$home_team,
      away_team = game_info_data$away_team
    ) %>%
    dplyr::mutate(
      priority =
        1 * (event_type %in% c("TAKE", "GIVE", "MISS", "HIT", "SHOT", "BLOCK") & !(game_period == 5 & session == "R")) +
        2 * (event_type == "GOAL" & !(game_period == 5 & session == "R")) +
        3 * (event_type == "STOP" & !(game_period == 5 & session == "R")) +
        4 * (event_type == "DELPEN" & !(game_period == 5 & session == "R")) +  ## new DELPEN event comes before PENL event
        5 * (event_type == "PENL" & !(game_period == 5 & session == "R")) +
        6 * (event_type == "CHANGE" & !(game_period == 5 & session == "R")) +
        7 * (event_type == "PEND" & !(game_period == 5 & session == "R")) +
        8 * (event_type == "GEND" & !(game_period == 5 & session == "R")) +
        9 * (event_type == "FAC" &  !(game_period == 5 & session == "R"))
    ) %>%
    arrange(
      game_period,
      game_seconds,
      priority
    ) %>%
    dplyr::mutate(event_index = as.numeric(cumsum(!is.na(game_id)))) %>%
    select(-priority) %>%
    data.frame()


  is_on_matrix_home <- foreach(i = 1:length(roster_data$player_team_num), .combine = cbind) %do% {

    vec <- cumsum(
      1 * (grepl(paste0("\\b", roster_data$player_team_num[i], "\\b"), pbp_df$players_on) &  ## "\\b" forces exact match
             pbp_df$event_type == "CHANGE" &
             pbp_df$event_team == pbp_df$home_team) -
        1 * (grepl(paste0("\\b", roster_data$player_team_num[i], "\\b"), pbp_df$players_off) &  ## "\\b" forces exact match
               pbp_df$event_type == "CHANGE" &
               pbp_df$event_team == pbp_df$home_team)
    )

  }


  is_on_matrix_away <- foreach(i = 1:length(roster_data$player_team_num), .combine = cbind) %do% {

    vec <- cumsum(
      1 * (grepl(paste0("\\b", roster_data$player_team_num[i], "\\b"), pbp_df$players_on) &  ## "\\b" forces exact match
             pbp_df$event_type == "CHANGE" &
             pbp_df$event_team == pbp_df$away_team) -
        1 * (grepl(paste0("\\b", roster_data$player_team_num[i], "\\b"), pbp_df$players_off) &  ## "\\b" forces exact match
               pbp_df$event_type == "CHANGE" &
               pbp_df$event_team == pbp_df$away_team)
    )

  }

  ## Set column names of matrices
  colnames(is_on_matrix_home) <- roster_data$player
  colnames(is_on_matrix_away) <- roster_data$player


  ## Combine matrices (from Manny Perry's code)
  is_on_df_home <- which(
    is_on_matrix_home == 1,
    arr.ind = TRUE
  ) %>%
    data.frame() %>%
    group_by(row) %>%
    summarise(
      home_on_1 = colnames(is_on_matrix_home)[unique(col)[1]],
      home_on_2 = colnames(is_on_matrix_home)[unique(col)[2]],
      home_on_3 = colnames(is_on_matrix_home)[unique(col)[3]],
      home_on_4 = colnames(is_on_matrix_home)[unique(col)[4]],
      home_on_5 = colnames(is_on_matrix_home)[unique(col)[5]],
      home_on_6 = colnames(is_on_matrix_home)[unique(col)[6]],
      home_on_7 = colnames(is_on_matrix_home)[unique(col)[7]]
    ) %>%
    data.frame()

  is_on_df_away <- which(
    is_on_matrix_away == 1,
    arr.ind = TRUE
  ) %>%
    data.frame() %>%
    group_by(row) %>%
    summarise(
      away_on_1 = colnames(is_on_matrix_away)[unique(col)[1]],
      away_on_2 = colnames(is_on_matrix_away)[unique(col)[2]],
      away_on_3 = colnames(is_on_matrix_away)[unique(col)[3]],
      away_on_4 = colnames(is_on_matrix_away)[unique(col)[4]],
      away_on_5 = colnames(is_on_matrix_away)[unique(col)[5]],
      away_on_6 = colnames(is_on_matrix_away)[unique(col)[6]],
      away_on_7 = colnames(is_on_matrix_away)[unique(col)[7]]
    ) %>%
    data.frame()


  ## Create vector to use for goalie determination
  goalie_vec <- c(
    as.character(filter(roster_data, position_type == "G", Team == game_info_data$home_team)$player),
    as.character(filter(roster_data, position_type == "G", Team == game_info_data$away_team)$player)
  )

  ## Add home goalies
  is_on_df_home_return <- is_on_df_home %>%
    dplyr::mutate(
      home_goalie =
        case_when(
          home_on_1 %in% goalie_vec ~ home_on_1,
          home_on_2 %in% goalie_vec ~ home_on_2,
          home_on_3 %in% goalie_vec ~ home_on_3,
          home_on_4 %in% goalie_vec ~ home_on_4,
          home_on_5 %in% goalie_vec ~ home_on_5,
          home_on_6 %in% goalie_vec ~ home_on_6,
          home_on_7 %in% goalie_vec ~ home_on_7
        )
    ) %>%
    rename(event_index = row) %>%  ## row determined above in matrix construction, renamed for joining
    data.frame()

  ## Add away goalies
  is_on_df_away_return <- is_on_df_away %>%
    dplyr::mutate(
      away_goalie =
        case_when(
          away_on_1 %in% goalie_vec ~ away_on_1,
          away_on_2 %in% goalie_vec ~ away_on_2,
          away_on_3 %in% goalie_vec ~ away_on_3,
          away_on_4 %in% goalie_vec ~ away_on_4,
          away_on_5 %in% goalie_vec ~ away_on_5,
          away_on_6 %in% goalie_vec ~ away_on_6,
          away_on_7 %in% goalie_vec ~ away_on_7
        )
    ) %>%
    rename(event_index = row) %>%  ## row determined above in matrix construction, renamed for joining
    data.frame()


  ## Return data as list
  return_list <- list(
    pbp_final =     pbp_df,
    is_on_df_home = is_on_df_home_return,
    is_on_df_away = is_on_df_away_return
  )

}

## Finalize PBP Data
sc.pbp_finalize <- function(pbp_data, on_data_home, on_data_away, roster_data, game_info_data, live_scrape) {

  ## Create vectors to be used below
  skater_vec_home <- as.character(filter(roster_data, position_type != "G", Team == game_info_data$home_team)$player)
  skater_vec_away <- as.character(filter(roster_data, position_type != "G", Team == game_info_data$away_team)$player)

  goalie_vec_home <- as.character(filter(roster_data, position_type == "G", Team == game_info_data$home_team)$player_team_num)
  goalie_vec_away <- as.character(filter(roster_data, position_type == "G", Team == game_info_data$away_team)$player_team_num)
  goalie_vec <-      c(goalie_vec_home, goalie_vec_away)


  ## Combine and modify to form final pbp data frame
  pbp_combined <- pbp_data %>%
    left_join(on_data_home, by = "event_index") %>%
    left_join(on_data_away, by = "event_index") %>%
    group_by(game_id) %>%
    arrange(event_index) %>%
    dplyr::mutate(
      event_player_1 = roster_data$player[match(event_player_1, roster_data$player_team_num)],
      event_player_2 = roster_data$player[match(event_player_2, roster_data$player_team_num)],
      event_player_3 = roster_data$player[match(event_player_3, roster_data$player_team_num)],
      ## Set skaters & goalies for shootouts
      home_goalie =  ifelse(game_period == 5 & game_info_data$session == "R" & event_type %in% st.fenwick_events & is.na(home_goalie), last(na.omit(home_goalie)), home_goalie),
      away_goalie =  ifelse(game_period == 5 & game_info_data$session == "R" & event_type %in% st.fenwick_events & is.na(away_goalie), last(na.omit(away_goalie)), away_goalie),
      home_on_1 =    ifelse(game_period == 5 & game_info_data$session == "R" & event_team == home_team, event_player_1, home_on_1),
      away_on_1 =    ifelse(game_period == 5 & game_info_data$session == "R" & event_team == away_team, event_player_1, away_on_1),
      home_on_2 =    ifelse(game_period == 5 & game_info_data$session == "R", home_goalie, home_on_2),
      away_on_2 =    ifelse(game_period == 5 & game_info_data$session == "R", away_goalie, away_on_2)
    ) %>%
    ## Fix skaters for penalty shots
    mutate_at(
      vars(home_on_1:home_on_7, away_on_1:away_on_7),
      funs(ifelse(grepl("penalty shot", tolower(event_description)) & . != event_player_1 & . != home_goalie & . != away_goalie, NA, .))
    ) %>%
    dplyr::mutate(
      home_skaters = 7 -
        1 * (is.na(home_on_1)) - 1 * (is.na(home_on_2)) - 1 * (is.na(home_on_3)) -
        1 * (is.na(home_on_4)) - 1 * (is.na(home_on_5)) - 1 * (is.na(home_on_6)) - 1 * (is.na(home_on_7)) -
        1 * (!is.na(home_goalie)),
      away_skaters = 7 -
        1 * (is.na(away_on_1)) - 1 * (is.na(away_on_2)) - 1 * (is.na(away_on_3)) -
        1 * (is.na(away_on_4)) - 1 * (is.na(away_on_5)) - 1 * (is.na(away_on_6)) - 1 * (is.na(away_on_7)) -
        1 * (!is.na(away_goalie)),
      home_score =   cumsum(event_type == "GOAL" & event_team == home_team) - 1 * (event_type == "GOAL" & event_team == home_team),
      away_score =   cumsum(event_type == "GOAL" & event_team == away_team) - 1 * (event_type == "GOAL" & event_team == away_team),
      ## Determine event length and check potential issues
      event_length = lead(game_seconds, 1) - game_seconds,
      event_length = ifelse(is.na(event_length) | event_length < 0, 0, event_length)
    ) %>%
    group_by(event_index) %>%
    ## Goalie change indication
    dplyr::mutate(
      home_g_change =   ifelse(sum(1 * (goalie_vec %in% strsplit(paste(players_on, players_off, sep = ", "), ", ")[[1]])) == 1,
                               ifelse(goalie_vec[goalie_vec %in% strsplit(paste(players_on, players_off, sep = ", "), ", ")[[1]]] %in% goalie_vec_home,
                                      goalie_vec[goalie_vec %in% strsplit(paste(players_on, players_off, sep = ", "), ", ")[[1]]], NA),
                               NA),
      away_g_change =   ifelse(sum(1 * (goalie_vec %in% strsplit(paste(players_on, players_off, sep = ", "), ", ")[[1]])) == 1,
                               ifelse(goalie_vec[goalie_vec %in% strsplit(paste(players_on, players_off, sep = ", "), ", ")[[1]]] %in% goalie_vec_away,
                                      goalie_vec[goalie_vec %in% strsplit(paste(players_on, players_off, sep = ", "), ", ")[[1]]], NA),
                               NA),
      home_g_change =   roster_data$player[match(home_g_change, roster_data$player_team_num)],
      away_g_change =   roster_data$player[match(away_g_change, roster_data$player_team_num)]
    ) %>%
    group_by(game_id) %>%
    dplyr::mutate(
      game_strength_state = paste(
        ifelse(is.na(home_goalie), "E", home_skaters),
        ifelse(is.na(away_goalie), "E", away_skaters),
        sep = "v"
      ),
      game_strength_state = ifelse((home_skaters == 6 & !is.na(home_goalie)) | (away_skaters == 6 & !is.na(away_goalie)), "illegal", game_strength_state), ## Determine if strength state is illegal
      game_score_state =    paste(home_score, away_score, sep = "v"),
      ## Alternate strength state calculation (disregards goalies)
      home_skaters_alt =
        1 * (home_on_1 %in% skater_vec_home) + 1 * (home_on_2 %in% skater_vec_home) + 1 * (home_on_3 %in% skater_vec_home) +
        1 * (home_on_4 %in% skater_vec_home) + 1 * (home_on_5 %in% skater_vec_home) + 1 * (home_on_6 %in% skater_vec_home) + 1 * (home_on_7 %in% skater_vec_home),
      away_skaters_alt =
        1 * (away_on_1 %in% skater_vec_away) + 1 * (away_on_2 %in% skater_vec_away) + 1 * (away_on_3 %in% skater_vec_away) +
        1 * (away_on_4 %in% skater_vec_away) + 1 * (away_on_5 %in% skater_vec_away) + 1 * (away_on_6 %in% skater_vec_away) + 1 * (away_on_7 %in% skater_vec_away),
      game_strength_state_alt = gsub("6|7", "E", paste0(home_skaters_alt, "v", away_skaters_alt))
    ) %>%
    select(
      ## Main selections
      season, game_id, game_date, session, event_index, game_period, game_seconds, event_type,
      event_description, event_detail, event_zone, event_team, event_player_1:event_player_3, event_length, coords_x, coords_y,
      num_on, num_off, players_on, players_off, home_on_1:home_on_7, away_on_1:away_on_7, home_goalie, away_goalie,
      home_team, away_team, home_skaters, away_skaters, home_score, away_score, game_score_state, game_strength_state,
      ## Additional selections (to be split out)
      home_skaters_alt, away_skaters_alt, game_strength_state_alt,
      home_g_change, away_g_change,
      event_description_alt
    ) %>%
    arrange(game_id, event_index) %>%
    data.frame()


  ## Use alternate strength state if game is in progress
  if (live_scrape == TRUE) {
    pbp_combined <- pbp_combined %>%
      dplyr::mutate(game_strength_state = game_strength_state_alt)

  }


  ## Split into base and extra data to return
  pbp_base <- pbp_combined %>%
    select(season:game_strength_state)

  pbp_extras <- pbp_combined %>%
    select(game_id, event_index, home_skaters_alt:event_description_alt)


  ## Return data as a list
  return_list <- list(
    pbp_base =   pbp_base,
    pbp_extras = pbp_extras
  )

}



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


