###################################################################################
#####         Evolving-Hockey Scraper         ||          2019-05-18          #####
###################################################################################

## Current as of: R version 3.6.0 (2019-04-26)


## *** Notes & examples provided below all functions

## --------------- ##
##   Source URLs   ##
## --------------- ##

#####################

## HTML main source:
# events source:        http://www.nhl.com/scores/htmlreports/20182019/PL020001.HTM
# rosters source:       http://www.nhl.com/scores/htmlreports/20182019/RO020001.HTM
# shifts source (home): http://www.nhl.com/scores/htmlreports/20182019/TH020001.HTM
# shifts source (away): http://www.nhl.com/scores/htmlreports/20182019/TV020001.HTM

## HTML extras:
# game summary:  http://www.nhl.com/scores/htmlreports/20182019/GS020001.HTM
# event summary: http://www.nhl.com/scores/htmlreports/20182019/ES020001.HTM

## NHL API:
# events source:  https://statsapi.web.nhl.com/api/v1/game/2018020001/feed/live?site=en_nhl
# shifts source:  http://www.nhl.com/stats/rest/shiftcharts?cayenneExp=gameId=2018020001
# shifts charts:  http://www.nhl.com/stats/shiftcharts?id=2018020001  *** (for viewing)
# schedule source: https://statsapi.web.nhl.com/api/v1/schedule?startDate=2018-10-03&endDate=2018-10-03

## ESPN links:
# ESPN game IDs source: http://www.espn.com/nhl/scoreboard?date=20181003
# ESPN XML source:      http://www.espn.com/nhl/gamecast/data/masterFeed?lang=en&isAll=true&rand=0&gameId=401044320
# ESPN events check:    http://www.espn.com/nhl/playbyplay/_/gameId/401044320


## ------------------------------------ ##
##   Notes for sc.scrape_pbp function   ##
## ------------------------------------ ##
#
# This function is used to scrape one or more games from the NHL's publicly available data
# A list is returned with data that is requested
# Example:
# pbp_scrape <- sc.scrape_pbp(games = c("2018020001", "2018020002"))
#
#
## 'games':
# a vector of full NHL game IDs (one or more may be provided)
# example: 2018020001
#
#
## 'scrape_type' options available:
# "full" - all data returned
# "event_summary" - only event summary, rosters, and scratches information returned
# "rosters" - only rosters and scratches information returned
# default is "full"
#
#
## 'live_scrape' call:
# FALSE = function adjusts incorrect player & goalie shifts
# TRUE = function does not adjust incorrect player & goalie shifts (this should be used when scraping games that are in progress)
# default is FALSE
#
#
## 'verbose'
# TRUE = print the system time for each game that is scraped
# default is TRUE
#
#
## 'sleep':
# time to wait between each game being scraped (in seconds)
# default is 0
#
#
#
## ------------------ ##
##   Example Scrape   ##
## ------------------ ##
#
# *** Scrape the first 100 games from the 20182019 regular season
#
# games_vec <- c(as.character(seq(2018020001, 2018020100, by = 1)))
#
# pbp_scrape <- sc.scrape_pbp(games = games_vec)
#
## Pull out of list
# game_info_df_new <-     pbp_scrape$game_info_df               ## game information data
# pbp_base_new <-         pbp_scrape$pbp_base                   ## main play-by-play data
# pbp_extras_new <-       pbp_scrape$pbp_extras                 ## extra play-by-play data
# player_shifts_new <-    pbp_scrape$player_shifts              ## full player shifts data
# player_periods_new <-   pbp_scrape$player_periods             ## player TOI sums per period (from the shifts source)
# roster_df_new <-        pbp_scrape$roster_df                  ## roster data
# scratches_df_new <-     pbp_scrape$scratches_df               ## scratches data
# event_summary_df_new <- pbp_scrape$events_summary_df          ## event summary data (box score stats, etc.)
# scrape_report <-        pbp_scrape$report                     ## report showing number of rows and time to scrape game
#
#
## Get API info
# player_info_df_new <- sc.player_info_API(season_id_fun = "20182019")
#
## Join NHL player IDs with roster data
# roster_df_new_add <- roster_df_new %>%
#   left_join(player_info_df_new %>% select(player, NHL_ID, birthday),
#             by = "player"
#            ) %>%
#   data.frame()
#
#
## Add additional information to pbp data
# pbp_expand <- sc.pbp_expand(data = pbp_base_new)
# pbp_expand <- sc.pbp_index(data = pbp_expand)
#
#
#
## -------------------------------- ##
##   sc.scrape_schedule() Example   ##
## -------------------------------- ##
#
## Get yesterday's schedule
# schedule_current <- sc.scrape_schedule(start_date = Sys.Date() - 1,
#                                        end_date =   Sys.Date() - 1)
#
