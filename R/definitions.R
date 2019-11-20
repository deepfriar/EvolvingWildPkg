## Dead NHL games (html source)
dead_games <- c(
  "2007020011", "2007021178",
  "2008020259", "2008020409", "2008021077", "2008030311",
  "2009020081", "2009020658", "2009020885",
  "2010020124"
)

## Create Team IDs (to use for API team triCodes)
Team_ID <-
  data.frame(
    Team =
      c("N.J", "NYI", "NYR", "PHI", "PIT", "BOS", "BUF", "MTL", "OTT", "TOR", "ATL", "CAR", "FLA", "T.B",
        "WSH", "CHI", "DET", "NSH", "STL", "CGY", "COL", "EDM", "VAN", "ANA", "DAL", "L.A", "ARI", "S.J",
        "CBJ", "MIN", "WPG", "ARI", "VGK"
      ),
    ID = c(seq(1:33))
  ) %>%
  dplyr::mutate(ID = ifelse(ID == 31, 52,
                            ifelse(ID == 32, 53,
                                   ifelse(ID == 33, 54, ID))))

## For identifying event_team in HTM events
Team_ID_vec <- c(
  "ANA", "ARI", "BOS", "BUF", "CAR", "CBJ", "CGY", "CHI", "COL", "DAL", "DET", "EDM", "FLA", "L.A", "MIN",
  "MTL", "N.J", "NSH", "NYI", "NYR", "OTT", "PHI", "PIT", "S.J", "STL", "T.B", "TOR", "VAN", "WPG", "WSH",
  "PHX", "ATL", "VGK", "L.V"
)

full_team_names <-
  data.frame(
    fullTeam =
      c("ANAHEIM DUCKS", "ARIZONA COYOTES", "ATLANTA THRASHERS", "BOSTON BRUINS", "BUFFALO SABRES", "CALGARY FLAMES", "CAROLINA HURRICANES",
        "CHICAGO BLACKHAWKS", "COLORADO AVALANCHE", "COLUMBUS BLUE JACKETS", "DALLAS STARS", "DETROIT RED WINGS", "EDMONTON OILERS",
        "FLORIDA PANTHERS", "LOS ANGELES KINGS", "MINNESOTA WILD", "MONTREAL CANADIENS", "CANADIENS MONTREAL", "NASHVILLE PREDATORS", "NEW JERSEY DEVILS",
        "NEW YORK ISLANDERS", "NEW YORK RANGERS", "OTTAWA SENATORS", "PHILADELPHIA FLYERS", "PHOENIX COYOTES", "PITTSBURGH PENGUINS", "SAN JOSE SHARKS",
        "ST. LOUIS BLUES", "TAMPA BAY LIGHTNING", "TORONTO MAPLE LEAFS", "VANCOUVER CANUCKS", "VEGAS GOLDEN KNIGHTS", "WASHINGTON CAPITALS",
        "WINNIPEG JETS"
      ),
    Team =
      c("ANA", "ARI", "ATL", "BOS", "BUF", "CGY", "CAR", "CHI", "COL", "CBJ", "DAL", "DET", "EDM", "FLA", "L.A",
        "MIN", "MTL", "MTL", "NSH", "N.J", "NYI", "NYR", "OTT", "PHI", "ARI", "PIT", "S.J", "STL", "T.B", "TOR", "VAN", "VGK", "WSH", "WPG"
      ),
    partTeam =
      c("DUCKS", "COYOTES", "THRASHERS", "BRUINS", "SABRES", "FLAMES", "HURRICANES",
        "BLACKHAWKS", "AVALANCHE", "BLUE JACKETS", "STARS", "RED WINGS", "OILERS",
        "PANTHERS", "KINGS", "WILD", "CANADIENS", "MONTREAL", "PREDATORS", "DEVILS",
        "ISLANDERS", "RANGERS", "SENATORS", "FLYERS", "COYOTES", "PENGUINS", "SHARKS",
        "BLUES", "LIGHTNING", "MAPLE LEAFS", "CANUCKS", "GOLDEN KNIGHTS", "CAPITALS",
        "JETS"
      )
  )




## ESPN's team IDs & event type codes
ESPN_team_IDs <-
  data.frame(
    team_ID =
      as.numeric(c(
        "25", "24", "1", "2", "7", "29", "3", "4", "17", "9", "5", "6", "26", "8",
        "30", "10", "11", "27", "12", "13", "14", "15", "16", "18", "19", "20", "21",
        "22", "37", "28", "23"
      )),
    Team =
      c("ANA", "ARI", "BOS", "BUF", "CAR", "CBJ", "CGY", "CHI", "COL", "DAL", "DET", "EDM", "FLA",
        "L.A", "MIN", "MTL", "N.J", "NSH", "NYI", "NYR", "OTT", "PHI", "PIT", "S.J", "STL", "T.B",
        "TOR", "VAN", "VGK", "WPG", "WSH"
      )
  )


ESPN_codes <-
  data.frame(
    event =
      c("FAC", "HIT", "GvTk", "GOAL", "SHOT", "MISS", "BLOCK", "PENL","STOP", "PRDY", "PSTR", "PEND",
        "PERD", "SOC", "GEND", "SOut","error", "TAKE", "GIVE", "early intermission", "nothing", "nothing"
      ),
    code =
      as.character(c(
        502, 503, 504, 505, 506, 507, 508, 509, 516, 517, 518, 519, 520, 521, 522, 0, 9999,
        1401, 1402, -2147483648, 1, 5
      ))
  )


## Other objects
sc.main_events <- c("GOAL", "SHOT", "MISS", "BLOCK", "HIT", "GIVE", "TAKE", "FAC", "PENL")

st.shot_events <-     c("SHOT",  "GOAL")
st.fenwick_events <-  c("SHOT", "GOAL", "MISS")
st.corsi_events <-    c("SHOT", "GOAL", "MISS", "BLOCK" )
st.strength_states <- c("3v3", "5v5", "4v4", "5v4", "4v5", "5v3", "3v5", "4v3", "3v4", "5vE", "Ev5", "4vE", "Ev4", "3vE", "Ev3") %>% as.factor()
st.even_strength <-   c("5v5", "4v4", "3v3") %>% as.factor()
st.pp_strength <-     c("5v4", "4v5", "5v3", "3v5", "4v3", "3v4") %>% as.factor()
st.empty_net <-       c("5vE", "Ev5", "4vE", "Ev4", "3vE", "Ev3") %>% as.factor()
