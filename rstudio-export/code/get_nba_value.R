get_nba_value <- function(player, attribute, game="all") {
  if (game==1) {
    y <- as.numeric(as.character(gsw_1 %>% filter(Player==player) %>% pull(attribute)))
  }
  
  if (game==2) {
    y <- as.numeric(as.character(gsw_2 %>% filter(Player==player) %>% pull(attribute)))
  }
  
  if (game==3) {
    y <- as.numeric(as.character(gsw_3 %>% filter(Player==player) %>% pull(attribute)))
  }
  
  if (game==4) {
    y <- as.numeric(as.character(gsw_4 %>% filter(Player==player) %>% pull(attribute)))
  }
  
  if (game=="all") {
    y <- as.numeric(as.character(gsw_1 %>% filter(Player==player) %>% pull(attribute))) +
      as.numeric(as.character(gsw_2 %>% filter(Player==player) %>% pull(attribute))) +
      as.numeric(as.character(gsw_3 %>% filter(Player==player) %>% pull(attribute))) +
      as.numeric(as.character(gsw_4 %>% filter(Player==player) %>% pull(attribute)))
  }
  y
}

get_nba_value("Klay Thompson", "X3P")
get_nba_value("Klay Thompson", "X3PA")

get_nba_value("Team Totals", "X3P")
get_nba_value("Team Totals", "X3PA")


get_nba_value("Klay Thompson", "PTS")
get_nba_value("Team Totals", "PTS")




get_nba_value("Team Totals", "X3P")
get_nba_value("Stephen Curry", "X3P")
get_nba_value("Klay Thompson", "X3P")
get_nba_value("Kevin Durant", "X3P")
get_nba_value("Draymond Green", "X3P")


get_nba_value("Team Totals", "PTS")
get_nba_value("Stephen Curry", "PTS")
get_nba_value("Klay Thompson", "PTS")
get_nba_value("Kevin Durant", "PTS")
get_nba_value("Draymond Green", "PTS")

get_nba_value("Team Totals", "AST")
get_nba_value("Kevin Durant", "AST")
get_nba_value("Draymond Green", "AST")
get_nba_value("Klay Thompson", "AST")
get_nba_value("Stephen Curry", "AST")

get_nba_value("Draymond Green", "AST") / get_nba_value("Team Totals", "AST")


get_nba_value("Team Totals", "AST", game=1)
get_nba_value("Team Totals", "AST", game=2)
