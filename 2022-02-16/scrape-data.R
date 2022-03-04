library(rvest)
library(tidyverse)
library(lubridate)
library(stringr)

home_url    <- 'https://www.basketball-reference.com'
players_url <- paste0(home_url, '/players')

webpage <- read_html(players_url)
results_df_v1 <- cbind(hover=webpage %>% html_nodes('a') %>% html_text,
                       url=webpage %>% html_nodes('a') %>% html_attr('href')) %>% data.frame()

results_df_v2 <- results_df_v1 %>%
  filter(nchar(hover)==1) %>%
  mutate(full_url=paste0(home_url, url))

az_tables <- lapply(results_df_v2 %>% pull(full_url), function(k) {
  # Read webpage.
  webpage_k <- read_html(k)
  
  # Grab table and clean player name.
  table_k <- webpage_k %>% html_nodes('table')
  table_k <- html_table(table_k)[[1]]
  table_k <- table_k %>%
    mutate(Player=gsub('\\*', '', Player)) %>%
    mutate(HOF=ifelse(grepl('\\*', Player), 1, 0))
  
  # Associate links to dataframe.
  links_k <- cbind(hover=webpage_k %>% html_nodes('a') %>% html_text,
                   url=webpage_k %>% html_nodes('a') %>% html_attr('href')) %>% data.frame()
  
  table_url_k <- links_k %>%
    filter(hover %in% table_k$Player) %>%
    rename(Player=hover) %>%
    right_join(table_k)
  
  return(table_url_k)
})


az_df <- do.call(rbind, az_tables)
# write_csv(az_df, file='../2022-02-16/az-player-table.csv')


# Scrape player pages ---------------------------------------------------------
player_info  <- vector(mode='list', length=nrow(az_df))
pergame_info <- vector(mode='list', length=nrow(az_df))

# k <- 2395
for (k in 178:nrow(az_df)) {
  
  print(paste0(k, ': ', az_df[k, 'Player']))
  
  player_url_k   <- paste0(home_url, az_df[k,'url'])
  
  if (grepl(pattern='executives|coaches', player_url_k)) {
    pergame_df_exec <- data.frame(matrix(ncol=30, nrow=0))
    colnames(pergame_df_exec) <- c('Season', 'Age', 'Tm', 'Lg', 'Pos', 'G', 'GS', 'MP', 'FG',
                           'FGA', 'FG.', 'X3P', 'X3PA', 'X3P.', 'X2P', 'X2PA', 'X2P.', 'eFG.',
                           'FT', 'FTA', 'FT.', 'ORB', 'DRB', 'TRB', 'AST', 'STL', 'BLK', 'TOV',
                           'PF', 'PTS')
    pergame_info[[k]] <- pergame_df_exec
    
    player_info_exec <- data.frame(matrix(ncol=7, nrow=0))
    colnames(player_info_exec) <- c('Player',  'Shoots', 'Height', 'Weight', 'Origin', 'Debut', 'Position')
    player_info[[k]]  <- player_info_exec
    
    next
  }
  
  player_site_k  <- read_html(player_url_k)
  
  # 1. Per-game stats.
  player_table_k <- player_site_k %>% html_nodes('table#per_game')
  player_pg_k <- html_table(player_table_k[1]) %>%
    data.frame() %>%
    filter(!is.na(Age))
  
  if (nrow(player_pg_k)>0) {
    # Bind player name if there is a table of NBA stats.
    pergame_info[[k]] <- cbind(Player=az_df[k, 'Player'], player_pg_k)
  } else {
    # Sometimes, there is no NBA stats. Out-of-league play.
    pergame_info[[k]] <- player_pg_k
  }
  
  
  
  # 2. Player info.
  tmp <- player_site_k %>% html_node('div#info') %>% html_children()
  
  ## Shooting hand.
  shoots_ix <- tmp[1] %>%
    html_nodes('p') %>%
    html_text() %>%
    grepl(pattern='Shoots') %>%
    which()
  
  shoots <- tmp[1] %>%
    html_nodes('p') %>%
    `[`(shoots_ix) %>%
    html_text() %>%
    strsplit(split='Shoots:') %>%
    unlist() %>%
    `[`(2) %>%
    trimws()
  
  ## Birthplace.
  born_ix <- tmp[1] %>%
    html_nodes('p') %>%
    html_text() %>%
    grepl(pattern='Born') %>%
    which()
  
  born <- tmp[1] %>%
    html_nodes('p') %>%
    `[`(born_ix) %>%
    html_text() %>%
    strsplit(split='in') %>%
    unlist() %>%
    `[`(2) %>%
    trimws() %>%
    strsplit(split='\n') %>%
    unlist %>%
    `[`(1) %>%
    str_trim()
  
  ## Size.
  height <- tmp[1] %>%
    html_nodes("span[itemprop='height']") %>%
    html_text()
  
  weight <- tmp[1] %>%
    html_nodes("span[itemprop='weight']") %>%
    html_text()
  
  ## Debut date.
  debut_ix <- tmp[1] %>%
    html_nodes('p') %>%
    html_text() %>%
    grepl(pattern='NBA Debut') %>%
    which()
  
  debut <- tmp[1] %>%
    html_nodes('p') %>%
    `[`(debut_ix) %>%
    html_node('a') %>%
    html_text()
  
  ## Position(s).
  
  positions_ix <- tmp[1] %>%
    html_nodes('p') %>%
    html_text() %>%
    grepl(pattern='Position') %>%
    which()
  
  positions <- tmp[1] %>%
    html_nodes('p') %>%
    `[`(positions_ix) %>%
    html_text() %>%
    strsplit(split='Position:|Shoots:') %>%
    unlist() %>%
    `[`(2) %>%
    str_replace_all(pattern='\\n|[^[:alnum:]]', replacement=' ') %>%
    str_trim()
  
  player_info_k <- cbind(Player=az_df[k, 'Player'], Shoots=shoots, Height=height,
                         Weight=weight, Origin=born, Debut=debut, Position=positions) %>%
    data.frame()
  
  player_info[[k]] <- player_info_k
  
  # Slow down the downloads.
  Sys.sleep(0.5)
  
}



# Convert dataframe a-z players pt.2 ------------------------------------------
player_info_v2  <- lapply(player_info, function(k) {
  if (nrow(k)>0) k else NULL
})

player_info_df <- do.call(bind_rows, player_info_v2)

tmp <- left_join(az_df, player_info_df)
# anti_join(y=az_df, player_info_df)

# write_csv(player_info_df, file='../2022-02-16/player-info-df.csv')
# write_csv(tmp, file='../2022-02-16/az-players-dataset.csv')




# Convert dataframe per-game statistics ---------------------------------------

# Mutate everything to character so we can bind the rows.
pergame_dfs <- sapply(pergame_info, function(k) {
  if (!is.null(k)) {
    k2 <- tibble(k)
    k2 %>% mutate_all(.funs='as.character')
  } else {
    NULL
  }
})

pergame_info_df <- do.call(dplyr::bind_rows, pergame_dfs)
pergame_info_df_v2 <- pergame_info_df %>%
  group_by(Player) %>%
  mutate(TradeSequence=1:n())

# write_csv(pergame_info_df_v2, file='../2022-02-16/pergame-stats.csv')





# az_df[k,]
# player_info[[k]]
# pergame_info[[k]]
# player_info_df[k-3,]
# pergame_info_df_v2[k,]
