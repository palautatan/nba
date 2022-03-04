# Load in data ----------------------------------------------------------------
az_players    <- read_csv('az-players-dataset.csv')
pergame_stats <- read_csv('pergame-stats.csv')


# Player selection ------------------------------------------------------------
desired_players <- c('Steve Kerr', 'Gary Payton II', 'Stephen Curry', 'Damion Lee',
                     'Chris Chiozza', 'Moses Moody', 'Quinndary Weatherspoon', 'Klay Thompson')
desired_players <- ordered(desired_players, levels=desired_players)

joined_table <- az_players %>%
  filter(Player %in% desired_players) %>%
  select(Player, Ht, Wt) 

# Summarize data --------------------------------------------------------------
table_a <- joined_table %>%
  left_join(pergame_stats) %>%
  filter(!grepl('did not play', tolower(Tm))) %>%
  mutate(Player=ordered(Player, levels=desired_players)) %>%
  filter(Age %in% seq(24,30,2)) %>%
  mutate(Age=as.factor(Age)) %>%
  group_by(Age, Player) %>%
  summarize(Player=unique(Player),
            G=median(as.numeric(G), na.rm=TRUE),
            `2P`=median(as.numeric(X2P), na.rm=TRUE),
            `3P`=median(as.numeric(X3P), na.rm=TRUE),
            AST=median(as.numeric(AST, na.rm=TRUE))) %>%
  ungroup() %>%
  arrange(Age, Player)

# Format table ----------------------------------------------------------------
reshape_table_a <- lapply(seq(24,30,2), function(k) {
  tmp <- table_a %>%
    filter(Age==k) %>%
    select(-Age) %>%
    t()
  
  tmp2 <- data.frame(cbind(Age=k, Attribute=rownames(tmp), tmp))
  
  names(tmp2) <- c('Age', 'Attribute', tmp[1,])
  tmp2 <- tmp2[-1,]
  
})

table_a_clean <- do.call(bind_rows, reshape_table_a)
row.names(table_a_clean) <- 1:nrow(table_a_clean)


# Add in heights and weights --------------------------------------------------
htwt_tmp <- joined_table %>%
  mutate(Player=ordered(Player, levels=desired_players)) %>%
  arrange(Player) %>%
  rename(Height=Ht) %>%
  rename(Weight=Wt)
  
htwt_tmp$Height <- sapply(strsplit(htwt_tmp$Height, split='-'), function(m) {
  as.numeric(m[1])*12 + as.numeric(m[2])
})
  
htwt_tmp <- htwt_tmp %>%
  t() %>%
  data.frame()

names(htwt_tmp) <- htwt_tmp[1,]  
htwt_tmp <- htwt_tmp[-1,]
htwt_tmp <- cbind(Age='A', Attribute=c('Height', 'Weight'), htwt_tmp)
htwt_tmp <- tibble(htwt_tmp)


# Final formatting ------------------------------------------------------------
table_a_final <- bind_rows(table_a_clean, htwt_tmp) %>%
  mutate(Age=ordered(Age, levels=c('A', seq(24, 30, 2)))) %>%
  arrange(Age, Attribute) %>%
  mutate_at(.vars=names(table_a_final)[3:ncol(table_a_final)],
            .funs=function(k) ifelse(is.na(k), '', k))


write_csv(table_a_final, file='answer-key-table.csv')


# Scoring similarities --------------------------------------------------------

scores <- table_a_final %>%
  filter(Age %in% c('A', 24)) %>%
  select(-`Damion Lee`, -`Moses Moody`) %>%
  mutate(SC=as.numeric(`Stephen Curry`)-as.numeric(`Steve Kerr`)) %>%
  mutate(GP2=as.numeric(`Gary Payton II`)-as.numeric(`Steve Kerr`)) %>%
  mutate(CC=as.numeric(`Chris Chiozza`)-as.numeric(`Steve Kerr`)) %>%
  mutate(QW=as.numeric(`Quinndary Weatherspoon`)-as.numeric(`Steve Kerr`)) %>%
  mutate(KT=as.numeric(`Klay Thompson`)-as.numeric(`Steve Kerr`)) %>%
  select(Age, Attribute, SC, GP2, CC, QW, KT) %>%
  filter(Attribute != 'G')
  
  
scores_tmp <- scores[,3:ncol(scores)]
scores_tmp2 <- apply(scores_tmp, 1, function(k) {
  (k-mean(k))^2/sd(k)
}) %>% t()

colSums(scores_tmp2)
