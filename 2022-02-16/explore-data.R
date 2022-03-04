# Arrange players by how long their NBA career was/is.
az_df %>%
  mutate(LOC=To-From) %>%
  arrange(-LOC) %>%
  View()

# Arrange players by how old they were when they started their career.
az_df %>%
  mutate(DOB=mdy(`Birth Date`)) %>%
  mutate(DOE=ymd(paste0(From, '-01-01'))) %>%
  mutate(DOX=ymd(paste0(To, '-01-01'))) %>%
  mutate(AgeStart=interval(DOB, DOE) / years(1)) %>%
  arrange(AgeStart) %>%
  View()