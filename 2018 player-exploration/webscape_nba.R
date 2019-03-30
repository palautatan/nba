players_url = "https://www.basketball-reference.com/players/"
home <- "https://www.basketball-reference.com"

library(rvest)

webpage <- read_html(players_url)
webpage


results_df <- data.frame(
  cbind(
    hover=webpage %>% html_nodes("a") %>% html_text,
    url=webpage %>% html_nodes("a") %>% html_attr("href")
    )
)

letter_pages <- results_df[which(as.character(results_df[,1]) %in% LETTERS),]
letter_pages[,2] <- paste0(home, letter_pages[,2])
letter_pages

letter_urls <- letter_pages[,2]
letter_urls

letter_webpage <- read_html(letter_urls[1]) 

this_letter_df <- data.frame(
  cbind(
    hover=letter_webpage %>% html_nodes("a") %>% html_text,
    url=letter_webpage %>% html_nodes("a") %>% html_attr("href")
  )
)




all_players <- paste0(home, results)

for (this_player in all_players) {
  webpage_2 <- read_html(this_player)
  results_2 <- webpage_2 %>% html_nodes("table")
  results_2
  
  pg_table <- html_table(results_2)[[1]]
  fg_percentage <- pg_table[which(pg_table$Age=="23"),which(colnames(pg_table)=="FG%")]
  
}
