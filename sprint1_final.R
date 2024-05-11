library(tidyverse)
library(gapminder)
library(dplyr)
library(rvest)

batting_link <- "https://www.mlb.com/stats/"
batting_page <- batting_link %>% read_html()

names <- batting_page %>% html_nodes(".full-3fV3c9pF") %>% html_text()
#names
full_name <- paste(names[seq(1, length(names), by = 2)], names[seq(2, length(names), by = 2)])
#full_name

position <- batting_page %>% html_nodes(".position-28TbwVOg") %>% html_text()
#position

team <- batting_page %>% html_nodes(".align-left-3L2SU-Mk") %>% html_text() %>% .[-1]

stats <- batting_page %>% html_nodes(".bui-table td") %>% html_text()
for (x in seq_along(stats)) {
  if (stats[x] %in% team) {
    stats[x] <- NA
  }
}
stats <- na.omit(stats)
#stats

batting_stats <- data.frame(
  G = stats[seq(1, length(stats), by = 16)],
  AB = stats[seq(2, length(stats), by = 16)],
  Run = stats[seq(3, length(stats), by = 16)],
  Hits = stats[seq(4, length(stats), by = 16)],
  `2B` = stats[seq(5, length(stats), by = 16)],
  `3B` = stats[seq(6, length(stats), by = 16)],
  HR = stats[seq(7, length(stats), by = 16)],
  RBI = stats[seq(8, length(stats), by = 16)],
  BB = stats[seq(9, length(stats), by = 16)],
  SO = stats[seq(10, length(stats), by = 16)],
  SB = stats[seq(11, length(stats), by = 16)],
  CS = stats[seq(12, length(stats), by = 16)],
  AVG = stats[seq(13, length(stats), by = 16)],
  OBP = stats[seq(14, length(stats), by = 16)],
  SLG = stats[seq(15, length(stats), by = 16)],
  OPS = stats[seq(16, length(stats), by = 16)],
  stringsAsFactors = FALSE
)
#View(batting_stats)

player <- data.frame(full_name, position, team, stringsAsFactors = FALSE)
#View(player)

full_table <- cbind(player, batting_stats)
#View(full_table)

pitching_link <- "https://www.mlb.com/stats/pitching?sortState=asc"
pitching_page <- pitching_link %>% read_html()

pitcher_name <- pitching_page %>% html_nodes(".bui-link span") %>% html_text()
pitcher_name[1:2] <- NA
pitcher_name[seq(4, length(pitcher_name), by = 3)] <- NA
pitcher_name <- na.omit(pitcher_name)
pitcher_full_name <- paste(pitcher_name[seq(1, length(pitcher_name), by = 2)], pitcher_name[seq(2, length(pitcher_name), by = 2)])
#pitcher_full_name

pitching_stats <- pitching_page %>% html_nodes(".table-scroller-2FeRJsQr table td") %>% html_text()
pitching_players <- data.frame(
  full_name = pitcher_full_name,
  Team = pitching_stats[seq(1, length(pitching_stats), by = 20)], 
  W = pitching_stats[seq(2, length(pitching_stats), by = 20)],
  L = pitching_stats[seq(3, length(pitching_stats), by = 20)],
  ERA = pitching_stats[seq(4, length(pitching_stats), by = 20)],
  G = pitching_stats[seq(5, length(pitching_stats), by = 20)],
  GS = pitching_stats[seq(6, length(pitching_stats), by = 20)],
  CG = pitching_stats[seq(7, length(pitching_stats), by = 20)],
  SHO = pitching_stats[seq(8, length(pitching_stats), by = 20)],
  SV = pitching_stats[seq(9, length(pitching_stats), by = 20)],
  SVO = pitching_stats[seq(10, length(pitching_stats), by = 20)],
  IP = pitching_stats[seq(11, length(pitching_stats), by = 20)],
  H = pitching_stats[seq(12, length(pitching_stats), by = 20)],
  R = pitching_stats[seq(13, length(pitching_stats), by = 20)],
  ER = pitching_stats[seq(14, length(pitching_stats), by = 20)],
  HR = pitching_stats[seq(15, length(pitching_stats), by = 20)],
  HB = pitching_stats[seq(16, length(pitching_stats), by = 20)],
  BB = pitching_stats[seq(17, length(pitching_stats), by = 20)],
  SO = pitching_stats[seq(18, length(pitching_stats), by = 20)],
  WHIP = pitching_stats[seq(19, length(pitching_stats), by = 20)],
  AVG = pitching_stats[seq(20, length(pitching_stats), by = 20)]
)
#View(pitching_players)

batting_links <- function(links){
  new_page <- paste("https://www.mlb.com/stats/?page=", links, sep = "") %>% read_html()
  new_names <- new_page %>% html_nodes(".full-3fV3c9pF") %>% html_text()
  new_full_name <- paste(new_names[seq(1, length(new_names), by = 2)], new_names[seq(2, length(new_names), by = 2)])
  new_position <- new_page %>% html_nodes(".position-28TbwVOg") %>% html_text()
  new_team <- new_page %>% html_nodes(".align-left-3L2SU-Mk") %>% html_text() %>% .[-1]
  new_stats <- new_page %>% html_nodes(".bui-table td") %>% html_text()

  for (x in seq_along(new_stats)) {
    if (new_stats[x] %in% new_team) {
      new_stats[x] <- NA
    }
  }
  new_stats <- na.omit(new_stats)
  
  new_batting_stats <- data.frame(
    G = new_stats[seq(1, length(new_stats), by = 16)],
    AB = new_stats[seq(2, length(new_stats), by = 16)],
    Run = new_stats[seq(3, length(new_stats), by = 16)],
    Hits = new_stats[seq(4, length(new_stats), by = 16)],
    `2B` = new_stats[seq(5, length(new_stats), by = 16)],
    `3B` = new_stats[seq(6, length(new_stats), by = 16)],
    HR = new_stats[seq(7, length(new_stats), by = 16)],
    RBI = new_stats[seq(8, length(new_stats), by = 16)],
    BB = new_stats[seq(9, length(new_stats), by = 16)],
    SO = new_stats[seq(10, length(new_stats), by = 16)],
    SB = new_stats[seq(11, length(new_stats), by = 16)],
    CS = new_stats[seq(12, length(new_stats), by = 16)],
    AVG = new_stats[seq(13, length(new_stats), by = 16)],
    OBP = new_stats[seq(14, length(new_stats), by = 16)],
    SLG = new_stats[seq(15, length(new_stats), by = 16)],
    OPS = new_stats[seq(16, length(new_stats), by = 16)],
    stringsAsFactors = FALSE
  )
  
  new_player <- data.frame(new_full_name, new_position, new_team, stringsAsFactors = FALSE)
  new_full_table <- cbind(new_player, new_batting_stats)
  return(new_full_table)
}

pitching_stats <- function(links){
  new_pitching_link <- paste("https://www.mlb.com/stats/pitching?page=", links, "&sortState=asc", sep = "")
  new_pitching_page <- new_pitching_link %>% read_html()
  new_pitcher_name <- new_pitching_page %>% html_nodes(".bui-link span") %>% html_text()
  new_pitcher_name[1:2] <- NA
  new_pitcher_name[seq(4, length(new_pitcher_name), by = 3)] <- NA
  new_pitcher_name <- na.omit(new_pitcher_name)
  new_pitcher_full_name <- paste(new_pitcher_name[seq(1, length(new_pitcher_name), by = 2)], new_pitcher_name[seq(2, length(new_pitcher_name), by = 2)])
  #new_pitcher_full_name
  
  new_pitching_stats <- new_pitching_page %>% html_nodes(".table-scroller-2FeRJsQr table td") %>% html_text()
  new_pitching_players <- data.frame(
    full_name = new_pitcher_full_name,
    Team = new_pitching_stats[seq(1, length(new_pitching_stats), by = 20)], 
    W = new_pitching_stats[seq(2, length(new_pitching_stats), by = 20)],
    L = new_pitching_stats[seq(3, length(new_pitching_stats), by = 20)],
    ERA = new_pitching_stats[seq(4, length(new_pitching_stats), by = 20)],
    G = new_pitching_stats[seq(5, length(new_pitching_stats), by = 20)],
    GS = new_pitching_stats[seq(6, length(new_pitching_stats), by = 20)],
    CG = new_pitching_stats[seq(7, length(new_pitching_stats), by = 20)],
    SHO = new_pitching_stats[seq(8, length(new_pitching_stats), by = 20)],
    SV = new_pitching_stats[seq(9, length(new_pitching_stats), by = 20)],
    SVO = new_pitching_stats[seq(10, length(new_pitching_stats), by = 20)],
    IP = new_pitching_stats[seq(11, length(new_pitching_stats), by = 20)],
    H = new_pitching_stats[seq(12, length(new_pitching_stats), by = 20)],
    R = new_pitching_stats[seq(13, length(new_pitching_stats), by = 20)],
    ER = new_pitching_stats[seq(14, length(new_pitching_stats), by = 20)],
    HR = new_pitching_stats[seq(15, length(new_pitching_stats), by = 20)],
    HB = new_pitching_stats[seq(16, length(new_pitching_stats), by = 20)],
    BB = new_pitching_stats[seq(17, length(new_pitching_stats), by = 20)],
    SO = new_pitching_stats[seq(18, length(new_pitching_stats), by = 20)],
    WHIP = new_pitching_stats[seq(19, length(new_pitching_stats), by = 20)],
    AVG = new_pitching_stats[seq(20, length(new_pitching_stats), by = 20)]
  )
  return(new_pitching_players)
}


links <- batting_page %>% html_nodes(".bui-button.tab-27nhZTIl span") %>% html_text()
links <- links[-1:-3]
#links

if (1 %in% links) {
  links <- links[links != 1]
}

#View(full_table)

for (x in links) {
  new_full_table <- batting_links(x)
  names(new_full_table) <- names(full_table)
  #View(new_full_table)
  full_table <- rbind(full_table, new_full_table)
}

pitching_pagenums <- pitching_page %>% html_nodes(".bui-button.tab-27nhZTIl .button-3wq5VxsJ.sm-1_yhMOW5 span") %>% html_text()
pitching_pagenums <- pitching_pagenums[-1:-3]
#pitching_pagenums
for (x in pitching_pagenums){
  new_table <- pitching_stats(x)
  pitching_players <- rbind(pitching_players, new_table)
}

#View(pitching_players)
#View(full_table)

#Which team has the best batters based on OBP?
full_table$OBP <- as.numeric(full_table$OBP)
#summary(full_table$OBP)
ggplot(full_table, aes(x = team, y = OBP)) +
  geom_boxplot(fill = "skyblue", color = "blue") +
  geom_hline(yintercept = mean(full_table$OBP), color = "red", linetype = "dashed") +
  labs(title = "Distribution of OBP by Team",
       x = "Team",
       y = "OBP") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Which team has the best pitchers based on ERA?
pitching_players$ERA <- as.numeric(pitching_players$ERA)
#summary(pitching_players$ERA)
ggplot(pitching_players, aes(x = Team, y = ERA)) +
  geom_boxplot(fill = "skyblue", color = "blue") +
  geom_hline(yintercept = mean(pitching_players$ERA, na.rm = TRUE), color = "red", linetype = "dashed") +
  labs(title = "Distribution of ERA by Team",
       x = "Team",
       y = "ERA") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
