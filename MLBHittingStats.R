library(dplyr)
library(ggplot2)
MLBhitstats <- read.csv("batting_2023_data.csv")
str(MLBhitstats)

mlb.player.stats <- function(data, player_name) {
  player_data <- data %>% filter(Name == player_name)
  selectedplayerstats <- data.frame(
    Statistic = c("Batting Average", "Hits", "Home Runs", "Runs Batted In", "Slugging Percentage", "On Base Percentage"),
    Value = c(player_data$AVG, player_data$H, player_data$HR, player_data$RBI, player_data$SLG, player_data$OBP)
  )

  return(selectedplayerstats)
}


mlb.player.comparison <- function(data, player1, player2) {
  player1_data <- data %>% filter(Name == player1)
  player2_data <- data %>% filter(Name == player2)
  comparison <- data.frame(
    Metric = c("Batting Average", "Hits", "Home Runs", "Runs Batted In", "Slugging Percentage", "On Base Percentage"),
    Player1 = c(player1_data$AVG, player1_data$H, player1_data$HR, player1_data$RBI, player1_data$SLG, player1_data$OBP),
    Player2 = c(player2_data$AVG, player2_data$H, player2_data$HR, player2_data$RBI, player2_data$SLG, player2_data$OBP)
  )

  colnames(comparison)[2:3] <- c(player1, player2)

  return(comparison)
}


mlb.player.top <- function(data) {
  stats_and_labels <- list(
    "AVG" = "Batting Average",
    "H" = "Hits",
    "HR" = "Home Runs",
    "RBI" = "Runs Batted In",
    "SLG" = "Slugging Percentage",
    "OBP" = "On Base Percentage"
  )
  results <- data.frame(Metric = character(), Player = character(), Value = numeric(), stringsAsFactors = FALSE)
  for (stat in names(stats_and_labels)) {
    stat_label <- stats_and_labels[[stat]]
    top_player <- data[which.max(data[[stat]]), ]
    results <- rbind(results, data.frame(Metric = stat_label, Player = top_player$Name, Value = top_player[[stat]]))
  }

  return(results)
}

