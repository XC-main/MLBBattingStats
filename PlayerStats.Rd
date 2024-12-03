\name{PlayerStats}
\alias{PlayerStats}
\title{PlayerStats}
\description{This function retreives the batting average, hits, home runs,
    runs batted in, slugging percentage, and on base percentage of a selected player from within th dataset}
\examples{
mlb.player.stats(MLBhitstats, "Manny Machado")

## The function is currently defined as
function (x)
{mlb.player.stats <- function(data, player_name) {
  player_data <- data %>% filter(Name == player_name)
  selectedplayerstats <- data.frame(
    Statistic = c("Batting Average", "Hits", "Home Runs", "Runs Batted In", "Slugging Percentage", "On Base Percentage"),
    Value = c(player_data$AVG, player_data$H, player_data$HR, player_data$RBI, player_data$SLG, player_data$OBP)
  )

  return(selectedplayerstats)
}
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
