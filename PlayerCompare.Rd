\name{PlayerCompare}
\alias{PlayerCompare}
\title{
MLB Player Hitting Stats Comparison
}
\description{
This function compares the batting average, hits, home runs,
    runs batted in, slugging percentage, and on base percentage between two players by displaying them side by side.
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or standard data sets, see data().
mlb.player.comparison(MLBhitstats,"Manny Machado","Aaron Judge")
## The function is currently defined as
function (x)
{mlb.player.comparison <- function(data, player1, player2) {
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
