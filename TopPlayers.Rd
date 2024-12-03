\name{TopPlayers}
\alias{TopPlayers}
\title{
MLB Top Statistical Players
}
\description{
     This function retreives the highest value in batting average, hits, home runs,
     runs batted in, slugging percentage, and on base percentage then returns the player that possess the highest in those stats.
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or standard data sets, see data().
mlb.players.top(MLBhitstats)
## The function is currently defined as
function (x)
{mlb.player.top <- function(data) {
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
