#' Summarize the win/loss record and winning percentage for a specific team.
#'
#' This function takes a tibble containing game data and a specific team name
#' and computes the win/loss record and winning percentage for that team.
#' It filters the game data to include only matches where the specified team
#' was either Team1 or Team2, calculates whether the team won or lost each match,
#' and then summarizes the total number of wins, losses, and the winning percentage.
#'
#' @param tibble A tibble containing game data with columns Team1, Score1, Team2, and Score2
#' representing the two teams playing and their respective scores.
#' @param team A character string specifying the name of the team for which to compute
#' the win/loss record and winning percentage.
#'
#' @return A tibble summarizing the win/loss record and winning percentage for the specified team.
#' The tibble includes columns for the total number of wins, losses, winning percentage, and the team name.
#'
#' @examples
#' # Load necessary packages
#' library(dplyr)
#'
#' # Create sample game data
#' game_data <- tibble(
#'   Team1 = c("Team A", "Team B", "Team A"),
#'   Score1 = c(80, 75, 85),
#'   Team2 = c("Team C", "Team A", "Team B"),
#'   Score2 = c(75, 70, 70)
#' )
#'
#' # Compute the win/loss record and winning percentage for "Team A"
#' team_win_loss(game_data, "Team A")
#'
#' @importFrom dplyr filter mutate summarize
#'
#' @export

team_win_loss <- function(tibble, team) {
  team_games <- filter(tibble, Team1 == team | Team2 == team)
  team_games <- mutate(team_games, Win_Loss = case_when(
    Team1 == team & as.numeric(Score1) > as.numeric(Score2) ~ "Win",
    Team1 == team & as.numeric(Score1) < as.numeric(Score2) ~ "Loss",
    Team2 == team & as.numeric(Score2) > as.numeric(Score1) ~ "Win",
    Team2 == team & as.numeric(Score2) < as.numeric(Score1) ~ "Loss"
  ))
  team_summary <- summarize(team_games, Wins = sum(Win_Loss == "Win"), Losses = sum(Win_Loss == "Loss"), Win_Percent = mean(Win_Loss == "Win"))
  team_summary <- mutate(team_summary, Team = team)
  return(team_summary)
}
