#' Create a ggplot box plot comparing the scores of home and away teams or a selected team's wins and losses.
#'
#' This function takes a tibble containing game data and creates a box plot
#' comparing the scores of home and away teams when no team is specified,
#' or creates a box plot showing the distribution of scores for a selected team's wins and losses.
#'
#' @param tibble A tibble containing game data with columns Score1 and Score2 representing
#' the scores of the home and away teams, respectively.
#' @param team (Optional) A character string specifying the team name for which the box plot should be created.
#' Defaults to NULL.
#'
#' @return A ggplot box plot comparing the scores of home and away teams or a selected team's wins and losses.
#'
#' @examples
#' # Load necessary packages
#' library(tidyverse)
#'
#' # Create sample game data
#' game_data <- tibble(
#'   Score1 = c(2, 3, 1, 2),
#'   Score2 = c(1, 2, 2, 1)
#' )
#'
#' # Create the box plot of home vs. away scores
#' home_vs_away_points(game_data)
#'
#' # Create the box plot of a selected team's wins and losses
#' home_vs_away_points(game_data, "Akron")
#'
#' @importFrom ggplot2 aes geom_boxplot labs theme_minimal
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr if_else filter
#' @importFrom stringr str_to_lower
#'
#' @export
home_vs_away_points <- function(tibble, team = NULL) {
  if (is.null(team)) {
    # Create a box plot for the scores of home and away teams
    scores <- tibble %>%
      pivot_longer(cols = c(Score1, Score2), names_to = "Team", values_to = "Score") %>%
      mutate(Team = if_else(Team == "Score1", "Home", "Away"))
  } else {
    # Filter the data for the selected team
    team <- str_to_lower(team)
    scores <- tibble %>%
      filter(str_to_lower(Team1) == team | str_to_lower(Team2) == team) %>%
      mutate(Win_Loss = if_else((str_to_lower(Team1) == team & Score1 > Score2) |
                                  (str_to_lower(Team2) == team & Score2 > Score1), "Win", "Loss"))
  }

  scores_plot <- ggplot(scores, aes(x = ifelse(is.null(team), Team, Win_Loss), y = Score, fill = ifelse(is.null(team), Team, Win_Loss))) +
    geom_boxplot() +
    labs(title = ifelse(is.null(team), "Scores of Home and Away Teams", paste("Scores of", toupper(team), "Wins and Losses")),
         x = ifelse(is.null(team), "Team", "Outcome"), y = "Score") +
    theme_minimal()
  
  return(scores_plot)
}
