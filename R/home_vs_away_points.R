#' Create a ggplot box plot comparing the scores of home and away teams.
#'
#' This function takes a tibble containing game data and creates a box plot
#' comparing the scores of home and away teams. It pivots the data to have a
#' tidy format suitable for plotting and creates a box plot showing the
#' distribution of scores for both home and away teams.
#'
#' @param tibble A tibble containing game data with columns Score1 and Score2 representing
#' the scores of the home and away teams, respectively.
#'
#' @return A ggplot box plot comparing the scores of home and away teams.
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
#' @importFrom ggplot2 aes geom_boxplot labs theme_minimal
#' @importFrom tidyr pivot_longer
#'
#' @export

home_vs_away_points <- function(tibble) {
  # Create a box plot for the scores of the home teams compared with a box plot of the scores of the away teams
  scores <- tibble %>%
    pivot_longer(cols = c(Score1, Score2), names_to = "Team", values_to = "Score") %>%
    mutate(Team = ifelse(Team == "Score1", "Away", "Home"))

  scores_plot <- ggplot(scores, aes(x = Team, y = Score, fill = Team)) +
    geom_boxplot() +
    labs(title = "Scores of Home and Away Teams", x = "Team", y = "Score") +
    theme_minimal()
  return(scores_plot)
}
