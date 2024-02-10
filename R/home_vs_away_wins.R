#' Create a ggplot bar plot of the number of home and away wins.
#'
#' This function takes a tibble containing game data and creates a bar plot
#' showing the number of home wins and away wins. It summarizes the total times
#' the home team won compared to the away team and plots the results as a bar chart.
#'
#' @param tibble A tibble containing game data with columns Score1 and Score2 representing
#' the scores of the home and away teams, respectively.
#'
#' @return A ggplot bar plot showing the number of home wins and away wins.
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
#' # Create the bar plot of home vs. away wins
#' home_vs_away_wins(game_data)
#'
#' @importFrom ggplot2 aes geom_bar labs theme_minimal
#' @importFrom dplyr summarize bind_cols
#' @importFrom tidyr pivot_longer
#'
#' @export

home_vs_away_wins <- function(tibble) {
  # Summarize the total times the home team won compared to the away team
  home_wins <- summarize(tibble, Home_Wins = sum(as.numeric(Score1) < as.numeric(Score2)))
  away_wins <- summarize(tibble, Away_Wins = sum(as.numeric(Score1) > as.numeric(Score2)))

  # Combine home_wins and away_wins data frames
  total_wins <- bind_cols(home_wins, away_wins)

  # Reshape the data for plotting
  total_wins <- pivot_longer(total_wins, cols = c(Home_Wins, Away_Wins), names_to = "Location", values_to = "Wins")

  # Create a bar plot of the total times the home team won compared to the away team
  wins_plot <- ggplot(total_wins, aes(x = Location, y = Wins, fill = Location)) +
    geom_bar(stat = "identity") +
    labs(title = "Wins by Home and Away Teams", x = "Team", y = "Wins") +
    theme_minimal()
  return(wins_plot)
}
