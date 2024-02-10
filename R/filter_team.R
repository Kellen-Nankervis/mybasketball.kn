#' Filter game data to include matches involving a specific team.
#'
#' This function takes a tibble containing game data and filters it to include only
#' matches where a specific team is involved. It checks both Team1 and Team2 columns
#' to find matches involving the specified team.
#'
#' @param tibble A tibble containing game data with columns Team1 and Team2 representing
#' the two teams playing in each match.
#' @param team A character string specifying the name of the team to filter matches for.
#'
#' @return A tibble containing only matches where the specified team is involved,
#' either as Team1 or Team2.
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
#' # Filter game data to include matches involving "Team A"
#' filter_team(game_data, "Team A")
#'
#' @importFrom dplyr filter
#'
#' @export

filter_team <- function(tibble, team) {
  filter(tibble, Team1 == team | Team2 == team)
}
