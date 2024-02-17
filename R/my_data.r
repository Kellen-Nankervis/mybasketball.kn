#' @title My Data Loading Function
#' @description Load my custom dataset
#' @export
my_data <- function() {
  data <- read.csv("data-raw/cbbga24_2023.csv")
  return(data)  # Load your dataset
}