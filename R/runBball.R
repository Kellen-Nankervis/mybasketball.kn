#' Basketball Shiny App
#' 
#' This function launches a Shiny app called "basketball_app" included in the 
#' package "mybasketball.kn". The app provides a plot and analysis of 
#' college basketball data.
#' 
#' @export

runBball <- function() {
    appDir <- system.file("shiny", "basketball_app", package = "mybasketball.kn")

    if (appDir == "") {
        stop("Could not find the Shiny app directory", call. = FALSE)
    }

    shiny::runApp(appDir, display.mode = "normal")
}