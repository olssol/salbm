#' Run Web-Based \code{salbm} application
#'
#' Call Shiny to run \code{salbm} as a web-based application.
#'
#' @details
#'
#' A web browser will be brought up for users to access the GUI of \code{salbm}.
#'
#' @examples
#' \dontrun{
#' salbmShiny()}
#'
#' @export
#'
salbmShiny <- function() {
    
    if (!requireNamespace("shiny", quietly = TRUE)) {
        stop("Shiny is needed for this function to work. Please install it.",
        call. = FALSE)
    }
    
    if (!requireNamespace("DT", quietly = TRUE)) {
        stop("DT is needed for this function to work. Please install it.",
        call. = FALSE)
    }
    
    if (!requireNamespace("shinydashboard", quietly = TRUE)) {
        stop("shinydashboard is needed for this function to work. Please install it.",
        call. = FALSE)
    }
    
    if (!requireNamespace("dplyr", quietly = TRUE)) {
        stop("dplyr is needed for this function to work. Please install it.",
        call. = FALSE)
    }
    
    appDir <- system.file("shiny", package = "salbm")
    if (appDir == "") {
        stop("Could not find Shiny directory. Try re-installing `salbm`.",
        call. = FALSE)
    }
    
    
    shiny::runApp(appDir, display.mode = "normal");
}
