#' Launch the Shiny app
#'
#' @export
launch_app <- function(){
  shiny::runApp(system.file("app", package='octoflushow'))
}
