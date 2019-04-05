#' Launch the Shiney app
#'
#' @export
launch_app <- function(){
  shiny::runApp(system.file("app", package='swinesurvey'))
}
