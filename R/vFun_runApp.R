
#' Run HiCeekR
#'
#' Run HiCeekR Shiny application
#'
#'
#' @return None
#'
#' @example
#'
#'
#' @export
HiCeekR=function (){
    #shiny::runApp(appDir="R/")
    shiny::shinyApp(server=server, ui=ui)
}

