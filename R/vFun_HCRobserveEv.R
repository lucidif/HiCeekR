#' HCRobserveEvent
#'
#' @param eventExpr
#' @param handlerExpr
#' @param event.env
#' @param event.quoted
#' @param handler.env
#' @param handler.quoted
#' @param label
#' @param suspended
#' @param priority
#' @param domain
#' @param autoDestroy
#' @param ignoreNULL
#' @param ignoreInit
#' @param once
#'
#' @return
#' @keywords internal
#'
#' @examples
HCRobserveEvent<-function(eventExpr, handlerExpr, event.env = parent.frame(),
                    event.quoted = FALSE, handler.env = parent.frame(),
                    handler.quoted = FALSE, label = NULL, suspended = FALSE,
                    priority = 0, domain = getDefaultReactiveDomain(),
                    autoDestroy = TRUE, ignoreNULL = TRUE, ignoreInit = FALSE,
                    once = FALSE ){

    shiny::observeEvent(input$prjButton,{

        busyIndServer("prjButton",{

        })

    })

}
