# busyIndUI <- function(button) {
#     id <- button[['attribs']][['id']]
#     shiny::div(
#         `data-for-btn` = id,
#         button,
#         shiny::span(
#             class = "btn-loading-container",
#             shinyjs::hidden(
#                 shiny::img(src = "/www/loaderBar.gif", class = "btn-loading-indicator"),
#                 shiny::icon("check", class = "btn-done-indicator")
#             )
#         ),
#         shinyjs::hidden(
#             shiny::div(class = "btn-err",
#                 shiny::div(icon("exclamation-circle"),
#                     tags$b("Error: "),
#                     shiny::span(class = "btn-err-msg")
#                 )
#             )
#         )
#     )
# }

#' BusyIndServer
#'
#' @param buttonId
#' @param expr
#'
#' @return
#' @keywords internal
#'
#' @examples
busyIndServer <- function(buttonId, expr) {
    # UX stuff: show the "busy" message, hide the other messages, disable the button
    loadingEl <- sprintf("[data-for-btn=%s] .btn-loading-indicator", buttonId)
    doneEl <- sprintf("[data-for-btn=%s] .btn-done-indicator", buttonId)
    errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
    shinyjs::disable(buttonId)
    shinyjs::show(selector = loadingEl)
    shinyjs::hide(selector = doneEl)
    shinyjs::hide(selector = errEl)
    on.exit({
        shinyjs::enable(buttonId)
        shinyjs::hide(selector = loadingEl)
    })

    # Try to run the code when the button is clicked and show an error message if
    # an error occurs or a success message if it completes
    tryCatch({
        value <- expr
        shinyjs::show(selector = doneEl)
        shinyjs::delay(2000, shinyjs::hide(selector = doneEl, anim = TRUE, animType = "fade",
                                           time = 0.5))
        value
    }, error = function(err) { errorFunc(err, buttonId) })
}

#' errorFunc
#'
#' @param err
#' @param buttonId
#'
#' @return
#' @keywords internal
#'
#' @examples
errorFunc <- function(err, buttonId) {
    errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
    errElMsg <- sprintf("[data-for-btn=%s] .btn-err-msg", buttonId)
    errMessage <- gsub("^ddpcr: (.*)", "\\1", err$message)
    shinyjs::html(html = errMessage, selector = errElMsg)
    shinyjs::show(selector = errEl, anim = TRUE, animType = "fade")
}
