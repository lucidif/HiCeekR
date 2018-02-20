moduleTemplate_UI <- function(id, label="moduleTemplate"){
    ns <- shiny::NS(id)

    shiny::fluidPage(

#=========================================================
        shiny::fluidRow(
            shiny::column(12,
                shiny::wellPanel("Additional Inputs",
                                shiny::br(),
                                shiny::br(),
                                shiny::br()
                                )
                )
        ),
#=========================================================
        shiny::fluidRow(
            shiny::column(3,
                shiny::wellPanel("Options",
                                shiny::br(),
                                shiny::br(),
                                shiny::br(),
                                shiny::br(),
                                shiny::br(),
                                shiny::br()
                                )
                    ),
            shiny::column(9,
                shiny::tabsetPanel(
                    shiny::tabPanel("Results"),
                    shiny::tabPanel("Help")
                )
                    )
        )
    )
}


moduleTemplate_Server <- function(input, output, session){
    nspace<-session$ns
}
