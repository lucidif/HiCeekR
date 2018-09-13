prjMan_UI<-function(id, label="prjManager"){
    shiny::fluidPage(
        shiny::fluidRow(
            shiny::column(12,
                shiny::wellPanel(
                          shiny::fluidRow(
                              shiny::column(4),
                              shiny::column(4,
                                    shiny::helpText(
                                        "please select or create new project")
                                ),
                              shiny::column(4)
                            )
                        )
                        )
        )

    )
}

prjMan_server<-function(input, output, session, HCRwdDir){



}
