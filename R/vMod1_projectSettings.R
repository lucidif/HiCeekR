prjSettings_UI <- function(id, label="prjSettings"){
    ns <- NS(id)
    shiny::fluidPage(
        shiny::fluidRow(
            shiny::wellPanel(
            shiny::fluidRow(
                shiny::column(3,
                    shiny::uiOutput(ns("selector"))
                        ),
                shiny::column(6,
                        shiny::uiOutput(ns("prjName"))
                                ),
                shiny::column(3,
                        shiny::br(),
                        shiny::br(),
                        shiny::uiOutput(ns("executeBut"))
                        )
                    ),
            shiny::fluidRow(
                shiny::column(12,
                              shiny::uiOutput(ns("anSetted"))
                )
            )
                )
        ),
        shiny::fluidRow(
                shiny::wellPanel(
                    shiny::fluidRow(
                        shiny::column(12,
                            shiny::uiOutput(ns("anSettings"))
                                      )
                    )
                        )
        ),
        shiny::fluidRow(
                        shiny::dataTableOutput(ns("pathViewer"))
        )

    )
}

prjSettings_Server <- function(input, output, session){
    nspace<-session$ns
    workingDir<-as.character((read.table('HCRwd.info'))[1,1])
    rea<-reactiveValues()
    output$selector<-shiny::renderUI({
        shiny::selectInput(nspace("loadNewPrj"),
                           label=h5("project"),
                           choices=c("new",
                                    "load"))
    })
    output$pathViewer<-shiny::renderDataTable({
            folderFrame(paste0(workingDir, "Projects/"))
    })

    shiny::observeEvent(input$loadNewPrj,{
        if (input$loadNewPrj == "load"){
            output$prjName<-shiny::renderUI({
                shiny::selectInput(nspace("prjLoadName"),
                                    label=h5('select project'),
                                    choices=as.array(
                                        list.files(pointin(workingDir,
                                                        "Projects"))
                                    ))
            })

            output$executeBut<-shiny::renderUI({
                shiny::actionButton(nspace('prjButton'), label="load this")
            })
        }
        if (input$loadNewPrj == "new"){
            output$prjName<-renderUI({
                shiny::textInput(nspace("prjNewName"),
                                label=h5("select name"),
                                value=""
                )
            })

            output$executeBut<-renderUI({
                shiny::actionButton(nspace("prjButton"), label="make new")
            })
        }
    })

    shiny::observeEvent(input$prjButton,{
        if(input$loadNewPrj == "new"){
            pName<-input$prjNewName
        }else{4
            pName<-input$prjLoadName
        }
        rea$anFolder<-paste0(workingDir, pName, "/")

        output$selector<-shiny::renderUI({
            shiny::fluidRow(column(12))

        })

        output$prjName<-shiny::renderUI({

        })

        output$executeBut<-shiny::renderUI({})

        output$anSetted<-shiny::renderUI({
            shiny::fluidRow(
                shiny::column(9,
                              shiny::helpText('project selected')
                              ),
                shiny::column(3,
                shiny::actionButton(nspace("resetPrj"), label="reset project"))
                )
        })



        output$anSettings<-shiny::renderUI({
            shiny::wellPanel(paste0("Project:",pName),
                shiny::br(),
                shiny::br(),
                shiny::fluidRow(
                    shiny::column(3,
                        shiny::selectInput(nspace("loadNewAn"),
                                            label=h5("analysis"),
                                            choices=c("new","load"))
                        ),
                    shiny::column(6,
                        shiny::uiOutput(nspace("anNames"))
                                ),
                    shiny::column(3,
                        shiny::br(),
                        shiny::br(),
                        shiny::uiOutput(nspace("anExecuteBut"))
                        )
                    )
                )
                            })
    })

    shiny::observeEvent(input$resetPrj,{
        output$anSettings<-shiny::renderUI({})
        output$selector<-shiny::renderUI({
            shiny::selectInput(nspace("loadNewPrj"),
                               label=h5("project"),
                               choices=c("new",
                                         "load"))
        })
        if (input$loadNewPrj == "load"){
            output$prjName<-shiny::renderUI({
                shiny::selectInput(nspace("prjLoadName"),
                                   label=h5('select project'),
                                   choices=as.array(
                                       list.files(pointin(workingDir,
                                                        "Projects"))
                                   ))
            })

            output$executeBut<-shiny::renderUI({
                shiny::actionButton(nspace('prjButton'), label="load this")
            })
        }
        if (input$loadNewPrj == "new"){
            output$prjName<-renderUI({
                shiny::textInput(nspace("prjNewName"),
                                 label=h5("select name"),
                                 value=""
                )
            })
            }
        output$pathViewer<-shiny::renderDataTable({
            folderFrame(paste0(workingDir, "Projects/"))
        })
    })

    shiny::observeEvent(input$loadNewAn,{
        if(input$loadNewAn == "new"){
            output$anNames<-shiny::renderUI({
                shiny::textInput(nspace("anNewNames"), label=h5("Analysis"))
            })

            output$anExecuteButton<-shiny::renderUI({
                shiny::actionButton(nspace("anButton"), label="make")
            })
        }
        if(input$loadNewAn == "load"){
            output$anNames<-shiny::renderUI({
                shiny::selectInput(nspace("anLoadNames"), label=h5("Analysis"),
                                    choices=c(list.dirs(rea$anFolder))
                                    )
            })

            output$anExecuteButton<-shiny::renderUI({
                shiny::actionButton(nspace("anButton"), label="load")
            })
        }
        output$pathViewer<-shiny::renderDataTable({
            folderFrame(rea$anFolder)
        })
    })


}
