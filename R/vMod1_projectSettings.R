prjSettings_UI <- function(id, label="prjSettings"){
    ns <- shiny::NS(id)
    shiny::fluidPage(
        shiny::fluidRow(shiny::column(12,
            shiny::wellPanel("Project",
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
        )
        ),
        shiny::fluidRow(
                    #shiny::fluidRow(
                        shiny::column(12,
                            shiny::uiOutput(ns("anSettings"))
                                        )
                    #)
        ),
        shiny::fluidRow(
                        shiny::column(12,
                            shiny::dataTableOutput(ns("pathViewer"))
                        )
        )

    )
}

prjSettings_Server <- function(input, output, session){
    nspace<-session$ns
    workingDir<-as.character((read.table('HCRwd.info'))[1,1])
    rea<-reactiveValues()
    output$selector<-shiny::renderUI({
        shiny::selectInput(nspace("loadNewPrj"),
                           label=h5(""),
                           choices=c("new",
                                    "load"))
    })
    output$pathViewer<-shiny::renderDataTable({
            folderFrame(paste0(workingDir, "Projects/"))
    })

    shiny::observeEvent(input$loadNewPrj,{
        if (input$loadNewPrj == "load"){
            output$prjName<-shiny::renderUI({
                shiny::fluidRow(
                    shiny::column(2,
                        shiny::br(),
                        shiny::helpText("name:")
                                ),
                    shiny::column(6,
                        shiny::selectInput(nspace("prjLoadName"),
                                           label=h5(""),
                                           choices=as.array(
                                               list.files(pointin(workingDir,
                                                                  "Projects"))
                                            ))
                                ),
                    shiny::column(4,
                        shiny::br(),
                        shiny::actionButton(nspace('prjButton'),
                                            label="load this")
                                )
                )
            })

            # output$executeBut<-shiny::renderUI({
            #     shiny::actionButton(nspace('prjButton'), label="load this")
            # })
        }
        if (input$loadNewPrj == "new"){
            output$prjName<-shiny::renderUI({
                                shiny::fluidRow(
                                    shiny::column(2,
                                        shiny::br(),
                                        shiny::helpText("name:")
                                                ),
                                    shiny::column(6,
                                        shiny::textInput(nspace("prjNewName"),
                                                       label=h5(""),
                                                       value=""
                                                  )
                                                ),
                                    shiny::column(4,
                                        shiny::br(),
                                        shiny::actionButton(nspace("prjButton"),
                                                            label="make new")
                                                )
                                )

            })

            # output$executeBut<-renderUI({
            #     shiny::actionButton(nspace("prjButton"), label="make new")
            # })
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
                            shiny::helpText(paste0('project selected:       ',
                                                    pName))
                              ),
                shiny::column(3,
                shiny::actionButton(nspace("resetPrj"), label="reset project"))
                )
        })



        output$anSettings<-shiny::renderUI({
            shiny::wellPanel(
                shiny::br(),
                shiny::br(),
                shiny::fluidRow(
                    shiny::column(3,
                        shiny::selectInput(nspace("loadNewAn"),
                                            label=h5("Analysis Settings"),
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
                    ),
                shiny::fluidRow(
                    shiny::column(12,
                        shiny::uiOutput(nspace("anOptionsSlot"))
                        )
                ),
                shiny::fluidRow(
                    shiny::column(12,
                        shiny::uiOutput(nspace("anOptionsSlot2"))
                        )
                ),
                shiny::fluidRow( shiny::br(),
                    shiny::column(12,
                        shiny::uiOutput(nspace("anOptionsSlot3"))
                        )
                )
                )
                            })
    })

    shiny::observeEvent(input$resetPrj,{
        output$anSettings<-shiny::renderUI({})
        output$selector<-shiny::renderUI({
                    #shiny::br(),
                    shiny::selectInput(nspace("loadNewPrj"),
                                       label=h5(""),
                                       choices=c("new",
                                                 "load"))
        })
        if (input$loadNewPrj == "load"){
            output$prjName<-shiny::renderUI({
                shiny::fluidRow(
                    shiny::column(2,
                                  shiny::br(),
                                  shiny::helpText("name:")
                    ),
                    shiny::column(6,
                                  shiny::selectInput(nspace("prjLoadName"),
                                                     label=h5(""),
                                                     choices=as.array(
                                                         list.files(pointin(workingDir,
                                                                            "Projects"))
                                                     ))
                    ),
                    shiny::column(4,
                                  shiny::br(),
                                  shiny::actionButton(nspace('prjButton'), label="load this")
                    )
                )
            })

            # output$executeBut<-shiny::renderUI({
            #     shiny::actionButton(nspace('prjButton'), label="load this")
            # })
        }
        if (input$loadNewPrj == "new"){
            output$prjName<-shiny::renderUI({
                shiny::fluidRow(
                    shiny::column(2,
                                  shiny::br(),
                                  shiny::helpText("name:")
                    ),
                    shiny::column(6,
                                  shiny::textInput(nspace("prjNewName"),
                                                   label=h5(""),
                                                   value=""
                                  )
                    ),
                    shiny::column(4,
                                  shiny::br(),
                                  shiny::actionButton(nspace("prjButton"), label="make new")
                    )
                )

            })
            }
        output$pathViewer<-shiny::renderDataTable({
            folderFrame(paste0(workingDir, "Projects/"))
        })
        output$anSetted<-shiny::renderUI({})
    })

    shiny::observeEvent(input$loadNewAn,{
        if(input$loadNewAn == "new"){
            output$anNames<-shiny::renderUI({
                shiny::textInput(nspace("anNewNames"), label=h5("Analysis"))
            })

            output$anExecuteBut<-shiny::renderUI({
                #shiny::actionButton(nspace("anButton"), label="make")
            })

            output$anOptionsSlot2<-shiny::renderUI({
                shiny::fluidRow(
                    shiny::column(4, shiny::br(), shiny::br(), shiny::br(),
                            shiny::helpText("input file type: ")
                            ),
                    shiny::column(5, shiny::br(), shiny::br(),
                                shiny::selectInput(nspace("inputType"),
                                                    label = h5(""),
                                                    choices = c("BAM",
                                                                "Matrix",
                                                                "Multi")
                                )
                    )

                    ,shiny::column(3, shiny::br(), shiny::br(),
                                shiny::actionButton(nspace("newAnalysis"),
                                                label = h5("make new analysis"),
                                                width = "100%"
                                )

                    )
                )
            })

            output$anOptionsSlot3<-shiny::renderUI({
                shiny::fluidRow(shiny::column(12,
                                    shiny::fluidRow(
                                        shiny::column(4,shiny::br(),
                                            shiny::helpText("Select bam file:")
                                            ),
                                        shiny::column(8,
                                            shiny::fileInput(nspace("bamFile"),
                                                            label=h5("")
                                                            )
                                                    )
                                    ),
                                    shiny::fluidRow(
                                        shiny::column(2,
                                            shiny::helpText("Set Enzyme")
                                            ),
                                    shiny::column(5,
                                        shiny::textInput(nspace("cutSite"),
                                                label = shiny::h5("cut site"),
                                                value = "ACTGC"
                                                )
                                            ),
                                        shiny::column(5,
                                            shiny::numericInput(
                                                    nspace("overhang"),
                                                    label=shiny::h5("overhang"),
                                                    value = 3
                                                    )
                                                  )
                                            ),
                                        shiny::fluidRow(
                                            #shiny::fluidRow(
                                            shiny::column(2,
                                                shiny::helpText("Other options")
                                                    ),
                                            shiny::column(5,
                                                shiny::selectInput(
                                                    nspace("binSize"),
                                                    label = "bin size",
                                                    choices = c( "1000000",
                                                                "100000",
                                                                "50000",
                                                                "20000",
                                                                "10000"
                                                                )
                                                        )
                                                  ),
                                            shiny::column(5, br(),
                                                shiny::checkboxInput(
                                                    nspace("lowMem"),
                                                    label = h5("low memory"),
                                                    value = FALSE
                                                        )
                                                  )
                                                  #)
                                            )

                ))
            })

#             output$anOptionsSlot<-shiny::renderUI({
#                 shiny::fluidRow(
#                     shiny::column(6,
#                         shiny::selectInput(nspace("newLoadSelector"),
#                                             label = h5("analysis"),
#                                             choices = c("new","load")
#                                             ),
#                         shiny::uiOutput(nspace("anActionButtonSlot"))
#                         )
#                 )
#             })
        }
        if(input$loadNewAn == "load"){
            output$anNames<-shiny::renderUI({
                shiny::selectInput(nspace("anLoadNames"), label=h5("Analysis"),
                                    choices=c(list.dirs(rea$anFolder))
                                    )
            })

            output$anExecuteBut<-shiny::renderUI({
                shiny::actionButton(nspace("anButtonLoad"), label="load")
            })
            output$anOptionsSlot2<-shiny::renderUI({})
            output$anOptionsSlot3<-shiny::renderUI({})
        }
        output$pathViewer<-shiny::renderDataTable({
            folderFrame(rea$anFolder)
        })
    })


    shiny::observeEvent(input$inputType,{
        if (input$inputType == "BAM"){
            output$anOptionsSlot3<-shiny::renderUI({
                shiny::fluidRow(
                    shiny::column(12,
                        shiny::fluidRow(
                            shiny::column(4,shiny::br(),
                                shiny::helpText("Select bam file:")
                                    ),
                                shiny::column(8,
                                shiny::fileInput(nspace("bamFile"),
                                                label=h5("")
                                                )
                                    )
                            ),
                    shiny::fluidRow(
                        shiny::column(2,
                                shiny::helpText("Set Enzyme")
                                    ),
                        shiny::column(5,
                                shiny::textInput(nspace("cutSite"),
                                                label = shiny::h5("cut site"),
                                                value = "ACTGC"
                                                )
                                    ),
                        shiny::column(5,
                                shiny::numericInput(nspace("overhang"),
                                                    label=shiny::h5("overhang"),
                                                    value = 3

                                                    )
                                    )
                    ),
                    shiny::fluidRow(
                        #shiny::fluidRow(
                            shiny::column(2,
                                shiny::helpText("Other options")
                                ),
                            shiny::column(5,
                                shiny::selectInput(nspace("binSize"),
                                                    label = "bin size",
                                                    choices = c( "1000000",
                                                                "100000",
                                                                "50000",
                                                                "20000",
                                                                "10000"
                                                    )
                                                    )
                                ),
                            shiny::column(5, br(),
                                shiny::checkboxInput(nspace("lowMem"),
                                                    label = h5("low memory"),
                                                    value = FALSE
                                                    )
                                )
                        #)

                    ),
                    shiny::fluidRow(

                    )

                ))
            })

        } else {
            if (input$inputType == "Matrix") {
                output$anOptionsSlot3<-shiny::renderUI({

                    shiny::fluidRow(
                        shiny::column(12,
                            shiny::fluidRow(
                                shiny::column(4,shiny::br(),
                                            shiny::helpText("Select matrix:")
                                ),
                                shiny::column(8,
                                            shiny::fileInput(nspace("maFile"),
                                                            label=h5("")
                                              )
                                )
                            ),
                            shiny::fluidRow(
                                shiny::column(2,
                                    shiny::helpText("Matrix Type")
                                        ),
                                    shiny::column(5,
                                        shiny::selectInput(nspace("maType"),
                                                            label="",
                                                            choices=c("sparse",
                                                                    "dense"
                                                                    )
                                                        )
                                          ),
                                    shiny::column(5,
                                        shiny::selectInput(nspace("maCounts"),
                                                            label="",
                                                            choices=c("raw",
                                                            "Normalized"
                                                                    )
                                                        )
                                          )
                                      )
                                    )
                    )

                })
            } else {#input$inputType == "Multi"
                output$anOptionsSlot3<-shiny::renderUI({
                    shiny::fluidRow(
                        shiny::column(12,
                            shiny::fileInput(nspace("loadMultiFiles"),
                                            label=h5("please select
                                                    interesting files"),
                                            multiple=TRUE
                                            )
                            )
                    )
                })
            }
        }
    })


}
