#vFun_prjManagerUI
#' prjManagerMainUI
#'
#' @return
#' @keywords internal
#'
#' @examples
prjManagerMainUI<-function(){
    return(
        shiny::fluidRow(column(12,
            shiny::fluidRow(
                shiny::column(12,
                    shiny::wellPanel(
                        shiny::fluidRow(
                            shiny::column(12,
                                shiny::textOutput("prjState")
                                )
                        ),
                        shiny::fluidRow(
                            shiny::column(3,
                                shiny::uiOutput("selector")
                        ),
                            shiny::column(6,
                                shiny::uiOutput("prjName")
                                ),
                            shiny::column(3,
                                shiny::br(),
                                shiny::br(),
                                shiny::uiOutput("executeBut")
                                )
                        ),
                        shiny::fluidRow(
                            shiny::column(12,
                                shiny::uiOutput("anSetted")
                            )
                        )
                    )
                )
            ),
            shiny::fluidRow(
                shiny::column(12,
                                shiny::uiOutput("anSettings")
                )
            )
            # shiny::fluidRow(
            #     shiny::column(12,
            #         shiny::dataTableOutput("pathViewer")
            #     )
            # )
        ))
    )
}

#' prjManPanel_welcome
#'
#' @return
#' @keywords internal
#'
#' @examples
prjManPanel_welcome<-function(){
    return(
        shiny::wellPanel(
            shiny::fluidRow(
                shiny::column(3),
                shiny::column(6, align="center",
                            shiny::helpText(h1("Welcome to HiCeekR"))
                ),
                shiny::column(3)
            ),
            shiny::fluidRow(
                shiny::column(12,
                    shiny::textOutput("configFileText")
                            )
            )
        )

    )
}

#' prjManPanel_prjSelector
#'
#' @return
#' @keywords internal
#'
#' @examples
prjManPanel_prjSelector<-function(){

    return(
    shiny::selectInput("loadNewPrj",
                       label=h5(""),
                       choices=c("new",
                                 "load"))
    )

}

#' prjManPanel_loadProject
#'
#' @param workingDir
#'
#' @return
#' @keywords internal
#'
#' @examples
prjManPanel_loadProject<- function(workingDir){
        shiny::fluidRow(
            shiny::column(2,
                        shiny::br(),
                        shiny::helpText("name:")
            ),
            shiny::column(6,
                        shiny::selectInput("prjLoadName",
                                            label=h5(""),
                                            choices=as.array(
                                                list.files(pointin(workingDir,
                                                                    "Projects"))
                                            ))
            ),
            shiny::column(4,
                          shiny::br(),
                          busyIndUI(
                            shiny::actionButton(
                                                "prjButton",
                                                label = "load this",
                                                class = "btn-primary"
                              )
                          )
                        # shiny::actionButton('prjButton',
                        #                     label="load this")
            )
        )
}

#' prjManPanel_newProject
#'
#' @return
#' @keywords internal
#'
#' @examples
prjManPanel_newProject<- function(){
    return(
        shiny::fluidRow(
            shiny::column(2,
                        shiny::br(),
                        shiny::helpText("name:")
            ),
            shiny::column(6,
                        shiny::textInput("prjNewName",
                                        label=h5(""),
                                        value=""
                        )
            ),
            shiny::column(4,
                        shiny::br(),
                            busyIndUI(
                                shiny::actionButton("prjButton",
                                                    label="make new",
                                                    class = "btn-primary"
                                                    )
                            )
            )
        )
    )
}

#' prjManPanel_reset
#'
#' @param pName
#'
#' @return
#' @keywords internal
#'
#' @examples
prjManPanel_reset<-function(pName){
    shiny::fluidRow(
        shiny::column(8,
            #shiny::helpText(pName)
            shiny::textOutput("pNameSlot")
                ),

        shiny::column(1,
            shiny::conditionalPanel(condition="$('html').hasClass('shiny-busy')"
                                    ,helpText (h5('loading'))
                                    #,icon = icon("www/busy_icon.png")
                                    #,img("www/busy_icon.png")
                      )
        ),

        shiny::column(3,
                busyIndUI(
                    shiny::actionButton("resetPrj", label="reset project",
                                        class = "btn-primary"
                                        )
                    )
                )
    ,div(style = "height:40px"))
}

#' prjManPanel_analysisSettings
#'
#' @return
#' @keywords internal
#'
#' @examples
prjManPanel_analysisSettings<-function(){
    shiny::wellPanel(
        shiny::br(),
        shiny::br(),
        shiny::fluidRow(
            shiny::column(3,
                          shiny::selectInput("loadNewAn",
                                             label=h5("Analysis Settings"),
                                             choices=c("new","load"))
            ),
            shiny::column(6,
                          shiny::uiOutput("anNames")
            ),
            shiny::column(3,
                        shiny::br(),
                        shiny::br(),
                        shiny::uiOutput("anExecuteBut")
            )
        ),
        shiny::fluidRow(
            shiny::column(12,
                          shiny::uiOutput("anOptionsSlot")
            )
        ),
        shiny::fluidRow(
            shiny::column(12,
                        shiny::uiOutput("anOptionsSlot2")
            )
        ),
        shiny::fluidRow( shiny::br(),
                         shiny::column(12,
                                    shiny::uiOutput("anOptionsSlot3")
                         )
        )
    )
}

#' prjManPanel_analysisInputType
#'
#' @return
#' @keywords internal
#'
#' @examples
prjManPanel_anlysisInputType<- function(){
    return(
    shiny::fluidRow(
        shiny::column(4, shiny::br(), shiny::br(), shiny::br(),
                      shiny::helpText("input file type: ")
        ),
        shiny::column(5, shiny::br(), shiny::br(),
                      shiny::selectInput("inputType",
                                        label = h5(""),
                                        choices = c("BAM"
                                                    #,"Matrix"
                                        #,"Multi"
                                        )
                      )
        )

        # ,shiny::column(3, shiny::br(), shiny::br(),
        #             busyIndUI(
        #                 shiny::actionButton("newAnalysis",
        #                                     label = h5("make new analysis"),
        #                                     class = "btn-primary",
        #                                     width = "100%"
        #                 )
        #                )
        #
        # )
    )
    )
}

#' prjManPanel_inputBAM
#'
#' @param prjFolder
#' @param input
#'
#' @return
#' @keywords internal
#'
#' @examples
prjManPanel_inputBAM<-function(prjFolder,input){
    #questo puoi inserirlo in un conditional panel

    return(
        shiny::fluidRow(shiny::column(12,
            shiny::fluidRow(column(1,shiny::helpText("Select files"))),
            shiny::fluidRow(
                shiny::column(4,shiny::br(),
                    shiny::helpText("bam:")
                    ),
                # shiny::column(8,
                #         selectFile("bamFile",
                #                 label=h5(""),
                #                 path=paste0(
                #                 prjFolder,
                #                 "/ProjectData/Hi-C/"),
                #                 subset=TRUE,
                #                 pattern=".bam"
                #                 )
                #   )
                shiny::column(8,
                    shinyFiles::shinyFilesButton('bamFile',
                                                label='select BAM',
                                                title='Please select a file',
                                                multiple=FALSE)
                    )
                ),
            shiny::fluidRow(
                shiny::column(4,
                    shiny::helpText("reference:")
                ),
                shiny::column(8,
                    shiny::uiOutput("refgenSlot")
                )
            ),
            shiny::fluidRow(column(1,shiny::helpText("Options"))),
            shiny::fluidRow(
                shiny::column(2,
                    shiny::helpText("Set Enzyme")
                ),
                shiny::column(5,
                    shiny::textInput("cutSite",
                                    label = shiny::h5("cut site"),
                                    value = "ACTGC"
                    )
                ),
                shiny::column(5,
                    shiny::numericInput("overhang",
                                        label=shiny::h5("overhang"),
                                        value = 3
                    )
                )
            ),
            shiny::fluidRow(
                #shiny::fluidRow(
                shiny::column(2, shiny::br(),
                    shiny::helpText("Other options")
                ),
            shiny::column(5, shiny::br(),
                shiny::selectInput("binSize",
                                    label = "bin size deprecated",
                                    choices = c( "1000000",
                                                "100000",
                                                "50000",
                                                "20000",
                                                "10000"
                                                )
                                    )
            ),
            shiny::column(5, shiny::br(), shiny::br(),
                          #busyIndUI(
                              shiny::actionButton("newAnalysis",
                                                  label = shiny::h5("make new analysis"),
                                                  #class = "btn-primary",
                                                  width = "100%"
                              )
                          #)

            )
            # ,shiny::column(5
            #     # ,shiny::br(),
            #     # shiny::checkboxInput("lowMem",
            #     #                     label = h5("low memory"),
            #     #                     value = FALSE
            #     # )
            #     # selectFile(
            #     #     id="refGenome",
            #     #     path = pointin(rea$workingDir, "RefGen"),
            #     #     label= "reference"
            #     # )
            #     #shiny::uiOutput("refgenSlot")
            # )
            #)
            )


        ))
    )
}

#' prjManPanel_inputMatrix
#'
#' @return
#' @keywords internal
#'
#' @examples
prjManPanel_inputMatrix<-function(){
    return(
        shiny::fluidRow(
            shiny::column(12,
                          shiny::fluidRow(
                              shiny::column(4,shiny::br(),
                                            shiny::helpText("Select matrix:")
                              ),
                              shiny::column(8,
                                            shiny::fileInput("maFile",
                                                             label=h5("")
                                            )
                              )
                          ),
                          shiny::fluidRow(
                              shiny::column(2,
                                            shiny::helpText("Matrix Type")
                              ),
                              shiny::column(5,
                                            shiny::selectInput("maType",
                                                               label="",
                                                               choices=c("simmetric",
                                                                         "reduced"
                                                               )
                                            )
                              ),
                              shiny::column(5,
                                            shiny::selectInput("maCounts",
                                                               label="",
                                                               choices=c("raw",
                                                                         "Normalized"
                                                               )
                                            )
                              )
                          )
            )
        )
    )
}

#' prjManPanel_inputMulti
#'
#' @return
#' @keywords internal
#'
#' @examples
prjManPanel_inputMulti<-function(){
                        return(
                            shiny::fluidRow(
                                shiny::column(12,
                                    shiny::fileInput("loadMultiFiles",
                                           label=h5("please select
                                                    interesting files"),
                                           multiple=TRUE
                          )
            )
        )
    )
}
