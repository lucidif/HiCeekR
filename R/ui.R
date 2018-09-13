


#====================================CSS loadScript===========================

appCSS <- "
#loading-content {
position: absolute;
background: #000000;
opacity: 0.9;
z-index: 100;
left: 0;
right: 0;
height: 100%;
text-align: center;
color: #FFFFFF;
}
"
#====================================CSS busyClick==========================
appCSS_busyClick <- "
.btn-loading-container {
  margin-left: 10px;
  font-size: 1.2em;
}
.btn-done-indicator {
  color: green;
}
.btn-err {
  margin-top: 10px;
  color: red;
}
"


#======================================main UI===============================

#non so perchè ma se non carico qui le funzioni nella ui non me le vede!?
#TODO prova a spostarle nel global

##====================preliminary fun =======================================




#' busyIndUI
#'
#' @param button
#'
#' @return
#' @keywords internal
#'
#' @examples
busyIndUI <- function(button) {
    id <- button[['attribs']][['id']]
    shiny::div(
        `data-for-btn` = id,
        button,
        shiny::span(
            class = "btn-loading-container",
            shinyjs::hidden(
                shiny::img(src = "/www/loaderBar.gif", class = "btn-loading-indicator"),
                shiny::icon("check", class = "btn-done-indicator")
            )
        ),
        shinyjs::hidden(
            shiny::div(class = "btn-err",
                       shiny::div(shiny::icon("exclamation-circle"),
                                shiny::tags$b("Error: "),
                                shiny::span(class = "btn-err-msg")
                       )
            )
        )
    )
}

#' obtainWd
#'
#' @return
#' @keywords internal
#'
#' @examples
obtainWd <- function (){

    if (file.exists("HCR.config")==TRUE){
        patto<-pointin(as.character(paste0((read.table('HCR.config'))[1,1],"HiCeekRwd/")), "RefGen")
    } else {
        patto<-"./"
    }

    return(patto)

}

ui<-shiny::shinyUI (



    shiny::div ( id="mainApp",


    shiny::fluidPage (

        shinyjs::useShinyjs(),
        shinyjs::inlineCSS(appCSS),
        shiny::tags$style(appCSS_busyClick),

        # Loading message
        shiny::div(
                id = "loading-content",
                shiny::h2("Loading...")
        ),

        shiny::fluidRow(
            shiny::column(12,
                shiny::uiOutput("welcomeSlot")
                )
        ),
        shiny::conditionalPanel( condition =
            "output.configFileText == 'no config file found, please make a config file with panel below'"
            ,shiny::wellPanel(shiny::fluidRow(shiny::column(4,
                                                shiny::selectInput("loadOrNewConf",
                                                                label=".config",
                                                                choices=c("load"
                                                                        ,"new"
                                                                        )
                                                                    )),
                                            shiny::column(8, shiny::br(),
                                            shiny::textOutput("configHelper")
                                                        )
                                            ),
            shiny::conditionalPanel( condition="input.loadOrNewConf == 'new'",
                shiny::fluidRow(shiny::column(12,
                    shiny::fluidRow(
                        shiny::column(2,
                                    shiny::helpText("select working folder:")
                                    ),
                        shiny::column(1,
                                shinyFiles::shinyDirButton("wdPath",
                                                        label="explore",
                                                        title="select working Path"
                                                        )
                            # busyIndUI(
                            #     shiny::actionButton("makeConfig",
                            #                         label="set",
                            #                         class = "btn-primary"
                            #     )
                            # )
                                ),
                    shiny::column(1
                            ,busyIndUI(
                                shiny::actionButton("makeConfig",
                                                    label="set",
                                                    class = "btn-primary"
                                                    ,width="100%"
                                                    )
                                )
                            ),
                    shiny::column(8,
                                shiny::uiOutput("wdPathText")
                                )
                ),
                shiny::fluidRow(
                    shiny::column(12)
                )
            ))
            ),

        shiny::conditionalPanel(condition="input.loadOrNewConf == 'load'",
                shiny::fluidRow(
                    shiny::column(2,
                        shiny::helpText("select .config file")
                                ),
                    shiny::column(1,
                        shinyFiles::shinyFilesButton("configPath",
                                                    label="explore",
                                                    title="select working Path",
                                                    multiple=FALSE
                                  )

                                ),
                    shiny::column(1,
                                busyIndUI(
                                    shiny::actionButton("loadConfig",
                                                        label="set",
                                                        class = "btn-primary"
                                                        ,width="100%"
                                      )
                                  )
                                ),
                    shiny::column(8,
                                shiny::textOutput("configPathText")
                                )
                )
                                )

        )
        ),



        shiny::fluidRow(
            shiny::column(12,
                shiny::uiOutput("prjManagerSlot")
                        )
            # ,shiny::column(1,
            #             shiny::wellPanel(
            #                 shiny::conditionalPanel(
            #                     condition="$('html').hasClass('shiny-busy')",
            #                     #helpText (h5('loading'))
            #                     shiny::img(src='.sysImg/busy_icon.png',
            #                                 align='right', height=20,width=20)
            #                 ),shiny::br()
            #             )
            #         )
        ),

#===========================conditionalPanel(condition="input.inputType=='BAM'"
#====
        shiny::conditionalPanel(condition="input.inputType=='BAM'
                                        && output.prjState=='Project Selected'
                                        && input.loadNewAn=='new'",
                                #shiny::fluidRow(shiny::column(12,
                                shiny::wellPanel(
                                    # shiny::fluidRow(column(1,
                                    #     shiny::helpText("Select files"))
                                    #     ),
                                    shiny::fluidRow(
                                        shiny::column(2,shiny::br(),
                                            shiny::helpText("select files:")
                                                    ),
                                        shiny::column(2, shiny::br(),
                                            shinyFiles::shinyFilesButton(
                                                                'bamFile',
                                                    label='select BAM',
                                                    title='Please select a file'
                                                    ,multiple=FALSE)
                                                    ,align="left"),
                                        shiny::column(2, shiny::br(),
                                                      shinyFiles::shinyFilesButton(
                                                          'refGenome',
                                                          label='select reference',
                                                          title='Select reference genome'
                                                          ,multiple=FALSE)
                                        )
                                        # ,shiny::column(12,
                                        #     shiny::textOutput("bamPathShow")
                                        #             )
                                    ),
                                    shiny::fluidRow(
                                        shiny::column(2,
                                        shiny::helpText("Bam:")),
                                        shiny::column(10, shiny::br(),
                                            shiny::textOutput("bamPathShow")
                                        )
                                        # shiny::column(2,shiny::br(),
                                        #               shiny::helpText("reference:")
                                        # ),
                                        # shiny::column(2, shiny::br(),
                                        #               shinyFiles::shinyFilesButton(
                                        #                   'refGenome',
                                        #                   label='select reference',
                                        #                   title='Select reference genome'
                                        #                   ,multiple=FALSE)
                                        # ),
                                        # shiny::column(8,
                                        #               shiny::textOutput("bamPathShow")
                                        # )
                                    ),
                                    shiny::fluidRow(
                                        shiny::column(2,shiny::helpText("Reference:")),
                                        shiny::column(10,shiny::br(),
                                            shiny::textOutput("textReference")
                                        )
                                    ),
                                    shiny::fluidRow(
                                        shiny::column(2,
                                            shiny::helpText("Set Enzyme")
                                            ),
                                        shiny::column(5,
                                            shiny::textInput("cutSite",
                                                        label = shiny::h5(
                                                                "cut site"),
                                                        value = "AAGCTT"
                                                    )
                                                ),
                                        shiny::column(5,
                                            shiny::numericInput("overhang",
                                                            label=shiny::h5(
                                                                "overhang"),
                                                            value = 4
                                                                )
                                                )
                                            ),
                                    shiny::fluidRow(
                                        shiny::column(2,
                                            shiny::helpText("Other options")
                                            ),
                                        shiny::column(5,
                                            shiny::selectInput("binSize",
                                                            label = "bin size",
                                                            choices = c(
                                                                "1000000",
                                                                "100000",
                                                                "50000",
                                                                "20000",
                                                                "10000"
                                                                    )
                                                                )
                                                    ),
                                        shiny::column(2
                                            #,shiny::br(),
                                            ## shiny::checkboxInput("lowMem",
                                            ##                     label =
                                            ##                     shiny::h5
                                            ##                     ("low memory"),
                                            ##                     value = FALSE
                                            ##                     )
                                            ## selectFile(
                                            ##     id="refGenome",
                                            ##     path = obtainWd(),
                                            ##     label= "reference"
                                            ## )
                                            ## shiny::uiOutput("refgenSlot")
                                            # shinyFiles::shinyFilesButton(
                                            #     'refGenome',
                                            #     label='select reference',
                                            #     title='Select reference genome'
                                            #     ,multiple=FALSE)
                                                    )
                                        # ,shiny::column(2, shiny::br(),
                                        #     shiny::textOutput("textReference")
                                        #         )
                                            )
                                )
                                #)
        ),

#========================conditionalPanel(condition="input.inputType=='Matrix'"
#====
        shiny::conditionalPanel(condition="input.inputType=='Matrix'
                                && output.prjState=='Project Selected'
                                && input.loadNewAn=='new'",
                                shiny::wellPanel(
                                    #shiny::column(12,
                                    shiny::fluidRow(
                                        shiny::column(2,shiny::br(),
                                            shiny::helpText("Select matrix:")
                                                    ),
                                        shiny::column(2,
                                                shinyFiles::shinyFilesButton(
                                                    'matrixFile',
                                                    label='select Matrix',
                                                    title='Please select a file'
                                                    ,multiple=FALSE)
                                            # shiny::fileInput("maFile",
                                            #                 label=shiny::h5("")
                                            #                 )
                                                    ),
                                        shiny::column(8,
                                            shiny::textOutput("matrixPathShow")
                                                )
                                            ),
                                    shiny::fluidRow(
                                        shiny::column(2,
                                            shiny::helpText("Matrix Type")
                                                    ),
                                        shiny::column(5,

                                            shiny::selectInput("maType",
                                                                label="",
                                                                choices=c(
                                                                    "simmetric",
                                                                    "reduced"
                                                                        )
                                                            )
                                                      ),
                                        shiny::column(5,
                                            shiny::selectInput("maCounts",
                                                                    label="",
                                                                    choices=c(
                                                                        "raw",
                                                                    "Normalized"
                                                                            )
                                                                )
                                                      )
                                                  )
                                    #)
                                )
                                ),
#====
        shiny::fluidRow(
        shiny::column(12,
            shiny::dataTableOutput("pathViewer")
            )
        ),
#===============================================================================endTabsetPanel


        shiny::fluidRow(
            shiny::column(12,
                shiny::uiOutput("navSlot")
                )
        ),

    shiny::fluidRow(
        shiny::column (12,
                       shiny::uiOutput('selectPn')
        )
    ),

    shiny::fluidRow (

        shiny::column (12,
        shiny::uiOutput('moduleScreen')
        )
    )



    ,shinyjs::hidden(
        shiny::div(
                id = "app-content",
                shiny::p("HiCeekR 0.99 free and open-source software")
                  )
              )



)

)

)
