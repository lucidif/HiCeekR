#' pcaComp_postProcessing_UI
#'
#' @param id
#' @param label
#'
#' @return
#' @keywords internal
#'
#' @examples
pcaComp_postProcessing_UI <- function(id, label = "compartments") {
    compNs <- NS(id)
    shiny::fluidPage(

        shiny::fluidRow (
            shiny::column(12,
                shiny::uiOutput(compNs('mainPanelSlot'))))

    )

}

#==================================================================================


# Module server function

#' pcaComp_postProcessing_Server
#'
#' @param input
#' @param output
#' @param session
#' @param stringsAsFactors
#' @param wdPath
#'
#' @return
#' @keywords internal
#'
#' @examples
pcaComp_postProcessing_Server <- function(input, output,
                                        session, stringsAsFactors,
                                        wdPath
                                        ) {
    compNs<-session$ns

    output$mainPanelSlot<- shiny::renderUI({
        shiny::wellPanel (

            shiny::fluidRow (
                shiny::column (8,
                        selectFile (compNs('matrixFile'), path=pointin(wdPath,'Normalization'), label='select normalizzed contact matrix'
                                    , subset=FALSE, pattern='')
                ),
                shiny::column (4

                )
            ),

            shiny::fluidRow (
                shiny::column (8,
                    shiny::textInput (compNs('filename'),
                                      label=h5('select file name'))
                ),
                shiny::column (4,
                    shiny::actionButton (compNs('startBut'),
                                        label='find compartments')
                )

            )

            # ,fluidRow (  ##file to report
            #
            #   column (12)
            #
            # ),
            #
            # fluidRow (
            #
            #   column (12)
            #
            # )

        )
    })

    shiny::observeEvent(input$startBut,{


        path=paste0(pointin(wdPath,'Normalization'),input$matrixFile)
        name=paste0(input$filename,'.PCAloading')
        print (paste0("wdPath",wdPath))
        print (paste0("pointin: ",pointin(wdPath,'Downstream')))
        outPath=pointin(wdPath,'Downstream')
        conMa=readContactMatrix(path)
        View (head(conMa))
        #pcaTab= prcomp (conMa)
        pcaTab= prcomp (conMa, na.action=na.omit)
        pcaTabPC2=pcaTab$rotation[,1:2]
        HCRwrite (pcaTabPC2, name, path=outPath , quote=FALSE, col.names=TRUE, row.names=TRUE)

        fileRepo<-paste0(outPath,name,'.tsv')

        output$fileReportSlot<-shiny::renderText({
            fileRepo
        })

        output$mainPanelSlot<-shiny::renderUI({
            shiny::wellPanel (

                shiny::fluidRow (
                    shiny::column (8,
                        shiny::textOutput(compNs('fileReportSlot'))
                    ),
                    shiny::column (4

                    )
                ),

                shiny::fluidRow (
                    shiny::column (8,
                        shiny::helpText('was created')
                    ),
                    shiny::column (4,
                        shiny::actionButton (compNs('startNew'), label='start new')
                    )

                )

                # ,fluidRow (  ##file to report
                #
                #   column (12)
                #
                # ),
                #
                # fluidRow (
                #
                #   column (12)
                #
                # )

            )
        })


    })

    shiny::observeEvent(input$startNew,{
        output$mainPanelSlot<-shiny::renderUI({
            shiny::wellPanel (

                shiny::fluidRow (
                    shiny::column (8,
                            selectFile (compNs('matrixFile'), path=pointin(wdPath,'Normalization'), label='select normalizzed contact matrix'
                                        , subset=FALSE, pattern='')
                    ),
                    shiny::column (4

                    )
                ),

                shiny::fluidRow (
                    shiny::column (8,
                        shiny::textInput (compNs('filename'), label=h5('select file name'))
                    ),
                    shiny::column (4,
                        shiny::actionButton (compNs('startBut'), label='find compartments')
                    )

                )

                # ,fluidRow (  ##file to report
                #
                #   column (12)
                #
                # ),
                #
                # fluidRow (
                #
                #   column (12)
                #
                # )

            )
        })
    })

}
