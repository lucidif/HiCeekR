
#' HMM_postProcessing_UI
#'
#' @param id
#' @param label
#'
#' @return
#' @keywords internal
#'
#' @examples
HMM_postProcessing_UI <- function(id, label = "TADs") {
    hmmNs <- shiny::NS(id)
    shiny::fluidPage(

        shiny::fluidRow(

            shiny::column (12,

                           shiny::wellPanel (

                               shiny::fluidRow (shiny::column(12

                                    # ,selectFile (hmmNs('fragInput'), path=pointin(wdPath,'Pre-Processing'), label='select cutGenome file'
                                    #                  , subset=TRUE, pattern='.cutGenome'
                                    #      )

                        )
                        ),

                        shiny::fluidRow(shiny::column(12
                                        # ,selectFile (hmmNs('h5Input'), path=pointin(wdPath,'Pre-Processing'), label='select h5 file'
                                        #             , subset=TRUE, pattern='.h5'
                                        # )
                        )),

                        shiny::fluidRow(shiny::column (12
                                         # ,selectFile (hmmNs('binTabInput'), path=pointin(wdPath,'Binning'), label='al regions bins table'
                                         #             , subset=TRUE, pattern='.bint.bed'
                                         # )

                        )),

                        shiny::fluidRow(

                            shiny::column (12,

                                    shiny::uiOutput(hmmNs('wellPanelSlot'))

                                    # wellPanel (
                                    #   fluidRow (column (12))
                                    # )
                            )

                        )



                    )

            )

        )

    )

}

#==================================================================================


# Module server function

#' HMM_postProcessing_Server
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
HMM_postProcessing_Server <- function(input, output, session, stringsAsFactors,
                                        wdPath) {
    hmmNs<-session$ns

    output$wellPanelSlot<-shiny::renderUI({
        shiny::wellPanel(

            shiny::fluidRow(

                shiny::column(12,
                    shiny::textInput(hmmNs('fileName'),label=h5('select file name')))

            ),

            shiny::fluidRow(

                shiny::column (4,
                    shiny::actionButton(hmmNs('startBut'), label=h5('find TADs'))
                ),

                shiny::column (8,
                    shiny::helpText('create the file that contains the directional indexes')
                )

            ),

            shiny::fluidRow (
                shiny::column(12,
                    shiny::textOutput(hmmNs('report'))))

        )
    })

    shiny::observeEvent(input$startBut,{

        filne<-paste0(input$fileName,'.DI')
        filpath<-pointin (wdPath,'Downstream')
        refpath<-paste0(pointin(wdPath,'Pre-Processing', sys=TRUE),"refGenFrag.cutGenome.tsv") #cutgenome
        h5path<- paste0(pointin(wdPath,'Filtering', sys=TRUE),"trimmed.h5")  #h5
        #non crei il bint.bed da nessuna parte, ma ti serve
        bintabpath<- paste0(pointin(wdPath,'Binning', sys=TRUE),"allRegions.bint.bed") #bint.bed



        #binsize<- as.numeric(HCRread('',paste0(wdPath, 'info.tsv'))[1])
        binsize<-HCRread(file='info.tsv', path=wdPath, header=FALSE)[2,2]

        resu<-hmmDI(fileOutName=filne, outputPath=filpath, refFragsPATH=refpath, h5PATH=h5path, bintablePath=bintabpath, bin.size=binsize)
        print ("hmm indices finded")
        #reportTxt<-paste0(filepath,filne)
        output$fileRep<- renderText({
            paste0(filpath,filne)
        })

        output$wellPanelSlot<-shiny::renderUI ({
            shiny::wellPanel(

                shiny::fluidRow(

                    shiny::column (4,
                        shiny::actionButton(hmmNs('newfile'), label=h5('find new TADs file'))
                    ),

                    shiny::column (8,
                        shiny::helpText('restart finding with new data')
                    )

                ),

                shiny::br(), shiny::br(),

                shiny::fluidRow(

                    shiny::column(8,
                        shiny::textOutput(hmmNs('fileRep'))
                    )

                ),

                shiny::fluidRow (

                    shiny::column (3,
                        shiny::helpText('   was created'))

                )

            )
        })

    })

    shiny::observeEvent (input$newfile, {

        output$wellPanelSlot<-shiny::renderUI ({
            shiny::wellPanel(

                shiny::fluidRow(

                    shiny::column (12,
                        shiny::textInput(hmmNs('fileName'),
                                        label=h5('select file name')))

                ),

                shiny::fluidRow(

                    shiny::column (4,
                                   shiny::actionButton(hmmNs('startBut'), label=h5('find TADs'))
                    ),

                    shiny::column (8,
                        shiny::helpText('create file with DI')
                    )

                ),

                shiny::fluidRow (
                    shiny::column(12,
                        shiny::textOutput(hmmNs('report'))))

            )
        })

    } )

}
