##bedConvertion_postProcessing

#' bed2track_postProcessing_UI
#'
#' @param id
#' @param label
#'
#' @return
#' @keywords internal
#'
#' @examples
bed2track_postProcessing_UI <- function (id, label='bed2track' ){
    pcaNS<- shiny::NS (id)

    shiny::fluidPage(

            shiny::fluidRow (

                 shiny::column (2,
                                shiny::wellPanel(
                                    shiny::fluidRow(
                                        shiny::column(12,
                                            shiny::uiOutput (pcaNS('paramSlot'))
                                                      )
                                    ),
                                    shiny::fluidRow(
                                        shiny::uiOutput(pcaNS('startPcaSlot'))
                                    )

                 )
             ),

                                     shiny::column (10, shiny::br(), shiny::br(),
                                                    shiny::dataTableOutput(pcaNS("results"))
                                                    #,shiny::uiOutput(pcaNS('startPcaSlot'))
                                                    #shiny::actionButton (pcaNS('startPca'),
                                                    #                      'start PCA')

                                     )

                                 ),


                                 shiny::fluidRow(

                                     shiny::column (12, plotOutput(pcaNS('pcaPlot')) )

                                 ),

                                 shiny::fluidRow (

                                     shiny::column (12,
                                                    shiny::uiOutput(pcaNS('selectPCui')) )

                                 )







    )

}

#' bed2track_postProcessing_Server
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
bed2track_postProcessing_Server <- function (input, output, session, stringsAsFactors,
                                       wdPath
){
    print ("start bed2track module")
    #plusval<-0
    nSpace<- session$ns
    #require ('Rsamtools')

    ##reactiveValue

    Reac<- shiny::reactiveValues(tP='none', S_pcaMod='none', S_exportThis='none') #non c'è bisogno che definisci prima la variabile reactiveValues() e poi definisci dopo

    print ("parameters render")
    output$paramSlot<- shiny::renderUI ({

        shiny::fluidRow(

            shiny::fluidRow(
                shiny::column(12,
                              shiny::br(),
                              shiny::textInput(nSpace("filename"), label="File Name")
                              )
            ),
            shiny::fluidRow(shiny::column(12,
                            shiny::fileInput(nSpace("bedfile"),label="import bed file"))

                            )
        )


    })

    observeEvent(input$bedfile,{
        #print("observe matrix")
        #paste0(pointin(wdPath,'Normalization'),input$pcaTableLoaded)

            output$startPcaSlot<-shiny::renderUI({
                shiny::actionButton (nSpace('start'),
                                     'start')
            })
            #}

        #}
    })



    shiny::observeEvent(input$start,{

        print ('.....start bed2track conversion.....')

        #require(HiTC)

        filename<-input$filename
        bedfile<-input$bedfile$datapath
        bincoords<-paste0(pointin(wdPath,'Binning'),"/allRegions.bint.bed")
        outpath<-pointin(wdPath,'Downstream')

        window=input$windowSize

        print(paste0("filename: ",input$filename))
        print(paste0("bed",bedfile))
        print(paste0("bincoords: ", bincoords))

        track<-bed2track(filename=filename, bedpath=bedfile,
                         bincoord=bincoords, outpath=outpath)

        output$results<-renderDataTable({
            track
        })

    })


}
