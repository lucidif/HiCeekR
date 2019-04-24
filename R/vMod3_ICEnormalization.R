##diffHiC_normalization

# Module UI function
#' diffHic_Normalization_UI
#'
#' @param id
#' @param label
#'
#' @return
#' @keywords internal
#'
#' @examples
diffHic_Normalization_UI <- function(id, label = "Visualization") {
    diffHicNormNs <- NS(id)

    shiny::fluidPage(
        shiny::fluidRow(
            shiny::wellPanel(
            shiny::fluidRow(
                shiny::column(4,shiny::helpText("raw matrix file load : ")),
                shiny::column(8,shiny::uiOutput(diffHicNormNs("rawFpSlot")))
            ))
        ),
        shiny::fluidRow(
            shiny::column(3,
                shiny::wellPanel(
                    shiny::fluidRow(#shiny::column(2,
                                    #    shiny::helpText("raw matrix")),
                                    shiny::column(4
                                        ,shiny::helpText("max interaction")
                                                  ),
                                    shiny::column(8
                                        #,shiny::uiOutput(diffHicNormNs("rawFpSlot"))
                                        ,shiny::numericInput(diffHicNormNs("maxIter")
                                                             ,label=""
                                                             ,value=50
                                                             )
                                        )
                                    ),
                    shiny::fluidRow(
                        shiny::column(4
                                      ,shiny::helpText("eps")
                        ),
                        shiny::column(8
                                      ,shiny::numericInput(diffHicNormNs("eps")
                                                           ,label=""
                                                           ,value=1e-4
                                                           )
                        )
                    ),
                    shiny::fluidRow(
                        shiny::column(4
                                      ,shiny::helpText("sparse filter")
                        ),
                        shiny::column(8
                                      ,shiny::numericInput(diffHicNormNs("spFilter")
                                                           ,label=""
                                                           ,value=0.02
                                      )
                        )
                    ),
                    shiny::fluidRow(
                        # shiny::column(4
                        #               ,shiny::helpText("sparse filter")
                        # ),
                        shiny::column(8
                                      ,shiny::actionButton(diffHicNormNs('rdButton'),
                                                           label='Start Normalization')
                        )
                    )

                )
                          ),
            shiny::column(9,
                shiny::uiOutput(diffHicNormNs("resultSlot"))
                          )
        )
        )

}

#==================================================================================


# Module server function

#' diffHic_Normalization_Server
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
diffHic_Normalization_Server <- function(input, output, session,
                                        stringsAsFactors, wdPath) {


    diffHicNormNs<-session$ns
    saveMe<-shiny::reactiveValues(firstV='none')

    output$rawFpSlot<-shiny::renderUI({
        shiny::column(6, selectFile(diffHicNormNs('matrixPath'), path=pointin(wdPath, 'Binning') , subset=TRUE, pattern='_raw_matrix.tsv'))
    })


    observeEvent(input$rdButton,{
        resu<-ICEnormalization(paste0( pointin(wdPath,'Binning'), input$matrixPath),max_iter=input$maxIter ,eps=input$eps, sparse.filter=input$spFilter)

        # arrchr<-c()
        # for (i in 1:length(strsplit(colnames(resu),":"))){
        #     arrchr[i]<-strsplit(colnames(resu),":")[[i]][1]
        # }
        chr<-unique(gsub("\\:.*","",colnames(resu)))
        sparsity<-(length(which(resu>0)))/(dim(resu)[1]*dim(resu)[2])

        filename<-paste0("ICEnorm_",paste(chr,collapse="VS"),"_mxIt",input$maxIter,"_eps",input$eps, "_spfil", input$spFilter)


        HCRwrite(resu, file=filename,
                 path=pointin(wdPath,'Normalization'), row.names=TRUE)

        output$histSlot<-shiny::renderPlot({
            hist(log2(resu),col="cadetblue1",  main="values distribution")
        })



        output$resultSlot<-shiny::renderUI({
            shiny::fluidRow(
                shiny::fluidRow(
                    shiny::column(4,shiny::helpText("chromosomes: ")),
                    shiny::column(8,shiny::helpText(chr))
                ),
                shiny::fluidRow(
                    shiny::column(4,shiny::helpText("sparsity: ")),
                    shiny::column(8,shiny::helpText(sparsity))
                ),
                shiny::fluidRow(
                    shiny::column(4,shiny::helpText("normalized matrix saved in: ")),
                    shiny::column(8,shiny::helpText(pointin(wdPath,'Normalization')))
                ),
                shiny::fluidRow(
                    shiny::column(4,shiny::helpText("file name")),
                    shiny::column(8,shiny::helpText(filename))
                ),
                shiny::fluidRow(
                    #shiny::column(4,shiny::helpText("file name")),
                    shiny::column(12,shiny::plotOutput(diffHicNormNs("histSlot")))
                )

            )
        })






    })




}
