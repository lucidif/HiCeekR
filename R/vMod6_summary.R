
#' sum_UI
#'
#' @param id
#' @param label
#'
#' @return
#' @keywords internal
#'
#' @examples
sum_UI<-function(id, label="summary"){
    sumns<-NS(id)
    shiny::fluidPage(
        shiny::fluidRow(
            shiny::column(12,
                shiny::wellPanel("summary:", shiny::br(), shiny::br(),

                    shiny::fluidRow(

                        shiny::column(6,
                                    shiny::textOutput(sumns("prjNameSlot"))
                                    ),

                        shiny::column(6,
                                      shiny::textOutput(sumns("anNameSlot"))
                        )
                        ), shiny::br(),

                    shiny::fluidRow(

                        shiny::column(3,
                                      shiny::textOutput(sumns("intySlot"))
                        ),

                        shiny::column(9,
                                   shiny::textOutput(sumns("inpaSlot"))
                                    )
                        ), shiny::br(),

                    shiny::fluidRow(
                        shiny::column(6,
                                      shiny::textOutput(sumns("resoSlot"))
                        ),
                        shiny::column(6,
                                    shiny::textOutput(sumns("otherComSlot"))
                                    )
                        )),

                    shiny::fluidRow(
                        shiny::column(12,
                                    shiny::uiOutput(sumns("PreProcessingSlot"))
                                    )
                        ),
                    shiny::fluidRow(
                        shiny::column(12,
                                      shiny::uiOutput(sumns("FilteringSlot"))
                        )
                    ),
                    shiny::fluidRow(

                        shiny::column(12,
                                      shiny::uiOutput(sumns("BinningSlot"))
                        )
                    ),
                    shiny::fluidRow(
                        shiny::column(12,
                                      shiny::uiOutput(sumns("NormalizationSlot"))
                        )
                    )


                )
        )
    )
}


#' sum_Server
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
sum_Server<-function(input, output, session, stringsAsFactors,
                    wdPath){

    #prjNameSlot anNameSlot intySlot inpaSlot otherComSlot resoSlot

    sumns<-session$ns

    val<-shiny::reactiveValues()
    infotab<-read.table(paste0(wdPath,"info.tsv"), sep="\t", header=FALSE )
    #infotabTT<<-infotab
    val$anName<-as.character(infotab[5,3])
    val$prjName<-as.character(infotab[4,3])
    val$intype<-as.character(infotab[3,3])
    val$inpa<-as.character(infotab[1,3])
    val$reso<-as.character(infotab[2,3])
    val$otrCom<-as.character(infotab[6,3])

    output$prjNameSlot<-shiny::renderText({paste0("project name:      ",val$prjName)})
    output$anNameSlot<-shiny::renderText({paste0("analysis name:      ",val$anName)})
    output$intySlot<-shiny::renderText({paste0("input type:      ",val$intype)})
    output$inpaSlot<-shiny::renderText({paste0("path:      ",val$inpa)})
    output$resoSlot<-shiny::renderText({paste0("resolution:      ",val$reso)})
    output$otherComSlot<-shiny::renderText({val$otrCom})




    if (file.exists(paste0(pointin(wdPath, "Pre-Processing", sys=TRUE)
                           ,"h5file_bamMatching_report"))==TRUE){
        # addResourcePath("plt", pointin(wdPath, "Pre-Processing", sys=TRUE)
        #                                          )
        #print (paste0(pointin(wdPath, "Pre-Processing", sys=TRUE),"fragments.jpeg"))
        file.copy(from=paste0(pointin(wdPath, "Pre-Processing", sys=TRUE),"fragments.jpeg"),
                  to=paste0(getwd(),"/www")
                  )

        file.copy(from=paste0(pointin(wdPath, "Pre-Processing", sys=TRUE),"inOutWard.jpeg"),
                  to=paste0(getwd(),"/www")
        )

        #print (paste0(getwd(),"/www"))
        output$preImage<-shiny::renderImage({
            list(src = "www/fragments.jpeg",
                contentType = 'image/jpeg',
                 width = 400,
                 height = 300,
                 alt = "plot not finded")
        })

        output$preInOutWard<-shiny::renderImage({
            list(src = "www/inOutWard.jpeg",
                 contentType = 'image/jpeg',
                 width = 400,
                 height = 300,
                 alt = "plot not finded")
        })

        output$PreProcessingSlot<-shiny::renderUI({
            shiny::fluidRow(
                shiny::column(12,
                    shiny::fluidRow(
                        shiny::column(12,
                            shiny::wellPanel(
                                shiny::fluidRow(
                                    shiny::column(12,
                                        shiny::helpText(
                                        "Pre-Processing successfully excuted")
                                        )
                                )
                            )
                            )
                    ),
                    shiny::fluidRow(
                        shiny::column(4,
                            shiny::tableOutput(sumns("preProTableSlot"))
                            ),
                        shiny::column(8,
                            shiny::fluidRow(
                                shiny::column(12,
                                    #shiny::plotOutput(sumns("fragPPplot"))
                                    #shiny::img("fragments.jpeg")
                                    shiny::imageOutput(sumns("preImage"))
                                    )
                            ),
                            shiny::fluidRow(
                                shiny::column(12,
                                    #shiny::plotOutput(sumns("wardPPplot"))
                                    shiny::imageOutput(sumns("preInOutWard"))
                                              )
                            )
                            )
                    )
                            )
            )
        })

        pretab<-read.table(paste0(pointin(wdPath, "Pre-Processing", sys=TRUE)
                          ,"h5file_bamMatching_report")
                    , sep="\t", header=FALSE)

        odd<-c()
        even<-c()
        oddInd<-1
        evenInd<-1
        for (i in 1:length(pretab[,1])){
            if ( (i %% 2 == 0)==TRUE ){
                even[evenInd]<-as.character(pretab[i,1])
                evenInd<-evenInd+1
            } else {#dispari
                odd[oddInd]<-as.character(pretab[i,1])
                oddInd<-oddInd+1
            }
        }

        pretab2<-data.frame(odd,even)
        colnames(pretab2)<-c("", "")



        output$preProTableSlot<-renderTable(
            pretab2
        )

    }

    if (file.exists(paste0(pointin(wdPath, "Filtering", sys=TRUE)
                           ,"parameter.tsv"))==TRUE){

        file.copy(from=paste0(pointin(wdPath, "Filtering", sys=TRUE),"fragments_trimmed.jpeg"),
                  to=paste0(getwd(),"/www")
        )

        file.copy(from=paste0(pointin(wdPath, "Filtering", sys=TRUE),"inOutWard_trimmed.jpeg"),
                  to=paste0(getwd(),"/www")
        )

        output$filImage<-shiny::renderImage({
            list(src = "www/fragments_trimmed.jpeg",
                 contentType = 'image/jpeg',
                 width = 400,
                 height = 300,
                 alt = "plot not finded")
        })

        output$filInOutWard<-shiny::renderImage({
            list(src = "www/inOutWard_trimmed.jpeg",
                 contentType = 'image/jpeg',
                 width = 400,
                 height = 300,
                 alt = "plot not finded")
        })



        output$FilteringSlot<-shiny::renderUI({
            shiny::fluidRow(
                shiny::column(12,
                              shiny::fluidRow(
                                  shiny::column(12,
                                                shiny::wellPanel(
                                                    shiny::fluidRow(
                                                        shiny::column(12,
                                                                      shiny::helpText("Filtering successfully excuted")
                                                        )
                                                    )
                                                )
                                  )
                              ),
                              shiny::fluidRow(
                                  shiny::column(4,
                                                shiny::tableOutput(sumns("filteringTableSlot"))
                                  ),
                                  shiny::column(8,
                                                shiny::fluidRow(
                                                    shiny::column(12,
                                                                  #shiny::plotOutput(sumns("fragPPplot"))
                                                                  #shiny::img("fragments.jpeg")
                                                                  shiny::imageOutput(sumns("filImage"))
                                                    )
                                                ),
                                                shiny::fluidRow(
                                                    shiny::column(12,
                                                                  #shiny::plotOutput(sumns("wardPPplot"))
                                                                  shiny::imageOutput(sumns("filInOutWard"))
                                                    )
                                                )
                                  )
                              )
                )
            )
        })

        filta<-read.table(paste0(pointin(wdPath, "Filtering", sys=TRUE)
                          ,"parameter.tsv"), sep="\t", header=FALSE)
        colnames(filta)<-c("", "")

        output$filteringTableSlot<-renderTable({
                    filta
        })
    }

    if (file.exists(paste0(pointin(wdPath, "Binning", sys=TRUE)
                           ,"parameter.tsv"))==TRUE){

        output$BinningSlot<-shiny::renderUI({
            shiny::fluidRow(
                shiny::column(12,
                              shiny::fluidRow(
                                  shiny::column(12,
                                        shiny::wellPanel(
                                            shiny::fluidRow(
                                                shiny::column(12,
                                                    shiny::helpText("Binning successfully excuted")
                                                        )
                                                    )
                                                )
                                  )
                              ),
                              shiny::fluidRow(
                                  shiny::column(12,
                                                shiny::tableOutput(sumns("binningTableSlot"))
                                  )
                              )
                )
            )
        })
        binta<-read.table(paste0(pointin(wdPath, "Binning", sys=TRUE)
                          ,"parameter.tsv"), sep="\t", header=FALSE)
        colnames(binta)<-c("","")
        output$binningTableSlot<-shiny::renderTable({
                binta
        })

    }


    if (file.exists(paste0(pointin(wdPath, "Normalization", sys=TRUE)
                           ,"parameter.tsv"))==TRUE){


        output$NormalizationSlot<-shiny::renderUI({
            shiny::fluidRow(
                shiny::column(12,
                              shiny::fluidRow(
                                  shiny::column(12,
                                                shiny::wellPanel(
                                                    shiny::fluidRow(
                                                        shiny::column(12,
                                                                      shiny::helpText(
                                                                          "Normalizzation successfully excuted")
                                                        )
                                                    )
                                                )
                                  )
                              ),
                              shiny::fluidRow(
                                  shiny::column(12,
                                                shiny::tableOutput(sumns("NormTableSlot"))
                                  )
                              )
                )
            )
        })

        normatab<-read.table(paste0(pointin(wdPath, "Normalization", sys=TRUE)
                                  ,"parameter.tsv")
                           , sep="\t", header=FALSE)

        colnames(normatab)<-c("","")

        output$NormTableSlot<-renderTable(
            normatab
        )

    }





}
