##filModule

# Module UI function
#' FilmodUI
#'
#' @param id
#' @param label
#'
#' @return
#' @keywords internal
#'
#' @examples
FilmodUI <- function(id, label = "filtering") {
    filns <- shiny::NS(id)

    #print (filns)
    shiny::fluidPage(

        # shiny::fluidRow(
        #     shiny::column(12,
        #         shiny::uiOutput(
        #             filns("startSlot")
        #         )
        #                 )
        # ),

        # shiny::conditionalPanel(condition = javaNs(filns,
        #                                         name = "reStart",
        #                                         type = "input",
        #                                         condition = ">0"
        #                                             ),
            shiny::fluidRow(
                shiny::column(12,
                    shiny::fluidRow (
                        shiny::column (3,
                            shiny::wellPanel (
                                shiny::fluidRow(
                                    shiny::column(12,
                                        shiny::numericInput(filns("maxFrag"),
                                                        label="max frag length",
                                                        value=600
                                                        )
                                                )
                                        ),
                                shiny::fluidRow(
                                    shiny::column(12,
                                        shiny::checkboxInput(filns("fixValue"),
                                                            label="fix values",
                                                            value= FALSE
                                                            )
                                                )
                                            ),
                                shiny::fluidRow(
                                    shiny::column(12,
                                        shiny::conditionalPanel(
                                                        condition=javaNs(filns,
                                                            name="fixValue",
                                                            type="input",
                                                                "==true"),
                                            shiny::fluidRow(
                                                shiny::column(6,
                                                    shiny::numericInput(
                                                        filns("minInward"),
                                                        label="min inward",
                                                        value=1500
                                                        )
                                                    ),
                                                shiny::column(6,
                                                    shiny::numericInput(
                                                        filns("minOutward"),
                                                        label="min outward",
                                                        value=25000
                                                        )
                                                    )
                                                            )
                                                                )
                                                    )
                                                ),
                                    shiny::fluidRow(
                                        shiny::column(12,
                                            shiny::conditionalPanel(
                                                    condition =
                                                    javaNs(filns,
                                                    name="fixValue",
                                                    type="input",
                                                        "==false"
                                                                ),
                                            shiny::fluidRow(
                                                shiny::column(12,
                                                shiny::fluidRow(
                                                    shiny::column(12,
                                                        shiny::sliderInput(
                                                       filns("minInwardSlider"),
                                                           label="min inward",
                                                            min=0,
                                                            max=25000,
                                                            step=100,
                                                            value=1000
                                                            )
                                                            )
                                                        ),
                                                shiny::fluidRow(
                                                    shiny::column(12,
                                                        shiny::sliderInput(
                                                            filns("minOutwardSlider"),
                                                            label= "min outward",
                                                            min=0,
                                                            max=25000,
                                                            step=100,
                                                            value=25000
                                                            )
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                    )
                                ),
                                shiny::fluidRow(
                                    # shiny::column(5,
                                    #       selectFile (filns("refgenFrag"), path=pointin(wdPath,'Pre-Processing') , label=".cutGenome.tsv file", subset=TRUE, pattern='.cutGenome')
                                    # ),
                                    #
                                    # shiny::column(5,
                                    #      selectFile(filns("h5"),path=pointin(anFolder$hisave,'Pre-Processing'), label = "h5 file", subset=TRUE, pattern='.h5')
                                    # ),

                                    shiny::column(12, br(),
                                        shiny::actionButton(filns('filBut'),
                                            'start Filtering',
                                            class="btn-primary"
                                                )
                                            )

                                        )
                                    )
                                ),
                                shiny::column(8,
                                    shiny::fluidRow(
                                        shiny::column(12,
                                            plotOutput(filns("beforePlot"))
                                            )
                                        ),
                                    shiny::fluidRow(
                                        shiny::column(12,
                                            plotOutput(filns("beforePlot2"))
                                                )
                                            )
                                  )
                              ),

                              shiny::fluidRow(
                                  shiny::column (10,
                                    shiny::fluidRow(
                                        shiny::uiOutput(filns('mainscreen')))
                                  ),
                                  shiny::column (2,
                                     shiny::uiOutput (filns('mainPanel'))
                                        )

                                  #    column (8,
                                  #            fluidRow  (uiOutput(filns('mainscreen')))
                                  #    )
                                        )
                    )
            )
#        ) #conditional end Bracket

        )#fluidPage end

}

#==================================================================================


# Module server function

#' FilmodServer
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
FilmodServer <- function(input, output, session,
                        stringsAsFactors, wdPath) {
    #print (input$bamSel)
    rhdf5::h5closeAll()
    filns<-session$ns
    #prova<-QCmodServer(filns('hsparam'))
    #print('param FilmodServer')
    #print(prova)

    #======================================================================
    # > filteringTable<-matrix(ncol=2, nrow=3)
    # > filteringTable<-matrix(ncol=1, nrow=3)
    # > rownames(filteringTable)<-c("maxFragLength", "minInward", "minOutward")
    # > filteringTable[,1]<-c("600", "1500", "25000")
    # > write.table(filteringTable, paste0(pointin(wdPath, "Filtering", sys=TRUE),"parameter.tsv"), sep="\t", quote=FALSE, col.names=FALSE)
    #
    #======================================================================

    values <- shiny::reactiveValues()

    values$h5_pa<-paste0(pointin(wdPath, "Pre-Processing", sys=TRUE),
                         "h5file.h5")
    print(paste0("h5_pa:   ",values$h5_pa))

    values$refgenFrag<-paste0(pointin(wdPath, "Pre-Processing", sys=TRUE),
                              "refGenFrag.cutGenome.tsv")
    print(paste0("refgenFrag:   ", values$refgenFrag))

    # ##beforePlot operation====================================================
    # #questa va eseguita nella sezione di prima e diags nei sysout
    # print("beforePlot execution")
    # values$rFrags<- read.table(values$refgenFrag, sep='\t', header=TRUE)
    # print("beforePlot execution 1")
    # values$rFragsGrange<-GenomicRanges::makeGRangesFromDataFrame(values$rFrags)
    # print("beforePlot execution 2")
    # values$paramFil <- diffHic::pairParam(values$rFragsGrange)
    # print("beforePlot execution 3")
    # values$diags <- diffHic::getPairData(values$h5_pa, values$paramFil)

    load(paste0(pointin(wdPath, "Pre-Processing", sys=TRUE ), "diags.Robj"))
    values$diags<-diags

    # print("beforePlot completed")
    #
    #
    # # Constructing the histograms for each orientation.
    #
    #questi vanno trasfromrati in widgets

    # output$startSlot<-shiny::renderUI({
    #     controlFile<-paste0(pointin(wdPath,"Filtering", sys=TRUE), "trimmed.h5")
    #     output$stateText<-shiny::renderText({
    #         "filtering already executed"
    #     })
    #     if (file.exists(controlFile)==TRUE){
    #         shiny::wellPanel(
    #             shiny::fluidRow(
    #                 shiny::column(12,
    #                             shiny::helpText("filtering already executed")
    #                               )
    #             ),
    #             shiny::fluidRow(
    #                 shiny::column(6,
    #                             shiny::actionButton(filns("reStart"),
    #                                                 label="Re-Start")
    #                             ),
    #                 shiny::column(6,
    #                             shiny::actionButton(filns("branch"),
    #                                                 label="branch"
    #                                                 )
    #                             )
    #             )
    #         )
    #     } else {
    #         shiny::wellPanel( "Filtering:" ,
    #             shiny::fluidRow(
    #                 shiny::column(12,
    #                               shiny::helpText("the mapped reads should be
    #                                             filtered to ensure that only
    #                                             informative and reliable
    #                                             read pairs pro- ceedto further
    #                                             analysis.")
    #                 )
    #             ),
    #             shiny::fluidRow(
    #                 shiny::column(6,
    #                               shiny::actionButton(filns("start"),
    #                                                   label="Start")
    #                 )
    #                 # ,shiny::column(6,
    #                 #               shiny::actionButton(filns("branch"),
    #                 #                                   label="branch"
    #                 #               )
    #                 # )
    #             )
    #         )
    #     }
    # })

    observeEvent(input$minOutward,{

        values$min.outward <- input$minOutward
    })

    observeEvent(input$minInward,{
        values$min.inward <- input$minInward
    })

    observeEvent(input$minInwardSlider,{
        values$min.inward <- input$minInwardSlider
    })

    observeEvent(input$minOutwardSlider,{
        values$min.outward <- input$minOutwardSlider
    })


    #
    # #=======================================================================
    values$llinsert <- log2(values$diags$insert + 1L)
    values$intra <- !is.na(values$llinsert)
    values$breaks <- seq(min(values$llinsert[values$intra]),
                         max(values$llinsert[values$intra]), length.out=30)
    values$inward <- hist(values$llinsert[values$diags$orientation==1L],
                        plot=FALSE, breaks=values$breaks)
    values$outward <- hist(values$llinsert[values$diags$orientation==2L] ,
                            plot=FALSE, breaks=values$breaks)
    values$samestr <- hist(values$llinsert[values$diags$orientation==0L | values$diags$orientation==3L],
                    plot=FALSE, breaks=values$breaks)
    values$samestr$counts <- values$samestr$counts/2
    # Setting up the axis limits.
    values$ymax <- max(values$inward$counts, values$outward$counts,
                        values$samestr$counts)/1e6
    values$xmax <- max(values$inward$mids, values$outward$mids,
                        values$samestr$mids)
    values$xmin <- min(values$inward$mids, values$outward$mids,
                        values$samestr$mids)

    ##=======================================================================

    # jpeg(file=paste0(pointin(wdPath, "Pre-Processing", sys=TRUE),"fragments.jpeg"))
    # hist(values$diags$length[diags$length < 1000], ylab="Frequency",
    #      xlab="Spacing (bp)", main="", col="grey80")
    # dev.off()
    #
    # jpeg(file=paste0(pointin(wdPath, "Pre-Processing", sys=TRUE),"inOutWard.jpeg"))
    # plot(0,0,type="n", xlim=c(values$xmin, values$xmax), ylim=c(0, values$ymax),
    #      xlab=expression(log[2]~"[insert size (bp)]"), ylab="Frequency (millions)")
    # lines(values$inward$mids, values$inward$counts/1e6, col="darkgreen", lwd=2)
    # #abline(v=log2(values$min.inward), col="darkgrey")
    # lines(values$outward$mids, values$outward$counts/1e6, col="red", lwd=2)
    # #abline(v=log2(values$min.outward), col="darkgrey", lty=2)
    # lines(values$samestr$mids, values$samestr$counts/1e6, col="blue", lwd=2)
    # legend("topright", c("inward", "outward", "same"),
    #        col=c("darkgreen", "red", "blue"), lwd=2)
    # dev.off()

    ##1) show  previous results results
    output$beforePlot<-shiny::renderPlot({

        shiny::withProgress(message="Making Plot", min=0, max=1, {

            # #=================================================================
            # values$llinsert <- log2(values$diags$insert + 1L)
            # shiny::setProgress(1)
            #
            # values$intra <- !is.na(values$llinsert)
            # shiny::setProgress(2)
            #
            # values$breaks <- seq(min(values$llinsert[values$intra]),
            #                      max(values$llinsert[values$intra]), length.out=30)
            # shiny::setProgress(3)
            #
            # values$inward <- hist(values$llinsert[values$diags$orientation==1L],
            #                       plot=FALSE, breaks=values$breaks)
            # shiny::setProgress(4)
            #
            # values$outward <- hist(values$llinsert[values$diags$orientation==2L] ,
            #                        plot=FALSE, breaks=values$breaks)
            # shiny::setProgress(5)
            #
            # values$samestr <- hist(values$llinsert[values$diags$orientation==0L
            #                         | values$diags$orientation==3L],
            #                        plot=FALSE, breaks=values$breaks)
            # shiny::setProgress(6)
            #
            # values$samestr$counts <- values$samestr$counts/2
            # shiny::setProgress(7)
            #
            # # Setting up the axis limits.
            # values$ymax <- max(values$inward$counts, values$outward$counts,
            #                    values$samestr$counts)/1e6
            # shiny::setProgress(8)
            #
            # values$xmax <- max(values$inward$mids, values$outward$mids,
            #                    values$samestr$mids)
            # shiny::setProgress(9)
            #
            # values$xmin <- min(values$inward$mids, values$outward$mids,
            #                    values$samestr$mids)
            # shiny::setProgress(10)
            #
            #

            #=================================================================
            #jpeg(file=paste0(pointin(wdPath, "Pre-Processing", sys=TRUE),"fragments.jpeg"))
            hist(values$diags$length[diags$length < 1000], ylab="Frequency",
                 xlab="Spacing (bp)", main="", col="grey80")
            })
            #dev.off()
        })

    output$beforePlot2<-shiny::renderPlot({
        #jpeg(file=paste0(pointin(wdPath, "Pre-Processing", sys=TRUE),"inOutWard.jpeg"))
        plot(0,0,type="n", xlim=c(values$xmin, values$xmax), ylim=c(0, values$ymax),
             xlab=expression(log[2]~"[insert size (bp)]"), ylab="Frequency (millions)")
        lines(values$inward$mids, values$inward$counts/1e6, col="darkgreen", lwd=2)
        abline(v=log2(values$min.inward), col="darkgrey")
        lines(values$outward$mids, values$outward$counts/1e6, col="red", lwd=2)
        abline(v=log2(values$min.outward), col="darkgrey", lty=2)
        lines(values$samestr$mids, values$samestr$counts/1e6, col="blue", lwd=2)
        legend("topright", c("inward", "outward", "same"),
               col=c("darkgreen", "red", "blue"), lwd=2)
        #dev.off()
    })


    observeEvent(input$filBut,{

        busyIndServer("filBut",{

            shiny::withProgress("filtering start", min=0, max=13 , {
                shiny::setProgress(message = 'Filtering in progress',
                                   detail = 'This may take a while...')
                rFrags<- read.table(values$refgenFrag, sep='\t', header=TRUE)
                setProgress(value = 1)
                #     #print (rFrags)
                rFragsGrange<-GenomicRanges::makeGRangesFromDataFrame(rFrags)
                setProgress(value = 2)
                paramFil <- diffHic::pairParam(rFragsGrange)
                setProgress(value = 3)
                print(paste0("min.inward=",values$min.inward))
                print(paste0("min.outward=",values$min.outward))
                counted <- diffHic::prunePairs(paste0(pointin(wdPath, "Pre-Processing", sys=TRUE)
                                                      ,"h5file.h5"),
                                               paramFil,
                                               file.out= paste0(pointin(wdPath,
                                                                        "Filtering",
                                                                        sys=TRUE),
                                                                "trimmed.h5"
                                               ),
                                               max.frag=input$maxFrag,
                                               min.inward=values$min.inward,
                                               min.outward=values$min.outward)
                setProgress(value = 4)

                rFrags<- read.table(paste0(pointin(wdPath,"Pre-Processing", sys = TRUE),
                                           "refGenFrag.cutGenome.tsv"), sep='\t', header=TRUE)
                #print("beforePlot execution 1")
                setProgress(value = 5)

                rFragsGrange<-GenomicRanges::makeGRangesFromDataFrame(rFrags)
                #print("beforePlot execution 2")
                setProgress(value = 6)
                paramFil <- diffHic::pairParam(rFragsGrange)
                setProgress(value = 7)
                #print("beforePlot execution 3")
                diags <- diffHic::getPairData(paste0(pointin(wdPath,"Filtering", sys = TRUE),
                                                     "trimmed.h5"),
                                              paramFil)
                setProgress(value = 8)
                #print("beforePlot execution 4")
                save(diags, file=paste0(pointin(wdPath,"Filtering", sys = TRUE),"diags.Robj"))
                setProgress(value = 9)

                load(paste0(pointin(wdPath, "Filtering", sys=TRUE ), "diags.Robj"))
                values$diags<-diags
                setProgress(value = 10)

                values$llinsert <- log2(values$diags$insert + 1L)


                values$intra <- !is.na(values$llinsert)

                values$breaks <- seq(min(values$llinsert[values$intra]),
                                     max(values$llinsert[values$intra]), length.out=30)

                values$inward <- hist(values$llinsert[values$diags$orientation==1L],
                                      plot=FALSE, breaks=values$breaks)

                values$outward <- hist(values$llinsert[values$diags$orientation==2L] ,
                                       plot=FALSE, breaks=values$breaks)
                values$samestr <- hist(values$llinsert[values$diags$orientation==0L | values$diags$orientation==3L],
                                       plot=FALSE, breaks=values$breaks)
                values$samestr$counts <- values$samestr$counts/2
                # Setting up the axis limits.
                values$ymax <- max(values$inward$counts, values$outward$counts,
                                   values$samestr$counts)/1e6
                values$xmax <- max(values$inward$mids, values$outward$mids,
                                   values$samestr$mids)
                values$xmin <- min(values$inward$mids, values$outward$mids,
                                   values$samestr$mids)

                setProgress(value = 11)
                ##=======================================================================

                jpeg(file=paste0(pointin(wdPath, "Filtering", sys=TRUE),"fragments_trimmed.jpeg"))
                hist(values$diags$length[diags$length < 1000], ylab="Frequency",
                     xlab="Spacing (bp)", main="", col="grey80")
                dev.off()


                jpeg(file=paste0(pointin(wdPath, "Filtering", sys=TRUE),"inOutWard_trimmed.jpeg"))
                plot(0,0,type="n", xlim=c(values$xmin, values$xmax), ylim=c(0, values$ymax),
                     xlab=expression(log[2]~"[insert size (bp)]"), ylab="Frequency (millions)")
                lines(values$inward$mids, values$inward$counts/1e6, col="darkgreen", lwd=2)
                abline(v=log2(values$min.inward), col="darkgrey")
                lines(values$outward$mids, values$outward$counts/1e6, col="red", lwd=2)
                abline(v=log2(values$min.outward), col="darkgrey", lty=2)
                lines(values$samestr$mids, values$samestr$counts/1e6, col="blue", lwd=2)
                legend("topright", c("inward", "outward", "same"),
                       col=c("darkgreen", "red", "blue"), lwd=2)
                dev.off()

                ##1) show  previous results results
                output$beforePlot<-shiny::renderPlot({

                    shiny::withProgress(message="Making Plot", min=0, max=1, {

                        hist(values$diags$length[diags$length < 1000], ylab="Frequency",
                             xlab="Spacing (bp)", main="", col="grey80")
                    })
                })

                setProgress(value = 12)

                output$beforePlot2<-shiny::renderPlot({
                    plot(0,0,type="n", xlim=c(values$xmin, values$xmax), ylim=c(0, values$ymax),
                         xlab=expression(log[2]~"[insert size (bp)]"), ylab="Frequency (millions)")
                    lines(values$inward$mids, values$inward$counts/1e6, col="darkgreen", lwd=2)
                    abline(v=log2(values$min.inward), col="darkgrey")
                    lines(values$outward$mids, values$outward$counts/1e6, col="red", lwd=2)
                    abline(v=log2(values$min.outward), col="darkgrey", lty=2)
                    lines(values$samestr$mids, values$samestr$counts/1e6, col="blue", lwd=2)
                    legend("topright", c("inward", "outward", "same"),
                           col=c("darkgreen", "red", "blue"), lwd=2)
                })

                #saveParameters
                filteringTable<-matrix(ncol=2, nrow=3)
                filteringTable<-matrix(ncol=1, nrow=3)
                rownames(filteringTable)<-c("maxFragLength", "minInward", "minOutward")
                if (input$fixValue==FALSE){
                    mini=input$minInwardSlider
                    mino=input$minOutwardSlider
                } else {
                    mini=input$minInward
                    mino=input$minOutward
                }
                filteringTable[,1]<-c(input$maxFrag, mini, mino)
                print(paste("writing filtering parameters",pointin(wdPath, "Filtering", sys=TRUE)))
                write.table(filteringTable, paste0(pointin(wdPath, "Filtering", sys=TRUE),"parameter.tsv"), sep="\t", quote=FALSE, col.names=FALSE)
                print("filtering parameters writted")


                setProgress(value = 13)

            })






        })


    })



    # shiny::observeEvent (input$fileVSprevious,{
    #
    #         output$h5<- shiny::renderUI ({
    #             shiny::fileInput(filns("h5"), label = h6("h5 file"))
    #         })
    #
    #     if (input$fileVSprevious==1){
    #
    #         output$refgenFrag <- shiny::renderUI ({
    #             selectFile (filns("refgenFrag"),path=paste0(anFolder$hisave,'Pre-Processing/'))
    #             #fileInput(filns("refgenFrag"), label = h6("reference genome frags"))
    #         })
    #
    #     } else {
    #         output$refgenFrag <- shiny::renderUI ({
    #             #fileInput(filns("refgenFrag"), label = h6("prova"))
    #         })
    #
    #     }
    #
    # })



    # shiny::observeEvent (input$startbut,{
    #
    #     fileVSprevious<-1
    #     print ('startBut.....pressed ')
    #     #if (input$checkbox==TRUE){
    #     #
    #     #} else {
    #     #  refFrags<<-input$refgenFrag$datapath
    #     #  h5file<<-input$h5$datapath
    #     #}
    #
    #     #values$h5_pa<-input$h5
    #     # values$h5_pa<-paste0(pointin(wdPath, "Pre-Processing", sys=TRUE),
    #     #                     "h5file.h5")
    #     # print("h5_pa:   ",values$h5_pa)
    #
    #     #h5file<<- paste0 ( anFolder$hisave, 'Pre-Processing/' ,input$h5)
    #     infoImport<- HCRread(file='info.tsv', path=wdPath, header=FALSE)
    #     print ('info.....imported')
    #     #bin.size<<-as.numeric(infoImport[2,1])
    #     #bin.size<<-input$text
    #
    #     #values$refgenFrag_pa<- input$refgenFrag
    #     # values$refgenFrag<-paste0(pointin(wdPath, "Pre-Processing", sys=TRUE),
    #     #                         "refGenFrag.cutGenome.tsv")
    #     # print(paste0("refgenFrag:   ", values$refgenFrag))
    #     #refFrags<<- paste0 (anFolder$hisave, 'Pre-Processing/' ,input$refgenFrag)
    #
    #     rFrags<- read.table(values$refgenFrag, sep='\t', header=TRUE)
    #     #print (rFrags)
    #     rFragsGrange<-GenomicRanges::makeGRangesFromDataFrame(rFrags)
    #     paramFil <- diffHic::pairParam(rFragsGrange)
    #
    #
    #     # output$NBbut <-renderUI ({
    #     #   actionButton (filns('NButton'), label = 'get Pair Data')
    #     # })
    #
    #     #In this model, the overall NB mean across all libraries is (probably) an independent
    #     #filter statistic. The log-mean-per-million is known as the average abundance and
    #     #can be computed with the aveLogCPM function in edgeR
    #     print ('get Pair Data.....START')
    #
    #     #diags<<- getPairData (h5file, paramFil)
    #
    #     diags<- diffHic::getPairData (values$h5_pa, paramFil)
    #
    #     output$LEhist <- shiny::renderPlot({
    #         hist(diags$length[diags$length < 1000], ylab='frequency', xlab='Spacing (bp)' )
    #     })
    #
    #     output$minInwardSlot<- shiny::renderUI ({
    #         numericInput (filns('minInward'), label=h5('min.inward'), value = 1000 )
    #     })
    #
    #     output$minOutwardSlot<- shiny::renderUI ({
    #         numericInput (filns('minOutward'), label=h5('min.Outward'), value = 25000 )
    #     })
    #
    #     output$lengthSlot<- shiny::renderUI ({
    #         numericInput (filns('fragLength'), label=h5('fragment length'), value = 600 )
    #     })
    #
    #     output$filteringButSlot<- shiny::renderUI ({
    #         actionButton (filns('filteringButton'), label=h5('filtering'))
    #     })
    #
    #     # output$viewButSlot<- renderUI ({
    #     #   actionButton (filns('viewButton'), label=h5('View'))
    #     # })
    #
    #     intra <- !is.na(diags$insert)
    #     #min.inward <<-  1000  #as.numeric(input$minInward)
    #     #min.outward <<- 25000 #as.numeric(input$minOutward)
    #
    #
    #
    #     #print (c('minInward', 'minOutward', min.inward, min.outward))
    #
    #     llinsert <- log2(diags$insert + 1L)
    #     values$llinsert2<-llinsert
    #
    #     intra <- !is.na(llinsert)
    #     values$intra2 <- intra
    #
    #     breaks <- seq(min(llinsert[intra]), max(llinsert[intra]), length.out=30)
    #     values$breaks2 <- breaks
    #
    #     inward <- hist(llinsert[diags$orientation==1L], plot=FALSE, breaks=breaks)
    #     values$inward2<- inward
    #
    #     outward <- hist(llinsert[diags$orientation==2L] ,plot=FALSE, breaks=breaks)
    #     values$outward2<- outward
    #
    #     #samestr <- hist(llinsert[diags$orientation==0L | diags$orientation==3L], plot=FALSE, breaks=breaks)
    #     #values$samestr2<-samestr
    #
    #     #samestr$counts <- samestr$counts/2
    #
    #
    #     # ymax <- max(inward$counts, outward$counts, samestr$counts)/1e6
    #     # xmax <- max(inward$mids, outward$mids, samestr$mids)
    #     # xmin <- min(inward$mids, outward$mids, samestr$mids)
    #     ymax <- max(inward$counts, outward$counts)/1e6
    #     xmax <- max(inward$mids, outward$mids)
    #     xmin <- min(inward$mids, outward$mids)
    #
    #     output$degSelfGraph <- shiny::renderPlot ({
    #
    #         plot(0,0,type="n", xlim=c(xmin, xmax), ylim=c(0, ymax), xlab=expression(log[2]~"[insert size (bp)]"), ylab="Frequency (millions)")
    #         lines(inward$mids, inward$counts/1e6, col="darkgreen", lwd=2)
    #         abline(v=log2(min.inward), col="darkgrey")
    #         lines(outward$mids, outward$counts/1e6, col="red", lwd=2)
    #         abline(v=log2(min.outward), col="darkgrey", lty=2)
    #         # lines(samestr$mids, samestr$counts/1e6, col="blue", lwd=2)
    #         # legend("topright", c("inward", "outward", "same"), col=c("darkgreen", "red", "blue"), lwd=2)
    #
    #         legend("topright", c("inward", "outward"), col=c("darkgreen", "red"), lwd=2)
    #
    #
    #     })
    #
    #     output$mainPanel<- shiny::renderUI({
    #         shiny::wellPanel(
    #
    #             ##  selectInput(filns("fileVSprevious"), label = h5("Select box"),
    #             ##              choices = list("load from files" = 1, "load previous" = 2),
    #             ##              selected = 1),
    #             #fileInput(filns("refgenFrag"), label = h6("reference genome frags")),
    #             #fileInput(filns("h5"), label = h6("h5 file")),
    #             ###uiOutput(filns('refgenFrag')),
    #             #selectFile (filns("refgenFrag"),path=wsFolder$hisave),
    #
    #             uiOutput(filns('h5')),
    #             #textInput(filns('text'),label=h6('bin size'), value="1e5"),
    #             #checkboxInput("checkbox", label = "no data load", value = TRUE),
    #             #actionButton(filns('startbut'), 'import')
    #             shiny::br(),
    #             shiny::fluidRow (
    #                 shiny::column (1, shiny::uiOutput(filns('NBbut')))
    #
    #             ),
    #
    #             shiny::br(),
    #
    #             shiny::fluidRow(
    #
    #                 shiny::column (12, shiny::uiOutput(filns('lengthSlot')))
    #
    #             ),
    #
    #             shiny::fluidRow (
    #                 shiny::column (12, shiny::uiOutput (filns('minInwardSlot')))
    #             ),
    #
    #             shiny::fluidRow (
    #
    #                 shiny::column (12, shiny::uiOutput(filns('minOutwardSlot')))
    #
    #             ),
    #
    #
    #             shiny::fluidRow (
    #
    #                 shiny::column (12, shiny::uiOutput(filns('viewButSlot')))
    #
    #             ),
    #
    #             shiny::fluidRow (
    #                 shiny::column (12, shiny::uiOutput(filns('filteringButSlot')))
    #             )
    #
    #
    #         )
    #     })
    #
    #
    #     output$mainscreen<- shiny::renderUI ({
    #         shiny::tabsetPanel (
    #             shiny::tabPanel('reads length',
    #                             shiny::plotOutput(filns('LEhist')),
    #                             plotOutput(filns ('degSelfGraph'))
    #             )
    #             # ,tabPanel('direct filtering', textOutput(filns('difiVal')),
    #             #          fluidRow(column(6,uiOutput(filns('MFCval'))),column(6,br(),br(),uiOutput(filns('MFCBut'))))
    #             #          ,tableOutput (filns('sum2'))
    #             # )
    #         )
    #     })
    #
    #
    #
    #
    # }
    #
    # )
    #
    #
    # shiny::observeEvent(input$NBfilButton,{
    #     count.keep <- ave.ab >= aveLogCPM(5, lib.size=mean(data$totals))
    #     summary<-capture.output(as.data.frame(show (summary(count.keep))))
    #     summama<- matrix (nrow=2, ncol=4)
    #     summaryA<- strsplit (summary[1], split=' ')
    #     summaryA<- as.vector (summaryA[[1]])
    #     summaryA<- summaryA[summaryA != ""]
    #     summaryB<- strsplit (summary[2],split=' ')
    #     summaryB<- as.vector (summaryB[[1]])
    #     summaryB<- summaryB[summaryB != ""]
    #     print(summaryA)
    #     print(summaryB)
    #     summama[1,]<-summaryA
    #     summama[2,]<-summaryB
    #     print (summama)
    #     output$sum<- renderTable ({
    #         summama
    #     })
    #
    #
    #
    # })
    #
    # shiny::observeEvent (input$minFoldButton,{
    #     direct.keep <- direct$abundances > log2(5) + direct$threshold
    #     summary<-capture.output(as.data.frame(show (summary(direct.keep))))
    #     print (length(summary))
    #     #    summaryC<- strsplit (summary[1], split=' ')
    #     #    summaryC<- as.vector (summaryC[[1]])
    #     #    summaryC<- summaryC[summaryC != ""]
    #     #    lensumC<- length (summaryC)
    #     #    summaryD<- strsplit (summary[2],split=' ')
    #     #    summaryD<- as.vector (summaryD[[1]])
    #     #    summaryD<- summaryD[summaryD != ""]
    #     #    lensumD<- length (summaryD)
    #     #    if (lensumC>=lensumD){
    #     #      summama2<- matrix (nrow=2, ncol=lensumC)
    #     #    } else {summama2<- matrix (nrow=2, ncol=lensumD)}
    #
    #     #    summama2[1,]<-summaryC
    #     #    summama2[2,]<-summaryD
    #     output$sum2<- renderTable ({
    #         summama2<- tableFromConsole (summary(direct.keep), 1)
    #     })
    # })
    #
    # shiny::observeEvent (input$viewButton, {
    #
    #     ##diags<- getPairData (h5file, paramFil)
    #     print ('viewbutton is pressed')
    #     intra <- !is.na(diags$insert)
    #     min.inward <<-  as.numeric(input$minInward)
    #     min.outward <<- as.numeric(input$minOutward)
    #
    #     print (c('minInward', 'minOutward', min.inward, min.outward))
    #     #print (input$minOutwardSlot)
    #     #print (min.inward)
    #     #print (min.outward)
    #     llinsert <- values$llinsert  #log2(diags$insert + 1L)
    #     intra <-  !is.na(llinsert)
    #
    #     breaks <- values$breaks2 #seq(min(llinsert[intra]), max(llinsert[intra]), length.out=30)
    #
    #     values$inward <- values$inward2 #hist(llinsert[diags$orientation==1L], plot=FALSE, breaks=breaks)
    #     values$outward <- values$outward2 #hist(llinsert[diags$orientation==2L] ,plot=FALSE, breaks=breaks)
    #     #samestr <- values$samestr2 #hist(llinsert[diags$orientation==0L | diags$orientation==3L], plot=FALSE, breaks=breaks)
    #     #samestr$counts <- samestr$counts/2
    #
    #     # ymax <- max(inward$counts, outward$counts, samestr$counts)/1e6
    #     # xmax <- max(inward$mids, outward$mids, samestr$mids)
    #     # xmin <- min(inward$mids, outward$mids, samestr$mids)
    #
    #     ymax <- max(inward$counts, outward$counts)/1e6
    #     xmax <- max(inward$mids, outward$mids)
    #     xmin <- min(inward$mids, outward$mids)
    #
    #
    #     output$degSelfGraph <- shiny::renderPlot ({
    #
    #         plot(0,0,type="n", xlim=c(xmin, xmax), ylim=c(0, ymax), xlab=expression(log[2]~"[insert size (bp)]"), ylab="Frequency (millions)")
    #         lines(inward$mids, inward$counts/1e6, col="darkgreen", lwd=2)
    #         abline(v=log2(min.inward), col="darkgrey")
    #         lines(outward$mids, outward$counts/1e6, col="red", lwd=2)
    #         abline(v=log2(min.outward), col="darkgrey", lty=2)
    #         #lines(samestr$mids, samestr$counts/1e6, col="blue", lwd=2)
    #         #legend("topright", c("inward", "outward", "same"), col=c("darkgreen", "red", "blue"), lwd=2)
    #         legend("topright", c("inward", "outward"), col=c("darkgreen", "red"), lwd=2)
    #
    #     })
    #
    #
    # })

    shiny::observeEvent(input$filteringButton,{    #deprecated button see filBut

        print ('filtering.....START')
        ##ATTENZIONE: FILTRA SOLO A 600 DI max.frag
        fileName<-paste0(anFolder$hisave,'Pre-Processing/',sub('.h5', '' ,values$h5_pa, fixed=TRUE),'_trimmed.h5')
        if (min.inward==0){min.inward=NA}
        if (min.outward==0){min.outward=NA}


        filtered<- prunePairs (h5file, paramFil, file.out='trimmed.h5', max.frag=input$fragLength, min.inward, min.outward)

        file.copy("./trimmed.h5", fileName, overwrite = TRUE)

        file.remove("trimmed.h5")

        #filtered<- prunePairs (h5file, paramFil, file.out=fileName, max.frag=input$fragLength, min.inward, min.outward)
        #print(paste0(anFolder$hisave,'Pre-Processing/',sub('.h5', '' ,input$h5, fixed=TRUE),'_trimmed.h5','......CREATED'))
        print(paste0(fileName,'......CREATED'))
        print ('filtering.....END')

        #saveParameters
         filteringTable<-matrix(ncol=2, nrow=3)
         filteringTable<-matrix(ncol=1, nrow=3)
         rownames(filteringTable)<-c("maxFragLength", "minInward", "minOutward")
         if (input$fixValue==FALSE){
             mini=input$minInwardSlider
             mino=input$minOutwardSlider
         } else {
             mini=input$minInward
             mino=input$minOutward
         }
         filteringTable[,1]<-c(input$maxFrag, mini, mino)
         print(paste("writing filtering parameters",pointin(wdPath, "Filtering", sys=TRUE)))
         write.table(filteringTable, paste0(pointin(wdPath, "Filtering", sys=TRUE),"parameter.tsv"), sep="\t", quote=FALSE, col.names=FALSE)
         print("filtering parameters writted")


        output$mainscreen<- renderUI ({
            wellPanel (helpText('filtering Done, please if you want see results load trimmed h5 file'))
        })

    })

    shiny::observeEvent(input$minInward,{
        ##diags<- getPairData (h5file, paramFil)
        #print ('viewbutton is pressed')
        intra <- !is.na(diags$insert)
        min.inward <-  as.numeric(input$minInward)
        min.outward <- as.numeric(input$minOutward)

        print (c('minInward', 'minOutward', min.inward, min.outward))
        #print (input$minOutwardSlot)
        #print (min.inward)
        #print (min.outward)
        llinsert <- values$llinsert  #log2(diags$insert + 1L)
        intra <-  !is.na(llinsert)

        breaks <- values$breaks2 #seq(min(llinsert[intra]), max(llinsert[intra]), length.out=30)

        inward <- values$inward2 #hist(llinsert[diags$orientation==1L], plot=FALSE, breaks=breaks)
        outward <- values$outward2 #hist(llinsert[diags$orientation==2L] ,plot=FALSE, breaks=breaks)
        samestr <- values$samestr2 #hist(llinsert[diags$orientation==0L | diags$orientation==3L], plot=FALSE, breaks=breaks)
        samestr$counts <- samestr$counts/2

        # ymax <- max(inward$counts, outward$counts, samestr$counts)/1e6
        # xmax <- max(inward$mids, outward$mids, samestr$mids)
        # xmin <- min(inward$mids, outward$mids, samestr$mids)

        ymax <- max(inward$counts, outward$counts)/1e6
        xmax <- max(inward$mids, outward$mids)
        xmin <- min(inward$mids, outward$mids)



        output$degSelfGraph <- shiny::renderPlot ({

            plot(0,0,type="n", xlim=c(xmin, xmax), ylim=c(0, ymax), xlab=expression(log[2]~"[insert size (bp)]"), ylab="Frequency (millions)")
            lines(inward$mids, inward$counts/1e6, col="darkgreen", lwd=2)
            abline(v=log2(min.inward), col="darkgrey")
            lines(outward$mids, outward$counts/1e6, col="red", lwd=2)
            abline(v=log2(min.outward), col="darkgrey", lty=2)
            #lines(samestr$mids, samestr$counts/1e6, col="blue", lwd=2)
            #legend("topright", c("inward", "outward", "same"), col=c("darkgreen", "red", "blue"), lwd=2)
            legend("topright", c("inward", "outward"), col=c("darkgreen", "red"), lwd=2)

        })
    })

    shiny::observeEvent(input$minOutward, {
        ##diags<- getPairData (h5file, paramFil)
        #print ('viewbutton is pressed')
        intra <- !is.na(diags$insert)
        #min.inward <<-  as.numeric(input$minInward)
        #min.outward <<- as.numeric(input$minOutward)

        #print (c('minInward', 'minOutward', min.inward, min.outward))
        #print (input$minOutwardSlot)
        #print (min.inward)
        #print (min.outward)
        llinsert <- values$llinsert  #log2(diags$insert + 1L)
        intra <-  !is.na(llinsert)

        breaks <- values$breaks2 #seq(min(llinsert[intra]), max(llinsert[intra]), length.out=30)

        inward <- values$inward2 #hist(llinsert[diags$orientation==1L], plot=FALSE, breaks=breaks)
        outward <- values$outward2 #hist(llinsert[diags$orientation==2L] ,plot=FALSE, breaks=breaks)
        samestr <- values$samestr2 #hist(llinsert[diags$orientation==0L | diags$orientation==3L], plot=FALSE, breaks=breaks)
        samestr$counts <- samestr$counts/2

        # ymax <- max(inward$counts, outward$counts, samestr$counts)/1e6
        # xmax <- max(inward$mids, outward$mids, samestr$mids)
        # xmin <- min(inward$mids, outward$mids, samestr$mids)

        ymax <- max(inward$counts, outward$counts)/1e6
        xmax <- max(inward$mids, outward$mids)
        xmin <- min(inward$mids, outward$mids)



        filPlot <- shiny::renderPlot({
            #jpeg(file=paste0(pointin(wdPath, "Pre-Processing", sys=TRUE),"filplot.jpeg"))
            plot(0,0,type="n", xlim=c(xmin, xmax), ylim=c(0, ymax), xlab=expression(log[2]~"[insert size (bp)]"), ylab="Frequency (millions)")
            lines(inward$mids, inward$counts/1e6, col="darkgreen", lwd=2)
            abline(v=log2(min.inward), col="darkgrey")
            lines(outward$mids, outward$counts/1e6, col="red", lwd=2)
            abline(v=log2(min.outward), col="darkgrey", lty=2)
            #lines(samestr$mids, samestr$counts/1e6, col="blue", lwd=2)
            #legend("topright", c("inward", "outward", "same"), col=c("darkgreen", "red", "blue"), lwd=2)
            legend("topright", c("inward", "outward"), col=c("darkgreen", "red", "blue"), lwd=2)
            #dev.off()

        })

        #save (filPlot, file=paste0(pointin(wdPath, "Pre-Processing", sys=TRUE),"filplot.Robj"))



        output$degSelfGraph<- filPlot



    })


}




