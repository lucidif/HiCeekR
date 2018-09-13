##DiffHiC_Binning




#' DiffHiC_BinningV2_UI
#'
#' @param id
#' @param label
#'
#' @return
#' @keywords internal
#'
#' @examples
DiffHiC_BinningV2_UI <- function(id, label = "binning") {
    binns <- NS(id)
    print (binns)
    shiny::fluidPage(

        # fluidRow (
        #     column (12,
        #             wellPanel(
        #
        #                 fluidRow (
        #
        #                     column (3, br(), helpText('select cutGenome file') ),
        #                     column (9, selectFile (binns("refgenFrag"),pointin(anFolder$hisave,'Pre-Processing')
        #                                            ,label="",subset=TRUE,pattern='.cutGenome'))
        #
        #                 ),
        #                 fluidRow (
        #
        #                     column (3, br(), helpText('select h5 file')),
        #                     column (9,  selectFile (binns("h5"),pointin(anFolder$hisave,'Pre-Processing'),
        #                                             label="",subset=TRUE, pattern='.h5'))
        #                 )
        #             ))
        # ),

        shiny::fluidRow(

            shiny::column (2,

                        shiny::wellPanel(


                            shiny::fluidRow(

                                shiny::column(6,
                                    shiny::numericInput (binns('filterValue'),
                                                        label='min frags number'
                                                        ,value = 1)
                            ),

                                shiny::column(6,
                                    shiny::checkboxInput(binns("filtered"),
                                                        "filtered data",
                                                        value=TRUE)
                                )

                            ),

                        shiny::uiOutput(binns('refgenFrag')),


                        shiny::uiOutput(binns('h5')),

                        busyIndUI(shiny::actionButton(binns('startbut'), 'start'))
                        ,

                        shiny::br(), shiny::br(),
                        shiny::fluidRow (
                            shiny::column (1,
                                shiny::uiOutput(binns('NBbut'))
                                )

                        ),
                        shiny::br(), shiny::br(),
                        shiny::fluidRow(
                            shiny::column(1,
                                shiny::uiOutput(binns('Export'))
                                )

                        )

                    )

            ),


            shiny::column (10,

                    shiny::fluidRow(
                        shiny::column(12,
                            shiny::uiOutput(binns('tableOpt_slot')))),
                    shiny::fluidRow(
                        shiny::column(12,
                            shiny::uiOutput(binns('mainscreen'))))
                    #fluidRow(column(12, dataTableOutput (binns('table_slot'))))

            )
        )

    )

}

#==================================================================================


# Module server function

#' DiffHiC_BinningV2_Server
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
DiffHiC_BinningV2_Server <- function(input, output, session, stringsAsFactors,
                                    wdPath) {

    binns<-session$ns

    binSaved<- shiny::reactiveValues (chrNames='none', S_redata='none', reload="FALSE")



    #========================================================================

    # > binningTable<-matrix(ncol=1,nrow=2)
    # > rownames(binningTable)<-c("minFragNumber","dataType")
    # > binningTable[,1]<-c("1","filtered")
    # > write.table(binningTable, paste0(pointin(wdPath, "Binning", sys=TRUE),"parameter.tsv"), sep="\t", quote=FALSE, col.names=FALSE)
    #
    #=======================================================================


    shiny::observeEvent(input$startbut,{

        busyIndServer("startbut",{

        print ('binning start')

        #h5file<<-input$h5

        if (input$filtered == TRUE){
            binSaved$h5file<-paste0(pointin(wdPath,"Filtering", sys=TRUE),
                                    "trimmed.h5")
            #binSaved$h5file<-"trimmed.h5"
        } else {
            binSaved$h5file<-paste0(pointin(wdPath,"Pre-Processing", sys=TRUE),
                                    "h5file.h5")

            #binSaved$h5file<-"h5file.h5"
        }

        infoImport<- HCRread(file='info.tsv', path=wdPath, header=FALSE)
        print ('info.....imported')
        binSaved$bin.size<-as.numeric(infoImport[2,2])
        print (c('bin.size: ', binSaved$bin.size, typeof(binSaved$bin.size)))


        binSaved$refFrags<-paste0(pointin(wdPath,'Pre-Processing',
                                    sys=TRUE), "refGenFrag.cutGenome.tsv")

        binSaved$chr<-importChrFromFragList(binSaved$refFrags)
        binSaved$chr<-binSaved$chr[2:length(binSaved$chr)]
        #binSaved$chr<-c('all',binSaved$chr)

        ##########################################
        print('sto per importare param')
        rFrags<-read.table (binSaved$refFrags, sep='\t', header=TRUE)
        rFragsGrange<-GenomicRanges::makeGRangesFromDataFrame(rFrags)
        binSaved$paramFil <- diffHic::pairParam(rFragsGrange)


        print('param importato')


        #    sqcount<<- HiClass$new ('squareCount per plot', redata)

        print ('squareCount.....start')
        print (c('bin.size', binSaved$bin.size ))
        binSaved$redata <- diffHic::squareCounts(
                                        binSaved$h5file,
                                        binSaved$paramFil,
                                        width=binSaved$bin.size,
                                        filter=input$filterValue)

        print ('squareCounts.....OK')
        intSet<-binSaved$redata
        print(paste0("savePa:", paste0(pointin(wdPath, "Binning", sys=TRUE),"intSet.Robj")))
        save(intSet, file=paste0(pointin(wdPath, "Binning", sys=TRUE),"intSet.Robj"))
        rm(intSet)
        #redataTT<-binSaved$redata

        chrInt1=binSaved$chr[2]
        chrInt2=binSaved$chr[2]

        conMatrix= extractContact(binSaved$redata,chrInt1,chrInt2)

        print ('contact matrix extracted')

        ##RENDERS================================================

        output$tableOpt_slot<- shiny::renderUI ({
            shiny::wellPanel(

                shiny::fluidRow(
                    shiny::column(4,
                                  shiny::selectInput(binns("chrInt1Box"),
                                            label = h5("first chromosome"),
                                            choices = binSaved$chr,
                                            selected = binSaved$chr[2])
                ),
                shiny::column(4,
                              shiny::selectInput(binns("chrInt2Box"),
                                    label = h5("second chromosome"),
                                    choices = binSaved$chr,
                                    selected = binSaved$chr[2])
                )))
        })

        output$mainscreen <- shiny::renderUI({

            shiny::fluidRow(
                shiny::column (12,

                    shiny::fluidRow (
                        shiny::column(12,
                            shiny::wellPanel(
                                shiny::fluidRow(
                                                        # column(4,
                                                        #         selectInput(binns("chrInt1Box"), label = h5("first chromosome"),
                                                        #                     choices = binSaved$chr,
                                                        #                     selected = 1)
                                                        #         ),
                                                        #  column(4,
                                                        #         selectInput(binns("chrInt2Box"), label = h5("second chromosome"),
                                                        #                     choices = binSaved$chr,
                                                        #                     selected = 1)
                                                        #         ),

                                    shiny::column(4,
                                        shiny::actionButton (binns('showInt'),
                                                             label='show' )
                                                        )

                                                    )

                                                )
                              )),

                                shiny::fluidRow (

                                    shiny::dataTableOutput(binns('table_slot'))

                              )

            ))

        })

        output$table_slot <- shiny::renderDataTable ({
            conMatrix
        })

        output$Export <-shiny::renderUI ({
            shiny::actionButton (binns('exportResults'), label = 'Export')
        })


        })

        #saveParameters
        binningTable<-matrix(ncol=1,nrow=2)
        rownames(binningTable)<-c("minFragNumber","dataType")
        if (input$filtered==TRUE){
            dtype<-"filtered"
        } else {
            dtype<-"noFilter"
        }
        binningTable[,1]<-c(input$filterValue,dtype)
        write.table(binningTable, paste0(pointin(wdPath, "Binning", sys=TRUE),"parameter.tsv"),
                    sep="\t", quote=FALSE, col.names=FALSE)



    })

    shiny::observeEvent(input$showInt,{
        print ('recalculate contact regions of interest')
        conMatrix <- extractContact(binSaved$redata, input$chrInt1Box, input$chrInt2Box )
        output$table_slot <- shiny::renderDataTable ({
            conMatrix
        })
        print ('recalculate SUCCESS')

    })

    shiny::observeEvent(input$exportResults,{

        output$mainscreen <- shiny::renderUI ({

            shiny::wellPanel (


                shiny::fluidRow (
                    # shiny::column(4,
                    #     shiny::checkboxInput(binns("allRegBinTable_check"),
                    #                         label = h5("all Region bin table"),
                    #                         value = TRUE)),
                    shiny::column(4,
                        shiny::checkboxInput(binns("selectRegBinTable_check"),
                                            label = h5("selective bin table"),
                                            value = FALSE)),
                    shiny::column(4,
                        shiny::checkboxInput(binns("squareCount_check"),
                                            label = h5("squareCount R object"),
                                            value = TRUE))
                ),

                shiny::fluidRow (
                    shiny::column(4,
                        shiny::checkboxInput(binns("contactMatrix_check"),
                                             label = h5("Contact Matrix"),
                                             value = TRUE)),
                    #column(4,checkboxInput(binns("selectRegBinTable_check"), label = "selective bin table", value = FALSE)),
                    # shiny::column(4,
                    #     shiny::checkboxInput(binns("contactTable_check"),
                    #                          label = h5("Contact Table"),
                    #                          value = TRUE)),
                    shiny::column(4,
                        shiny::checkboxInput(binns("prefix_check"),
                                             label = h5("add prefix"),
                                             value = FALSE))
                ),

                shiny::fluidRow (
                    shiny::column (6,
                        shiny::textInput(binns('prefix'),
                                         label=h5('prefix'),
                                         value='')
                    ),
                    shiny::column (6, shiny::br(),
                        shiny::actionButton(binns('saveResults'), label='save')
                    )
                )




            )
        })

    })

    shiny::observeEvent(input$saveResults,{
        print ('saving.....START')
        #a questo nome a seconda del dato da salvare puoi agiungere l'estensione in base al file da generare


        #===========================================================

        if (input$prefix_check=='TRUE'){
            filenameCommon<-paste0( input$prefix , '_' ,
                                    input$chrInt1Box,'_VS_',
                                    input$chrInt2Box,'_' ,
                                    obtainName (wdPath))
        } else {
            filenameCommon<-paste0( input$chrInt1Box,
                                    '_VS_',input$chrInt2Box,
                                    '_' ,obtainName(wdPath))
        }


        #not Selectable results==============================================
        HCRwrite (InteractionSet::regions(binSaved$redata),
                  file='allRegions.bint.bed' ,
                  path=pointin(wdPath, 'Binning', sys=TRUE),
                  col.names=FALSE,
                  extension=FALSE )
        HCRwrite (InteractionSet::regions(binSaved$redata),
                  file='allRegions.bint.bed' ,
                  path=pointin(wdPath, 'Binning', sys=FALSE),
                  col.names=FALSE,
                  extension=FALSE )

        filename<-paste0(filenameCommon,'_conTable')
        conTab<-extractContact(binSaved$redata,input$chrInt1Box,input$chrInt2Box)
        HCRwrite (conTab,
                  file=filename ,
                  path=pointin(wdPath,'Binning', sys=FALSE)
                  )
        #print (paste0(filename,'.....SAVED'))

        #print ('allRegions.bint.bed.....SAVED')


        # if (input$allRegBinTable_check==TRUE){
        #     #filename='allRegions.bint.bed'
        #     HCRwrite (InteractionSet::regions(binSaved$redata),
        #             file='allRegions.bint.bed' ,
        #             path=pointin(wdPath,'Binning'),
        #             col.names=FALSE,
        #             extension=FALSE )
        #     print ('allRegions.bint.bed.....SAVED')
        # }

        if (input$selectRegBinTable_check==TRUE){
            filename <- paste0(filenameCommon,'.bint.bed')
            reframe<- as.data.frame (InteractionSet::regions( binSaved$redata))
            reframe<- subset (reframe, reframe[[1]]==input$chrInt1Box
                                | reframe[[1]]==input$chrInt2Box )
            HCRwrite (reframe, file=filename,
                      path=pointin(wdPath,'Binning'),
                      append=FALSE , col.names=FALSE, extension=FALSE )
            print (paste0(filename,'.....SAVED'))
        }

        #questa puoi toglierla da qui=======================
        if (input$squareCount_check==TRUE){
            filename= paste0(filenameCommon,'.counts.RData')
            red=binSaved$redata
            save (red, file=paste0(pointin(wdPath,'Binning'),filename))
            print (paste0(filename,'.....SAVED'))
        }
        #===================================================

        if (input$contactMatrix_check==TRUE){
            filename= paste0(filenameCommon,'_raw_matrix')
            maType <-'chr'
            #maType è chr quando utilizzi i chromosomi specifici, 'all' se invece li utilizzi tutti e per all VS chr ???
            toSaveMatrix<-matrixExport(redata=binSaved$redata, type=maType, chrA=input$chrInt1Box , chrB=input$chrInt2Box )
            HCRwrite (toSaveMatrix, file=filename,
                      path=pointin(wdPath,'Binning'), row.names=TRUE)
            #controlla se ci sono i nomi, come row e col names, se ci sono non hai bisogno di produrne altri altrimenti vediti
            #mane nel vecchio modulo del binning
            print (paste0(filename,'.....SAVED'))
        }

        # if (input$contactTable_check==TRUE){
        #     filename<-paste0(filenameCommon,'_conTable')
        #     #redataTT<<-binSaved$redata
        #     conTab<-extractContact(binSaved$redata,input$chrInt1Box,input$chrInt2Box)
        #     ##nella funzione non hai ancora integrato all VS all e all VS chr
        #     HCRwrite (conTab, file=filename , path=pointin(wdPath,'Binning'))
        #     print (paste0(filename,'.....SAVED'))
        # }

        print ('all results EXPORTED')

    })

}
