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

                shiny::fluidRow (

                    #     shiny::column (4,
                    #         shiny::selectInput("selectInputType", label = h5("select input type"),
                    #                         choices = c('Robj','Matrix'),
                    #                         selected = 'Matrix')
                    #     )
                    #
                    # ),

                    # shiny::fluidRow (
                    #     shiny::column (6,
                    #         shiny::conditionalPanel ( condition = "input.selectInputType=='Matrix'" ,
                    #                                 shiny::fileInput(diffHicNormNs('matrixToLoad'),label=h5('select matrix to load'))
                    #             )
                    #     ),
                    #
                    #     shiny::column (6,
                    #         shiny::conditionalPanel ( condition = "input.selectInputType=='Matrix'" ,
                    #                             shiny::fileInput(diffHicNormNs('wideMatrixToLoad'), label=h5('select all chromosomes matrix to load'))
                    #             )
                    #     )
                    #
                    # ),



                    # shiny::fluidRow (
                    #     shiny::column (12,
                    #         shiny::conditionalPanel (condition = "input.selectInputType=='Matrix'" )
                    #     )
                    # ),
                    shiny::fluidRow (
                        shiny::column(4,
                                    shiny::helpText("NAmin: ")
                                      ),
                        shiny::column (8,
                            shiny::checkboxInput(diffHicNormNs('NAminBox'),
                                                label="",
                                                value=FALSE
                            )
                        )
                    ),


                    shiny::fluidRow(
                        shiny::column(3,
                                      shiny::helpText("winsor.high:   ")
                        ),
                        shiny::column(3,
                                      shiny::numericInput(
                                          diffHicNormNs('winHi'),
                                          label="", value=0)

                        ),
                        shiny::column(3,
                                      shiny::helpText("ignore.low:   ")
                        ),
                        shiny::column(3,
                                      shiny::numericInput(
                                          diffHicNormNs('igLow'),
                                          label="",
                                          value=0
                                      )
                        )
                    ),

                    # shiny::fluidRow (
                    #     shiny::column(12,
                    #         shiny::actionButton(diffHicNormNs('rdButton'),label='Start Normalization')
                    #         )
                    # ),

                    # shiny::fluidRow (
                    #
                    #     shiny::column (10, #conditionalPanel ( condition = "input.selectInputType=='Robj'",
                    #         selectFile(diffHicNormNs('rdLoad'), path=pointin(wdPath,'Binning')
                    #                    ,label='load .counts.RData file',
                    #                     subset=TRUE, pattern='.counts.RData'
                    #                                    )
                    #     #)
                    #     )
                    #
                    #
                    #
                    # ),

                    # shiny::fluidRow (
                    #
                    #     shiny::column (10, shiny::conditionalPanel( condition = "input.selectInputType=='Robj'" ,
                    #                             selectFile(diffHicNormNs('bintLoad'),
                    #                                     path=pointin(wdPath,'Binning')
                    #                                     ,label='load .bint.bed file',
                    #                                     subset=TRUE, pattern='.bint.bed'
                    #                                   ))
                    #     ),
                    #
                    #     shiny::column (2, shiny::br(), shiny::br()
                    #             #,actionButton(diffHicNormNs('rdButton'),label='start')
                    #     )
                    #
                    # ),

                    # fluidRow (
                    #   column (4,
                    #           checkboxInput(diffHicNormNs("filterbox"), label = "Pre-Norm Filtering", value = FALSE)
                    #           ),
                    #   column (8,
                    #           uiOutput (diffHicNormNs("filterpanel_slot"))
                    #           )
                    # ),

                    # shiny::fluidRow (
                    #     shiny::column (8, shiny::conditionalPanel( condition = "input.selectInputType=='Matrix'" ,
                    #                                         shiny::actionButton (diffHicNormNs('fromMatrixStartBut'), label=h5('Normalize')) ))
                    # ),

                    shiny::fluidRow (

                        shiny::column (8, #shiny::conditionalPanel( condition = "input.selectInputType=='Robj'",
                                       shiny::uiOutput(diffHicNormNs('chrChoice_slot'))
                                       #)
                        ),

                        shiny::column (4, #shiny::conditionalPanel( condition = "input.selectInputType=='Robj'",
                                       uiOutput(diffHicNormNs('rdButton_slot'))
                                       #                               )
                        )

                    ),

                    shiny::fluidRow (
                        shiny::column(12,
                                      shiny::actionButton(diffHicNormNs('rdButton'),label='Start Normalization')
                        )
                    ),

                    shiny::fluidRow(

                        # column (6,
                        #         selectFile (diffHicNormNs('fragments'), path=pointin(anFolder$hisave,'Pre-Processing') ,label='cutGenome',
                        #                     subset=TRUE, pattern='.cutGenome'
                        #         )
                        #
                        #         ),
                        # column (4, br(), br() ,
                        #         actionButton(diffHicNormNs('setGen'),label='set this')
                        #         )

                    ),

                    shiny::uiOutput(diffHicNormNs('mainP'))

                    ###        # fluidRow(
                    #   # column(4,#fileInput (diffHicNormNs('fragments'),label=h6('fragments'))
                    #   #         selectFile (diffHicNormNs('fragments'), path=pointin(anFolder$hisave,'Pre-Processing') ,label='cutGenome',
                    #   #                     subset=TRUE, pattern='.cutGenome'
                    #   #                     )
                    #   #       )
                    #   column(4,#fileInput(diffHicNormNs('h5file'),label=h6('h5 file'))
                    #           selectFile (diffHicNormNs('h5file'), path=pointin(anFolder$hisave,'Pre-Processing') ,label='h5file',
                    #                       subset=TRUE, pattern='.h5')
                    #           )
                    #
                    #
                    #
                    #   ,column(2,   #uiOutput(diffHicNormNs('chr'))
                    #                 selectInput (diffHicNormNs('chromo'), label= h5('chromosomes'),
                    #                              choices=chrname[,1]
                    #                              )
                    #           )
                    #   ,column(1, #textInput(diffHicNormNs("binSize"), label = "bin size", value = "1e5")
                    #           actionButton(diffHicNormNs('startBut'), label=h5('start'))
                    #          )
                    #
                    #
                    ###        # )

                )
            ),
            shiny::fluidRow (

                shiny::tabsetPanel ( id=diffHicNormNs('diffHicMainTabpanel')



                                     # ,tabPanel ('correct contact',
                                     #
                                     #   wellPanel(
                                     #            fluidRow(
                                     #
                                     #             #column(3,textInput(diffHicNormNs("winsor.hi"), label =h5("winsor.high"), value = "0.02"))
                                     #             #,column(3,textInput(diffHicNormNs('ignore.lo'), label=h5('ignore.low'),value="0.02")),
                                     #             column(3,uiOutput(diffHicNormNs('chr')))
                                     #             ,column(3,br(),br(),actionButton(diffHicNormNs('startBut'), label=h5('start')))
                                     #           ),
                                     #
                                     #       #    fluidRow(
                                     #             #column(4,textInput(diffHicNormNs("binSize"), label = "bin size", value = "1e5")),
                                     #       #      column(4,uiOutput(diffHicNormNs('chr')))
                                     #       #      ,column(4,actionButton(diffHicNormNs('startBut'), label=h5('start')))
                                     #       #    ),
                                     #
                                     #           fluidRow(
                                     #             column (6, uiOutput (diffHicNormNs('newName')))
                                     #             ,column (4, uiOutput(diffHicNormNs('save')))
                                     #           )
                                     # )
                                     # )

                                     # ,tabPanel ('normalize CNV',
                                     #           wellPanel(
                                     #           actionButton(diffHicNormNs('startBut2'), label=h5('execute'))
                                     #           )
                                     # )

                )

            ),
            shiny::fluidRow(
                shiny::column (12, shiny::uiOutput(diffHicNormNs('exportPanel')))
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
    #require ('diffHic')
    #source ('diffHic_fun.R')
    saveMe<-shiny::reactiveValues(firstV='none')

    ##==========================================================================NEW version start

    ##rdButton-----------------------------------------------
    load(file=paste0(pointin(wdPath, "Binning", sys=TRUE), "intSet.Robj"))
    saveMe$intSet<-intSet
    rm(intSet)

    #print (regions(red))
    saveMe$chrList<- as.data.frame(unique(
        GenomicRanges::seqnames(
            InteractionSet::regions(saveMe$intSet))))
    colnames(saveMe$chrList)='chromosomes'
    #saveMe$chrList<-c('all',saveMe$chrList)

    output$chrChoice_slot<- shiny::renderUI ({

        shiny::wellPanel (

            shiny::fluidRow (
                shiny::column (5,
                               shiny::selectInput(diffHicNormNs('chrInt1'),
                                                  label='first chromosome',
                                                  choices= saveMe$chrList,
                                                  selected=saveMe$chrList[1]))
                ,shiny::column (2,
                                shiny::br(),
                                shiny::br() ,
                                shiny::helpText('Vs')
                ),
                shiny::column (5,
                               shiny::selectInput(diffHicNormNs('chrInt2'),
                                                  label='second chromosome',
                                                  choices= saveMe$chrList,
                                                  selected=saveMe$chrList[1])
                )
            )

        )

    })

    observeEvent (input$rdButton ,{

        #store run time
        #runTime4<<-proc.time()

        #load (file=paste0(pointin(anFolder$hisave,'Binning'),input$rdLoad))
        #load always specific file
        # load( file=paste0(pointin(wdPath, "Binning", sys=TRUE), "intSet.Robj"))
        # saveMe$intSet<-intSet
        # rm(intSet)
        #
        # #print (regions(red))
        # saveMe$chrList<- as.data.frame(unique(seqnames(regions(saveMe$intSet))))
        # colnames(saveMe$chrList)='chromosomes'
        # saveMe$chrList<-c('all',saveMe$chrList)
        #print (saveMe$chrList)

        # output$chrChoice_slot<- shiny::renderUI ({
        #
        #     shiny::wellPanel (
        #
        #         shiny::fluidRow (
        #             shiny::column (5,
        #                         shiny::selectInput(diffHicNormNs('chrInt1'),
        #                                             label='first chromosome',
        #                                             choices= saveMe$chrList,
        #                                             selected=saveMe$chrList[2]))
        #             ,shiny::column (2,
        #                         shiny::br(),
        #                         shiny::br() ,
        #                         shiny::helpText('Vs')
        #                         ),
        #             shiny:: column (5,
        #                     shiny::selectInput(diffHicNormNs('chrInt2'),
        #                                     label='second chromosome',
        #                                     choices= saveMe$chrList,
        #                                     selected=saveMe$chrList[2])
        #                     )
        #         )
        #
        #     )
        #
        # })

        # output$rdButton_slot<-shiny::renderUI({
        #     shiny::actionButton(diffHicNormNs('rdButton'),label='start')
        # })

        saveMe$correction <- diffHic::correctedContact (saveMe$intSet
                                                        ,ignore.low= input$igLow #0
                                                        ,winsor.high= input$winHi #0.02
        )
        correction<-saveMe$correction
        save(correction, file=paste0(pointin(wdPath, "Normalization", sys=TRUE)
                                     ,"iceNorm_correction.Robj"
        ))
        rm(correction)
        print ('correction.....OK')

        #questo devi metterlo in un altro tasto

        if (input$chrInt1==input$chrInt2){
            NormMatrix<- as.brute.matrix (saveMe$intSet, first=input$chrInt1
                                          ,second=input$chrInt2
                                          ,fill=saveMe$correction$truth
                                          ,alternativeFill=TRUE
                                          ,NAmin=input$NAminBox
            )
        }else{
            NormMatrix<- as.brute.matrix (saveMe$intSet, first=c(input$chrInt1,input$chrInt2)
                                          ,second=c(input$chrInt1,input$chrInt2)
                                          ,fill=saveMe$correction$truth
                                          ,alternativeFill=TRUE
                                          ,NAmin=input$NAminBox
            )
        }


        print ('NormMatrix.....OK')
        #print (rownames(NormMatrix))
        #View (NormMatrix)

        ##creare la normmatrix inchiomma la ram devo capire perche???
        ##NormMatrix<<- as.matrix (red, first='chr1', second='chr1', fill=correction$truth)

        output$exportPanel<- shiny::renderUI ({
            shiny::fluidRow(
                shiny::column(12,
                              shiny::wellPanel (
                                  shiny::h4('Export Data'),
                                  # fluidRow(column(12,
                                  #                 textInput(diffHicNormNs('fileName'), label=h5('name'))
                                  #                 ##input$fileName to read value
                                  #                 )),
                                  shiny::fluidRow(
                                      shiny::column(6,
                                                    shiny::selectInput(
                                                        diffHicNormNs('chrRange')
                                                        ,label=h5('Int Range'),
                                                        choices="VS",
                                                        #c('all','VS'),
                                                        selected=1
                                                    )
                                      ),

                                      shiny::column(6,
                                                    shiny::uiOutput(
                                                        diffHicNormNs('chrChoices'))
                                      )
                                  ),
                                  # fluidRow(column(12,
                                  #                 selectInput (diffHicNormNs('maType'), label=h5('matrix Type'),
                                  #                              choices = c('row counts'
                                  #                                          #,'k-means'
                                  #                                          ),
                                  #                              selected =1
                                  #                              )
                                  #
                                  #                 )),

                                  shiny::fluidRow(
                                      shiny::column(3,
                                                    shiny::checkboxInput(
                                                        diffHicNormNs('prefixCheck')
                                                        ,label='add prefix' ,
                                                        value=FALSE)
                                      ),
                                      shiny::column(9,
                                                    shiny::textInput(
                                                        diffHicNormNs('prefixText'),
                                                        label=h5('add prefix'),
                                                        value='')
                                      )

                                  ),

                                  shiny::fluidRow(
                                      shiny::column(12,
                                                    shiny::actionButton(
                                                        diffHicNormNs('expBut'),
                                                        label=h5('Export'))

                                      ))
                              )
                )
            )
        })


        observeEvent(input$chrRange,{
            if (input$chrRange=='VS'){
                output$chrChoices<- renderUI ({

                    # wellPanel (
                    #     selectInput(diffHicNormNs('chrChoicesA'), label=h5('first chromosome '),
                    #                 choices=input$chrInt1,
                    #                 selected=1
                    #     ),
                    #     selectInput(diffHicNormNs('chrChoicesB'), label=h5('second chromosome'),
                    #                 choices=input$chrInt2,
                    #                 selected=1
                    #     )
                    # )
                })

            } else {

                if (input$chrRange=='all'){
                    output$chrChoices<- renderUI ({
                        wellPanel (h5('all chromosomes'))
                    })
                }
                else {
                    if (input$chrRange=='VS'){
                        output$chrChoices<- renderUI ({
                            wellPanel (h5('VS'))
                        })
                    }
                }

            }


        })


        ##va messo in un observe analogo a export nel modulo del binning
        #    print (c('chromosomo of interest',input$chromo))
        #    NormMatrix<<- as.matrix (red, first=input$chromo, second=input$chromo, fill=correction$truth)
        #    print ('NormMatrix.....OK')
        #    #write.table (NormMatrix, file='test_NormMatrix.tsv', sep='\t')
        #    HCRwrite (NormMatrix, file=norMaFilename, path=pointin(anFolder$hisave,'Normalization'), row.names=TRUE)
        ##########################################################################################
        #runTime4<<-proc.time()-runTime4
    })

    observeEvent (input$expBut,{
        #load (file=paste0(pointin(wdPath,'Binning'),"intSet.Robj"))
        #====================================

        ##Export Iterated  matrix



        if (input$chrRange=='VS'){

            maType <-'chr'

            print (c('chrRange.....VS', 'maType:' ,maType))



            toSaveMatrix2 <- matrixExport (redata=saveMe$intSet, type=maType,
                                           chrA=input$chrInt1 ,
                                           chrB=input$chrInt2,
                                           chgFill=TRUE,
                                           alternativeFill=saveMe$correction$truth
                                           ,NAas0=FALSE )

            #toSaveMatrix2<<- as.matrix (red, first=c(input$chrChoicesA,input$chrChoicesB), second=c(input$chrChoicesA,input$chrChoicesB) , fill=correction$truth)
            #View (toSaveMatrix2)

            print ('raw matrix.....OK')
            rawfilename<- paste0(obtainName(wdPath),'_',input$chrInt1 ,'VS', input$chrInt2 ,'_iterative_matrix')

            if (input$prefixCheck==TRUE){
                rawfilename<-paste0(input$prefixText,'_',rawfilename)
            }

            rawfilenameN<- paste0 (rawfilename,'_NAMES')
            ##fai caricare il bint.bed tramite select file
            #bintName<-paste0 (obtainName(anFolder$hisave),'.bint.bed')
            bintName <- "allRegions.bint.bed"  #input$bintLoad

            #toSaveMatrix2TT<<-toSaveMatrix2
            HCRwrite (toSaveMatrix2, file=rawfilename, path=pointin(wdPath,'Normalization'), row.names=TRUE)
            # mane<-matrixWithNames (paste0(pointin(wdPath,'Normalization'),rawfilename,'.tsv'),
            #                        paste0(pointin(wdPath,'Binning'),bintName))
            # HCRwrite (mane, file=rawfilenameN, path=pointin(wdPath,'Normalization'), row.names=TRUE)
            # HCRwrite (toSaveMatrix2, file=paste0(rawfilename,'_withNames'), path=pointin(anFolder$hisave,'Normalization'), row.names=TRUE)
            print (c('sp1 matrix saved as:', rawfilename))



        } else {##input$chrRange=='all'

            maType <-'all'
            print (c('chrRange.....all', 'maType:' ,maType))

            toSaveMatrix2<- matrixExport (redata=saveMe$intSet, type=maType, chgFill=TRUE,
                                          alternativeFill=saveMe$correction$truth
                                          , NAas0=FALSE )
            print ('raw matrix.....OK')
            rawfilename<- paste0(obtainName(wdPath),
                                 input$chrInt1,
                                 input$chrInt2,
                                 '_iterative_matrix')
            rawfilenameN<- paste0 (rawfilename,'_NAMES')

            #toSaveMatrix2TT<<-toSaveMatrix2
            HCRwrite (toSaveMatrix2, file=rawfilename, path=pointin(wdPath,'Normalization'), row.names=TRUE)
            #bintName<-paste0 (obtainName(anFolder$hisave),'.bint.bed')
            bintName<-input$bintLoad  ##questa devi dirgli di caricare quella fissa, ma puoi caricare la all bin table?
            # mane<-matrixWithNames(paste0(pointin(wdPath,'Normalization'),
            #                             rawfilename,'.tsv'),
            #                     paste0(pointin(wdPath,'Binning'),"allRegions.bint.bed"))
            # HCRwrite (mane, file=rawfilenameN, path=pointin(wdPath,'Normalization'), row.names=TRUE)
            print (c('sp2 matrix saved as:', rawfilename))

        }
        filteringTable<-matrix(ncol=1, nrow=3)
        rownames(filteringTable)<-c("Normalization", "winsor.hight", "ignore.low")
        filteringTable[,1]<-c("ICE", input$winHi, input$igLow)
        write.table(filteringTable, paste0(pointin(wdPath, "Normalization", sys=TRUE),"parameter.tsv"),
                    sep="\t", quote=FALSE, col.names=FALSE)

    })

    # observeEvent (input$rdLoad,{
    #     # load (file=paste0(pointin(wdPath,'Binning'),input$rdLoad))
    #     # #print (regions(red))
    #     # saveMe$chrList<- as.data.frame(unique(seqnames(regions(saveMe$intSet))))
    #     # colnames(saveMe$chrList)='chromosomes'
    #     # saveMe$chrList<-c('all',saveMe$chrList)
    #     # print (saveMe$chrList)
    #     #
    #     # output$chrChoice_slot<- renderUI ({
    #     #
    #     #     wellPanel (
    #     #
    #     #         fluidRow (
    #     #             column (5, selectInput(diffHicNormNs('chrInt1'), label='first chromosome', choices= saveMe$chrList, selected=saveMe$chrList[2])),
    #     #             column (2, br(),br() ,helpText('Vs') ),
    #     #             column (5, selectInput(diffHicNormNs('chrInt2'), label='second chromosome', choices= saveMe$chrList, selected=saveMe$chrList[2]))
    #     #         )
    #     #
    #     #     )
    #     #
    #     # })
    #     #
    #     # output$rdButton_slot<-renderUI ({
    #     #
    #     #     actionButton(diffHicNormNs('rdButton'),label='start')
    #     #
    #     # })
    #
    #
    #
    # })

    observeEvent (input$filterbox,{

        if (input$filterbox==TRUE){

            output$filterpanel_slot<- renderUI ({
                wellPanel (
                    fluidRow (
                        column (8,
                                selectInput(diffHicNormNs("filterType"), label = h5("Filter Type"),
                                            choices = c('Direct','Trended'),
                                            selected = 1)
                        ),
                        column (4, br(), br(),
                                checkboxInput(diffHicNormNs("hightreso_check"),
                                              label = "hight resolution", value = FALSE)
                        )
                    ),
                    fluidRow (
                        column(12,uiOutput(diffHicNormNs('inputReference_slot')))
                    )

                )
            })
        } else {
            output$filerpanel_slot<- renderUI ({})
        }

    })

    observeEvent (input$hightreso_check,{
        print ('hightreso_check.....PRESSED')
        if (input$hightreso_check==TRUE){
            output$inputReference_slot<- renderUI({
                fileInput (diffHicNormNs('inputReference'),
                           label=h6('reference (.counts.RData; bin size>= 10e6 )'))
            })
        } else {
            output$inputReference_slot<- renderUI({})
        }

    })

    observeEvent (input$fromMatrixStartBut,{
        #load (file=paste0(pointin(anFolder$hisave,'Binning'),input$rdLoad))
        tabPath<- input$matrixToLoad$datapath
        libPath<- input$wideMatrixToLoad$datapath

        allChrTab<- read.table (libPath)
        #allChrTabTT<<-allChrTab

        table<-readContactMatrix(tabPath)
        rwNam<- rownames(table)
        #rwNamTT<<-rwNam
        lista<- strsplit(rwNam,':',fixed=TRUE)
        chromosome<-   unlist(lista)[c(TRUE,FALSE)]
        rm (table)
        chromosome<-unique (chromosome)


        if (length(allChrTab[1,])==3 & length(allChrTab[,1])!=length(allChrTab[1,])){
            #è una sparse matrix
            summa=sum(allChrTab[,3])
            print (paste0('all chromosomes matrix is sparse matrix, lib.size=', summa))
        } else {
            #è una square matrix
            allChrTab= readContactMatrix (libPath)   ##fallo meglio
            summa=sum(allChrTab)
            print (paste0('all chromosomes matrix is square matrix, lib.size=', summa))
        }

        saveMe$size=summa
        saveMe$intSet<- makeInset (tabPath, rebuildTotals=TRUE, lib.size=saveMe$size)
        # redTT<<-red
        # print (regions(red))
        # #unique (seqnames(regions))

        saveMe$correction <- correctedContact (saveMe$intSet, ignore.low=input$igLow #0
                                               , winsor.high=input$winHi #0
                                               )
        print ('correction.....OK')

        if(chromosome==chromosome){
            NormMatrix<- as.brute.matrix (saveMe$intSet, first=chromosome, second=chromosome,
                                          fill=saveMe$correction$truth,
                                          alternativeFill=TRUE,
                                          NAmin=input$NAminBox
            )
        }

        saveMe$NormMatrixS<-NormMatrix

        print ('NormMatrix.....OK')
        #print (rownames(NormMatrix))
        #View (NormMatrix)

        ##creare la normmatrix inchiomma la ram devo capire perche???
        ##NormMatrix<<- as.matrix (red, first='chr1', second='chr1', fill=correction$truth)


        output$exportPanel<- renderUI ({
            wellPanel(
                fluidRow(
                    column (6,
                            textInput(diffHicNormNs('prefix2'), label='add prefix', value='')
                    ),
                    column (6,
                            actionButton(diffHicNormNs('exportMatrix'), label='export')
                    )
                )
            )
        })


        observeEvent(input$chrRange,{
            if (input$chrRange=='VS'){
                output$chrChoices<- renderUI ({

                    # wellPanel (
                    #     selectInput(diffHicNormNs('chrChoicesA'), label=h5('first chromosome'),
                    #                 choices=input$chrInt1,
                    #                 selected=1
                    #     ),
                    #     selectInput(diffHicNormNs('chrChoicesB'), label=h5('second chromosome'),
                    #                 choices=input$chrInt2,
                    #                 selected=1
                    #     )
                    # )
                })

            } else {

                if (input$chrRange=='all'){
                    output$chrChoices<- renderUI ({
                        wellPanel (h5('all chromosomes'))
                    })
                }
                else {
                    if (input$chrRange=='VS'){
                        output$chrChoices<- renderUI ({
                            wellPanel (h5('VS'))
                        })
                    }
                }

            }


        })


        ##va messo in un observe analogo a export nel modulo del binning
        #    print (c('chromosomo of interest',input$chromo))
        #    NormMatrix<<- as.matrix (red, first=input$chromo, second=input$chromo, fill=correction$truth)
        #    print ('NormMatrix.....OK')
        #    #write.table (NormMatrix, file='test_NormMatrix.tsv', sep='\t')
        #    HCRwrite (NormMatrix, file=norMaFilename, path=pointin(anFolder$hisave,'Normalization'), row.names=TRUE)
        ##########################################################################################

    })

    observeEvent (input$exportMatrix,{
        normMatrix= saveMe$NormMatrixS
        uploadedFileName= input$matrixToLoad$name
        suffix= '_iterative_matrix'
        prefix= input$prefix2
        HCRwrite (normMatrix, paste0(prefix, uploadedFileName, suffix),
                  pointin(wdPath,'Normalization'), row.names=TRUE)

    }) #export results when use matrix input

    ##--------------------------------------------------------

    ##====================================================================================NEW version stop

    observeEvent (input$setGen,{
        #dovresti far comparire tutte le altre icone dopo il caricameno di h5 e frag tutto il resto dei comandi

        frag<-read.table (paste0(pointin(wdPath,'Pre-Processing'),input$fragments), sep='\t')
        chrname<- unique(frag[,1])
        chrname<- as.matrix (chrname)

        output$mainP<-renderUI ({

            wellPanel (

                fluidRow(
                    # column(4,#fileInput (diffHicNormNs('fragments'),label=h6('fragments'))
                    #         selectFile (diffHicNormNs('fragments'), path=pointin(anFolder$hisave,'Pre-Processing') ,label='cutGenome',
                    #                     subset=TRUE, pattern='.cutGenome'
                    #                     )
                    #       )
                    column(6,#fileInput(diffHicNormNs('h5file'),label=h6('h5 file'))
                           selectFile (diffHicNormNs('h5file'),
                                       path=pointin(wdPath,'Pre-Processing') ,
                                       label='h5file',
                                       subset=TRUE, pattern='.h5')
                    )



                    ,column(2,   #uiOutput(diffHicNormNs('chr'))
                            selectInput (diffHicNormNs('chromo'), label= h5('chromosomes'),
                                         choices=chrname[,1]
                            )
                    )
                    ,column(1,br(), br(), #textInput(diffHicNormNs("binSize"), label = "bin size", value = "1e5")
                            actionButton(diffHicNormNs('startBut'), label=h5('start'))
                    )


                )


            )


        })


        # output$chr<- renderUI ({
        #   selectInput (diffHicNormNs('chromo'), label= h5('chromosomes'),
        #                choices=chrname[,1]
        #                )
        # })

    })

    observeEvent (input$startBut,{
        #funzionalizza questi passaggi================================
        saveMe$h5file<-paste0 ( pointin(wdPath, 'Pre-Processing',
                                        sys=TRUE) , input$h5file)
        print (saveMe$h5file)
        #size<-input$binSize

        infoImport<-HCRread (file='info.tsv', path=wdPath, header=FALSE)
        print ('info.....imported')
        saveMe$size<-as.numeric(infoImport[2,1])
        fileName<- infoImport[1,1]
        fileName<- gsub (".bam", "" ,fileName)
        print (c('fileName',fileName))
        norMaFilename<-paste0(fileName, '_' ,input$chromo ,'_ICEnormMatrix')


        saveMe$refFrags<-input$fragments
        #rFrags<-read.table (refFrags, sep='\t')
        #rFragsGrange<-makeGRangesFromDataFrame(rFrags)
        #paramFil <<- pairParam(rFragsGrange)

        refPosition<- paste0 (pointin (wdPath,'Pre-Processing'),saveMe$refFrags)
        saveMe$chr<-importChrFromFragList(refPosition)
        #rFrags<-read.table (refPosition, sep='\t', header=TRUE)
        rFrags<-read.table (refPosition, sep='\t', header=TRUE)
        #View (rFrags)
        rFragsGrange<-GenomicRanges::makeGRangesFromDataFrame(rFrags)
        saveMe$paramFil <- pairParam(rFragsGrange)
        #print (paramFil$hisave)
        print('param importato')

        #paramFil<<-importParam (refFrags)
        print ('pairParam.....OK')
        saveMe$intSet <- squareCounts(saveMe$h5file, saveMe$paramFil, width=saveMe$size, filter=1)
        print ('squareCount.....OK')
        #=============================================================
        ##correction <- correctedContact (red, winsor.high=input$winsor.hi, ignore.low=input$ignore.lo)
        saveMe$correction <- correctedContact (saveMe$intSet, ignore.low=input$igLow, winsor.high=input$winHi)
        print ('correction.....OK')
        ##creare la normmatrix inchiomma la ram devo capire perche???
        ##NormMatrix<<- as.matrix (red, first='chr1', second='chr1', fill=correction$truth)
        print (c('chromosomo of interest',input$chromo))
        NormMatrix<- as.matrix (saveMe$intSet, first=input$chromo, second=input$chromo,
                                fill=saveMe$correction$truth)
        print ('NormMatrix.....OK')
        #write.table (NormMatrix, file='test_NormMatrix.tsv', sep='\t')
        #NormMatrixTT<<-NormMatrix
        HCRwrite (NormMatrix, file=norMaFilename, path=pointin(wdPath,'Normalization'), row.names=TRUE)

    })


    observeEvent (input$startBut2,{
        saveMe$h5file<-input$h5file$datapath
        saveMe$refFrags<-input$fragments$datapath
        saveMe$size<-input$binSize
        imp<-importParam (saveMe$refFrags)
        print ('imp.....OK')
        margin.data <- marginCounts (saveMe$h5file,imp,width=saveMe$size)
        print ('margin.data.....OK')
        saveMe$intSet <- squareCounts(saveMe$h5file, saveMe$paramFil, width=saveMe$size, filter=1)
        print ('squareCounts.....OK')
        cnv.offs<- normalizeCNV (saveMe$intSet,margin.data)
        print ('cnv.offs.....OK')

    })



}
