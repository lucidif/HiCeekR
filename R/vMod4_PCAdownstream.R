##pca_postProcessing

#' pca_postProcessing_UI
#'
#' @param id
#' @param label
#'
#' @return
#' @keywords internal
#'
#' @examples
pca_postProcessing_UI <- function (id, label='PCA' ){
    pcaNS<- shiny::NS (id)

    shiny::fluidPage(


        shiny::tabsetPanel ( #pcaNS('tabSetPCA'),

            # shiny::tabPanel( 'prepare PCA'
            #
            #                  ,shiny::wellPanel (
            #
            #                      shiny::fluidRow (
            #
            #                          shiny::column (12,
            #
            #                                         shiny::selectInput (pcaNS('dataType'),
            #                                                             label='data type',
            #                                                             choices = c('bint.tsv'
            #                                                                         ,'epiCounts'
            #                                                                         #,'EpiCounts.bed'
            #                                                             )
            #                                         )
            #
            #                          )
            #
            #                      ),
            #
            #                      shiny::fluidRow (
            #                          shiny::column (8,
            #
            #                                         #fileInput (pcaNS('binTable'), label='select binTable file')
            #
            #                                         #selectFile (pcaNS('binTable'), path=pointin (wdPath, 'Binning') ,label='select binTable file', subset=TRUE, pattern='.bint')
            #                                         shiny::uiOutput (pcaNS('selFileSlot'))
            #
            #                          )
            #
            #                          #column (3,
            #                          #        textInput (pcaNS('name'), label='name')
            #                          #        )
            #                          #,
            #                          ,
            #                          shiny::column (2,
            #                                         shiny::br(),
            #                                         shiny::br(),
            #                                         shiny::actionButton (pcaNS('genPcaMod'),
            #                                                              label='generate')
            #                          )
            #                          #,
            #                          #column(1,br(),
            #                          #      uiOutput (pcaNS('finish'))
            #                          #        )
            #
            #                      ),
            #
            #
            #
            #                      shiny::uiOutput (pcaNS('secondRow')),
            #
            #                      shiny::uiOutput (pcaNS('thirdRow')),
            #
            #                      shiny::fluidRow (
            #
            #                          shiny::dataTableOutput(pcaNS('tableScreen'))
            #                          #dataTableOutput(pcaNS('tableScreen'))
            #
            #                      )
            #
            #
            #
            #                  )
            # ),

            shiny::tabPanel ('PCA'

                              ,shiny::wellPanel (

                                  shiny::fluidRow (

                                      shiny::column (10,

                                                     #fileInput (pcaNS ('pcaTableLoaded'), 'pca table')
                                                     #selectFile (pcaNS('pcaTableLoaded'), path=pointin(wdPath, 'PCA') ,'pca table', subset=TRUE, pattern='.epiCounts.tsv')
                                                     shiny::uiOutput (pcaNS('pcaTableSlot'))
                                      ),

                                      shiny::column (2, shiny::br(), shiny::br(),

                                                     shiny::actionButton (pcaNS('startPca'),
                                                                          'start PCA')

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

            )

        )

    )

}

#' pca_postProcessing_Server
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
pca_postProcessing_Server <- function (input, output, session, stringsAsFactors,
                                       wdPath
){
    print ("start PCA module")
    #plusval<-0
    pcaNServer<- session$ns
    #require ('Rsamtools')

    ##reactiveValue
    #tPcaRea<- reactiveValues(tP='none')
    Reac<- shiny::reactiveValues(tP='none', S_pcaMod='none', S_exportThis='none') #non c'è bisogno che definisci prima la variabile reactiveValues() e poi definisci dopo

    print ("pcaTabSlot render")
    output$pcaTableSlot<- shiny::renderUI ({

        shiny::fluidRow(
            shiny::column(10, shiny::br(),
                          selectFile (pcaNServer('pcaTableLoaded'), path=pointin(wdPath, 'Normalization') ,'raw contact matrix', subset=TRUE, pattern='.tsv')
                          #shiny::fileInput(pcaNServer('pcaTableLoaded'), label="pca file")
                          ),
            shiny::column(2,
                          shiny::numericInput(pcaNServer('PCs'), label = h5("PCs"), value = 3)
                          )
        )


    })

    print ("start observer")
    shiny::observeEvent(input$bamLoadPath,{
        print("bamLoadPath")
        print (input$bamLoadPath)
        if ( input$bamLoadPath!="please select file"){
            print(paste0("EpiPath:",pointin(wdPath,'Epi'),input$bamLoadPath))
            #Reac$bedTab<-read.table(paste0(pointin(wdPath,'Epi'),input$bamLoadPath), sep="\t")
            #rownames(Reac$bedTab)<-rep(paste0(Reac$bedTab[,1],":",Reac$bedTab[,2],"-",Reac$bedTab[,3]))
            #Reac$bedTab<-Reac$bedTab[,-c(1:4)]
            # output$colSelectorSlot<-shiny::renderUI({
            #     shiny::selectInput(pcaNServer('colSelector'),
            #                 label="select column",
            #                 choices=c(1:length(Reac$bedTab[1,]))
            #                     )
            # })
            #bedTabTT<<-bedTab
        }

    })

    shiny::observeEvent (input$genPcaMod,{
        print ('generate start')
        #plusval<- plusval+1
        #nameD<- paste0('data', plusval)
        #print (nameD)
        #pcaMod<-Reac$A_pcaMod

        if (input$dataType=='bint.tsv'){
            bintabPath<- paste0 ( pointin(wdPath,'Binning'), input$binTable)
            Reac$pcaMod<-pcaMatrixModel(bintabPath)
            Reac$A_pcaMod<-Reac$pcaMod
        }

        if (input$dataType=='epiCounts'){
            print ('in epiCounts')
            bintabPath<- paste0 ( pointin(wdPath,'PCA'), input$epicTable)
            Reac$pcaMod<-HCRread('',path=bintabPath)
            Reac$A_pcaMod<-Reac$pcaMod
            regionsN<- rownames (Reac$pcaMod)
            regions<- matrix (ncol=1,nrow=length(regionsN))
            rownames(regions)<-rownames (Reac$pcaMod)
            colnames(regions)<-'regions'
            regions[,1]<-regionsN
            #View (regions)
            rm (regionsN)
            #print (regions)
            Reac$pcaMod <- cbind (regions,Reac$pcaMod)

            #View (pcaMod)
            #print (pcaMod)
        }

        if (input$dataType=='EpiCounts.bed'){
            #testTT<<-mergeBed(input$epicBed$datapath)
            #testTTnames<<-input$epicBed$name

            clnm<-gsub(".bed","",input$epicBed$name)
            Reac$pcaMod<-mergeBed(input$epicBed$datapath, customHeader=TRUE,
                                  header=clnm
            )
            regionsN<- rownames (Reac$pcaMod)
            regions<- matrix (ncol=1,nrow=length(regionsN))
            rownames(regions)<-rownames (Reac$pcaMod)
            colnames(regions)<-'regions'
            regions[,1]<-regionsN
            #View (regions)
            rm (regionsN)
            #print (regions)
            Reac$pcaMod <- cbind (regions,Reac$pcaMod)
        }


        #bintabPath<- paste0 ( pointin(wdPath,'Binning'), input$binTable)

        #print (c('bintabPath'), bintabPath)
        #tagAlignPath<- input$tagAlignFiles$datapath
        #bintab<- read.table (bintabPath, sep='\t')

        #pcaMod<-pcaMatrixModel(bintabPath)

        #colnames(pcaMod)<-'regions'
        #pcaModReduct<- pcaMod [1:10,]
        print ('pcaMod.....Generated')
        #print (c('typeof pcaMod', typeof(pcaMod)))

        #View (pcaMod)

        output$tableScreen<- shiny::renderDataTable ({
            as.data.frame(Reac$pcaMod)
        })#, colnames=TRUE)

        output$secondRow<- shiny::renderUI ({

            shiny::wellPanel(

                shiny::fluidRow(

                    shiny::column (5, shiny::br(), # esce quando selezioni bin table bin table
                                   #textInput(pcaNServer('bamLoadPath'), label='bam file path')
                                   # 3=================================================
                                   selectFile (pcaNServer('bamLoadPath'),
                                               path=pointin (wdPath,'Epi') ,
                                               label='bam file path',
                                               pattern=".bed"
                                   )
                    ),

                    shiny::column(3,
                                  shiny::uiOutput(pcaNServer("colSelectorSlot"))

                    ),

                    shiny::column (3, shiny::br(),
                                   textInput (pcaNServer('name'), label='col name')
                    ),

                    shiny::column (1, shiny::br(), shiny::br(),
                                   shiny::checkboxInput(pcaNServer('addBox'), 'add', value=FALSE)
                    )

                ),

                shiny::fluidRow(
                    shiny::column (3,
                                   shiny::actionButton (pcaNServer('addData'),'add data')
                    )


                )

            )


        })

        if (Reac$pcaMod[1,]>=2){

            output$thirdRow<- shiny::renderUI ({

                shiny::wellPanel(
                    shiny::fluidRow(
                        shiny::column(4,
                                      shiny::textInput(pcaNServer('fileName'),'file name') ),
                        shiny::column(2,shiny::br(),
                                      shiny::actionButton (pcaNServer('export'), 'export table')),
                        shiny::column(2,shiny::br(),
                                      shiny::checkboxInput(pcaNServer("checkNorm"), label = "Normalization", value = FALSE)),
                        shiny::column(4,
                                      shiny::uiOutput(pcaNServer('normTypeIn')) )
                    ),
                    shiny::fluidRow(shiny::column(12,
                                                  shiny::uiOutput(pcaNServer('normOpt_slot')) ))
                )

            })

        }
        Reac$A_pcaMod<-Reac$pcaMod
    })

    shiny::observeEvent(input$addData,{ #============addData 2  prima volta
        print ("addData 2")
        Reac$pcaMod<-Reac$A_pcaMod
        print ('add pressed')

        output$secondRow<- shiny::renderUI({

            shiny::wellPanel (shiny::h5('please wait', width='100%'))

        })

        print ('please wait')

        if (input$dataType=='bint.tsv'){
            pcaTable<-bamPca(paste0 ( pointin(wdPath,'Binning'),
                                      input$binTable),
                             paste0 ( pointin(wdPath,'Epi'),
                                      input$bamLoadPath)
                             , Reac$pcaMod, columnName=input$name, add=input$addBox )
            print (paste0(pointin(wdPath,'Epi'),input$bamLoadPath))
            # bedLoad<-read.table(paste0 ( pointin(wdPath,'Epi'),
            #                                 input$bamLoadPath),
            #                     sep="\t"
            #                     )
            #bedLoadTT<<-bedLoad

            #tabTT<<-merge(Reac$pcaMod, paste0 ( pointin(wdPath,'Epi'), input$bamLoadPath))

            #      )
            if (input$addBox==TRUE){

            } else {#input$addBox==FALSE

            }

        }

        if (input$dataType=='epiCounts'){
            pcaTable<-bamPca (paste0 ( pointin(wdPath,'Binning'),
                                       input$binTableM),
                              paste0 ( pointin(wdPath,'Epi'),
                                       input$bamLoadPath)
                              , pcaMod, columnName=input$name,
                              add=input$addBox )
        }


        # pcaTable<-bamPca (paste0 ( pointin(wdPath,'Binning'), input$binTable), paste0 ( pointin(wdPath,'Epi'), input$bamLoadPath)
        #                    , pcaMod, columnName=input$name, add=input$addBox )

        print ('bamPca.....OK')

        Reac$pcaMod<-pcaTable

        output$tableScreen<- renderDataTable ({
            as.data.frame(Reac$pcaMod)
        })


        output$thirdRow<- shiny::renderUI ({

            shiny::wellPanel(
                shiny::fluidRow(
                    shiny::column(4,
                                  shiny::textInput(pcaNServer('fileName'),'file name') ),
                    shiny::column(2,shiny::br(),
                                  shiny::actionButton (pcaNServer('export'),
                                                       'export table')),
                    shiny::column(2,shiny::br(),
                                  shiny::checkboxInput(pcaNServer("checkNorm"),
                                                       label = "Normalization",
                                                       value = FALSE)),
                    shiny::column(4, shiny::uiOutput(pcaNServer('normTypeIn')) )
                ),
                shiny::fluidRow(
                    shiny::column(12,
                                  shiny::uiOutput(pcaNServer('normOpt_slot')) ))
            )

        })

        output$secondRow<- shiny::renderUI ({

            shiny::wellPanel(

                shiny::fluidRow(

                    shiny::column (5,
                                   #textInput(pcaNServer('bamLoadPath'), label='bam file path')
                                   # 4 =======================================
                                   selectFile (pcaNServer('bamLoadPath'),
                                               path=pointin (wdPath,'Epi') ,
                                               label='bad file path',
                                               pattern=".bed"
                                   )
                    ),

                    shiny::column(3,
                                  shiny::uiOutput(pcaNServer("colSelectorSlot"))

                    ),

                    shiny::column (3, shiny::br(),
                                   shiny::textInput (pcaNServer('name'), label='col name')
                    ),

                    shiny::column (1, shiny::br(), shiny::br(),
                                   shiny::checkboxInput(pcaNServer('addBox'),
                                                        'add',
                                                        value=FALSE)
                    )

                ),

                shiny::fluidRow(
                    shiny::column (3,
                                   shiny::actionButton (pcaNServer('addData'),'add data')
                    )


                )

            )

        })



        Reac$A_pcaMod<-Reac$pcaMod
    })

    shiny::observeEvent (input$export,{
        print ('export table start')
        nameToAssign<-paste0(input$fileName,'.epiCounts')
        #write.table ( pcaMod , nameF, sep='\t', quote=FALSE )
        pcaMod<-Reac$A_pcaMod
        #normalization
        if (input$checkNorm==TRUE){

            if (input$normType=='RPM'){
                # epiNorm<-pcaMod

                for (i in 2:length (Reac$pcaMod[1,])) {
                    somma<-sum (as.numeric(Reac$pcaMod[,i]))
                    ratio<-somma/1000000
                    Reac$pcaMod[,i] <-  rep (as.numeric(Reac$pcaMod[,i])/ratio)

                }

            } else {

                if (input$normType=='byInput'){
                    print ('byInput norm start')
                    #pcaModTT2<<-pcaMod
                    pcaMod2<-Reac$pcaMod[,-which(colnames(pcaMod) %in% input$choiceInput)] #così puoi rimuovere la colonna specifica
                    #pcaMod2<-pcaMod[,-which(colnames(TTpcaMod) %in% 'H4K20me1')] #così puoi rimuovere la colonna specifica
                    pcaMod3= as.matrix(Reac$pcaMod[,input$choiceInput]) #generi matrice dei soli input
                    m<-sum(as.numeric(pcaMod3))

                    for (i in 2:length(pcaMod2[1,])){
                        n<-sum(as.numeric(pcaMod2[,i]))

                        pcaMod2[,i]<- rep ( ((as.numeric(pcaMod2[,i]))*(as.numeric(m))) / ( (as.numeric(pcaMod3[,1])) * (as.numeric(n)) ) )

                    }
                    Reac$pcaMod<-pcaMod2
                }

            }



        }
        Reac$A_pcaMod<-Reac$pcaMod
        HCRwrite (Reac$pcaMod, nameToAssign , path=pointin(wdPath, 'PCA') ,  quote=FALSE )
        output$pcaTableSlot<- shiny::renderUI ({
            # selectFile (pcaNServer('pcaTableLoaded'),
            #             path=pointin(wdPath, 'PCA') ,
            #             'pca table',
            #             subset=TRUE,
            #             pattern='.epiCounts.tsv')
            shiny::fileInput(pcaNServer('pcaTableLoaded'),
                             lebel="pca table"
            )


        })
        print ('table export SUCCESS')

    })

    shiny::observeEvent(input$startPca,{

        print ('.....start pca.....')


        #pcaTable3<- read.table (input$pcaTableLoaded$datapath, sep='\t', header=TRUE)

        #================================old pipe
        # pcaTable3<- read.table (paste0 (pointin(wdPath,'PCA'),
        #                                 input$pcaTableLoaded
        # ),
        # sep='\t',
        # header=TRUE)
        # columnsNames<- pcaTable3[,1]
        # pcaTable3<- pcaTable3[,-1]
        # #View (pcaTable3)
        # #print ('readTable.....OK')
        # zScoreTable<- zScore (pcaTable3)
        # #print ('zscore.....OK')
        # #zScoreTable[is.na(zScoreTable)] <- 0
        # tTable3<- t(zScoreTable)
        # colnames (tTable3)<-columnsNames
        # #View (tTable3)
        # print ('traspose.....OK')
        # tPca<- prcomp (tTable3)
        # Reac$tP<-tPca$rotation
        # #print (tPca)
        # #View (tPca)
        # #regions <-c('regions',colnames(tTable3))
        # regions <-colnames(tTable3)
        #
        # exportThis<- cbind (regions,tPca$rotation)
        # Reac$S_exportThis<-exportThis
        # #View (exportThis)
        # print ('pca.....OK')
        # pcNum<- length (tPca$rotation[1,])
        # print (c('pcNum.....OK', pcNum))
        #================================================end old pipe


        #================================================new Start
        require(HiTC)


        intdata_path<-paste0 (pointin(wdPath,'Normalization'),input$pcaTableLoaded)

        outpath<-pointin(wdPath,'Downstream')

        PCnumber=input$PCs

        print(paste0("matrix Path: ",intdata_path))
        print(paste0("outpath: ",outpath))
        print(paste0("PCnumber: ",PCnumber))

        intdata<-read.table(intdata_path,sep="\t",header=TRUE)
        #intdataTT<<-intdata
        rownames(intdata)<-intdata$index
        intdata<-intdata[,-1]
        intdata<-Matrix::as.matrix(intdata)
        colnames(intdata)<-rownames(intdata) #solo se simmetriche ordinate

        #int<-data.matrix(intdata)

        #binGr<-c()
        # testnam<-rownames(intdata)[1]
        #
        # chr<-strsplit(testnam,":")[[1]][1]
        # cord<-strsplit(testnam,":")[[1]][2]
        # start<-strsplit(cord,"-")[[1]][1]
        # end<-strsplit(cord,"-")[[1]][2]

        chr<-c()
        cord<-c()
        start<-c()
        end<-c()
        for (i in 1:length(rownames(intdata))){
            complete<-rownames(intdata)[i]
            chr[i]<-strsplit(complete,":")[[1]][1]
            cord[i]<-strsplit(complete,":")[[1]][2]
            start[i]<-strsplit(cord[i],"-")[[1]][1]
            end[i]<-strsplit(cord[i],"-")[[1]][2]

        }

        df<-data.frame(chr,start,end)
        ls<-list(chr=chr,start=start,end=end)
        gr<-makeGRangesFromDataFrame(df)

        #clnam<-rep(paste0("cl",1:length(intdata[1,])))
        #rwnam<-rep(paste0("rw",1:length(intdata[,1])))
        bnnam<-rep(paste0("bn",1:length(intdata[,1])))
        originalNames<-rownames(intdata)


        #rownames(intdata)<-rwnam
        #colnames(intdata)<-clnam

        rownames(intdata)<-bnnam
        colnames(intdata)<-bnnam

        grrw<-gr
        #names(grrw)<-rwnam
        names(grrw)<-bnnam
        grcl<-gr
        #names(grcl)<-clnam
        names(grcl)<-bnnam

        intdata<-Matrix::Matrix(intdata)#,dimnames=ls)
        #names(gr)<-rownames(intdata)

        htcxp<-methods::new("HTCexp",intdata,xgi=grcl,ygi=grcl)
        #htcxp<-new("HTCexp",intdata)
        #pr<-pca.hic(htcxp,normPerExpected=FALSE,npc=1)
        #pr1<-pca.hic(htcxp,normPerExpected=TRUE,npc=1)
        #pr2<-pca.hic(htcxp,normPerExpected=TRUE,npc=2)
        #pr3<-pca.hic(htcxp,normPerExpected=TRUE,npc=3,asGRangesList=TRUE)
        pcaRes<-pca.hic(htcxp,normPerExpected=FALSE,npc=PCnumber,asGRangesList=FALSE)

        resTable<-matrix(ncol=PCnumber,nrow=length(originalNames))
        colnames(resTable)<-rep(paste0("PC",1:PCnumber))
        rownames(resTable)<-originalNames
        for (i in 2:length(pcaRes)){
            resTable[,i-1]<-pcaRes[[i]]
        }

        bin<-colnames(resTable)
        resTable<-rbind(bin,resTable)
        colnames(resTable)<-NULL

        write.table(resTable,paste0(outpath,"/pca_",input$pcaTableLoaded),sep="\t",quote=FALSE,col.names = FALSE, row.names =TRUE)

        #=========================================new end

        #=========================================old plot
        # output$pcaPlot <- shiny::renderPlot ({
        #     barplot(tPca$rotation[1:100,1])
        # })
        print ('plot.....OK')

        # output$selectPCui<- shiny::renderUI ({
        #
        #     shiny::wellPanel (
        #
        #         shiny::fluidRow (
        #             shiny::column (4,
        #                            shiny::selectInput(pcaNServer("select"),
        #                                               label = h5("PC to show"),
        #                                               choices = 1:pcNum,
        #                                               selected = 1)
        #             ),
        #             shiny::column (4,
        #                            shiny::textInput (pcaNServer("exportEvName"),
        #                                              label=h5('File Name'),
        #                                              value =('evToPlot'))
        #             ),
        #             shiny::column (4, shiny::br(), shiny::br(),
        #                            shiny::actionButton (pcaNServer("exportEvButton"),
        #                                                 label='Save')
        #             )
        #         )
        #
        #     )
        #     #    selectInput(pcaNServer("select"), label = h5("PC to show"),
        #     #                choices = 1:pcNum,
        #     #                selected = 1)
        #
        # })


        # observeEvent (input$exportEvButton, {
        #   print ('PCA esport start')
        #   tabName<-paste0 (input$exportEvName)
        #   #write.table (tPca$rotation, tabName, sep= "\t", quote=FALSE )
        #   #  View (tPca$rotation)
        #   #  print (paste0 (wdPath, 'Post-Processing'))
        #   HCRwrite (exportThis, tabName, path=pointin (wdPath,'PCA') , quote=FALSE)
        #
        # })
        #===============================================old plot end

    })

    shiny::observeEvent (input$exportEvButton, {
        print ('PCA export start')
        tabName<-paste0 (input$exportEvName)
        #write.table (tPca$rotation, tabName, sep= "\t", quote=FALSE )
        #  View (tPca$rotation)
        #  print (paste0 (wdPath, 'Post-Processing'))
        HCRwrite (Reac$S_exportThis, tabName,
                  path=pointin (wdPath,'PCA') ,
                  quote=FALSE)

    })

    shiny::observeEvent (input$select, {
        pc<- as.numeric(input$select)
        print (Reac$tP[1:100,pc])
        output$pcaPlot <- shiny::renderPlot ({
            barplot(Reac$tP[1:100,pc])
        })

    })

    shiny::observeEvent (input$checkNorm,{



        if (input$checkNorm==TRUE) {


            output$normTypeIn<- shiny::renderUI ({

                shiny::wellPanel (
                    shiny::selectInput (pcaNServer ('normType'),
                                        label='type',
                                        choices=c('RPM','byInput'),
                                        selected='RPM'))

            })

            # if (input$normType=='RPM'){
            #   # output$normOpt_slot<- renderUI ({
            #   #
            #   #   wellPanel (selectInput (pcaNServer ('normType'), label='type', choices=c('RPM','byInput')))
            #   #
            #   # })
            # } else {
            #
            #   if (input$normType=='byInput'){
            #     pcaMod<-Reac$A_pcaMod
            #     output$normOpt_slot<- renderUI ({
            #
            #       #wellPanel (selectInput (pcaNServer ('normType'), label='type', choices=c('RPM','byInput')))
            #       fluidRow (column(12,
            #
            #                        selectInput(pcaNServer('choiceInput'), label='Input Column', choices=colnames(pcaMod))
            #
            #                        ))
            #
            #     })
            #   }
            #
            # }



        } else
            output$normTypeIn<- shiny::renderUI ({
                shiny::wellPanel ()
            })
        {

            }

    })

    shiny::observeEvent (input$dataType,{

        if (input$dataType=='bint.tsv'){
            output$selFileSlot <- shiny::renderUI ({
                selectFile (pcaNServer('binTable'),
                            path=pointin (wdPath, 'Binning') ,
                            label='select binTable file',
                            subset=TRUE,
                            pattern='.bint')
            })
        }
        if (input$dataType=='epiCounts'){
            output$selFileSlot <- shiny::renderUI ({

                shiny::wellPanel (
                    # shiny::fluidRow(
                    #     selectFile (pcaNServer('binTableM'),
                    #                 path=pointin (wdPath, 'Binning') ,
                    #                 label='select binTable file',
                    #                 subset=TRUE,
                    #                 pattern='.bint')
                    # ),
                    shiny::fluidRow(
                        selectFile (pcaNServer('epicTable'),
                                    path=pointin (wdPath, 'PCA') ,
                                    label='select epiCounts file',
                                    subset=TRUE,
                                    pattern='.epiCounts.tsv')
                    )
                )

            })
        }

        if (input$dataType=="EpiCounts.bed"){
            output$selFileSlot <- shiny::renderUI ({
                shiny::wellPanel (
                    # shiny::fluidRow(
                    #     selectFile (pcaNServer('binTableM'),
                    #                 path=pointin (wdPath, 'Binning') ,
                    #                 label='select binTable file',
                    #                 subset=TRUE,
                    #                 pattern='.bint')
                    # ),
                    shiny::fluidRow(
                        shiny::fileInput(
                            pcaNServer('epicBed'),
                            label='select EpiCounts.bed',
                            multiple=TRUE
                        )
                        # selectFile (pcaNServer('epicTable'),
                        #             path=pointin (wdPath, 'PCA') ,
                        #             label='select epiCounts file',
                        #             subset=TRUE,
                        #             pattern='.epiCounts.tsv')
                    )
                )
            })
        }

    })

    shiny::observeEvent (input$normType,{
        if (input$normType=='RPM'){
            # output$normOpt_slot<- renderUI ({
            #
            #   wellPanel (selectInput (pcaNServer ('normType'), label='type', choices=c('RPM','byInput')))
            #
            # })
        } else {

            if (input$normType=='byInput'){
                Reac$pcaMod<-Reac$A_pcaMod

                output$normOpt_slot<- shiny::renderUI ({

                    #wellPanel (selectInput (pcaNServer ('normType'), label='type', choices=c('RPM','byInput')))
                    shiny::fluidRow (
                        shiny::column(12,
                                      shiny::selectInput(pcaNServer('choiceInput'),
                                                         label='Input Column',
                                                         choices=colnames(Reac$pcaMod)[2:length(colnames(Reac$pcaMod))])

                        ))

                })
            }

        }
    })

}
