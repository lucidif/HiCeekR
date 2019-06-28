##networksV2_Visualization


#UI===================================================================
networksV2_Visualization_UI <- function(id, label = "Visualization") {
    netv2NS <- shiny::NS(id)

    shiny::fluidPage(

        # custHeaderPanel("ReportingTools",
        #                 js = list.files(system.file("extdata/jslib", package="ReportingTools"),
        #                                 full.names=TRUE),
        #                 css = list.files(system.file("extdata/csslib", package="ReportingTools"),
        #                                  pattern="bootstrap.css", full.names=TRUE)
        # ),



         shiny::tabsetPanel (
        #
             shiny::tabPanel ('load data',
                shiny::fluidRow ( #bottom panel
                    shiny::wellPanel(

                        #=================FIRST ROW
                        shiny::fluidRow(
                            shiny::column(2, shiny::br()
                                ,shiny::helpText('contact table')
                            )
                        ,shiny::column(8
                            ,shiny::uiOutput(netv2NS('conTableSlot'))
                        #                  #                                # selectFile ( netv2NS('conTable'),
                        #                  #                                #         pointin(wdPath,
                        #                  #                                #                 'Binning'),
                        #                  #                                #         label="",
                        #                  #                                #         subset=TRUE,
                        #                  #                                #         pattern='_conTable.tsv')
                            )
                        ),

                        #===============SECOND ROW
                        shiny::fluidRow(
                            shiny::column(2, br(),
                                shiny::helpText('normalized matrix')
                                ),
                            shiny::column(8,
                                shiny::uiOutput(netv2NS('conMatrixSlot'))
                                )
                            ),

                        #===============THIRD ROW
                    # shiny::fluidRow(
                    #     shiny::column(2, br(),
                    #                   shiny::helpText('binTable')
                    #     ),
                    #     shiny::column(8,
                    #                   shiny::uiOutput(netv2NS('binTableSlot'))
                    #     )
                    # ),

                    #=================FOURTH ROW
                    shiny::fluidRow(
                        shiny::column(2, br(),
                                      shiny::helpText('Annotation')
                        ),
                        shiny::column(8,
                                      shiny::uiOutput(netv2NS('AnnotationSlot'))
                        )
                    )

                    #=================4,5 Row
                    ,shiny::fluidRow(
                        shiny::column(2, br(),
                                      shiny::helpText('Expression data (facultaty)')
                        ),
                        shiny::column(8,
                                      shiny::fileInput(netv2NS('exprFiles'),
                                                       label="select tsv files",
                                                       multiple=TRUE
                                                       )
                        )
                    )
                    #===============FIFTH ROW
                     ,shiny::fluidRow(

                         shiny::column(8 , shiny::br()
                            ,shiny::actionButton(netv2NS('setButton'),
                                                label='set input')
                                                     )
                         )
                    # )
                    #========================


                )


                    )
            ),



            shiny::tabPanel ('show',

                    shiny::fluidRow (

                          shiny::column(3, #in this thera are all side panel obj
                                #shiny::wellPanel()
                                shiny::uiOutput (netv2NS('sidePanelSlot'))


                          ),

                        shiny::column (9, #in this there are all main screen obj

                                shiny::fluidRow (

                                    shiny::column (12, #bottom panel with region option



                                            shiny::fluidRow (
                                                shiny::column (12,
                                                    shiny::uiOutput (netv2NS('bottomPanelSlot'))
                                              ))



                                      )

                                  ),
                                shiny::fluidRow (
                                    shiny::column (12,

                                        networkD3::forceNetworkOutput (netv2NS('networkSlot'))

                                  )),

                                shiny::fluidRow (
                                    shiny::column(12,
                                        shiny::uiOutput(netv2NS('tables')))
                                  ),

                                shiny::fluidRow (
                                    shiny::column (12,
                                        shiny::uiOutput (netv2NS('genesTab_optionsSlot'))
                                  )),

                                shiny::fluidRow (
                                    shiny::column (12,
                                        shiny::dataTableOutput(netv2NS('genesTab'))
                                  ))


                          )

                      )

            )

        )




    )

}


#Server================================================================

networksV2_Visualization_Server <- function(input, output,
                                            session, stringsAsFactors, wdPath) {

    # require (ReportingTools)
    # require (hwriter)
    # require (gProfileR)
    # require (ggplot2)

    resave <- function(..., list = character(), file) {
        previous  <- load(file)
        var.names <- c(list, as.character(substitute(list(...)))[-1L])
        for (var in var.names) assign(var, get(var, envir = parent.frame()))
        save(list = unique(c(previous, var.names)), file = file)
    }

    nsServer<-session$ns
    #source ('diffHic_fun.R')
    savedValue<-shiny::reactiveValues (regiSave='none', resulSave='none')

    ##initialize selectFiles
    output$conTableSlot<-shiny::renderUI({
        selectFile( nsServer('conTable'),
                     pointin(wdPath,
                             'Binning'
                             ),
                     label="",
                     subset=TRUE,
                     pattern='_conTable.tsv')
    })

    output$conMatrixSlot<-shiny::renderUI({
        selectFile ( nsServer('conMatrix'),
                     pointin(wdPath,
                             'Normalization'),
                     label="",
                     subset=TRUE,
                     pattern='_matrix')
    })

    # output$binTableSlot<-shiny::renderUI({
    #     selectFile(nsServer('binTable'),
    #                pointin(wdPath,'Binning'),
    #                label="",
    #                subset=TRUE,
    #                pattern='bint.bed')
    # })

    output$AnnotationSlot<-shiny::renderUI({

        #shiny::fileInput(nsServer('Annotation'), label="")

        selectFile ( nsServer('Annotation'),
                     pointin(wdPath,'Annotation'),
                     label=""
                     #,pattern='.gtf'
                     )
    })


    ##observers===========================================================

    ##observe set button===============================================
    shiny::observeEvent (input$setButton,{

        #calculate the parameter that you need

        # contactPath <-
        # matrixPath  <-
        # bintPath    <-

        #1) chromosome from normatrix matrix
        dataRegions<-regionsInMatrix (paste0 (pointin(wdPath,'Normalization'),input$conMatrix))
        dataChr<- unique(dataRegions[,1])
        #print (dataChr)
        #matrixChromosomes <<- HiClass$new ('chromosomes extract from matrix of interest', dataChr)
        #anchorchr.unique<<- matrixChromosomes$hisave

        output$bottomPanelSlot<- shiny::renderUI({

            shiny::wellPanel (

                shiny::fluidRow (

                    shiny::column (4,
                        shiny::selectInput(nsServer("chromo"),
                                           label = h5("chromosome"),
                                        choices = dataChr,
                                        selected = 1)
                    ),

                    shiny::column(4,
                        shiny::numericInput(nsServer("startWi"),
                                            label = h5("start"),
                                            value = 128900001)
                    ),

                    shiny::column(4,
                        shiny::numericInput(nsServer("endWi"),
                                            label = h5("end"),
                                            value = 142600000)
                    )

                )

            )

        })


        output$sidePanelSlot<- shiny::renderUI({

            shiny::wellPanel(

                shiny::fluidRow(
                    shiny::column(12,
                        shiny::sliderInput(nsServer("normValue"),
                                           label = h5("normValue"), min = 0,
                                              max = 1 , value = 0.5, step=0.01)
                )

                # ,column (4, br(), br(),
                #         checkboxInput (nsServer('globalM'), label=h5('global'), value = FALSE)
                #         )

                ),

                shiny::fluidRow (shiny::column (6,
                                  #helpText (h5('distance'))
                                shiny::checkboxInput (nsServer('globalM'),
                                                      label=h5('global'),
                                                      value = FALSE)
                ),
                shiny::column (6,
                    shiny::checkboxInput (nsServer('intra'),
                                        label='intra Chr',
                                        value =FALSE)
                )
                ),

                shiny::fluidRow (column (12,

                                shiny::uiOutput (nsServer('distanceSliderSlot'))

                )),

                shiny::fluidRow (shiny::column (4,
                            busyIndUI(
                                shiny::actionButton (nsServer('viewButton'),
                                                     label='View',
                                                     class = "btn-primary")

                )),

                shiny::column (8,
                        shiny::textOutput (nsServer('noRegionsWarning'))
                )

                )

            )

        })


    })


    ##observe check of intra/extra chromosomal regions========================
    shiny::observeEvent (input$intra,{


        if (input$intra==TRUE){
            print ('sono in TRUE')
            output$distanceSliderSlot<- shiny::renderUI ({

                shiny::wellPanel (

                    shiny::fluidRow(
                        shiny::column(12,
                            shiny::numericInput(nsServer("distanceVal"),
                                                label = h5("min bin distance"),
                                                value = 0)
                            )
                        )

                )


            })

        } else {
            print ('non sono in TRUE')
            output$distanceSliderSlot<- shiny::renderUI({

                shiny::wellPanel()

            })

        }

    })


    ##observe view button (in questo fa tutto fino al plot finale )
    shiny::observeEvent (input$viewButton,{

        busyIndServer("viewButton",{
        #1) acquisizioni di dati


        contactPath   <- paste0 (pointin(wdPath,'Binning'),input$conTable)
        matrixPath    <- paste0 (pointin(wdPath,'Normalization'),input$conMatrix)
        #bintPath      <- paste0 (pointin(wdPath,'Binning'),input$binTable)
        bintPath      <- paste0 (pointin(wdPath,'Binning', sys=TRUE),"allRegions.bint.bed")
        bint.table<-read.table (bintPath, header=FALSE,sep='\t')
        annoPath      <- paste0 (pointin(wdPath,'Annotation'),input$Annotation)


        chr   <- as.character (input$chromo)
        start <- as.numeric   (input$startWi)
        end   <- as.numeric   (input$endWi)



        ratio<- as.numeric (input$normValue)  #value from 0 to 1
        globalMax<- input$globalM


        if (input$intra==FALSE){

            minDistance<- 'NA'

        } else {

            minDistance<- input$distanceVal

        }

        print (paste0('contactPath:',contactPath))
        print (paste0('matrixPath:',matrixPath))
        print (paste0('bintPath:',bintPath))
        print (paste0('annoPath:',annoPath))
        print (paste0('chr:',chr))
        print (paste0('start:',start))
        print (paste0('end:',end))
        print (paste0('ratio:',ratio))
        print (paste0('minDistance:', minDistance))
        print (paste0('globalMax:', globalMax))

        save(contactPath, matrixPath, bintPath, annoPath,chr, start, end, ratio, minDistance, globalMax, file=paste0(pointin(wdPath,'Visualization'),"/network.Rdata"))

        # controllo della validità delle coordinate impostate==================

        #======================================================================


        #savedValue$gobalOpt=globalMax
        cordinate=paste0(chr,':',start,'-',end)
        ##new standard ex: HIC2_0.1_intraChrDist1_globalTRUE_minRange0.1_chr14:105548398-107323566.html

        savedValue$tamplateName=paste0 ('_intraChrDist',
                                        minDistance,
                                        '_global',
                                        globalMax,
                                        '_minRange',
                                        ratio,
                                        '_',
                                        cordinate)
        savedValue$chromo=chr
        ########################################################################################################
        #test_extractFromContactTable ==========================================================================
        ########################################################################################################

        zeroMatch<-FALSE


        ##contactTable header 'T_Chr','T_start','T_end','T_width','A_Chr','A_start','A_end','A_width','counts','indexT','indexA'

        #con<-HCRread ( file='' , contactPath)
        con<-read.table (contactPath, sep='\t', header=TRUE)
        #conTT<<-con

        le<-length (con[,1])


        #reg1-reg2-id1-id2-normValue-distance



        ##mo devi prendere la bint.tsv per trovarti gli indici e e distanze quando si parla di singoli cromosomi

        ##=====================================================================================================end

        #passiamo al subset di conta, quello che mi serve sapere è la mia regione di interesse con quali altre regioni dello stesso scromosoma
        #interagisce di più

        #se ti trasfrotmi le due colonne in array e estrai la regione di interesse con le funzioni (findRegionInBint) che già hai scritto, poi puoi usare le regioni
        #che ti sei trovato per subsettare conta

        #con[,1] , con[,2] , con[,3]

        ##tutte le regioni inserite nella stessa matrice da subsettare con le regioni di interesse

        contaTar <- matrix (ncol=3, nrow=le)
        contaTar[,1]<- as.character(con[,1])
        contaTar[,2]<- con[,2]
        contaTar[,3]<- con[,3]

        contaAn <- matrix (ncol=3, nrow=le)
        contaAn[,1]<- as.character(con[,5])
        contaAn[,2]<- con[,6]
        contaAn[,3]<- con[,7]



        ##conta forse non ti serve per niente
        conta<- matrix (ncol=6, nrow=le)
        colnames (conta)<- c('reg1','reg2','id1','id2','normValue','distance')

        conta[,1]<- rep (paste0(contaTar[,1],':',contaTar[,2], '-' , contaTar[,3] ) )
        conta[,2]<- rep (paste0(contaAn[,1],':',contaAn[,2], '-' , contaAn[,3] ) )

        ##=================================================================================end

        #con1bed<- as.matrix (conta[,1])
        #con2array<- as.array (conta[,2])

        #tarBed<- as.matrix (conta[,1])
        #ancBed<-as.matrix (conta[,2])
        tar<- matrix (ncol=6, nrow=le)
        tar[,1]<-contaTar[,1]
        tar[,2]<-contaTar[,2]
        tar[,3]<-contaTar[,3]

        tar[,4]<-contaAn[,1]
        tar[,5]<-contaAn[,2]
        tar[,6]<-contaAn[,3]

        anc<-matrix (ncol=6, nrow=le)
        anc[,1]<-contaAn[,1]
        anc[,2]<-contaAn[,2]
        anc[,3]<-contaAn[,3]

        anc[,4]<-contaTar[,1]
        anc[,5]<-contaTar[,2]
        anc[,6]<-contaTar[,3]

        ##subset region of interest versus all other of the dataset
        my_reg_tar<-findRegionInBint (tar, id=FALSE, imported=TRUE, chr=chr, start=start, end=end)
        rm (tar)
        tar_results<- matrix (ncol=6, nrow=length(my_reg_tar[,1]))
        tar_results[,1]<- rep (paste0(my_reg_tar[,1],':',my_reg_tar[,2],'-',my_reg_tar[,3]))
        tar_results[,2]<- rep (paste0(my_reg_tar[,4],':',my_reg_tar[,5],'-',my_reg_tar[,6]))
        rm (my_reg_tar)


        my_reg_anc<-findRegionInBint (anc, id=FALSE, imported=TRUE, chr=chr, start=start, end=end)
        rm (anc)
        anc_results<- matrix (ncol=6, nrow=length(my_reg_anc[,1]))
        anc_results[,1]<- rep (paste0(my_reg_anc[,4],':',my_reg_anc[,5],'-',my_reg_anc[,6]))
        anc_results[,2]<- rep (paste0(my_reg_anc[,1],':',my_reg_anc[,2],'-',my_reg_anc[,3]))
        rm (my_reg_anc)

        results<-rbind (tar_results, anc_results)
        results<- unique (results)
        colnames (results)<- c('reg1','reg2','id1','id2','normValue','distance')
        rm (tar_results)
        rm (anc_results)

        resultsLen <- length(results)

        print (paste0('resultsLen:',resultsLen))

        #1)       #controlla la lunghezza di results qui se e' maggiore di uno vai avanti
        if (length(results)>=1){





            ##ora bisogna riempire la colonna normValue  #===========================================================
            matri<-readContactMatrix(matrixPath)
            #matriTT<<-matri
            #sresultsTT<<-results
            if (globalMax==TRUE){
                umatri<-unlist(matri)
                umatri<-umatri[!is.na(umatri)]
                maxV<- max (as.numeric(umatri))
                rm(umatri)

            }


            # matri [results [1,1],results[1,2]]
            #
            #resu<-rep ( matri [results [,1],results[,2]],1)

            ##il for dovresti sotituirlo con un rep
            print (paste0('length(results[,1]:',length(results[,1])))
            #resultsTT3<<-results
            #matriTT<<-matri

            #if (length(results[,1])==0){
            #resultsTT2<<-results
            for (i in 1:length(results[,1])){

                results [i,5]<- as.numeric (matri[results [i,1],results[i,2]])
                print ('no Result')
            }
            #}

            rm (matri)
            #=========================================================================================================



            ##adesso ti devi aggiungere gli id prendendoli dalla bint.tsv  #==========================================

            #bint.table<-read.table (bintPath, header=FALSE,sep='\t')
            index<- matrix (ncol=1,nrow=length(bint.table[,1]))
            bint.table<-cbind(bint.table, index)
            bint.table[,7]<-rownames(bint.table)
            newBint<- matrix(ncol=1, nrow=(length(bint.table[,1])))
            rownames(newBint) <- rep (paste0(bint.table[,1],':',bint.table[,2],'-',bint.table[,3]))
            newBint[,1]<-bint.table[,7]

            rm (bint.table)

            #results[,3] <-rep (newBint[results[1,],])
            #resultsTT<<-results

            for (i in 1:length (results[,1])){
                results[i,3]<- as.numeric(newBint[results[i,1],1])
                results[i,4]<- as.numeric(newBint[results[i,2],1])
            }

            #=========================================================================================================

            #adesso cercati la distanza tra gli intercromosomici #====================================================

            #potrebbe non servirti

            resultsChr<-results

            split<-strsplit (results[,1],':')
            split2<-strsplit (results[,2],':')
            lenspli<- length (split)
            #chrlist<-c(1:lenspli)
            for (i in 1:lenspli){
                resultsChr[i,1]<-split[[i]][1]
                resultsChr[i,2]<-split2[[i]][1]
            }

            #passaggio lento
            for (i in 1:length(resultsChr[,1])){

                if (resultsChr[i,1]==resultsChr[i,2]){
                    #results[i,6]<- abs (as.numeric(results[i,3])-as.numeric(results[i,4]))
                    results[i,6]<- (as.numeric(results[i,3])-as.numeric(results[i,4]))
                } else {
                    results[i,6]<- 'NA'
                }

            }

            #=========================================================================================================

            #sorting for best normValue #=============================================================================

            results<-results[order(as.numeric(results[,5]),
                                                 decreasing = TRUE),]
            #print ('--------------------------------results------------------------------ ')
            #print (results)

            #=========================================================================================================

            ##subsets #===============================================================================================

            #puoi sortare per distanza, se vuoi sortare per regioni di altri cromosomi devi settare NA
            if (minDistance=='NA'){
                results<- subset (results, results[,6]=='NA')
            } else {
                results<- subset (results,
                                  as.numeric(results[,6])>=as.numeric(minDistance))
                #resultsTest<<- results
            }


            #2)       secondo controllo di results
            if (length(results[,1]>=1)){

                #subset per normValue

                if (globalMax==FALSE){

                    print ('-----------------------RESULTS------------------------')
                    print (results)

                    maxV<-which.max(as.numeric(results[,5]))
                    maxV<-as.numeric(results[maxV,5])
                    #maxV<- max (as.numeric(results[,5]))
                }

                print ('---------------------maxV-----------------------------')
                print (maxV)
                #resultsTT<<-results
                results<- subset (
                    results, (as.numeric(results[,5])/as.numeric(maxV))>=as.numeric(ratio)
                    )
                # resultsDF<- as.data.frame(results)
                # resultsDF[,3]<-as.numeric(as.character(results[,3]))
                # resultsDF[,4]<-as.numeric(as.character(results[,4]))
                # resultsDF[,5]<-as.numeric(as.character(results[,5]))
                # resultsDF[,6]<-as.numeric(as.character(results[,6]))
                # resultsDFTT<<-resultsDF
                savedValue$resulSave<-results
                #resultsTT<<-results
                #3)       terzo controllo di results
                if (length(results[,1]>=1)){

                    #=========================================================================================================

                    #plot the results

                    nodes1<- unique (results[,1])
                    #nodes1TT<<-nodes1
                    nodes2<- unique (results[,2])
                    #nodes2TT<<-nodes2
                    nodes<- c(nodes1,nodes2)
                    #nodesTT<<-nodes2
                    nodes<- unique(nodes)
                    print ('nodes')
                    print (nodes)
                    nodesMa<- matrix (ncol=3, nrow=length(nodes))
                    colnames(nodesMa)<-c('name','group','size')
                    nodesMa[,1]<-nodes

                    #OOOnodes<<-nodes

                    ##riempio la colonna group
                    split<-strsplit (nodesMa[,1],':')
                    lenspli<- length (split)
                    #chrlist<-c(1:lenspli)
                    for (i in 1:lenspli){
                        ##attenzione così facendo se poi è critto chr tipo con caratteri diversi tipo CHR o Chr non ti funziona
                        nodesMa[i,2]<- gsub ('chr', '',split[[i]][1], fixed=TRUE)
                    }

                    ##la colonna value non mi serve non so se la posso togliere quindi per il momento la riempio con un calore costante 12
                    nodesMa[,3]<- rep (12,length(nodesMa[,2]))

                    ##e mo ti voglio devi creare la marice dei link
                    #fai esattamento lo stesso ragioanmento che hai fatto con gli indici
                    rowNum<- matrix (ncol=1,nrow=length(nodesMa[,1]))
                    nodesExt<-cbind(nodesMa, rowNum)
                    nodesExt[,4]<- c(1:length(nodesMa[,1]))
                    print ('riempio linksMa')
                    linksMa<- matrix (ncol=3, nrow=length(results[,1]))
                    colnames(linksMa)<- c('source','target','value')
                    linksMa[,1]<-results[,1]
                    linksMa[,2]<-results[,2]
                    rownames(nodesExt) <- nodesExt[,1]
                    nodesExt<-nodesExt[,-1]
                    nodesExt[,3]<- as.numeric(nodesExt[,3])-1

                    for (i in 1:length (linksMa[,1])){
                        linksMa[i,1]<- as.numeric(nodesExt[linksMa[i,1],3])

                        linksMa[i,2]<- as.numeric(nodesExt[linksMa[i,2],3])
                    }




                    #linksMa<- matrix (ncol=3, nrow=length(results[,1]))


                    ##ora posso riempire linksMa con i normValue
                    linksMa [,3]<- as.numeric(results[,5])*50



                    if (length(results[,1])>=1){
                        # arRegion1<-as.array(results[,1])
                        # arRegion2<-as.array(results[,2])
                        # arRegions<- data.frame (arRegion1,arRegion2)
                        # simpleNetwork(arRegions, zoom=TRUE)

                        linksMa<- as.data.frame (linksMa)
                        #linksMa$source<-as.numeric (linksMa$source)
                        #linksMa$target<-as.numeric (linksMa$target)
                        #linksMa$value<-as.numeric(linksMa$value)
                        nodesMa<- as.data.frame (nodesMa)
                        #nodesMa$group<- as.numeric (nodesMa$group)
                        #nodesMa$size <- as.numeric (nodesMa$size)

                        output$noRegionsWarning<-renderText ({
                            ''
                        })


                        net<-networkD3::forceNetwork (Links=linksMa, Nodes=nodesMa, Source = "source", Target = "target",
                                                 Value = "value", NodeID = "name",
                                                 Group = "group", Nodesize= "size" , opacity = 0.8, zoom=TRUE)
                        resave(linksMa,nodesMa,file=paste0(pointin(wdPath,'Visualization'),"/network.Rdata"))

                        output$networkSlot<- networkD3::renderForceNetwork ({
                            #pdf(file=paste0(pointin(wdPath,"Visualization"),"netout.pdf"))
                            networkD3::forceNetwork (Links=linksMa, Nodes=nodesMa, Source = "source", Target = "target",
                                          Value = "value", NodeID = "name",
                                          Group = "group", Nodesize= "size" , opacity = 0.8, zoom=TRUE)

                            #dev.off()

                        })
                    } else {
                        print ('no region found')
                    }


                }#fine terzo controllo di results
                else {
                    output$noRegionsWarning<-shiny::renderText ({
                        'no regions found'
                    })
                }
            }#fine secondo controllo di results
            else {
                output$noRegionsWarning<-shiny::renderText ({
                    'no regions found'
                })
            }
        }#fine primo controllo di results
        else {
            output$noRegionsWarning<-shiny::renderText({
                'no regions found'
            })
        }


        ########################################################################################################
        ##======================================================================================================
        ########################################################################################################

        ##ORA SI DEVE SALVARE LA TABELLA DELLE ANNOTAZIONI E LA MOSTRA IN UN DATA TABLE

        an<-read.table (annoPath, sep='\t', header=TRUE)
        #an<-importAnnotation(name="",path=annoPath)

        #considera la tabella con gli ensebl come prima colonna
        #         an[,2]<-rep (an[,2]<-paste0('chr',an[,2]))   ##questo va eliminato con tabelle stile chrN
        ensembl<-as.matrix (an[,1])
        an<-an[,-1]
        an<-an[,1:4]
        an<-cbind (an,ensembl)
        #anTest<<-an

        leno<-length(nodes)
        print (paste0('leno:',leno))
        ##il punto è quando non trova geni al primo colpo che deve fare?
        n<-1
        for (i in 1:leno){
            print (i)
            if (i>1){
                regiPre<-regi
            }
            #nodesTest<<-nodes
            splitA<- strsplit( nodes[i], ':', fixed=TRUE)
            chrCor<-splitA[[1]][1]
            splitB<- strsplit (splitA[[1]][2],'-', fixed=TRUE)
            startCor<-splitB[[1]][1]
            endCor<- splitB[[1]][2]
            print (paste0('chrCor:   ', chrCor))
            print (paste0('startCor:  ',startCor))
            print (paste0('endCor:  ',endCor))

            regi <- findRegionInBint(an, id=FALSE, imported=TRUE, chr=chrCor ,
                                    start =startCor, end=endCor )

            #regiTest<<-regi

            if (length(regi[,1])>=1){
                print("regi[,1])>=1")
                colnames (regi)<- c('bin','start','end','geneName', 'ensembl')
                binInt<-nodes[i]



                regi[,1]<- rep (regi[,1]<-paste0 (nodes[i]))

                if (i > 1) {
                    regi<-rbind(regiPre,regi)
                }
            } else {
                print("regi[,1])<=1")
                ##qui devi agiungere la possibilità che non ci siano risultati anche al primo colpo
                ##il punto è che se non trova regioni al primo ciclo, quello entra qui e si ritrova senza niente!
                ## con questo if sembra risolto, c'è ancora un problema tipo se arriva alla fine e non trova regioni geniche?
                ## dovrebbe fare lo stesso errore che prima faceva al primo ciclo
                ## if (n==leno){non fare niente } else { ci metti l'if i==n }
                if (exists('regiPre')==TRUE){
                    print("exists('regiPre')==TRUE")
                    regi<-regiPre
                }

            }



        }

        print (regi)
        uniqueBin<- unique (regi[,1])


        #link<-matrix (ncol=1,nrow=length(regi[,1]))
        #colnames (link)<- 'link'
        #regi<- cbind (regi,link)

        #<a href='url'>link text</a>
        #regi[,5]<- rep (paste0("<a href=","'http://www.genecards.org/cgi-bin/carddisp.pl?gene=", regi[,4] ,"'>", "'http://www.genecards.org/cgi-bin/carddisp.pl?gene=", regi[,4],"</a>" ))
        regiNLcol4=regi[,4]
        regiNLcol5=regi[,5]

        #regiTT<<-regi

        print("expression files check")
        if(length(input$exprFiles$datapath)>0){
            print("length(input$exprFiles$datapath)>0")
            #exprfileSelect<<-input$exprFiles$datapath
            exprTable<-matrix(nrow=length(regi[,5]),ncol=length(input$expFiles$dataPath))
            rownames(exprTable)<-regi[,5]
            #exprTable<-read.table(input$expFiles$datapath, sep=)
            print("for ix in 1:length(input$exprFiles$datapath)")
            print (input$exprFiles$datapath)
            for(ix in 1:length(input$exprFiles$datapath) ){
                if (ix!=1){impTab2<-impTab}
                print(paste0("ix:",ix,"/",length(input$exprFiles$datapath)))
                impTab<-read.table(input$exprFiles$datapath[ix], header=FALSE, sep="\t")
                nam<-input$exprFiles$name[ix]
                #nmTT<<-nam
                print(paste0("file name:",nam))
                #check name univocity
                if (length(unique(as.character(impTab[,1])))!=length(impTab[,1])){
                    print("no unique names in exprTable")
                    geterrmessage("no unique names in exprTable")
                    stop()
                }
                rownames(impTab)<-impTab[,1]
                colnames(impTab)<-c("ensembl",nam)
                missGenes<-regi[which(!(regi[,5] %in% impTab[,1])),5]
                if (length(missGenes)!=0){
                    missValue<-rep(NA,length(missGenes))
                    miss<-data.frame(missGenes,missValue)
                    rownames(miss)<-miss[,1]
                    colnames(miss)<-colnames(impTab)
                    impTab<-rbind(impTab,miss)
                }

                if (ix!=1){
                    #impTab<-cbind(impTab,c(rep("NA",length(impTab[,1]))))
                    #impTab2<-impTab
                    missGenes<-which(!(regi[,5] %in% impTab[,1]))
                    #impTab<-cbind(impTab2,impTab)
                    impTab<-merge(impTab2,impTab,by="ensembl")
                }
            }
            #regiTT<<-regi
            #merge(regi,impTab,by=)
            #impTabTT<<-impTab

            regiCols<-colnames(regi)
            impTabCols<-colnames(impTab)[-1]
            cortCols<-c(regiCols,impTabCols)
            regi<-merge(regi,impTab,by="ensembl")
            regi<-regi[,cortCols]

        } else {
            print("no expr files selected")
            #exprfileSelect<<-"none"
        }
        # regiCols<-colnames(regi)
        # impTabCols<-colnames(impTab)[-1]
        # cortCols<-c(regiCols,impTabCols)
        # regi<-merge(regi,impTab,by="ensembl")
        # regi<-regi[,cortCols]


        regi[,4]<- rep(paste0("<a href=","'http://www.genecards.org/cgi-bin/carddisp.pl?gene=", regi[,4], "'>", regi[,4],"</a>" ))
        regi[,5]<- rep(paste0("<a href=","'http://www.ensembl.org/Multi/Search/Results?q=", regi[,5], "'>", regi[,5],"</a>" ))

        savedValue$regiSave<-regi
        print("genes table link exported")
        regiNL<-regi

        #regiNLTT<<-regi
        #regiNLcol4TT<<-regiNLcol4
        #regiNLcol5TT<<-regiNLcol5

        regiNL[,4]<-regiNLcol4
        regiNL[,5]<-regiNLcol5
        print("export regions NL table")
        savedValue$regiNLsave=regiNL

        regi<- as.data.frame (regi)
        print ("export Regions table data frame")
        #regiNL<<-regiNL

        #addExpr IF present


        #modifica results aggiungengo i geneName presi da regi:

        #results format ->reg1  reg2  id1    id2    normValue    distance

        #aggiungi due colonne a results
        #controlla il nome dei bin nel results
        #per la reg1  cerca in regi tutti i geni associati a quel regione (bin)
        #per la reg2  cerca in regi tutti i geni associati a quella regione (bin)
        #vai alla riga successiva e prosegui fino alla fine
        #print (results)

        if (length(results[,1])>=1){

            #aggiungi due colonne a results
            resNam<-matrix (ncol=2, nrow=length(results[,1]))
            colnames(resNam) <- c('reg1_genes','reg2_genes')
            results<-cbind (results,resNam)
            resultsNL=results
            #resultsTT<<-results
            #regiTT<<-regi
            #controlla nomi delle reg e aggiungi genesNames nella colonne 7 e 8
            for (i in 1:length(results[,1])){
                binOfInterest1<-results[i,1]
                binOfInterest2<-results[i,2]

                regSub <- subset (regi , regi[,1]==binOfInterest1)
                regNLsub= subset (regiNL, regiNL[,1]==binOfInterest1)
                genesNames<- as.array (regSub[,4])
                genesNLNames= as.array (regNLsub[,4])
                #genesNames<- rep (paste0(genesNames,'<br>'))
                genesNames<- rep (paste0(genesNames,','))
                genesNLNames=rep (paste0(genesNLNames,','))
                genesNames<- Reduce (paste0,genesNames)
                genesNLNames= Reduce (paste0,genesNLNames)

                # results [i,7]<-genesNames
                # resultsNL [i,7]<-genesNLNames

                results[i,length(results[1,])-1]<-genesNames
                resultsNL[i,length(results[1,])-1]<-genesNLNames


                regSub <- subset (regi , regi[,1]==binOfInterest2)
                regNLsub= subset (regiNL, regiNL[,1]==binOfInterest2)

                print (regSub)
                #regSubTT<<-regSub

                genesNames<- as.array (regSub[,4])
                genesNLNames= as.array (regNLsub[,4])
                genesNames<- rep (paste0(genesNames,','))
                genesNLNames=rep (paste0(genesNLNames,collapse=','))
                genesNames<- Reduce (paste0,genesNames)
                genesNLNames= Reduce (paste0,genesNLNames)

                results [i,length(results[1,])]<-genesNames
                resultsNL [i,length(results[1,])]<-genesNLNames
            }
            savedValue$resultsNLsave=resultsNL
            ##resultsNLTT<<-resultsNL
            #View (resultsNL)
            #resultsTT<<-results
            if (input$intra==TRUE){
                fileOutName<-paste0(ratio,"_",chr,"_",start,"_",end,"_gb",
                                    globalMax,"_mindist",minDistance,".txt")
            } else {
                fileOutName<-paste0(ratio,"_",chr,"_",start,"_",end,"_gb",
                                    globalMax,".txt")
            }

            print(paste0(pointin(wdPath,'Visualization',sys=FALSE),fileOutName))
            resave(resultsNL,file=paste0(pointin(wdPath,'Visualization'),"/network.Rdata"))
            write.table(resultsNL,paste0(pointin(wdPath,'Visualization',sys=FALSE),
                                       fileOutName), sep="\t",row.names=FALSE,
                        col.names=TRUE, quote=FALSE)

        }

        output$tables<- shiny::renderUI ({

            shiny::tabsetPanel (
                shiny::tabPanel ('Interactions',

                            shiny::fluidRow (
                                shiny::column(12,
                                    shiny::dataTableOutput( nsServer('resultable')
                                                            )
                                    )
                                )

                ),
                shiny::tabPanel ('Genes',

                    shiny::fluidRow ( shiny::column (12,
                                        shiny::wellPanel (
                                            shiny::fluidRow (
                                                shiny::column(6,
                                                    shiny::selectInput(nsServer('selectBin'),
                                                                        label='select bin of interest',
                                                                        choices=uniqueBin
                                                             )
                                                     ),
                                                    shiny::column (6, br(),
                                                        shiny::actionButton (nsServer('binSub'),
                                                                             label='start')
                                                     )
                                                 )
                                             )
                          )
                          ),

                        shiny::fluidRow (
                            shiny::column(12,
                                shiny::dataTableOutput(nsServer('genesTab2'))
                        ))

                ),

                shiny::tabPanel('Functional',

                    shiny::fluidRow(shiny::column(12,
                                        shiny::wellPanel(
                                            shiny::fluidRow(
                                                 #column (6, selectInput (nsServer('pathway_selectBin'), label='select bin of interest', choices=uniqueBin)),
                                                # shiny::column (3,
                                                #     shiny::numericInput(nsServer("threshold"),
                                                #                         label = h5("threshold"),
                                                #                         value = 0.001 )),

                                                shiny::column (2,
                                                    shiny::selectInput(nsServer('pathway_organism'),
                                                                    label=h5('organisim'),
                                                                    choices=c('hsapiens','mmusculus'))),

                                                shiny::column (3,
                                                    shiny::br(),
                                                    busyIndUI(
                                                        shiny::actionButton(

                                                            nsServer('pathFinder'),
                                                            label=h5('Enrich'),
                                                            class = "btn-primary"
                                                            ))
                                                         #actionButton(nsServer('exploreContact'), label=h5('find bin'))
                                                 )

                                             ),

                                            shiny::fluidRow(

                                                shiny::column(12,
                                                    shiny::uiOutput(nsServer('pathway_secondRow')))

                                             )

                                             # ,fluidRow(
                                             #
                                             #   column(12, dataTableOutput(nsServer('pathway_thirdRow')))
                                             #
                                             # )

                                         ))),

                        shiny::fluidRow(
                            shiny::column(12,
                                shiny::dataTableOutput(nsServer('pathway_thirdRow'))))


                )

            )

        })

        #CONTROLLA SE HAI DEGLI HTML REPORT SE CE LI HAI PRENDILI E GENERA UN DATA FRAME ALTRIMENTI VAI OLTRE
        #htmlFiles<-list.dirs (pointin(wdPath,'netReport'), pattern='www')
        htmlFiles<-list.files(paste0(pointin(wdPath,'netReports'),'www/'), pattern='.html')
        if (length(htmlFiles)==0){

            link<-as.data.frame ('none')
            savedValue$linksSe<-link
            #linksSeTT<<-link

            output$htmlReport<- shiny::renderDataTable({
                link
            },escape=FALSE)

        }else{
            print ('www substitution')
            unlink (paste0(getwd(),'/www'), recursive=TRUE)
            file.copy(from=paste0(pointin(wdPath,'netReports'),'www/'),
                      to=paste0(getwd(),'/'), recursive=TRUE)

            linksSet<-c()
            for ( i in 1:length(htmlFiles)){
                link<-htmlFiles[i]
                link<-paste0("<a href='", link,"' target='_blank'>", link,"</a>")
                linksSet[i]<-link
            }
            savedValue$linksSe<-as.data.frame(linksSet)
            #linksSeTT<<-as.data.frame(linksSet)
            output$htmlReport<- shiny::renderDataTable({
                as.data.frame(linksSet)
            },escape=FALSE)

        }

        output$genesTab2<- shiny::renderDataTable({
            regi
        },escape=FALSE)

        results<-as.data.frame(results)
        results[,1]<-as.character(results[,1])
        results[,2]<-as.character(results[,2])
        results[,3]<-as.numeric(as.character(results[,3]))
        results[,4]<-as.numeric(as.character(results[,4]))
        results[,5]<-as.numeric(as.character(results[,5]))
        results[,6]<-as.numeric(as.character(results[,6]))
        resave(results,file=paste0(pointin(wdPath,'Visualization'),"/network.Rdata"))
        output$resultable<- shiny::renderDataTable ({
            results
        }, escape=FALSE)
        #resutest<<-as.data.frame(results)
        savedValue$resulFrame=as.data.frame(results)
    })})

    ##observe genesTab options
    shiny::observeEvent(input$binSub,{

        regi<-savedValue$regiSave
        regiSub<-subset (regi, regi[,1]==input$selectBin)

        output$genesTab2<- shiny::renderDataTable({
            regiSub
        },escape=FALSE)

    })

    ##observe slots column
    shiny::observeEvent(input$fileInput_slot1,{

        tabPath<- input$fileInput_slot1$datapath
        tab<-HCRread(file='', path=tabPath)
        colu<- colnames (tab)

        output$pos_slot1<- shiny::renderUI ({
            shiny::wellPanel (

                shiny::fluidRow (

                    shiny::column (10,

                        shiny::selectInput (nsServer('selCol'),
                                            label='select column',
                                            choices=colu,
                                            selected=1
                            )

                    )



                )


            )
        })

    })

    shiny::observeEvent (input$fileInput_slot2,{

        tabPath_slot2<- input$fileInput_slot2$datapath
        tab_slot2<-HCRread(file='', path=tabPath_slot2)
        colu_slot2<- colnames(tab_slot2)

        output$pos_slot2<- shiny::renderUI({
            shiny::wellPanel(fluidRow (
                shiny::column(10,
                        shiny::selectInput(nsServer('selCol_slot2'),
                                            label='select column',
                                            choices=colu_slot2,
                                            selected=1
                        ))))
        })
    })

    #slot3 #================================
    shiny::observeEvent(input$fileInput_slot3,{

        tabPath_slot3<- input$fileInput_slot3$datapath
        tab_slot3<-HCRread (file='', path=tabPath_slot3)
        colu_slot3<- colnames (tab_slot3)

        output$pos_slot3<- shiny::renderUI({
            shiny::wellPanel (
                shiny::fluidRow (
                    shiny::column (10,
                        shiny::selectInput (nsServer('selCol_slot3'),
                                            label='select column',
                                            choices=colu_slot3,
                                            selected=1
                            )
                        )
                    )
                )
        })
    })

    #slot4 #================================
    shiny::observeEvent(input$fileInput_slot4,{

        tabPath_slot4<- input$fileInput_slot4$datapath
        tab_slot4<-HCRread(file='', path=tabPath_slot4)
        colu_slot4<- colnames (tab_slot4)

        output$pos_slot4<- shiny::renderUI({
            shiny::wellPanel(
                shiny::fluidRow (
                    shiny::column (10,
                        shiny::selectInput (nsServer('selCol_slot4'),
                                            label='select column',
                                            choices=colu_slot4,
                                            selected=1
                        )
                        )
                    )
                )
        })
    })

    #slot5 #================================
    shiny::observeEvent(input$fileInput_slot5,{

        tabPath_slot5<- input$fileInput_slot5$datapath
        tab_slot5<-HCRread(file='', path=tabPath_slot5)
        colu_slot5<- colnames (tab_slot5)

        output$pos_slot5<- shiny::renderUI ({
            shiny::wellPanel (
                shiny::fluidRow (
                    shiny::column (10,
                        shiny::selectInput(nsServer('selCol_slot5'),
                                           label='select column',
                                            choices=colu_slot5,
                                            selected=1
                        )
                        )
                    )
                )
        })
    })

    ##observe reportBut
    shiny::observeEvent(input$repButton,{

        resulShow=savedValue$resulFrame[,1:6]

        ##=============================================================================================
        ##slots in hrml Report=========================================================================
        ##=============================================================================================

        if (input$activeSlot1==TRUE){

            #subset column of interest
            toPlot=HCRread (file='', path=input$fileInput_slot1$datapath)
            #toPlotRegions=as.array(rownames(toPlot))
            toPlot=as.data.frame(toPlot[,input$selCol])
            #toPlot=c(toPlotRegions,toPlotValue)
            colnames(toPlot)=c(input$selCol)
            #maxVal<- as.numeric(max(toPlot[,1]))
            #minVal<- as.numeric(min(toPlot[,1]))
            maxVal<- as.numeric(max(toPlot[min(grep(savedValue$chromo,rownames(toPlot), fixed=TRUE)):max(grep (savedValue$chromo,rownames(toPlot), fixed=TRUE)),1]))
            minVal<- as.numeric(min(toPlot[min(grep (savedValue$chromo,rownames(toPlot), fixed=TRUE)):max(grep (savedValue$chromo,rownames(toPlot), fixed=TRUE)),1]))
            #toPlotTT<<-toPlot
            imagename <- c()
            ##per la colonna 1 sempre
            for (i in 1:nrow(resutest)){

                intRegion1<-resulShow[i,1]
                intRegion2<-resulShow[i,2]

                regBarValue1<-toPlot[intRegion1,1]
                regBarValue2<-toPlot[intRegion2,1]

                print (paste0 ('regBarValue1;regBarValue2:',regBarValue1,';',regBarValue2))

                imagename[i] <- paste0(input$reportName,"_plot1_", i, ".png")
                png(filename = paste0("www/", imagename[i]))

                plotAr=c(regBarValue1,regBarValue2)
                taplot=as.table (plotAr)
                print (taplot)
                rownames(taplot)=c(as.character(intRegion1),as.character(intRegion2))

                barplot(taplot, main=paste0(colnames(toPlot)[1],'_',intRegion1,'_',intRegion2),
                        xlab='', col=c("skyblue1","skyblue3"), ylim= c(minVal,maxVal),
                        #legend = rownames(counts),
                        beside=TRUE)

                dev.off()
                ##in imagename ci sono salvate i nomi delle immagini di caricare
            }

            #creo un nuovo elemento del mio dataframe che connette hai nomi i link delle immagini
            slot1=input$selCol #vedi bene dove ti conviene metterlo
            resulShow$Image <- hwriter::hwriteImage(imagename, link = imagename, table = FALSE, width=100, height=100)
            #riempire sempre l'ultimo potrebbe funzionare!
            colnames(resulShow)[length(resulShow[1,])]=slot1 #rename column image with col name of observed data
            #-------------------------------------------------------------------------------------------------

        }

        #resutest=resutest[c('reg1','reg2','id1','id2','normValue','distance','Image','genes1','genes2')]
        #resulShow=cbind(resulShow,savedValue$resulFrame[,7:8])

        ##====================================================================qui metti gli if degli slot
        ##alla fine di ogni slot metti resulShow=cbind(resulShow,savedValue$resulFrame[,7:8])
        if (input$activeSlot2==TRUE){

            #subset column of interest
            toPlot=HCRread (file='', path=input$fileInput_slot2$datapath)
            #toPlotRegions=as.array(rownames(toPlot))
            toPlot=as.data.frame(toPlot[,input$selCol_slot2])
            #toPlot=c(toPlotRegions,toPlotValue)
            colnames(toPlot)=c(input$selCol_slot2)
            # maxVal<- as.numeric(max(toPlot[,1]))
            # minVal<- as.numeric(min(toPlot[,1]))
            maxVal<- as.numeric(max(toPlot[min(grep (savedValue$chromo,rownames(toPlot), fixed=TRUE)):max(grep (savedValue$chromo,rownames(toPlot), fixed=TRUE)),1]))
            minVal<- as.numeric(min(toPlot[min(grep (savedValue$chromo,rownames(toPlot), fixed=TRUE)):max(grep (savedValue$chromo,rownames(toPlot), fixed=TRUE)),1]))
            ##toPlotTT<<-toPlot
            imagename <- c()
            ##per la colonna 1 sempre
            for (i in 1:nrow(resutest)){

                intRegion1<-resulShow[i,1]
                intRegion2<-resulShow[i,2]

                regBarValue1<-toPlot[intRegion1,1]
                regBarValue2<-toPlot[intRegion2,1]

                print (paste0 ('regBarValue1;regBarValue2:',regBarValue1,';',regBarValue2))

                imagename[i] <- paste0(input$reportName,"_plotSlot2_", i, ".png")
                png(filename = paste0("www/", imagename[i]))

                plotAr=c(regBarValue1,regBarValue2)
                taplot=as.table (plotAr)
                #print (taplot)
                rownames(taplot)=c(as.character(intRegion1),as.character(intRegion2))

                barplot(taplot, main=paste0(colnames(toPlot)[1],'_',intRegion1,'_',intRegion2),
                        xlab='', col=c("green1","green3"), ylim= c(minVal,maxVal),
                        #legend = rownames(counts),
                        beside=TRUE)

                dev.off()
                ##in imagename ci sono salvate i nomi delle immagini di caricare
            }

            #creo un nuovo elemento del mio dataframe che connette hai nomi i link delle immagini
            slot1=input$selCol_slot2 #vedi bene dove ti conviene metterlo
            resulShow$Image <- hwriter::hwriteImage(imagename,
                                                    link = imagename,
                                                    table = FALSE,
                                                    width=100,
                                                    height=100)
            #riempire sempre l'ultimo potrebbe funzionare!
            colnames(resulShow)[length(resulShow[1,])]=slot1 #rename column image with col name of observed data
            #-------------------------------------------------------------------------------------------------

        }

        if (input$activeSlot3==TRUE){

            #subset column of interest
            toPlot=HCRread(file='', path=input$fileInput_slot3$datapath)
            #toPlotRegions=as.array(rownames(toPlot))
            toPlot=as.data.frame(toPlot[,input$selCol_slot3])
            #toPlot=c(toPlotRegions,toPlotValue)
            colnames(toPlot)=c(input$selCol_slot3)
            # maxVal<- as.numeric(max(toPlot[,1]))
            # minVal<- as.numeric(min(toPlot[,1]))
            maxVal<- as.numeric(max(toPlot[min(grep (savedValue$chromo,rownames(toPlot), fixed=TRUE)):max(grep (savedValue$chromo,rownames(toPlot), fixed=TRUE)),1]))
            minVal<- as.numeric(min(toPlot[min(grep (savedValue$chromo,rownames(toPlot), fixed=TRUE)):max(grep (savedValue$chromo,rownames(toPlot), fixed=TRUE)),1]))
            #toPlotTT<<-toPlot
            imagename <- c()
            ##per la colonna 1 sempre
            for (i in 1:nrow(resutest)){

                intRegion1<-resulShow[i,1]
                intRegion2<-resulShow[i,2]

                regBarValue1<-toPlot[intRegion1,1]
                regBarValue2<-toPlot[intRegion2,1]

                print (paste0 ('regBarValue1;regBarValue2:',
                               regBarValue1,';',regBarValue2))

                imagename[i] <- paste0(input$reportName,"_plotSlot3_", i, ".png")
                png(filename = paste0("www/", imagename[i]))

                plotAr=c(regBarValue1,regBarValue2)
                taplot=as.table(plotAr)
                print (taplot)
                rownames(taplot)=c(as.character(intRegion1),
                                   as.character(intRegion2))

                barplot(taplot, main=paste0(colnames(toPlot)[1],'_',intRegion1,'_',intRegion2),
                        xlab='', col=c("red1","red3"), ylim= c(minVal,maxVal),
                        #legend = rownames(counts),
                        beside=TRUE)

                dev.off()
                ##in imagename ci sono salvate i nomi delle immagini di caricare
            }

            #creo un nuovo elemento del mio dataframe che connette ai nomi i link delle immagini
            colName=input$selCol_slot3 #vedi bene dove ti conviene metterlo
            resulShow$Image <- hwriter::hwriteImage(imagename,
                                                   link = imagename,
                                                   table = FALSE,
                                                   width=100,
                                                   height=100)
            #riempire sempre l'ultimo potrebbe funzionare!
            colnames(resulShow)[length(resulShow[1,])]=colName #rename column image with col name of observed data
            #-------------------------------------------------------------------------------------------------

        }

        if (input$activeSlot4==TRUE){

            #subset column of interest
            toPlot=HCRread (file='', path=input$fileInput_slot4$datapath)
            #toPlotRegions=as.array(rownames(toPlot))
            toPlot=as.data.frame(toPlot[,input$selCol_slot4])
            #toPlot=c(toPlotRegions,toPlotValue)
            colnames(toPlot)=c(input$selCol_slot4)
            # maxVal<- as.numeric(max(toPlot[,1]))
            # minVal<- as.numeric(min(toPlot[,1]))
            maxVal<- as.numeric(max(toPlot[min(grep (savedValue$chromo,rownames(toPlot), fixed=TRUE)):max(grep (savedValue$chromo,rownames(toPlot), fixed=TRUE)),1]))
            minVal<- as.numeric(min(toPlot[min(grep (savedValue$chromo,rownames(toPlot), fixed=TRUE)):max(grep (savedValue$chromo,rownames(toPlot), fixed=TRUE)),1]))
            #toPlotTT<<-toPlot
            imagename <- c()
            ##per la colonna 1 sempre
            for (i in 1:nrow(resutest)){

                intRegion1<-resulShow[i,1]
                intRegion2<-resulShow[i,2]

                regBarValue1<-toPlot[intRegion1,1]
                regBarValue2<-toPlot[intRegion2,1]

                print (paste0 ('regBarValue1;regBarValue2:',
                               regBarValue1,';',regBarValue2))

                imagename[i] <- paste0(input$reportName,"_plotSlot4_", i, ".png")
                png(filename = paste0("www/", imagename[i]))

                plotAr=c(regBarValue1,regBarValue2)
                taplot=as.table (plotAr)
                print (taplot)
                rownames(taplot)=c(as.character(intRegion1),
                                   as.character(intRegion2))

                barplot(taplot, main=paste0(colnames(toPlot)[1],'_',intRegion1,'_',intRegion2),
                        xlab='', col=c("yellow1","yellow3"), ylim= c(minVal,maxVal),
                        #legend = rownames(counts),
                        beside=TRUE)

                dev.off()
                ##in imagename ci sono salvate i nomi delle immagini di caricare
            }

            #creo un nuovo elemento del mio dataframe che connette hai nomi i link delle immagini
            colName=input$selCol_slot4 #vedi bene dove ti conviene metterlo
            resulShow$Image <- hwriter::hwriteImage(imagename,
                                           link = imagename,
                                           table = FALSE,
                                           width=100,
                                           height=100)
            #riempire sempre l'ultimo potrebbe funzionare!
            colnames(resulShow)[length(resulShow[1,])]=colName #rename column image with col name of observed data
            #-------------------------------------------------------------------------------------------------

        }

        if (input$activeSlot5==TRUE){

            #subset column of interest
            toPlot= HCRread(file='', path=input$fileInput_slot5$datapath)
            #toPlotRegions=as.array(rownames(toPlot))
            toPlot=as.data.frame(toPlot[,input$selCol_slot5])
            #toPlot=c(toPlotRegions,toPlotValue)
            colnames(toPlot)=c(input$selCol_slot5)
            # maxVal<- as.numeric(max(toPlot[,1]))
            # minVal<- as.numeric(min(toPlot[,1]))
            maxVal<- as.numeric(max(toPlot[min(grep (savedValue$chromo,rownames(toPlot), fixed=TRUE)):max(grep (savedValue$chromo,rownames(toPlot), fixed=TRUE)),1]))
            minVal<- as.numeric(min(toPlot[min(grep (savedValue$chromo,rownames(toPlot), fixed=TRUE)):max(grep (savedValue$chromo,rownames(toPlot), fixed=TRUE)),1]))
            #toPlotTT<<-toPlot
            imagename <- c()
            ##per la colonna 1 sempre
            for (i in 1:nrow(resutest)){

                intRegion1<-resulShow[i,1]
                intRegion2<-resulShow[i,2]

                regBarValue1<-toPlot[intRegion1,1]
                regBarValue2<-toPlot[intRegion2,1]

                print (paste0 ('regBarValue1;regBarValue2:',regBarValue1,';',regBarValue2))

                imagename[i] <- paste0(input$reportName,"_plotSlot5_", i, ".png")
                png(filename = paste0("www/", imagename[i]))

                plotAr=c(regBarValue1,regBarValue2)
                taplot=as.table (plotAr)
                print (taplot)
                rownames(taplot)=c(as.character(intRegion1),as.character(intRegion2))

                barplot(taplot, main=paste0(colnames(toPlot)[1],'_',intRegion1,'_',intRegion2),
                        xlab='', col=c("magenta1","magenta3"), ylim= c(minVal,maxVal),
                        #legend = rownames(counts),
                        beside=TRUE)

                dev.off()
                ##in imagename ci sono salvate i nomi delle immagini di caricare
            }

            #creo un nuovo elemento del mio dataframe che connette hai nomi i link delle immagini
            colName=input$selCol_slot5 #vedi bene dove ti conviene metterlo
            resulShow$Image <- hwriter::hwriteImage(imagename, link = imagename, table = FALSE, width=100, height=100)
            #riempire sempre l'ultimo potrebbe funzionare!
            colnames(resulShow)[length(resulShow[1,])]=colName #rename column image with col name of observed data
            #-------------------------------------------------------------------------------------------------

        }

        #resutest=resutest[c('reg1','reg2','id1','id2','normValue','distance','Image','genes1','genes2')]
        resulShow=cbind(resulShow,savedValue$resulFrame[,7:8])


        ##===============================================================================================


        #creo il report

        reportNam =paste0(input$reportName, savedValue$tamplateName)
        #View (regiNLsave)
        HCRwrite(savedValue$regiNLsave ,paste0('regions_', reportNam), "./www/")
        HCRwrite(savedValue$resultsNLsave,paste0('interactions_', reportNam ), "./www/")
        htmlRep5 <- ReportingTools::HTMLReport(shortName = reportNam,
                                                title = "",
                                                reportDirectory = "./www")
        #paste0(pointin(wdPath,'Plots'),'reports') )
        #"./reports")
        publish(resulShow, htmlRep5)
        finish(htmlRep5)

        file.copy(from='./www', to=pointin(wdPath,'netReports'), recursive=TRUE)
        print (pointin(wdPath,'netReports'))
        print ('report page CREATED')

        # output$htmlReport<- renderUI({
        #   includeHTML('/home/lucio/Dropbox/HiCeekR/HiCeekR_0.9/reports/my_html_test_HCR.html') ##DEVI ASSOLUTAMENTE SOSTITUIRLO CON UN PATH DINAMICO
        # },escape=FALSE)

        #link<-as.data.frame (paste0("<a href=","'", "/home/lucio/Dropbox/HiCeekR/HiCeekR_0.9/reports/my_html_test_HCR.html", "'>", 'report_link',"</a>" ))
        link<-reportNam
        link<-as.data.frame(paste0("<a href='", paste0(link,".html"),"' target='_blank'>", link,"</a>"))

        if (savedValue$linksSe[1,1]=='none'){

        }else {
            #linkTT2<<-link
            colnames(link)<-colnames(savedValue$linksSe)
            link<-rbind (savedValue$linksSe,link)
        }

        colnames(link)<-'html_report_link'
        output$htmlReport<- shiny::renderDataTable({
            link
        },escape=FALSE)


    })


    ##pathway

    shiny::observeEvent(input$exploreContact,{

        ##1)cerca la regione di interesse e le sue interazioni con normValue>= threshold

        intera=savedValue$resultsNLsave
        #rownames(intera)=intera[,1]
        region=input$pathway_selectBin
        #intera= HCRread ('', patho)
        #reg1= as.matrix (rownames(intera))
        #colnames(reg1)='reg1'
        #intera= cbind (reg1,intera)

        interaSub1= subset (intera, intera[,1]==region)
        interaSub2= subset (intera, intera[,2]==region)

        interaSubFinal= rbind(interaSub1,interaSub2)

        interaSubFinal= unique(interaSubFinal)
        #interaSubFinalTT<<-interaSubFinal
        #interaSubFinal= subset(interaSubFinal, as.numeric(interaSubFinal[,5])>=input$threshold)
        savedValue$interaSubFinal=interaSubFinal

        #2)cerca le regioni con cui intergisce (questa poi andrà inserito all'interno di un select input della GUI)
        interaSubFinal2= subset(interaSubFinal, interaSubFinal[,1]==region)
        interaReg1=as.array(as.character(interaSubFinal2[,2]))
        #interaReg1TT<<-interaReg1
        interaSubFinal2= subset(interaSubFinal, interaSubFinal[,2]==region)
        interaReg2=as.array(as.character(interaSubFinal2[,1]))
        #interaReg2TT<<-interaReg2
        interaRegFinal=c(interaReg1,interaReg2)
        #interaRegFinalTT<<-interaRegFinal
        #print (interaRegFinal)

        output$pathway_secondRow= shiny::renderUI({

            shiny::fluidRow (

                shiny::column(12,
                        shiny:selectInput(nsServer('pathway_secondRegion'), label=h5('second region'),
                                     choices=interaRegFinal,
                                     selected=1
                        )
                ),
                shiny::column(12,
                    busyIndUI(
                    shiny::actionButton(nsServer('pathFinder'), label=h5('Enrich'),
                                        class = "btn-primary"
                                        )
                ))

            )

        })

    })


    shiny::observeEvent(input$pathFinder,{
        #print ("pathFinder pressed")
        busyIndServer("pathFinder",{
        #print ("pathFinder busyInd pressed")
        intera=savedValue$resultsNLsave
        #interaTT<<-intera
        print ("1")
        #intera= subset(intera, as.numeric(intera[,5])>=input$threshold) #se questo è vuoto rimane muto
        if (length(intera[,1])==0){

            print("no pathway finded")
            #stop("no pathway finded")
            # Warning="no pathway"
            # Message = "finded"

            output$pathway_thirdRow=shiny::renderDataTable ({
                 Wr<-data.frame(Warning="no region finded")
             })

            }
        print ("2")
        reg1Genes <-do.call(paste0,c(as.list(intera[,7])))
        print ("3")
        reg2Genes <-do.call(paste0,c(as.list(intera[,8])))
        print ("4")
        totalGenes<-paste0(reg1Genes,reg2Genes)


        #3)seleziona la seconda regione che ti interessa ed ottieni alla fine una sola interazione
        # region2=input$pathway_secondRegion
        # interaSubFinal=savedValue$interaSubFinal
        # interaSubFinal3=subset(interaSubFinal, interaSubFinal[,2]==region2)
        # interaSubFinal4=subset(interaSubFinal, interaSubFinal[,1]==region2)
        # interaSubFinal5=unique(rbind(interaSubFinal3,interaSubFinal4))
        print ("5")
        orga=input$pathway_organism
        #geneReg1=interaSubFinal5[1,7]
        #geneReg2=interaSubFinal5[1,8]
        print ("6")
        #totalGenesTT<<-totalGenes
        regDivided=unique(c(strsplit(totalGenes,',')[[1]])) #qui anche si può bloccare
        #regDividedTT<<-regDivided

        #subReg2gene=c(strsplit(geneReg2,',')[[1]])
        #regAllGene=c(subReg1gene, subReg2gene)

        print ("7")
        saveG=as.data.frame(gProfileR::gprofiler(regDivided,organism=orga, significant=FALSE))
        #saveGTT<<-saveG
        print ("8")
        saveG=data.frame(saveG[,9],saveG[,10],saveG[,12],saveG[,14],saveG[,3],saveG[,4],saveG[,6])
        print ("9")
        colnames(saveG)=c('term.id','domain','term.name','intersection','p-value','term.size','overlap.size')
        print ("10")
        output$pathway_thirdRow=shiny::renderDataTable ({
            saveG
        })

    })})

    ##expression

    shiny::observeEvent(input$geneReport,{


        reportNam = paste0(input$gene_reportName, '_regions' ,savedValue$tamplateName)
        #View (selectfinal)

        reportTab=savedValue$regiSave


        if (input$expCheck==TRUE){
            regions= savedValue$regiNLsave
            expressionFilePath=input$fileInput_common$datapath
            tsvFinal=HCRread ('',expressionFilePath,  header=TRUE)


            regions<- subset (regions, regions[,5]  %in% rownames(tsvFinal))
            tsvFinal<-subset (tsvFinal,rownames(tsvFinal) %in% regions[,5])

            allregExp=matrix (ncol=1,nrow=length(regions[,1]))


            expressionFilePath=input$fileInput_common$datapath
            tsvFinal=HCRread ('',expressionFilePath,  header=TRUE)
            #tsvTT<<-tsvFinal
            rem1<-strsplit(reportTab[,5],"<a href='http://www.ensembl.org/Multi/Search/Results?q=", fixed=TRUE)
            nam1<-sapply(rem1,"[",2)
            rem2<-strsplit (nam1, "'>", fixed=TRUE)
            nam2<-sapply(rem2,"[",1)
            reportTab[,5]<-nam2
            #rpTT<<-reportTab
            #regTT<<-regions
            reportTab=subset(reportTab, reportTab[,5] %in% regions[,5])

            ##subsetta la tabella dei gruppi utilizzando reportTab
            if (input$groupFileCheck==TRUE){
                groupsPath<-input$groupFile$datapath
                groupTab=HCRread ('',groupsPath,  header=TRUE)
                #grTabTT2<<-groupTab
                #rpTT<<-reportTab
                groupTab=subset(groupTab, rownames(groupTab) %in% reportTab[,5])
                #grTabTT2<<-groupTab
            }

            nam2<-reportTab[,5]
            nam3<- rep (paste0("<a href='http://www.ensembl.org/Multi/Search/Results?q=",nam2,"'>",nam2,"</a>"))
            reportTab[,5]<-nam3
            #rpTT2<<-reportTab

        }

        #creo il report



        htmlRep5 <- ReportingTools::HTMLReport(shortName = reportNam,
                               title = "",
                               reportDirectory = "./www")

        #publish(resulShow, htmlRep5)
        publish(reportTab, htmlRep5)
        finish(htmlRep5)

        file.copy(from='./www', to=pointin(wdPath,'netReports'), recursive=TRUE)
        print (pointin(wdPath,'netReports'))
        print ('report page CREATED')

        # output$htmlReport<- renderUI({
        #   includeHTML('/home/lucio/Dropbox/HiCeekR/HiCeekR_0.9/reports/my_html_test_HCR.html') ##DEVI ASSOLUTAMENTE SOSTITUIRLO CON UN PATH DINAMICO
        # },escape=FALSE)

        #link<-as.data.frame (paste0("<a href=","'", "/home/lucio/Dropbox/HiCeekR/HiCeekR_0.9/reports/my_html_test_HCR.html", "'>", 'report_link',"</a>" ))
        link<-reportNam
        link<-as.data.frame(paste0("<a href='", paste0(link,".html"),"' target='_blank'>", link,"</a>"))

        if (savedValue$linksSe[1,1]=='none'){

        }else {
            #linkTT2<<-link
            colnames(link)<-colnames(savedValue$linksSe)
            link<-rbind (savedValue$linksSe,link)
        }

        colnames(link)<-'html_report_link'
        output$htmlReport<- shiny::renderDataTable({
            link
        },escape=FALSE)

        if (input$expCheck==TRUE){

            #regions= savedValue$regiNLsave
            #regionsTT<<-regions
            #allregExp=matrix (ncol=1,nrow=length(regions[,1]))

            checkState<-matrix (nrow=1, ncol=6)

            colnames(checkState)<-c('expDataCheck1','expDataCheck2','expDataCheck3','expDataCheck4','expDataCheck5','expDataCheck6')

            #checkState[1,]<-c(input$expDataCheck1,input$expDataCheck2,input$expDataCheck3,input$expDataCheck4,input$expDataCheck5,input$expDataCheck6)


            checkState[1,]<-FALSE

            for (i in 1:input$selectDim){

                checkState[1,i]<-TRUE

            }

            trValue<-sum(checkState, na.rm=TRUE) #how many TRUE

            valueCal<-matrix(nrow=2,ncol=trValue)
            if(input$groupFileCheck==TRUE){valueCal<-matrix(nrow=2,ncol=trValue)} else {valueCal<-matrix(nrow=1,ncol=trValue)}
            #colnames(valueCal)=colnames(subset(checkState, checkState[1,]==TRUE))

            che<-as.matrix(as.character(checkState))
            rownames(che)=colnames(checkState)
            che<-subset(che, che[,1]=='TRUE')
            colnames(valueCal)<-rownames(che)

            # expressionFilePath=input$fileInput_common$datapath
            # tsvFinal=HCRread ('',expressionFilePath,  header=TRUE)

            for (i in 1:input$selectDim){

                colnames(valueCal)[i]=colnames(tsvFinal)[i]

            }

            # for (i in 1:length(valueCal[1,])){
            #   if (colnames(valueCal)[i]=='expDataCheck1'){colnames(valueCal)[i]=input$expressionFile$name}
            #   if (colnames(valueCal)[i]=='expDataCheck2'){colnames(valueCal)[i]=input$expressionFile2$name}
            #   if (colnames(valueCal)[i]=='expDataCheck3'){colnames(valueCal)[i]=input$expressionFile3$name}
            #   if (colnames(valueCal)[i]=='expDataCheck4'){colnames(valueCal)[i]=input$expressionFile4$name}
            #   if (colnames(valueCal)[i]=='expDataCheck5'){colnames(valueCal)[i]=input$expressionFile5$name}
            #   if (colnames(valueCal)[i]=='expDataCheck6'){colnames(valueCal)[i]=input$expressionFile6$name}
            # }
            #tsvFinalTT<<-tsvFinal
            ##fai il subset delle regioni nei toPlot
            # regions<- subset (regions, regions[,5]  %in% rownames(tsvFinal) )
            # tsvFinal<-subset (tsvFinal,rownames(tsvFinal) %in% regions[,5])

            ###valueCalTT<<-valueCal
            ###checkStateTT<<-checkState
            #print (paste0('checkState:  ',checkState))
            maxMinTotal<-c()

            # expressionFilePath=input$fileInput_common$datapath
            # tsvFinal=HCRread ('',expressionFilePath,  header=TRUE)

            if (input$selectDim>=1){
                #expressionPath=input$expressionFile$datapath
                #tsvFinal=HCRread ('',expressionFilePath,  header=TRUE)
                toPlot=as.data.frame(tsvFinal[,1])
                #tpTT<<-toPlot
                rownames(toPlot)=rownames(tsvFinal)
                toPlot<-subset(toPlot, rownames(toPlot) %in% regions[,5] )
                #regions<-subset (regions, regions[,5] %in% rownames(toPlot2) )
                #toPlotTTT<<-toPlot
                #toPlot<-subset (toPlot, regions[,5] %in% rownames(toPlot) )

                #maxVal<- apply [toPlot,2,max]
                #maxVal<- apply [toPlot,2,min]  ##http://www.personality-project.org/r/r.commands.html
                maxVal<- as.numeric(max(as.numeric(as.character(toPlot[,1]))))
                minVal<- as.numeric(min(as.numeric(as.character(toPlot[,1]))))
                #maxTT<<-maxVal
                #minTT<<-minVal
                maxMinTotal[1]<-maxVal
                maxMinTotal[2]<-minVal
                print (paste0('maxVal:',maxVal,'minVal',minVal))
                #toPlotTT<<-toPlot



            }
            if (input$selectDim>=2){
                #expressionPath2=input$expressionFile2$datapath
                #tsvFinal=HCRread ('',expressionFilePath,  header=TRUE)
                toPlot2=as.data.frame(tsvFinal[,2])
                rownames(toPlot2)=rownames(tsvFinal)
                toPlot2<-subset(toPlot2, rownames(toPlot2) %in% regions[,5] )
                #toPlot2<-subset (regions, regions[,5] %in% rownames(toPlot2) )
                #maxVal2<- as.numeric(max(toPlot2))
                #minVal2<- as.numeric(min(toPlot2))

                maxVal2<-as.numeric(max(as.numeric(as.character(toPlot2[,1]))))
                minVal2<-as.numeric(min(as.numeric(as.character(toPlot2[,1]))))

                maxMinTotal[3]<-maxVal2
                maxMinTotal[4]<-minVal2
                print (paste0('maxVal2:',maxVal2,'minVal2',minVal2))
                #toPlotTT<<-toPlot
            }
            if (input$selectDim>=3){
                #expressionPath3=input$expressionFile3$datapath
                #tsvFinal=HCRread ('',expressionFilePath,  header=TRUE)
                toPlot3=as.data.frame(tsvFinal[,3])
                rownames(toPlot3)=rownames(tsvFinal)
                toPlot3<-subset (toPlot3, rownames(toPlot3) %in% regions[,5] )
                #maxVal3<- as.numeric(max(toPlot3))
                #minVal3<- as.numeric(min(toPlot3))

                maxVal3<-as.numeric(max(as.numeric(as.character(toPlot3[,1]))))
                minVal3<-as.numeric(min(as.numeric(as.character(toPlot3[,1]))))

                maxMinTotal[5]<-maxVal3
                maxMinTotal[6]<-minVal3
                print (paste0('maxVal3:',maxVal3,'minVal3',minVal3))

            }
            if (input$selectDim>=4){
                #expressionPath4=input$expressionFile4$datapath
                #tsvFinal=HCRread ('',expressionFilePath,  header=TRUE)
                toPlot4=as.data.frame(tsvFinal[,4])
                rownames(toPlot4)=rownames(tsvFinal)
                toPlot4<-subset (toPlot4, rownames(toPlot4) %in% regions[,5] )
                #maxVal4<- as.numeric(max(toPlot4))
                #minVal4<- as.numeric(min(toPlot4))

                maxVal4<-as.numeric(max(as.numeric(as.character(toPlot4[,1]))))
                minVal4<-as.numeric(min(as.numeric(as.character(toPlot4[,1]))))

                maxMinTotal[7]<-maxVal4
                maxMinTotal[8]<-minVal4
                print (paste0('maxVal4:',maxVal4,'minVal4',minVal4))
            }
            if (input$selectDim>=5){
                #expressionPath5=input$expressionFile5$datapath
                #tsvFinal=HCRread ('',expressionFilePath,  header=TRUE)
                toPlot5=as.data.frame(tsvFinal[,5])
                rownames(toPlot5)=rownames(tsvFinal)
                toPlot5<-subset(toPlot5, rownames(toPlot5) %in% regions[,5] )
                #maxVal5<- as.numeric(max(toPlot5))
                #minVal5<- as.numeric(min(toPlot5))

                maxVal5<- as.numeric(max(as.numeric(as.character(toPlot5[,1]))))
                minVal5<- as.numeric(min(as.numeric(as.character(toPlot5[,1]))))

                maxMinTotal[9]<-maxVal5
                maxMinTotal[10]<-minVal5
                print (paste0('maxVal5:',maxVal5,'minVal5',minVal5))
            }
            if (input$selectDim>=6){
                #expressionPath6=input$expressionFile6$datapath
                #tsvFinal=HCRread ('',expressionFilePath,  header=TRUE)
                toPlot6=as.data.frame(tsvFinal[,6])
                rownames(toPlot6)=rownames(tsvFinal)
                toPlot6<-subset (toPlot6, rownames(toPlot6) %in% regions[,5] )
                maxVal6<-as.numeric(max(toPlot6))
                minVal6<-as.numeric(min(toPlot6))
                maxMinTotal[11]<-maxVal6
                maxMinTotal[12]<-minVal6
                print(paste0('maxVal6:',maxVal6,'minVal6',minVal6))
            }

            totalMax<-max(na.omit(maxMinTotal))
            totalMin<-min(na.omit(maxMinTotal))

            print (paste0('totalMax: ',totalMax,'    totalMin: ',totalMin))

            if(input$groupFileCheck==TRUE){

                groupTab<-HCRread('',input$groupFile$datapath)

            }


            #expressionPath=input$expressionFile$datapath

            # tsvFinal=HCRread ('',expressionPath,  header=TRUE)
            # regions= savedValue$regiNLsave
            # regionsTT<<-regions
            # allregExp=matrix (ncol=1,nrow=length(regions[,1]))
            #toPlot=as.data.frame(tsvFinal[,1])
            #rownames(toPlot)=rownames(tsvFinal)
            # toPlot<-subset (toPlot, rownames(toPlot) %in% regions[,5] )
            # maxVal<- as.numeric(max(toPlot))
            # minVal<- as.numeric(min(toPlot))
            # print (paste0('maxVal:',maxVal,'minVal',minVal))
            # toPlotTT<<-toPlot
            imagename <- c()
            ##per la colonna 1 sempre
            for (i in 1:nrow(regions)){



                if (input$selectDim>=1){

                    intGene1<-regions[i,5]
                    ind<-which(rownames(toPlot)==as.character(intGene1))
                    regBarValue1<-as.numeric(as.character((toPlot[ind,1])))
                    #regBarValue1<-as.numeric(toPlot[as.character(intGene1),1])

                    if(input$groupFileCheck==TRUE){
                        grId<-which(rownames(groupTab)==as.character(intGene1))
                        valueCal[2,colnames(tsvFinal)[1]]=groupTab[grId,1]}
                    print(paste0('intGene1:  ',intGene1, '   value: ', regBarValue1 ))

                    #regBarValue1 deve essere inserito nel plot
                    #valueCal[1,1]=input$expressionFile$name
                    #valueCal[2,1]=regBarValue1
                    valueCal[1,colnames(tsvFinal)[1]]=regBarValue1

                    print(paste0('valueCal: ', valueCal[1,colnames(tsvFinal)[1]] ))

                    # if (as.numeric(regions[i,5])>0){
                    #   regBarValueTT  <<- regBarValue1
                    # }

                    #print (paste0 ('regBarValue1;regBarValue2:',regBarValue1,';',regBarValue2))
                    # imagename[i] <- paste0(input$gene_reportName,"_intPlot_", i, ".png")
                    # png(filename = paste0("www/", imagename[i]))

                    # plotAr=regBarValue1
                    # taplot= as.table(plotAr)
                    # taplotTT<<-taplot
                    # rownames(taplot)= input$expressionFile$name


                }

                if (input$selectDim>=2){

                    intGene1<-regions[i,5]
                    ind<- which(rownames(toPlot2)==as.character(intGene1))
                    print(paste0('ind:',ind))
                    #regBarValue1<-as.numeric(toPlot2[as.character(intGene1),1])
                    regBarValue1<-as.numeric(as.character((toPlot2[ind,1])))
                    #regBarValue1<-as.numeric(toPlot2[ind,1])
                    if(input$groupFileCheck==TRUE){
                        grId<-which(rownames(groupTab)==as.character(intGene1))
                        valueCal[2,colnames(tsvFinal)[2]]=groupTab[grId,2]
                    }
                    print(paste0('intGene1:  ',intGene1, '   value: ', regBarValue1 ))
                    print(paste0('intGene1:  ',intGene1, '   value: ', regBarValue1 ))

                    valueCal[1,colnames(tsvFinal)[2]]=regBarValue1
                    print(paste0('valueCal: ', valueCal[1,colnames(tsvFinal)[2]] ))

                }

                if (input$selectDim>=3){

                    intGene1<-regions[i,5]
                    ind<-which(rownames(toPlot3)==as.character(intGene1))
                    #regBarValue1<-as.numeric(toPlot3[ind,1])
                    regBarValue1<-as.numeric(as.character((toPlot3[ind,1])))
                    #regBarValue1<-as.numeric(toPlot3[as.character(intGene1),1])
                    #if(input$groupFileCheck==TRUE){group3<-groupTab[id,3]}
                    if(input$groupFileCheck==TRUE){valueCal[2,colnames(tsvFinal)[3]]=groupTab[id,3]}
                    print(paste0('intGene1:  ',intGene1, '   value: ', regBarValue1 ))

                    if(input$groupFileCheck==TRUE){
                        grId<-which (rownames(groupTab)==as.character(intGene1))
                        valueCal[2,colnames(tsvFinal)[3]]=groupTab[grId,3]
                    }

                    valueCal[1,input$colnames(tsvFinal)[3]]=regBarValue1
                    print (paste0('valueCal: ', valueCal[1,colnames(tsvFinal)[3]] ))

                }

                if (input$selectDim>=4){

                    intGene1<-regions[i,5]
                    ind<- which (rownames(toPlot4)==as.character(intGene1))
                    #regBarValue1<-as.numeric(toPlot4[ind,1])
                    regBarValue1<-as.numeric(as.character((toPlot4[ind,1])))
                    #if(input$groupFileCheck==TRUE){group4<-groupTab[id,4]}
                    if(input$groupFileCheck==TRUE){valueCal[2,colnames(tsvFinal)[4]]=groupTab[id,4]}
                    #regBarValue1<-as.numeric(toPlot4[as.character(intGene1),1])
                    print(paste0('intGene1:  ',intGene1, '   value: ', regBarValue1 ))

                    if(input$groupFileCheck==TRUE){
                        grId<-which (rownames(groupTab)==as.character(intGene1))
                        valueCal[2,colnames(tsvFinal)[4]]=groupTab[grId,4]
                    }

                    valueCal[1,colnames(tsvFinal)[4]]=regBarValue1
                    print (paste0('valueCal: ', valueCal[1,colnames(tsvFinal)[4]] ))

                }

                if (input$selectDim>=5){

                    intGene1<-regions[i,5]
                    ind<- which(rownames(toPlot5)==as.character(intGene1))
                    #regBarValue1<-as.numeric(toPlot5[ind,1])
                    regBarValue1<-as.numeric(as.character((toPlot5[ind,1])))
                    #regBarValue1<-as.numeric(toPlot5[as.character(intGene1),1])
                    #if(input$groupFileCheck==TRUE){group5<-groupTab[id,5]}
                    if(input$groupFileCheck==TRUE){valueCal[2,colnames(tsvFinal)[5]]=groupTab[id,5]}
                    print (paste0('intGene1:  ',intGene1, '   value: ', regBarValue1 ))

                    if(input$groupFileCheck==TRUE){
                        grId<-which (rownames(groupTab)==as.character(intGene1))
                        valueCal[2,colnames(tsvFinal)[5]]=groupTab[grId,5]
                    }

                    valueCal[1,colnames(tsvFinal)[5]]=regBarValue1
                    print(paste0('valueCal: ', valueCal[1,colnames(tsvFinal)[5]] ))

                }

                if (input$selectDim>=6){

                    intGene1<-regions[i,5]
                    ind<- which(rownames(toPlot6)==as.character(intGene1))
                    #regBarValue1<-as.numeric(toPlot6[ind,1])
                    regBarValue1<-as.numeric(as.character((toPlot6[ind,1])))
                    #regBarValue1<-as.numeric(toPlot6[as.character(intGene1),1])
                    #if(input$groupFileCheck==TRUE){group6<-groupTab[id,6]}
                    if(input$groupFileCheck==TRUE){valueCal[2,colnames(tsvFinal)[6]]=groupTab[id,6]}
                    print (paste0('intGene1:  ',intGene1, '   value: ', regBarValue1 ))

                    if(input$groupFileCheck==TRUE){
                        grId<-which(rownames(groupTab)==as.character(intGene1))
                        valueCal[2,colnames(tsvFinal)[6]]=groupTab[grId,6]
                    }

                    valueCal[1,colnames(tsvFinal)[6]]=regBarValue1
                    print(paste0('valueCal: ', valueCal[1,colnames(tsvFinal)[6]] ))

                }

                #valueCalArrayTT<<- as.array(valueCal)
                #valCalTT<<-valueCal
                Names<-as.array(colnames(valueCal))
                #NamesTT<<-Names
                Values<-as.array(as.numeric(valueCal[1,]))
                #ValuesTT<<- Values

                if (input$groupFileCheck==TRUE){
                    groups<-as.array(valueCal[2,])
                }

                plotDf<-data.frame (Names,Values)
                if(input$groupFileCheck==TRUE){
                    #Group<-
                }
                #              tabFrame<-

                imagename[i] <- paste0(input$gene_reportName,"_intPlot_", i, ".png")


                #plotAr=regBarValue1
                #taplot= as.table(valueCal)
                #taplotTT<<-taplot
                #rownames(taplot)=colnames(valueCal)
                #taplotTT<<-taplot
                #rownames(taplot)= input$fileInput_common$name

                #   png(filename = paste0("www/", imagename[i]))
                #    barplot(taplot, main=paste0(colnames(toPlot)[1],'_',intGene1),
                #            xlab='', col=c("lawngreen","green1","green3","green4","forestgreen","darkgreen"),
                #            ylim= c(totalMin,totalMax),
                #            beside=TRUE)
                #    dev.off()

                if (input$groupFileCheck==FALSE){
                    ggplot2::ggplot (data=plotDf, aes(x=Names, y=Values)) +
                        ggplot2::geom_bar (stat="identity",color="darkblue", fill="cornflowerblue") +
                        ggplot2::scale_y_continuous(limits=c(totalMin,totalMax)) +
                        ggplot2::ggsave( paste0("www/", imagename[i]))
                } else {
                    ggplot2::ggplot (data=plotDf, aes(x=Names, y=Values, fill=groups)) +
                        ggplot2::geom_bar (stat="identity") +
                        ggplot2::scale_y_continuous(limits=c(totalMin,totalMax)) +
                        ggplot2::ggsave( paste0("www/", imagename[i]))
                }



                # dev.off()

            }

            #imagenameTT<<-imagename


            reportTab$Image <- hwriter::hwriteImage(imagename, link = imagename, table = FALSE, width=100, height=100)



            #-------------------------------------------------------------------------------------------------

        }

        htmlRep5 <- ReportingTools::HTMLReport(shortName = reportNam,
                               title = "",
                               reportDirectory = "./www")

        #publish(resulShow, htmlRep5)
        ReportingTools::publish(reportTab, htmlRep5)
        finish(htmlRep5)

        file.copy(from='./www', to=pointin(wdPath,'netReports'), recursive=TRUE)
        print (pointin(wdPath,'netReports'))
        print ('report page CREATED')

        # output$htmlReport<- renderUI({
        #   includeHTML('/home/lucio/Dropbox/HiCeekR/HiCeekR_0.9/reports/my_html_test_HCR.html') ##DEVI ASSOLUTAMENTE SOSTITUIRLO CON UN PATH DINAMICO
        # },escape=FALSE)

        #link<-as.data.frame (paste0("<a href=","'", "/home/lucio/Dropbox/HiCeekR/HiCeekR_0.9/reports/my_html_test_HCR.html", "'>", 'report_link',"</a>" ))
        link<-reportNam
        link<-as.data.frame(paste0("<a href='", paste0(link,".html"),"' target='_blank'>", link,"</a>"))

        if (savedValue$linksSe[1,1]=='none'){

        }else {
            #linkTT2<<-link
            colnames(link)<-colnames(savedValue$linksSe)
            link<-rbind(savedValue$linksSe,link)
        }

        colnames(link)<-'html_report_link'
        output$htmlReport<- renderDataTable({
            link
        },escape=FALSE)


    })

    shiny::observeEvent(input$expCheck,{



        if (input$expCheck==TRUE){

            output$expOptSlot<-shiny::renderUI({

                shiny::fluidRow(
                    shiny::column(12,

                        shiny::fluidRow(column(12),
                                        shiny::wellPanel('select Expression table',

                                        shiny::fluidRow (
                                            shiny::column(12,
                                                shiny::fileInput (nsServer('fileInput_common'),
                                                                label='file')))

                                                                # ,fluidRow (selectInput(nsServer("selectDim"), label = h5("Select number of columns"),
                                                                #                       choices = c( 1, 2,  3, 4, 5, 6),
                                                                #                       selected = 1))
                                                                #

                                                                ,shiny::fluidRow (
                                                                    shiny::column(12,
                                                                        shiny::uiOutput(nsServer('dimSlot'))))
                                   ))

                                   #                    ,fluidRow(column(12
                                   #
                                   #                                     wellPanel (
                                   #
                                   #   fluidRow (   #primo expression file
                                   #
                                   #     column (4,
                                   #             checkboxInput(nsServer('expDataCheck1'), label='activate')
                                   #             ),
                                   #     column (8, fileInput(nsServer("expressionFile"), label = h5("expression table")))
                                   #   ),
                                   #
                                   #   fluidRow (   #secondo expression file
                                   #
                                   #     column (4,checkboxInput(nsServer('expDataCheck2'), label='activate')),
                                   #     column (8, fileInput(nsServer("expressionFile2"), label = h5("expression table")))
                                   #
                                   #     ),
                                   #
                                   #   fluidRow (   #terzo expression file
                                   #
                                   #     column (4,checkboxInput(nsServer('expDataCheck3'), label='activate')),
                                   #     column (8, fileInput(nsServer("expressionFile3"), label = h5("expression table")))
                                   #
                                   #   ),
                                   #
                                   #   fluidRow (   #quarto expression file
                                   #
                                   #     column (4,checkboxInput(nsServer('expDataCheck4'), label='activate')),
                                   #     column (8, fileInput(nsServer("expressionFile4"), label = h5("expression table")))
                                   #
                                   #   ),
                                   #
                                   #   fluidRow (   #quinto expression file
                                   #
                                   #     column (4,checkboxInput(nsServer('expDataCheck5'), label='activate')),
                                   #     column (8, fileInput(nsServer("expressionFile5"), label = h5("expression table")))
                                   #
                                   #   ),
                                   #
                                   #   fluidRow (   #sesto expression file
                                   #
                                   #     column (4,checkboxInput(nsServer('expDataCheck6'), label='activate')),
                                   #     column (8, fileInput(nsServer("expressionFile6"), label = h5("expression table")))
                                   #
                                   #   )
                                   #
                                   # )
                                   #))
                ))

            })

        } else {

            output$expOptSlot<- shiny::renderUI ({})

        }

    })

    shiny::observeEvent(input$fileInput_common,{

        input$fileInput_common$datapath
        tab<-HCRread(file='', path=input$fileInput_common$datapath)

        output$dimSlot<- shiny::renderUI ({

            shiny::fluidRow(
                shiny::column(12,
                    shiny::fluidRow (
                        shiny::column (8,
                            shiny::selectInput(nsServer("selectDim"),
                                               label = h5("Select number of columns"),
                                                               choices = if (length(tab[1,])<=6){c( 1:length(tab[1,]))}else{c(1:6)},
                                                               selected = 1)),

                                        shiny::column (4,
                                            shiny::checkboxInput(nsServer('groupFileCheck'),
                                                                 label=h5('group file'),
                                                                 value=FALSE)
                                         )
            ),

            shiny::fluidRow(
                shiny::column(12,

                            #conditionalPanel (condition='output.groupFileCheck==false',
                            #                   fileInput(nsServer('selectGroupFile'), label=h5('select group file'))
                            #                   )
                    shiny::uiOutput(nsServer('selectGroupFileSlot'))
            )
            )


            )
            )

        })

    })

    shiny::observeEvent(input$groupFileCheck,{

        if (input$groupFileCheck==TRUE){
            output$selectGroupFileSlot<-shiny::renderUI({
                shiny::fileInput(nsServer('groupFile'), label=h5("file of groups"))
            })
        } else {
            output$selectGroupFileSlot<-shiny::renderUI({})
        }

    })



}
