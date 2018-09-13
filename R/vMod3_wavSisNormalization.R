##chromoR normalization

# Module UI function
#' chromoR_Normalization_UI
#'
#' @param id
#' @param label
#'
#' @return
#' @keywords internal
#'
#' @examples
chromoR_Normalization_UI <- function(id, label = "Normalization") {
    chromoNormNs <- shiny::NS(id)

    shiny::fluidPage(

        shiny::tabsetPanel ( id=chromoNormNs('chromoMainTabpanel'),

                      #tabPanel ('chromoR'

                      #),

                      shiny::tabPanel('correct',

                                shiny::fluidRow(
                                    shiny::column (8
                                        ,shiny::uiOutput(
                                            chromoNormNs("chrIntSlot")
                                        )
                                    # ,selectFile (chromoNormNs('matrix'),
                                    #                     path=
                                    #                     pointin(wdPath,
                                    #                             'Binning'),
                                    #                     label='raw matrix',
                                    #                     subset=TRUE,
                                    #                     pattern='_raw_matrix')
                                ),
                                shiny::column (4,
                                        shiny::wellPanel(
                                            shiny::helpText('work only on one
                                                    chromosome simmetric
                                                            matrices'))
                                )
                                ),
                                # fluidRow(column (12,
                                #                  selectFile (chromoNormNs('name'), path=pointin(wdPath,'Binning'), label='regions', subset=TRUE, pattern='.bint.bed')
                                # )),

                                shiny::fluidRow(
                                    # column(3,#fileInput (chromoNormNs('matrix'),label=h5('matrix'))
                                    #           selectFile (chromoNormNs('matrix'), path=pointin(wdPath,'Binning'),label='raw matrix', subset=TRUE, pattern='_raw_matrix')
                                    #         ),
                                    # column(3,#fileInput(chromoNormNs('name'),label=h5('name'))
                                    #          selectFile (chromoNormNs('name'), path=pointin(wdPath,'Binning'), label='regions', subset=TRUE, pattern='_regions')
                                    #         ),
                                    shiny::column(3,
                                        shiny::br(), shiny::br(),
                                           shiny::checkboxInput(
                                               chromoNormNs("removeUncovered"),
                                               label = "remove uncovered",
                                               value = FALSE)
                                    )
                                    , shiny::column(3,
                                            shiny::br(),
                                            shiny::br(),
                                            shiny::actionButton(
                                                chromoNormNs('startBut'),
                                                label=h5('start'))
                                    )

                                    , shiny::column(6,
                                            shiny::uiOutput(chromoNormNs
                                                             ('completeMessSlot'
                                                                 ))
                                    )
                                ),

                                shiny::fluidRow(
                                    shiny::column(6,
                                        shiny::uiOutput (chromoNormNs('newName')))
                                    ,shiny::column (4,
                                        shiny::uiOutput(chromoNormNs('save')))
                                )

                      )

        )

    )

}

#===============================================================================


# Module server function

#' chromoR_Normalization_Server
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
chromoR_Normalization_Server <- function(input, output, session,
                                         stringsAsFactors, wdPath) {


    chromoNormNs<-session$ns

    rVal<-shiny::reactiveValues()
    load(file=paste0(pointin(wdPath, "Binning", sys=TRUE), "intSet.Robj"))
    rVal$intSet<-intSet
    rm(intSet)



    #print (regions(red))
    rVal$chrList<- as.data.frame(unique(
        GenomicRanges::seqnames(
            InteractionSet::regions(rVal$intSet))))
    colnames(rVal$chrList)<-'chromosomes'
    #rVal$chrList<-c('all',rVal$chrList)

    output$chrIntSlot<-shiny::renderUI({
        shiny::selectInput(chromoNormNs("chrInt"),
                            label = "chromosome of interest",
                            choices = rVal$chrList
                            )
    })

    shiny::observeEvent(input$startBut,{

        ##puoi fare un controllo sulla matrice che carichi se rownames e
        ##colnames non sono uguali e se non è presente un solo cromosoma
        ##ti da un messaggio di errore

        #devi farlo partire dall'Robj che contiene lo squareCounts presente nel
        #sysPath


        #m1<- paste0(pointin(wdPath,'Binning') ,input$matrix)

        #seg1<- paste0 ( pointin (wdPath,'Binning'),  input$name )
        #m2<- read.table (m1, sep='\t', header=TRUE)
            #m2<- readContactMatrix(m1)
        #View (head(m2))
        #inSetTT<<-rVal$intSet
        #chrTT<<-input$chrInt

        m2<- as.brute.matrix(rVal$intSet, first=input$chrInt ,
                            second=input$chrInt ,
                            #fill=saveMe$correction$truth,
                            alternativeFill=FALSE)


        #m3<-as.matrix (m2[,1])
        #View (head(m3))
        #colnames (m3)<-'index'

        #View (m3)
        #m2<- m2[,-1]
        #View (head(m2))
        m2<- as.matrix(m2)
        print('m2.....ok')
        #outputName<- paste0(input$chrInt1,"_WavSis_Norm.tsv") #gsub('raw_matrix.tsv', "", input$matrix)
        #outName<-paste0(outputName,'WavSisNormMatrix')
        outName<-paste0(input$chrInt,"_WavSis_Norm.tsv")
        print(length(m2[,1]))
        #colnames(m2)<- row.names(m2)
        #seg2<- read.table (seg1, sep='\t', header=TRUE)
        seg2<-rownames(m2)
        #seg2TT<<-seg2
        split1<-strsplit(seg2,':', fixed=TRUE)
        seg3<-matrix(nrow=length(seg2), ncol=3)
        for (xy in 1:length(seg2)){
            seg3[xy,1]<-split1[[xy]][1]
            split2<-strsplit(split1[[xy]][2],'-',fixed=TRUE)
            seg3[xy,2]<-split2[[1]][1]
            seg3[xy,3]<-split2[[1]][2]
        }

        colnames(seg3)<-c('chr','start','end')
        seg2<-as.data.frame(seg3)

        #seg2TT<<-seg2
        #m2TT<<-m2
        #View (head(seg2))
        print ('seg1.....ok')
        #seg2$ranges.width<-NULL
        #print (length(seg2))
        remUnc<-input$removeUncovered
        m2[is.na(m2)] = 0
        #m2TT<<-m2
        #seg2TT<<-seg2
        #remUncTT<<-remUnc

        # ho un problema con correctCIM che utilizza la funzione wd di wavethresh che però nello script non
        #sarà stato nello script con ::
        # i pacchatti caricati da chromoR sono 3 haarfisz wavethresh MASS
        library(haarfisz)
        norMa<-chromoR::correctCIM(m2, seg2, removeUncovered = remUnc)


        #View(head(norMa))
        #colnames(norMa[[1]])<-m3[,1]
        #new <- cbind (m3,norMa[[1]])
        new<-norMa[[1]]
        #View (new)
        #write.table(norMa[[1]], file=paste0(pointin(wdPath,'Normalization'),outName), sep='\t')
        #HCRwrite (norMa[[1]], file=outName, path=pointin(wdPath,'Normalization'), row.names=TRUE, col.names=TRUE)
        colnames(new)<-colnames(m2)
        rownames(new)<-rownames(m2)

        HCRwrite(new, file=outName,
                    path=pointin(wdPath,'Normalization'),
                    row.names=TRUE,
                    col.names=TRUE)
        print ('correctCIM.....ok')

        output$completeMessSlot<-shiny::renderUI ({
            shiny::wellPanel(
                shiny::helpText('normalized matrix saved in
                                "Normalization" folder')
            )
        })

        #removeUncovered
        filteringTable<-matrix(ncol=1, nrow=2)
        rownames(filteringTable)<-c("Normalization", "remove.Uncovered")
        filteringTable[,1]<-c("WavSiS", input$removeUncovered)
        write.table(filteringTable, paste0(pointin(wdPath, "Normalization", sys=TRUE),"parameter.tsv"),
                    sep="\t", quote=FALSE, col.names=FALSE)

    })


}
