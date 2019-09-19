
#' HiCsegTADs_postProcessing_UI
#'
#' @param id
#' @param label
#'
#' @return
#' @keywords internal
#'
#' @examples
HiCsegTADs_postProcessing_UI <- function (id, label='HiCseg' ){
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
                                    shiny::fluidRow( br(),
                                        shiny::uiOutput(pcaNS('startPcaSlot'))
                                    )

                 )
             ),

                                     shiny::column (10, shiny::br(), shiny::br(),
                                                    shiny::dataTableOutput(pcaNS("results"))
                                                    #,shiny::uiOsuntrackExportutput(pcaNS('startPcaSlot'))
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

#' HiCsegTADs_postProcessing_Server
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
HiCsegTADs_postProcessing_Server <- function (input, output, session, stringsAsFactors,
                                       wdPath
){
    print ("start HiCsegTADs module")
    #plusval<-0
    nSpace<- session$ns
    #require ('Rsamtools')

    ##reactiveValue

    Reac<- shiny::reactiveValues(tP='none', S_pcaMod='none', S_exportThis='none') #non c'è bisogno che definisci prima la variabile reactiveValues() e poi definisci dopo

    print ("parameters render")
    output$paramSlot<- shiny::renderUI ({

        shiny::fluidRow(
            shiny::fluidRow(
                shiny::column(12, shiny::br(),
                    selectFile (nSpace('matrix'),
                                path=pointin(wdPath, 'Normalization') ,
                                'Contact Matrix', subset=TRUE, pattern='.tsv')
                    #shiny::fileInput(pcaNServer('pcaTableLoaded'), label="pca file")
            )
),
            shiny::fluidRow(
                shiny::column(6,
                              shiny::selectInput(nSpace("distrib"),
                                                 label="distribution",
                                                 choices=c("G","B","P"),
                                                 selected="G"
                              )

                ),
                shiny::column(6,
                              shiny::selectInput(nSpace("model"),
                                                 label="model",
                                                 choices=c("D","Dplus"),
                                                 selected="D"
                              )

                )
            ),
            shiny::fluidRow(

                shiny::column(6, br(),
                            shiny::numericInput(nSpace("kmax"),
                                                label="Kmax",
                                                value=10
                                                    )
                            ),

                shiny::column(6,
                              shiny::uiOutput(nSpace('chromoSelectorSlot'))
                )

            )

        )


    })

    observeEvent(input$matrix,{
        #print("observe matrix")
        #paste0(pointin(wdPath,'Normalization'),input$pcaTableLoaded)
        if (file.exists(paste0(pointin(wdPath,'Normalization'),input$matrix))==TRUE){
            print("detect chromosomes")
            chromos<-detectChromos(paste0(pointin(wdPath,'Normalization'),
                                          input$matrix))
            #if(length(chromos)>1){
            output$chromoSelectorSlot<-shiny::renderUI({
                shiny::fluidRow(
                    shiny::column(12,
                        shiny::fluidRow(
                            shiny::column(12,
                                        shiny::selectInput(nSpace('chromoSelector'),
                                                        label="chromo of interest",
                                                        choices = chromos,
                                                        selected=chromos[1]
                                          )
                                          )
                        )

                                  )
                )

            })

            output$startPcaSlot<-shiny::renderUI({
                shiny::actionButton (nSpace('start'),
                                     'start')
            })
            #}

        }
    })

    print ("start observer")

    shiny::observeEvent(input$start,{

        print ('.....start HiCsegTADs searching.....')

        #require(HiTC)

        intdata_path<-paste0 (pointin(wdPath,'Normalization'),input$matrix)

        outpath<-pointin(wdPath,'Downstream')

        window=input$windowSize

        print(paste0("matrix Path: ",intdata_path))
        print(paste0("outpath: ",outpath))
        print(paste0("window: ",window))
        print(paste0("distrib: ",input$distrib))
        print(paste0("model: ",input$model))
        print(paste0("Kmax: ",input$kmax))

        intdata<-read.table(intdata_path,sep="\t",header=TRUE,check.names = FALSE)
        rownames(intdata)<-intdata$index
        intdata<-intdata[,-1]
        intdata<-Matrix::as.matrix(intdata)
        colnames(intdata)<-rownames(intdata) #solo se simmetriche ordinate
        #intdataTT<<-intdata

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
        print(paste0("chromosomes:" , unique(chr)))
        if (length(unique(chr))>1){
            print("extract only one chr")

            intdata<-intdata[which(chr==input$chromoSelector),which(chr==input$chromoSelector)]
            #chr<-chr[which(chr==input$chromoSelector)]
            #start<-start[which(chr==input$chromoSelector)]
            #end<-end[which(chr==input$chromoSelector)]
            HCRwrite (intdata, file="tmpSubMatrix", path=pointin(wdPath,'Normalization'), row.names=TRUE)
            intdata_path=paste0(pointin(wdPath,'Normalization'),"tmpSubMatrix.tsv")
        }

        rm("intdata")

        print(paste0("start Fun HiCsegTADs on ",intdata_path))
        #bed<-TopDomTADs(matrixPath=intdata_path, resultsPath=outpath,
        #                window.size=window)

        resu<-HiCsegTADs(matrixPath=intdata_path,
                         outpath = outpath,
                         distrib=input$distrib ,
                         model=input$model,
                         Kmax=input$kmax)

        #resuTT<<-resu

        limits<-read.table(paste0(outpath,"/HiCsegTADs.bed"),sep="\t",header=FALSE)
        j<-resu$J
        limits<-cbind(limits,j)
        #

        #
        #
         #export visualization track


        #
         allreg<-read.table(file=paste0(pointin(wdPath,'Binning'),"allRegions.bint.bed"),
                            sep="\t"
                            )
        #
        # allregTT<<-allreg
        #
         onechrReg<-subset(allreg,allreg$V1==unique(limits$V1))
         trackExport<-onechrReg[,c(1:3)]
         trackExport<-cbind(trackExport,rep(0,length(trackExport[,1])))
        for( j in 1:length(limits$V2)){
            bondend<-which(trackExport$V3==limits$V3[j])
            bondstart<-which(trackExport$V2==limits$V2[j])
            trackExport$`rep(0, length(trackExport[, 1]))`[bondstart:bondend]<-limits[j,4]
        }
        #
         trackExport<-cbind(rep(paste0(trackExport$V1,":",trackExport$V2,"-",trackExport$V3)),trackExport$`rep(0, length(trackExport[, 1]))`)


         colnames(trackExport)<-c("bin","HiCsegJ")

         #trackExportTT<<-trackExport
         subtrackExport<-subset(trackExport, trackExport[,2]!=0)


         output$results<-shiny::renderDataTable ({
             subtrackExport
         })

         write.table(trackExport,file=paste0(pointin(wdPath,'Downstream'),
                                             unique(chr),"_HiCsegTADs.tsv"),
                     sep="\t",row.names = FALSE, col.names = TRUE, quote=FALSE)
        #
        # #allreg<-subset(allreg,allreg$V1==unique(bed$chrom))
        # #allreg<-read.table("/media/lucio/data/1.Bioinformatic/1.heavyWD/1.R/test4/HiCeekRwd/Projects/Grubert2015/test1/Results/Binning/allRegions.bint.bed",sep="\t")
        #
        # #findbins<-rep(paste0(bed$chrom,bed$chromStart,bed$End))
        # #which(allreg$V3==bed$chromEnd)
        #
        # #track<-data.frame(bin,boundaries)
        #

    })


}
