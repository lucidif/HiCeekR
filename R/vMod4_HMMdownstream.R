
#' HMM_postProcessing_UI
#'
#' @param id
#' @param label
#'
#' @return
#' @keywords internal
#'
#' @examples
HMM_postProcessing_UI <- function(id, label = "TADs") {
    hmmNs <- shiny::NS(id)
    shiny::fluidPage(

        shiny::fluidRow(

            shiny::column (12,

                           shiny::wellPanel (

                               shiny::fluidRow (shiny::column(12

                                    # ,selectFile (hmmNs('fragInput'), path=pointin(wdPath,'Pre-Processing'), label='select cutGenome file'
                                    #                  , subset=TRUE, pattern='.cutGenome'
                                    #      )

                        )
                        ),

                        shiny::fluidRow(shiny::column(12
                                        # ,selectFile (hmmNs('h5Input'), path=pointin(wdPath,'Pre-Processing'), label='select h5 file'
                                        #             , subset=TRUE, pattern='.h5'
                                        # )
                        )),

                        shiny::fluidRow(shiny::column (12
                                         # ,selectFile (hmmNs('binTabInput'), path=pointin(wdPath,'Binning'), label='al regions bins table'
                                         #             , subset=TRUE, pattern='.bint.bed'
                                         # )

                        )),

                        shiny::fluidRow(

                            shiny::column (12,

                                    shiny::uiOutput(hmmNs('wellPanelSlot'))

                                    # wellPanel (
                                    #   fluidRow (column (12))
                                    # )
                            )

                        )



                    )

            )

        )

    )

}

#==================================================================================


# Module server function

#' HMM_postProcessing_Server
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
HMM_postProcessing_Server <- function(input, output, session, stringsAsFactors,
                                        wdPath) {
    hmmNs<-session$ns

    output$wellPanelSlot<-shiny::renderUI({
        shiny::wellPanel(

            shiny::fluidRow(

                shiny::column(12,
                    #shiny::textInput(hmmNs('fileName'),label=h5('select file name')))
                        selectFile(hmmNs('filePath'),
                                #path=pointin (wdPath, 'Binning') ,
                                path=pointin (wdPath, 'Normalization') ,
                                label='Contact Matrix',
                                subset=TRUE,
                                pattern='.tsv'
                                #pattern='_raw_matrix.tsv'
                                ))

            ),

            shiny::fluidRow(

                shiny::column (4,
                    shiny::actionButton(hmmNs('startBut'), label=h5('find TADs'))
                ),

                shiny::column (8,
                    shiny::helpText('create the file that contains the directional indexes')
                )

            ),

            shiny::fluidRow (
                shiny::column(12,
                    shiny::textOutput(hmmNs('report'))))

        )
    })

    shiny::observeEvent(input$startBut,{


        #=======================================New Pipe
        require(HiTC)

        #intdata_path<-paste0 (pointin(wdPath,'Binning'),input$filePath)
        intdata_path<-paste0 (pointin(wdPath,'Normalization'),input$filePath)

        outpath<-pointin(wdPath,'Downstream')

        print(paste0("matrix Path: ",intdata_path))
        print(paste0("outpath: ",outpath))

        intdata<-read.table(intdata_path,sep="\t",header=TRUE,check.names = FALSE)
        rownames(intdata)<-intdata$index
        intdata<-intdata[,-1]
        intdata<-Matrix::as.matrix(intdata)


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

        bnnam<-rep(paste0("bn",1:length(intdata[,1])))
        originalNames<-rownames(intdata)

        rownames(intdata)<-bnnam
        colnames(intdata)<-bnnam

        grrw<-gr
        names(grrw)<-bnnam
        grcl<-gr
        names(grcl)<-bnnam

        intdata<-Matrix::Matrix(intdata)
        htcxp<-new("HTCexp",intdata,xgi=grcl,ygi=grcl)
        intChr<-unique(seqnames(grcl))[1]

        hox <- extractRegion(htcxp, chr=intChr,from=1, to=end(grcl)[length(end(grcl))])
        di<-directionalityIndex(hox)
        di<-as.array(di)
        row.names(di)<-originalNames
        di<-as.matrix(di)
        bin<-"di"
        di<-rbind(bin,di)
        write.table(di,paste0(outpath,"/",intChr,"_hmm.tsv"),sep="\t",quote=FALSE,col.names = FALSE, row.names =TRUE)

        predictedTADs<-predictTADs(paste0(outpath,"/",intChr,"_hmm.tsv"),saveBed = paste0(outpath,"/",intChr,"_predTADs.bed"))

        #write.table(di,paste0(outpath,"/hmm.tsv"),sep="\t",quote=FALSE,col.names = FALSE, row.names =TRUE)

        #========================================End new pipe

        #=========================================old pipe
        # filne<-paste0(input$fileName,'.DI')
        # filpath<-pointin (wdPath,'Downstream')
        # refpath<-paste0(pointin(wdPath,'Pre-Processing', sys=TRUE),"refGenFrag.cutGenome.tsv") #cutgenome
        # h5path<- paste0(pointin(wdPath,'Filtering', sys=TRUE),"trimmed.h5")  #h5
        # #non crei il bint.bed da nessuna parte, ma ti serve
        # bintabpath<- paste0(pointin(wdPath,'Binning', sys=TRUE),"allRegions.bint.bed") #bint.bed
        #
        #
        #
        # #binsize<- as.numeric(HCRread('',paste0(wdPath, 'info.tsv'))[1])
        # binsize<-HCRread(file='info.tsv', path=wdPath, header=FALSE)[2,2]
        #
        # resu<-hmmDI(fileOutName=filne, outputPath=filpath, refFragsPATH=refpath, h5PATH=h5path, bintablePath=bintabpath, bin.size=binsize)
        # print ("hmm indices finded")
        # #reportTxt<-paste0(filepath,filne)
        # output$fileRep<- renderText({
        #     paste0(filpath,filne)
        # })
        #=========================================end pipe

        #============================================old render
        output$wellPanelSlot<-shiny::renderUI ({
            shiny::wellPanel(

                shiny::fluidRow(

                    # shiny::column (4,
                    #     shiny::actionButton(hmmNs('newfile'), label=h5('find new TADs file'))
                    # ),

                    shiny::column (8,
                        shiny::helpText('restart finding with new data')
                    )

                ),

                shiny::br(), shiny::br(),

                shiny::fluidRow(

                    shiny::column(8,
                        shiny::textOutput(hmmNs('fileRep'))
                    )

                ),

                shiny::fluidRow (

                    shiny::column (3,
                        shiny::helpText('   was created'))

                )

            )
        })
        #======================================end old render

    })

    shiny::observeEvent (input$newfile, {

        output$wellPanelSlot<-shiny::renderUI ({
            shiny::wellPanel(

                shiny::fluidRow(

                    shiny::column (12,
                        shiny::textInput(hmmNs('fileName'),
                                        label=h5('select file name')))

                ),

                shiny::fluidRow(

                    shiny::column (4,
                                   shiny::actionButton(hmmNs('startBut'), label=h5('find TADs'))
                    ),

                    shiny::column (8,
                        shiny::helpText('create file with DI')
                    )

                ),

                shiny::fluidRow (
                    shiny::column(12,
                        shiny::textOutput(hmmNs('report'))))

            )
        })

    } )

}
