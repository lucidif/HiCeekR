#' hmmDI
#'
#' @param fileOutName
#' @param outputPath
#' @param refFragsPATH
#' @param h5PATH
#' @param bintablePath
#' @param bin.size
#'
#' @return
#' @keywords internal
#'
#' @examples
hmmDI<- function (fileOutName, outputPath, refFragsPATH, h5PATH, bintablePath, bin.size){
    rFrags<-read.table (refFragsPATH, sep='\t', header=TRUE)
    binTab<-read.table (bintablePath, sep='\t', header=FALSE)
    rFragsGrange<-GenomicRanges::makeGRangesFromDataFrame(rFrags)
    print("star pairparam")
    paramFil <- diffHic::pairParam(rFragsGrange)
    print("end pairparam")

    #h5PATH<<-h5PATH
    #paramFil<<-paramFil
    #bin.size<<-bin.size
    finder <- diffHic::domainDirections(h5PATH, paramFil, width=bin.size, span=10)

    print("finded domains")

    all.counts <- cbind(SummarizedExperiment::assay(finder, "up"),
                        SummarizedExperiment::assay(finder, "down"))
    DI<-matrix(ncol=2,nrow=length(all.counts[,1]))
    DI[,1]<-c(1:length(all.counts[,1]))
    colnames(DI)<-c('bin','value')

    for (i in 1:length(DI[,1])){
        #print (paste0('i:',i,'/',length(DI[,1])))
        a<-all.counts[i,1]
        b<-all.counts[i,2]
        e<-(a+b)/2
        DI[i,2]<-((b-a)/abs(b-a))*((((a-e)^2)/e)+ (((b-e)^2)/e)  )
    }

    binNames<-matrix (ncol=1,nrow=length(binTab[,1]))
    binNames[,1]<-rep (paste0(binTab[,1],':',binTab[,2],'-',binTab[,3]))



    DI[,1]<-binNames[,1]


    print("wrote HMM results")
    HCRwrite (DI, fileOutName, path=outputPath , quote=FALSE, col.names=TRUE, row.names=TRUE)

    return(DI)

}

