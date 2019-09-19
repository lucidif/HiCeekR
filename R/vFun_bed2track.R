#' Title
#'
#' @param filename
#' @param bedpath
#' @param bincoord
#' @param outpath
#'
#' @return
#' @keywords internal
#'
#' @examples
bed2track<-function(filename,bedpath,bincoord,outpath){
    bedreg<-read.table (bedpath, sep='\t', header=FALSE)
    debreg<-bedreg[1:3]
    colnames(bedreg)<-c("chr", "start" , "end")
    rangeBed<-GenomicRanges::makeGRangesFromDataFrame(as.data.frame(bedreg))

    rFrags<-read.table (bincoord, sep='\t', header=FALSE)
    rFrags<-rFrags[,1:3]
    colnames(rFrags)<-c("chr", "start", "end")

    rFragsGrange<-GenomicRanges::makeGRangesFromDataFrame(rFrags)

    finder<-GenomicRanges::findOverlaps(rFragsGrange, rangeBed)
    finded<-finder@from

    rg<-as.data.frame(rFragsGrange@ranges)
    binNames<-rep(paste0(rFragsGrange@seqnames,":",rg$start,"-",rg$end))
    track<-data.frame(binNames,rep(0,length(binNames)))
    colnames(track)<-c("bin",filename)

    track[unique(finded),2]<-1

    if(length(unique(track$bins))!=length(track$bins)){
        print(paste0("WARNING: ", "length(unique(track$bins))!=length(track$bins)"))
    } else {
        write.table(track, file=paste0(outpath,"/","HCRtrack_",filename,".tsv") ,col.names = TRUE, row.names = FALSE, sep="\t")
    }

    return(track)

}
