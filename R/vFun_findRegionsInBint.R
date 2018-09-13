
#===========================================================================
#findRegionInBint   Find specific region (chr,start,end) in bint.bed file
#===========================================================================

#' findRegionInBint
#'
#' @param bint.bed_datapath
#' @param chr
#' @param start
#' @param end
#' @param id
#' @param twoReg
#' @param secChr
#' @param secStart
#' @param secEnd
#' @param imported
#' @param only.chr
#'
#' @return
#' @keywords internal
#'
#' @examples
findRegionInBint<- function (bint.bed_datapath, chr, start, end, id=TRUE,
                             twoReg=FALSE, secChr, secStart, secEnd,
                             imported=FALSE, only.chr=FALSE){
    #test region <- c(chr1, 2997219, 12006958)

    if (imported==TRUE){
        bintBegin<-bint.bed_datapath

    } else {
        bintBegin<-read.table (bint.bed_datapath, sep='\t')
    }


    bintBegin<-subset (bintBegin, bintBegin[,1]==chr)

    if (only.chr==FALSE){
        bintBegin<-subset (bintBegin,
                            as.numeric(bintBegin[,2])>=as.numeric (start) |
                            (as.numeric(bintBegin[,3])>=as.numeric(start) &
                            as.numeric(bintBegin[,2])<=as.numeric(start)) )
        bintBegin<-subset (bintBegin,
                            as.numeric(bintBegin[,3])<=as.numeric (end) |
                            (as.numeric(bintBegin[,3])>=as.numeric(end) &
                            as.numeric(bintBegin[,2])<=as.numeric(end))
        )
    }

    print (bintBegin)

    if (twoReg==TRUE){

        bintBegin1<-bintBegin
        bintBegin<-read.table (bint.bed_datapath, sep='\t')
        bintBegin<-subset (bintBegin, bintBegin[,1]==secChr)

        if (only.chr==FALSE){
            bintBegin<-subset (bintBegin,
                            as.numeric(bintBegin[,2])>=as.numeric (secStart) |
                            (as.numeric(bintBegin[,3])>=as.numeric(secStart) &
                            as.numeric(bintBegin[,2])<=as.numeric(secStart)) )
            bintBegin<-subset (bintBegin,
                            as.numeric(bintBegin[,3])<=as.numeric (secEnd) |
                            (as.numeric(bintBegin[,3])>=as.numeric(secEnd) &
                            as.numeric(bintBegin[,2])<=as.numeric(secEnd))
            )
        }

        bintBegin2<-bintBegin
        print (bintBegin2)
        namesA<- rownames (bintBegin1)
        print (namesA)
        print ('names1.....maked')
        namesB<- rownames (bintBegin2)
        print (namesB)
        print ('names2.....maked')
        namesTot<- c (namesA,namesB)
        print (namesTot)
        bintBegin<-merge(bintBegin1, bintBegin2, by = "V1", all = TRUE)
        rownames(bintBegin)<-namesTot
    }


    return (if (id==FALSE){bintBegin}
            else {rownames(bintBegin)}
    )

}

