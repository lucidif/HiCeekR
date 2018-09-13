#' matrixExport
#'
#' @param redata
#' @param type
#' @param chrA
#' @param chrB
#' @param chgFill
#' @param alternativeFill
#' @param NAas0
#'
#' @return
#' @keywords internal
#'
#' @examples
matrixExport<- function (redata, type='all' ,
                         chrA, chrB, chgFill=FALSE,
                         alternativeFill , NAas0=TRUE){

    #redataTT<<-redata
    ##vsA & vsB = 'chr' ; 'all'

    #bb<-read.table (bint.bed, sep='\t')
    #allChr<- unique (bb[,1])
    #View (bb)
    if (type=='chr' ){

        ##first subset for chromosome
        if (chrA==chrB){

            if (chgFill==FALSE){
                firstSubMatrix<- as.brute.matrix (redata, first=chrA  ,
                                                  second=chrA  ,
                                                  fill=as.numeric(SummarizedExperiment::assay(redata)[,1]))
            } else {#chgFill=TRUE
                firstSubMatrix<- as.brute.matrix (redata, first=chrA  ,
                                                  second=chrA  ,
                                                  fill=as.numeric(alternativeFill),
                                                  alternativeFill=TRUE)
            }

        } else {

            if (chgFill==FALSE){
                firstSubMatrix<- as.brute.matrix (redata, first=c(chrA, chrB)  ,
                                                  second=c(chrA, chrB)  ,
                                                  fill=as.numeric(SummarizedExperiment::assay(redata)[,1]))
            } else {
                firstSubMatrix<- as.brute.matrix (redata, first=c(chrA, chrB)  ,
                                                  second=c(chrA, chrB)  ,
                                                  fill=as.numeric(alternativeFill), alternativeFill=TRUE)
            }


        }
        resultMatrix<-firstSubMatrix
    }

    else {#type=='all'


        #========================================================
        if (chgFill==FALSE){
            resultMatrix<-as.brute.matrix (redata)
        } else {#chgFill=TRUE
            resultMatrix<- as.brute.matrix (redata,  fill=alternativeFill, alternativeFill=TRUE)
        }
        #========================================================

    }

    if (NAas0==TRUE){
        resultMatrix[is.na(resultMatrix)]<- 0
    }

    return (resultMatrix)

}
