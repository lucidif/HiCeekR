#' as.brute.matrix
#'
#' @param InteractionSer_object
#' @param first
#' @param second
#' @param fill
#' @param alternativeFill
#'
#' @return
#' @keywords internal
#'
#' @examples
as.brute.matrix<- function (InteractionSer_object, first='all', second='all',
                            fill=NULL, alternativeFill=FALSE){

    chri<- InteractionSet::regions(InteractionSer_object)

    if (length(first)>1){
        nmChriFi<-c()
        for (cy in 1:length(first)){
            print (cy)
            chri1<-chri[GenomicRanges::seqnames(InteractionSet::regions(InteractionSer_object))==first[cy]]
            assign(paste0("chriFis",cy),chri1)
            nmChriFi[cy]<-paste0("chriFis",cy)
        }
        for (cy in 1:length(nmChriFi)){
            if (cy==1){
                chri1<-get(nmChriFi[cy])
            } else {
                chri1<-c(chri1,get(nmChriFi[cy]))
            }
        }

    }else{
        chri1<-InteractionSet::regions (InteractionSer_object)
        if (first!='all'){
        chri1<-InteractionSet::regions (InteractionSer_object)
        chri1<-chri1[GenomicRanges::seqnames(InteractionSet::regions(InteractionSer_object))==first]
        }
    }
    # chri1<-chri1[seqnames(regions(InteractionSer_object))==first]
    chri1names<- as.data.frame (chri1)
    chri1names[,1]<- rep (paste0(chri1names[,1],':',chri1names[,2],'-',chri1names[,3]))
    chri1names<- unique(as.data.frame (chri1names[,1]))




    if (length(second)>1){
        nmChri<-c()
        for (cy in 1:length(second)){
            chri2<-chri[GenomicRanges::seqnames(InteractionSet::regions(InteractionSer_object))==second[cy]]
            assign(paste0("chriSec",cy),chri2)
            nmChri[cy]<-paste0("chriSec",cy)
        }
        for (cy in 1:length(nmChri)){
            if (cy==1){
                chri2<-get(nmChri[cy])
            } else {
                chri2<-c(chri2,get(nmChri[cy]))
            }
        }

    }else{
        chri2<-InteractionSet::regions (InteractionSer_object)
        if (second!='all'){
            chri2<-InteractionSet::regions (InteractionSer_object)
            chri2<-chri2[GenomicRanges::seqnames(InteractionSet::regions(InteractionSer_object))==second]
        }
    }




    # chri2<-chri2[seqnames(regions(InteractionSer_object))==second]
    chri2names<- as.data.frame (chri2)
    chri2names[,1]<- rep (paste0(chri2names[,1],':',chri2names[,2],'-',chri2names[,3]))
    chri2names<- unique(as.data.frame (chri2names[,1]))

    if (alternativeFill==FALSE){
        contma<-InteractionSet::inflate(InteractionSer_object, chri1, chri2,
                                        fill=SummarizedExperiment::assay(InteractionSer_object)[,1])
    } else {
        contma<-InteractionSet::inflate(InteractionSer_object, chri1, chri2, fill=fill)
    }
    ##need install "Matrix"
    #chri1TT<<-chri1
    #chri2TT<<-chri2
    #contmaTT<<-contma
    #isObjTT<<-InteractionSer_object
    #fillTT<<-fill
    contma2<- Matrix::as.matrix (contma)
    brutema<- as (contma2, 'matrix')
    #brutemaTT<<- as (contma2, 'matrix')
    #chri1namesTT<<-chri1names
    #chri2namesTT<<-chri2names
    rownames(brutema)<-chri1names[,1]
    colnames(brutema)<-chri2names[,1]

    return(brutema)

}
