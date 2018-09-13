#' extractContact
#'
#' @param interactionSetObj
#' @param intChrFirst
#' @param intChrSecond
#'
#' @return
#' @keywords internal
#'
#' @examples
extractContact <- function (interactionSetObj, intChrFirst, intChrSecond){
    ##intChr= chormosome name or 'none' or 'all'
    #contMa= as.data.frame(interactions(interactionSetObj)[(seqnames(anchors(interactionSetObj,type='first'))==intChrFirst&seqnames(anchors(interactionSetObj,type='second'))==intChrSecond)|(seqnames(anchors(interactionSetObj,type='first'))==intChrSecond&seqnames(anchors(interactionSetObj,type='second'))==intChrFirst)])

    if (intChrFirst=='all'){

        if (intChrSecond=='all'){ #allVsAll
            contMa= as.data.frame(InteractionSet::interactions(interactionSetObj))
        } else { #allVsChr
            contMa= as.data.frame(
                InteractionSet::interactions(interactionSetObj)[(
                    GenomicRanges::seqnames(
                        InteractionSet::anchors(
                            interactionSetObj,type='second'))==intChrSecond)])
        }
    } else {
        if (intChrSecond=='all'){#chrVsAll
            contMa= as.data.frame(
                InteractionSet::interactions(interactionSetObj)[(
                    GenomicRanges::seqnames(
                        InteractionSet::anchors(
                            interactionSetObj,type='first'))==intChrFirst)])
        } else { #chrVsChr
            contMa= as.data.frame(
                InteractionSet::interactions(interactionSetObj)[(
                    GenomicRanges::seqnames(
                        InteractionSet::anchors(interactionSetObj,
                                                type='first'))==
                        intChrFirst&
                        GenomicRanges::seqnames(
                            InteractionSet::anchors(interactionSetObj,
                                                    type='second'))==
                        intChrSecond)|
                        (GenomicRanges::seqnames(
                            InteractionSet::anchors(interactionSetObj,
                                                    type='first'))==
                             intChrSecond &
                             GenomicRanges::seqnames(
                                 InteractionSet::anchors(interactionSetObj,
                                                         type='second'))==
                             intChrFirst)])
        }
    }

    #View(contMa)
    #contMa= as.data.frame(interactions(interactionSetObj)[(seqnames(anchors(interactionSetObj,type='first'))==intChrFirst&seqnames(anchors(interactionSetObj,type='second'))==intChrSecond)])
    collen<- length(contMa[1,])
    #print (paste0('collen',collen))
    collenFinal<- 8
    overlen<- (collen-collenFinal)/2
    #paste0('overlen:',overlen)
    contMa2=matrix (ncol=8, nrow=length(contMa[,1]))
    contMa2[,1]<-as.character(contMa[,1])
    contMa2[,2]<-contMa[,2]
    contMa2[,3]<-contMa[,3]
    contMa2[,4]<-contMa[,4]
    contMa2[,5]<-as.character(contMa[,5+overlen])
    contMa2[,6]<-contMa[,6+overlen]
    contMa2[,7]<-contMa[,7+overlen]
    contMa2[,8]<-contMa[,8+overlen]
    # for (i in 1:overlen/2){
    #   contMa=contMa[,-5]
    #   contMa=contMa[,-9]
    # }

    # contMa=contMa[,-5]
    # contMa=contMa[,-5]
    # contMa=contMa[,-9]
    # contMa=contMa[,-9]
    View (contMa)
    colnames(contMa2)<- c('T_Chr','T_start','T_end','T_width','A_Chr',
                          'A_start','A_end','A_width')
    return (contMa2)
}
