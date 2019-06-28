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
                            fill=NULL, alternativeFill=FALSE, NAmin=FALSE, wkFolder=""){

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

    if(NAmin==TRUE){

        cr<-parallel::detectCores()
        workers<-as.integer((cr/4)*3)

        divisor<-as.integer(length(brutema[,1])/workers)
        divma<-c()
        dvidx<-1
        for(i in 1:workers){

            if (i==workers){
                subma<-brutema[((divisor*(i-1))+1):(length(brutema[,1])),]
            } else {
                subma<-brutema[((divisor*(i-1))+1):(divisor*i),]
            }

            nam<-paste0("ma",i)
            assign(nam,subma)
            divma[i]<-nam
        }

        #length(rownames(ma1))+length(rownames(ma2))+length(rownames(ma3))+length(rownames(ma4))+length(rownames(ma5))+length(rownames(ma6))

        parSos<-function(x, matrixArrayNames,tmpFolder="", min=0){
            #replace NA with min matrix value
            wkMatrix<- get(matrixArrayNames[x])
            #wkMa<<-wkMatrix

            idx <- which(is.na(wkMatrix)==TRUE, arr.ind=TRUE)

            for(i in 1:length(idx[,1])){
                wkMatrix[idx[i,1], idx[i,2]] <- min
            }
            HCRwrite(wkMatrix,file=matrixArrayNames[x],path=tmpFolder, row.names=TRUE)
        }

        min <- min(brutema, na.rm = TRUE)
        param <- BiocParallel::MulticoreParam(workers = workers)


        BiocParallel::bplapply(1:workers,
                               parSos,
                               matrixArrayNames=divma,
                               tmpFolder=wkFolder,
                               min=min
                               ,BPPARAM = param
                               )

        for (i in 1:length(divma)){
            if (i==1){
                #baseMa<-get(divma[i])
                baseMa<-HCRread(file=paste0(divma[i],".tsv"),path=wkFolder)
                #file.remove(paste0(wkFolder,"/",divma[i],".tsv"))
            } else {
                #addMa<-get(get(divma[i]))
                addMa<-HCRread(file=paste0(divma[i],".tsv"),path=wkFolder)
                baseMa<-rbind(baseMa,addMa)
                #file.remove(paste0(wkFolder,"/",divma[i],".tsv"))
            }

        }

        brutema<-baseMa

    } #NAmin TRUE end

    return(brutema)

}
