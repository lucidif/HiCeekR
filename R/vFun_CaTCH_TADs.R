#' CaTCH_TADs
#'
#' @param matrixPath
#' @param chr
#' @param outPath
#'
#' @return
#' @keywords internal
#'
#' @examples
CaTCH_TADs<-function(matrixPath, chr, outPath ){
    library(CaTCH)
    table<-read.table(matrixPath, header=TRUE,sep="\t", check.names = FALSE)

    row.names(table)<-table$index
    table<-table[,-1]
    idxCoord<-row.names(table)
    idx<-c(1:length(idxCoord))
    rownames(table)<-idx
    colnames(table)<-idx

    #deve partire dall'indice sopra
    id1<-c()
    id2<-c()
    value<-c()
    rwid<-0

    for (i in 1:length(table[,1])){ #sostituisci questo for con una funzione da dare in pasto a biocparralle: fai che un primo for genera una serie di file temporanei per ogni riga della tabella Es. rw1.txt , rw2.txt .... paste0("rw",(length(table[,1])),".txt") su questi file applichi il "for j" ottenendo per ogni riga una tabella con le colonne: id1, id2 , value. Alla fine unisci tutte le tabelle con rbind
        print(paste0(i,"/",length(table[,1])))
        if(rwid==0){
            rwid<-1
        }
        for (j in i:length(table[1,])){
            id1[rwid]<-i
            id2[rwid]<-j
            value[rwid]<-table[i,j]
            rwid<-rwid+1
        }
    }


    nosym<-data.frame(rep(chr,length(value)),id1,id2,value)
    write.table(nosym,file=paste0(outPath,"/CaTCH.txt"),col.names=FALSE,row.names = FALSE, sep=" " ,quote=FALSE)

    result<-domain.call(paste0(outPath,"/CaTCH.txt"))


}



