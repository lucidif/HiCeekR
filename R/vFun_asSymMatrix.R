#' Title
#'
#' @param chrAma
#' @param chrBma
#' @param chrABma
#'
#' @return
#' @keywords internal
#'
#' @examples
as.sym.matrix<-function(chrAma,chrBma,chrABma){
    namA<-rownames(chrAma)
    chrA<-c()
    startA<-c()
    endA<-c()
    namA<-gsub(":","-",namA)
    stringA<-strsplit(namA,"-")
    for (i in 1:length(namA)){
        chrA[i]<-stringA[[i]][1]
        startA[i]<-stringA[[i]][2]
        endA[i]<-stringA[[i]][3]
    }

    namB<-rownames(chrBma)
    chrB<-c()
    startB<-c()
    endB<-c()
    namB<-gsub(":","-",namB)
    stringB<-strsplit(namB,"-")
    for (i in 1:length(namB)){
        chrB[i]<-stringB[[i]][1]
        startB[i]<-stringB[[i]][2]
        endB[i]<-stringB[[i]][3]
    }

    namAB<-rownames(chrABma)
    chrAB<-c()
    startAB<-c()
    endAB<-c()
    namAB<-gsub(":","-",namAB)
    stringAB<-strsplit(namAB,"-")
    for (i in 1:length(namAB)){
        chrAB[i]<-stringAB[[i]][1]
        startAB[i]<-stringAB[[i]][2]
        endAB[i]<-stringAB[[i]][3]
    }

    print("name exported")
    #AplusAB<-cbind(chrAma,chrABma)
    AplusAB<-dplyr::bind_cols(as.data.frame(chrAma),as.data.frame(chrABma))
    print("bind cols 1 OK")
    rm(chrAma)
    gc()
    chrABma<-t(chrABma)
    #names(chrABma)<-names(chrBma)
    #ABplusB<-cbind(chrABma,chrBma)
    ABplusB<-dplyr::bind_cols(as.data.frame(chrABma),as.data.frame(chrBma))
    print("bind cols 2 OK")
    rm(chrABma)
    rm(chrBma)
    gc()

    #symma<-rbind(AplusAB,ABplusB)
    symma<-dplyr::bind_rows(AplusAB,ABplusB)
    rm(AplusAB)
    rm(ABplusB)
    gc()

    return(symma)
}
