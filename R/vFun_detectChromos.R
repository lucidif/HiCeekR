#' Title
#'
#' @param path
#' @param onlyChr
#'
#' @return
#' @keywords internal
#'
#' @examples
detectMatrixCoordinate<-function(path, onlyChr=FALSE){
    ma<-read.table(path, check.names = FALSE,sep="\t",header=TRUE)
    rownames(ma)<-ma[,1]
    chrAma<-ma[,-1]



    namma<-rownames(ma)
    chrma<-c()
    startma<-c()
    endma<-c()
    namma<-gsub(":","-",namma)
    stringma<-strsplit(namma,"-")
    for (i in 1:length(namma)){
        chrma[i]<-stringma[[i]][1]
        startma[i]<-stringma[[i]][2]
        endma[i]<-stringma[[i]][3]
    }

    if(onlyChr==TRUE){
        chromos<-unique(chrma)
        return(chromos)

    }else{
        return(df<-data.frame(chrma,startma,endma))
    }
}


#' Title
#'
#' @param path
#'
#' @return
#' @keywords internal
#'
#' @examples
detectChromos<-function(path){
    namstring<-readLines(path,n=1)
    namstring<-unlist(strsplit(namstring,"\t"))
    namstring<-namstring[2:length(namstring)]

    chrma<-c()
    startma<-c()
    endma<-c()
    namstring<-gsub(":","-",namstring)
    stringma<-strsplit(namstring,"-")
    for (i in 1:length(namstring)){
        chrma[i]<-stringma[[i]][1]
        startma[i]<-stringma[[i]][2]
        endma[i]<-stringma[[i]][3]
    }

    chromos<-unique(chrma)
    return(chromos)
}
