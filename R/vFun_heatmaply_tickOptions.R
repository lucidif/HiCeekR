#' tickOptions
#'
#' @param tickNamesObj
#' @param chr
#' @param start
#' @param end
#'
#' @return
#' @keywords internal
#'
#' @examples
tickOptions<- function (tickNamesObj, chr=TRUE, start=TRUE, end=FALSE){

    split<-strsplit (tickNamesObj,':')


    lenspli<- length (split)
    chrlist<-c(1:lenspli)
    splitted<- matrix (ncol=3, nrow=lenspli)
    for (i in 1:lenspli){
        splitted[i,1]<-split[[i]][1]
        splitted[i,2]<-split[[i]][2]
        #splitted[i,3]<-split[[i]][3]




    }

    split2<-strsplit (splitted[,2],'-')
    for (i in 1:lenspli){
        splitted[i,2]<-split2[[i]][1]
        splitted[i,3]<-split2[[i]][2]
        #splitted[i,3]<-split[[i]][3]
    }


    #print (splitted)
    colnames (splitted)<-c('chr','start','end')
    #-which(colnames(splitted) %in% c("z","u"))
    if (chr==FALSE){
        splitted<- as.matrix(splitted[,-which(colnames(splitted) %in% 'chr')])
    }

    if (start==FALSE){
        splitted<- as.matrix(splitted[,-which(colnames(splitted) %in% 'start')])
    }

    if (end==FALSE){
        splitted<- as.matrix(splitted[,-which(colnames(splitted) %in% 'end')])
    }

    #-------------------------------------------------------------------------

    print (length(splitted[1,]))
    preSplit<-splitted

    if (as.numeric(length(splitted[1,])==3)){
        print ('dim3')
        result<-rep(paste0(splitted[,1],':',splitted[,2],'-',splitted[,3]))
    }
    if (as.numeric(length(splitted[1,])==2)){
        print ('dim2')
        result<-rep(paste0(splitted[,1],'-',splitted[,2]))

        if (colnames(splitted)[1]=='chr'){
            result<-rep(paste0(splitted[,1],':',splitted[,2]))
        } else{result<-rep(paste0(splitted[,1],'-',splitted[,2]))}

    }
    if (as.numeric(length(splitted[1,])==1)){
        print ('dim1')
        result<-rep(paste0(splitted[,1]))
    }
    if (as.numeric(length(splitted[1,])==0)){
        print ('dim0')
        result<-rep(paste0(preSplit[,1],':',preSplit[,2],'-',preSplit[,3]))
    }

    return (as.array(result))

}
