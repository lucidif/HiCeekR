
#=============================================================================
#regionsInMatrix
#cerca i cromosomi contenuti in rownames e col names della contact matrix
#=============================================================================

#' regionsInMatrix
#'
#' @param matrixPath
#' @param separate
#' @param on
#' @param unique
#' @param as.Path
#'
#' @return
#' @keywords internal
#'
#' @examples
regionsInMatrix<- function (matrixPath, separate=FALSE, on='row',
                            unique=TRUE, as.Path=TRUE){

    if (as.Path==TRUE){
        ma<-readContactMatrix(matrixPath)
        #print ('as.Path==TRUE')
    } else {
        #print ('as.Path==FALSE')
        ma<-matrixPath

    }
    #maTT<<-ma

    if (separate==TRUE){
        #print ('separate==TRUE')
        if (on=='row'){
            #print ("on=='row'")
            names<-rownames (ma)
        }

        if (on=='col'){
            #print ("on=='col'")
            names<-colnames (ma)
        }
    } else {
        #print ('separate==FALSE')
        rn<-rownames (ma)
        cn<-colnames (ma)
        names<-c(rn,cn)
        rm (rn)
        rm (cn)
    }
    #namesTT<<-names
    split<-strsplit (names,':')
    lenspli<- length (split)
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
    colnames (splitted)<-c('chr','start','end')

    if (unique==TRUE){
        splitted<- unique (splitted)
    }

    return (splitted)

}
