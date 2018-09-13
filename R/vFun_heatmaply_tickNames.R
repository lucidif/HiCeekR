##2)ticksNames================================================================
#' ticksNames
#'
#' @param ticks
#' @param matrix
#' @param axis
#' @param as.path
#' @param revert
#' @param simMirror
#'
#' @return
#' @keywords internal
#'
#' @examples
ticksNames<- function (ticks, matrix, axis='row' ,as.path=FALSE, revert=FALSE,
                        simMirror=FALSE){
    ##axis types:    'row', 'col'

    lenti <-length(ticks)
    ticknames<-c(1:lenti)

    if (as.path==TRUE){

        matrix<-readContactMatrix(pathM)
        df <- as.data.frame(matrix)
        df_mat <- data.matrix(df)

    } else {
        df_mat<-matrix
    }

    if (axis=='row'){
        names<-rownames (matrix)
    }

    if (axis=='col'){
        names<-colnames (matrix)
    }

    if (revert==TRUE){
        names<- rev (names)
    }

    if (simMirror==TRUE){
        ##CERCA I CROMOSOMI
        split<-strsplit (names,':')
        lenspli<- length (split)
        chrlist<-c(1:lenspli)
        for (i in 1:lenspli){
            chrlist[i]<-paste0(split[[i]][1],':')
        }
        chrlist<-unique (chrlist)
        #print (paste0('chrlist:',chrlist))
        revlist<-rev (chrlist)
        #print (paste0('revlist',revlist))
        namesInverted<-c (1:2)
        ##mo devi greppare su ogni singolo cromosoma ordinato
        #in revlist estrarre da qui i singoli array per singole
        #matrici e poi unirle assieme print
        #(paste0('length(revlist)<-',length(revlist)))

        for (i in 1:length(revlist)){
            if (i > 1) {
                prenames<-namesOfReg
                indexChr<-grep (revlist[i],names, fixed=TRUE)
                namesOfReg<- rev(names[indexChr])
                namesOfReg<- c(prenames,namesOfReg)
            } else {
                indexChr<-grep (revlist[i],names, fixed=TRUE)
                namesOfReg<- rev(names[indexChr])
            }
        }
        #print (namesOfReg)
        #print ('end namesOfReg')

        for (i in 1:lenti){
            ticknames[i]<-namesOfReg[as.numeric(ticks[i])]
        }
        #firstIndexChr<-grep (,rownames(mat), fixed=TRUE)
        #rownames (mat)[ttt]
    } else {
        for (i in 1:lenti){
            ticknames[i]<-names[as.numeric(ticks[i])]
        }

    }


    return (ticknames)

}


##============================================================================
