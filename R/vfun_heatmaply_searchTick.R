##1)searchTicks=================================================================
#' searchTicks
#'
#' @param matrix
#' @param axis
#' @param numberOfTicks
#' @param as.path
#'
#' @return
#' @keywords internal
#'
#' @examples
searchTicks <- function (matrix, axis='row' , numberOfTicks=14 , as.path=FALSE){
    ##matrix as data.matrix
    ##axis types:    'row', 'col'

    if (as.path==TRUE){

        matrix<-readContactMatrix(pathM)
        df <- as.data.frame(matrix)
        df_mat <- data.matrix(df)

    } else {

        df_mat<-matrix
    }


    ticks<- c(1:numberOfTicks)

    if (axis=='row'){
        rnames<-rownames (df_mat)
        rnamelen<- length (rnames)
        passo<- as.numeric (rnamelen/numberOfTicks)
        print (paste0('passoBefore=',passo))
        if (passo<1){
            passo<-1
        }
        passo<- as.integer (passo)

        print (paste0('passo=',passo))
        y<-1
        i<-1
        while (i <= rnamelen){

            ticks[y]<-i
            i<-i+passo
            y<-y+1
        }

        #ticks[y]<- rnamelen

    }

    if (axis=='col'){
        cnames<- colnames (df_mat)
        cnamelen<- length (cnames)
        passo<- as.numeric (cnamelen/numberOfTicks)
        print (paste0('passoBefore=',passo))
        if (passo<1){
            passo<-1
        }
        passo<- as.integer (passo)

        print (paste0('passo=',passo))
        y<-1
        i<-1
        while (i <= cnamelen){

            ticks[y]<-i
            i<-i+passo
            y<-y+1
        }

        #ticks[y]<- cnamelen

    }

    return (ticks)

}

##============================================================================
