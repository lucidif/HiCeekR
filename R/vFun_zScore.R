#' zScore
#'
#' @param pcaTable
#'
#' @return
#' @keywords internal
#'
#' @examples
zScore<- function (pcaTable){

    numcol<- length (pcaTable[1,])
    numrow<- length (pcaTable[,1])
    pcaTable[is.na(pcaTable)] <- 0

    for (col in 1:numcol){

        stanDev<-sd (pcaTable[,col], na.rm=TRUE)
        meanCol<- mean (pcaTable[,col])

        for (row in 1:numrow){

            zVal<- (pcaTable[row,col]-meanCol)/stanDev
            pcaTable[row,col]<- zVal

        }

        return (pcaTable)

    }

}
