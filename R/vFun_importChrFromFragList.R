#' importChrFromFragList
#'
#' @param regPath
#'
#' @return
#' @keywords internal
#'
#' @examples
importChrFromFragList<- function (regPath){
    rea<-read.table (regPath, sep='\t')
    readerList.unique<- unique (rea[,1])
    readerList.unique<-as.character(readerList.unique)
    return (readerList.unique)
}
