#' obtainName
#'
#' @param analysisFolder
#'
#' @return
#' @keywords internal
#'
#' @examples
obtainName <-function (analysisFolder){
    subSt<- strsplit (analysisFolder,'/')
    maxlen<-length(subSt[[1]])
    analysisName<-subSt[[1]][maxlen]
    return ( analysisName )
}
