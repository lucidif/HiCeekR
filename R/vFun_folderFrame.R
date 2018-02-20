#'folderFrame
#'
#'obtain data frame with list of folder in specific path
#'
#' @param path path from which to obtain the data frame
#'
#' @return
#' @export
#'
#' @examples
folderFrame<-function(path){
    a<-as.data.frame(list.files(path))
    #colnames(a)<-path
    return(a)
}
