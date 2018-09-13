#' folderList
#'
#' @param path
#' @param type
#' @param ecception
#' @param ecceptionName
#'
#' @return
#' @keywords internal
#'
#' @examples
folderList<- function (path, type="dirs", ecception=FALSE, ecceptionName) {
    # "dirs" "files" "all"

    if (type=="dirs"){
        patto<-list.dirs (path, full.names=FALSE)

        if (length(patto)==1){
            selected<-patto
        } else {
            patto<-subset(patto, patto!="")
            selected<-c()
            for (i in 1:length(patto)){
                diva<-strsplit(patto[i], "/")
                if (length(diva[[1]])==1){
                    selected[i]<-patto[i]
                }
            }
            selected<-subset(selected, is.na(selected)==FALSE)
        }
    }

    if (ecception==TRUE){
        selected<-subset(selected, selected!=ecceptionName)
    }

    return(selected)

}
