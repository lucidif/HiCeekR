#1)infoTable test



#' testInfoTable
#'
#' test if infoTable is correct
#'
#' @param infoTable the table that contain analysis information
#' @param stop stop process if test fail
#'
#' @return boolean TRUE if input is correct
#' @keywords internal
#'
#' @examples
testInfoTable<-function(infoTable, stop=TRUE){

    if(length(infoTable[1,])==2){
        if(length(infoTable[,1])==6){
            return(infoTable)
        } else {
            if (stop==TRUE){
                stop("incorrect infoTable variable")
            }else {#stop==FALSE
                warning("incorrect infoTable variable")
            }
        }
    }

}
