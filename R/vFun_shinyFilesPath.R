#' shinyFilesPath
#'
#' @param input
#' @param last
#'
#' @return
#' @keywords internal
#'
#' @examples
shinyFilesPath<-function(input, last=FALSE){
    unin<-input
    #uninTT<<-unin
    #unin<-unlist(input$files)
    for (i in 1:(length(unin)-1)){
        unin[[i]][1]<-paste0(unin[[i]][1],"/")
    }

    lasto<-unin[length(unin)]

    path<-paste(unin,collapse="")

    if(last==TRUE){
        print(paste0("lasto:",lastro))
        return(lasto)
    } else { #last==FALSE
        print(paste0("path:",path))
        return(path)
    }


}

#' shinyDirPath
#'
#' @param input
#'
#' @return
#' @keywords internal
#'
#' @examples
shinyDirPath<-function(input){
    unin<-unlist(input)
    unin2<-c()
    for (i in 1:(length(unin)-1)){
        unin2[i]<-paste0(unin[[i]][1],"/")
    }

    path<-paste(unin2,collapse="")
    return(path)

}
