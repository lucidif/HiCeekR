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
    #uninTT<<-parseFilePaths(input)
    #unin<-unlist(input$files)
    for (i in 1:(length(unin)-1)){
        unin[[i]][1]<-paste0(unin[[i]][1],"/")
    }

    lasto<-unin[length(unin)]

    path<-paste(unin,collapse="")

    #path assoluto da modificare
    # path<-paste0(
    #     shinyFiles::getVolumes()()["Computer"],
    #     shinyFiles::getVolumes()()[Sys.info()["user"]],
    #     path
    # )


    if(last==TRUE){
        print(paste0("lasto:",lasto))
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

    # path<-paste0(
    #     shinyFiles::getVolumes()()["Computer"],
    #     shinyFiles::getVolumes()()[Sys.info()["user"]],
    #     path
    # )

    return(path)

}
