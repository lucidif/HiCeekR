#' selectFile
#'
#' @param id
#' @param path
#' @param label
#' @param subset
#' @param pattern
#' @param width
#'
#' @return
#' @keywords internal
#'
#' @examples
selectFile<-function (id  ,path, label='Select File', subset=FALSE,
                        pattern='NA', width='100%'){

    if (subset==TRUE){
        fileList<-list.files(path, pattern=pattern)
        if (length (fileList)==0){
            fileList<- 'none'
        }
    } else {#subset==FALSE
        fileList<-list.files(path)
        if (length (fileList)==0){
            fileList<- 'none'
        }
    }


    return (

        selectInput(id, label = label,
                    choices = c("please select file",fileList),
                    selected = "please select file", width=width)


    )
}
