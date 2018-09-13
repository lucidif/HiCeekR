#' restartFrom
#'
#' @param infoTablePath
#' @param pipelineStep
#' @param configFile
#'
#' @return
#' @keywords internal
#'
#' @examples
restartFrom<-function(infoTablePath, pipelineStep, configFile="./HCR.config"){

    workingDir<-as.character((read.table(configFile))[1,1])
    it<-as.matrix(data.frame(read.table(infoTablePath, sep="\t")))

    if (length(it[1,]==3)){
        it<-it[,-1]
    }
    mainPrj<-it[4,2]
    mainPath<-paste0(workingDir,"HiCeekRwd/Projects/",mainPrj,"/")
    currentProject<-it[5,2]
    pipStep<-c("Pre-Processing", "Filtering", "Binning",
               "Normalization", "Downstream", "Report")

    for (i in which(pipStep==pipelineStep):length(pipStep)){
        sysFiles<-list.files(paste0(mainPath,currentProject), recursive=TRUE)
        sysFiles<-sysFiles[grep(paste0("/", pipStep[i]) ,sysFiles, fixed=TRUE)]
        if (length(sysFiles)>=1){
            for (j in 1:length(sysFiles)){
                print (paste0("j:",j))
                file.remove(paste0(mainPath,currentProject,"/",sysFiles[j])
                )
            }
        }


    }

}


