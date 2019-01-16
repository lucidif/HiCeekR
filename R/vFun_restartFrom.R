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

    print (paste0("infoTablePath: ", infoTablePath))
    print (paste0("pipelineStep: ", pipelineStep))
    print (paste0("configFile: ", configFile))
    workingDir<-paste0(getwd(),as.character((read.table(configFile))[1,1]))
    it<-as.matrix(data.frame(read.table(infoTablePath, sep="\t")))
    print (paste0("workingDir: ", workingDir))

    if (length(it[1,]==3)){
        it<-it[,-1]
    }
    mainPrj<-it[4,2]
    mainPath<-paste0(workingDir,"HiCeekRwd/Projects/",mainPrj,"/")
    currentProject<-it[5,2]
    print(paste0("currentProjectPath: ",paste0(mainPath,currentProject)))

    pipStep<-c("Pre-Processing", "Filtering", "Binning",
               "Normalization", "Downstream", "Report")

    for (i in which(pipStep==pipelineStep):length(pipStep)){
        sysFiles<-list.files(paste0(mainPath,currentProject), recursive=TRUE)
        sysFiles<-sysFiles[grep(paste0("/", pipStep[i]) ,sysFiles, fixed=TRUE)]
        if (length(sysFiles)>=1){
            for (j in 1:length(sysFiles)){
                print (paste0("j:",j))
                file.remove(paste0(mainPath,currentProject,"/",sysFiles[j]))
                print(paste0("removed file: ",
                            paste0(mainPath,currentProject,"/",sysFiles[j])
                            ))
            }
        }


    }

}


