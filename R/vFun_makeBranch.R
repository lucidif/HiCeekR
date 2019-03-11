#' makeBranch
#'
#' @param branchName
#' @param pipelineStep
#' @param configFile
#' @param infoTsv
#'
#' @return
#' @keywords internal
#'
#' @examples
makeBranch<-function(branchName, pipelineStep, configFile="./HCR.config", infoTsv){
    #pipelineStep possibility: Binning, Downstream, Filtering, Normalization,
    #                           Pre-Processing, Report, Visualization

    workingDir<-paste0(getwd(),as.character((read.table(configFile))[1,1]))
    it<-as.matrix(data.frame(read.table(infoTsv, sep="\t")))

    if (length(it[1,]==3)){
        it<-it[,-1]
    }
    mainPrj<-it[4,2]
    mainPath<-paste0(workingDir,"HiCeekRwd/Projects/",mainPrj,"/")
    currentProject<-it[5,2]
    it[5,2]<-branchName
    pipStep<-c("Pre-Processing", "Filtering", "Binning",
               "Normalization", "Downstream", "Report")

    # itNames<-c("InputPath", "Resolution", "Type",
    #            "ProjectName", "AnalysisName","comment")
    # itValues<-c(shinyFilesPath(input$bamFile) , input$binSize,
    #             input$inputType, prjName, input$anNewName,
    #             paste0("Reference Genome:",
    #                    shinyFilesPath(input$refGenome, last=TRUE)))


    if (dir.exists(paste0(mainPath,branchName))==FALSE){
        makeHCRan(branchName, mainPath, infoTable=it)
        file.copy(paste0(mainPath,currentProject), paste0(mainPath,branchName))

        for (i in 1:(which(pipStep==pipelineStep)-1)){
            print (paste0("i:",i))
            print(paste0("mainPath:",mainPath,
                         "    branchName:",branchName,
                         "    currentProject:",currentProject))
            sysFiles<-list.files(paste0(mainPath,currentProject), recursive=TRUE)
            #print(sysFiles)
            sysFiles<-sysFiles[grep(paste0("/", pipStep[i]) ,sysFiles, fixed=TRUE)]
            #print(sysFiles)
            if (length(sysFiles)>=1){
                for (j in 1:length(sysFiles)){
                    print (paste0("j:",j))
                    file.copy(paste0(mainPath,currentProject,"/",sysFiles[j]),
                              paste0(mainPath,branchName,"/",sysFiles[j]),
                              overwrite=TRUE)
                }
            }



        }
    } else {
        return(print("already existing folder"))
    }



}
