
#' pointin
#'
#' @param analysisFolder Character vector with path of specific analysis folder
#' @param specificFolderName Character vector with name of folder of interest
#' @param sys bulean fro choicing if save in Results folder or in SysOut folder
#'
#' @return
#' @keywords internal
#'
#' @examples
pointin<- function(analysisFolder, specificFolderName , sys=FALSE){

    exSw<-FALSE

    subSt<-strsplit(analysisFolder, "/")
    maxlen<-as.numeric(length(subSt[[1]]))
    analysisName<-subSt[[1]][maxlen]
    projectName<-subSt[[1]][maxlen-1]
    #wd<- projectName<-subSt[[1]][maxlen-3]
    toremove<-paste0(projectName, "/",analysisName, "/")
    #print(c( "analysisFolder ",analysisFolder))
    toremoveAll<-paste0( "Projects/",projectName, "/",analysisName, "/")
    #print(c( "toremoveAll ",toremoveAll))
    zl<- sub(as.character(toremove), "" ,analysisFolder, fixed=TRUE)
    spacewd<- sub(as.character(toremoveAll),"",analysisFolder, fixed=TRUE)
    #print(c( "spacewd ",spacewd))

    if (sys==TRUE){
        beforePath<-"SysOut/"
    }else{#sys==FALSE
        beforePath<-"Results/"
    }

    #=========================================================================
    if(specificFolderName == "Projects"){
        poi<-paste0(spacewd, "Projects/")
        exSw<-TRUE
    }

    if(specificFolderName ==  "Annotation"){
        poi<-paste0(spacewd, "Annotations/")
        exSw<-TRUE
    }

    if(specificFolderName == "RefGen"){
        #poi<- paste0(zl,projectName, "/ProjectData/Genomes/")
        poi<- paste0(spacewd, "RefGen/")
        exSw<-TRUE
    }

    #==========================================================================

    if(specificFolderName == "Hi-C"){
        poi<- paste0(zl,projectName, "/ProjectData/Hi-C/")
        exSw<-TRUE
    }

    if(specificFolderName == "Exp"){
        #poi<- paste0(zl,projectName, "/ProjectData/Genomes/")
        poi<- paste0(zl,projectName, "/ProjectData/Exp/")
        exSw<-TRUE
    }

    if(specificFolderName == "Epi"){
        poi<- paste0(zl,projectName, "/ProjectData/Epi/")
        exSw<-TRUE
    }

    #==========================================================================

    if (specificFolderName == "Pre-Processing" |
        specificFolderName == "Binning" |
        specificFolderName == "Filtering"|
        specificFolderName == "Normalization" |
        specificFolderName == "Downstream" |
        specificFolderName == "Visualization" |
        specificFolderName == "Report" |
        specificFolderName == "tmpimg"
    ) {

        poi<- paste0(zl,projectName, "/", analysisName, "/",
                     beforePath , specificFolderName, "/")

        exSw<-TRUE
    }

    if (specificFolderName == "PCA"){
        mafoDown<-"Downstream/"
        # poi<-paste0(zl, projectName, "/", analysisName, "/",
        #             beforePath, mafoDown, specificFolderName,"/"
        # )
        poi<-paste0(zl, projectName, "/", analysisName, "/",
                    beforePath, mafoDown, "/"
        )

        exSw<-TRUE

    }

    if (specificFolderName == "Networks"){
        mafo<-"Report/"
        poi<-paste0(zl, projectName, "/", analysisName, "/",
                    beforePath, mafo, specificFolderName,"/"
        )
        exSw<-TRUE
    }


    #deprecated specificFolderName
    if (specificFolderName == "netReports"){

            ##netReports is deprecated, now point in Networks
            poi<- paste0(zl,projectName, "/", analysisName, "/",
                         beforePath, "Reports/Networks/")
            exSw<-TRUE


    }

    #deprecated specifiFolderName
    if(specificFolderName == "Plots"){
        ##plots is deprecated, now point in Reports
        poi<- paste0(zl,projectName, "/", analysisName,  "/Reports/")
        exSw<-TRUE
    }

    # if(specificFolderName == "PCA"){
    #     poi<- paste0(zl,projectName, "/", analysisName,
    #                 "/Post-Processing/PCA/")
    #     exSw<-TRUE
    # }


    # if(specificFolderName == "Pre-Processing"){
    #     poi<- paste0(zl,projectName, "/", analysisName,
    #                 beforePath ,"Pre-Processing/")
    #     exSw<-TRUE
    # }
    #
    # if(specificFolderName == "Binning"){
    #     poi<- paste0(zl,projectName, "/", analysisName,
    #                 beforePath ,"Binning/")
    #     exSw<-TRUE
    # }
    #
    # if(specificFolderName == "Normalization"){
    #     poi<- paste0(zl,projectName, "/", analysisName,
    #                  "Results/Normalization/")
    #     exSw<-TRUE
    # }
    #
    # if(specificFolderName == "SysNormalization"){
    #     poi<- paste0(zl,projectName, "/", analysisName,
    #                  "SysOut/Normalization/")
    #     exSw<-TRUE
    # }
    #
    # if(specificFolderName == "Plots"){
    #     ##plots is deprecated, now point in Reports
    #     poi<- paste0(zl,projectName, "/", analysisName,  "/Reports/")
    #     exSw<-TRUE
    # }
    #
    # if(specificFolderName ==  "Reports"){
    #     ##plots is deprecated, now point in Reports
    #     poi<- paste0(zl,projectName, "/", analysisName,  "/Reports/")
    #     exSw<-TRUE
    # }
    #
    # if(specificFolderName == "netReports"){
    #     ##netReports is deprecated, now point in Networks
    #     poi<- paste0(zl,projectName, "/", analysisName, "/Reports/Networks/")
    #     exSw<-TRUE
    # }
    #
    # if(specificFolderName == "Post-Processing"){
    #     poi<- paste0(zl,projectName, "/", analysisName,  "/Post-Processing/")
    #     exSw<-TRUE
    # }
    #
    # if(specificFolderName == "PCA"){
    #     poi<- paste0(zl,projectName, "/", analysisName,
    #                 "/Post-Processing/PCA/")
    #     exSw<-TRUE
    # }
    #
    if (exSw == FALSE){
        warning("pointin function did not find folder; path set in
                'SysOut folder'")
    }

    return(poi)

}
