#' moduleStartPanel
#'
#' @param mainNavItem
#' @param wdPath
#' @param moduleDescription
#'
#' @return
#' @keywords internal
#'
#' @examples
moduleStartPanel <- function(mainNavItem, wdPath, moduleDescription){

    #mainNavItem: "Filtering"; "Binning"; "iterative"; "WavSis" ; "TADsHMM"

    dependency<-FALSE

    if (mainNavItem=="Filtering"){
        #print("moduleStartPanel filtering dep")
        dependency<-TRUE
        existCtrFile<- file.exists(paste0(pointin(wdPath,
                                                "Filtering",
                                                sys=TRUE),

                                    "trimmed.h5"
                                   ))

        existPrevious<-file.exists(paste0(pointin(wdPath,
                                                  "Pre-Processing",
                                                  sys=TRUE),

                                          "diags.Robj"
        ))

        #existPrevious<-FALSE
        moduleSet<-mainNavItem
    }

    if (mainNavItem=="Binning"){
        #print("moduleStartPanel binning dep")
        dependency<-TRUE
        existCtrFile<- file.exists(paste0(pointin(wdPath,
                                                "Binning",
                                                sys=TRUE),

                                    "intSet.Robj"
                                    ))
        existPrevious<-file.exists(paste0(pointin(wdPath,
                                                  "Filtering",
                                                  sys=TRUE),

                                    "trimmed.h5"
                                    ))
        moduleSet<-mainNavItem
    }

    if (mainNavItem=="iterative" || mainNavItem=="WavSis"){
        print ("nav Normalization")
        dependency<-TRUE
        existCtrFile1<- file.exists(paste0(pointin(wdPath,
                                                  "Normalization",
                                                  sys=TRUE),

        "iceNorm_correction.Robj"
        ))
        print (paste0("existCtrFile1",existCtrFile1))
        existCtrFile2<- file.exists(paste0(pointin(wdPath,
                                                 "Normalization",
                                                 sys=TRUE),

        "WavSis_correction.Robj"
        ))
        print (paste0("existCtrFile2",existCtrFile2))
        existCtrFile3<-c(existCtrFile1,existCtrFile2)
        print (paste0("existCtrFile3",existCtrFile3))
        if (length(which(existCtrFile3==TRUE))>0){
            existCtrFile<-TRUE
        } else {
            existCtrFile<-FALSE
        }
        print (paste0("existCtrFile",existCtrFile))

        existPrevious<-file.exists(paste0(pointin(wdPath,
                                   "Binning",
                                   sys=TRUE),

        "intSet.Robj"
        ))

        moduleSet<-mainNavItem
    }

    if (mainNavItem=="Heatmaps" || mainNavItem=="Networks"){
        print ("Visualization")
        dependency<-FALSE
        # existCtrFile1<- file.exists(paste0(pointin(wdPath,
        #                                            "Normalization",
        #                                            sys=TRUE),
        #
        #                                    "iceNorm_correction.Robj"
        # ))
        # print (paste0("existCtrFile1",existCtrFile1))
        # existCtrFile2<- file.exists(paste0(pointin(wdPath,
        #                                            "Normalization",
        #                                            sys=TRUE),
        #
        #                                    "WavSis_correction.Robj"
        # ))
        # print (paste0("existCtrFile2",existCtrFile2))
        # existCtrFile3<-c(existCtrFile1,existCtrFile2)
        # print (paste0("existCtrFile3",existCtrFile3))
        # if (length(which(existCtrFile3==TRUE))>0){
        #     existCtrFile<-TRUE
        # } else {
        #     existCtrFile<-FALSE
        # }
        # print (paste0("existCtrFile",existCtrFile))
        #
        # existPrevious<-file.exists(paste0(pointin(wdPath,
        #                                           "Binning",
        #                                           sys=TRUE),
        #
        #                                   "intSet.Robj"
        # ))

        moduleSet<-mainNavItem
    }
    # if (mainNavItem=="WavSis"){
    #     dependency<-TRUE
    #     existCtrFile<- file.exists(paste0(pointin(wdPath,
    #                                             "Normalization",
    #                                             sys=TRUE),
    #
    #     "iceNorm_correction.Robj"
    #     ))
    #
    #     existPrevious<-file.exists(paste0(pointin(wdPath,
    #                                             "Binning",
    #                                             sys=TRUE),
    #
    #     "intSet.Robj"
    #     ))
    # }

    # existCtrFile=FALSE
    # existPrevious=FALSE
    # print (paste0("mainNavItem:",mainNavItem))
    # print (paste0("existCtrFile:",existCtrFile,
    #               "        existPrevious:",existPrevious))
    #print (paste0("moduleSet",moduleSet))
    if (dependency==TRUE){
        if (existCtrFile==TRUE){ #already executed step
            #print(paste0(mainNavItem,"_reStart"))
            return(shiny::wellPanel(mainNavItem,
                                    shiny::fluidRow(
                                        shiny::column(12,
                                                      helpText(moduleDescription)
                                        )
                                    ),
                                    shiny::fluidRow(
                                        shiny::column(12,
                                                      helpText("already executed step")
                                        )
                                    ),
                                    shiny::fluidRow(
                                        shiny::column(3),
                                        shiny::column(3,
                                            shiny::actionButton(paste0(mainNavItem,"_reStart"),
                                                                    label=paste0(mainNavItem,
                                                                                 "Re-Start"))
                                            # shiny::actionButton("restart",
                                            #                         label=paste0(mainNavItem,
                                            #                                      "Re-Start"))
                                        ),
                                        shiny::column(3,
                                            shiny::actionButton(paste0(mainNavItem,"_branch"),
                                                                label="Make Branch")
                                        ),
                                        shiny::column(3)

                                    ),
                                    shiny::fluidRow(
                                        shiny::uiOutput("breSlot") # a che mi serve?
                                            )
                                    # ,shiny::fluidRow(
                                    #     shiny::conditionalPanel(condition="input.yesBranch%2==1",
                                    #
                                    #                             )
                                    #)
                                    )

            )


        } else {#existCtrFile==FALSE
            if (existPrevious==TRUE){ #executable step
                #return(shiny::wellPanel(
                return(shiny::wellPanel(mainNavItem,
                                        shiny::fluidRow(
                                            shiny::column(12,
                                                          shiny::helpText(
                                                              moduleDescription)
                                            )
                                        ),
                                        shiny::fluidRow(
                                            shiny::column(4),
                                            shiny::column(4,
                                                shiny::actionButton(
                                                            paste0(mainNavItem,"_start"),
                                                            label="Start"
                                                          )
                                            ),
                                            shiny::column(4)
                                        )
                )
                )

                #)
                #)
            }else{#existPrevious==FALSE : #no previous step
                return(shiny::wellPanel(mainNavItem,
                                        shiny::fluidRow(
                                            shiny::column(12,
                                                          shiny::helpText(moduleDescription)
                                            )
                                        ),
                                        shiny::fluidRow(
                                            shiny::column(12,
                                                          shiny::helpText(
                                                "No Previous Step Detected, please
                                                do the steps before this")
                                            )
                                        )
                )
                )
                #)
            }
        }
    } else { ##modules that don't depend on previous
        return(shiny::wellPanel(mainNavItem,
                                shiny::fluidRow(
                                    shiny::column(12,
                                        shiny::helpText(
                                                    moduleDescription)
                                    )
                                ),
                                shiny::fluidRow(
                                    shiny::column(4),
                                    shiny::column(4,
                                                  shiny::actionButton(
                                                      paste0(mainNavItem,"_start"),
                                                      label="Start"
                                                  )
                                    ),
                                    shiny::column(4)
                                )
        )
        )
    }

}

#' moduleStartPanel2
#'
#' @return
#' @keywords internal
#'
#' @examples
moduleStartPanel2<-function(){
    return(
        shiny::wellPanel("do you want to start this Step?",
                         shiny::fluidRow(
                             shiny::column(3),
                             shiny::column(3,
                                           shiny::actionButton("startModule", label="Yes")
                             ),
                             shiny::column(3,
                                shiny::actionButton("returnToSummary", label="No")
                             ),
                             shiny::column(3)
                         )

        )
    )
}

#' moduleRestartPanel
#'
#' @return
#' @keywords internal
#'
#' @examples
moduleRestartPanel<-function(addid=""){
    return(
        shiny::wellPanel(paste0(addid,"do you want to restart analysis from this point?"),
            shiny::fluidRow(
                shiny::column(3),
                shiny::column(3,
                    shiny::actionButton("startModule", label="Yes")
                            ),
                shiny::column(3,
                    shiny::actionButton("returnToSummary", label="No")
                            ),
                shiny::column(3)
            )

                )
    )
}

#' moduleBranchPanel
#'
#' @return
#' @keywords internal
#'
#' @examples
moduleBranchPanel<-function(){
    return(
        shiny::wellPanel("do you want to start new analysis from this point?",
            shiny::fluidRow(
                shiny::column(3),
                    shiny::column(3,
                        shiny::actionButton("yesBranch", label="Yes")   #devi generare nel conditionale lo startModule button
                    ),
                    shiny::column(3,
                        shiny::actionButton("returnToSummary", label="No")
                            ),
                    shiny::column(3)
                         ),
            shiny::fluidRow(
                shiny::conditionalPanel(condition="input.yesBranch%2==1",
                                        shiny::fluidRow(
                                            shiny::column(9,
                                                shiny::textInput("branchName",
                                                                label="Branch Name")
                                                ),
                                            shiny::column(3,
                                                shiny::br(),
                                                shiny::actionButton("startModule", label="make")
                                                )
                                            )

                                        )
            )
                        )
    )
}
