options(shiny.maxRequestSize=300*1024^2,stringsAsFactors = FALSE)
shiny::addResourcePath("www", "./www")
#R.home

#require(R6)
#source ('requirement.R')

#===================================my ws===================================================================
#                                                                               myDrop<<-TRUE             #metti myDrop<-FALSE se stai lavorando con cartelle progetto all'interno del main folder di HiCeekR
#                                                                               workingPC<<- 'myPC_cln'      # 1) 'labPC'   2) 'stebbidone'   3)travelmatto  4)homePC   5)homePC_cln  6)labPC_cln  7)myPC_cln
#                        source('1_preliminaryStatements.R')

#==============================================================================
#wk<<-getwd()


#======mainStataments==========================================================
server<-shiny::shinyServer(function(input, output, session , rea, wdDir) {
    #print (session$ns)
    shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade")
    shinyjs::show("app-content")

    if (file.exists("./SysVal.Robj")==FALSE){
        SysVal<-matrix(nrow=1,ncol=1)
        SysVal[1,1]<-TRUE
        rownames(SysVal)<-"restart"
        save(SysVal,file=paste0(getwd(),"/SysVal.Robj"))
        print("sysGenerate")
    }

    if (file.exists("./HCR.config")==TRUE){
        print("preWd")
        workingDir<-as.character(
            paste0(
            (read.table(configFilePath))[2,1],
            (read.table(configFilePath))[1,1])
            )
        print(paste0("working dir:",workingDir))
        print("postWd")
        print("workingDir")
        ####confTable<-matrix(ncol=1,nrow=2)
        ####confTable[1,1]<-workingDir
        #####confTable[2,1]<-projectName
        write.table(workingDir,"HCRtmp.config", col.names=FALSE, row.names=FALSE,
                    quote=FALSE, sep="\t")
    }


    load("./SysVal.Robj")

    rea<-shiny::reactiveValues()

    #rea$volumes<-c(root = "/")
    #rea$volumes<-c(root = getVolumes)

    #rea$volumes<-shinyFiles::getVolumes()
    rea$volumes<-c(root=getwd()) #shinyFiles::getVolumes()
    rea$rStart<-as.logical(SysVal["restart",1])

    print (isolate(rea$rStart))

    if (isolate(rea$rStart==TRUE)){
        SysVal["restart",1]<-"FALSE"
        save(SysVal,file="./SysVal.Robj")
        session$reload()

     }
    restart<-FALSE



    shiny::onStop(function(){
        if (file.exists(paste0(getwd(),"/www/fragments.jpeg"))==TRUE){
            file.remove(paste0(getwd(),"/www/fragments.jpeg"))
        }

        if (file.exists(paste0(getwd(),"/www/inOutWard.jpeg"))==TRUE){
            file.remove(paste0(getwd(),"/www/inOutWard.jpeg"))
        }

        if (file.exists(paste0(getwd(),"/www/fragments_trimmed.jpeg"))==TRUE){
            file.remove(paste0(getwd(),"/www/fragments_trimmed.jpeg"))
        }

        if (file.exists(paste0(getwd(),"/www/inOutWard_trimmed.jpeg"))==TRUE){
            file.remove(paste0(getwd(),"/www/inOutWard_trimmed.jpeg"))
        }

         # if (file.exists("./SysVal.Robj")==TRUE){
         #     load("./SysVal.Robj")
         #     if (SysVal["restart",1]==FALSE){
         #         file.remove("./SysVal.Robj")
         #         print("Sys removed")
         #     }
         # }



    })

    prjManExport<-shiny::reactiveValues()

    shinyFiles::shinyFileChoose(input,
                                "bamFile",
                                root=isolate(rea$volumes),
                                filetypes=c("bam"))

    shinyFiles::shinyDirChoose(input,
                                "wdPath",
                               root=isolate(rea$volumes)
                            )

    shinyFiles::shinyFileChoose(input,
                                "configPath",
                                root=isolate(rea$volumes),
                                filetypes=c("config")
                                )

    shinyFiles::shinyFileChoose(input,
                                "refGenome",
                                root=isolate(rea$volumes),
                                filetypes=c("fa")
                                )

    shiny::observeEvent(input$restart,{
        #rea$runmodState<-"restart"
        print("start ok")
    })


    shiny::observeEvent(input$wdPath,{

        output$dataPh <-shiny::renderText({
            "data path selected"
        })

        #output$wdPathText<-shiny::renderText({
        output$phLoc<-shiny::renderText({
            if (is.atomic(input$wdPath)==FALSE){
            # paste0(
            #         "path selected: ",
            #         shinyDirPath(unlist(input$wdPath))
            # )
                shinyDirPath(unlist(input$wdPath))
                }
            })
    })

    shiny::observeEvent(input$configPath,{
        # output$configTxt<-shiny::renderText({
        #     if (is.atomic(input$configPath)==FALSE){
        #
        #             "file selected: "
        #             #shinyFilesPath(unlist(input$configPath))
        #         }
        # })

        output$dataPh <-shiny::renderText({
            "data path selected"
        })

        #output$configPathText<-shiny::renderText({
        output$phLoc<-shiny::renderText({
            if (is.atomic(input$configPath)==FALSE){
            # paste0(
            #     "file selected: ",
            #     shinyFilesPath(unlist(input$configPath$files))
            #     #shinyFilesPath(unlist(input$configPath))
            # )
                shinyFilesPath(unlist(input$configPath$files))
                }
        })
    })

    shiny::observeEvent(input$bamFile,{
        output$bamPathShow<-shiny::renderText({
            if (is.atomic(input$bamFile)==FALSE){
            paste0("file selected:", rea$volumes,
                   shinyFilesPath(unlist(input$bamFile$files)))
            }
        })
    })

    shiny::observeEvent(input$refGenome,{
        output$textReference<-shiny::renderText({
            if (is.atomic(input$refGenome)==FALSE){
                paste0("file selected:", rea$volumes,
                       shinyFilesPath(unlist(input$refGenome$files)))
            }

        })
    })

    output$welcomeSlot<-shiny::renderUI({
        prjManPanel_welcome()
    })



if (file.exists("HCRtmp.config")==TRUE){
    #print("temp exist")
    #print(getwd())
    output$configFileText<-shiny::renderText({""})
    #ttCfg<<-read.table('HCRtmp.config')
    #rea$workingDir<-as.character((read.table('HCRtmp.config'))[1,1])
    # rea$workingDir<-as.character(paste0((read.table('HCRtmp.config'))[2,1],
    #                                     (read.table('HCRtmp.config'))[1,1],
    #                                     "HiCeekRwd/"))
    rea$workingDir<-as.character(paste0(
                                        (read.table('HCRtmp.config'))[1,1],
                                        "HiCeekRwd/"))

    #print (paste0("baseDir:", (read.table('HCRtmp.config'))[2,1]))
    print (paste0("prjDir:", (read.table('HCRtmp.config'))[1,1]))
    print (paste0("wkDir: ", isolate(rea$workingDir)))

    #prjName<-as.character((read.table('HCRtmp.config'))[2,1])

    file.remove("HCRtmp.config")

    # refGenomeDir<-paste0(isolate(rea$workingDir), "RefGenomes/")
    # print(paste0("referenceGenomes:", refGenomeDir))
    # output$refgenSlot <- renderUI({
    #     selectFile(
    #         id="refGenome",
    #         path = refGenomeDir,
    #         label= "reference"
    #     )
    # })

    output$prjManagerSlot<-shiny::renderUI({
        prjManagerMainUI()
    })
    output$pathViewer<-shiny::renderDataTable({
        folderFrame(paste0(rea$workingDir, "Projects/"))
    })
} else {

    rea$config<-"none"
    output$configFileText<-shiny::renderText({"no config file found, please make a config file with panel below"})
    #print(isolate(rea$config))

}


shiny::observeEvent(input$loadOrNewConf, {

    if (input$loadOrNewConf=="new"){
        output$configHelper<-shiny::renderText({
            paste0("create new config file in: ",getwd())
        })
    } else {#input$loadOrNewConf=="load"
        output$configHelper<-shiny::renderText({
            "select existent HCR.config"
        })
        }

})


shiny::observeEvent(input$showBut,{
    output$navSlot<-shiny::renderUI({
        mainNav()
    })
})

shiny::observeEvent(input$resetBut,{
    #output$navSlot<-shiny::renderUI({})
    print ("reset")
    session$reload()
})


#======prjManager =============================================================


#rea$resetVal<-"noReset"

#rea$volumes <- c("R Installation" = "")
#print(paste0("isolate", isolate(rea$volumes)))





output$prjState<-shiny::renderText({
    "Select Project"
})

output$selector<-shiny::renderUI({
    prjManPanel_prjSelector()
})


shiny::observeEvent(input$loadNewPrj,{
    #pointTest<<-pointin(rea$workingDir,
    #        "Projects")
    if (input$loadNewPrj == "load"){
        output$prjName<-shiny::renderUI({
            prjManPanel_loadProject(rea$workingDir)
        })
    } else { #input$loadNewPrj == "new"
        output$prjName<-shiny::renderUI({
            prjManPanel_newProject()
        })
    }

})


shiny::observeEvent(input$loadNewAn,{
    if(input$loadNewAn == "new"){
        output$anNames<-shiny::renderUI({
            shiny::textInput("anNewName", label=h5("Analysis"))
        })

        output$anOptionsSlot2<-shiny::renderUI({
            prjManPanel_anlysisInputType()
        })

    } else {#input$loadNewAn == "load"
        output$anNames<-shiny::renderUI({
            shiny::selectInput("anLoadNames", label=h5("Analysis"),
                               choices=c(folderList(rea$prjDir, ecception=TRUE, ecceptionName="ProjectData"))
            )
        })
        output$anExecuteBut<-shiny::renderUI({
            shiny::actionButton("anButtonLoad", label="load")
        })

        output$anOptionsSlot2<-shiny::renderUI({})
        output$anOptionsSlot3<-shiny::renderUI({})
    }
    output$pathViewer<-shiny::renderDataTable({
        folderFrame(rea$prjDir)
    })
})



            #=======prjMan actionButton========================================

shiny::observeEvent(input$makeConfig,{

    busyIndServer("makeConfig",{
        #workingDir<-as.character((read.table('HCRtmp.config'))[1,1])

        #============================================
        makeConfig(shinyDirPath(unlist(input$wdPath)))
        #============================================

        rea$workingDir<-as.character(paste0((read.table('HCR.config'))[2,1],
                                            (read.table('HCR.config'))[1,1],
                                            "HiCeekRwd/"))

        # dir.create(paste0((read.table('HCR.config'))[2,1],
        #                   (read.table('HCR.config'))[1,1],
        #                   "HiCeekRwd/"),
        #             showWarnings = TRUE
        #            )

        #dir.create(paste0(rea$working))
        makeHCRwd(path=paste0((read.table('HCR.config'))[2,1],
                              (read.table('HCR.config'))[1,1]
                              ))

        output$configFileText<-renderText({""})

        print (paste0("wkDir: ", rea$workingDir))
        #prjName<-as.character((read.table('HCR.config'))[2,1])

        output$prjManagerSlot<-shiny::renderUI({
            prjManagerMainUI()
        })
        output$pathViewer<-shiny::renderDataTable({
            folderFrame(paste0(rea$workingDir, "Projects/"))
        })
    })


})

shiny::observeEvent(input$loadConfig,{
    busyIndServer("loadConfig",{
        #file.copy(from=shinyFilesPath(input$configPath),to=paste0(getwd(),"/HCR.config"))
        print("config copy")
        #print(paste0("from: ",shinyFilesPath(unlist(input$configPath$files))))
        # print(paste0("from: ",
        #              shinyFiles::getVolumes()()["Computer"],
        #              shinyFiles::getVolumes()()[Sys.info()["user"]],
        #              shinyFilesPath(unlist(input$configPath$files))))
        # flPath<-paste0(shinyFiles::getVolumes()()["Computer"],
        #                 shinyFiles::getVolumes()()[Sys.info()["user"]],
        #                 shinyFilesPath(unlist(input$configPath$files)))

        setwd((read.table(shinyFilesPath(unlist(input$configPath$files))))[2,1])



        print(paste0("to: ", paste0(getwd(),"/HCR.config")))
        file.copy(from=shinyFilesPath(unlist(input$configPath$files)),to=paste0(getwd(),"/HCR.config"))
        #unlist(input$configPath$files)
        output$configFileText<-shiny::renderText({""})

        #rea$workingDir<-as.character(paste0((read.table('HCR.config'))[1,1],"HiCeekRwd/"))

        rea$workingDir<-as.character(paste0((read.table('HCR.config'))[2,1],
                                            (read.table('HCR.config'))[1,1],
                                            "HiCeekRwd/"))
        print(paste0("wkDir: ", rea$workingDir))
        #prjName<-as.character((read.table('HCR.config'))[2,1])
        prjName<-"NONE" #<- va definito dopo

        output$prjManagerSlot<-shiny::renderUI({
            prjManagerMainUI()
        })
        output$pathViewer<-shiny::renderDataTable({
            folderFrame(paste0(rea$workingDir, "Projects/"))
        })

    })
})

shiny::observeEvent(input$resetPrj,{


    output$prjState<-shiny::renderText({
        "Select Project"
    })

    output$selector<-shiny::renderUI({prjManPanel_prjSelector()})
    if (input$loadNewPrj == "load"){
        output$prjName<-shiny::renderUI({prjManPanel_loadProject()})
    }else{#input$loadNewPrj == "new"
        output$prjName<-shiny::renderUI({prjManPanel_newProject()})
    }
    output$pathViewer<-shiny::renderDataTable({
        folderFrame(paste0(rea$workingDir, "Projects/"))
    })
    output$anSetted<-shiny::renderUI({})

    output$anSettings<-shiny::renderUI({})
    session$reload()
})

shiny::observeEvent(input$prjButton,{

    refGenomeDir<-paste0(isolate(rea$workingDir), "RefGenomes/")
    print(paste0("referenceGenomes:", refGenomeDir))
    output$refgenSlot <- shiny::renderUI({
        selectFile(
            id="refGenome",
            path = refGenomeDir,
            label= h5("reference")
        )
    })

    busyIndServer("prjButton",{
        if (input$loadNewPrj == "new"){
            if (input$prjNewName == ""){

                stop("please give a name to project")
                # if (input$loadNewPrj == "error"){
                #     stop("please give a name to project")
                # }

            } else {
                makeHCRprj(input$prjNewName, paste0(rea$workingDir,"Projects/"))
                prjManExport$prjFolder <- paste0(rea$workingDir,"Projects/",
                                                 input$prjNewName, "/"
                )
                pName<-input$prjNewName

                print(prjManExport$prjFolder)
            }

        } else { #input$loadNewPrj == "load"
            prjManExport$prjFolder <- paste0(rea$workingDir,"Projects/",
                                             input$prjLoadName, "/"
            )
            pName<-input$prjLoadName
            print(prjManExport$prjFolder)
        }

        output$prjState<-shiny::renderText({
            "Project Selected"
        })



        #rea$anFolder<-paste0(rea$workingDir, "Projects/", pName, "/")
        rea$prjDir<-paste0(rea$workingDir, "Projects/", pName, "/")
        #print(paste0("rea$anFolder=",rea$anFolder))
        output$selector<-shiny::renderUI({})
        output$prjName<-shiny::renderUI({})
        output$anSetted<-shiny::renderUI({prjManPanel_reset(pName)})
        #output$pNameSlot<- shiny::renderText({pName})
        output$anSettings<-shiny::renderUI({prjManPanel_analysisSettings()})

    })

})

shiny::observeEvent(input$newAnalysis,{


    busyIndServer("newAnalysis",{
        if (input$loadNewPrj=="new"){
            prjName<-input$prjNewName
            print(paste0("prjName:    ",input$prjNewName))
        } else {#input$loadNewPrj=="load"
            prjName<-input$prjLoadName
        }

        rea$anDir <- paste0(rea$prjDir,input$anNewName,"/")
        print("rea$anDir....ok")

        if (input$inputType == "BAM"){

            it<-matrix(ncol=2, nrow=5)
            #itTT<<-it
            # print(paste0("inputpath:    ",
            #                 #parseFilePaths(rea$volumes, input$bamFile)
            #                 shinyFilesPath(input$bamFile)
            #                 ))
            #parseFilePaths(roots, input$bamFile)
            itNames<-c("InputPath", "Resolution", "Type",
                       "ProjectName", "AnalysisName","comment")
            #print(shinyFilesPath(input$bamFile))
            #print(shinyFilesPath(input$refGenome))
            #print(shinyFilesPath(input$refGenome, last=TRUE))
            #print(unlist(input$bamFile$files))
            itValues<-c(shinyFilesPath(paste0(
                rea$volumes,
                unlist(input$bamFile$files))) ,
                input$binSize,
                        input$inputType, prjName, input$anNewName,
                        paste0("Reference Genome:", rea$volumes,
                               shinyFilesPath(unlist(input$refGenome$files), last=TRUE)
                               )
                        )
            #itValuesTT<<-itValues
            it<-data.frame(itNames,itValues)
            it<-as.matrix(it)
            #itTT<<-it
            print ("it.....ok")

            if (input$anNewName==""){
                stop("no name for new analysis")
            } else {
                makeHCRan(input$anNewName, prjManExport$prjFolder, infoTable=it)
            }

            print ("analysis created !!!")
            aN<-input$anNewName
            output$pNameSlot<- shiny::renderText({aN})

#============================================================================

            print("before Param")
            print(unlist(input$refGenome$files))
            param<-refGenomeEdit(refGenome = paste0(rea$volumes,
                                                    shinyFilesPath(unlist(input$refGenome$files)
                                                                   )
                                                    ),
                                enzyme = input$cutSite,
                                overhang = input$overhang,
                                name = "refGenFrag",
                                cutGenomeOutDir = pointin(rea$anDir,
                                                             "Pre-Processing",
                                                            sys=TRUE)
            )

            print("param obtained")

            reportTable<-bamMatching(
                bam.datapath=paste0(rea$volumes,shinyFilesPath(#paste0(getwd(),
                    unlist(input$bamFile$files)
                    #)
                    )),
                param=param,
                h5PathOut=pointin(rea$anDir,
                                  "Pre-Processing",
                                  sys=TRUE
                )
            )

            #fagli salvare al reportTable

            reportTable<-unlist(reportTable)
            write.table(reportTable, paste0(pointin(rea$anDir,
                                            "Pre-Processing", sys=TRUE),
                                            "reportTable.tsv"),
                        sep="\t",
                        quote=FALSE,
                        col.names=FALSE
                        )

            print("bamMatching executed")


            #print("beforePlot execution")
            rFrags<- read.table(paste0(pointin(rea$anDir,"Pre-Processing", sys = TRUE),
                                "refGenFrag.cutGenome.tsv"), sep='\t', header=TRUE)
            #print("beforePlot execution 1")
            rFragsGrange<-GenomicRanges::makeGRangesFromDataFrame(rFrags)
            #print("beforePlot execution 2")
            paramFil <- diffHic::pairParam(rFragsGrange)
            #print("beforePlot execution 3")
            diags <- diffHic::getPairData(paste0(pointin(rea$anDir,"Pre-Processing", sys = TRUE),
                                                 "h5file.h5"),
                                            paramFil)
            #print("beforePlot execution 4")
            save(diags, file=paste0(pointin(rea$anDir,"Pre-Processing", sys = TRUE),"diags.Robj"))
            #print("beforePlot completed")


            #=================================================================

            output$navSlot<-shiny::renderUI({
                mainNav(inType="BAM")
            })

            output$prjState<-shiny::renderText({
                "Analysis Selected"
            })

            output$prjManagerSlot<-shiny::renderUI({prjManPanel_reset(input$anNewName)})
            ##devi creare uno specifico panel per il reset dall'analisi
            #output$prjManagerSlot<-shiny::renderUI({})
            output$welcomeSlot<-shiny::renderUI({})
            output$pathViewer<-shiny::renderUI({})

            #======================================esporta plot
            rea$diags<-diags
            rea$llinsert <- log2(rea$diags$insert + 1L)
            rea$intra <- !is.na(rea$llinsert)
            rea$breaks <- seq(min(rea$llinsert[rea$intra]),
                                 max(rea$llinsert[rea$intra]), length.out=30)
            rea$inward <- hist(rea$llinsert[rea$diags$orientation==1L],
                                  plot=FALSE, breaks=rea$breaks)
            rea$outward <- hist(rea$llinsert[rea$diags$orientation==2L] ,
                                   plot=FALSE, breaks=rea$breaks)
            rea$samestr <- hist(rea$llinsert[rea$diags$orientation==0L | rea$diags$orientation==3L],
                                   plot=FALSE, breaks=rea$breaks)
            rea$samestr$counts <- rea$samestr$counts/2
            # Setting up the axis limits.
            rea$ymax <- max(rea$inward$counts, rea$outward$counts,
                               rea$samestr$counts)/1e6
            rea$xmax <- max(rea$inward$mids, rea$outward$mids,
                               rea$samestr$mids)
            rea$xmin <- min(rea$inward$mids, rea$outward$mids,
                               rea$samestr$mids)

            ##=======================================================================

            jpeg(file=paste0(pointin(rea$anDir, "Pre-Processing", sys=TRUE),"fragments.jpeg"))
            hist(rea$diags$length[diags$length < 1000], ylab="Frequency",
                 xlab="Spacing (bp)", main="", col="grey80")
            dev.off()

            jpeg(file=paste0(pointin(rea$anDir, "Pre-Processing", sys=TRUE),"inOutWard.jpeg"))
            plot(0,0,type="n", xlim=c(rea$xmin, rea$xmax), ylim=c(0, rea$ymax),
                 xlab=expression(log[2]~"[insert size (bp)]"), ylab="Frequency (millions)")
            lines(rea$inward$mids, rea$inward$counts/1e6, col="darkgreen", lwd=2)
            #abline(v=log2(rea$min.inward), col="darkgrey")
            lines(rea$outward$mids, rea$outward$counts/1e6, col="red", lwd=2)
            #abline(v=log2(rea$min.outward), col="darkgrey", lty=2)
            lines(rea$samestr$mids, rea$samestr$counts/1e6, col="blue", lwd=2)
            legend("topright", c("inward", "outward", "same"),
                   col=c("darkgreen", "red", "blue"), lwd=2)
            dev.off()

            #=================================================


        } else {

            if (input$inputType == "Matrix"){

            }

        }



    })

})

#====================================================Initizialize Module Selection Panels

shiny::observeEvent(input$anButtonLoad,{

    rea$anDir <- paste0(rea$prjDir,input$anLoadNames,"/")

    #==========================================================================

    output$navSlot<-shiny::renderUI({
        mainNav(inType="BAM")
    })

    output$prjState<-shiny::renderText({
        "Analysis Selected"
    })

    output$prjManagerSlot<-shiny::renderUI({prjManPanel_reset(input$anLoadNames)})
    ##devi creare uno specifico panel per il reset dall'analisi
    #output$prjManagerSlot<-shiny::renderUI({})
    output$welcomeSlot<-shiny::renderUI({})
    output$pathViewer<-shiny::renderUI({})
    aN<-input$anNewName
    output$pNameSlot<- shiny::renderText({aN})
    print(input$anLoadNames)
    print("analysis start")




})


shiny::observeEvent(input$returnToSummary,{

    shiny::updateTabsetPanel(session, "mainNav", selected="Summary")

    #output$moduleScreen<-shiny::renderUI({})
    tool_DetectInteraction<-"sum"
    moduleUI_DetectInteraction<- "sum_UI"
    moduleLoad_DetectInteraction<- "sum_Server"

    output$summarySlot <- shiny::renderUI ({
        get(moduleUI_DetectInteraction) (tool_DetectInteraction,label='')
    })

    loadApp_DetectInteraction <- shiny::callModule(
        get(moduleLoad_DetectInteraction)
        , tool_DetectInteraction,
        stringsAsFactors = FALSE,
        wdPath = rea$anDir
    )
})

#======================================================

shiny::observeEvent(input$Filtering_reStart ,{
    #salva nelle reactiveValues lo stato restart
    #output$moduleScreen<-shiny::renderUI({})



    print ("Filtering Restart")
    rea$modset<-"Filtering"
    rea$runmodState<-"restart"
    output$moduleScreen<-shiny::renderUI({
        moduleRestartPanel()
        # shiny::wellPanel("do you want to restart analysis from this point?2",
        #                  shiny::fluidRow(
        #                      shiny::column(3),
        #                      shiny::column(3,
        #                                    shiny::actionButton("startModule2", label="Yes2")
        #                      ),
        #                      shiny::column(3,
        #                                    shiny::actionButton("returnToSummary2", label="No2s")
        #                      ),
        #                      shiny::column(3)
        #                  )
        # )
    })
})
#========================================================


# shiny::observeEvent(input$restart,{
#     rea$runmodState<-"restart"
#     print("start ok")
# })


shiny::observeEvent(input$Filtering_branch,{
    #salva nella reactiveValues lo stato branch
    #output$moduleScreen<-shiny::renderUI({})
    rea$modset<-"Filtering"
    rea$runmodState<-"branch"
    output$moduleScreen<-shiny::renderUI({
        moduleBranchPanel()
    })
})

shiny::observeEvent(input$Filtering_start,{
    #output$moduleScreen<-shiny::renderUI({})
    rea$runmodState<-"start"
    output$moduleScreen<-shiny::renderUI({
        moduleStartPanel2()
    })
})

shiny::observeEvent(input$Binning_reStart ,{

    output$moduleScreen<-shiny::renderUI({})
    #salva nelle reactiveValues lo stato restart
    print ("binning Restart")
    rea$modset<-"Binning"
    rea$runmodState<-"restart"
    output$moduleScreen<-shiny::renderUI({
        moduleRestartPanel()
        # shiny::wellPanel("do you want to restart analysis from this point?",
        #                  shiny::fluidRow(
        #                      shiny::column(3),
        #                      shiny::column(3,
        #                                    shiny::actionButton("startModule", label="Yes")
        #                      ),
        #                      shiny::column(3,
        #                                    shiny::actionButton("returnToSummary", label="No")
        #                      ),
        #                      shiny::column(3)
        #                  )
        #                 )

    })
})

shiny::observeEvent(input$Binning_branch,{
    #salva nella reactiveValues lo stato branch
    #output$moduleScreen<-shiny::renderUI({})
    rea$modset<-"Binning"
    rea$runmodState<-"branch"
    output$moduleScreen<-shiny::renderUI({
        moduleBranchPanel()
    })
})

shiny::observeEvent(input$Binning_start,{
    #salva nella reactiveValues lo stato branch
    #output$moduleScreen<-shiny::renderUI({})
    rea$runmodState<-"start"
    output$moduleScreen<-shiny::renderUI({
        moduleStartPanel2()
    })
})

shiny::observeEvent(input$iterative_reStart ,{
    #salva nelle reactiveValues lo stato restart
    #output$moduleScreen<-shiny::renderUI({})
    rea$modset<-"Normalization"
    rea$runmodState<-"restart"
    output$moduleScreen<-shiny::renderUI({
        moduleRestartPanel("Normalization")
    })
})

shiny::observeEvent(input$iterative_branch,{
    #salva nella reactiveValues lo stato branch
    #output$moduleScreen<-shiny::renderUI({})
    rea$modset<-"Normalization"
    rea$runmodState<-"branch"
    output$moduleScreen<-shiny::renderUI({
        moduleBranchPanel()
    })
})

shiny::observeEvent(input$iterative_start,{
    #salva nella reactiveValues lo stato branch
    #output$moduleScreen<-shiny::renderUI({})
    rea$runmodState<-"start"
    output$moduleScreen<-shiny::renderUI({
        moduleStartPanel2()
    })
})

shiny::observeEvent(input$WavSis_reStart ,{
    #salva nelle reactiveValues lo stato restart
    #output$moduleScreen<-shiny::renderUI({})
    rea$modset<-"Normalization"
    rea$runmodState<-"restart"
    output$moduleScreen<-shiny::renderUI({
    moduleRestartPanel("Normalization")
    })
})

shiny::observeEvent(input$WavSis_branch,{
    #salva nella reactiveValues lo stato branch
    #output$moduleScreen<-shiny::renderUI({})
    rea$modset<-"Normalization"
    rea$runmodState<-"branch"
    output$moduleScreen<-shiny::renderUI({
        moduleBranchPanel()
    })
})

shiny::observeEvent(input$WavSis_start,{
    #salva nella reactiveValues lo stato branch
    #output$moduleScreen<-shiny::renderUI({})
    rea$runmodState<-"start"
    output$moduleScreen<-shiny::renderUI({
        moduleStartPanel2()
    })
})

shiny::observeEvent(input$TADsHMM_reStart,{
    #output$moduleScreen<-shiny::renderUI({})
    rea$modset<-"Downstream"
    rea$runmodState<-"restart"
    output$moduleScreen<-shiny::renderUI({
        moduleRestartPanel("Downstream")
    })
})

shiny::observeEvent(input$TADsHMM_branch,{
    #output$moduleScreen<-shiny::renderUI({})
    rea$modset<-"Downstream"
    rea$runmodState<-"branch"
    output$moduleScreen<-shiny::renderUI({
        moduleBranchPanel()
    })
})

shiny::observeEvent(input$TADsHMM_start,{
    #output$moduleScreen<-shiny::renderUI({})
    rea$runmodState<-"start"
    output$moduleScreen<-shiny::renderUI({
        moduleStartPanel2()
    })
})

shiny::observeEvent(input$CompartmentsPCA_start,{
    #output$moduleScreen<-shiny::renderUI({})
    rea$runmodState<-"start"
    output$moduleScreen<-shiny::renderUI({
        moduleStartPanel2()
    })
})

shiny::observeEvent(input$EpigeneticFeatures_start,{
    #output$moduleScreen<-shiny::renderUI({})
    rea$runmodState<-"start"
    output$moduleScreen<-shiny::renderUI({
        moduleStartPanel2()
    })
})

shiny::observeEvent(input$Heatmaps_start,{
    rea$runmodState<-"start"
    output$moduleScreen<-shiny::renderUI({
        moduleStartPanel2()
    })
})

shiny::observeEvent(input$Networks_start,{
    rea$runmodState<-"start"
    output$moduleScreen<-shiny::renderUI({
        moduleStartPanel2()
    })
})


##======WELCOME================================================================

    # tool_prjSettings<-'prjSettings'
    # moduleUI_prjSettings<-paste0 (tool_prjSettings, '_UI')
    # moduleLoad_prjSettings<-paste0 (tool_prjSettings,'_Server')
    # output$prjSettingsSlot<-shiny::renderUI ({
    # get(moduleUI_prjSettings)(tool_prjSettings,label='prjSettings')
    # })
    # loadApp_prjSettings<-shiny::callModule(get(moduleLoad_prjSettings),
    #                                        tool_prjSettings)
    #

    ##ogni nuovo modulo da aggiungere devi aggiungere un pezzo
    ##nell'observe della mainNav e una altro pezzo nell'observe dello startModule
    ##e devi mettere gli observe dei bottini start restart e branch
    ##MODULE STARTPANELS
    shiny::observeEvent (input$mainNav,{

        if (input$mainNav == 'Summary' & file.exists(paste0(rea$anDir, 'info.tsv'))==TRUE ){
            output$moduleScreen<-shiny::renderUI({})
            output$selectPn<-shiny::renderUI({})
            tool_DetectInteraction<-"sum"
            moduleUI_DetectInteraction<- "sum_UI"
            moduleLoad_DetectInteraction<- "sum_Server"

            output$summarySlot <- shiny::renderUI ({
                get(moduleUI_DetectInteraction) (tool_DetectInteraction,label='')
            })

            loadApp_DetectInteraction <- shiny::callModule(
                get(moduleLoad_DetectInteraction)
                , tool_DetectInteraction,
                stringsAsFactors = FALSE,
                wdPath = rea$anDir
            )
        }

        if (input$mainNav == 'Filtering' &
            file.exists(paste0(rea$anDir, 'info.tsv'))==TRUE ){
            print("Filtering tab")
            output$selectPn<-shiny::renderUI({})
            output$moduleScreen<-shiny::renderUI({})
            # if (exists("rea$moduleSet")==TRUE){
            #     print(paste0("moduleSet:",rea$moduleSet))
            # }else{
            #     print("moduleSet non difined")
            # }
            #print(paste0("moduleSet:",rea$moduleSet))
            #output$moduleScreen<-renderUI({})

            #output$filteringSlot<-renderUI({moduleStartPanel("Filtering",
            output$selectPn<-shiny::renderUI({moduleStartPanel("Filtering",
                                                    rea$anDir,
                                                    callModuleDescription(
                                                        "Filtering")
                                                    )
                                    })
        }

        if (input$mainNav == 'Binning' &
            file.exists(paste0(rea$anDir, 'info.tsv'))==TRUE ){
            print("Binning tab")
            output$selectPn<-shiny::renderUI({})
            output$moduleScreen<-shiny::renderUI({})
            # if (exists("rea$moduleSet")==TRUE){
            #     print(paste0("moduleSet:",rea$moduleSet))
            # }else{
            #     print("moduleSet non difined")
            # }
            #print(paste0("moduleSet:",rea$moduleSet))
            #output$moduleScreen<-renderUI({})
            #output$binningSlot<-renderUI({moduleStartPanel("Binning",
            output$selectPn<-renderUI({moduleStartPanel("Binning",
                                                             rea$anDir,
                                                             callModuleDescription(
                                                                 "Binning")
            )
            })
        }

        if (input$mainNav == 'iterative' &
            file.exists(paste0(rea$anDir, 'info.tsv'))==TRUE ){
            #print("Filtering tab")
            output$selectPn<-shiny::renderUI({})
            output$moduleScreen<-shiny::renderUI({})
            #output$iterativeSlot<-renderUI({moduleStartPanel("iterative",
            output$selectPn<-shiny::renderUI({moduleStartPanel("iterative",
                                                        rea$anDir,
                                                        callModuleDescription(
                                                        "iterative")
            )
            })
        }

        if (input$mainNav == 'WavSis' &
            file.exists(paste0(rea$anDir, 'info.tsv'))==TRUE ){
            output$moduleScreen<-shiny::renderUI({})
            output$selectPn<-shiny::renderUI({})
            #output$WavSisSlot<-renderUI({moduleStartPanel("WavSis",
            output$selectPn<-shiny::renderUI({moduleStartPanel("WavSis",
                                                             rea$anDir,
                                                             callModuleDescription(
                                                                 "WavSis")
            )
            })
        }

        if (input$mainNav == 'TADsHMM' &
            file.exists(paste0(rea$anDir, 'info.tsv'))==TRUE ){
            output$moduleScreen<-shiny::renderUI({})
            output$selectPn<-shiny::renderUI({})
            #output$hmmSlot<-renderUI({moduleStartPanel("TADsHMM",
            output$selectPn<-shiny::renderUI({moduleStartPanel("TADsHMM",
                                                        rea$anDir,
                                                        callModuleDescription(
                                                            "TADsHMM")
            )
            })
        }

        if (input$mainNav == 'CompartmentsPCA' &
            file.exists(paste0(rea$anDir, 'info.tsv'))==TRUE ){
            output$moduleScreen<-shiny::renderUI({})
            output$selectPn<-shiny::renderUI({})
            #output$pcaCompSlot<-shiny::renderUI({moduleStartPanel("CompartmentsPCA",
            output$selectPn<-shiny::renderUI({moduleStartPanel("CompartmentsPCA",
                                                        rea$anDir,
                                                        callModuleDescription(
                                                            "CompartmentsPCA")
            )
            })
        }

        if (input$mainNav == 'PCA' &
            file.exists(paste0(rea$anDir, 'info.tsv'))==TRUE ){
            output$moduleScreen<-shiny::renderUI({})
            output$selectPn<-shiny::renderUI({})
            #output$pcaSlot<-shiny::renderUI({moduleStartPanel("EpigeneticFeatures",
            output$selectPn<-shiny::renderUI({moduleStartPanel("EpigeneticFeatures",
                                                                rea$anDir,
                                                                callModuleDescription(
                                                                "EpigeneticFeatures")
            )
            })
        }

        if (input$mainNav== 'Heatmap' &
            file.exists(paste0(rea$anDir, 'info.tsv'))==TRUE ){

            output$moduleScreen<-shiny::renderUI({})
            output$selectPn<-shiny::renderUI({})
            #output$pcaSlot<-shiny::renderUI({moduleStartPanel("EpigeneticFeatures",
            output$selectPn<-shiny::renderUI({moduleStartPanel("Heatmaps",
                                                           rea$anDir,
                                                           callModuleDescription(
                                                            "Heatmaps")
        )
        })
        }

        if (input$mainNav== 'Networks' &
            file.exists(paste0(rea$anDir, 'info.tsv'))==TRUE ){

            output$moduleScreen<-shiny::renderUI({})
            output$selectPn<-shiny::renderUI({})

            output$selectPn<-shiny::renderUI({moduleStartPanel("Networks",
                                                                rea$anDir,
                                                                callModuleDescription(
                                                                "Networks")
            )
            })
        }


    })

#==============================================================================
#startModule pressend condition================================================
#
#===========Initizialize Modules===============================================
#==============================================================================

    shiny::observeEvent (#input$mainNav,
                         input$startModule
                         ,
                        {
        #se questo observe Lo metti su startModule e non su mainNav
        # shiny::observeEvent(input$startModule,{
        #
        # })

        # if (input$mainNav == 'Summary' & file.exists(paste0(rea$anDir, 'info.tsv'))==TRUE ){
        #     tool_DetectInteraction<-"sum"
        #     moduleUI_DetectInteraction<- "sum_UI"
        #     moduleLoad_DetectInteraction<- "sum_Server"
        #
        #     output$summarySlot <- shiny::renderUI ({
        #         get(moduleUI_DetectInteraction) (tool_DetectInteraction,label='')
        #     })
        #
        #     loadApp_DetectInteraction <- shiny::callModule(
        #         get(moduleLoad_DetectInteraction)
        #         , tool_DetectInteraction,
        #         stringsAsFactors = FALSE,
        #         wdPath = rea$anDir
        #     )
        # }

        output$selectPn<-renderUI({})

        if (rea$runmodState=="branch"){
            print("branch pressed")
            makeBranch(input$branchName, rea$modset, infoTsv=paste0(rea$anDir, 'info.tsv'))
            session$reload()

            #rea$anDir <- paste0(rea$prjDir,input$branchName,"/")
            #pName<-input$branchName
            #output$anSetted<-shiny::renderUI({prjManPanel_reset(pName)})
            #output$pNameSlot<- shiny::renderText({input$branchName})
            #session$reload()
        }else {
            if (rea$runmodState=="restart"){
                print("restart pressed")
                restartFrom(paste0(rea$anDir, 'info.tsv'),rea$modset)
                #print("restartFrom executed")
                session$reload()
                #print("session end")
            } else{
                #print("render module")
                output$moduleScreen<-shiny::renderUI({})

                #leggi in reactive se lo stato è branch o reStart
                #se branch crea nuovo progetto
                #se reStart cancella fino ad un certo punto e riprendi la là
                #agiorna lo stato nel reactive come "ready"

                withProgress(message="please wait module pre processing", min=0, max=3, {
                    setProgress(message = 'Calculation in progress', detail = 'This may take a while...')
                    if (input$mainNav == 'Filtering' & file.exists(paste0(rea$anDir, 'info.tsv'))==TRUE ){


                        #source ('DiffHiC_DetectInteraction.R')
                        tool_DetectInteraction<-"Filmod"
                        moduleUI_DetectInteraction<- "FilmodUI"
                        moduleLoad_DetectInteraction<- "FilmodServer"
                        setProgress(value = 1)
                        #output$filteringSlot <- shiny::renderUI ({
                        output$selectPn <- shiny::renderUI ({
                            get(moduleUI_DetectInteraction) (tool_DetectInteraction,label='')
                        })
                        setProgress(value = 2)
                        loadApp_DetectInteraction <- shiny::callModule(
                            get(moduleLoad_DetectInteraction)
                            , tool_DetectInteraction,
                            stringsAsFactors = FALSE,
                            wdPath = rea$anDir
                        )
                        setProgress(value = 3)
                    }
                })

                if (input$mainNav == 'Binning'
                    & file.exists(paste0(rea$anDir, 'info.tsv')) == TRUE ){

                    tool_Binning <- 'DiffHiC_BinningV2'

                    moduleUI_Binning <- paste0 (tool_Binning, '_UI')
                    moduleLoad_Binning <- paste0 (tool_Binning,'_Server')
                    # output$binningSlot <-renderUI ({
                    output$selectPn <-renderUI ({
                        get(moduleUI_Binning) (tool_Binning,label='helpingUI')
                    })
                    loadApp_Binning <-shiny::callModule(get(moduleLoad_Binning), tool_Binning,
                                                 stringsAsFactors = FALSE,
                                                 wdPath = rea$anDir
                    )

                }

                if (input$mainNav == 'iterative' &
                    file.exists(paste0(rea$anDir, 'info.tsv'))==TRUE ){

                    tool_DiffHiC_Norm <- 'diffHic_Normalization'
                    moduleUI_DiffHiC_Norm<- paste0 (tool_DiffHiC_Norm, '_UI')
                    moduleLoad_DiffHiC_Norm<- paste0 (tool_DiffHiC_Norm,'_Server')
                    #output$iterativeSlot<-renderUI ({
                    output$selectPn <-shiny::renderUI ({
                        get(moduleUI_DiffHiC_Norm) (tool_DiffHiC_Norm,label='iceUI')
                    })
                    loadApp_DiffHiC_Norm <- shiny::callModule(get(moduleLoad_DiffHiC_Norm),
                                                       tool_DiffHiC_Norm,
                                                       stringsAsFactors = FALSE,
                                                       wdPath = rea$anDir
                    )
                }

                if (input$mainNav == 'WavSis' &
                    file.exists(paste0(rea$anDir, 'info.tsv'))==TRUE ){

                    tool_chromoR <- 'chromoR_Normalization'
                    moduleUI_chromoR<- paste0 (tool_chromoR, '_UI')
                    moduleLoad_chromoR<- paste0 (tool_chromoR,'_Server')
                    #output$WavSisSlot<- shiny::renderUI ({
                    output$selectPn <-shiny::renderUI ({
                        get(moduleUI_chromoR) (tool_chromoR,label='helpingUI')
                    })
                    loadApp_chromoR <-shiny::callModule(get(moduleLoad_chromoR),
                                                        tool_chromoR,
                                                        stringsAsFactors = FALSE,
                                                        wdPath = rea$anDir
                    )
                }

                if (input$mainNav == 'TADsHMM' &
                    file.exists(paste0(rea$anDir, 'info.tsv'))==TRUE ){

                    tool_hmm <- 'HMM_postProcessing'
                    moduleUI_hmm<- paste0 (tool_hmm, '_UI')
                    moduleLoad_hmm<- paste0 (tool_hmm,'_Server')
                    #output$hmmSlot<-renderUI ({
                    output$selectPn <-shiny::renderUI ({
                        get(moduleUI_hmm) (tool_hmm,label='helpingUI')
                    })
                    loadApp_hmm <-  shiny::callModule(get(moduleLoad_hmm), tool_hmm,
                                               stringsAsFactors = FALSE,
                                               wdPath=rea$anDir
                    )
                }

                if (input$mainNav == 'CompartmentsPCA' & file.exists(paste0(
                    rea$anDir, 'info.tsv'))==TRUE ){
                    #source ('pcaComp_postProcessing.R')
                    tool_pcaComp <- 'pcaComp_postProcessing'
                    moduleUI_pcaComp<- paste0 (tool_pcaComp, '_UI')
                    moduleLoad_pcaComp<- paste0 (tool_pcaComp,'_Server')
                    #output$pcaCompSlot<- shiny::renderUI({
                    output$selectPn <-shiny::renderUI ({
                        get(moduleUI_pcaComp) (tool_pcaComp,label='pcaUI')
                    })
                    loadApp_pcaComp <-  shiny::callModule(get(moduleLoad_pcaComp),
                                                          tool_pcaComp,
                                                          stringsAsFactors = FALSE,
                                                          wdPath=rea$anDir)
                }

                if (input$mainNav == 'PCA' &
                    file.exists(paste0(rea$anDir, 'info.tsv'))==TRUE ){
                    #source ('pca_postProcessing.R')
                    tool_pca <- 'pca_postProcessing'
                    moduleUI_pca<- paste0 (tool_pca, '_UI')
                    moduleLoad_pca<- paste0 (tool_pca,'_Server')
                    #output$pcaSlot<-shiny::renderUI ({
                    output$selectPn <-shiny::renderUI ({
                        get(moduleUI_pca) (tool_pca,label='epifUI')
                    })
                    loadApp_pca <- shiny::callModule(get(moduleLoad_pca), tool_pca,
                                                     stringsAsFactors = FALSE,
                                                     wdPath=rea$anDir
                    )
                }

                if (input$mainNav == 'Heatmap' &
                    file.exists(paste0(rea$anDir, 'info.tsv'))==TRUE
                ){
                    tool_htm <- 'TADsV2_Visualization'
                    moduleUI_htm<- paste0 (tool_htm, '_UI')
                    moduleLoad_htm<- paste0 (tool_htm,'_Server')
                    #output$pcaSlot<-shiny::renderUI ({
                    output$selectPn <-shiny::renderUI ({
                        get(moduleUI_htm) (tool_htm,label='htmUI')
                    })
                    loadApp_htm <- shiny::callModule(get(moduleLoad_htm), tool_htm,
                                                     stringsAsFactors = FALSE,
                                                     wdPath=rea$anDir
                    )
                }

                if (input$mainNav == 'Networks' &
                    file.exists(paste0(rea$anDir, 'info.tsv'))==TRUE
                ){
                    tool_ntw <- 'networksV2_Visualization' #networksV2_Visualization_UI
                    moduleUI_ntw<- paste0 (tool_ntw, '_UI')
                    moduleLoad_ntw<- paste0 (tool_ntw,'_Server')
                    #output$pcaSlot<-shiny::renderUI ({
                    output$selectPn <-shiny::renderUI ({
                        get(moduleUI_ntw) (tool_ntw,label='ntwUI')
                    })
                    loadApp_ntw <- shiny::callModule(get(moduleLoad_ntw), tool_ntw,
                                                     stringsAsFactors = FALSE,
                                                     wdPath=rea$anDir
                    )
                }
            }
        }
        # #print("render module")
        # output$moduleScreen<-shiny::renderUI({})
        #
        # #leggi in reactive se lo stato è branch o reStart
        # #se branch crea nuovo progetto
        # #se reStart cancella fino ad un certo punto e riprendi la là
        # #agiorna lo stato nel reactive come "ready"
        #
        # withProgress(message="please wait module pre processing", min=0, max=3, {
        #     setProgress(message = 'Calculation in progress', detail = 'This may take a while...')
        #     if (input$mainNav == 'Filtering' & file.exists(paste0(rea$anDir, 'info.tsv'))==TRUE ){
        #
        #
        #         #source ('DiffHiC_DetectInteraction.R')
        #         tool_DetectInteraction<-"Filmod"
        #         moduleUI_DetectInteraction<- "FilmodUI"
        #         moduleLoad_DetectInteraction<- "FilmodServer"
        #         setProgress(value = 1)
        #         #output$filteringSlot <- shiny::renderUI ({
        #         output$selectPn <- shiny::renderUI ({
        #             get(moduleUI_DetectInteraction) (tool_DetectInteraction,label='')
        #         })
        #         setProgress(value = 2)
        #         loadApp_DetectInteraction <- shiny::callModule(
        #                                         get(moduleLoad_DetectInteraction)
        #                                         , tool_DetectInteraction,
        #                                         stringsAsFactors = FALSE,
        #                                         wdPath = rea$anDir
        #         )
        #         setProgress(value = 3)
        #     }
        # })
        #
        # if (input$mainNav == 'Binning'
        #     & file.exists(paste0(rea$anDir, 'info.tsv')) == TRUE ){
        #
        #     tool_Binning <- 'DiffHiC_BinningV2'
        #
        #     moduleUI_Binning <- paste0 (tool_Binning, '_UI')
        #     moduleLoad_Binning <- paste0 (tool_Binning,'_Server')
        #     # output$binningSlot <-renderUI ({
        #     output$selectPn <-renderUI ({
        #         get(moduleUI_Binning) (tool_Binning,label='helpingUI')
        #     })
        #     loadApp_Binning <-callModule(get(moduleLoad_Binning), tool_Binning,
        #                                 stringsAsFactors = FALSE,
        #                                 wdPath = rea$anDir
        #                                 )
        #
        # }
        #
        # if (input$mainNav == 'iterative' &
        #     file.exists(paste0(rea$anDir, 'info.tsv'))==TRUE ){
        #
        #     tool_DiffHiC_Norm <- 'diffHic_Normalization'
        #     moduleUI_DiffHiC_Norm<- paste0 (tool_DiffHiC_Norm, '_UI')
        #     moduleLoad_DiffHiC_Norm<- paste0 (tool_DiffHiC_Norm,'_Server')
        #     #output$iterativeSlot<-renderUI ({
        #     output$selectPn <-renderUI ({
        #         get(moduleUI_DiffHiC_Norm) (tool_DiffHiC_Norm,label='iceUI')
        #     })
        #     loadApp_DiffHiC_Norm <- callModule(get(moduleLoad_DiffHiC_Norm),
        #                                         tool_DiffHiC_Norm,
        #                                         stringsAsFactors = FALSE,
        #                                         wdPath = rea$anDir
        #                                         )
        # }
        #
        # if (input$mainNav == 'WavSis' &
        #     file.exists(paste0(rea$anDir, 'info.tsv'))==TRUE ){
        #
        #     tool_chromoR <- 'chromoR_Normalization'
        #     moduleUI_chromoR<- paste0 (tool_chromoR, '_UI')
        #     moduleLoad_chromoR<- paste0 (tool_chromoR,'_Server')
        #     #output$WavSisSlot<- shiny::renderUI ({
        #     output$selectPn <-renderUI ({
        #     get(moduleUI_chromoR) (tool_chromoR,label='helpingUI')
        #     })
        #     loadApp_chromoR <-shiny::callModule(get(moduleLoad_chromoR),
        #                                     tool_chromoR,
        #                                     stringsAsFactors = FALSE,
        #                                     wdPath = rea$anDir
        #                                     )
        # }
        #
        # if (input$mainNav == 'TADsHMM' &
        #     file.exists(paste0(rea$anDir, 'info.tsv'))==TRUE ){
        #
        #     tool_hmm <- 'HMM_postProcessing'
        #     moduleUI_hmm<- paste0 (tool_hmm, '_UI')
        #     moduleLoad_hmm<- paste0 (tool_hmm,'_Server')
        #     #output$hmmSlot<-renderUI ({
        #     output$selectPn <-renderUI ({
        #         get(moduleUI_hmm) (tool_hmm,label='helpingUI')
        #     })
        #     loadApp_hmm <-  callModule(get(moduleLoad_hmm), tool_hmm,
        #                                 stringsAsFactors = FALSE,
        #                                 wdPath=rea$anDir
        #                                 )
        # }
        #
        # if (input$mainNav == 'CompartmentsPCA' & file.exists(paste0(
        #     rea$anDir, 'info.tsv'))==TRUE ){
        #     #source ('pcaComp_postProcessing.R')
        #     tool_pcaComp <- 'pcaComp_postProcessing'
        #     moduleUI_pcaComp<- paste0 (tool_pcaComp, '_UI')
        #     moduleLoad_pcaComp<- paste0 (tool_pcaComp,'_Server')
        #     #output$pcaCompSlot<- shiny::renderUI({
        #     output$selectPn <-renderUI ({
        #     get(moduleUI_pcaComp) (tool_pcaComp,label='pcaUI')
        #     })
        #     loadApp_pcaComp <-  shiny::callModule(get(moduleLoad_pcaComp),
        #                                             tool_pcaComp,
        #                                 stringsAsFactors = FALSE,
        #                                 wdPath=rea$anDir)
        # }
        #
        # if (input$mainNav == 'EpigeneticFeatures' &
        #     file.exists(paste0(rea$anDir, 'info.tsv'))==TRUE ){
        #     #source ('pca_postProcessing.R')
        #     tool_pca <- 'pca_postProcessing'
        #     moduleUI_pca<- paste0 (tool_pca, '_UI')
        #     moduleLoad_pca<- paste0 (tool_pca,'_Server')
        #     #output$pcaSlot<-shiny::renderUI ({
        #     output$selectPn <-renderUI ({
        #         get(moduleUI_pca) (tool_pca,label='epifUI')
        #     })
        #     loadApp_pca <- shiny::callModule(get(moduleLoad_pca), tool_pca,
        #                             stringsAsFactors = FALSE,
        #                             wdPath=rea$anDir
        #                             )
        # }
        #
        # if (input$mainNav == 'Heatmap' &
        #     file.exists(paste0(rea$anDir, 'info.tsv'))==TRUE
        #     ){
        #     tool_htm <- 'TADsV2_Visualization'
        #     moduleUI_htm<- paste0 (tool_htm, '_UI')
        #     moduleLoad_htm<- paste0 (tool_htm,'_Server')
        #     #output$pcaSlot<-shiny::renderUI ({
        #     output$selectPn <-renderUI ({
        #         get(moduleUI_htm) (tool_htm,label='htmUI')
        #     })
        #     loadApp_htm <- shiny::callModule(get(moduleLoad_htm), tool_htm,
        #                                      stringsAsFactors = FALSE,
        #                                      wdPath=rea$anDir
        #     )
        # }
        #
        # if (input$mainNav == 'Networks' &
        #     file.exists(paste0(rea$anDir, 'info.tsv'))==TRUE
        #     ){
        #     tool_ntw <- 'networksV2_Visualization' #networksV2_Visualization_UI
        #     moduleUI_ntw<- paste0 (tool_ntw, '_UI')
        #     moduleLoad_ntw<- paste0 (tool_ntw,'_Server')
        #     #output$pcaSlot<-shiny::renderUI ({
        #     output$selectPn <-shiny::renderUI ({
        #         get(moduleUI_ntw) (tool_ntw,label='ntwUI')
        #     })
        #     loadApp_ntw <- shiny::callModule(get(moduleLoad_ntw), tool_ntw,
        #                                      stringsAsFactors = FALSE,
        #                                      wdPath=rea$anDir
        #     )
        # }


    # if (input$mainNav == 'Analysis' ){
    #     source ('anSettings.R')
    #     tool_anSettings <- 'anSettings'
    #     moduleUI_anSettings<- paste0 (tool_anSettings, '_UI')
    #     moduleLoad_anSettings<- paste0 (tool_anSettings,'_Server')
    #     output$anSetPlot<-renderUI ({
    #     get(moduleUI_anSettings) (tool_anSettings,label='helpingUI')
    #     })
    #     loadApp_anSettings <-  callModule(get(moduleLoad_anSettings), tool_anSettings,
    #                                     stringsAsFactors = FALSE)
    # }
    #
    # if (input$mainNav == 'Pre-Processing' & file.exists(paste0(anFolder$hisave, 'info.tsv'))==TRUE ){
    #     source ('DiffHiC_DetectInteraction.R')
    #     tool_DetectInteraction <- 'DiffHiC_DetectInteraction'
    #     moduleUI_DetectInteraction<- paste0 (tool_DetectInteraction, '_UI')
    #     moduleLoad_DetectInteraction<- paste0 (tool_DetectInteraction,'_Server')
    #     output$preProSlot<-renderUI ({
    #         get(moduleUI_DetectInteraction) (tool_DetectInteraction,label='helpingUI')
    #     })
    #     loadApp_DetectInteraction <-  callModule(get(moduleLoad_DetectInteraction), tool_DetectInteraction,
    #                                             stringsAsFactors = FALSE)
    #}
    #
    # if (input$mainNav == 'Binning' & file.exists(paste0(anFolder$hisave, 'info.tsv'))==TRUE ){
    #     source ('DiffHiC_BinningV2.R')
    #     tool_Binning <- 'DiffHiC_BinningV2'
    #     moduleUI_Binning<- paste0 (tool_Binning, '_UI')
    #     moduleLoad_Binning<- paste0 (tool_Binning,'_Server')
    #     output$binningSlot<-renderUI ({
    #         get(moduleUI_Binning) (tool_Binning,label='helpingUI')
    #     })
    #     loadApp_Binning <-callModule(get(moduleLoad_Binning), tool_Binning,
    #                                     stringsAsFactors = FALSE)
    # }
    #
    # if (input$mainNav == 'WavSis' & file.exists(paste0(anFolder$hisave, 'info.tsv'))==TRUE ){
    #     source ('chromoR_Normalization.R')
    #     tool_chromoR <- 'chromoR_Normalization'
    #     moduleUI_chromoR<- paste0 (tool_chromoR, '_UI')
    #     moduleLoad_chromoR<- paste0 (tool_chromoR,'_Server')
    #     output$WavSisSlot<-renderUI ({
    #     get(moduleUI_chromoR) (tool_chromoR,label='helpingUI')
    #     })
    #     loadApp_chromoR <-  callModule(get(moduleLoad_chromoR), tool_chromoR,
    #                                     stringsAsFactors = FALSE)
    # }
    #
    # if (input$mainNav == 'iterative' & file.exists(paste0(anFolder$hisave, 'info.tsv'))==TRUE ){
    #     source ('diffHic_Normalization.R')
    #     tool_DiffHiC_Norm <- 'diffHic_Normalization'
    #     moduleUI_DiffHiC_Norm<- paste0 (tool_DiffHiC_Norm, '_UI')
    #     moduleLoad_DiffHiC_Norm<- paste0 (tool_DiffHiC_Norm,'_Server')
    #     output$iterativeSlot<-renderUI ({
    #         get(moduleUI_DiffHiC_Norm) (tool_DiffHiC_Norm,label='helpingUI')
    #   })
    #     loadApp_DiffHiC_Norm <<-  callModule(get(moduleLoad_DiffHiC_Norm), tool_DiffHiC_Norm,
    #                                         stringsAsFactors = FALSE)
    # }
    #
    # if (input$mainNav == 'Epigenetic Features' & file.exists(paste0(anFolder$hisave, 'info.tsv'))==TRUE ){
    #     source ('pca_postProcessing.R')
    #     tool_pca <- 'pca_postProcessing'
    #     moduleUI_pca<- paste0 (tool_pca, '_UI')
    #     moduleLoad_pca<- paste0 (tool_pca,'_Server')
    #     output$pcaSlot<-renderUI ({
    #         get(moduleUI_pca) (tool_pca,label='helpingUI')
    #     })
    #     loadApp_pca <-  callModule(get(moduleLoad_pca), tool_pca,
    #                             stringsAsFactors = FALSE)
    # }
    #
    # if (input$mainNav == 'Compartments-PCA' & file.exists(paste0(anFolder$hisave, 'info.tsv'))==TRUE ){
    #     source ('pcaComp_postProcessing.R')
    #     tool_pcaComp <- 'pcaComp_postProcessing'
    #     moduleUI_pcaComp<- paste0 (tool_pcaComp, '_UI')
    #     moduleLoad_pcaComp<- paste0 (tool_pcaComp,'_Server')
    #     output$pcaCompSlot<-renderUI ({
    #     get(moduleUI_pcaComp) (tool_pcaComp,label='helpingUI')
    #     })
    #     loadApp_pcaComp <-  callModule(get(moduleLoad_pcaComp), tool_pcaComp,
    #                                 stringsAsFactors = FALSE)
    # }
    #
    #
    # if (input$mainNav == 'TADs-HMM' & file.exists(paste0(anFolder$hisave, 'info.tsv'))==TRUE ){
    #     source ('HMM_postProcessing.R')
    #     tool_hmm <- 'HMM_postProcessing'
    #     moduleUI_hmm<- paste0 (tool_hmm, '_UI')
    #     moduleLoad_hmm<- paste0 (tool_hmm,'_Server')
    #     output$hmmSlot<-renderUI ({
    #         get(moduleUI_hmm) (tool_hmm,label='helpingUI')
    #     })
    #     loadApp_hmm <-  callModule(get(moduleLoad_hmm), tool_hmm,
    #                                 stringsAsFactors = FALSE)
    # }
    #
    #
    # if (input$mainNav == 'Heatmap' & file.exists(paste0(anFolder$hisave, 'info.tsv'))==TRUE ){
    #     source ('TADsV2_Visualization.R')
    #     tool_tads <- 'TADsV2_Visualization'
    #     moduleUI_tads<- paste0 (tool_tads, '_UI')
    #     moduleLoad_tads<- paste0 (tool_tads,'_Server')
    #     output$TADsModule<-renderUI ({
    #     get(moduleUI_tads) (tool_tads,label='helpingUI')
    #     })
    #     loadApp_tads <-  callModule(get(moduleLoad_tads), tool_tads,
    #                                 stringsAsFactors = FALSE)
    # }
    #
    # if (input$mainNav == 'Networks' & file.exists(paste0(anFolder$hisave, 'info.tsv'))==TRUE ){
    #     source ('networksV2_Visualization.R')
    #     tool_net <- 'networksV2_Visualization'
    #     moduleUI_net<- paste0 (tool_net, '_UI')
    #     moduleLoad_net<- paste0 (tool_net,'_Server')
    #     output$netModule<-renderUI ({
    #     get(moduleUI_net) (tool_net,label='nets')
    #     })
    #     loadApp_net <-  callModule(get(moduleLoad_net), tool_net,
    #                                 stringsAsFactors = FALSE)
    # }



    } #fine observe MainNav
    )

})
