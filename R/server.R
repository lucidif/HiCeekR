options(shiny.maxRequestSize=300*1024^2)
#require(R6)
#source ('requirement.R')

#===================================my ws===================================================================
#                                                                               myDrop<<-TRUE             #metti myDrop<-FALSE se stai lavorando con cartelle progetto all'interno del main folder di HiCeekR
#                                                                               workingPC<<- 'myPC_cln'      # 1) 'labPC'   2) 'stebbidone'   3)travelmatto  4)homePC   5)homePC_cln  6)labPC_cln  7)myPC_cln
#                                                                               source('1_preliminaryStatements.R')

#===========================================================================================================
#wk<<-getwd()


server<-shiny::shinyServer(function(input, output, session ) {

workingDir<-as.character((read.table('HCRwd.info'))[1,1])
#setwd<-pathWd

##======WELCOME=========================================================================================
    # source ('prjSettings.R')
    # tool_prjSettings <- 'prjSettings'
    # moduleUI_prjSettings<- paste0 (tool_prjSettings, '_UI')
    # moduleLoad_prjSettings<- paste0 (tool_prjSettings,'_Server')
    # output$prjSetPlot<-renderUI ({
    # get(moduleUI_prjSettings) (tool_prjSettings,label='helpingUI')
    # })
    # loadApp_prjSettings <-  callModule(get(moduleLoad_prjSettings), tool_prjSettings,
    #                                   stringsAsFactors = FALSE)




    shiny::observeEvent (input$mainNav,{


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



    })

})
