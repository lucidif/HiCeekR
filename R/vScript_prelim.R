# ##1_preliminaryStatements
# ##this script is the first script of main server of HiCeekR
#
# #1)cleaning .www folder==================
# selected<-list.files (path='./www')
# selected=rep(paste0('./www/',selected))
# rep (file.remove (selected),1)
#
# #========================================
#
# zl<- paste0 (getwd(),'/HiCeekR_Projects/')    ##contain the folder path of projects folders #questo è quello generale per tutti i pc
#
# ##per i pc miei con progetti in dropbox cambio zl con il hiceekr_wd
#
# if (myDrop==TRUE){
#
#   if (workingPC=='labPC'){
#     zl<- paste0 ('/home/lucio/rPrj/1.HiCeekR_wd','/HiCeekR_Projects/')   ##pc CNR
#   }
#
#   if (workingPC=='stebbidone'){
#     zl<- paste0 ('/media/lucio/DATA/7.R/1.Rprj/HiCeekR_wd','/HiCeekR_Projects/')   ##notebook
#   }
#
#   if (workingPC=='travelmatto'){
#     zl<- paste0 ('/media/data/3.bioLab/7.HiCeekR_wd','/HiCeekR_Projects/')   ##netbook
#   }
#
#   if (workingPC=='homePC'){
#     # zl<- paste0 ('/media/utonto/crazy_lab_mob/Rprj/HiCeekR_wd','/HiCeekR_Projects/')   ##old
#     zl<- paste0 ('/media/utonto/bioData/1.R/3.workingDirectory/1.HiCeekR','/HiCeekR_Projects/') ##pc fisso CASA
#
#   }
#
#   if (workingPC=='homePC_cln'){
#     # zl<- paste0 ('/media/utonto/crazy_lab_mob/Rprj/HiCeekR_wd','/HiCeekR_Projects/')   ##old
#     zl<- paste0 ('/media/utonto/bioData/1.R/3.workingDirectory/2.HiCeekR_cln','/HiCeekR_Projects/') ##pc fisso CASA dataset ripuliti
#
#   }
#
#   if (workingPC=='labPC_cln'){
#     zl<- paste0 ('/home/lucio/rPrj/2.HiCeekR_cln','/HiCeekR_Projects/') ##pc fisso CASA dataset ripuliti
#   }
#
#   if (workingPC=='myPC_cln')
#     zl<- paste0('/media/lucio/data/1.Bioinformatic/1.heavyWD/1.R/2.HiCeekR_cln/', '/HiCeekR_Projects/') ##laptop potente
#
# }
#
# ##================================================================
#
#
# print (c('zeroLevelPath', zl))
# zeroLevel<-HiClass$new ('contain the folder path of projects folders', zl)
# # prjFolder<<-HiClass$new ('contain the folder path of projects folders', zl)
# # anFolder<<-HiClass$new ('contain the folder path of projects folders', zl)
# anFolder<<- HiClass$new ('whenStart anFolder=zl',zl)
#
# ##HiCeekR state
# ##prjState<<- HiClass$new ('project state',hisave='begin')
# generalState<- HiClass$new ('project state',hisave='begin')
# ##possible state : "begin", "setProjectOK", "setAnalysisOK"
