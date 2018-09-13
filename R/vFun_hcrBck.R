#' hcrBck
#'
#' @param pathFrom
#' @param pathTo
#' @param paName
#'
#' @return
#' @keywords internal
#'
#' @examples
hcrBck<-function(pathFrom="/media/lucio/FastData/3.SuperGrive/Grive/1.Bioinformatic/2.Rprj/2.devel/",
                 pathTo="/media/lucio/data/10.bck/HCRbck/",
                 paName="HiCeekR"
                 ){

    #name<-"HiCeekR/"
    sysT<-Sys.time()
    sysT<-gsub(" ", "_", sysT, fixed=TRUE)
    coName<-paste0(paName,"_",sysT)
    fromPa<-paste0(pathFrom,paName)
    toPa<-paste0(pathTo,coName)
    #file.copy(paste0(pathFrom,paName),paste0(pathTo,coName))

    system(paste("cp -a", fromPa, toPa))

}
