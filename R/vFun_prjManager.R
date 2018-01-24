
#' setHCRwd
#'
#' set folder in which HiCeekR works
#'
#' @param path character vector that contain folder path
#'
#' @return None
#'
#' @example
#'
#'
#' @export
#'
setHCRwd=function(path){
    write.table(path,"HCRwd.info", col.names=FALSE, row.names=FALSE,
                quote=FALSE, sep="\t")
}


#' makeHiCeekRwd
#'
#' make folder in which HiCeekR works
#'
#' @param path character vector that contain folder path
#'
#' @return None
#'
#' @example
#'
#'
#' @export
makeHCRwd=function(path){
    mainFolder=paste0(path,"HiCeekRwd/")
    SubFolder_lv1<-c('Projects','RefGenome','Annotations')
    dir.create (mainFolder, showWarnings=TRUE, recursive=FALSE)
    len=length(SubFolder_lv1)
    for (ls in 1:len){
        secondSub<-paste0 (mainFolder, SubFolder_lv1[ls], '/')

        dir.create (secondSub, showWarnings=TRUE, recursive=FALSE)


    }
}


#' make HiCeekR project folder
#'
#' make Project folder in which HiCeekR works
#'
#' @param path character vector that contain folder path
#' @param name character vector that defines name of project
#'
#' @return None
#'
#' @example
#'
#'
#' @export
makeHCRprj=function (name, path){

    ##path<-where you would create a folder with variable 'name' name
    ##quando trova cartelle con lo stesso nome sovrascrive i contenuti o unisce
    ##i contenuti delle cartelle

    ##folderTree structure
    ##-project_folder(sama name variable name)
    ##-----ProjectData
    ##------------Hi-C
    ##------------Epi
    ##------------Exp

    zeroLevel<-'ProjectData'
    ProjectData_SubFolder_lv1<-c('Hi-C','Epi','Exp')
    #,'Genomes')

    ##when you add new folder in project_folder folder add it to zeroLevel array
    ##and make new array with su folder
    ##Es:   zeroLevel<-c('ProjectData', 'exampleFolder')
    ##      exampleFolder_SubFolder_lv1<- c('example_subfolder1','example_subfolder2','example_subfolder3')
    ##if you wont add a new subfolder in example_subfolder1 add new subsubfolder 1 and 2 in
    ##example_subfolder1_subfolder_lv2<- c('subsubfolder1', 'subsubfolder2')

    mainFolder<- paste0 (path,name, '/')

    dir.create (mainFolder, showWarnings=TRUE, recursive=FALSE)
    lenFirst <- length (zeroLevel)
    lenSecond <- length (ProjectData_SubFolder_lv1)

    for (lf in 1:lenFirst){
        firstSub<-paste0 (mainFolder,zeroLevel[lf],'/')
        dir.create (firstSub, showWarnings=TRUE, recursive=FALSE)
    }

    ##for folder in first folder of zeroLevel insert hear
    ##you need to create new for for every zeroLevel element

    ##for element in ProjectData=====================
    for(ls in 1:lenSecond){
        secondSub<-paste0 (mainFolder,'ProjectData/', ProjectData_SubFolder_lv1[ls], '/')

        dir.create (secondSub, showWarnings=TRUE, recursive=FALSE)


    }
    ##===============================================

    #=============================================================

}
