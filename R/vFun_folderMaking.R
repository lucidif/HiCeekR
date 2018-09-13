
#' makeConfig
#'
#' make HiCeekR config file
#'
#' @param path character vector that contain folder path
#'
#' @return HCRwd.info file that contain the path of HiCeekR woorking directory
#'
#' @keywords internal
#'
#'
#' @export
#'
# makeConfig=function(path=paste0(getwd(),"/")){
#     write.table(path, "HCR.config", col.names=FALSE, row.names=FALSE,
#                 quote=FALSE, sep="\t")
#     #print(paste0("working path:",path, ".....setted"))
# }


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
# makeHCRwd<-function(path = getwd()){
#     mainFolder<-paste0(path,"HiCeekRwd/")
#     setHCRwd(mainFolder)
#     SubFolder_lv1<-c('Projects','RefGenomes','Annotations', "tmpDir")
#     dir.create (mainFolder, showWarnings=TRUE, recursive=FALSE)
#     len<-length(SubFolder_lv1)
#     for (ls in 1:len){
#         secondSub<-paste0 (mainFolder, SubFolder_lv1[ls], '/')
#
#         dir.create (secondSub, showWarnings=TRUE, recursive=FALSE)
#
#
#     }
# }


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
# makeHCRprj=function (name, path){
#
#     ##path<-where you would create a folder with variable 'name' name
#     ##quando trova cartelle con lo stesso nome sovrascrive i contenuti o unisce
#     ##i contenuti delle cartelle
#
#     ##folderTree structure
#     ##-project_folder(sama name variable name)
#     ##-----ProjectData
#     ##------------Hi-C
#     ##------------Epi
#     ##------------Exp
#
#     zeroLevel<-'ProjectData'
#     ProjectData_SubFolder_lv1<-c('Hi-C','Epi','Other')
#     #,'Genomes')
#
#     ##when you add new folder in project_folder folder add it to zeroLevel array
#     ##and make new array with su folder
#     ##Es:   zeroLevel<-c('ProjectData', 'exampleFolder')
#     ##      exampleFolder_SubFolder_lv1<- c('example_subfolder1','example_subfolder2','example_subfolder3')
#     ##if you wont add a new subfolder in example_subfolder1 add new subsubfolder 1 and 2 in
#     ##example_subfolder1_subfolder_lv2<- c('subsubfolder1', 'subsubfolder2')
#
#     mainFolder<- paste0 (path,name, '/')
#
#     dir.create (mainFolder, showWarnings=TRUE, recursive=FALSE)
#     lenFirst <- length (zeroLevel)
#     lenSecond <- length (ProjectData_SubFolder_lv1)
#
#     for (lf in 1:lenFirst){
#         firstSub<-paste0 (mainFolder,zeroLevel[lf],'/')
#         dir.create (firstSub, showWarnings=TRUE, recursive=FALSE)
#     }
#
#     ##for folder in first folder of zeroLevel insert hear
#     ##you need to create new for for every zeroLevel element
#
#     ##for element in ProjectData=====================
#     for(ls in 1:lenSecond){
#         secondSub<-paste0 (mainFolder,'ProjectData/',
#                            ProjectData_SubFolder_lv1[ls], '/')
#
#         dir.create (secondSub, showWarnings=TRUE, recursive=FALSE)
#
#
#     }
#     ##===============================================
#
#     #=============================================================
#
# }


#' makeHCRan
#'
#' @param name character varible that contain the name of analysis
#' @param prjPath  character variable that contain the path of projects that
#' @param infoTable table that contai analysis information
#'
#' @return
#' @keywords internal
#'
#' @examples
# makeHCRan<-function(name, prjPath, infoTable, makeInfo=TRUE){
#     mainFolder<-paste0(prjPath,name,"/")
#     dir.create(mainFolder,showWarnings=TRUE, recursive=FALSE)
#
#     lv1<-c("SysOut", "Results")
#     lv2<-c("Pre-Processing", "Filtering", "Binning", "Normalization",
#            "Downstream", "Visualization", "Report")
#
#     #dir creation
#     for(i in 1:length(lv1)){
#         lv1path<-paste0(mainFolder,lv1[i],"/")
#         dir.create(lv1path)
#         for(j in 1:length(lv2)){
#             dir.create(paste0(lv1path,lv2[j]))
#         }
#     }
#
#     #create info file
#     #control table
#     infoTable<-testInfoTable(infoTable)
#     if (makeInfo==TRUE){
#         write.table(infoTable,paste0(mainFolder,"info.tsv"),
#                     sep="\t",
#                     quote=FALSE,
#                     col.names=FALSE,
#                     row.names=FALSE)
#     }
#
#
# }
#
