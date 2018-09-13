#' matrixWithNames
#'
#' @param matrixWithIdPath
#' @param bint.bed_Path
#' @param speed
#' @param chromoA
#' @param chromoB
#' @param bedPath
#'
#' @return
#' @keywords internal
#'
#' @examples
matrixWithNames <- function (matrixWithIdPath , bint.bed_Path,
                             speed=FALSE ,chromoA, chromoB, bedPath=TRUE){

    matri<- HCRread ('', path=matrixWithIdPath)
    arr<- colnames(matri)
    arr<- sub('X','',arr, fixed=TRUE )
    colnames(matri)<-arr
    #View (matri)
    if (bedPath==TRUE){
        bedTab<-read.table (bint.bed_Path, sep='\t')
    } else {
        bedTab<-bint.bed_Path
    }

    if (speed==TRUE){
        bedTab<-subset (bedTab, bedTab[,1]==chromoA | bedTab[,1]==chromoB )
    }

    ##appezzoti generando una matrice identica a rawmatrix in dimensione e modificando rownames e colnames
    bedAr<- as.matrix(rep (paste0(bedTab[,1],':',bedTab[,2],'-',bedTab[,3])))
    rownames (bedAr)<- rownames (bedTab)

    rawmatrixBis <- matri

    ##gli devo far controllare gli id della matrice rowmatrix  e farglieli convertire in names
    ##ora sostituisco in rawmatrixBis gli indici con i nomi

    for (i in 1:length(rawmatrixBis[,1])){
        binReg<- subset (bedAr, rownames (bedAr)==rownames (rawmatrixBis)[i] )
        if (length (binReg[,1])==1){
            rownames (rawmatrixBis)[i] <-binReg[1,1]
        } else {print (c('i',i,'error more than one bin with same name in contact matrix'))}
    }

    for (i in 1:length(rawmatrixBis[1,])){
        binReg<- subset (bedAr, rownames (bedAr)==colnames (rawmatrixBis)[i] )
        if (length (binReg[,1])==1){
            colnames (rawmatrixBis)[i] <-binReg[1,1]
        } else {print (c('i',i,'error more than one bin with same name in contact matrix'))}
    }

    return (rawmatrixBis)

}
