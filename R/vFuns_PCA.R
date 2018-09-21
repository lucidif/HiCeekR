#' pcaMatrixModel
#'
#' @param binTablePath
#' @param columnNumber
#'
#' @return
#' @keywords internal
#'
#' @examples
pcaMatrixModel<- function (binTablePath, columnNumber=1){

    #---------------------------------------------------------
    bintab<- read.table (as.character(binTablePath), sep='\t', header=FALSE)#, row.names=FALSE)
    #View (bintab)
    #print ('function:binTableImport......binTablePath imported')
    lenbinTab<-length (bintab[,1])
    binTabImported<- matrix(ncol=1, nrow=lenbinTab)


    for (i in 1:lenbinTab){
        name<-paste0(as.character(bintab[i,1]),':',as.character(bintab[i,2]),'-',as.character(bintab[i,3]))
        print (name)

        binTabImported[i,1]<-name
    }

    #---------------------------------------------------------

    #View (binTabImported)

    lenBintab <- length (binTabImported[,1])
    pcaMatrix <- matrix (nrow=lenBintab, ncol=columnNumber)
    rownames(pcaMatrix)<- binTabImported [,1]
    pcaMatrix[,1]<-binTabImported [,1]
    colnames(pcaMatrix)<-'regions'



    return (pcaMatrix)
    #for (i in 1:lenBintab){

    #rownames(pcaMatrix[i])<- binTableImported [i,1]

    #}

}


#' bamPCA
#'
#' @param binTablePath
#' @param bamPath
#' @param pcaMatrixModelOut
#' @param columnName
#' @param add
#'
#' @return
#' @keywords internal
#'
#' @examples
bamPca <- function (binTablePath, bamPath, pcaMatrixModelOut ,
                    columnName , add=FALSE){
    ## se add=FALSE i valori vengono inseriti nell'ultima colonna
    ## se add=TRUE i valori vengono inseriti in una nuova colonna che viene generata dopo l'ultima

    pcaModel<- pcaMatrixModelOut

    #-------------------------------------------------------------
    if (add==TRUE){
        pcaModel <- cbind (pcaModel, 0)
    } else {
        len<-as.numeric(length(pcaModel[1,]))
        if ( len==1 ){
            pcaModel<- cbind (pcaModel, 0)
        }
    }
    colNumber  <- length (pcaModel[1,])
    colnames (pcaModel)[colNumber]<- columnName
    #-------------------------------------------------------------


    bintab<- as.matrix(read.table (binTablePath, sep='\t', header=FALSE))
    #View (bintab)
    lenbinTab<-length (bintab[,1])
    binTabImported<-matrix (ncol=3, nrow=lenbinTab)


    ##sincronizza nomi cromosomi
    bamchr <- Rsamtools::scanBamHeader(bamPath)
    print ('bamPath.....OK')
    bamchr<-as.data.frame(bamchr[[1]][1])
    print ('bamchr.....OK')
    binNames<-row.names(bamchr)
    print ('binNames.....OK')
    lenBinNames<- length (binNames)
    print ('lenBinNames.....OK')

    for (i in 1:lenbinTab){
        #name<-paste0(bintab[i,2],',',bintab[i,3],',',bintab[i,4])

        # binTabImported[i,1]<-as.character(bintab$seqnames)
        # binTabImported[i,2]<-bintab$starts
        # binTabImported[i,3]<-bintab$ends

        binTabImported[i,1]<-as.character(bintab[i,1])
        binTabImported[i,2]<-bintab[i,2]
        binTabImported[i,3]<-bintab[i,3]

    }

    View (binTabImported)

    #maxbinTabImpo<-length (unique (binTabImported[,1]))
    #indexMatrix<-matrix (nrow=maxbinTabImpo, ncol=2) #prima colonna nome cromosoma, seconda colonna numero riga in cui inizia

    #-------------------------------------------------------------

    lenMaxBinImported<-as.numeric( length (binTabImported[,1]) )

    ##indexMatrix [1,1]<- binTabImported [1,1]
    ##indexMatrix [1,2]<- 1
    ##index<-1

    #for (t in 1:lenMaxPcaModel){
    #if (binTabImported[t,1]!=binTabImported[t+1,1]){
    ##   index<- index+1
    ##  indexMatrix [index,1]<-binTabImported[t+1,1]
    ##  indexMatrix [index,2]<-t+1

    # }
    #}
    printer<- 0
    for (i in 1:lenMaxBinImported){
        switcho<- FALSE
        if (i!=1){
            if (binTabImported [i,1]!=binTabImported[i-1,1]){
                ##prima importi il bam e poi fai la ricerca per subset
                ##devi anche calcolare start ed end
                binTabImportedSub<- subset (binTabImported , binTabImported[,1]== binTabImported [i,1] )
                #print (head (binTabImportedSub))
                start <- as.numeric ( min (as.numeric(binTabImportedSub[,2]) ))
                end <- as.numeric (max (as.numeric(binTabImportedSub[,3])))
                chrom<-as.character(binTabImported [i,1])
                print (chrom)
                subChro<- subset(binNames, binNames==chrom)
                subnu<-as.numeric(length(subChro))
                if (subnu!=0){
                    bamCounts<-bamToDataFrame(bamPath=bamPath, chrom, start=start, end=end)
                    switcho<- TRUE
                }


            } else{
                ##non fare il bam e passa direttamente alla ricerca per subset
                switcho<- TRUE


            }
        } else {
            ##prima importi il bam poi fai la ricerca per subset della prima regione

            chrom<-as.character(binTabImported [i,1])

            binTabImportedSub<- subset (binTabImported , binTabImported[,1]== binTabImported [i,1] )
            print (head (binTabImportedSub))
            start <- as.numeric (min (as.numeric(binTabImportedSub[,2])))
            end <- as.numeric (max ( as.numeric(binTabImportedSub[,3])))

            subChro<- subset(binNames, binNames==chrom)
            subnu<-as.numeric(length(subChro))

            if (subnu!=0){

                bamCounts<-bamToDataFrame(bamPath=bamPath, chrom , start=start, end=end)
                switcho<- TRUE
            }


        }

        if (switcho==TRUE){
            bamCounts2<- subset (bamCounts, as.numeric(bamCounts[,2])<=as.numeric(binTabImported[i,3]))
            bamCounts2<- subset (bamCounts2, as.numeric(bamCounts2[,2])>=as.numeric(binTabImported[i,2]))
            counts<- length (bamCounts2[,1])
            pcaModel[i,colNumber]<- as.numeric (counts)

        }

        printer <- printer+1

        if (printer==50){
            print (c('i',i, 'lenMaxBinImported', lenMaxBinImported))
            printer<- 0
        }
    }

    #pcaModel<- subset (pcaModel, pcaModel[,1]!='seqnames:starts-ends')
    return (pcaModel)

    #----------------------------------------------------------------
    columnNumber<-binTableImported
    lenBintab <-length (binTableImported[,1])
    lenBamfre <-length (bamFrame[,1])
}









