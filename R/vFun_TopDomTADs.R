#' TopDomTADs
#'
#' @param matrixPath
#' @param resultsPath
#' @param window.size
#'
#' @return
#' @keywords internal
#'
#' @examples
TopDomTADs<-function(matrixPath, resultsPath, window.size = 7){
    #Es matrixPath="/media/lucio/data/1.Bioinformatic/1.heavyWD/1.R/test4/HiCeekRwd/Projects/Grubert2015/REP5_GSM1608509/Results/Normalization/REP5_GSM1608509_chr2VSchr2_iterative_matrix.tsv"
    library(TopDom)
    #resultsPath="/media/lucio/FastData/3.SuperGrive/Mega/bioinformatics/pipelines/Hi-C"

    cntTable<-read.table(file=matrixPath, sep="\t", header=TRUE, check.names = FALSE)
    row.names(cntTable)<-cntTable[,1]

    cntTable<-cntTable[,-1]


    chromosome<-c()
    from.coord<-c()
    to.coord<-c()
    for(i in 1:length(cntTable[,1])){
        split1<-strsplit(rownames(cntTable)[i],":")
        chromosome[i]<-split1[[1]][1]
        split2<-strsplit(split1[[1]][2],"-")
        from.coord[i]<-split2[[1]][1]
        to.coord[i]<-split2[[1]][2]
    }

    cntTb<-cbind(chromosome,from.coord,to.coord,cntTable)

    delta<-as.numeric(as.character(cntTb$to.coord[1]))-as.numeric(as.character(cntTb$from.coord[2]))

    cntTb$to.coord<-rep((as.numeric(as.character(cntTb$to.coord)))-(delta))

    #write.table(cntTb, file="/media/lucio/FastData/3.SuperGrive/Mega/bioinformatics/pipelines/Hi-C/TopDomIN", col.names = FALSE, row.names = FALSE, sep="\t")

    write.table(cntTb, file=paste0(resultsPath,"/TopDomIN"), col.names = FALSE, row.names = FALSE, sep=" ")


    hctb<-readHiC(paste0(resultsPath,"/TopDomIN"))

    res<-TopDom(data=hctb, window.size = window.size , outFile = paste0(resultsPath,"/TopDom"), debug=TRUE)

    recoordBed<-res$bed

    recoordBed$chromEnd<-rep(as.numeric(as.character(recoordBed$chromEnd))+3)

    return(recoordBed)

    }
