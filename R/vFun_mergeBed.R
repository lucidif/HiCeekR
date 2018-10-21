#' Title
#'
#' @param bedArray
#' @param customHeader
#' @param header
#'
#' @return
#' @keywords internal
#'
#' @examples
mergeBed<-function(bedArray, customHeader=FALSE, header){


    # str1<-"/media/lucio/data/1.Bioinformatic/1.heavyWD/1.R/3.cartellaTestNewHiCeekR/HiCeekRwd/Projects/ooo/testSubData/Results/Downstream/epiCounts_H3K9ac.bed"
    #
    # str2<-"/media/lucio/data/1.Bioinformatic/1.heavyWD/1.R/3.cartellaTestNewHiCeekR/HiCeekRwd/Projects/ooo/testSubData/Results/Downstream/epiCounts_H3K27ac.bed"
    #
    # b1name<-gsub(".bed","", strsplit(str1, "/")[[1]][length(strsplit(str1, "/")[[1]])])
    # b1<-read.table(str1, sep="\t")
    # b1<-data.frame(rep(paste0(b1$V1,":",b1$V2,"-",b1$V3)),b1[,"V6"])
    # colnames(b1)<-c("region",b1name)
    #
    # b2name<-gsub(".bed","", strsplit(str2, "/")[[1]][length(strsplit(str2, "/")[[1]])])
    # b2<-read.table(str2, sep="\t")
    # b2<-data.frame(rep(paste0(b2$V1,":",b2$V2,"-",b2$V3)),b2[,"V6"])
    # colnames(b2)<-c("region",b2name)



    #bm <-merge(b1,b2, by="region")

    for (i in 1:length(bedArray)){
        str1<-bedArray[i]
        b1name<-gsub(".bed","", strsplit(str1, "/")[[1]][length(strsplit(str1, "/")[[1]])])
        b1<-read.table(str1, sep="\t")
        b1<-data.frame(rep(paste0(b1$V1,":",b1$V2,"-",b1$V3)),b1[,"V6"])
        if (customHeader==TRUE){
            colnames(b1)<-c("region",header[i])
        } else {
            colnames(b1)<-c("region",b1name)
        }

        if (i==1){
            bm<-b1
        } else {

            bm <-merge(bm,b1, by="region")

        }
        # if (length(bedArray!=1)){
        #
        #     if (i==1){
        #         bm<-b1
        #     }
        #
        # } else {
        #     bm<- b1
        # }

    }

    rownames(bm)<-bm$region
    bm<-bm[,-1]


    return(bm)


}
