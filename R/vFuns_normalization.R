ICEnormalization<-function(matrixPath, max_iter = 50, eps = 1e-4 ,
                           sparse.filter = 0.02,
                           returnMatrix=TRUE
                           #,exportFolder=NULL
                           ){
    library(HiTC)
    #intdata<-read.table(matrixPath,sep="\t",header=TRUE,stringsAsFactor=FALSE)
    print("start import")
    intdata<-read.table(matrixPath,sep="\t",header=TRUE,stringsAsFactor=FALSE)
    #colnames(intdata)<-intdata[1,]
    #intdata<-intdata[-1,]
    rownames(intdata)<-intdata$index
    intdata<-intdata[,-1]
    colnames(intdata)<-rownames(intdata) #da mettere a posto in questo modo va bene solo con matrici simmetriche
    xnames<-colnames(intdata)
    ynames<-rownames(intdata)
    intdata<-Matrix::as.matrix(intdata)
    print("end import")

    chr<-c()
    cord<-c()
    start<-c()
    end<-c()
    for (i in 1:length(rownames(intdata))){
        complete<-rownames(intdata)[i]
        chr[i]<-strsplit(complete,":")[[1]][1]
        cord[i]<-strsplit(complete,":")[[1]][2]
        start[i]<-strsplit(cord[i],"-")[[1]][1]
        end[i]<-strsplit(cord[i],"-")[[1]][2]

    }

    df<-data.frame(chr,start,end)
    ls<-list(chr=chr,start=start,end=end)
    gr<-GenomicRanges::makeGRangesFromDataFrame(df)

    #clnam<-rep(paste0("cl",1:length(intdata[1,])))
    #rwnam<-rep(paste0("rw",1:length(intdata[,1])))
    bnnam<-rep(paste0("bn",1:length(intdata[,1])))
    originalNames<-rownames(intdata)


    #rownames(intdata)<-rwnam
    #colnames(intdata)<-clnam

    rownames(intdata)<-bnnam
    colnames(intdata)<-bnnam

    grrw<-gr
    #names(grrw)<-rwnam
    names(grrw)<-bnnam
    grcl<-gr
    #names(grcl)<-clnam
    names(grcl)<-bnnam

    intdata<-Matrix::Matrix(intdata)#,dimnames=ls)
    #names(gr)<-rownames(intdata)

    df <- data.frame(seqnames=GenomicRanges::seqnames(grcl),
                     starts=GenomicRanges::start(grcl)-1,
                     ends=GenomicRanges::end(grcl)
                     #,names=c(rep(".", length(grcl)))
                     #,scores=elementMetadata(grcl)$Conc,
                     #strands=strand(grcl)
    )

    htcxp<-methods::new("HTCexp",intdata,xgi=grcl,ygi=grcl)
    #htcxp<-new("HTCexp",intdata)
    #pr<-pca.hic(htcxp,normPerExpected=FALSE,npc=1)
    #pr1<-pca.hic(htcxp,normPerExpected=TRUE,npc=1)
    #pr2<-pca.hic(htcxp,normPerExpected=TRUE,npc=2)
    #pr3<-pca.hic(htcxp,normPerExpected=TRUE,npc=3,asGRangesList=TRUE)

    ICEhtcxp<-HiTC::normICE(htcxp,
                            max_iter = max_iter,
                            eps = eps ,
                            sparse.filter = sparse.filter)

    exportMatrix<-as.matrix(HiTC::intdata(ICEhtcxp))
    rownames(exportMatrix)<-ynames
    colnames(exportMatrix)<-xnames

    # if( is.null(exportFolder)==FALSE){
    #     #write.table()
    # }

    if (returnMatrix==TRUE){
        return(exportMatrix)
    }else{
        return(ICEhtcxp)
    }


}


LGFnormalization<-function(matrixPath, fit=c("poisson","nb")){
    modelfit<-fit[1]
    intdata<-read.table(matrixPath,sep="\t",header=TRUE)
    rownames(intdata)<-intdata$index
    intdata<-intdata[,-1]
    xnames<-colnames(intdata)
    ynames<-rownames(intdata)
    intdata<-Matrix::as.matrix(intdata)

    chr<-c()
    cord<-c()
    start<-c()
    end<-c()
    for (i in 1:length(rownames(intdata))){
        complete<-rownames(intdata)[i]
        chr[i]<-strsplit(complete,":")[[1]][1]
        cord[i]<-strsplit(complete,":")[[1]][2]
        start[i]<-strsplit(cord[i],"-")[[1]][1]
        end[i]<-strsplit(cord[i],"-")[[1]][2]

    }

    df<-data.frame(chr,start,end)
    ls<-list(chr=chr,start=start,end=end)
    gr<-GenomicRanges::makeGRangesFromDataFrame(df)

    #clnam<-rep(paste0("cl",1:length(intdata[1,])))
    #rwnam<-rep(paste0("rw",1:length(intdata[,1])))
    bnnam<-rep(paste0("bn",1:length(intdata[,1])))
    originalNames<-rownames(intdata)


    #rownames(intdata)<-rwnam
    #colnames(intdata)<-clnam

    rownames(intdata)<-bnnam
    colnames(intdata)<-bnnam

    grrw<-gr
    #names(grrw)<-rwnam
    names(grrw)<-bnnam
    grcl<-gr
    #names(grcl)<-clnam
    names(grcl)<-bnnam

    intdata<-Matrix::Matrix(intdata)#,dimnames=ls)
    #names(gr)<-rownames(intdata)

    df <- data.frame(seqnames=GenomicRanges::seqnames(grcl),
                     starts=GenomicRanges::start(grcl)-1,
                     ends=GenomicRanges::end(grcl)
                     #,names=c(rep(".", length(grcl)))
                     #,scores=elementMetadata(grcl)$Conc,
                     #strands=strand(grcl)
    )

    htcxp<-new("HTCexp",intdata,xgi=grcl,ygi=grcl)
    #htcxp<-new("HTCexp",intdata)
    #pr<-pca.hic(htcxp,normPerExpected=FALSE,npc=1)
    #pr1<-pca.hic(htcxp,normPerExpected=TRUE,npc=1)
    #pr2<-pca.hic(htcxp,normPerExpected=TRUE,npc=2)
    #pr3<-pca.hic(htcxp,normPerExpected=TRUE,npc=3,asGRangesList=TRUE)

    LGFhtcxp<-HiTC::normLGF(htcxp,family=modelfit)

    exportMatrix<-as.matrix(HiTC::intdata(LGFhtcxp))
    rownames(exportMatrix)<-ynames
    colnames(exportMatrix)<-xnames

    # if( is.null(exportFolder)==FALSE){
    #     #write.table()
    # }

    return(exportMatrix)
}

