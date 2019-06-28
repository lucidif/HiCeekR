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

    #matriceSimmetrica completa
    tarchr<-unique(chr)[1]

    #standard fun
    #tarchr<-unique(chr)[1:2]

    tarchr<-tarchr[!is.na(tarchr)]



    if(length(tarchr)>1){
        #xgrcl<-grcl[seqnames(grcl)==tarchr[1]]
        #ygrcl<-grcl[seqnames(grcl)==tarchr[2]]
        #which(chr==tarchr[1])
        #which(chr==tarchr[2])
        xnam<-paste0("bin",as.array(which(chr==tarchr[1]))) #column names
        ynam<-paste0("bin",as.array(which(chr==tarchr[2]))) #row names

        #=======================================matrice non simmetrica
        # intdata<-intdata[,which(chr==tarchr[1])]
        # intdata<-intdata[which(chr==tarchr[2]),]
        # colnames(intdata)<-xnam
        # rownames(intdata)<-ynam
        #=======================================

        #bnnam<-rep(paste0("bn",1:length(intdata[,1])))
        #colnames(intdata)<-bnnam
        #rownames(intdata)<-bnnam

        xchr<-chr[which(chr==tarchr[1])]
        xstart<-start[which(chr==tarchr[1])]
        xend<-end[which(chr==tarchr[1])]

        ychr<-chr[which(chr==tarchr[2])]
        ystart<-start[which(chr==tarchr[2])]
        yend<-end[which(chr==tarchr[2])]

        #nel casi di intra chr puoi fare xdf<-ydf<-df e vai avanti
        df<-data.frame(chr,start,end)

        xdf<-data.frame(xchr,xstart,xend)
        ydf<-data.frame(ychr,ystart,yend)

        xls<-list(chr=xchr,start=xstart,end=xend)
        yls<-list(chr=ychr,start=ystart,end=yend)

        xgr<-GenomicRanges::makeGRangesFromDataFrame(xdf)
        ygr<-GenomicRanges::makeGRangesFromDataFrame(ydf)

        grrw<-ygr
        grcl<-xgr

        names(grcl)<-xnam
        names(grrw)<-ynam

        intdata<-Matrix::Matrix(intdata)

    } else {
        #xgrcl<-grcl
        #ygrcl<-grcl
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


    }


    #if(length(unique(chr))>0){
        #convert in non sym matrix

    #}







    #tt<-intdata[,which(seqnames(grcl)==tarchr[1])]
    #tt<-tt[which(seqnames(grcl)==tarchr[2]),]1

    htcxp<-methods::new("HTCexp",intdata,xgi=sort(grcl),ygi=sort(grrw))
    #htcxp<-methods::new("HTCexp",intdata)
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

