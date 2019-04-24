
#' predictTADs
#'
#' @param hmmPath
#' @param tollerance
#' @param maxspan
#' @param spanMark
#' @param saveBed
#'
#' @return
#' @keywords internal
#'
#' @examples
predictTADs<-function(hmmPath,
                      tollerance=3,
                      maxspan=3,
                      spanMark=FALSE,
                      saveBed=NULL){
    options(stringsAsFactors = FALSE)
    hmmTab<-read.table(hmmPath, sep="\t", header=TRUE)

    #which(!is.na(hmmTab[,2]))

    hmmTab<-hmmTab[which(!is.na(hmmTab[,2])),]

    chromo<-c()
    for (i in 1:length(hmmTab[,1])){
        chromo[i]<-strsplit(as.character(hmmTab[i,1]),":")[[1]][1]
    }

    hmmTab<-hmmTab[which(chromo=="chr2"),]

    #hmmPath,tollerance=3, maxspan=3
    tadsLimit<-c()
    tadsCounter<-0
    #tollerance<-3
    #maxspan<-3
    predictBoundaries<-c()
    z<-1
    w<-1
    span<-0
    allref<-c()
    referVal="none"
    for ( i in 1:length(hmmTab[,1])){
        #print(paste0(i,"/",length(hmmTab[,1])))
        if (i!=1){
            previousVal<-currentVal
            #referVal<-previousVal
        }

        # if(previousVal=="none"){
        #     previousVal=currentVal
        # }

        if (hmmTab[i,2]>0){
            currentVal<-"positive"
            if(i==1){referVal<-currentVal}
        }else{
            currentVal<-"negative"
            if(i==1){referVal<-currentVal}
        }

        if(i!=1){
            #print (currentVal)
            if (currentVal!=previousVal){
                referVal<-previousVal #non può esere questo
                allref[w]<-referVal
                w<-w+1
                tollerance<-tollerance-1
                if (tollerance==0){
                    #print("count +1")
                    tadsCounter<-tadsCounter+1
                    tollerance=3
                    predictBoundaries[z]<-hmmTab[i,1]
                    z<-z+1
                    #currentVal<-"none"
                }
            } else {
                #print (paste0("refer:",referVal,", curr:",currentVal))
                if (i>maxspan+2){

                    if (hmmTab[i-maxspan,2]>0){
                        refVal<-"positive"
                    }else{
                        refVal<-"negative"
                    }

                    if (referVal!=currentVal){
                        print("oooo")
                        span<-span+1

                        if (span==maxspan){
                            print("count +1")
                            tadsCounter<-tadsCounter+1

                            if (spanMark==TRUE){
                                predictBoundaries[z]<-paste0("span_",hmmTab[i-3,1])
                            }else{
                                predictBoundaries[z]<-hmmTab[i-3,1]
                            }

                            z<-z+1
                            #currentVal<-"none"
                            span=0
                            referVal<-currentVal
                            #i<-i-2
                        }


                    }

                }



            }
        }
    }

    print(length(unique(predictBoundaries)))
    frbin<-hmmTab[1,1]

    if(length(which(predictBoundaries==frbin))==0){
        predictBoundaries<-c(frbin,predictBoundaries)
    }

    end<-hmmTab[length(hmmTab[,1]),1]
    if(length(which(predictBoundaries==end))==0){
        predictBoundaries<-c(predictBoundaries,end)
    }
    #return(predictBoundaries)
    TADs<-c()
    t<-1

    allBins<-as.character(hmmTab[,1])

    for (i in 2:length(predictBoundaries)){
        if (i==2){
            chr<-strsplit(predictBoundaries[i],":")[[1]][1]
            start<-strsplit(strsplit(allBins[1],":")[[1]][2],"-")[[1]][1]
            end<-strsplit(strsplit(predictBoundaries[i],":")[[1]][2],"-")[[1]][2]
            TADs[t]<-paste0(chr,":",start,"-",end)
            t<-t+1
        }else{
            chr<-strsplit(predictBoundaries[i],":")[[1]][1]
            end<-strsplit(strsplit(predictBoundaries[i],":")[[1]][2],"-")[[1]][2]
            stid<-which(allBins==predictBoundaries[i-1])+1
            start<-strsplit(strsplit(allBins[stid],":")[[1]][2],"-")[[1]][1]
            TADs[t]<-paste0(chr,":",start,"-",end)
            t<-t+1
        }
    }
    chr<-c()
    start<-c()
    end<-c()
    if(is.null(saveBed)==FALSE){
        #export bed file
        for (i in 1:length(TADs)){
            chr[i]<-strsplit(TADs[i],":")[[1]][1]
            start[i]<-strsplit(strsplit(TADs[i],":")[[1]][2],"-")[[1]][1]
            end[i]<-strsplit(strsplit(TADs[i],":")[[1]][2],"-")[[1]][2]
        }
        bedTab<-data.frame(chr,start,end)
        write.table(bedTab,file=saveBed,sep="\t",quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
    return(TADs)

}
