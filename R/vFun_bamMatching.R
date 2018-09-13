#' bamMatching
#'
#' @param bam.datapath
#' @param param
#' @param h5name
#' @param h5PathOut
#'
#' @return
#' @keywords internal
#'
#' @examples
bamMatching <- function (bam.datapath, param, h5name='h5file', h5PathOut=''){

    h5path<-h5PathOut
    bamPathNoPrefix  <- gsub('.bam', '', bam.datapath)
    #prefixNam<-paste0(bamPathNoPrefix, '.h5')
    prefixNam<-paste0(h5PathOut, h5name, '.h5')
    hs.param<-param
    diagnostics <- diffHic::preparePairs(bam.datapath, hs.param, file=prefixNam, dedup=TRUE, minq=10)

    ##===getPairData inseriscilo in una funzione per il QC
    diags <- diffHic::getPairData(prefixNam, hs.param)
    ##show results

    var_showw<-capture.output(as.data.frame(show (diagnostics)))
    print (var_showw)
    varshoww2<- strsplit(var_showw[2], " ")
    varshoww3<- strsplit(var_showw[3], " ")
    varshoww2<- as.vector (varshoww2[[1]])
    varshoww2<- varshoww2[varshoww2 != ""]
    varshoww3<- as.vector (varshoww3[[1]])
    varshoww3<- varshoww3[varshoww3 != ""]

    varmatrixpairs<- matrix(nrow=2,ncol=4)

    #print (varshoww2)
    #print (varshoww3)

    varmatrixpairs[1,]<- varshoww2
    varmatrixpairs[2,]<- varshoww3
    print ('varmatrixpairs')
    print (varmatrixpairs)

    #print (varmatrixpairs)
    varshoww6<- strsplit(var_showw[6], " ")
    varshoww7<- strsplit(var_showw[7], " ")
    varshoww6<- as.vector (varshoww6[[1]])
    varshoww6<- varshoww6[varshoww6 != ""]
    varshoww7<- as.vector (varshoww7[[1]])
    varshoww7<- varshoww7[varshoww7 != ""]

    varmatrixpairs2<- matrix(nrow=2,ncol=2)

    varmatrixpairs2[1,]<- varshoww6
    varmatrixpairs2[2,]<- varshoww7
    print('varmatrixpairs2')
    print(varmatrixpairs2)


    #varshoww9<- strsplit(var_showw[9], " ")
    varshoww9<- var_showw[9]
    varshoww10<-var_showw[10]
    #varshoww10<- strsplit(var_showw[10], " ")
    #varshoww9<- as.vector (varshoww6[[1]])
    #varshoww9<- varshoww6[varshoww6 != ""]
    #varshoww10<- as.vector (varshoww10[[1]])
    #varshoww10<- varshoww7[varshoww10 != ""]

    varmatrixpairs3<- matrix(nrow=2,ncol=1)


    varmatrixpairs3[1,]<- varshoww9
    varmatrixpairs3[2,]<- varshoww10
    print('varmatrixpairs3')
    print(varmatrixpairs3)

    #print (varshoww6)
    #print (varshoww7)
    #print ('varmatrixpairs fatta')
    #print (varmatrixpairs)


    #print (varmatrixpairs2)
    varshoww13<- strsplit(var_showw[13], " ")
    varshoww14<- strsplit(var_showw[14], " ")
    varshoww13<- as.vector (varshoww13[[1]])
    varshoww13<- varshoww13[varshoww13 != ""]
    varshoww14<- as.vector (varshoww14[[1]])
    varshoww14<- varshoww14[varshoww14 != ""]
    print (varshoww13)
    print (varshoww14)
    varmatrixpairs4<- matrix(nrow=2,ncol=4)
    varmatrixpairs4[1,]<- varshoww13
    varmatrixpairs4[2,]<- varshoww14
    print('varmatrixpairs4')
    print (varmatrixpairs4)


    print ('varmatrixpairs fatta')
    #print (varmatrixpairs3)


    #print (varmatrixpairs2)

    #write.table (varmatrixpairs,file='varma', sep='\t',row.names=FALSE, col.names= FALSE )
    #write.table (varmatrixpairs2,file='varma2', sep='\t',row.names=FALSE, col.names= FALSE )
    #write.table (varmatrixpairs3,file='varma3', sep='\t',row.names=FALSE, col.names= FALSE )
    varmaSum<-c(varmatrixpairs,varmatrixpairs2,varmatrixpairs3,varmatrixpairs4)
    write.table (varmaSum,
                 file=paste0(h5PathOut, h5name, '_bamMatching_report'), sep='\t',row.names=FALSE, col.names= FALSE, quote=FALSE )

    #varmatrixList<- list (var_showw,varmatrixpairs,varmatrixpairs2,varmatrixpairs3)

    return (diagnostics)
}
