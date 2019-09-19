#' HiCsegTADs
#'
#' @param matrixPath
#' @param outpath
#' @param distrib
#' @param model
#'
#' @return
#' @keywords internal
#'
#' @examples
HiCsegTADs<-function(matrixPath, outpath, distrib="G", model="D", Kmax=10){

    cntTable<-read.table(file=matrixPath, sep="\t", header=TRUE, check.names = FALSE)

    row.names(cntTable)<-cntTable[,1]

    cntTable<-cntTable[,-1]
    cntTable<-as.matrix(cntTable)

    #data(matrix)
    n=length(cntTable[,1])
    Kmax=Kmax
    res=HiCseg::HiCseg_linkC_R(n, Kmax, distrib=distrib, cntTable, model=model)
    #res=HiCseg_linkC_R(n,Kmax,"G",matrix,"D")
    print(res)

    chr<-c()
    start<-c()
    end<-c()
    for(i in 1:length(res$t_hat)){
        stringa<-rownames(cntTable)[res$t_hat[i]]
        split1<-strsplit(stringa,":")
        chr[i]<-split1[[1]][1]
        split2<-strsplit(split1[[1]][2],"-")
        start[i]<-split2[[1]][1]
        end[i]<-split2[[1]][2]
    }

    write.table(data.frame(chr,start,end), file=paste0(outpath, "/HiCsegTADs.bed"), sep="\t", quote=FALSE, col.names=FALSE, row.names = FALSE)

    return(res)
}
