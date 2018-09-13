readContactMatrix <- function (matrixPath){
    Mama<-read.table (matrixPath
                      , sep='\t'
                      , header=TRUE
                      , check.names=FALSE
    )
    #View(Mama)
    rownames(Mama)<-Mama[,1]
    Mama<-Mama[,-1]
    #colnames(Mama)<-rownames(Mama)
    arr<- colnames(Mama)
    arr<- sub('X','',arr, fixed=TRUE )
    colnames (Mama)<-arr

    return (Mama)

}
