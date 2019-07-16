#' importAnnotation
#'
#' @param name
#' @param path
#'
#' @return
#' @keywords internal
#'
#' @examples
importAnnotation= function (name ,path){
    #annoPath='/home/lucio/rPrj/1.HiCeekR_wd/Annotation/gencode.v24.primary_assembly.annotation.gtf'     #home
    annoPath=paste0(path,name)
    annoTab=HCRread('',annoPath, header=FALSE)
    #annoTab=subset(annoTab, rownames(annoTab)=='chr14')
    #annoTab=subset(annoTab, annoTab[,2]=='gene'|annoTab[,2]=='transcript')
    annoTab=subset(annoTab, annoTab[,2]=='gene')
    annoTab[,1]=rownames(annoTab)

    ##IL TUO FILE DI ANNOTAZIONE DEVE ESSERE FATTO COSI'
    ##ensembl_gene_id , chromosome_name , gene_start, gene_end, gene_name

    myStd<- matrix(ncol=5,nrow=length(annoTab[,1]))
    colnames(myStd)<-c('Ensembl_gene_id', 'chromosome_name' , 'gene_start' , 'gene_end' , 'gene_name' )
    ##attenzione adesso il cromosoma è già nella notazione 'chrN' (N è un numero)
    myStd[,2]<-annoTab[,1]
    myStd[,3]<-annoTab[,3]
    myStd[,4]<-annoTab[,4]



    ##riempi la colonna 1 e 5
    test<- annoTab[1,8]
    diviso<-strsplit(test, ';' ,fixed=TRUE)
    ensembl<-gsub('gene_id ','',diviso[[1]][1])
    ##se vuoi rimuovere anche la verisione .N proverò prima a non rimuovere e vedere come va il match
    diviso2<-strsplit(ensembl, '.' ,fixed=TRUE)
    ensembl2<-diviso2[[1]][1]

    gene_name<-gsub(' gene_name ','',diviso[[1]][grep(' gene_name ',diviso[[1]])])


    #alldiviso<-strsplit(annoTab[,8], ';', fixed=TRUE)

    #alldiviso<-rep(gsub)

    for (i in 1:length(annoTab[,1])){
        #print (paste0(i,'/',length(annoTab[,1])))
        test<- annoTab[i,8]
        diviso<-strsplit(test, ';' ,fixed=TRUE)
        ensembl<-gsub('gene_id ','',diviso[[1]][1])
        ##se vuoi rimuovere anche la verisione .N proverò prima a non rimuovere e vedere come va il match
        diviso2<-strsplit(ensembl, '.' ,fixed=TRUE)
        ensembl2<-diviso2[[1]][1]
        gene_name<-gsub(' gene_name ','',diviso[[1]][grep(' gene_name ',diviso[[1]])])
        myStd[i,1]<-ensembl2
        myStd[i,5]<-gene_name
        print (paste0('i:',i,'/',length(annoTab[,1])))
    }

    #HCRwrite (myStd, paste0(name,'_HiCeekRstd') ,path)
    return(myStd)

}
