configFilePath="./HCR.config"

if (file.exists(configFilePath)==TRUE){
    workingDir<-as.character((read.table(configFilePath))[1,1])
    ####confTable<-matrix(ncol=1,nrow=2)
    ####confTable[1,1]<-workingDir
    #####confTable[2,1]<-projectName
    write.table(workingDir,"HCRtmp.config", col.names=FALSE, row.names=FALSE,
                quote=FALSE, sep="\t")
}
