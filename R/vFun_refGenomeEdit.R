#' refGenomeEdit
#'
#' @param refGenome
#' @param enzyme
#' @param overhang
#' @param name
#' @param cutGenomeOutDir
#'
#' @return
#' @keywords internal
#'
#' @examples
refGenomeEdit <- function(refGenome,
                          enzyme,
                          overhang,
                          name='refGenFrags',
                          cutGenomeOutDir=''){
    #temp<-eval(parse(text=refGenome))
    temp<-refGenome
    hsfrag <- diffHic::cutGenome(temp, enzyme, overhang)
    hs.param <- diffHic::pairParam(hsfrag)
    #print ('ho fatto hsfrag!!!')
    #print (hsfrag)

    kriss<- as.data.frame (hsfrag)
    prefix<- '.cutGenome'
    nameFinal<- paste0(name, prefix)
    #write.table (kriss, file=nameFinal , sep='\t')
    HCRwrite (kriss, file=nameFinal, path=cutGenomeOutDir)
    #print ('kriss ok')

    var_show<-capture.output(as.data.frame(show (hs.param)))
    #write.table (var_show, file ='refencenceInfo')

    #print (var_show)
    #return(list(hs.param,hsfrag))
    return(hs.param)
}
