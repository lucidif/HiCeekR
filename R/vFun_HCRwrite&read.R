


#' HCRwrite
#'
#' @param tableToWrite
#' @param file
#' @param path
#' @param append
#' @param quote
#' @param sep
#' @param eol
#' @param na
#' @param dec
#' @param row.names
#' @param col.names
#' @param qmethod
#' @param fileEncoding
#' @param standard.row.names
#' @param extension
#'
#' @return
#' @keywords internal
#'
#' @examples
HCRwrite<- function (tableToWrite,file, path ="" , append = FALSE, quote = FALSE, sep = "\t",
                     eol = "\n", na = "NA", dec = ".", row.names = FALSE,
                     col.names = TRUE, qmethod = c("escape", "double"),
                     fileEncoding = "", standard.row.names=FALSE, extension=TRUE){

    if (extension == TRUE ) {
        tosave_name<-paste0 (path, file, '.tsv')
    } else {tosave_name<-paste0 (path,file)}


    if (row.names==TRUE){
        ##attenzione che tutto funzioni qui era colnames ed ora hai sostituito rownames
        index<- rownames(tableToWrite)
        #print (c('index',index))
        #View (index)
        tableToWrite<-cbind (index,tableToWrite)

    }

    write.table (x=tableToWrite, file=tosave_name, append=append, quote=quote, sep=sep, eol=eol,
                na=na, dec=dec, row.names=standard.row.names , col.names=col.names,qmethod=qmethod, fileEncoding=fileEncoding)


}


#HCRread==================================================

#' HCRread
#'
#' @param file
#' @param path
#' @param header
#' @param sep
#' @param quote
#' @param dec
#' @param numerals
#' @param row.names
#' @param col.names
#' @param as.is
#' @param na.strings
#' @param colClasses
#' @param nrows
#' @param skip
#' @param check.names
#' @param fill
#' @param strip.white
#' @param blank.lines.skip
#' @param comment.char
#' @param allowEscapes
#' @param flush
#' @param stringsAsFactors
#' @param fileEncoding
#' @param encoding
#' @param text
#' @param skipNul
#' @param asMatrix
#'
#' @return
#' @keywords internal
#'
#' @examples
HCRread<- function (file, path , header = TRUE, sep = "\t", quote = "\"'",
                    dec = ".", numerals = c("allow.loss", "warn.loss", "no.loss"),
                    row.names, col.names, as.is = !stringsAsFactors,
                    na.strings = "NA", colClasses = NA, nrows = -1,
                    skip = 0, check.names = TRUE, fill = !blank.lines.skip,
                    strip.white = FALSE, blank.lines.skip = TRUE,
                    comment.char = "#",
                    allowEscapes = FALSE, flush = FALSE,
                    stringsAsFactors = default.stringsAsFactors(),
                    fileEncoding = "", encoding = "unknown", text, skipNul = FALSE,
                    asMatrix=TRUE
){

    name<- paste0 (path,file)

    prime<-read.table (name,header = header, sep = sep, quote = quote,
                       dec = dec, numerals = numerals,
                       row.names=row.names, col.names=col.names, as.is = as.is,
                       na.strings = na.strings, colClasses = colClasses, nrows = nrows,
                       skip = skip, check.names = check.names , fill = fill,
                       strip.white = strip.white, blank.lines.skip = blank.lines.skip,
                       comment.char = comment.char,
                       allowEscapes = allowEscapes, flush = flush,
                       stringsAsFactors = stringsAsFactors,
                       fileEncoding = fileEncoding, encoding = encoding, text=text, skipNul = skipNul
    )


    if (asMatrix==TRUE){

        imn<- as.matrix(prime)
        rownames(imn) <- prime[,1]
        imn<- imn[,-1]
        #print (paste0('length(colnames(prime)=',length(colnames(prime))))
        imn<-as.matrix(imn)

        if (length(colnames(prime))>=2){
            colnames(imn)<-colnames(prime)[2:length(colnames(prime))]
        }

        #View (head(imn))


    } else {

        imn<- prime
        rownames(imn) <- prime[,1]
        imn<- prime[,-1]
        imn<-as.matrix(imn)

        if (length(colnames(prime))>=2){
            colnames(imn)<-colnames(prime)[2:length(colnames(prime))]
        }



    }
    #rownames(imn) <- prime[,1]
    return (imn)

}
