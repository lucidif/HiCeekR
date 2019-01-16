
#' HCRunlist
#'
#' @param x
#'
#' @return
#' @keywords internal
#'
#' @examples
HCRunlist <- function (x)
{
    ## do.call(c, ...) coerces factor to integer, which is undesired
    x1 <- x[[1L]]
    if (is.factor(x1)) {
        structure(unlist(x), class = "factor", levels = levels(x1))
    } else {
        do.call(c, x)
    }
}


#' bamToDataframe
#'
#' @param bamPath
#' @param chromosome
#' @param start
#' @param end
#'
#' @return
#' @keywords internal
#'
#' @examples
bamToDataFrame <- function (bamPath, chromosome, start, end){
    #read in entire BAM file

    #set Parameters
    maxBT<- end
    minBT<- start
    library("IRanges")
    #test<- paste0 ('wh <- RangesList(', chromosome,' = IRanges(minBT, maxBT))')
    wh <- GenomicRanges::GRanges( chromosome , IRanges(minBT, maxBT))
    #eval(parse(text=test))
    what <- c("rname","pos", "qwidth")
    parameter <- Rsamtools::ScanBamParam(which=wh, what=what)

    #read in entire BAM file
    bam <- Rsamtools::scanBam(bamPath, param=parameter)

    #names of the BAM fields
    print(names(bam[[1]]))

    #distribution of BAM flags
    print(table(bam[[1]]$flag))

    #function for collapsing the list of lists into a single list
    #as per the Rsamtools vignette

    #store names of BAM fields
    bam_field <- names(bam[[1]])

    #go through each BAM field and unlist
    list <- lapply(bam_field, function(y) HCRunlist(lapply(bam, "[[", y)))

    #store as data frame
    bam_df <- do.call("data.frame", list)
    #bam_dfTT<<-bam_df
    names(bam_df) <- bam_field

    print(dim(bam_df))

    return(bam_df)


}

# bamToDataFrame <- function (bamPath, chromosome, start, end){
#
#
#     #maxBT<- max (bintabSubset[,4])
#     #minBT<- min (bintabSubset[,3])
#     maxBT<- end
#     minBT<- start
#     print (c('min',minBT,'max',maxBT))
#
#     ##questo qui è un po un accrocchio, va corretto
#     library("IRanges")
#
#     #test<- paste0 ('wh <- RangesList(', chromosome,' = IRanges(minBT, maxBT))')
#
#     #test<- paste0 ('wh <- RangesList(', chromosome,' = IRanges(minBT, maxBT))')
#     #    Warning: The RangesList() constructor is deprecated. Please coerce to
#     #    IRangesList instead e.g. do 'as(list(x1, x2), "IRangesList")'
#     #    instead of 'RangesList(x1, x2)'. Alternatively, you can use the
#     #    IRangesList() constructor e.g. 'IRangesList(x1, x2,
#     #    compress=FALSE)'. See '?IRangesList' for more information.
#     #wh <- as(list(minBT, maxBT), "IRangesList")
#
#     #test<-paste0('wh<-as(',chromosome,'=list(minBT, maxBT), "IRangesList")')
#     #eval(parse(text=test))
#     #wh<-as(list())
#
#     #gr <- GenomicRanges::GRanges(seqnames = chromosome, strand = "*",
#     #                            ranges = IRanges(start = minBT, end = maxBT))
#     #grTT <<- GenomicRanges::GRanges(seqnames = chromosome, strand = "*",
#     #                             ranges = IRanges(start = minBT, end = maxBT))
#
#     test<-paste0("wh<-IRangesList(",chromosome,"=IRanges(",minBT,",",maxBT,"))")
#     eval(parse(text=test))
#
#     what <- c("rname","pos", "qwidth")
#     parameter <- Rsamtools::ScanBamParam(which=wh, what=what)
#     parameterTT <<- Rsamtools::ScanBamParam(which=wh, what=what)
#
#     bam <- Rsamtools::scanBam(bamPath, param=parameter)
#     bamTT<<-Rsamtools::scanBam(bamPath, param=parameter)
#
#     bam <- unname(bam)
#     elts <- setNames(Rsamtools::bamWhat(parameter),
#                      Rsamtools::bamWhat(parameter))
#     eltsTT<<-elts
#
#     lst <- lapply(elts, function(elt) HCRunlist(lapply(bam, "[[", elt)))
#     lstTT<<-lst
#
#
#     ## for data frame the command is :: do.call("DataFrame", lst)
#     frame<-do.call("DataFrame", lst)
#     frameTT<<-frame
#     return (frame)
#     ##in frame[1,] there are chr , pos , weigth of a read
#
#
# }
