
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


    #maxBT<- max (bintabSubset[,4])
    #minBT<- min (bintabSubset[,3])
    maxBT<- end
    minBT<- start
    print (c('min',minBT,'max',maxBT))

    ##questo qui è un po un accrocchio, va corretto
    test<- paste0 ('wh <- RangesList(', chromosome,' = IRanges(minBT, maxBT))')

    eval(parse(text=test))
    what <- c("rname","pos", "qwidth")
    parameter <- ScanBamParam(which=wh, what=what)
    bam <- scanBam(bamPath, param=parameter)

    bam <- unname(bam)
    elts <- setNames(bamWhat(parameter), bamWhat(parameter))
    lst <- lapply(elts, function(elt) HCRunlist(lapply(bam, "[[", elt)))

    ## for data frame the command is :: do.call("DataFrame", lst)
    frame<-do.call("DataFrame", lst)
    return (frame)
    ##in frame[1,] there are chr , pos , weigth of a read


}
