#vFun_javaNs

#' javaNs
#'
#' @param ns
#' @param name
#' @param type
#' @param condition
#'
#' @return
#' @keywords internal
#'
#' @examples
javaNs<-function(ns, name, type="input",condition){


    #result<-paste0("input['", ns("fixValue"), "'] == true ")
    result<-paste0(type,"['",ns(name),"'] ",condition)
    return(result)

}
