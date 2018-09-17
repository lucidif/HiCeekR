
#' Run HiCeekR
#'
#' Run HiCeekR Shiny application
#'
#'
#' @return None
#'
#' @example
#'
#'
#' @export
HiCeekR<-function (configFilePath="./HCR.config"){

    ####shiny::runApp(appDir="R/")
    # if (file.exists(configFilePath)==TRUE){
    # workingDir<-as.character((read.table(configFilePath))[1,1])
    # ####confTable<-matrix(ncol=1,nrow=2)
    # ####confTable[1,1]<-workingDir
    # #####confTable[2,1]<-projectName
    # write.table(workingDir,"HCRtmp.config", col.names=FALSE, row.names=FALSE,
    #             quote=FALSE, sep="\t")
    # }
    shiny::shinyApp(server=server, ui=ui)
    #options = list(display.mode = 'showcase')
}


#' HCRprjManager
#'
#' @param configFilePath
#'
#' @return
#' @keywords internal
#'
#' @examples
HCRprjManager<-function(configFilePath="./HCR.config"){
    workingDir<-as.character((read.table(configFilePath))[1,1])
    shiny::shinyApp(server=prjMan_server(HCRwdDir=workingDir), ui=prjMan_UI)

}





#' moduleTest
#'
#' Test the module without HiCeekR
#'
#' @param moduleName character variable that contain name of module
#'
#' @return shinyApp in web browser
#' @keywords internal
#'
#' @examples
moduleTest=function(moduleName){

    uiTest <- fluidPage(

        shiny::fluidRow(
            shiny::column(12,
              shiny::navbarPage("HiCeekR", id='mainNav',
                    shiny::tabPanel ('test navbar')
                    )
                )
            ),

        fluidRow(column(12,
                        uiOutput("moduleSlot")
                        ))
    )
    serverTest <- function(input, output, session, moduleNam=moduleName) {

        #moduleNam<-"moduleTemplate"
        tool<-moduleName
        moduleUI<-paste0 (moduleNam, '_UI')
        moduleLoad_moduleTemplate<-paste0 (moduleNam,'_Server')
        output$moduleSlot<-shiny::renderUI ({
            get(moduleUI)(tool,label=moduleNam)
        })
        loadApp_prjSettings<-shiny::callModule(get(moduleLoad_moduleTemplate),
                                               tool)

    }
    shiny::shinyApp(server=serverTest, ui=uiTest)
}
