
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
HiCeekR=function (){
    #shiny::runApp(appDir="R/")
    shiny::shinyApp(server=server, ui=ui)
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
