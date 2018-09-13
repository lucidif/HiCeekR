##heatmodule

#======================
#library(shiny)
#library(data.table)
#library(gplots)
#library(heatmaply)
#library(tools)
#======================

# Module UI function
#' TADsV2_Visualization_UI
#'
#' @param id
#' @param label
#'
#' @return
#' @keywords internal
#'
#' @examples
TADsV2_Visualization_UI <- function(id, label = "Visualization") {
    heatens <- shiny::NS(id)


    shiny::fluidPage (

        shiny::tabsetPanel (

            shiny::tabPanel ('layout',
                      #===============================================Main Settings========================================================
                      shiny::fluidRow(
                        shiny::column(12,
                            shiny::wellPanel ('Main Settings',

                                shiny::fluidRow (
                                    shiny::column (2,
                                        shiny::br(),
                                        shiny::helpText(h5('contact matrix')
                                                        )
                                                    ),

                                    shiny::column (8,
                                        shiny::fileInput (heatens('loadMatrix'),
                                                            label='')
                                                        ),

                                    shiny::column(2, #br(),

                                        shiny::fluidRow(
                                            shiny::column(12,
                                                shiny::checkboxInput(heatens('dataSub'),
                                                                     label='subset data',
                                                                     value=FALSE)
                                                                     )
                                                                 ),

                                        shiny::fluidRow(
                                            shiny::column (12,
                                                shiny::checkboxInput(heatens('check_corrPlot'),
                                                                    label='Correlation Plot',
                                                                    value=FALSE)
                                                                    )
                                                        )

                                                    )

                                                # column (2, br(),
                                                #       actionButton (heatens('loadMatrix_Button'), label='set this')
                                                #         )
                                                     )





                      )))



                      ##slots-----------------------------------------------
                      #slot1 #===============================================
                      ,shiny::fluidRow(
                          shiny::column(12,
                            shiny::wellPanel('slot 1',
                                shiny::fluidRow (
                                    shiny::column (3,
                                        shiny::checkboxInput (heatens('activeSlot1'),
                                                            label='active',
                                                            value=FALSE ) ),
                                    shiny::column (6,
                                        shiny::fileInput (heatens('fileInput_slot1'),
                                                        label='file') )
                                                    ),
                                shiny::fluidRow (shiny::column(12,
                                                    shiny::uiOutput(heatens('pos_slot1'))
                                                    )
                                                    ))))
                      #slot2 #=================================================
                      ,shiny::fluidRow(shiny::column(12,
                            shiny::wellPanel('slot 2',
                                shiny::fluidRow (
                                    shiny::column (3,
                                        shiny::checkboxInput(heatens('activeSlot2'),
                                                            label='active',
                                                            value=FALSE ) ),
                                    shiny::column(6,
                                        shiny::fileInput(heatens('fileInput_slot2'),
                                                         label='file'))
                                                    ),
                                        shiny::fluidRow (
                                            shiny::column (12,
                                                shiny::uiOutput(heatens('pos_slot2')))
                                                    ))))
                      #slot3 #=================================================
                      ,shiny::fluidRow(column(12,wellPanel('slot 3',
                            shiny::fluidRow (
                                shiny::column (3,
                                    shiny::checkboxInput(heatens('activeSlot3'),
                                                        label='active',
                                                        value=FALSE ) ),
                                shiny::column (6,
                                    shiny::fileInput (heatens('fileInput_slot3'),
                                                      label='file') )
                                                    ),
                            shiny::fluidRow (
                                shiny::column (12,
                                    shiny::uiOutput(heatens('pos_slot3')))
                                                    ))))
                      #slot4  #=================================================
                      ,shiny::fluidRow(
                          shiny::column(12,
                            shiny::wellPanel('slot 4',
                                shiny::fluidRow (
                                    shiny::column (3,
                                        shiny::checkboxInput (heatens('activeSlot4'),
                                                            label='active',
                                                            value=FALSE ) ),
                                    shiny::column (6,
                                        shiny::fileInput (heatens('fileInput_slot4'),
                                                          label='file') )
                                                    ),
                                shiny::fluidRow (
                                    shiny::column (12,
                                        shiny::uiOutput(heatens('pos_slot4')))
                                                    ))))

                      #slot5 #===================================================
                      ,shiny::fluidRow(
                          shiny::column(12,
                            shiny::wellPanel('slot 5',
                                shiny::fluidRow (
                                    shiny::column (3,
                                        shiny::checkboxInput (heatens('activeSlot5'),
                                                            label='active',
                                                            value=FALSE ) ),
                                    shiny::column (6,
                                        shiny::fileInput (heatens('fileInput_slot5'),
                                                        label='file') )
                                                    ),
                                shiny::fluidRow (
                                    shiny::column (12,
                                        shiny::uiOutput(heatens('pos_slot5')))
                                                    ))))


                      #====================================================

            ),

            shiny::tabPanel ('Visual',

                shiny::fluidRow( ##visual settings
                    shiny::column (12,

                        shiny::uiOutput (heatens('regionsPanel'))

                          )


                      ),


                      #===========================



                shiny::fluidRow (

                    shiny::column (3,
                        shiny::fluidRow(
                            shiny::column (12,
                                shiny::wellPanel(
                                    shiny::fluidRow (
                                        shiny::column(12,
                                            shiny::actionButton (
                                                heatens('loadMatrix_Button'),
                                                label='Visualize'))),
                                    shiny::fluidRow (
                                        shiny::column(12,
                                            shiny::br(),
                                            shiny::helpText('axis names:'))),
                                    shiny::fluidRow (
                                        shiny::column(4,
                                            shiny::checkboxInput(heatens("chrNameAxis"),
                                                                 label = "chr",
                                                                 value = TRUE)),
                                        shiny::column(4,
                                            shiny::checkboxInput(
                                                    heatens("startNameAxis"),
                                                    label = "start",
                                                    value = TRUE)),
                                        shiny::column(4,
                                            shiny::checkboxInput(
                                                heatens("endNameAxis"),
                                                label = "end",
                                                value = FALSE))
                                                         ),
                                    shiny::fluidRow (
                                        shiny::column (12,
                                            shiny::sliderInput(
                                                heatens("nticksSlider") ,
                                                label = h5("names limit"),
                                                min = 4,
                                                max = 200,
                                                value = 14)
                                                    )
                                                ),
                                    shiny::fluidRow (
                                        shiny::column (12,
                                            shiny::sliderInput(
                                                heatens("biasSlider") ,
                                                label = h5("bias"),
                                                min = 1,
                                                max = 10,
                                                value = 2
                                                )
                                            )
                                        )

                                                     ) #fine wellPanel
                                  )),
                        shiny::fluidRow (
                            shiny::column (12,
                                shiny::plotOutput (heatens('plotSlot2'),
                                                   width='100%'))
                                  )
                          ),

                    shiny::column (9,

                        shiny::fluidRow (
                            shiny::column(12
                                #plotOutput (heatens('plotSlot2'), width='100%')
                                #plotlyOutput (heatens('plotSlot'), width='100%')
                                  )),
                        shiny::fluidRow (
                            shiny::column (12,
                                    #plotOutput (heatens('plotSlot2'), width='100%')
                                    plotly::plotlyOutput (heatens('plotSlot'),
                                                  width='100%')
                                  ))


                          )

                      )
            )

        )


    ) #fluidPage







}

#==================================================================================


# Module server function

#' TADsV2_Visualization_Server
#'
#' @param input
#' @param output
#' @param session
#' @param stringsAsFactors
#' @param wdPath
#'
#' @return
#' @keywords internal
#'
#' @examples
TADsV2_Visualization_Server <- function(input, output, session,
                                        stringsAsFactors, wdPath) {

print("htm module start")
#===========================
    # library(shiny)
    # library(data.table)
    # library(gplots)
    # library(heatmaply)
    # library(tools)
#==========================
    #source('HiCeekR_base_fun.R')
    #source('diffHic_fun.R')
    #source('heatmaply_fun.R')
    #source ('fun.R')
    RV<-shiny::reactiveValues(dataChr="none")
    heatens<-session$ns

    ##================================when load a matrix extract same
    ##================================information from it
    shiny::observeEvent (input$loadMatrix , {
        dataRegions<-regionsInMatrix (input$loadMatrix$datapath, separate=TRUE,on='row')
        RV$dataChr<- unique(dataRegions[,1])
        print ("after dataChr")
        print (paste0("after dataChr   ", RV$dataChr))
        #dataChrTT<<-RV$dataChr
        #dataRegionsTT<<-dataRegions
        #print (dataChr)
        # matrixChromosomes <<- HiClass$new ('chromosomes extract from matrix of interest', dataChr)
        # anchorchr.unique<<- matrixChromosomes$hisave
    })
    #====================================================================================

    ##================================button for plotting
    #(UNA VOLTA PREMUTO ELABORA TUTTI I DATI IMPOSTATI IN LAYOUT E SETTINGS
    #E CACCIA IL PLOT FINALE)
    shiny::observeEvent ( input$loadMatrix_Button , {

        filePath<- input$loadMatrix$datapath
        variableName<- readContactMatrix (filePath)
        maxRange<-max(variableName, na.rm=TRUE)
        minRange<-min(variableName, na.rm=TRUE)
        #View (variableName)

        ##prima di fargli calcolare la data.matrix fagli fare tutte le operazioni di subset se la spunta per il subset (dataSub) è attiva

        ##1) prima per cromosoma

        if (input$dataSub==TRUE){
            #chrInt<- regionsInMatrix (ma, as.Path=FALSE )
            #print (paste0('input$xChr',input$xChr))
            #xChr Sub  così non lo puoi fare! devi estrarre direttamente le regioni complete quindi se vuoi estrarre un cromosoma
            #devi estratti dalla matrice tutte le regionsi che nel nome (chr:start-end) abbiano il chr di interesse
            #modifica findRegionInBint in modo tale che si può fermare al solo cromosoma ! ok fatto ora hai l'opzione only.chr

            # #chromosome X
            # regionsInt<- regionsInMatrix (variableName, as.Path=FALSE )
            # reg<-findRegionInBint (regionsInt, id=FALSE, imported=TRUE, chr=input$xChr ,only.chr=TRUE)
            # regions<-rep(paste0(reg[,1],':',reg[,2],'-',reg[,3]))
            # variableName<- variableName[,regions]
            ##------------end chr x

            # #chromoseme Y
            # regionsInt<- regionsInMatrix (variableName, as.Path=FALSE )
            # reg<-findRegionInBint (regionsInt, id=FALSE, imported=TRUE, chr=input$yChr ,only.chr=TRUE)
            # regions<-rep(paste0(reg[,1],':',reg[,2],'-',reg[,3]))
            # variableName<- variableName[regions,]
            # #------------end chr y

            #region X subset
            if (input$xType==TRUE){
                regionsInt<- regionsInMatrix(variableName, as.Path=FALSE )
                #print ('regionsInt')
                #print (regionsInt)
                reg<-findRegionInBint(regionsInt, id=FALSE, imported=TRUE,
                                    chr=input$xChr , start =as.numeric(input$xStart),
                                    end=as.numeric(input$xEnd) )
                regions<-rep(paste0(reg[,1],':',reg[,2],'-',reg[,3]))
                #print ('==========REGIONS XSUB============')
                #print (regions)
                variableName<- variableName[,regions]

                # regX<-regions
                # print ('regX')
                # print (regX)

            } else {

                #chromosome X
                regionsInt<- regionsInMatrix (variableName, as.Path=FALSE )
                reg<-findRegionInBint (regionsInt, id=FALSE, imported=TRUE,
                                       chr=input$xChr ,only.chr=TRUE)
                regions<-rep(paste0(reg[,1],':',reg[,2],'-',reg[,3]))

                variableName<- variableName[,regions]

            }
            #------------end chr X

            #region Y subset
            if (input$xType==TRUE){
                #variableNameTT<<-variableName
                regionsInt<- regionsInMatrix (variableName, as.Path=FALSE )
                reg<-findRegionInBint (regionsInt, id=FALSE, imported=TRUE,
                                        chr=input$yChr ,
                                        start =as.numeric(input$yStart),
                                        end=as.numeric(input$yEnd) )
                regions<-rep(paste0(reg[,1],':',reg[,2],'-',reg[,3]))
                variableName<- variableName[regions,]

                # regY<-regions
                # print ('regY')
                # print (regY)

            } else {
                #chromosome Y
                regionsInt<- regionsInMatrix (variableName, as.Path=FALSE )
                reg<-findRegionInBint(regionsInt, id=FALSE,
                                        imported=TRUE,
                                        chr=input$yChr ,
                                        only.chr=TRUE)
                regions<-rep(paste0(reg[,1],':',reg[,2],'-',reg[,3]))
                variableName<- variableName[regions,]
                #------------end chr y
            }
            #-----------end chr Y


        }

        #View (variableName)
        rawmatrix<-as.data.frame (variableName)
        rawmatrix<-data.matrix (rawmatrix)
        naNames <-'NA'
        maNames <- as.array (colnames(rawmatrix))
        maNames <- c (naNames, maNames)

        print (paste0('START SEARCH TICKS'))
        ticksY<-searchTicks(rawmatrix,axis='row', numberOfTicks=input$nticksSlider)
        print ('END SEARCH TICKS, START TICK NAMES')
        ticksNamesY<-ticksNames(ticksY, rawmatrix , axis='row', simMirror=TRUE)
        print ('END TICK NAMES')

        ticksX<-searchTicks(rawmatrix,axis='col', numberOfTicks=input$nticksSlider)
        ticksX2<-ticksX
        ticksNamesX<-ticksNames(ticksX, rawmatrix , axis='col')

        ax_SetY<- list(
            #title = '',
            zeroline = TRUE,
            showline = TRUE,
            showticklabels = TRUE,
            showgrid = TRUE
            ,tickmode='array'
            #,range= c(1,10)
            ,tickvals= ticksY
            ,ticktext= tickOptions(ticksNamesY,
                                    chr=input$chrNameAxis,
                                    start=input$startNameAxis,
                                    end=input$endNameAxis )
        )

        ax_SetX<- list(
            #title = "",
            #side='top',
            zeroline = TRUE,
            showline = TRUE,
            showticklabels = TRUE,
            showgrid = TRUE
            ,tickmode='array'
            #,ticks='outside'
            ,mirror=TRUE
            #,range= c(1,10)  ##dagli la start row o col e la end row o col!!!!
            ,tickvals= ticksX
            ,ticktext= tickOptions(ticksNamesX,
                                    chr=input$chrNameAxis,
                                    start=input$startNameAxis,
                                    end=input$endNameAxis)
        )

        ax_SetX2<- list(
            #title = "",
            #side='top',
            zeroline = TRUE,
            showline = TRUE,
            showticklabels = TRUE,
            showgrid = TRUE
            ,tickmode='array'
            #,ticks='outside'
            ,mirror=TRUE
            #,range= c(1,10)  ##dagli la start row o col e la end row o col!!!!
            ,tickvals= ticksX2
            ,ticktext= tickOptions(ticksNamesX,
                                    chr=input$chrNameAxis,
                                    start=input$startNameAxis,
                                    end=input$endNameAxis)
        )



        ##-------------------------------genero la heatmap
        require("dplyr")
        print (paste0('minRange,maxRange:',minRange,',',maxRange))
        q<- heatmaply::heatmaply(rawmatrix, k_row = 30, k_col = 4, srtCol = 45,
                      colors= colorRampPalette(c('white','red','red1','red2',
                                                 'red3','red4') ,
                                               bias=input$biasSlider),
                      limits=c(as.numeric(minRange),as.numeric(maxRange)),
                      Rowv=FALSE, Colv=FALSE, na.value = 'white',
                      #dendrogram=FALSE,
                      titleX=FALSE,
                      titleY=TRUE
                      #,draw_cellnote=TRUE,
                      #plot_method='plotly'
                      #,cellnote_textposition="top center"

        )   %>% plotly::layout(
            xaxis=ax_SetX2
        )
        ##-------------------------------------end heatmap

        ##-----------------------------------working on pca or other barplot

        #valutate active slot

        slots<- c('heatmap')

        #activeSlot control #==================================
        #activeSlot1 #======================================
        if (input$activeSlot1==TRUE){
            slots<- c(slots,'slot1')
            #ricorda che deve avere lo stesso nome della variabile
            #in cui e conservato il plot
            toPlot<-HCRread (file='', path=input$fileInput_slot1$datapath)
            #View (head(toPlot))
            subregion<-colnames (rawmatrix)
            #print ('==========col names of rawmatrix==========')
            #print (subregion)

            ##==============================one col data as.matrix & colnames
            saveColNam<-colnames(toPlot)
            toPlot<-as.matrix(toPlot[subregion,])
            colnames(toPlot)<-saveColNam
            ##===================================================

            # if (input$zscore==TRUE){
            #  require ('pca_fun.R')
            #  toPlot<-zScore (toPlot)
            # }

            View (head(toPlot))
            toPlot1col<-c('NA',rownames (toPlot))

            #print ('===============toPlot1col===============')
            #print (toPlot1col)

            #toPlot2col<-as.numeric (toPlot$input$selCol )
            #View (head(toPlot))
            toPlot2col<-c( 0 ,as.numeric (toPlot[,input$selCol] ))
            #print (toPlot2col)
            datoplo<- data.frame(toPlot1col,toPlot2col)

            #print ('==============datoplo===================')
            #print (datoplo)

            slot1table<-datoplo
            colnames(slot1table)<-c('regions',input$selCol)
            slot1table<-slot1table[-1,]


            print (ticksX)

            ax_SetBar<- list(

                categoryorder = "array",
                categoryarray = subregion,

                #title = "",
                #,zeroline = TRUE
                #,showline = TRUE
                showticklabels = TRUE
                #showgrid = TRUE
                #,tickmode='array'
                #,tickvals= c(1,10) #ticksX
                #,ticktext= c('hello','world')
                #tickOptions(ticksNamesX,
                            #chr=input$chrNameAxis,
                            #start=input$startNameAxis,
                            #end=input$endNameAxis)
            )

            #p<-plot_ly (datoplo, x=datoplo[,1], y=datoplo[,2] , type='bar' )
            slot1<-plot_ly (datoplo, y=datoplo[,2], text=maNames,
                            hoverinfo='y+text' , type='bar' ) %>% layout(
                                yaxis = list (title=input$selCol))
            #output$plotSlot<-plotly::renderPlotly({plotly::subplot (q, p, nrows=2, shareX=TRUE)})
        }

        #activeSlot2 #======================================
        if (input$activeSlot2==TRUE){
            slots<- c(slots,'slot2')
            #ricorda che deve avere lo stesso nome della variavile in cui
            #e conservato il plot
            toPlot<-HCRread (file='', path=input$fileInput_slot2$datapath)
            subregion<-colnames (rawmatrix)

            saveColNam<-colnames(toPlot)
            toPlot<-as.matrix(toPlot[subregion,])
            colnames(toPlot)<-saveColNam

            #toPlot<-toPlot[subregion,]
            toPlot1col<-c('NA', rownames (toPlot))
            toPlot2col<-c( 0 , as.numeric (toPlot[,input$selCol_slot2] ))
            datoplo<- data.frame(toPlot1col,toPlot2col)
            slot2table<-datoplo
            colnames(slot2table)<-c('regions',input$selCol_slot2)
            slot2table<-slot2table[-1,]
            print (ticksX)
            slot2<-plot_ly(datoplo,
                            y=datoplo[,2],
                            text=maNames ,
                            hoverinfo='y+text' ,
                            type='bar' )  %>% layout(yaxis = list (
                                title=input$selCol_slot2))
        }

        #activeSlot3 #======================================
        if (input$activeSlot3==TRUE){
            slots<- c(slots,'slot3')

            #ricorda che deve avere lo stesso nome della variabile
            #in cui e conservato il plot

            toPlot<-HCRread(file='', path=input$fileInput_slot3$datapath)
            subregion<-colnames(rawmatrix)

            saveColNam<-colnames(toPlot)
            toPlot<-as.matrix(toPlot[subregion,])
            colnames(toPlot)<-saveColNam

            #toPlot<-toPlot[subregion,]
            toPlot1col<-c('NA', rownames (toPlot))
            toPlot2col<-c( 0 , as.numeric (toPlot[,input$selCol_slot3] ))
            datoplo<- data.frame(toPlot1col,toPlot2col)
            slot3table<-datoplo
            colnames(slot3table)<-c('regions',input$selCol_slot3)
            slot3table<-slot3table[-1,]

            print (ticksX)
            slot3<-plot_ly(datoplo, y=datoplo[,2], text=maNames,
                            hoverinfo='y+text' , type='bar' )  %>% layout(
                                yaxis = list (title=input$selCol_slot3))
        }

        #activeSlot4 #======================================
        if (input$activeSlot4==TRUE){
            slots<- c(slots,'slot4')
            #ricorda che deve avere lo stesso nome della variabile
            #in cui e conservato il plot
            toPlot<-HCRread (file='', path=input$fileInput_slot4$datapath)
            subregion<-colnames (rawmatrix)

            saveColNam<-colnames(toPlot)
            toPlot<-as.matrix(toPlot[subregion,])
            colnames(toPlot)<-saveColNam

            #toPlot<-toPlot[subregion,]
            toPlot1col<-c('NA', rownames (toPlot))
            toPlot2col<-c( 0 , as.numeric (toPlot[,input$selCol_slot4] ))
            datoplo<- data.frame(toPlot1col,toPlot2col)
            slot4table<-datoplo
            colnames(slot4table)<-c('regions',input$selCol_slot4)
            slot4table<-slot4table[-1,]

            print (ticksX)
            slot4<-plot_ly (datoplo, y=datoplo[,2], text=maNames,
                            hoverinfo='y+text' , type='bar' ) %>% layout(
                                yaxis = list (title=input$selCol_slot4))
            print ('slot4 plot ok')
        }

        #activeSlot5 #======================================
        if (input$activeSlot5==TRUE){
            slots<- c(slots,'slot5')
            #ricorda che deve avere lo stesso nome della variavile
            #in cui e conservato il plot
            toPlot<-HCRread (file='', path=input$fileInput_slot5$datapath)
            subregion<-colnames (rawmatrix)

            saveColNam<-colnames(toPlot)
            toPlot<-as.matrix(toPlot[subregion,])
            colnames(toPlot)<-saveColNam

            #toPlot<-toPlot[subregion,]
            toPlot1col<-c('NA', rownames (toPlot))
            toPlot2col<-c( 0 , as.numeric (toPlot[,input$selCol_slot5] ))
            datoplo<- data.frame(toPlot1col,toPlot2col)
            slot5table<-datoplo
            colnames(slot5table)<-c('regions',input$selCol_slot5)
            slot5table<-slot5table[-1,]

            print (ticksX)
            slot5<-plot_ly (datoplo, y=datoplo[,2], text=maNames,
                            hoverinfo='y+text' , type='bar' ) %>% layout(
                                yaxis = list (title=input$selCol_slot5))
            print ('slot5 plot ok')
        }


        #render plot  RESULTS #=========================

        plotArray<- c('q','p','r','s','t','u')


        if (length (slots)==1){
            output$plotSlot<-plotly::renderPlotly({
                plotly::subplot (q,
                         nrows=1,
                         shareX=TRUE,
                         titleY=TRUE) %>% plotly::layout( height= 800,
                                                width= 800,
                                                plotly::plot_ly(),
                                                #,dimension= "ratio",
                                                xaxis = ax_SetX,
                                                yaxis = ax_SetY ,
                                                margin = list(l = 200, b = 200)
            )})
        } else {
            if (length (slots)==2){
                print (paste0('length(slots)=',length(slots)))
                p<-get(slots[2])
                output$plotSlot<-plotly::renderPlotly({
                    plotly::subplot (q,
                            p ,
                            nrows=2,
                            shareX=TRUE,
                            titleY=TRUE) %>% plotly::layout(height= 1000,
                                                    width= 800,
                                                    plotly::plot_ly(),
                                                    #,dimension= "ratio",
                                                    xaxis = ax_SetX,
                                                    yaxis = ax_SetY ,
                                                    margin = list(l = 200,
                                                                b = 200)
                )})
            } else {
                if (length (slots)==3){
                    for (i in 2:length(slots)){
                        assign ( plotArray[i] , get(slots[i]))
                    }
                    output$plotSlot<-plotly::renderPlotly({
                        plotly::subplot (q, p, r ,
                                nrows=3,
                                shareX=TRUE,titleY=TRUE) %>% plotly::layout(
                                                                height= 1200,
                                                                width= 800,
                                                                plotly::plot_ly(),
                                                        #,dimension= "ratio",
                                                        #xaxis = ax_SetX,
                                                                yaxis = ax_SetY,
                                                                margin = list(
                                                                    l = 200,
                                                                    b = 200)
                    )
                    })
                } else {
                    if (length (slots)==4){
                        for (i in 2:length (slots)){
                            assign ( plotArray[i] , get(slots[i]))
                        }
                        print ('slots 4 ok')
                        output$plotSlot<-plotly::renderPlotly({
                            plotly::subplot (q, p , r , s ,
                                     nrows=4,
                                     shareX=TRUE,
                                     titleY=TRUE) %>% plotly::layout( height= 1400,
                                                            width= 800,
                                                            plotly::plot_ly(),
                                                            #,dimension= "ratio",
                                                            xaxis = ax_SetX,
                                                            yaxis = ax_SetY ,
                                                            margin = list(
                                                                l = 200,
                                                                b = 200)
                        )})
                    } else {
                        if(length (slots)==5){
                            for (i in 2:length (slots)){
                                assign ( plotArray[i] , get(slots[i]))
                            }
                            output$plotSlot<-plotly::renderPlotly({
                                plotly::subplot (q, p , r , s , t ,
                                         nrows=5,
                                         shareX=TRUE,
                                         titleY=TRUE) %>% plotly::layout(height= 1600,
                                                                width= 800,
                                                                plotly::plot_ly(),
                                                        #,dimension= "ratio",
                                                                xaxis = ax_SetX,
                                                                yaxis = ax_SetY,
                                                                margin = list(
                                                                    l = 200,
                                                                    b = 200)
                            )})
                        } else {
                            if(length (slots)==6){
                                for (i in 2:length (slots)){
                                    assign ( plotArray[i] , get(slots[i]))
                                }
                                output$plotSlot<-plotly::renderPlotly({
                                    plotly::subplot (q, p , r , s , t , u,
                                            nrows=6,
                                            shareX=TRUE,
                                            titleY=TRUE) %>% plotly::layout(height=1600,
                                                                width= 800,
                                                                plotly::plot_ly(),
                                                        #,dimension= "ratio",
                                                                xaxis = ax_SetX,
                                                                yaxis = ax_SetY,
                                                                margin = list(
                                                                    l = 200,
                                                                    b = 200)
                                )})
                            }


                        }
                    }
                }
            }
        }

        #correlationplot ========================================================================================

        tables <-  slots
        for (i in 1:length (slots)){
            tables[i]<-paste0(slots[i],'table')
        }

        if (input$check_corrPlot==TRUE){

            if (length (tables)>=3){
                ##quà dentro ci devi mettere tutte le cose del corrplot
                ##ignora tables[1] che si riferisce alla heatmap

                for (i in 2: length(tables)){
                    # get(tables[i])  se fai get(tables[i])[,2] ti da la seconda colonna
                    if (i==2){
                        totalTable<- get(tables[i])
                    } else {
                        totalTable<- cbind (totalTable,get(tables[i])[,2])
                        colnames (totalTable)[i]<- colnames(get(tables[i]))[2]
                    }

                }

                rownames(totalTable)<-totalTable[,1]
                totalTable<-totalTable[,-1]
                #require ('corrplot')
                M <- corrplot::corrplot(totalTable)    #qui era scrirro cor e ho scritto corrplot

                output$plotSlot2<- shiny::renderPlot ({
                    #corrplot.mixed(M, upper="ellipse", lower='number')
                    corrplot::corrplot(M, type="upper", method="ellipse",
                             tl.pos="lt", tl.col="black",  tl.offset=1, tl.srt=0)
                    corrplot::corrplot(M, add=T, type="lower", method="number",
                             col="black", diag=F, tl.pos="n", cl.pos="n")
                })




            }

        }


    } )
    ##=======================================================================================================================================

    ##==================================observe data subset checkbox
    observeEvent (input$dataSub,{
        print(paste0("RV$dataChr",exists('RV$dataChr')))
        if (input$dataSub==TRUE){

            output$regionsPanel<- renderUI ({
                wellPanel ( fluidRow ( column (12,
                                               wellPanel(
                                                   ##===    x axis settings=========================================
                                                   fluidRow (

                                                       column (2, br()
                                                               #,textOutput (heatens('slot_chrAtext'))
                                                               ,helpText ('x-axis')
                                                       )

                                                       ,column (2,


                                                                selectInput (heatens('xChr'), label='',
                                                                             #choices= if (exists('RV$dataChr')==TRUE){RV$dataChr}else{'none'},
                                                                             #choices= if (exists('RV$dataChr')==TRUE){'pippo'}else{'none'},
                                                                             choices=RV$dataChr,
                                                                             selected =1

                                                                )

                                                       ),

                                                       column (2, br(),



                                                               checkboxInput(heatens("xType"), label = "region", value = FALSE)
                                                       ),

                                                       column (6,

                                                               uiOutput (heatens('xStartEnd'))

                                                       )
                                                   )

                                                   #========y=axis=settings=========================================================================
                                                   ,fluidRow (
                                                       column (2, br()
                                                               ,helpText ('y-axis')
                                                       )

                                                       ,column (2,

                                                                selectInput (heatens('yChr'), label='',
                                                                             #choices=if (exists('RV$dataChr')==TRUE){RV$dataChr}else{'none'},
                                                                             choices=RV$dataChr,
                                                                             selected =1
                                                                )

                                                       ),

                                                       column (2, br(),

                                                               checkboxInput(heatens("yType"), label = "region", value = FALSE)

                                                       ),
                                                       column (6,

                                                               uiOutput (heatens('yStartEnd'))

                                                       )
                                                   )
                                               ))


                )



                )
            })} else {

                output$regionsPanel<- renderUI ({
                    wellPanel ( fluidRow ( column (12,
                                                   wellPanel(

                                                       helpText ('If You are using an big input dataset. Please use subset check-box in layout tab for visualizing a limited dataset ')

                                                   ))))

                })}

    })
    ##==================================================================================

    ##==================================observe x "region" checkbox
    observeEvent (input$xType,{

        if (input$xType==TRUE){
            output$xStartEnd  <-renderUI({

                wellPanel (

                    fluidRow (



                        column (6,
                                numericInput(heatens("xStart"), label = h6("start"), value = 1)
                        ),



                        column (6,
                                numericInput(heatens("xEnd"), label =h6("end"), value = 2)
                        )

                    )

                )

            })

        } else {

            output$xStartEnd  <-renderUI({

                wellPanel (

                    fluidRow (
                        column (12, h6('all matrix x axis'))
                    ),

                    fluidRow (
                        column (12, h6('data are selected'))
                    )

                )

            })

        }

    })
    ##==================================================================================

    ##==================================observe y "region" checkbox
    observeEvent (input$yType,{

        if (input$yType==TRUE){
            output$yStartEnd  <-renderUI({

                wellPanel (

                    fluidRow (


                        column (6,
                                numericInput(heatens("yStart"), label = h6("start"), value = 1)
                        ),


                        column (6,
                                numericInput(heatens("yEnd"), label =h6("end"), value = 2)
                        )

                    )

                )

            })

        } else {

            output$yStartEnd  <-renderUI({

                wellPanel (

                    fluidRow (
                        column (12, h6('all matrix y axis'))
                    ),

                    fluidRow (
                        column (12, h6('data are selected'))
                    )

                )

            })

        }

    })
    ##==================================================================================

    #observeSlots  ================

    #slot1  #=================================
    observeEvent (input$fileInput_slot1,{

        tabPath<- input$fileInput_slot1$datapath
        tab<-HCRread (file='', path=tabPath)
        tabTT<<-tab
        colu<- colnames (tab)

        output$pos_slot1<- renderUI ({
            wellPanel (

                fluidRow (

                    column (10,

                            selectInput (heatens('selCol'), label='select column',
                                         choices=colu,
                                         selected=1
                            )

                    )



                )


            )
        })


        # if (input$activeSlot1){
        #
        #   output$pos_slot1<- renderUI ({
        #     wellPanel (
        #
        #       fluidRow (
        #
        #         column (10,
        #
        #                 selectInput (heatens('selCol'), label='select column',
        #                              choices=colu,
        #                              selected=1
        #                              )
        #
        #                 ),
        #
        #         column (2,
        #
        #                 checkboxInput (heatens('zscore'),label='Z-score', value=FALSE)
        #
        #                 )
        #
        #       )
        #
        #
        #     )
        #   })
        #
        # } else {
        #
        #   #renderUI con wellPanel con dentro helpText
        #
        # }
    })

    #slot2 #=================================
    observeEvent (input$fileInput_slot2,{

        tabPath_slot2<- input$fileInput_slot2$datapath
        tab_slot2<-HCRread (file='', path=tabPath_slot2)
        colu_slot2<- colnames (tab_slot2)

        output$pos_slot2<- renderUI ({
            wellPanel (fluidRow (
                column (10,
                        selectInput (heatens('selCol_slot2'), label='select column',
                                     choices=colu_slot2,
                                     selected=1
                        ))))
        })
    })

    #slot3 #================================
    observeEvent (input$fileInput_slot3,{

        tabPath_slot3<- input$fileInput_slot3$datapath
        tab_slot3<-HCRread (file='', path=tabPath_slot3)
        colu_slot3<- colnames (tab_slot3)

        output$pos_slot3<- renderUI ({
            wellPanel (fluidRow (
                column (10,
                        selectInput (heatens('selCol_slot3'), label='select column',
                                     choices=colu_slot3,
                                     selected=1
                        ))))
        })
    })

    #slot4 #================================
    observeEvent (input$fileInput_slot4,{

        tabPath_slot4<- input$fileInput_slot4$datapath
        tab_slot4<-HCRread (file='', path=tabPath_slot4)
        colu_slot4<- colnames (tab_slot4)

        output$pos_slot4<- renderUI ({
            wellPanel (fluidRow (
                column (10,
                        selectInput (heatens('selCol_slot4'), label='select column',
                                     choices=colu_slot4,
                                     selected=1
                        ))))
        })
    })

    #slot5 #================================
    observeEvent (input$fileInput_slot5,{

        tabPath_slot5<- input$fileInput_slot5$datapath
        tab_slot5<-HCRread (file='', path=tabPath_slot5)
        colu_slot5<- colnames (tab_slot5)

        output$pos_slot5<- renderUI ({
            wellPanel (fluidRow (
                column (10,
                        selectInput (heatens('selCol_slot5'), label='select column',
                                     choices=colu_slot5,
                                     selected=1
                        ))))
        })
    })

}
