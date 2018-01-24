

ui<-shiny::shinyUI (


    shiny::fluidPage (





        shiny::fluidRow(



            shiny::column(11,

                        shiny::navbarPage("HiCeekR", id='mainNav',

                            shiny::tabPanel ('Welcome',
                            shiny::uiOutput ('prjSetPlot')
                            ),

                shiny::tabPanel ('Analysis',
                            shiny::uiOutput ('anSetPlot')
                            ),

                    shiny::tabPanel ('Pre-Processing',
                            shiny::uiOutput ('preProSlot')
                            ),

                    shiny::tabPanel ('Binning',
                            shiny::uiOutput ('binningSlot')
                            ),

                    shiny::navbarMenu('Normalization',

                                shiny::tabPanel ('WavSis',
                                        shiny::uiOutput ('WavSisSlot')
                                        ),
                                shiny::tabPanel ('iterative',
                                        shiny::uiOutput ('iterativeSlot')
                                        )

                    ),

                    shiny::navbarMenu('Post-Processing',

                                shiny::tabPanel ('Epigenetic Features',
                                            shiny::uiOutput ('pcaSlot')
                                ),

                                shiny::tabPanel ('Compartments-PCA',
                                            shiny::uiOutput ('pcaCompSlot')
                                ),

                                shiny::tabPanel ('TADs-HMM',
                                            shiny::uiOutput ('hmmSlot')
                                )

                ),

                    shiny::navbarMenu('Visualization',

                                shiny::tabPanel ('Heatmap',

                                            shiny::uiOutput ('TADsModule')

                                ),

                                shiny::tabPanel ('Networks',

                                            shiny::uiOutput ('netModule')
                                )

                    )

        )),

        shiny::column(1,
                shiny::wellPanel(
                shiny::conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                #helpText (h5('loading'))
                                shiny::img(src='.sysImg/busy_icon.png',align='right', height=20,width=20)
        ),shiny::br()
        )
        )

    ),



    shiny::fluidRow (

        shiny::column (12,
        shiny::uiOutput('moduleScreen')
        )
    )



)

)
