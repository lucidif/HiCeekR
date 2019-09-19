#vFun_mainNavBar

#' mainNav
#'
#' @param inType
#'
#' @return
#' @keywords internal
#'
#' @examples
mainNav<-function(inType="BAM"){

    #inType: "BAM"  "rawMatrix"

    if(inType=="BAM"){
        return(
            shiny::navbarPage("HiCeekR", id='mainNav',

                              # shiny::tabPanel ('Welcome',
                              #                  shiny::uiOutput ('prjSetPlot')
                              # ),
                              #
                              # shiny::tabPanel ('Settings',
                              #                  shiny::uiOutput ('prjSettingsSlot')
                              # ),
                              #
                              # shiny::tabPanel ('Pre-Processing',
                              #                  shiny::uiOutput ('preProSlot')
                              # ),

                              shiny::tabPanel("Summary",
                                                shiny::uiOutput("summarySlot")
                                                ),

                                shiny::tabPanel("Filtering",
                                            shiny::uiOutput("filteringSlot")
                                            ),

                                shiny::tabPanel('Binning',
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
                                    shiny::tabPanel ('PCA',
                                                     shiny::uiOutput ('pcaSlot')
                                            ),

                                    shiny::tabPanel ('EpigeneticFeatures',
                                                     shiny::uiOutput ('bacovSlot')
                                    ),

                                    shiny::tabPanel ('directionalityIndex',
                                                shiny::uiOutput ('diSlot') #hmmSlot
                                                ),

                                    shiny::tabPanel ('TopDomTADs',
                                                     shiny::uiOutput ('TopDomTADsSlot')
                                    ),

                                    shiny::tabPanel ('HiCsegTADs',
                                                     shiny::uiOutput ('HiCsegTADsSlot')
                                    ),

                                    shiny::tabPanel ('bed2track',
                                                     shiny::uiOutput ('bed2trackSlot')
                                    )

                              ),

                              shiny::navbarMenu('Visualization',

                                                shiny::tabPanel('Heatmap',
                                                    shiny::uiOutput('TADsModule')
                                                ),

                                                shiny::tabPanel('Networks',
                                                    shiny::uiOutput('netModule')
                                                )
                                 )



            )


        )

    }

    if (inType=="rawMatrix") {
        return(
            shiny::navbarPage("HiCeekR", id='mainNav',

                              shiny::navbarMenu('Normalization',

                                    shiny::tabPanel('WavSis',
                                        shiny::uiOutput ('WavSisSlot')
                                                ),
                                        shiny::tabPanel('iterative',
                                            shiny::uiOutput('iterativeSlot')
                                                )

                              ),

                              shiny::navbarMenu('Post-Processing',

                                shiny::tabPanel('PCAdeprec',
                                    shiny::uiOutput('pcaSlot')
                                                ),

                                shiny::tabPanel('Compartments-PCA',
                                    shiny::uiOutput('pcaCompSlot')
                                                ),

                                shiny::tabPanel ('TADs-HMM',
                                    shiny::uiOutput ('hmmSlot')
                                                )

                              ),

                              shiny::navbarMenu('Visualization',

                                    shiny::tabPanel('Heatmap',

                                        shiny::uiOutput ('TADsModule')

                                            ),

                                    shiny::tabPanel('Networks',
                                        shiny::uiOutput('netModule')
                                                )
                                )

            )

        )
    }

    if (inType=="Multiple"){
        return(
            shiny::navbarPage("HiCeekR", id='mainNav',

                              # shiny::tabPanel ('Welcome',
                              #                  shiny::uiOutput ('prjSetPlot')
                              # ),
                              #
                              # shiny::tabPanel ('Settings',
                              #                  shiny::uiOutput ('prjSettingsSlot')
                              # ),
                              #
                              # shiny::tabPanel ('Pre-Processing',
                              #                  shiny::uiOutput ('preProSlot')
                              # ),

                              shiny::tabPanel("Summary",
                                              shiny::uiOutput("summarySlot")
                              ),

                              # shiny::tabPanel("Filtering",
                              #                 shiny::uiOutput("filteringSlot")
                              # ),

                              shiny::tabPanel('Binning',
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
                                                shiny::tabPanel ('PCA',
                                                                 shiny::uiOutput ('pcaSlot')
                                                ),

                                                shiny::tabPanel ('BAMcoverage',
                                                             shiny::uiOutput ('feCountsSlot')
                                                             ),

                                                shiny::tabPanel ('TADsHMM',
                                                                 shiny::uiOutput ('hmmSlot')
                                                )

                              ),

                              shiny::navbarMenu('Visualization',

                                                shiny::tabPanel('Heatmap',
                                                                shiny::uiOutput('TADsModule')
                                                ),

                                                shiny::tabPanel('Networks',
                                                                shiny::uiOutput('netModule')
                                                )
                              )



            )
        )
    }

}


