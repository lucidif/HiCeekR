# HiCeekR
HiCeekR is a novel Shiny based R package for Hi-C data analysis. In particular, HiCeekR
combines several R/Bioconductor packages widely used for Hi-C data analysis and visualization. It
starts from already aligned sequence files obtained from Hi-C experiments, then proceeds through a
series of steps from pre-processing and filtering, to the evaluation and normalization of the contact
matrices. Once the contact matrices are available, HiCeekR allows the users to perform several
downstream analyses. Moreover, HiCeekR produces several interactive graphics that allow exploring the
results by the usage of the mouse pointer.
Thanks to its GUI, HiCeekR friendly guides users during the entire analysis process, allowing them to
perform a complete data analysis pipeline (i.e., pre-processing, filtering, binning, normalization,
identification of compartments and TADs) and to integrate Hi-C data with other omic datasets such as
ChIP-seq and RNA-seq.

## Installation

The easiest way to install HiCeekR is via GitHub performing the following steps:
i) Download and install devtools R package from CRAN web site.
Open R console and digit:

````
install.packages("devtools")
````
ii) Download and install BiocManager from Bioconductor website.
In the R console digit: 

````
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
````

iii) Now install HiCeekR using the command

````
library(devtools) ; install_github("HenrikBengtsson/TopDom") ; install_github("lucidif/HiCeekR", repos=BiocManager::repositories())
````

## To get start

Once installed, launch HiCeekR Shiny app with the following commands:

````
library(HiCeekR)
HiCeekR()
````
For more details, please consult hiceek_manual.pdf documentation (https://github.com/lucidif/HiCeekR/blob/master/hiceekr_manual.pdf)

## Example Data

In order to understand and test HiCeekR functionalities, please use the data available at http://bioinfo.na.iac.cnr.it/hiceekr 

In particular, ou can download two foldes

a) Annotations.zip  contains the reference genome in fasta format and the HiCeekR annotations tsv file.

b) Example.zip e contains an example of all input files supported by HiCeekR:

   1)sortmark_REP7_SRR1802426.bam and sortmark_REP5_SRR1802424.bam Hi-C aligned reads files (Grubert 2015, GSE62742)
    
   2)ENCFF000ATY_H3K9ac_GM12878_hg19.bam/.bai and H3K27me3.bam/.bai ChiP-seq Bam files from ENCODE (ENCSR447YYN) and their        index .bai files.
    
   3)GSM2400247_ENCFF383EXA_gm12878_rnaSeq.tsv expression RNA-Seq Data from ENCODE (ENCFF383EXA)


## Session info

````
 R.utils_2.9.0               HiTC_1.28.0                 tidyselect_0.2.5           
  [4] heatmaply_0.16.0            RSQLite_2.1.2               AnnotationDbi_1.46.1       
  [7] htmlwidgets_1.3             grid_3.6.1                  TSP_1.1-7                  
 [10] BiocParallel_1.18.1         devtools_2.2.0              munsell_0.5.0              
 [13] codetools_0.2-16            DT_0.9                      withr_2.1.2                
 [16] colorspace_1.4-1            OrganismDbi_1.26.0          Biobase_2.44.0             
 [19] Category_2.50.0             knitr_1.25                  rstudioapi_0.10            
 [22] stats4_3.6.1                GenomeInfoDbData_1.2.1      hwriter_1.3.2              
 [25] bit64_0.9-7                 rhdf5_2.28.0                rprojroot_1.3-2            
 [28] vctrs_0.2.0                 xfun_0.9                    biovizBase_1.32.0          
 [31] csaw_1.18.0                 gclus_1.3.2                 R6_2.4.0                   
 [34] GenomeInfoDb_1.20.0         seriation_1.2-8             locfit_1.5-9.1             
 [37] AnnotationFilter_1.8.0      reshape_0.8.8               diffHic_1.16.0             
 [40] bitops_1.0-6                DelayedArray_0.10.0         assertthat_0.2.1           
 [43] promises_1.0.1              networkD3_0.4               scales_1.0.0               
 [46] nnet_7.3-12                 gtable_0.3.0                processx_3.4.1             
 [49] ggbio_1.32.0                ensembldb_2.8.0             rlang_0.4.0                
 [52] zeallot_0.1.0               genefilter_1.66.0           splines_3.6.1              
 [55] rtracklayer_1.44.4          lazyeval_0.2.2              acepack_1.4.1              
 [58] dichromat_2.0-0             checkmate_1.9.4             BiocManager_1.30.4         
 [61] reshape2_1.4.3              yaml_2.2.0                  GenomicFeatures_1.36.4     
 [64] backports_1.1.4             httpuv_1.5.2                Hmisc_4.2-0                
 [67] RBGL_1.60.0                 tools_3.6.1                 usethis_1.5.1              
 [70] ggplot2_3.2.1               ellipsis_0.2.0.1            gplots_3.0.1.1             
 [73] RColorBrewer_1.1-2          HiCseg_1.1                  BiocGenerics_0.30.0        
 [76] wavethresh_4.6.8            sessioninfo_1.1.1           plyr_1.8.4                 
 [79] Rcpp_1.0.2                  base64enc_0.1-3             progress_1.2.2             
 [82] zlibbioc_1.30.0             purrr_0.3.2                 RCurl_1.95-4.12            
 [85] ps_1.3.0                    prettyunits_1.0.2           rpart_4.1-15               
 [88] viridis_0.5.1               S4Vectors_0.22.1            SummarizedExperiment_1.14.1
 [91] cluster_2.1.0               chromoR_1.0                 fs_1.3.1                   
 [94] magrittr_1.5                data.table_1.12.2           ProtGenerics_1.16.0        
 [97] matrixStats_0.55.0          haarfisz_4.5                pkgload_1.0.2              
[100] hms_0.5.1                   shinyjs_1.0                 mime_0.7                   
[103] xtable_1.8-4                XML_3.98-1.20               IRanges_2.18.2             
[106] gridExtra_2.3               testthat_2.2.1              compiler_3.6.1             
[109] biomaRt_2.40.4              tibble_2.1.3                KernSmooth_2.23-15         
[112] crayon_1.3.4                R.oo_1.22.0                 ReportingTools_2.24.0      
[115] htmltools_0.3.6             GOstats_2.50.0              later_0.8.0                
[118] Formula_1.2-3               tidyr_1.0.0                 geneplotter_1.62.0         
[121] DBI_1.0.0                   corrplot_0.84               MASS_7.3-51.1              
[124] Matrix_1.2-17               cli_1.1.0                   R.methodsS3_1.7.1          
[127] gdata_2.18.0                parallel_3.6.1              igraph_1.2.4.1             
[130] GenomicRanges_1.36.1        pkgconfig_2.0.2             GenomicAlignments_1.20.1   
[133] registry_0.5-1              foreign_0.8-72              plotly_4.9.0               
[136] InteractionSet_1.12.0       foreach_1.4.7               annotate_1.62.0            
[139] webshot_0.5.1               XVector_0.24.0              AnnotationForge_1.26.0     
[142] VariantAnnotation_1.30.1    stringr_1.4.0               callr_3.3.1                
[145] digest_0.6.20               graph_1.62.0                Biostrings_2.52.0          
[148] htmlTable_1.13.1            gProfileR_0.6.8             dendextend_1.12.0          
[151] edgeR_3.26.8                GSEABase_1.46.0             curl_4.1                   
[154] Rsamtools_2.0.0             gtools_3.8.1                lifecycle_0.1.0            
[157] jsonlite_1.6                PFAM.db_3.8.2               Rhdf5lib_1.6.1             
[160] desc_1.2.0                  viridisLite_0.3.0           limma_3.40.6               
[163] BSgenome_1.52.0             pillar_1.4.2                GGally_1.4.0               
[166] lattice_0.20-38             shinyFiles_0.7.3            Rhtslib_1.16.1             
[169] httr_1.4.1                  pkgbuild_1.0.5              survival_2.43-3            
[172] GO.db_3.8.2                 glue_1.3.1                  remotes_2.1.0              
[175] iterators_1.0.12            bit_1.1-14                  Rgraphviz_2.28.0           
[178] stringi_1.4.3               blob_1.2.0                  DESeq2_1.24.0              
[181] latticeExtra_0.6-28         caTools_1.17.1.2            memoise_1.1.0              
[184] dplyr_0.8.3                 TopDom_0.8.1                
[1] ""
[1] "preWd"
[1] "load config wd:/media/lucio/data/1.Bioinformatic/HCR.config"
[1] "config: /media/lucio/data/1.Bioinformatic/HCR.config"
[1] "working dir:/media/lucio/data/1.Bioinformatic/1.heavyWD/1.R/test4/"
[1] "postWd"
[1] "workingDir"
[1] FALSE
[1] "prjDir:/media/lucio/data/1.Bioinformatic/1.heavyWD/1.R/test4/"
[1] "wkDir: /media/lucio/data/1.Bioinformatic/1.heavyWD/1.R/test4/HiCeekRwd/"  

````

