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
R version 3.6.1 (2019-07-05)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Ubuntu 18.04.2 LTS

Matrix products: default
BLAS:   /usr/lib/x86_64-linux-gnu/openblas/libblas.so.3
LAPACK: /usr/lib/x86_64-linux-gnu/libopenblasp-r0.2.20.so

locale:
 [1] LC_CTYPE=it_IT.UTF-8       LC_NUMERIC=C               LC_TIME=it_IT.UTF-8       
 [4] LC_COLLATE=it_IT.UTF-8     LC_MONETARY=it_IT.UTF-8    LC_MESSAGES=it_IT.UTF-8   
 [7] LC_PAPER=it_IT.UTF-8       LC_NAME=C                  LC_ADDRESS=C              
[10] LC_TELEPHONE=C             LC_MEASUREMENT=it_IT.UTF-8 LC_IDENTIFICATION=C       

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] shiny_1.3.2    HiCeekR_0.99.0

loaded via a namespace (and not attached):
  [1] R.utils_2.9.0               HiTC_1.28.0                 tidyselect_0.2.5           
  [4] heatmaply_0.16.0            RSQLite_2.1.2               AnnotationDbi_1.46.1       
  [7] htmlwidgets_1.3             grid_3.6.1                  TSP_1.1-7                  
 [10] BiocParallel_1.18.1         devtools_2.1.0              munsell_0.5.0              
 [13] codetools_0.2-16            withr_2.1.2                 colorspace_1.4-1           
 [16] OrganismDbi_1.26.0          Biobase_2.44.0              Category_2.50.0            
 [19] knitr_1.24                  rstudioapi_0.10             stats4_3.6.1               
 [22] GenomeInfoDbData_1.2.1      hwriter_1.3.2               bit64_0.9-7                
 [25] rhdf5_2.28.0                rprojroot_1.3-2             vctrs_0.2.0                
 [28] xfun_0.9                    biovizBase_1.32.0           csaw_1.18.0                
 [31] gclus_1.3.2                 R6_2.4.0                    GenomeInfoDb_1.20.0        
 [34] seriation_1.2-8             locfit_1.5-9.1              AnnotationFilter_1.8.0     
 [37] diffHic_1.16.0              bitops_1.0-6                reshape_0.8.8              
 [40] DelayedArray_0.10.0         assertthat_0.2.1            promises_1.0.1             
 [43] networkD3_0.4               scales_1.0.0                nnet_7.3-12                
 [46] gtable_0.3.0                processx_3.4.1              ggbio_1.32.0               
 [49] ensembldb_2.8.0             rlang_0.4.0                 zeallot_0.1.0              
 [52] genefilter_1.66.0           splines_3.6.1               rtracklayer_1.44.3         
 [55] lazyeval_0.2.2              acepack_1.4.1               dichromat_2.0-0            
 [58] checkmate_1.9.4             BiocManager_1.30.4          reshape2_1.4.3             
 [61] GenomicFeatures_1.36.4      backports_1.1.4             httpuv_1.5.1               
 [64] Hmisc_4.2-0                 RBGL_1.60.0                 tools_3.6.1                
 [67] usethis_1.5.1               ggplot2_3.2.1               gplots_3.0.1.1             
 [70] RColorBrewer_1.1-2          BiocGenerics_0.30.0         wavethresh_4.6.8           
 [73] sessioninfo_1.1.1           Rcpp_1.0.2                  plyr_1.8.4                 
 [76] base64enc_0.1-3             progress_1.2.2              zlibbioc_1.30.0            
 [79] purrr_0.3.2                 RCurl_1.95-4.12             ps_1.3.0                   
 [82] prettyunits_1.0.2           rpart_4.1-15                viridis_0.5.1              
 [85] S4Vectors_0.22.0            SummarizedExperiment_1.14.1 cluster_2.1.0              
 [88] chromoR_1.0                 fs_1.3.1                    magrittr_1.5               
 [91] data.table_1.12.2           ProtGenerics_1.16.0         matrixStats_0.54.0         
 [94] haarfisz_4.5                pkgload_1.0.2               hms_0.5.1                  
 [97] shinyjs_1.0                 mime_0.7                    xtable_1.8-4               
[100] XML_3.98-1.20               IRanges_2.18.2              gridExtra_2.3              
[103] testthat_2.2.1              compiler_3.6.1              biomaRt_2.40.4             
[106] tibble_2.1.3                KernSmooth_2.23-15          crayon_1.3.4               
[109] R.oo_1.22.0                 ReportingTools_2.24.0       htmltools_0.3.6            
[112] GOstats_2.50.0              later_0.8.0                 Formula_1.2-3              
[115] tidyr_0.8.3                 geneplotter_1.62.0          DBI_1.0.0                  
[118] corrplot_0.84               MASS_7.3-51.1               Matrix_1.2-17              
[121] cli_1.1.0                   R.methodsS3_1.7.1           gdata_2.18.0               
[124] parallel_3.6.1              igraph_1.2.4.1              GenomicRanges_1.36.0       
[127] pkgconfig_2.0.2             GenomicAlignments_1.20.1    registry_0.5-1             
[130] foreign_0.8-72              plotly_4.9.0                InteractionSet_1.12.0      
[133] foreach_1.4.7               annotate_1.62.0             webshot_0.5.1              
[136] XVector_0.24.0              AnnotationForge_1.26.0      VariantAnnotation_1.30.1   
[139] stringr_1.4.0               callr_3.3.1                 digest_0.6.20              
[142] graph_1.62.0                Biostrings_2.52.0           htmlTable_1.13.1           
[145] gProfileR_0.6.7             dendextend_1.12.0           edgeR_3.26.8               
[148] GSEABase_1.46.0             curl_4.0                    Rsamtools_2.0.0            
[151] gtools_3.8.1                jsonlite_1.6                PFAM.db_3.8.2              
[154] Rhdf5lib_1.6.0              desc_1.2.0                  viridisLite_0.3.0          
[157] limma_3.40.6                BSgenome_1.52.0             pillar_1.4.2               
[160] lattice_0.20-38             GGally_1.4.0                shinyFiles_0.7.3           
[163] Rhtslib_1.16.1              httr_1.4.1                  pkgbuild_1.0.5             
[166] survival_2.43-3             GO.db_3.8.2                 glue_1.3.1                 
[169] remotes_2.1.0               iterators_1.0.12            bit_1.1-14                 
[172] Rgraphviz_2.28.0            stringi_1.4.3               blob_1.2.0                 
[175] DESeq2_1.24.0               latticeExtra_0.6-28         caTools_1.17.1.2           
[178] memoise_1.1.0               dplyr_0.8.3                

````

