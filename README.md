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
devtools::install_github("lucidif/HiCeekR", repos=BiocManager::repositories())
````

## To get start

Once installed, launch HiCeekR Shiny app with the following commands:

````
library(HiCeekR)
HiCeekR()
````
For more details, please consult hiceek_manual.pdf documentation (https://github.com/lucidif/HiCeekR/blob/master/hiceekr_manual.pdf)

## Example Data

The example Data are available on http://bioinfo.na.iac.cnr.it/hiceekr 
you can download two files

a) Annotations zip file contains the reference genome in fasta format and the annotations in the format supported by HiCeekR

b) Input Files contains all input files supported by HiCeekR that are:

   1)sortmark_REP7_SRR1802426.bam and sortmark_REP5_SRR1802424.bam Hi-C alligned reads files (Grubert 2015, GSE62742)
    
   2)ENCFF000ATY_H3K9ac_GM12878_hg19.bam/bai and H3K27me3.bam/bai ChiP-seq Bam files from ENCODE (ENCSR447YYN) and their          index bai files.
    
   3)GSM2400247_ENCFF383EXA_gm12878_rnaSeq.tsv expression RNA-Seq Data from ENCODE (ENCFF383EXA)




