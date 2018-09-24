# HiCeekR
HiCeekR is a graphical interface tool that allows the study of the three-dimensional structure of chromatin through the analysis of Hi-C data and their integration with transcriptomic (RNA-seq) and epigenomic data (ChIP-seq). Through the visualization of interactive plots, the user can explore the results in their entirety and study the relationships between distant loci of chromatin structure.

## Installation

The easiest way to install HiCeekR is via GitHub. You need to install devtools R package from CRAN web site 

````
install.packages("devtools")
````
and BiocInstaller from Bioconductor website

````
source("https://bioconductor.org/biocLite.R")
biocLite("BiocInstaller")
````

Now you can install HiCeekR with command

````
devtools::install_github("lucidif/HiCeekR", dependencies=TRUE, repos=BiocInstaller::biocinstallRepos())
````

## To get start

At the begin, launch the app by commands

````
library(“HiCeekR”)
HiCeekR()
````
