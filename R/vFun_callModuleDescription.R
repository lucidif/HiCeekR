#' callmoduleDescription
#'
#' @param mainNavItem
#'
#' @return
#' @keywords internal
#'
#' @examples
callModuleDescription<-function(mainNavItem){
    #mainNavItem: "Filtering"; "Binning"; "iterative"; "WavSis":
    #           'EpigeneticFeatures' ; "CompartmentsPCA"; "TADsHMM"

    des<-"no description for the module"

    if (mainNavItem=="Filtering"){
        des<-"The initial processing step for Hi-C data typically consists of
             trimming of reads (if necessary), mapping the reads to the
             corresponding reference genome with assay-specific pre- and
             post-processing to improve the percent of mapped reads, and
             filtering of the mapped reads and read pairs at several different
             levels. In the filtering module the strand orientation for a read
             pair refers to the combination of strands for the two alignments
             are stored.  Lower insert sizes, spikes are observed in the ouward-
             and inward-facing distributions due to self-circularization and
             dangling ends, respectively. Thresholds should be chosen with
             min.inward min.otward parameters.
             The max.frag argument removes read pairs where the inferred length
             of the sequencing fragment (i.e., the ligation product) is greater
             than a specified value"

    }

    if (mainNavItem=="Binning"){
        des<-"the genome is partitioned into contiguous non-overlapping bins of
             constant size. Each interaction is defined as a pair of these bins.
             This approach avoids the need for prior knowledge of the loci of
             interest when summarizing Hi-C counts. Counting of read pairs
             between paired bins is performed using binning module. Bin pairs can
             also be filtered to remove those with to a count sum below a
             filter parameter value"

    }

    if (mainNavItem=="iterative"){
        des<-"Several sequence-dependent features were shown to substantially
             bias Hi-C readouts. These include biases that are associatedwith
             sequencing platforms (such as GC content) and read alignment
             (such as mappabil- ity), and those that are specific to Hi-C
             (such as fre- quency of restriction sites). Discovery of these
             biases led to several normalization or correction methods
             for Hi-C data. In the context of Hi-C, Imakaev et al. proposed an
             iterative method abbreviated as ICE, which applies an statistical
             algorithm repeatedly to achieve the bias reduction."

    }

    if (mainNavItem=="WavSis"){
        des<-"Several sequence-dependent features were shown to substantially
             bias Hi-C readouts. These include biases that are associatedwith
             sequencing platforms (such as GC content) and read alignment
             (such as mappability), and those that are specific to Hi-C
             (such as fre- quency of restriction sites). Discovery of these
             biases led to several normalization or correction methods
             for Hi-C data. WavSis module provides users with a statistical
             pipeline for analysing chromosomal interactions data (Hi-C data).
             It combines wavelet methods and a Bayesian approach for correction
             (bias and noise) and comparison (detecting significant changes
             between Hi-C maps) of Hi-C contact maps."

    }

    if (mainNavItem=="EpigeneticFeatures"){
        des<-"In the genomics literature many types of regulatory domains have
         been identified on the basis of specific epigenetic marks, DNA
         replication timing, lamina associations, nucleolus asso- ciations, or a
         joint analysis of some of these factors. All of these domains are
         defined by specific, patterns of one-dimensional signal tracks.With
         the avail- ability of genome-wide Hi-C data, several novel domain
         types have been identified that appear as specific patterns in
         contact maps.
         With this module it is possible to count epigenomic markers inside
         the Hi-C bins, starting from the beds associated with epigenetic markers
        "
    }

    if (mainNavItem=="CompartmentsPCA"){
        des<-"The principal component (eigenvector) correlates with the
         distribution of genes and with features of open and close chromatin.
         With this module the user can execute the PCA starting from the contact
         matrix and from epigenetic markers"
    }

    if (mainNavItem=="TADsHMM"){
        des<-"Using hidden Markov models (HMM) the researchers have shown
             that the genome can be segmented into regions of bins with the
             same state, leading to the definition of Topologically Associated
             Domains (TADs, ‘upstream’ or ‘downstream’ regions) and their
             boundaries."
    }



    return(des)
}
