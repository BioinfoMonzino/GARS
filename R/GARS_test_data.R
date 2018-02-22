#' RNA-seq dataset for testing GARS
#'
#' The class labels of the sample dataset
#'
#' @format A vector of type "factor" with 58 elements: 29 labelled as "N"
#' and 29 labelled as "T".
#'
#' @return
#' An example data for testing \code{GARS} package
#'
"GARS_classes"

#' RNA-seq dataset for testing GARS
#'
#' An RNA-seq normalized matrix to test several GARS functions; this dataset
#' was obtained using the \code{DaMirseq} package to normalize the raw count
#' matrix present in \code{MLSeq} package.
#'
#' @format A matrix of 157 genes (columns) and 58 samples (rows)
#'
#' @return
#' An example data for testing \code{GARS} package
#'
"GARS_data_norm"

#' RNA-seq dataset for testing GARS
#'
#' A numeric vector with the maximum fitness score for each iteration
#'
#' @format A numeric vector with 100 fitness scores
#'
#' @return
#' An example data for testing \code{GARS} package
#'
"GARS_fit_list"

#' RNA-seq dataset for testing GARS
#'
#' A numeric vector with the fitness scores for each chromosome in a
#' single generation
#'
#' @format A numeric vector with 50 fitness scores
#'
#' @return
#' An example data for testing \code{GARS} package
#'
"GARS_Fitness_score"


#' RNA-seq dataset for testing GARS
#'
#' A matrix to test several GARS functions, representing a
#' chromosome population
#'
#' @format A matrix of 20 rows (features) and 50 columns (chromosomes)
#'
#' @return
#' An example data for testing \code{GARS} package
"GARS_popul"

#' RNA-seq dataset for testing GARS
#'
#' A list containing 100 of consecutive chromosomes populations
#'
#' @format A list with 100 consecutive chromosomes populations
#'
#' @return
#' An example data for testing \code{GARS} package
"GARS_pop_list"

#' A GarsSelectedFeatures object for testing GARS
#'
#' An object representing the output of GARS_GA
#'
#' @format A GarsSelectedFeatures
#'
#' @return
#' An example data for testing \code{GARS} package
"GARS_res_GA"