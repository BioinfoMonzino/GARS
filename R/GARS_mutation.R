#' @title Perform the Mutation step
#'
#' @description This function implements the mutation step in the GA.
#' First, it checks and replace duplicate features in each chromosomes;
#' then, random mutation are applied to the entire population.
#'
#' @param chr.pop A matrix or a data.frame representing the chromosomes
#' population: each column is a chromosome and each element correspond
#'  to the feature position in the data matrix
#' @param mut.rate The probability to apply a random mutation to each
#' element. It must be between 0 and 1. Default is 0.01
#' @param totFeats The total number of features. Often, it corresponds
#'  to number of columns of the data matrix
#'
#' @return A matrix representing the "mutated" population. The dimensions of
#' this matrix are the same of 'chr.pop'
#'
#' @author Mattia Chiesa, Luca Piacentini
#'
#' @examples
#' # use example data:
#' data(GARS_popul)
#' data(GARS_data_norm)
#'
#' mutated_pop <- GARS.Mutation(GARS_popul, mut.rate=0.1,
#'  dim(GARS_data_norm)[2])
#'
#' @seealso
#' \code{\link{GARS.Elitism}},
#' \code{\link{GARS.Selection}},
#' \code{\link{GARS.Crossover}},
#'
#' @export

GARS.Mutation <- function(chr.pop,
                          mut.rate=0.01,
                          totFeats){
  # check arguments
  if (missing(chr.pop))
    stop("'chr.pop' argument must be provided")
  # check arguments
  if (missing(totFeats))
    stop("'totFeats' argument must be provided")
  if(!(is.matrix(chr.pop) | is.data.frame(chr.pop) ))
    stop("'data' must be a matrix or a data.frame")
  if(!(is.numeric(mut.rate)))
    stop("'mut.rate' must be numeric")
  if(!(is.numeric(totFeats)))
    stop("'totFeats' must be numeric")


  # check the presence of NA or Inf
  if (any(is.na(chr.pop)))
    stop("NA values are not allowed in the 'chr.pop' matrix")
  if (any(is.infinite(as.matrix(chr.pop))))
    stop("Inf values are not allowed in the 'chr.pop' matrix")

  # specific checks
  if (any(chr.pop <= 0))
    stop("'chr.pop' must have only positive values")
  if (any((chr.pop %% 1) != 0))
    stop("'chr.pop' must have only integer values")
  if ((dim(chr.pop)[2] %% 2) != 0)
    stop("The number of chromosomes must be even.")
  if (mut.rate <= 0 | mut.rate > 1)
    stop("'mut.rate' must be > 0 and < 1")
  if (totFeats <= 0)
    stop("'totFeats' must be > 0")
  if ((totFeats %% 1) != 0)
    stop("'totFeats' must be integer")
  if (dim(chr.pop)[1] == 1)
    stop("'dim(chr.pop)[1]' must be greater than 1")
  if (totFeats < max(chr.pop)){
    warning("'totFeats' is lower than max(chr.pop).
            totFeats will be replace with max(chr.pop)")
    totFeats <- max(chr.pop)
    }


  # body

  for (k in seq_len(dim(chr.pop)[2])){

    # check duplicates in chr: if yes -> mutate!
    ind_dup <- which(duplicated(chr.pop[,k]) == TRUE)
    val_ok <- setdiff(seq_len(totFeats), chr.pop[,k])
    chr.pop[ind_dup,k] <- sample(val_ok)[seq_len(length(ind_dup))]

    # standard mutation
    for (j in seq_len(dim(chr.pop)[1])){
      if(runif(1) <= mut.rate){
      index_ok <- setdiff(seq_len(totFeats), chr.pop[,k])
      chr.pop[j,k] <- sample(index_ok)[1]
      }
    }
  }
  return(chr.pop)
}





