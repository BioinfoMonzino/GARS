#' @title Separate chromosome on the basis of the Fitness Scores
#'
#' @description This function splits the chromosome population in two
#' parts allowing the best chromosomes to be preserved from the
#' "evolutionary" steps: Selection, Crossover and Mutation.
#'
#' @param chr.pop A matrix or a data.frame representing the chromosomes
#' population: each column is a chromosome and each element corresponds
#'  to the feature position in the data matrix
#' @param fitn.values A numeric vector where each element corresponds to
#'  the fitness score of each chromosome in 'chr.pop'
#' @param n.elit The number of best chromosomes to be selected by
#' elitism. This number must be even. Default is 10
#'
#' @return A list containing:
#' \itemize{
#'   \item The population of best chromosomes selected by elitism.
#'   \item The population of chromosomes not selected by elitism.
#'   \item The fitness values of best chromosomes selected by elitism.
#'   \item The fitness values of chromosomes not selected by elitism.
#' }
#'
#' @author Mattia Chiesa, Luca Piacentini
#'
#' @examples
#' data(GARS_popul)
#' data(GARS_Fitness_score)
#' pop_list <- GARS.Elitism(GARS_popul, GARS_Fitness_score)
#'
#' @seealso
#' \code{\link{GARS.Mutation}},
#' \code{\link{GARS.Selection}},
#' \code{\link{GARS.Crossover}},
#' \code{\link{GARS.FitFun}},
#'
#' @export
#'
GARS.Elitism <- function(chr.pop, fitn.values, n.elit=10){

  # check arguments
  if (missing(chr.pop))
    stop("'chr.pop' argument must be provided")
  if (missing(fitn.values))
    stop("'fitn.values' argument must be provided")
  if(!(is.matrix(chr.pop) | is.data.frame(chr.pop) ))
    stop("'chr.pop' must be a matrix or a data.frame")
  if(!(is.numeric(n.elit)))
    stop("'n.elit' must be numeric")
  if(!(is.vector(fitn.values)))
    stop("'fitn.values' must be a vector")

  # check the presence of NA or Inf
  if (any(is.na(chr.pop)))
    stop("NA values are not allowed in the 'chr.pop' matrix")
  if (any(is.infinite(as.matrix(chr.pop))))
    stop("Inf values are not allowed in the 'chr.pop' matrix")
  if (any(is.na(fitn.values)))
    stop("NA values are not allowed in the 'fitn.values' vector")
  if (any(is.infinite(fitn.values)))
    stop("Inf values are not allowed in the 'fitn.values' vector")

  # specific checks
  if (any(chr.pop <= 0))
    stop("'chr.pop' must have only positive values")
  if (any((chr.pop %% 1) != 0))
    stop("'chr.pop' must have only integer values")
  if(dim(chr.pop)[2] != length(fitn.values))
    stop("ncol(chr.pop) must be equal to length(fitn.values)")
  if ((dim(chr.pop)[2] %% 2) != 0)
    stop("The number of chromosomes must be even.")
  if ((n.elit %% 2) != 0)
    stop("n.elit must be even.")
  if ((n.elit %% 1) != 0)
    stop("'n.elit' must be an integer value")
  if (n.elit < 2 | n.elit >= dim(chr.pop)[2])
    stop("'n.elit' must be >= 2 and < dim(chr.pop)[2]")
  if (dim(chr.pop)[1] == 1)
    stop("'dim(chr.pop)[1]' must be greater than 1")

  # body
  index.list <- order(fitn.values, decreasing = TRUE)
  fitn.sort <- fitn.values[index.list]
  data.elit <- chr.pop[,index.list[seq_len(n.elit)]]
  data.non.elit <- chr.pop[,-index.list[seq_len(n.elit)]]


  return(list(chr.pop.elit = data.elit,
              chr.pop.non.elit = data.non.elit,
              fitn.sort.elit = fitn.sort[seq_len(n.elit)],
              fitn.sort.non.elit = fitn.sort[-seq_len(n.elit)]))
}
