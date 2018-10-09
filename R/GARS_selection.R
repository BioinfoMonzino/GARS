#' @title Perform the "Roulette Wheel" or the "Tournament" selection
#'
#' @description This function implements two kind of GA Selection step:
#' the "Roulette Wheel" and the "Tournament" selection.
#'
#' @param chr.pop A matrix or a data.frame representing the chromosomes
#' population: each column is a chromosome and each element corresponds
#'  to the feature position in the data matrix
#' @param type The type of selection method; Roulette Wheel ("RW")
#' and Tournament Selection ("TS") are allowed. Default is "RW"
#' @param fitn.values A numeric vector where each element corresponds to
#'  the fitness score of each chromosome in 'chr.pop'
#'
#' @return A matrix representing the "selected" population.
#' The dimensions of this matrix are the same of 'chr.pop'.
#'
#' @author Mattia Chiesa, Luca Piacentini
#'
#' @examples
#' # use example data:
#' data(GARS_popul)
#' data(GARS_Fitness_score)
#' selected_pop <- GARS_Selection(GARS_popul, "RW", GARS_Fitness_score)
#'
#' @seealso
#' \code{\link{GARS_Mutation}},
#' \code{\link{GARS_Crossover}},
#' \code{\link{GARS_Elitism}},
#'
#' @export
#'
GARS_Selection <- function(chr.pop, type = c("RW","TS"), fitn.values){

  # check arguments
  if (missing(chr.pop))
    stop("'chr.pop' argument must be provided")
  if (missing(fitn.values))
    stop("'fitn.values' argument must be provided")
  if (missing(type)){
    type <- type[1]
  }
  if(!(is.matrix(chr.pop) | is.data.frame(chr.pop) ))
    stop("'data' must be a matrix or a data.frame")
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
  if (dim(chr.pop)[1] == 1)
    stop("'dim(chr.pop)[1]' must be greater than 1")

  # body
  chr.pop.sel <- matrix(nrow = nrow(chr.pop),
                        ncol = ncol(chr.pop))

  if(type == "RW"){
    # Roulette Wheel
    fitn.sort <- fitn.values[order(fitn.values, decreasing = TRUE)]
    fitn.sort[which(fitn.sort < 0)] <- 0
    cumul.sum <- 1/cumsum(fitn.sort)
    cumul.sum.norm <- cumul.sum/sum(cumul.sum)


    for (i in seq_len(length(fitn.values))){
      alea <- runif(1)
      ind.alea <- which(cumul.sum.norm <= alea)

      if (length(ind.alea) == 0){
        # take the last chromosome
        chr.pop.sel[,i] <- chr.pop[,ncol(chr.pop)]
      }else{
        chr.pop.sel[,i] <- chr.pop[,ind.alea[1]]}
    }
  }else if(type == "TS"){
    # Tournament Selection
    for (i in seq_len(length(fitn.values))){
      ind.alea <- sample(seq_len(dim(chr.pop)[2]))[c(1,2)]
      chr.test <- chr.pop[,ind.alea]
      feat.test <- fitn.values[ind.alea]
      if(feat.test[1] >= feat.test[2]){
        chr.pop.sel[,i] <- chr.test[,1]
      }else{
        chr.pop.sel[,i] <- chr.test[,2]
      }
    }
  }else{
    stop("Please set 'RW or 'TS' as Selection type.")
  }

  return(chr.pop.sel)
}
