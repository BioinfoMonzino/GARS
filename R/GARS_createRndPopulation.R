#' @title Create a random chromosomes population
#'
#' @description This function creates the initial random population
#' of chromosomes
#'
#' @param data A matrix or a data.frame of the input data.
#' Rows and Cols have to be, respectively, observations and features
#' @param chr.num The number of chromosomes to generate. Default is
#' 1000
#' @param chr.len The length of chromosomes. This value corresponds
#' to the desired length of the feature set.
#'
#' @return A matrix representing the chromosomes population: each column
#' is a chromosome and each element correspond to the feature position in
#' 'data'
#'
#' @author Mattia Chiesa, Luca Piacentini
#'
#' @examples
#' # use example data:
#' data(GARS_data_norm)
#' GARS.create.rnd.population(GARS_data_norm, chr.len=10, chr.num=100)
#'
#' @export
#'
GARS.create.rnd.population <- function(data,
                                       chr.len,
                                       chr.num=1000){

  # check arguments
  if (missing(data))
    stop("'data' argument must be provided")
  if (missing(chr.len))
      stop("'chr.len' argument must be provided")
  if(!(is.matrix(data) | is.data.frame(data) ))
    stop("'data' must be a matrix or a data.frame")
  if(!(is.numeric(chr.num)))
    stop("'chr.num' must be numeric")
  if(!(is.numeric(chr.len)))
    stop("'chr.len' must be numeric")

  # check the presence of NA or Inf
  if (any(is.na(data)))
    stop("NA values are not allowed in the 'data' matrix")
  if (any(is.infinite(as.matrix(data))))
    stop("Inf values are not allowed in the 'data' matrix")

  # specific checks
  if (all(data == 0))
    stop("All elements are 0. Check the matrix!")
  if (chr.num <= 0)
    stop("'chr.num' must be positive")
  if ((chr.num %% 1) != 0)
    stop("'chr.num' must be integer")
  if (chr.len <= 0)
    stop("'chr.len' must be positive")
  if ((chr.len %% 1) != 0)
    stop("'chr.len' must be integer")
  if (chr.len == 1)
    stop("'chr.len' must be greater than 1")
  if ((chr.num %% 2) != 0){
    warning("'chr.num' must be even. A chromosome will be added.")
    chr.num <- chr.num + 1
  }

  # body
  # create empty population
  chr_pop <- matrix(nrow=chr.len, ncol = chr.num)

  # create random population
  for (i in seq_len(chr.num)){
    chr_pop[,i]<-sample(seq_len(dim(data)[2]), chr.len)
  }
  return(chr_pop)
}
