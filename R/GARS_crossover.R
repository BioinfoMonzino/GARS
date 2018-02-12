#' @title Perform the one-point and the two-point Crossover
#'
#' @description This function implements the one-point and the two-point
#'  cross-over.
#'
#' @param chr.pop A matrix or a data.frame representing the chromosomes
#' population: each column is a chromosome and each element corresponds
#'  to the feature position in the data matrix
#' @param co.rate The probability of each random couple of chromosomes
#'  to swap some parts. It must be between 0 and 1. Default is 0.8
#' @param type The type of crossover method; one-point ("one.p")
#' and two-point ("two.p") are allowed. Default is "one.p"
#' @param one.p.quart The position of the cromosome where performing
#' the crossover, if "one.p" is selected. The first quartile
#'  ("I.quart"), the second quartile ("II.quart", i.e. the median)
#'   and the third quartile ("III.quart") are allowed. Default is "I.quart"
#'
#' @return A matrix representing the "crossed" population. The dimensions of
#' this matrix are the same of 'chr.pop'
#'
#' @author Mattia Chiesa, Luca Piacentini
#'
#' @examples
#' data(GARS_popul)
#' crossed_pop <- GARS.Crossover(GARS_popul, co.rate=0.9)
#' crossed_pop <- GARS.Crossover(GARS_popul, type="two.p")
#' crossed_pop <- GARS.Crossover(GARS_popul, type="one.p",
#' one.p.quart= "II.quart")
#'
#' @seealso
#' \code{\link{GARS.Mutation}},
#' \code{\link{GARS.Selection}},
#' \code{\link{GARS.Elitism}},
#'
#' @export
#'
GARS.Crossover <- function(chr.pop,
                           co.rate=0.8,
                           type=c("one.p","two.p"),
                           one.p.quart=c("I.quart",
                                         "II.quart",
                                         "III.quart")){

  # check arguments
  if (missing(chr.pop))
    stop("'chr.pop' argument must be provided")
  if(!(is.matrix(chr.pop) | is.data.frame(chr.pop) ))
    stop("'chr.pop' must be a matrix or a data.frame")
  if(!(is.numeric(co.rate)))
    stop("'co.rate' must be numeric")
  if (missing(type)){
    type <- type[1]
  }
  if (missing(one.p.quart)){
    one.p.quart <- one.p.quart[1]
  }
  if (one.p.quart == "I.quart"){
    co.qt <- 0.25
  } else if(one.p.quart == "II.quart"){
    co.qt <- 0.5
  } else if(one.p.quart == "III.quart"){
    co.qt <- 0.75
  }else{
    stop("'one.p.quart' must be 'I.quart', 'II.quart' or 'II.quart'")
  }

  # check the presence of NA or Inf
  if (any(is.na(chr.pop)))
    stop("NA values are not allowed in the 'chr.pop' matrix")
  if (any(is.infinite(as.matrix(chr.pop))))

  # specific checks
  if (any(chr.pop <= 0))
      stop("'chr.pop' must have only positive values")
  if (any((chr.pop %% 1) != 0))
    stop("'chr.pop' must have only integer values")
  if ((dim(chr.pop)[2] %% 2) != 0)
    stop("The number of chromosomes must be even.")
  if (co.rate <= 0 | co.rate > 1)
    stop("'co.rate' must be > 0 and < 1")
  if (dim(chr.pop)[1] == 1)
    stop("'dim(chr.pop)[1]' must be greater than 1")


  # body
  chr_rnd <- sample(dim(chr.pop)[2])
  chr.pop <- chr.pop[,chr_rnd]

  if(type == "one.p"){
    # length of chr is even or not
    if ((dim(chr.pop)[1] %% 2)==0 ){
      co.pos <- round((dim(chr.pop)[1])*co.qt)
    }else{
      co.pos <- round((dim(chr.pop)[1]+1)*co.qt)
    }

  for (i in seq(1,dim(chr.pop)[2],2)){
    alea_iacta_est <-runif(1)
    if (alea_iacta_est <= co.rate){
      #bubblesort
      a <- c(chr.pop[seq_len(co.pos),i],
             chr.pop[(co.pos+1):dim(chr.pop)[1],i+1])
      b <- c(chr.pop[seq_len(co.pos),i+1],
             chr.pop[(co.pos+1):dim(chr.pop)[1],i])
      chr.pop[,i] <- a
      chr.pop[,i+1] <- b
    }
  }
  }else if(type == "two.p"){
    for (i in seq(1, dim(chr.pop)[2], 2)){
      alea <-runif(1)
      if (alea <= co.rate){
        co.pos_1 <- round((dim(chr.pop)[1])*0.25)
        co.pos_2 <- round((dim(chr.pop)[1])*0.75)
        #bubblesort
        a <- c(chr.pop[seq_len(co.pos_1),i],
               chr.pop[(co.pos_1+1):co.pos_2,i+1],
               chr.pop[(co.pos_2+1):dim(chr.pop)[1],i])

        b <- c(chr.pop[seq_len(co.pos_1),i+1],
               chr.pop[(co.pos_1+1):co.pos_2,i],
               chr.pop[(co.pos_2+1):dim(chr.pop)[1],i+1])
        chr.pop[,i] <- a
        chr.pop[,i+1] <- b
      }
    }
  }else{
    stop("Please set 'one.p or 'two.p' as Crossover type.")
  }

  return(chr.pop)
}
