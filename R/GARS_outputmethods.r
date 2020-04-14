#' @title Accessors for the 'MatrixFeatures' slot of a GarsSelectedFeatures object.
#'
#' @description The MatrixFeatures slot contains the reduced dataset
#'
#' @docType methods
#' @rdname MatrixFeatures
#' @aliases MatrixFeatures MatrixFeatures,GARS-MatrixFeatures
#' @param x a \code{GarsSelectedFeatures} object
#' @return a matrix with the reduced dataset
#'
#' @author Mattia Chiesa, Luca Piacentini
#'
#' @examples
#' data(GARS_res_GA)
#' ex_matrix <- MatrixFeatures(GARS_res_GA)
#' @export
setMethod("MatrixFeatures",
          c(x = "GarsSelectedFeatures"),
          function(x){
              data_matrix <- x@data_red
              return(data_matrix)
          })


#' @title Accessors for the 'LastPop' slot of a GarsSelectedFeatures object.
#'
#' @description The LastPop slot contains the last chromosome population
#'
#' @docType methods
#' @rdname LastPop
#' @aliases LastPop LastPop,GARS-LastPop
#' @param x a \code{GarsSelectedFeatures} object
#' @author Mattia Chiesa, Luca Piacentini
#' @return a matrix containing the last population
#' @examples
#' data(GARS_res_GA)
#' ex_pop <- LastPop(GARS_res_GA)
#' @export
setMethod("LastPop",
          c(x = "GarsSelectedFeatures"),
          function(x){
              data_LastPop <- x@last_pop
              return(data_LastPop)
          })


#' @title Accessors for the 'AllPop' slot of a GarsSelectedFeatures object.
#'
#' @description The AllPop slot contains the list of populations
#'
#' @docType methods
#' @rdname AllPop
#' @aliases AllPop AllPop,GARS-AllPop
#' @param x a \code{GarsSelectedFeatures} object
#' @author Mattia Chiesa, Luca Piacentini
#' @return a list containing all the populations
#' @examples
#' data(GARS_res_GA)
#' ex_pop <- AllPop(GARS_res_GA)
#' @export
setMethod("AllPop",
          c(x = "GarsSelectedFeatures"),
          function(x){
              data_allPop <- x@pop_list
              return(data_allPop)
          })

#' @title Accessors for the 'FitScore' slot of a GarsSelectedFeatures object.
#'
#' @description The FitScore slot contains the fitness values over the generations
#'
#' @docType methods
#' @rdname FitScore
#' @aliases FitScore FitScore,GARS-FitScore
#' @param x a \code{GarsSelectedFeatures} object
#' @author Mattia Chiesa, Luca Piacentini
#' @return a vector containing the fitness scores
#' @examples
#' data(GARS_res_GA)
#' ex_pop <- FitScore(GARS_res_GA)
#' @export

setMethod("FitScore",
          c(x = "GarsSelectedFeatures"),
          function(x){
              vector_fit_list <- x@fit_list
              return(vector_fit_list)
          })

