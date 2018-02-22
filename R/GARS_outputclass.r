#' The output class 'GarsSelectedFeatures'
#'
#' The output class for GARS_GA function
#'
#' @name GarsSelectedFeatures class
#' @rdname GarsSelectedFeatures-class
#' @docType class
#' @slot data_red a matrix containing the expression
#' values for the selected feature
#' @slot last_pop a matrix containing the chromosome
#' population of the last generation
#' @slot pop_list a list containing all the populations
#' produced over the generations
#' @slot fit_list a vector containing the maximum fitness scores
#' @examples
#' showClass("GarsSelectedFeatures")
#' @exportClass

setClass("GarsSelectedFeatures",
         slots = list(data_red = "matrix",
                      last_pop = "matrix",
                      pop_list = "list",
                      fit_list = "numeric"))
