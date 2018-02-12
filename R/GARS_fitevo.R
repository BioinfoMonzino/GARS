#' @title Plot the maximum fitness scores for each generation
#'
#' @description This function plots the maximum fitness scores for each
#' generation
#'
#' @param fitness.scores A numeric vector where each element corresponds
#'  to the fitness score
#'
#' @return A plot which represent the evolution of the fitness score
#' across the generations
#'
#' @author Mattia Chiesa, Luca Piacentini
#'
#' @examples
#' # use example data:
#' data(GARS_fit_list)
#' GARS.PlotFitnessEvolution(GARS_fit_list)
#'
#' @seealso
#' \code{\link{GARS.PlotFeaturesUsage}}
#'
#' @export
#'
GARS.PlotFitnessEvolution <- function(fitness.scores){


  if (missing(fitness.scores))
    stop("'fitness.List' argument must be provided")
  if(!(is.numeric(fitness.scores)))
    stop("'fitness.List' must be numeric")
  if(is.unsorted(fitness.scores))
    stop("'fitness.list' must be sorted")
  if(!(is.numeric(fitness.scores)))
    stop("'fitness.scores' must be numeric")
  if(!(is.vector(fitness.scores)))
    stop("'fitness.scores' must be a vector")

  #create df for ggplot
  df_plot <- as.data.frame(cbind(seq_len(length(fitness.scores)),
                                 fitness.scores))
  colnames(df_plot) <- c("V1","fit_list")

  #plot
  p <- ggplot(aes(x = V1, y = fit_list), data = df_plot) +
    geom_point(color="salmon1") +
    geom_line(color="salmon1") +
    ylim(min(fitness.scores)-0.01, max(fitness.scores)+0.01) +
    ggtitle("Maximum Fitness Evolution") +
    xlab("Generation") +
    ylab("max(fitness score)")

  print(p)
}
