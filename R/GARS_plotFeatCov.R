#' @title A bubble chart to assess the usage of each features
#'
#' @description This function allows assessing visually how many times
#'  a feature is selected across the generations. In principle, a
#'  highly recurring feature is more likely to be important.
#'
#' @param popul.list A SummarizedExpression object
#' @param allFeat A character vector containing the list of the all
#' features name. Often, it corresponds to the columns name of the data
#' matrix.
#' @param nFeat The number of features which have to be plotted.
#' Default is '\code{length(allFeat)}'
#'
#' @return A bubble chart where each plotted feature is represented by a
#' colored circle. A feature is important (i.e. conserved) if the size is
#' wide and the color tends to red; the smaller the size, the lighter
#' the color and less informative the feature.
#'
#'
#' @author Mattia Chiesa, Luca Piacentini
#'
#' @examples
#' # use example data:
#' data(GARS_data_norm)
#' data(GARS_pop_list)
#' allfeat_names <- colnames(GARS_data_norm)
#' GARS.PlotFeaturesUsage(GARS_pop_list, allfeat_names, nFeat = 10)
#' @seealso
#' \code{\link{GARS.PlotFitnessEvolution}}
#'
#' @export
#'
GARS.PlotFeaturesUsage <- function(popul.list,
                                   allFeat,
                                   nFeat = length(allFeat) ){

  if (missing(popul.list))
    stop("'popul.list' argument must be provided")
  if (missing(allFeat))
    stop("'allFeat' argument must be provided")
  if(!(is.list(popul.list)))
    stop("'popul.list' must be a list")
  if(!(is.character(allFeat)))
    stop("'allFeat' must be a character vector")
  if(!(is.numeric(nFeat)))
    stop("'nFeat' must be numeric")
  if (nFeat <= 0)
    stop("'nFeat' must be > 0")
  if ((nFeat %% 1) != 0)
    stop("'nFeat' must be integer")
  if(length(allFeat) < nFeat)
    stop("'nFeat' cannot be greater than length(allFeat)")


  # handle data
  pop_unlist <- unlist(popul.list)
  feat_Cov <- as.data.frame(table(pop_unlist))
  rownames(feat_Cov) <- feat_Cov$pop_unlist
  feats_name <- allFeat[as.numeric(rownames(feat_Cov))]
  rownames(feat_Cov) <- feats_name

  # subset features for plotting
  feat_Cov2 <- feat_Cov[order(-feat_Cov$Freq),][seq_len(nFeat),]
  feat_Cov2$x <- rev(seq_len(nFeat))
  colnames(feat_Cov2) <- c("pop_unlist", "Times", "Feature")
  p <- ggplot(feat_Cov2, aes(x = Feature, y = Times,
                                  label=rownames(feat_Cov2),
                                  fill=Times)) +
      geom_point(aes(size = Times), shape=21) +
      geom_text(size = 4, hjust =0.5, vjust = -1) +
      scale_size(range = c(1,10)) +
      theme_bw() +
      scale_fill_continuous(low = "orange", high = "red3") +
      ggtitle("Top Features used across all generations") +
      scale_x_continuous(breaks = seq(1, nFeat, 1))
  print(p)

}
