#' @title This function implements the Fitness Function of GARS
#'
#' @description In GARS the Fitness Function consists in calculating the
#' Averaged Silhouette Index after a Multi-Dimensional Scaling
#'
#' @param data A \code{SummarizedExperiment} object or a matrix or
#'  a data.frame. In case of matrix or data.frame:
#'  \itemize{
#'   \item Rows and Cols have to be, respectively, observations
#'   and features. The variables are tipically genes;
#'   \item GARS also accept other -omic features as well as any
#'   continuous or factorial variables
#'   (e.g. sex, age, cholesterol level,...);
#'   \item Usually the number of observation is << than the number
#'   of features
#'  }'
#' @param classes A vector of type "factor" with \code{nrow(data)} elements.
#'  Each element represents the class label for each observation.
#' @param chr.pop A matrix or a data.frame representing the chromosomes
#' population: each column is a chromosome and each element corresponds
#'  to the feature position in the expression data matrix
#'
#' @return A numeric vector where each element corresponds to
#'  the fitness score of each chromosome in 'chr.pop'
#'
#' @author Mattia Chiesa, Luca Piacentini
#'
#' @examples
#' # use example data:
#' data(GARS_data_norm)
#' data(GARS_classes)
#' data(GARS_popul)
#' fitness_scores <- GARS_FitFun(GARS_data_norm, GARS_classes, GARS_popul)
#'
#' @seealso
#' \code{\link{GARS_create_rnd_population}}
#'
#' @export
#'
GARS_FitFun <- function(data, classes, chr.pop){

  # check arguments
  if (missing(data))
    stop("'data' argument must be provided")
  if (missing(classes))
    stop("'classes' argument must be provided")
  if (missing(chr.pop))
    stop("'chr.pop' argument must be provided")
  if(!(
    is.matrix(data) | is.data.frame(data) | is(data, "SummarizedExperiment")
  ))
    stop("'data' must be a matrix, a data.frame or a SummarizedExperiment")
  if(is(data, "SummarizedExperiment")){
    idx_class <- which(colnames(colData(data)) %in% "class")
    if (length(idx_class) == 0){
      warning("'class' label not defined in colData(data). colData(data)[1] was used as 'class'")
      classes <- colData(data)[1][,1]
    }else{
      classes <- as.factor(classes$class)
    }
    data <- t(assay(data))

  }
  if(!(is.factor(classes)))
    stop("'classes' must be a factor")
  if(!(is.matrix(chr.pop) | is.data.frame(chr.pop) ))
    stop("'chr.pop' must be a matrix or a data.frame")

  # check the presence of NA or Inf
  if (any(is.na(data)))
    stop("NA values are not allowed in the 'data' matrix")
  if (any(is.infinite(as.matrix(data))))
    stop("Inf values are not allowed in the 'data' matrix")
  if (any(is.na(chr.pop)))
    stop("NA values are not allowed in the 'chr.pop' matrix")
  if (any(is.infinite(as.matrix(chr.pop))))
    stop("Inf values are not allowed in the 'chr.pop' matrix")

  # specific checks
  if (all(data == 0))
    stop("All elements are 0. Check the 'data' matrix!")
  if (any(chr.pop <= 0))
    stop("'chr.pop' must have only positive values")
  if (any((chr.pop %% 1) != 0))
    stop("'chr.pop' must have only integer values")
  if(dim(data)[1] != length(classes))
    stop("nrow(data) must be equal to length(classes)")
  if ((dim(chr.pop)[2] %% 2) != 0)
    stop("The number of chromosomes must be even.")
  if (dim(chr.pop)[1] == 1)
    stop("'dim(chr.pop)[1]' must be greater than 1")

  myFitFun <- function(x, data, classes){
    index_chr <- x
    chr_data <- data[,index_chr]

    # perform MDS
    mydist <- dist(chr_data) # Euclidean
    sampleDistMatrix <- as.matrix(mydist)
    colnames(sampleDistMatrix) <- rownames(chr_data)
    rownames(sampleDistMatrix) <- rownames(chr_data)
    mdsData <- data.frame(cmdscale(sampleDistMatrix))
    mds <- as.data.frame(cbind(mdsData, classes))

    # Calculate Averaged Silhouette Index
    SI_list <- silhouette(as.numeric(classes),
                     dmatrix = as.matrix(dist(mdsData)))
    mean_SI <- mean(SI_list[,3])
    return(mean_SI)
  }

  mean_SI <-0
  mean_SI <- apply(chr.pop, 2, myFitFun, data=data, classes=classes)
  mean_SI[which(mean_SI < 0)] <- 0

  return(mean_SI)

}
