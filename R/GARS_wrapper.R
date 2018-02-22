#' @title The wrapper fuction to use GARS
#'
#' @description This function allows the users to run all GARS
#' funtion at once. This is the easier and recommended way to
#' use GARS.
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
#' @param classes The class vector
#' @param chr.num The number of chromosomes to generate. Default is
#' 1000
#' @param chr.len The length of chromosomes. This value corresponds
#' to the desired length of the feature set
#' @param generation The maximum number of generations. Default is 1000
#' @param co.rate The probability of each random couple of chromosomes
#'  to swap some parts. It must be between 0 and 1. Default is 0.8
#' @param mut.rate The probability to apply a random mutation to each
#' element. It must be between 0 and 1. Default is 0.01
#' @param n.elit The number of best chromosomes to be selected by
#' elitism. This number must be even. Default is 10
#' @param type.sel The type of selection method; Roulette Wheel ("RW")
#' and Tournament Selection ("TS") are allowed. Default is "RW"
#' @param type.co The type of crossover method; one-point ("one.p")
#' and two-point ("two.p") are allowed. Default is "one.p"
#' @param type.one.p.co The position of the cromosome where performing
#' the crossover, if "one.p" is selected. The first quartile
#'  ("I.quart"), the second quartile ("II.quart", i.e. the median)
#'   and the third quartile ("III.quart") are allowed. Default is "I.quart"
#' @param n.gen.conv The number of consecutive generations with the same
#' maximum fitness score.
#' @param plots If graphs have to be plotted;
#'  "yes" or "no" are allowed. Default is "yes"
#' @param n.Feat_plot The number of features to be plotted
#' @param verbose If statistics have to be printed;
#'  "yes" or "no" are allowed. Default is "yes"
#'
#'
#' @return A GarsSelectedFeatures object, containg:
#' \describe{
#'  \item{data_red}{a matrix of selected features}
#'  \item{last_pop}{a matrix containg the last chromosome population}
#'  \item{pop_list}{a list containing all the populations
#'   produced over the generations}
#'  \item{fit_list}{a numeric vector containing the maximum fitness scores,
#'   computed in each generation}
#' }
#'
#' @author Mattia Chiesa, Luca Piacentini
#'
#' @examples
#' # use example data:
#' data(GARS_data_norm)
#' data(GARS_classes)
#'
#' res_ex <- GARS_GA(GARS_data_norm,
#'    GARS_classes,
#'    chr.num = 100,
#'    chr.len=10,
#'    generation = 5,
#'    co.rate = 0.8,
#'    mut.rate = 0.1,
#'    n.elit = 10,
#'    type.sel = "RW",
#'    type.co ="one.p",
#'    type.one.p.co = "II.quart",
#'    n.gen.conv = 80,
#'    plots = "no",
#'    verbose = "no")
#'
#' @export
#'
GARS_GA <- function(data,
                     classes,
                     chr.num = 1000,
                     chr.len,
                     generation = 500,
                     co.rate = 0.8,
                     mut.rate = 0.01,
                     n.elit = 10,
                     type.sel = c("RW",
                                "TS"),
                     type.co = c("one.p",
                               "two.p"),
                     type.one.p.co = c("I.quart",
                                     "II.quart",
                                     "III.quart"),
                    n.gen.conv = 80,
                    plots = c("yes","no"),
                    n.Feat_plot = 10,
                    verbose = c("yes","no")){
  # check arguments
  if (missing(data))
    stop("'data' argument must be provided")
  if (missing(chr.len))
    stop("'chr.len' argument must be provided")
  if(!(
    is.matrix(data) | is.data.frame(data) | is(data, "SummarizedExperiment")
    ))
    stop("'data' must be a matrix, a data.frame or a SummarizedExperiment")
  if (missing(classes))
    stop("'classes' argument must be provided")
  if(!(is.numeric(chr.num)))
    stop("'chr.num' must be numeric")
  if(!(is.numeric(chr.len)))
    stop("'chr.len' must be numeric")
  if(!(is.numeric(n.elit)))
    stop("'n.elit' must be numeric")
  if(!(is.numeric(co.rate)))
    stop("'co.rate' must be numeric")
  if(!(is.numeric(mut.rate)))
    stop("'mut.rate' must be numeric")
  if(!(is.numeric(generation)))
    stop("'generation' must be numeric")
  if(!(is.numeric(n.gen.conv)))
    stop("'n.gen.conv' must be numeric")
  if(!(is.numeric(n.Feat_plot)))
    stop("'n.Feat_plot' must be numeric")

  if (missing(type.sel)){
    type.sel <- type.sel[1]
  }
  if (missing(type.co)){
    type.co <- type.co[1]
  }
  if (missing(type.one.p.co)){
    type.one.p.co <- type.one.p.co[1]
  }
  if (missing(plots)){
    plots <- plots[1]
  }
  if (missing(verbose)){
    verbose <- verbose[1]
  }

  if (!(type.sel %in% "RW" | type.sel %in% "TS"))
    stop("'type.sel' must be 'RW' or 'TS'")
  if (!(type.co %in% "one.p" | type.co %in% "two.p"))
    stop("'type.co' must be 'one.p' or 'two.p'")
  if (!(type.one.p.co %in% "I.quart" |
        type.one.p.co %in% "II.quart" |
        type.one.p.co %in% "III.quart"))
    stop("'type.one.p.co' must be 'I.quart', 'II.quart' or 'III.quart'")
  if (!(plots %in% "yes" | plots %in% "no"))
    stop("'plots' must be 'yes' or 'no'")
  if (!(verbose %in% "yes" | verbose %in% "no"))
    stop("'verbose' must be 'yes' or 'no'")
  if(is(data, "SummarizedExperiment")){
    data <- t(assay(data))
    classes <- as.factor(classes$class)
  }
  if(!(is.factor(classes)))
    stop("'classes' must be a factor")

  # check the presence of NA or Inf
  if (any(is.na(data)))
    stop("NA values are not allowed in the 'data' matrix")
  if (any(is.infinite(as.matrix(data))))
    stop("Inf values are not allowed in the 'data' matrix")

  # specific checks
  if (all(data == 0))
    stop("All elements are 0. Check the 'data' matrix!")
  if(dim(data)[1] != length(classes))
    stop("nrow(data) must be equal to nrow(classes)")
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
  if (n.elit < 2 | n.elit >= chr.num)
    stop("'n.elit' must be >= 2 and < chr.num")
  if (co.rate <= 0 | co.rate > 1)
    stop("'co.rate' must be > 0 and < 1")
  if (mut.rate <= 0 | mut.rate > 1)
    stop("'mut.rate' must be > 0 and < 1")
  if (n.gen.conv <= 0)
    stop("'n.gen.conv' must be > 0")
  if ((n.gen.conv %% 1) != 0)
    stop("'n.gen.conv' must be integer")

  if(verbose %in% "yes"){
  # Print Message
  cat("GARS has been set with these parameters:", "\n",
      "Number of starting features:", dim(data)[2], "\n",
      "Number of samples:", dim(data)[1], "\n",
      "Number of classes:", length(levels(classes)), "\n",
      "Number of chromosomes:", chr.num,"\n",
      "Length of chromosomes (i.e. number of desired features):",
      chr.len,
      "\n",
      "Number of maximum generations:", generation, "\n",
      "Crossing-Over rate:", co.rate, "\n",
      "Mutation rate:", mut.rate, "\n",
      "Number of chromosomes kept by elitism:", n.elit, "\n",
      "Type of Selection method:", type.sel, "\n",
      "Type of CrossingOver method:", type.co, "\n",
      if(type.co %in% "one.p")
        "Position of the one-Point Crossover:", type.one.p.co, "\n",
      "Number of max generations allowed with the same Fitness:",
      n.gen.conv, "\n",
      "Produce graphs:", plots, "\n\n",
      "##############################", "\n",
      "####    GARS is running   ####", "\n",
      "##############################", "\n\n")
  }
  ###
  ## Start GA
  ###
  # create random population
  rand_popul <- GARS_create_rnd_population(data,
                                           chr.num = chr.num,
                                           chr.len = chr.len)
  popul <- rand_popul
  fit_list <- 0
  pop_list <- list()

  # launch GA
  for (j in seq_len(generation)){
    if (j %% 10 ==0)
    cat("Reached", j, "iterations...",
        "\n")

    # Fitness Function
    fitn_scores <- GARS_FitFun(data, classes, popul)

    # elitism
    res.elit <- GARS_Elitism(popul, fitn_scores, n.elit)

    # Round selection
    chr_pop_selected <- GARS_Selection(res.elit$chr.pop.non.elit,
                                       type.sel,
                                       res.elit$fitn.sort.non.elit)

    # cross-over
    chr_pop_crossed <- GARS_Crossover(chr_pop_selected,
                                      co.rate,
                                      type.co,
                                      type.one.p.co)

    # mutation
    chr_pop_mutated <- GARS_Mutation(chr_pop_crossed,
                                     mut.rate,dim(data)[2])

    popul <- cbind(res.elit$chr.pop.elit, chr_pop_mutated)

    # save info for downstream analysis
    fit_list[j] <- max(fitn_scores)
    pop_list[[j]] <- popul

    # check the evolution
    if (j > n.gen.conv){
      if(fit_list[j-n.gen.conv] == fit_list[j]){
        j_stop <- j-n.gen.conv + 1
        break
      }else{
        j_stop <- j
      }
    }else{
      j_stop <- j
  }
  }
  cat("GARS found a solution after", j_stop, "iterations.",
      "\n")

  # Plot and Print some statistics
  pop_unlist <- unlist(pop_list)
  feat_Coverage <- as.data.frame(table(pop_unlist))
  perc_covered <- (dim(feat_Coverage)[1]/dim(data)[2])*100
  best_chr<-data[,popul[,1],drop=FALSE]

  if(verbose %in% "yes"){
  cat("With these parameters, the best solution:", "\n\n",
      "1. is reached after", j_stop , "iterations;", "\n",
      "2. is reached looking at the", round(perc_covered,2),
      "% of the", dim(data)[2], "features;", "\n",
      "3. got a maximum fitness score =", round(fit_list[j],2), "\n",
      "4. is composed of the following features:", "\n",
      colnames(best_chr), "\n")
  }

  # plots
  if (plots %in% "yes"){
    #dev.new()
    GARS_PlotFitnessEvolution(fit_list)
    gene_selected <- colnames(data)
    #dev.new()
    GARS_PlotFeaturesUsage(pop_list, gene_selected,
                              nFeat = n.Feat_plot)
  }

  res_GA <- new("GarsSelectedFeatures",
                data_red = best_chr,
                last_pop = popul,
                pop_list = pop_list,
                fit_list = fit_list)


  # export useful data
  return(res_GA)
}
