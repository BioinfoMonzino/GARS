## load sample data
data(GARS_popul)
data(GARS_Fitness_score)

## check input
expect_error(GARS_Elitism())
expect_error(GARS_Elitism("GARS_popul"))
expect_error(GARS_Elitism(as.list(GARS_popul),
                          GARS_Fitness_score,
                          n.elit = 10))
expect_error(GARS_Elitism(GARS_popul))
expect_error(GARS_Elitism(GARS_popul, "GARS_Fitness_score"))
expect_error(GARS_Elitism(GARS_popul, GARS_Fitness_score, n.elit = 1000))
expect_error(GARS_Elitism(GARS_popul, GARS_Fitness_score, n.elit = -1))
expect_error(GARS_Elitism(GARS_popul, GARS_Fitness_score, n.elit = 0))
expect_error(GARS_Elitism(GARS_popul, GARS_Fitness_score, n.elit = "10"))

## check outputs
chr_elit <-10
pop_list <- GARS_Elitism(GARS_popul, GARS_Fitness_score,chr_elit)
expect_true(length(pop_list) == 4)
expect_true(dim(pop_list$chr.pop.elit)[2] == chr_elit)
expect_true(
  dim(pop_list$chr.pop.non.elit)[2] == (dim(GARS_popul)[2] - chr_elit)
  )
expect_true(
  length(GARS_Fitness_score) == (length(pop_list$fitn.sort.elit)
                                 +length(pop_list$fitn.sort.non.elit)))



