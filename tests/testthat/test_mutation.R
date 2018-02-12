## load sample data
data(GARS_popul)
data(GARS_data_norm)

totFeatures <- dim(GARS_data_norm)[2]


## check input
expect_error(GARS.Mutation())
expect_error(GARS.Mutation("GARS_popul"))
expect_error(GARS.Mutation(GARS_popul))
expect_error(GARS.Mutation(GARS_popul, mut.rate = 0.8))
expect_error(GARS.Mutation(GARS_popul, mut.rate = 11, totFeatures))
expect_error(GARS.Mutation(GARS_popul, mut.rate = -1, totFeatures))
expect_error(GARS.Mutation(GARS_popul, mut.rate = "0.8", totFeatures))
expect_error(GARS.Mutation(GARS_popul, mut.rate = 0.8))
expect_error(GARS.Mutation(GARS_popul, mut.rate = 0.8, "totFeatures"))
expect_error(GARS.Mutation(as.list(GARS_popul),
                           mut.rate = 0.8,
                           totFeatures))
## check outputs

pop_mutated <- GARS.Mutation(GARS_popul,
                             mut.rate = 0.8,
                             totFeatures)
expect_true(dim(pop_mutated)[1] == dim(GARS_popul)[1])
expect_true(dim(pop_mutated)[2] == dim(GARS_popul)[2])
expect_is(pop_mutated,"matrix")
