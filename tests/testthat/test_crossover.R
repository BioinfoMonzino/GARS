## load sample data
data(GARS_popul)

## check input
expect_error(GARS_Crossover())
expect_error(GARS_Crossover("GARS_popul"))
expect_error(GARS_Crossover(GARS_popul, co.rate = 11))
expect_error(GARS_Crossover(GARS_popul, co.rate = -1))
expect_error(GARS_Crossover(GARS_popul, co.rate = "0.8"))
expect_error(GARS_Crossover(GARS_popul, type = 3))
expect_error(GARS_Crossover(GARS_popul, type = "three.p"))
expect_error(GARS_Crossover(GARS_popul, one.p.quart = "IIII.quart"))
expect_error(GARS_Crossover(GARS_popul, one.p.quart = 14))


## check outputs

pop_crossed <- GARS_Crossover(GARS_popul)

expect_true(dim(pop_crossed)[1] == dim(GARS_popul)[1])
expect_true(dim(pop_crossed)[2] == dim(GARS_popul)[2])
expect_is(pop_crossed,"matrix")
