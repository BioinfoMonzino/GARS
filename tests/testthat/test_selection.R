## load data
data(GARS_popul)
data(GARS_Fitness_score)

## check input
expect_error(GARS_Selection())
expect_error(GARS_Selection(GARS_popul))
expect_error(GARS_Selection("GARS_popul"))
expect_error(GARS_Selection(GARS_popul,"GARS_Fitness_score"))
expect_error(GARS_Selection(GARS_popul, type="foo", GARS_Fitness_score))
expect_error(GARS_Selection(as.list(GARS_popul), GARS_Fitness_score))
expect_error(GARS_Selection(GARS_popul, as.matrix(GARS_Fitness_score)))

## check output
selected_pop <- GARS_Selection(GARS_popul, "RW", GARS_Fitness_score)

expect_true(dim(selected_pop)[1] == dim(GARS_popul)[1])
expect_true(dim(selected_pop)[2] == dim(GARS_popul)[2])
expect_is(selected_pop,"matrix")
