# load data
data(GARS_data_norm)
data(GARS_classes)
data(GARS_popul)

# check input
expect_error(GARS.FitFun())
expect_error(GARS.FitFun(GARS_data_norm))
expect_error(GARS.FitFun(GARS_data_norm, GARS_classes))
expect_error(GARS.FitFun(GARS_data_norm, GARS_popul))
expect_error(GARS.FitFun("GARS_data_norm", GARS_classes, GARS_popul))
expect_error(GARS.FitFun(GARS_data_norm, "GARS_classes", GARS_popul))
expect_error(GARS.FitFun(GARS_data_norm, GARS_classes, "GARS_popul"))
expect_error(GARS.FitFun(as.list(GARS_data_norm), GARS_classes, GARS_popul))
expect_error(GARS.FitFun(GARS_data_norm,
                         as.numeric(GARS_classes),
                         GARS_popul))
expect_error(GARS.FitFun(GARS_data_norm, GARS_classes[1:5], GARS_popul))


# check output
fitness_scores <-GARS.FitFun(GARS_data_norm, GARS_classes, GARS_popul)
expect_true(length(fitness_scores) == dim(GARS_popul)[2])
