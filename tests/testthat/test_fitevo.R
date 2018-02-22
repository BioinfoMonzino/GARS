## load sample data
data(GARS_fit_list)

## check input
expect_error(GARS_PlotFitnessEvolution())
expect_error(GARS_PlotFitnessEvolution("GARS_fit_list"))
expect_error(
  GARS_PlotFitnessEvolution(
    GARS_fit_list[sample(length(GARS_fit_list))]
  )
)
