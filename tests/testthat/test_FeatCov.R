## load sample data
data(GARS_data_norm)
data(GARS_pop_list)
allfeat_names <- colnames(GARS_data_norm)

expect_error(GARS_PlotFeaturesUsage())
expect_error(GARS_PlotFeaturesUsage("GARS_pop_list", allfeat_names , nFeat = 10))
expect_error(GARS_PlotFeaturesUsage(GARS_pop_list, "allfeat_names" , nFeat = 10))
expect_error(GARS_PlotFeaturesUsage(GARS_pop_list, allfeat_names , nFeat = "10"))
expect_error(GARS_PlotFeaturesUsage(GARS_pop_list, allfeat_names , nFeat = -1.3))
expect_error(GARS_PlotFeaturesUsage(GARS_pop_list, allfeat_names , nFeat = 10000))
expect_error(GARS_PlotFeaturesUsage(GARS_pop_list, allfeat_names , nFeat = "10"))
expect_error(GARS_PlotFeaturesUsage(GARS_pop_list, 11 , nFeat = 10))
