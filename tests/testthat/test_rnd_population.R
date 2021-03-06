## load sample data
data(GARS_data_norm)

## check inputs
expect_error(GARS_create_rnd_population())
expect_error(GARS_create_rnd_population(GARS_data_norm))
expect_error(GARS_create_rnd_population("GARS_data_norm"))
expect_error(GARS_create_rnd_population(as.list(GARS_data_norm),
                                        chr.len=10,
                                        chr.num=100))
expect_error(GARS_create_rnd_population(GARS_data_norm,
                                        chr.len="10"))
expect_error(GARS_create_rnd_population(GARS_data_norm,
                                        chr.len=10,
                                        chr.num="100"))
expect_error(GARS_create_rnd_population(GARS_data_norm,
                                        chr.len=100000,
                                        chr.num=10))
expect_error(GARS_create_rnd_population(GARS_data_norm,
                                        chr.len=-4,
                                        chr.num=10))
expect_error(GARS_create_rnd_population(GARS_data_norm,
                                        chr.len=4,
                                        chr.num=-10))
expect_warning(GARS_create_rnd_population(GARS_data_norm,
                                          chr.len=10,
                                          chr.num=1))


## check outputs
two_chrom <- GARS_create_rnd_population(GARS_data_norm,
                                        chr.len=10,
                                        chr.num=2)
expect_true(dim(two_chrom)[2] == 2)
expect_is(two_chrom,"matrix")
