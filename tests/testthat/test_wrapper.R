# load data

data(GARS_data_norm)
data(GARS_classes)

# check inputs
expect_error(GARS.GA())
expect_error(GARS.GA(GARS_data_norm))
expect_error(GARS.GA("GARS_data_norm",
                     GARS_classes,
                     chr.num = 100,
                     chr.len = 10,
                     generation = 5,
                     co.rate = 0.8,
                     mut.rate = 0.01,
                     n.elit = 10,
                     type.sel = "RW",
                     type.co = "one.p",
                     type.one.p.co = "II.quart",
                     n.gen.conv = 80,
                     plots = "no",
                     verbose = "no")
)
expect_error(GARS.GA(GARS_data_norm,
                     GARS_classes,
                     chr.num = -1,
                     chr.len = 10,
                     generation = 5,
                     co.rate = 0.8,
                     mut.rate = 0.01,
                     n.elit = 10,
                     type.sel = "RW",
                     type.co = "one.p",
                     type.one.p.co = "II.quart",
                     n.gen.conv = 80,
                     plots = "no",
                     verbose = "no")
)
expect_error(GARS.GA(GARS_data_norm,
                     GARS_classes,
                     chr.num = "100",
                     chr.len = 10,
                     generation = 5,
                     co.rate = 0.8,
                     mut.rate = 0.01,
                     n.elit = 10,
                     type.sel = "RW",
                     type.co = "one.p",
                     type.one.p.co = "II.quart",
                     n.gen.conv = 80,
                     plots = "no",
                     verbose = "no")
)
expect_error(GARS.GA(GARS_data_norm,
                     GARS_classes,
                     chr.num = 100,
                     chr.len = "10",
                     generation = 5,
                     co.rate = 0.8,
                     mut.rate = 0.01,
                     n.elit = 10,
                     type.sel = "RW",
                     type.co = "one.p",
                     type.one.p.co = "II.quart",
                     n.gen.conv = 80,
                     plots = "no",
                     verbose = "no")
)
expect_error(GARS.GA(GARS_data_norm,
                     GARS_classes,
                     chr.num = 100,
                     chr.len = 10,
                     generation = "5",
                     co.rate = 0.8,
                     mut.rate = 0.01,
                     n.elit = 10,
                     type.sel = "RW",
                     type.co = "one.p",
                     type.one.p.co = "II.quart",
                     n.gen.conv = 80,
                     plots = "no",
                     verbose = "no")
)
expect_error(GARS.GA(GARS_data_norm,
                     GARS_classes,
                     chr.num = 100,
                     chr.len = 10,
                     generation = 5,
                     co.rate = "0.8",
                     mut.rate = 0.01,
                     n.elit = 10,
                     type.sel = "RW",
                     type.co = "one.p",
                     type.one.p.co = "II.quart",
                     n.gen.conv = 80,
                     plots = "no",
                     verbose = "no")
)
expect_error(GARS.GA(GARS_data_norm,
                     GARS_classes,
                     chr.num = 100,
                     chr.len = 10,
                     generation = 5,
                     co.rate = 0.8,
                     mut.rate = "0.01",
                     n.elit = 10,
                     type.sel = "RW",
                     type.co = "one.p",
                     type.one.p.co = "II.quart",
                     n.gen.conv = 80,
                     plots = "no",
                     verbose = "no")
)
expect_error(GARS.GA(GARS_data_norm,
                     GARS_classes,
                     chr.num = 100,
                     chr.len = 10,
                     generation = 5,
                     co.rate = 0.8,
                     mut.rate = 0.01,
                     n.elit = "10",
                     type.sel = "RW",
                     type.co = "one.p",
                     type.one.p.co = "II.quart",
                     n.gen.conv = 80,
                     plots = "no",
                     verbose = "no")
)

expect_error(GARS.GA(GARS_data_norm,
                     GARS_classes,
                     chr.num = 100,
                     chr.len = 10,
                     generation = 5,
                     co.rate = 0.8,
                     mut.rate = 0.01,
                     n.elit = 10,
                     type.sel = "RW",
                     type.co = "one.p",
                     type.one.p.co = "II.quart",
                     n.gen.conv = "80",
                     plots = "no",
                     verbose = "no")
)

expect_error(GARS.GA(GARS_data_norm,
                     GARS_classes,
                     chr.num = -100,
                     chr.len = 10,
                     generation = 5,
                     co.rate = 0.8,
                     mut.rate = 0.01,
                     n.elit = 10,
                     type.sel = "RW",
                     type.co = "one.p",
                     type.one.p.co = "II.quart",
                     n.gen.conv = 80,
                     plots = "no",
                     verbose = "no")
)

expect_error(GARS.GA(GARS_data_norm,
                     GARS_classes,
                     chr.num = 100,
                     chr.len = -10,
                     generation = 5,
                     co.rate = 0.8,
                     mut.rate = 0.01,
                     n.elit = 10,
                     type.sel = "RW",
                     type.co = "one.p",
                     type.one.p.co = "II.quart",
                     n.gen.conv = 80,
                     plots = "no",
                     verbose = "no")
)

expect_error(GARS.GA(GARS_data_norm,
                     GARS_classes,
                     chr.num = 100,
                     chr.len = 10,
                     generation = -5,
                     co.rate = 0.8,
                     mut.rate = 0.01,
                     n.elit = 10,
                     type.sel = "RW",
                     type.co = "one.p",
                     type.one.p.co = "II.quart",
                     n.gen.conv = 80,
                     plots = "no",
                     verbose = "no")
)

expect_error(GARS.GA(GARS_data_norm,
                     GARS_classes,
                     chr.num = 100,
                     chr.len = 10,
                     generation = -5,
                     co.rate = 11,
                     mut.rate = 0.01,
                     n.elit = 10,
                     type.sel = "RW",
                     type.co = "one.p",
                     type.one.p.co = "II.quart",
                     n.gen.conv = 80,
                     plots = "no",
                     verbose = "no")
)

expect_error(GARS.GA(GARS_data_norm,
                     GARS_classes,
                     chr.num = 100,
                     chr.len = 10,
                     generation = 5,
                     co.rate = 0.8,
                     mut.rate = -1,
                     n.elit = 10,
                     type.sel = "RW",
                     type.co = "one.p",
                     type.one.p.co = "II.quart",
                     n.gen.conv = 80,
                     plots = "no",
                     verbose = "no")
)

expect_error(GARS.GA(GARS_data_norm,
                     GARS_classes,
                     chr.num = 100,
                     chr.len = 10,
                     generation = 5,
                     co.rate = 0.8,
                     mut.rate = 0.01,
                     n.elit = -10,
                     type.sel = "RW",
                     type.co = "one.p",
                     type.one.p.co = "II.quart",
                     n.gen.conv = 80,
                     plots = "no",
                     verbose = "no")
)


expect_error(GARS.GA(GARS_data_norm,
                     GARS_classes,
                     chr.num = 100,
                     chr.len = 10,
                     generation = 5,
                     co.rate = 0.8,
                     mut.rate = 0.01,
                     n.elit = 10,
                     type.sel = "RW",
                     type.co = "one.p",
                     type.one.p.co = "II.quart",
                     n.gen.conv = -80,
                     plots = "no",
                     verbose = "no")
)


expect_error(GARS.GA(GARS_data_norm,
                     GARS_classes,
                     chr.num = 100,
                     chr.len = 10,
                     generation = 5,
                     co.rate = 0.8,
                     mut.rate = 0.01,
                     n.elit = 10,
                     type.sel = "foo",
                     type.co = "one.p",
                     type.one.p.co = "II.quart",
                     n.gen.conv = 80,
                     plots = "no",
                     verbose = "no")
)

expect_error(GARS.GA(GARS_data_norm,
                     GARS_classes,
                     chr.num = 100,
                     chr.len = 10,
                     generation = 5,
                     co.rate = 0.8,
                     mut.rate = 0.01,
                     n.elit = 10,
                     type.sel = "RW",
                     type.co = "foo",
                     type.one.p.co = "II.quart",
                     n.gen.conv = 80,
                     plots = "no",
                     verbose = "no")
)

expect_error(GARS.GA(GARS_data_norm,
                     GARS_classes,
                     chr.num = 100,
                     chr.len = 10,
                     generation = 5,
                     co.rate = 0.8,
                     mut.rate = 0.01,
                     n.elit = 10,
                     type.sel = "RW",
                     type.co = "one.p",
                     type.one.p.co = "foo",
                     n.gen.conv = 80,
                     plots = "no",
                     verbose = "no")
)

expect_error(GARS.GA(GARS_data_norm,
                     GARS_classes,
                     chr.num = 100,
                     chr.len = 10,
                     generation = 5,
                     co.rate = 0.8,
                     mut.rate = 0.01,
                     n.elit = 10,
                     type.sel = "RW",
                     type.co = "one.p",
                     type.one.p.co = "II.quart",
                     n.gen.conv = 80,
                     plots = "foo",
                     verbose = "no")
)

expect_error(GARS.GA(GARS_data_norm,
                     GARS_classes,
                     chr.num = 100,
                     chr.len = 10,
                     generation = 5,
                     co.rate = 0.8,
                     mut.rate = 0.01,
                     n.elit = 10,
                     type.sel = "RW",
                     type.co = "one.p",
                     type.one.p.co = "II.quart",
                     n.gen.conv = 80,
                     plots = "no",
                     verbose = "foo")
)

# check outputs

chr_num <- 100
chr_len <- 10
generat <- 5

res <- GARS.GA(GARS_data_norm,
               GARS_classes,
               chr.num = chr_num,
               chr.len = chr_len,
               generation = generat,
               co.rate = 0.8,
               mut.rate = 0.01,
               n.elit = 10,
               type.sel = "RW",
               type.co = "one.p",
               type.one.p.co = "II.quart",
               n.gen.conv = 80,
               plots = "no",
               verbose = "no")

expect_is(res, "list")
expect_is(res$data_red, "matrix")
expect_is(res$last_pop, "matrix")
expect_is(res$pop_list, "list")
expect_is(res$fit_list, "numeric")

expect_true(length(res) == 4)
expect_true(length(res$pop_list) == generat)
expect_true(length(res$fit_list) == generat)
expect_true(dim(res$data_red)[2] == chr_len)
expect_true(dim(res$last_pop)[1] == chr_len)
expect_true(dim(res$data_red)[1] == dim(GARS_data_norm)[1])
expect_true(dim(res$data_red)[1] == length(GARS_classes))
