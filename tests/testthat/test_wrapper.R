# load data

data(GARS_data_norm)
data(GARS_classes)

# check inputs
expect_error(GARS_GA())
expect_error(GARS_GA(GARS_data_norm))
expect_error(GARS_GA("GARS_data_norm",
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
expect_error(GARS_GA(GARS_data_norm,
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
expect_error(GARS_GA(GARS_data_norm,
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
expect_error(GARS_GA(GARS_data_norm,
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
expect_error(GARS_GA(GARS_data_norm,
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
expect_error(GARS_GA(GARS_data_norm,
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
expect_error(GARS_GA(GARS_data_norm,
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
expect_error(GARS_GA(GARS_data_norm,
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

expect_error(GARS_GA(GARS_data_norm,
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

expect_error(GARS_GA(GARS_data_norm,
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

expect_error(GARS_GA(GARS_data_norm,
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

expect_error(GARS_GA(GARS_data_norm,
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

expect_error(GARS_GA(GARS_data_norm,
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

expect_error(GARS_GA(GARS_data_norm,
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

expect_error(GARS_GA(GARS_data_norm,
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


expect_error(GARS_GA(GARS_data_norm,
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


expect_error(GARS_GA(GARS_data_norm,
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

expect_error(GARS_GA(GARS_data_norm,
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

expect_error(GARS_GA(GARS_data_norm,
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

expect_error(GARS_GA(GARS_data_norm,
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

expect_error(GARS_GA(GARS_data_norm,
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

res <- GARS_GA(GARS_data_norm,
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

expect_is(res, "GarsSelectedFeatures")
expect_is(MatrixFeatures(res), "matrix")
expect_is(LastPop(res), "matrix")
expect_is(AllPop(res), "list")
expect_is(FitScore(res), "numeric")

expect_true(length(res) == 1)
expect_true(length(slotNames(res)) == 4)
expect_true(length(AllPop(res)) == generat)
expect_true(dim(MatrixFeatures(res))[2] == chr_len)
expect_true(dim(LastPop(res))[1] == chr_len)
expect_true(dim(MatrixFeatures(res))[1] == dim(GARS_data_norm)[1])
expect_true(dim(MatrixFeatures(res))[1] == length(GARS_classes))
