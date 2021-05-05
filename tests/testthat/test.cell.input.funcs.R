context("check cell input functions")
#
testthat::test_that ("evaluate path definitions for cells",{
  #
  out <- list()
  out$Slide_ID = 'T6,T7,T8'
  out$Antibody <- 'PD1'
  out$Opal1 <- '650'
  out$Concentration <- '250,500,1000,2000'
  out$protocol.type <- '7color'
  out$Naming.convention<-TRUE
  out$titration.type<-"Primary Antibody"
  out$a.type = 'cells'
  wd = 'E:/working_code/R/sample data/Phenotype'
  out$Vars_cells <- c('folders','flowout','phenotyped','decile.logical')
  #
  out.gen <- check.gen.input.vars(out)
  out.chkbx <- check.chkbx.input.vars(out)
  #
  # error on cancel
  #
  wd = NA
  testthat::expect_error(check.define.input.paths(out.chkbx, out.gen, wd))
  #
  # normal method
  #
  wd = 'E:/working_code/R/sample data/Phenotype'
  out.paths = check.define.input.paths(out.chkbx, out.gen, wd)
  testthat::expect_match(out.paths$wd, wd)
  testthat::expect_vector(out.paths$paths, ptype = character(), 4)
  #
  # same folder
  #
  wd = 'E:/working_code/R/sample data/Phenotype/1folder'
  out$Vars_cells <- c('flowout', 'decile.logical','phenotyped')
  out.chkbx <- check.chkbx.input.vars(out)
  out.paths = check.define.input.paths(out.chkbx, out.gen, wd)
  testthat::expect_match(out.paths$wd, wd)
  testthat::expect_vector(out.paths$paths, ptype = character(), 4)
  #
})
#
testthat::test_that ("evaluate cell variables",{
  #
  out <- list()
  out$Slide_ID = 'T6,T7,T8'
  out$Antibody <- 'PD1'
  out$Opal1 <- '650'
  out$Concentration <- '250,500,1000,2000'
  out$protocol.type <- '7color'
  out$Naming.convention<-TRUE
  out$titration.type<-"Primary Antibody"
  out$compartment <- 'Membrane'
  out$pheno.antibody <- 'PD1'
  out$a.type = 'cells'
  out$Vars_cells <- c('folders','flowout','phenotyped','decile.logical')
  out.gen <- check.gen.input.vars(out)
  wd = 'E:/working_code/R/sample data/Phenotype'
  #
  out.chkbx <- mIFTO::check.chkbx.input.vars(out)
  out.cells <- mIFTO::check.cell.input.vars(out, out.gen, out.chkbx)
  testthat::expect_false(out.px$ihc.logical)
  #
  out$Vars_pxp <- c('folders','flowout', 'decile.logical')
  out.chkbx <- mIFTO::check.chkbx.input.vars(out)
  out.px = mIFTO::check.pixel.input.vars(out, out.gen, out.chkbx)
  testthat::expect_false(out.px$ihc.logical)
  #
})
#
testthat::test_that ("evaluate whole check vars function for cells",{
  #
  out <- list()
  out$Slide_ID = 'T6,T7,T8'
  out$Antibody <- 'PD1'
  out$Opal1 <- '650'
  out$Concentration <- '250,500,1000,2000'
  out$protocol.type <- '7color'
  out$Naming.convention<-TRUE
  out$titration.type<-"Primary Antibody"
  out$compartment <- 'Membrane'
  out$pheno.antibody <- 'PD1'
  out$a.type = 'cells'
  out$Vars_cells <- c('folders','flowout','phenotyped','decile.logical')
  wd = 'E:/working_code/R/sample data/Phenotype'
  #
  outchecked <- mIFTO::check.vars(out, wd)
  #
  testthat::expect_match(outchecked$wd, wd)
  testthat::expect_vector(outchecked$Slide_ID, ptype = character(), size = 3)
  testthat::expect_vector(outchecked$Concentration, ptype = double(), size = 4)
  testthat::expect_match(outchecked$Antibody, 'PD1')
  testthat::expect_match(outchecked$Opal1, '650')
  testthat::expect_match(outchecked$Antibody_Opal, 'PD1 \\(650\\)')
  testthat::expect_match(outchecked$Protocol, '7color')
  testthat::expect_equal(outchecked$err.val, 0)
  testthat::expect_equal(outchecked$num.of.tiles, 10)
  testthat::expect_true(outchecked$flowout)
  testthat::expect_false(outchecked$ihc.logical)
  testthat::expect_true(outchecked$m.folders)
  testthat::expect_false(outchecked$threshold.logical)
  testthat::expect_true(outchecked$decile.logical)
  testthat::expect_vector(outchecked$out.thresh$ihc.connected.pixels, ptype = list(), size = 3)
  testthat::expect_vector(outchecked$out.thresh$ihc.Thresholds, ptype = list(), size = 3)
  testthat::expect_match(outchecked$titration.type.name, 'PD1')
  testthat::expect_vector(outchecked$paths, ptype = character(), 4)
  testthat::expect_vector(outchecked$out.thresh$Thresholds, ptype = list(), size = 3)
  testthat::expect_vector(outchecked$out.thresh$connected.pixels, ptype = list(), size = 3)
  testthat::expect_vector(outchecked$out.thresh$Thresholds$T6, ptype = double(), size = 4)
  testthat::expect_vector(outchecked$out.thresh$connected.pixels$T6, ptype = double(), size = 4)
  #
})
#