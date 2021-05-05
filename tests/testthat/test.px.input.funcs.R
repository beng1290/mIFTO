context("test pixel input functions")

#
testthat::test_that ("evaluate path definitions",{
  #
  out <- list()
  out$Slide_ID = 'T3,T6,TA,TD,TE'
  out$Antibody <- 'ERG'
  out$Opal1 <- '570'
  out$Concentration <- '800,1600,3200,6400'
  out$Thresholds <- '8.212,5.260,3.370,2.025'
  out$protocol.type <- '9color'
  out$Naming.convention<-TRUE
  out$titration.type<-"Primary Antibody"
  out$a.type = 'pixels'
  out$Vars_pxp <- c('folders','flowout', 'decile.logical',
                    'ihc','threshold.logical')
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
  wd = 'E:/working_code/R/sample data/erg'
  out.paths = check.define.input.paths(out.chkbx, out.gen, wd)
  testthat::expect_match(out.paths$wd, wd)
  testthat::expect_vector(out.paths$paths, ptype = character(), 4)
  #
  # same folder
  #
  wd = 'E:/working_code/R/sample data/erg/m.data'
  out$Vars_pxp <- c('flowout', 'decile.logical',
                    'ihc','threshold.logical')
  out.chkbx <- check.chkbx.input.vars(out)
  out.paths = check.define.input.paths(out.chkbx, out.gen, wd)
  testthat::expect_match(out.paths$wd, wd)
  testthat::expect_vector(out.paths$paths, ptype = character(), 4)
  #
})
#
testthat::test_that ("evaluate pixel variables",{
  #
  out <- list()
  out$Slide_ID = 'T3,T6,TA,TD,TE'
  out$Antibody <- 'ERG'
  out$Opal1 <- '570'
  out$Concentration <- '800,1600,3200,6400'
  out$protocol.type <- '9color'
  out$Naming.convention<-TRUE
  out$titration.type<-"Primary Antibody"
  out$a.type = 'pixels'
  out.gen <- check.gen.input.vars(out)
  wd = 'E:/working_code/R/sample data/erg'
  #
  out$Vars_pxp <- c('folders','flowout', 'ihc')
  out.chkbx <- mIFTO::check.chkbx.input.vars(out)
  testthat::expect_error(mIFTO::check.pixel.input.vars(out, out.gen, out.chkbx))
  #
  out$Vars_pxp <- c('folders','flowout', 'ihc', 'decile.logical')
  out.chkbx <- mIFTO::check.chkbx.input.vars(out)
  testthat::expect_error(mIFTO::check.pixel.input.vars(out, out.gen, out.chkbx))
  #
  out$Vars_pxp <- c('folders','flowout', 'decile.logical')
  out.chkbx <- mIFTO::check.chkbx.input.vars(out)
  out.px <- mIFTO::check.pixel.input.vars(out, out.gen, out.chkbx)
  #
  out$Vars_pxp <- c('folders','flowout', 'threshold.logical')
  out$Thresholds <- '8.212,5.260,3.370,2.025'
  out$connected.pixels <- '20,20,20,20'
  out.chkbx <- mIFTO::check.chkbx.input.vars(out)
  out.px <- mIFTO::check.pixel.input.vars(out, out.gen, out.chkbx)
  testthat::expect_false(out.px$ihc.logical)
  #
  out$Vars_pxp <- c('folders','flowout', 'decile.logical',
                    'ihc','threshold.logical')
  out$Thresholds <- '8.212,5.260,3.370,2.025,2.4'
  out$connected.pixels <- '20,20,20,20,20'
  out.chkbx <- mIFTO::check.chkbx.input.vars(out)
  out.px = mIFTO::check.pixel.input.vars(out, out.gen, out.chkbx)
  testthat::expect_true(out.px$ihc.logical)
  #
})
#
testthat::test_that ("evaluate whole check vars function for pixels",{
  #
  out <- list()
  out$Slide_ID = 'T3,T6,TA,TD,TE'
  out$Antibody <- 'ERG'
  out$Opal1 <- '570'
  out$Concentration <- '800,1600,3200,6400'
  out$protocol.type <- '9color'
  out$Naming.convention<-TRUE
  out$titration.type<-"Primary Antibody"
  out$a.type = 'pixels'
  wd = 'E:/working_code/R/sample data/erg'
  out$Vars_pxp <- c('folders','flowout', 'decile.logical',
                    'ihc','threshold.logical')
  out$Thresholds <- '8.212,5.260,3.370,2.025,2.4'
  out$connected.pixels <- '20,20,20,20,20'
  #
  outchecked <- mIFTO::check.vars(out, wd)
  #
  testthat::expect_match(outchecked$wd, wd)
  testthat::expect_vector(outchecked$Slide_ID, ptype = character(), size = 5)
  testthat::expect_vector(outchecked$Concentration, ptype = double(), size = 4)
  testthat::expect_match(outchecked$Antibody, 'ERG')
  testthat::expect_match(outchecked$Opal1, '570')
  testthat::expect_match(outchecked$Antibody_Opal, 'ERG \\(570\\)')
  testthat::expect_match(outchecked$Protocol, '9color')
  testthat::expect_equal(outchecked$err.val, 0)
  testthat::expect_equal(outchecked$num.of.tiles, 10)
  testthat::expect_true(outchecked$flowout)
  testthat::expect_true(outchecked$ihc.logical)
  testthat::expect_true(outchecked$m.folders)
  testthat::expect_true(outchecked$threshold.logical)
  testthat::expect_true(outchecked$decile.logical)
  testthat::expect_vector(outchecked$out.thresh$ihc.connected.pixels, ptype = list(), size = 5)
  testthat::expect_vector(outchecked$out.thresh$ihc.Thresholds, ptype = list(), size = 5)
  testthat::expect_match(outchecked$titration.type.name, 'ERG')
  testthat::expect_vector(outchecked$paths, ptype = character(), 4)
  testthat::expect_vector(outchecked$out.thresh$Thresholds, ptype = list(), size = 5)
  testthat::expect_vector(outchecked$out.thresh$connected.pixels, ptype = list(), size = 5)
  testthat::expect_vector(outchecked$out.thresh$Thresholds$T3, ptype = double(), size = 4)
  testthat::expect_vector(outchecked$out.thresh$connected.pixels$T3, ptype = double(), size = 4)
  #
})
#
testthat::test_that ("evaluate whole check vars function for decile only pixels",{
  #
  out <- list()
  out$Slide_ID = 'T3,T6,TA,TD,TE'
  out$Antibody <- 'ERG'
  out$Opal1 <- '570'
  out$Concentration <- '800,1600,3200,6400'
  out$protocol.type <- '9color'
  out$Naming.convention<-TRUE
  out$titration.type<-"Primary Antibody"
  out$a.type = 'pixels'
  wd = 'E:/working_code/R/sample data/erg'
  out$Vars_pxp <- c('folders','flowout', 'decile.logical')
  out$Thresholds <- '8.212,5.260,3.370,2.025,2.4'
  out$connected.pixels <- '20,20,20,20,20'
  #
  outchecked <- mIFTO::check.vars(out, wd)
  #
  testthat::expect_match(outchecked$wd, wd)
  testthat::expect_vector(outchecked$Slide_ID, ptype = character(), size = 5)
  testthat::expect_vector(outchecked$Concentration, ptype = double(), size = 4)
  testthat::expect_match(outchecked$Antibody, 'ERG')
  testthat::expect_match(outchecked$Opal1, '570')
  testthat::expect_match(outchecked$Antibody_Opal, 'ERG \\(570\\)')
  testthat::expect_match(outchecked$Protocol, '9color')
  testthat::expect_equal(outchecked$err.val, 0)
  testthat::expect_equal(outchecked$num.of.tiles, 10)
  testthat::expect_true(outchecked$flowout)
  testthat::expect_false(outchecked$ihc.logical)
  testthat::expect_true(outchecked$m.folders)
  testthat::expect_false(outchecked$threshold.logical)
  testthat::expect_true(outchecked$decile.logical)
  testthat::expect_vector(outchecked$out.thresh$ihc.connected.pixels, ptype = list(), size = 5)
  testthat::expect_vector(outchecked$out.thresh$ihc.Thresholds, ptype = list(), size = 5)
  testthat::expect_match(outchecked$titration.type.name, 'ERG')
  testthat::expect_vector(outchecked$paths, ptype = character(), 4)
  testthat::expect_vector(outchecked$out.thresh$Thresholds, ptype = list(), size = 5)
  testthat::expect_vector(outchecked$out.thresh$connected.pixels, ptype = list(), size = 5)
  testthat::expect_vector(outchecked$out.thresh$Thresholds$T3, ptype = double(), size = 4)
  testthat::expect_vector(outchecked$out.thresh$connected.pixels$T3, ptype = double(), size = 4)
  #
})
