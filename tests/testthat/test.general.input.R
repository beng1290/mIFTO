context("check general input function")

testthat::test_that ("check general input returns a named list",{
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
  #
  out2 <- check.gen.input.vars(out)
  testthat::expect_named(out2, c('Slide_ID','Concentration','Antibody',
                                 'Opal1','Antibody_Opal',
                       'titration.type.name','Protocol','err.val'))
  testthat::expect_vector(out2$Slide_ID, ptype = character(), size = 5)
  testthat::expect_vector(out2$Concentration, ptype = double(), size = 4)
  testthat::expect_match(out2$Antibody, 'ERG')
  testthat::expect_match(out2$Opal1, '570')
  testthat::expect_match(out2$Antibody_Opal, 'ERG \\(570\\)')
  testthat::expect_match(out2$Protocol, '9color')
  testthat::expect_equal(out2$err.val, 0)
})
#
testthat::test_that ("evaluate titration.type.name in different configurations",{
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
  #
  out2 <- check.gen.input.vars(out)
  testthat::expect_match(out2$titration.type.name, 'ERG')
  #
  out$titration.type<-"Fluorophore (TSA)"
  out2 <- check.gen.input.vars(out)
  testthat::expect_match(out2$titration.type.name, '570')
  #
  out$Naming.convention<-FALSE
  out2 <- check.gen.input.vars(out)
  testthat::expect_match(out2$titration.type.name, '')
  #
})
#
testthat::test_that ("check general input returns errors on empty conditions",{
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
  #
  out$Slide_ID = ''
  testthat::expect_error(check.gen.input.vars(out))
  out$Slide_ID = 'T3, T6,TA,TD, TE'
  #
  out$Slide_ID = ''
  testthat::expect_error(check.gen.input.vars(out))
  out$Slide_ID = 'T3,T6,TA,TD,TE'
  #
  out$Antibody = ''
  testthat::expect_error(check.gen.input.vars(out))
  out$Antibody = 'ERG'
  #
  out$Opal1 = ''
  testthat::expect_error(check.gen.input.vars(out))
  out$Opal1 = '570'
  #
  out$Concentration = ''
  testthat::expect_error(check.gen.input.vars(out))
  out$Concentration = '800,1600,3200,6400'
  #
})
#
testthat::test_that ("evaluate checkbox selection",{
  #
  out <- list()
  a.type = 'pixels'
  out$Vars_pxp <- c('folders','flowout', 'decile.logical',
                    'ihc','threshold.logical')
  #
  out2 <- check.chkbx.input.vars(out, a.type)
  testthat::expect_true(out2$flowout)
  testthat::expect_true(out2$m.folders)
  testthat::expect_true(out2$ihc.logical)
  testthat::expect_true(out2$flowout)
  testthat::expect_true(out2$threshold.logical)
  testthat::expect_false(out2$named)
  testthat::expect_equal(out2$num.of.tiles, 10)
  testthat::expect_false(out2$phenotyped)
  #
  a.type = 'cells'
  out2 <- check.chkbx.input.vars(out, a.type)
  testthat::expect_false(out2$flowout)
  #
  out$Vars_cells <- c('folders','flowout', 'decile.logical',
                    'ihc','threshold.logical', 'phenotyped')
  out2 <- check.chkbx.input.vars(out, a.type)
  testthat::expect_true(out2$flowout)
  testthat::expect_true(out2$phenotyped)
  #
})
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
  a.type = 'pixels'
  out$Vars_pxp <- c('folders','flowout', 'decile.logical',
                    'ihc','threshold.logical')
  out.gen <- check.gen.input.vars(out)
  out.chkbx <- check.chkbx.input.vars(out, a.type)
  wd = 'E:/working_code/R/sample data/erg'
  #
  out.paths = check.define.input.paths(out.chkbx, out.gen)
  testthat::expect_match(out.paths$wd, wd)
  #
})
#
