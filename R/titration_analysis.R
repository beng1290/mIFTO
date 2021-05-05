#########################titration_analysis################################

#'Used by mIFTOapp to do titration_analysis on individual images for
#'IF titrations;
#'Created By: Benjamin Green, Charles Roberts;
#'Last Edited 02/11/2021
#'
#'This function is designed to aid in the analysis of an IF titration series. 
#' The code collects data (pixel, cell seg, or tissue segmented data) from the
#' inForm output and, using specified input to the mIFTOapp UI, takes particular
#' measurements, performs statistical testing and outputs data tables and graphical
#' pdfs. In this code each image is treated as a separate data point taken for 
#' each slide/ concentration pair. Data is grouped and graphed accordingly.
#'
#' The code is meant to be run through the mIFTOapp, so particular input to the
#' 'out' variable is not specified in great detail. Additional information can 
#' be found in the mIFTO readme file in the github repo at AstropathJHU/mIFTO
#'
#'
#' @param out is the list of variables given by the GUI function
#' @param pb is the progress bar created by the GUI
#' @return exports a variety of graphs and tables, for more information please 
#' reference the readme file in the github repo
#' @export
#'
titation_analysis <- function(out, pb.Object, a.type) {
  ##############################input parameters########################
  #
  pb.count = 0; mIFTO::doupdate.pgbar(
    pb.count, pb.Object, 'Browse For Folder')
  #
  # check input parameters and allocate some for easier indexing
  #
  oc <- mIFTO::check.vars(out, a.type)
  err.val <- outchecked$err.val
  if (err.val != 0) {
    return(err.val)
  }
  #
  ##############################create results folders##################
  #
  pb.count = 1; mIFTO::doupdate.pgbar(
    pb.count, pb.Object, 'Generating Folders')
  v <- mIFTO::create.dir(oc$wd, a.type, oc$flowout)
  rm(v)
  #
  ###############################Reads in data##########################
  #
  time <- system.time(
    Tables <- mIFTO::populate.tables(a.type, oc, pb.count, pb.Object)
  )
  err.val <- Tables$err.val
  if (err.val != 0) {
    return(err.val)
  }
  #
  time1 <- time[['elapsed']]/60
  mins <- round(time1, digits = 0)
  secs <- round(60 * (time1 - mins), digits = 0)
  #
  if (sign(secs) == -1 ){
    mins = mins - 1
    secs = 60 + secs
  }
  #
  mIFTO::doupdate.pgbar(90, pb.Object, paste0(
    'Finished gathering image data - Elapsed Time: ',
    mins, ' mins ', secs,' secs'))
  Sys.sleep(0.5)
  #
  ##################prepares some parameters for the graphs#############
  #
  graph.out <- mIFTO::create.my.theme(Antibody_Opal)
  theme1 <- graph.out$theme1
  colors <- graph.out$colors
  Antibody_Opal.snratio <- graph.out$Antibody_Opal.snratio
  Antibody_Opal.ttest <- graph.out$Antibody_Opal.ttest
  con_type <- 'factor'
  #
  ###############################generate plots#########################
  #
  if (threshold.logical){
    mIFTO::map.and.plot.threshold.graphs(
      wd, Antibody_Opal, Antibody, Slide_Descript, Concentration, Tables,
      Thresholds, connected.pixels, ihc.logical, ihc.Thresholds,
      ihc.connected.pixels, m.folders, theme1, con_type, colors,
      Antibody_Opal.snratio, Antibody_Opal.ttest, pb.Object)
  }
  #
  # some decile graphs
  #
  if (decile.logical){
    mIFTO::map.and.plot.decile.graphs(
      wd, Antibody_Opal, Antibody, Slide_Descript, Concentration, Tables,
      theme1, con_type, colors, Antibody_Opal.snratio, Antibody_Opal.ttest,
      pb.Object)
  }
  #
  ###############################Histogram Graphs ######################
  #
  ii = 97;mIFTO::doupdate.pgbar(
    ii, pb.Object, 'Generating Histogram Graphs')
  #
  mIFTO::map.and.write.histograms(
    wd, Antibody_Opal, Slide_Descript,
    Concentration, Thresholds, Tables$Tables.wholeslide, theme1, colors)
  #
  ############################### Finished #############################
  #
  mIFTO::doupdate.pgbar(100, pb.Object, 'Fin')
  #
  return(err.val)
}
