#########################run.titration.analysis################################

#'The main workflow wrapper for analysis; reads files, collects data, and 
#'produces output
#'Created By: Benjamin Green;
#'Last Edited 09/02/2020
#'
#'This function is desgined to do analysis for IF titration series
#'in cell, tissue, or pixel data provding output grouped by slide and 
#' concentrations; both in an image and a cell setting
#'
#'It is meant to be run from the mIFTOapp
#'
#'decile data will always be outputed; (or 1/100th depending if
#''sparse' option is choose in the GUI) if threshold information is
#' filled out in the GUI; threshold analysis will be run
#'
#' @param out is the list of variables given by the GUI function
#' @param pb is the progress bar created by the GUI
#' @param a.type is the type of analysis; cell, pixel, or tissue
#' @return exports a variety of graphs displayed in the documentation
#'  Such as SNRatio graphs, t statisitics and graphs,
#'  histograms of the log intensity profiles
#'  for images, positivity measures given thresholds
#' @export
#'
run.titration.analysis <- function(out, pb.Object, a.type) {
  ##############################input parameters########################
  #
  pb.count = 0; mIFTO::doupdate.pgbar(
    pb.count, pb.Object, 'Browse For Folder')
  #
  # check input parameters and allocate some for eaiser indexing
  #
  outchecked <- mIFTO::check.vars(out, a.type)
  err.val <- outchecked$err.val
  if (err.val != 0) {
    return(err.val)
  }
  #
  # general use
  #
  wd <- outchecked$wd
  Slide_Descript <- outchecked$Slide_ID
  Antibody <- outchecked$Antibody
  Opal1 <- outchecked$Opal1
  Antibody_Opal <- outchecked$Antibody_Opal
  Concentration <- outchecked$Concentration
  flowout <- outchecked$flowout
  ihc.logical <- outchecked$ihc.logical
  m.folders <- outchecked$m.folders
  Protocol <- outchecked$Protocol
  paths <- outchecked$paths
  titration.type.name <- outchecked$titration.type.name
  decile.logical <- outchecked$decile.logical
  step.value <- outchecked$num.of.tiles
  #
  # analysis specific variables
  #
  # pixels
  ihc.connected.pixels <- outchecked$ihc.connected.pixels
  ihc.Thresholds <- outchecked$ihc.Thresholds
  connected.pixels <- outchecked$connected.pixels
  Thresholds <- outchecked$Thresholds
  threshold.logical <- outchecked$threshold.logical
  # cells
  compartment = outchecked$compartment
  phenotype.logical = outchecked$phenotyped
  named = outchecked$named
  pheno.antibody = outchecked$pheno.antibody
  #
  #
  rm(outchecked, out)
  #
  ##############################create results folders##################
  #
  pb.count = 1; mIFTO::doupdate.pgbar(
    pb.count, pb.Object, 'Generating Folders')
  v <- mIFTO::create.dir(wd,a.type, flowout)
  rm(v)
  #
  ###############################Reads in data##########################
  #
  time <- system.time(
    Tables <- mIFTO::populate.tables(
      a.type, Slide_Descript, Concentration, Antibody_Opal, Thresholds, Opal1,
      flowout, Protocol, paths, titration.type.name, decile.logical, step.value, 
      connected.pixels, threshold.logical, compartment, phenotype.logical,
      pheno.antibody, pb.count, pb.Object
    )
  )
  
}