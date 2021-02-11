#################################check.chkbx.input.vars#####################################

#' evaluate the checkbox input values
#'Created By: Benjamin Green
#'Last Edited 09/24/2019
#'
#'Description
#'This function was designed to evaluate the check box input values for later 
#'use
#'
#' @param out is the list of variables given by the GUI function
#' @param a.type is the type of analysis; cell, pixel, or tissue
#' @return exports multiple variables for use in the main titration codes
#' @export
#'
check.chkbx.input.vars <- function(out, a.type){
  #
  # set up Vars variable to respond to checkbox input for various types
  #
  if (a.type == 'pixels') {
    Vars <- out$Vars_pxp
  } else if (a.type == 'cells'){
    Vars <- out$Vars_cell
  } else if (a.type == 'tissue'){
    Vars <- out$Vars_tissue
  }
  #
  if (!is.null(Vars)){
    Vars <- paste(Vars, collapse = ", ")
  } else {
    Vars <- ','
  }
  #
  # whether or not to output the flow like csv results
  #
  if (grepl(paste0("flowout"), Vars)) {
    flowout <- TRUE
  } else {
    flowout <- FALSE
  }
  #
  # whether or not to output decile results
  #
  if(grepl(paste0("decile.logical"), Vars)) {
    decile.logical <- TRUE
  } else {
    decile.logical <- FALSE
  }
  #
  # whether or not an ihc was done and images are present
  #
  if(grepl(paste0("ihc"), Vars)) {
    ihc.logical <- TRUE
  } else {
    ihc.logical <- FALSE
  }
  #
  if(grepl(paste0("folders"), Vars)) {
    m.folders <- TRUE
  } else {
    m.folders <- FALSE
  }
  #
  #
  # check if the data was thresholded or not
  #
  if(grepl("threshold.logical",Vars)) {
    threshold.logical <- TRUE
  } else {
    threshold.logical <- FALSE
  }
  #
  # whether or not the ABs were named in inForm
  #
  if (grepl(paste0("named"), Vars)) {
    named <- TRUE
  } else {
    named <- FALSE
  }
  #
  # whether ab of interest was sparse
  #
  if (grepl(paste0("ab_sparse"), Vars)) {
    num.of.tiles <- 100
  } else {
    num.of.tiles <- 10
  }
  #
  if (grepl(paste0("phenotyped"), Vars)) {
    phenotyped <- TRUE
    #
    if (!(a.type == 'cells')){
      n <- shiny::showNotification(
        'Phenotype analysis can only be done on cells',
        type = 'warning')
      phenotyped <- FALSE
    }
    #
  } else {
    phenotyped <- FALSE
  }
  #
  out.chkbx <- list(Vars = Vars, flowout = flowout,
                    decile.logical = decile.logical,
                    ihc.logical = ihc.logical,
                    a.type = a.type, 
                    m.folders = m.folders,
                    threshold.logical = threshold.logical,
                    named = named,
                    num.of.tiles = num.of.tiles,
                    phenotyped = phenotyped)
}