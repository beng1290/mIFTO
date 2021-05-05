#################################check.cell.input.vars#########################

#' check the cell tab input values
#'Created By: Benjamin Green
#'Last Edited 09/03/2020
#'
#'Description
#'This function was designed to check input variables from the cell input
#' tab for any errors that have occurred in input
#'
#'
#' @param out is the list of variables given by the GUI function
#' @param out.chckbx the values from the analysis check boxes in logical format
#' @param out.gen the values from the general input tab
#' @return exports multiple variables for use in the main titration codes
#' @export
#'
check.cell.input.vars <- function(out, out.gen, out.chkbx){
  #
  # if threshold, phenotype, and decile pixels are false then return an error
  #
  if ((!out.chkbx$threshold.logical & !out.chkbx$decile.logical & !out.chkbx$phenotyped)){
    modal_out <- shinyalert::shinyalert(
      title = "ERROR: Must apply thresholds, phenotype, or run quantile analysis.",
      text = paste(
        "All analysis types cannot be left blank, please threshold, phenotype, or", 
        "run the quantile analysis."
      ),
      type = 'error',
      showConfirmButton = TRUE
    )
    err.val <- 4
    return(list(err.val = err.val))
  }
  #
  # cell compartment to analyze
  #
  compartment<-out$compartment 
  #
  # what was the name used for the positive antibody
  #
  pheno.antibody <- out$pheno.antibody #error checking
  #
  # ihc check 
  #
  if (out.chkbx$phenotyped | out.chkbx$threshold.logical) {
    ihc.logical = out.chkbx$ihc.logical
  } else {
    #
    if (out.chkbx$ihc.logical){
      n <- shiny::showNotification(
        paste("Must use phenotype or threshold analysis for",
              "IHC compatability, IHC analysis turned off."),
        type = 'warning')
      
    } 
    ihc.logical <- F
    #
  }
  #
  out.thresh <- mIFTO::check.threshold.input.vars(out, out.gen, out.chkbx, ihc.logical)
  #
  out.cell <- list(compartment = compartment, 
                   ihc.logical = ihc.logical,
                   out.thresh = out.thresh,
                   err.val = out.thresh$err.val)
  #
  }