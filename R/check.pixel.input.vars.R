#################################check.pixel.input.vars#########################

#' check the pixel tab input values
#'Created By: Benjamin Green
#'Last Edited 02/09/2021
#'
#'Description
#'This function was designed to check input variables from the pixel input
#' tab for any errors that have occurred in input
#'
#'
#' @param out is the list of variables given by the GUI function
#' @param out.chckbx the values from the analysis check boxes in logical format
#' @param out.gen the values from the general input tab
#' @return exports multiple variables for use in the main titration codes
#' @export
#'
check.pixel.input.vars <- function(out, out.gen, out.chkbx){
  #
  # if both threshold and decile logical are false then return an error
  #
  if ((!out.chkbx$threshold.logical & !out.chkbx$decile.logical)){
    modal_out <- shinyalert::shinyalert(
      title = "ERROR: Must apply thresholds or run quantile analysis.",
      text = paste(
        "Both analysis types cannot be left blank, please threshold the data or ",
        "run the quantile analysis."
      ),
      type = 'error',
      showConfirmButton = TRUE
    )
    err.val <- 4
    return(list(err.val = err.val))
  }
  #
  # ihc check
  #
  if (out.chkbx$threshold.logical) {
    ihc.logical = out.chkbx$ihc.logical
  } else {
    #
    if (out.chkbx$ihc.logical){
      n <- shiny::showNotification(
        paste("Must use threshold analysis for",
              "IHC compatability, IHC analysis turned off."),
        type = 'warning')
      
    } 
    ihc.logical <- F
    #
  }
  #
  out.thresh <- mIFTO::check.threshold.input.vars(out, out.gen, out.chkbx, ihc.logical)
  #
  out.px <- list(ihc.logical = ihc.logical,
                 out.thresh = out.thresh,
                 err.val = out.thresh$err.val)
  #
}