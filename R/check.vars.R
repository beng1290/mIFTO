#################################check.vars#####################################

#'Check that the input variables are accurate and get a working directory
#'CheckVars
#'Created By: Benjamin Green
#'Last Edited 09/24/2019
#'
#'Description
#'This function was designed to check input variables for any unforseen errors
#' that may occur
#'
#'
#' @param out is the list of variables given by the GUI function
#' @param a.type is the type of analysis; cell, pixel, or tissue
#' @return exports multiple variables for use in the main titration codes
#' @export
#'
check.vars <- function(out, a.type) {
  #
  ## General Input -----------------------------
  #
  out.gen <- mIFTO::check.gen.input.vars(out)
  err.val <- out.gen$err.val
  #
  if (err.val != 0) {
    return(err.val)
  }
  #
  ## Check-Box Input -----------------------------
  #
  out.chkbx <- mIFTO::check.chkbx.input.vars(out, a.type)
  #
  ## Define Paths -------------------------------
  #
  out.paths <- mIFTO::check.define.input.paths(out.chkbx, out.gen)
  #
  err.val <- out.paths$err.val
  if (err.val != 0) {
    return(err.val)
  }
  #
  ## Analysis specific Input -----------------------------
  #
  if (grepl(a.type, 'pixels')){
    out.px <- mIFTO::check.pixel.input.vars(out, out.gen, out.chkbx)
    err.val <- out.px$err.val
  } else if (grepl(a.type, 'cells')){
    out.cell <- check.cell.input.vars(out, out.gen, out.chkbx)
    err.val <- out.cell$err.val
  } else if (grepl(a.type, 'tissue')){
    out.cell <- check.tissue.input.vars(out, out.gen, out.chkbx)
    err.val <- out.cell$err.val
  }
  #
  if (err.val != 0) {
    return(err.val)
  }



  return(outnew)
}
