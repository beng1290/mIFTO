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
#' @return exports multiple variables for use in the main titration codes
#' @export
#'
check.vars <- function(out, wd = choose.dir(
  caption = 'Select the folder the data is contained in')) {
  #
  ## General Input -----------------------------
  #
  out.gen <- mIFTO::check.gen.input.vars(out)
  err.val <- out.gen$err.val
  #
  if (err.val != 0) {
    return(list(err.val = err.val))
  }
  #
  ## Check-Box Input -----------------------------
  #
  out.chkbx <- mIFTO::check.chkbx.input.vars(out)
  #
  ## Define Paths -------------------------------
  #
  out.paths <- mIFTO::check.define.input.paths(out.chkbx, out.gen, wd)
  #
  err.val <- out.paths$err.val
  if (err.val != 0) {
    return(list(err.val = err.val))
  }
  #
  ## Analysis specific Input -----------------------------
  #
  if (grepl(out$a.type, 'pixels')){
    outnew <- mIFTO::check.pixel.input.vars(out, out.gen, out.chkbx)
    err.val <- outnew$err.val
  } else if (grepl(out$a.type, 'cells')){
    outnew <- check.cell.input.vars(out, out.gen, out.chkbx)
    err.val <- outnew$err.val
  } else if (grepl(out$a.type, 'tissue')){
    outnew<- check.tissue.input.vars(out, out.gen, out.chkbx)
    err.val <- outnew$err.val
  }
  #
  if (err.val != 0) {
    return(list(err.val = err.val))
  }
  #
  outnew <- c(outnew, out.paths, out.chkbx, out.gen)
  #
  return(outnew)
  #
}
