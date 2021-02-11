#################################check.threshold.input.vars#########################

#' check the pixel tab input values
#'Created By: Benjamin Green
#'Last Edited 02/09/2021
#'
#'Description
#'This function was designed to check input variables from the threshold input
#' tab for any errors that have occurred in input
#'
#'
#' @param out is the list of variables given by the GUI function
#' @param out.chckbx the values from the analysis check boxes in logical format
#' @param out.gen the values from the general input tab
#' @return exports multiple variables for use in the main titration codes
#' @export
#'
check.threshold.input.vars <- function(out, out.gen, out.chkbx, ihc.logical){
  #
  # create the threshold values and connected pixel values
  #
  if (out.chkbx$threshold.logical){
    if (!grepl("nConsistent",out.chkbx$Vars)) {
      #
      # if thresholds and connected.pixel values are the same across cases
      # add them into vector the size of the concentration * slideid
      #
      Thresholds = lapply(
        1:length(out.gen$Slide_ID), function(x)out$Thresholds
      )
      #
      connected.pixels <- lapply(
        1:length(out.gen$Slide_ID), function(x)out$connected.pixels
      )
    } else {
      #
      # if thresholds and connected.pixel values are not the same across cases
      # loop through the provided names as variables and add them into the vector
      #
      Thresholds = lapply(
        1:length(out.gen$Slide_ID), function(x)out[[paste0("Thresholds",x)]]
      )
      #
      connected.pixels <- lapply(
        1:length(out.gen$Slide_ID), function(x)out[[paste0("connected.pixels",x)]]
      )
      #
    }
    #
  } else {
    #
    # if no thresholds are included, still create the variables but fill with 0s
    #
    Thresholds = lapply(
      1:length(out.gen$Slide_ID), function(x)rep(0,length(out.gen$Concentration))
    )
    #
    connected.pixels <- lapply(
      1:length(out.gen$Slide_ID), function(x)rep(0,length(out.gen$Concentration))
    )
  }
  #
names(Thresholds) <- out.gen$Slide_ID
names(connected.pixels) <- out.gen$Slide_ID
#
ihc.Thresholds <- vector(mode = 'list', length= length(out.gen$Slide_ID))
names(ihc.Thresholds) <- out.gen$Slide_ID
ihc.connected.pixels <- vector(mode = 'list', length= length(out.gen$Slide_ID))
names(ihc.connected.pixels) <- out.gen$Slide_ID
#
v1 = 1
v2 = 1
v3 = 1
#
for (x in 1:length(out.gen$Slide_ID)){
  #
  if( grepl(' ',Thresholds[[x]],perl = TRUE )) {
    if (v1 == 1){
      n <- shiny::showNotification(
        'Thresholds contain spaces ... removing spaces in threshold list',
        type = 'warning')
      v1 = 0
    }
    Thresholds[[x]] <- gsub(" ", "",Thresholds[[x]], fixed = TRUE)
  } else {
    Thresholds[[x]] <- Thresholds[[x]]
  }
  #
  # try to convert to a valid string
  #
  Thresholds1 <- tryCatch({
    as.numeric(
      unlist(
        strsplit(
          Thresholds[[x]], split =','
        )
      )
    )
  }, warning = function(cond) {
    modal_out <- shinyalert::shinyalert(
      title = "Error in threshold input.",
      text = paste0(
        "Could not parse threshold input:", Thresholds[[x]],
        ". Please enter a valid list of numeric thresholds, separated by ",
        "commas."
      ),
      type = 'error',
      showConfirmButton = TRUE
    )
    return(-1)
  }, error = function(cond) {
    modal_out <- shinyalert::shinyalert(
      title = "Error in threshold input.",
      text = paste0(
        "Could not parse threshold input:", Thresholds[[x]],
        ". Please enter a valid list of numeric thresholds, separated by ",
        "commas."
      ),
      type = 'error',
      showConfirmButton = TRUE
    )
    return(-1)
  })
  #
  if (length(Thresholds1) == 1){
    if (Thresholds1 == -1){
      err.val = 3
      return(list(err.val = err.val))
    }
  }
  #
  Thresholds[[x]] <- Thresholds1
  #
  # remove the ihc threshold and place into a new vector if applicable
  #
  if (ihc.logical){
    ihc.Thresholds[[x]] <- Thresholds[[x]][[length(Thresholds[[x]])]]
    Thresholds[[x]] <- Thresholds[[x]][-length(Thresholds[[x]])]
  }
  #
  # check that the number of thresholds
  # == the number of concentrations
  #
  if (length(Concentration) != length(Thresholds[[x]])){
    modal_out <- shinyalert::shinyalert(
      title = "Error in threshold input.",
      text = paste(
        "The length of concentration list does",
        "not equal the length of threshold list"
      ),
      type = 'error',
      showConfirmButton = TRUE
    )
    err.val <- 3
    return(list(err.val = err.val))
  }
  #
  # set up connected pixel values
  #
  if( grepl(' ',connected.pixels[[x]],perl = TRUE ) ) {
    if (v2 == 1){
      n <- shiny::showNotification(
        paste("Connected pixel list contains spaces",
              "... removing spaces in connected pixel list"),
        type = 'warning')
      v2 = 0
    }
    connected.pixels[[x]] <- gsub(
      " ", "",connected.pixels[[x]], fixed = TRUE
    )
  } else {
    connected.pixels[[x]] <- connected.pixels[[x]]
  }
  #
  # try to convert to a valid string
  #
  connected.pixels1 <- tryCatch({
    as.numeric(
      unlist(
        strsplit(
          connected.pixels[[x]], split =','
        )
      )
    )
  }, warning = function(cond) {
    modal_out <- shinyalert::shinyalert(
      title = "Error in connected pixel input.",
      text = paste0(
        "Could not parse connected pixel input:", Thresholds[[x]],
        ". Please enter a valid list of numeric connected pixel values, ",
        "separated by commas."
      ),
      type = 'error',
      showConfirmButton = TRUE
    )
    return(-1)
  }, error = function(cond) {
    modal_out <- shinyalert::shinyalert(
      title = "Error in connected pixel input.",
      text = paste0(
        "Could not parse connected pixel input:", Thresholds[[x]],
        ". Please enter a valid list of numeric connected pixel values, ",
        "separated by commas."
      ),
      type = 'error',
      showConfirmButton = TRUE
    )
    return(-1)
  }
  )
  #
  if (length(connected.pixels1) == 1){
    if (connected.pixels1 == -1){
      err.val = 3
      return(list(err.val = err.val))
    }
  }
  #
  if(!isTRUE(all(connected.pixels1 == floor(connected.pixels1)))){
    if (v3 == 1){
      n <- shiny::showNotification(
        paste("Connected pixel list must contain",
              "integers ... rounding down"),
        type = 'warning')
      v3 = 0
    }
    connected.pixels1 <- floor(connected.pixels1)
  }
  connected.pixels[[x]] <- connected.pixels1
  #
  # remove the ihc con pixels and place into a new vector if applicable
  #
  if (ihc.logical){
    ihc.connected.pixels[[x]] <- connected.pixels[[x]][[length(connected.pixels[[x]])]]
    connected.pixels[[x]] <- connected.pixels[[x]][-length(connected.pixels[[x]])]
  }
  #
  # check that the number of conn pixels
  # == the number of concentrations
  #
  if (length(Concentration) != length(connected.pixels[[x]])){
    modal_out <- shinyalert::shinyalert(
      title = "Error in connected pixels input.",
      text = paste(
        "The length of concentration list does",
        "not equal the length of connected pixels list"
      ),
      type = 'error',
      showConfirmButton = TRUE
    )
    err.val <- 3
    return(list(err.val = err.val))
  }
  #
}
#
# check the eb image installation
#
out.eb.check <- mIFTO::check.EBImage.install(connected.pixels)
err.val <- out.eb.check$err.val
#
if (err.val != 0) {
  return(err.val)
}
out.px <- list(Thresholds = Thresholds, ihc.Thresholds = ihc.Thresholds,
               connected.pixels = connected.pixels, 
               ihc.connected.pixels = ihc.connected.pixels, 
               threshold.logical = threshold.logical, err.val = 0)
}