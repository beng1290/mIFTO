#################################check.gen.input.vars#####################################

#' check the general input tab input values
#'Created By: Benjamin Green
#'Last Edited 09/24/2019
#'
#'Description
#'This function was designed to check input variables from the general input
#' tab for any errors that have occured in input
#'
#'
#' @param out is the list of variables given by the GUI function
#' @return exports multiple variables for use in the main titration codes
#' @export
#'
check.gen.input.vars <- function(out) {
  #
  # check the slide IDs
  #
  if (out$Slide_ID == ""){
    modal_out <- shinyalert::shinyalert(
      title = "Slide description input is empty.",
      text = paste(
        "Please enter valid slide desciptor input."),
      type = 'error',
      showConfirmButton = TRUE
    )
    err.val <- 1
    return(list(err.val = err.val))
  }
  #
  if( grepl('[-|+|&]',out$Slide_ID,perl = TRUE ) ) {
    n <- shiny::showNotification(
      'Slide Descriptors contain an illegal character this may cause issues',
      type = 'warning')
  }
  if( grepl(' ',out$Slide_ID,perl = TRUE ) ) {
    n <- shiny::showNotification(
      'Slide Descriptors contain spaces ... removing spaces in names',
      type = 'warning')
    Slide_ID <- gsub(" ", "",out$Slide_ID, fixed = TRUE)
  } else {
    Slide_ID <- out$Slide_ID
  }
  #
  Slide_ID <- unlist(strsplit(Slide_ID, split = ','))
  #
  # get the antibody name
  #
  Antibody <- out$Antibody
  #
  if (Antibody == ""){
    modal_out <- shinyalert::shinyalert(
      title = "Antibody input is empty.",
      text = paste(
        "Please enter a value for the antibody input."),
      type = 'error',
      showConfirmButton = TRUE
    )
    err.val <- 1
    return(list(err.val = err.val))
  }
  #
  # get the concentration values
  #
  if (out$Concentration == ""){
    modal_out <- shinyalert::shinyalert(
      title = "Concentration input is empty.",
      text = paste(
        "Please enter valid concentration input."),
      type = 'error',
      showConfirmButton = TRUE
    )
    err.val <- 1
    return(list(err.val = err.val))
  }
  #
  if( grepl(' ',out$Concentration,perl = TRUE ) ) {
    n <- shiny::showNotification(
      'Concentrations contain spaces ... removing spaces in concentrations',
      type = 'warning')
    Concentration <- gsub(" ", "",out$Concentration, fixed = TRUE)
  } else {
    Concentration <- out$Concentration
  }
  #
  Concentration1 <- tryCatch({
    Concentration1 <- as.numeric(
      unlist(
        strsplit(
          Concentration, split =','
        )
      )
    )
  }, warning = function(cond) {
    modal_out <- shinyalert::shinyalert(
      title = "Error in concentration input.",
      text = paste(
        "Concentration input:", Concentration, "not valid. Please enter a list",
        "of numeric values separated by commas."),
      type = 'warning',
      showConfirmButton = TRUE
    )
    return(-1)
  }, error = function(cond) {
    modal_out <- shinyalert::shinyalert(
      title = "Error in concentration input.",
      text = paste(
        "Concentration input:", Concentration, "not valid. Please enter a list",
        "of numeric values separated by commas."),
      type = 'warning',
      showConfirmButton = TRUE
    )
    return(-1)
  }
  )
  #
  if (length(Concentration1) == 1){
    if (Concentration1 == -1){
      err.val = 1
      return(list(err.val = err.val))
    }
  }
  #
  Concentration <- Concentration1
  if (is.unsorted(Concentration) || !min(Concentration) > 0){
    err.val = 1
    return(list(err.val = err.val))
  }
  #
  # the opal name
  #
  Opal1 <- out$Opal1
  #
  if (Opal1 == ""){
    modal_out <- shinyalert::shinyalert(
      title = "Fluorophore (TSA) input is empty.",
      text = paste(
        "Please enter a value for the Fluorophore (TSA) input."),
      type = 'error',
      showConfirmButton = TRUE
    )
    err.val <- 1
    return(list(err.val = err.val))
  }
  #
  # an antibody opal name pair
  #
  Antibody_Opal <- paste0(Antibody, ' (', Opal1, ')', fixed = TRUE)
  #
  # put the names together to find the proper dilutions
  #
  Naming.convention<-out$Naming.convention
  titration.type<-out$titration.type
  #
  if(Naming.convention){
    if(titration.type=='Primary Antibody'){
      titration.type.name<-Antibody
    }else if (titration.type =='Fluorophore (TSA)'){
      titration.type.name<-Opal1}
  }else{
    titration.type.name<-''
  }
  #
  gen.out <- list(Slide_ID = Slide_ID, Concentration = Concentration,
                  Antibody = Antibody, Opal1 = Opal1, 
                  Antibody_Opal = Antibody_Opal,
                  titration.type.name = titration.type.name,
                  Protocol = out$protocol.type, err.val = 0)
  
}