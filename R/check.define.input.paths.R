#################################check.define.input.paths#######################

#' check and define the input paths for the data
#'Created By: Benjamin Green
#'Last Edited 02/09/2021
#'
#'Description
#'This function was designed to check input paths to ensure that they exist and 
#' that one path exists for each concentration-slide pair (no dup or miss-named
#' directories are mistaken for a work dir)
#'
#'
#' @param out.chckbx the values from the analysis check boxes in logical format
#' @param out.gen the values from the general input tab
#' @return exports the working directory and the folders + check for common errors
#' @export
#'
check.define.input.paths <- function(out.chkbx, out.gen, wd ) {
  #
  # get the working directory
  #
  if(is.na(wd)) {
    modal_out <- shinyalert::shinyalert(
      title = "Directory not valid.",
      text = paste(
        "User selected cancel. Please select a valid directory."
      ),
      type = 'error',
      showConfirmButton = TRUE
    )
    err.val <- 2
    return(list(err.val = err.val))
  }
  #
  # get the IF paths
  #
  if(out.chkbx$m.folders) {
    #
    pp <- list.dirs(wd, recursive = F)
    #
    paths<-sapply(1:length(out.gen$Concentration),function(x){
      str = paste0(
        out.gen$titration.type.name,'_1to',out.gen$Concentration[x],'$|',
        out.gen$titration.type.name,'_1to',out.gen$Concentration[x],'[^0]')
      pp[grepl(str,pp)]
    })
    #
  }else{
    paths<-sapply(1:length(out.gen$Concentration),function(x) wd)
  }
  #
  # check that there is one path for each concentration
  # (if folders is false vector paths will be filled with one
  # path for each concentration)
  #
  for (x in 1:length(paths)){
    if (length(paths[[x]]) != 1){
      modal_out <- shinyalert::shinyalert(
        title = "Error could not find paths.",
        text = paste(
          "The number of paths for each concentration does not equal 1.",
          "Please check the status of the naming convention on folders and that",
          "all folders exist."
        ),
        type = 'error',
        showConfirmButton = TRUE
      )
      err.val <- 2
      return(list(err.val = err.val))
    }
  }
  #
  # whether or not an ihc was done and images are present
  #
  if(out.chkbx$ihc.logical) {
    #
    # at least one imageID exists for each slide id
    #
    for(x in out.gen$Slide_ID){
      #
      # regular expression to grab this slide descript IHC
      #
      if (grepl(out.chkbx$a.type, 'pixels')){
        str =  paste0('.*', x, '.*IHC.*_component_data.tif')
      } else if (grepl(out.chkbx$a.type, 'cells')){
        str =  paste0('.*', x, '.*IHC.*_cell_seg_data.txt')
      } else if (grepl(out.chkbx$a.type, 'tissue')){
        str =  paste0('.*', x, '.*IHC.*_binary_seg_maps.tif&',
                      '.*', x, '.*IHC.*_component_data.tif')
      } 
      #
      # IHC files
      #
      if(out.chkbx$m.folders) {
        cImage.IDs <-  list.files(
          c(paste0(wd, '/IHC'), paste0(wd, '/',out.gen$Antibody,'_IHC')),
          pattern = str, ignore.case = T, include.dirs = F)
      } else {
        cImage.IDs <-  list.files(
          wd, pattern = str, ignore.case = T)
      }
      #
      # check that files exist for each slide
      #
      if(length(cImage.IDs) == 0 ){
        modal_out <- shinyalert::shinyalert(
          title =  paste('Search failed for', x, out.gen$titration.type.name,
                         'IHC images'),
          text = paste0(
            'Please check slide names and that correct data for ',
            x, ' IHC exist. For data separated in folders by dilution, put IHC ',
            'data in an "IHC" or "', out.gen$Antibody, '_IHC" folder'),
          type = 'error',
          showConfirmButton = TRUE
        )
        err.val <- 2
        return(list(err.val = err.val))
      }
    }
    #
  }
  #
  out.paths <- list(wd = wd, paths = paths, err.val = 0)
  #
}
