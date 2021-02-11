###############################tiff.list###########################

#'Used by analysis functions to read component_tiff images
#'
#'tiff.list;
#'Created By: Benjamin Green;
#'Last Edited 11/12/2018
#'
#'This script is designed to read in the
#'component_tiff image exported from inForm (R) CellAnaylsis
#'The function returns a data.frame of n columns, each column in the
#'data.frame designates a different layer of the image designated
#'here as DAPI; Flours; AF; however the column/names order may change
#'depending on the library used to export images from inForm(R)
#'
#' @param wd is the working directory
#' @param pattern.in is the pattern used by R to determine which image or images to read in
#' @return is a list of two; tiff.list is a column data.frame where each column
#' holds the pixel intensities for a single image layer, labeled by the inForm
#' image layer names, and err.val a value indicating an error (1) or not (0)
#' @export
#'
tiff.list <- function(wd, pattern.in) {
  #
  # get all images with similar image names
  #
  err.val <- 0
  image_name <- list.files(
    wd,
    pattern = paste0(pattern.in, '_component_data.tif'),
    full.names = T,
    ignore.case = T,
    recursive = F,
    include.dirs = F
  )
  if (length(image_name) != 1){
    err.val <- 1
    return(list(err.val = err.val))
  }
  pattern.match="\\<Name\\>(.*?)\\<Name\\>"
  #
  # get the names of the layers for the protocol
  #
  a <- ijtiff::read_tags(image_name,'all' )
  results.match <- matrix(length(a), 1)
  #
  for (i.1 in 1:length(a)){
    result.match.1 <- regmatches(
      a[[i.1]]$description, regexec(pattern.match,a[[i.1]]$description)
    )
    result.match.1 <- result.match.1[[1]][2]
    result.match.1 <- substring(
      result.match.1, 2, (nchar(result.match.1[[1]])-2)
    )
    results.match[[i.1]] <- result.match.1
  }
  #
  types <- results.match[1:length(results.match)-1]
  #
  m2 <- list()
  #
  # read each image in separately
  #
  for (count2 in 1:length(image_name)) {
    v <- tiff::readTIFF(image_name[count2],native = F,all = T,as.is = F)
    if (!(length(v)-1) == length(types)){
      return(err.val = 15)
    }
    v <- v[1:length(types)]
    names(v) <- types
    m2 <- c(m2, list(v))
  }
  return(list(data.out = m2, err.val = err.val))
}
