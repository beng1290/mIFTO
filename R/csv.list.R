###############################csv.list###########################

#'Used by analysis functions to read cell seg data text files
#'
#'csv.list;
#'Created By: Benjamin Green;
#'Last Edited 11/12/2018
#'
#'This script is designed to read in the
#'cell seg text file exported from inForm (R) CellAnaylsis
#'The function returns a data.frame of n columns, each column in the
#'data.frame designates a different layer of the image designated
#'here as DAPI; Flours; AF; however the column/names order may change
#'depending on the library used to export images from inForm(R)
#'
#' @param wd is the working directory
#' @param pattern.in is the pattern used by R to determine which image or 
#' images to read in
#' @param compartment the compartment of interest to be extracted from the tables
#' will always extract mean values 
#' @param phenotype.logical for cell analysis whether the data was phenotyped 
#' or not
#' @param measurement the measurement to extract default is mean
#' @return is a list of two; data.out a column data.frame where each column
#' holds the pixel intensities for a single image layer, labeled by the inForm
#' image layer names, and err.val a value indicating an error (1) or not (0)
#' @export
#'
csv.list <- function(wd, pattern.in, compartment, 
                     phenotype.logical, measurement = 'Mean') {
  #
  # get all images with similar image names
  #
  err.val <- 0
  image_name <- list.files(
    wd,
    pattern = paste0(pattern.in, '_cell_seg_data.txt'),
    full.names = T,
    ignore.case = T,
    recursive = F,
    include.dirs = F
  )
  if (length(image_name) != 1){
    err.val <- 1
    return(list(err.val = err.val))
  }
  #
  tbl <- data.table::fread(image_name, na.strings=c('NA', '#N/A'),
                           data.table= FALSE )
  #
  pattern.match=paste0(compartment, '.*',measurement,'.*')
  #
  cols <- colnames(tbl)
  cols2 <- cols[grepl(pattern.match, cols, ignore.case = T)]
  result.match <- regmatches(
    cols2, regexec(
      paste0(compartment,'\\s*(.*?)\\s*',measurement),
      cols2
    )
  )
  cols2 <- sapply(result.match, '[[', 2)
  #
  if (phenotype.logical){
    pattern.match=paste0('cell id|phenotype|', pattern.match)
    cols2 <- c('cellid','phenotype', cols2)
  } else {
    pattern.match=paste0('cell id|', pattern.match)
    cols2 <- c('cellid', cols2)
  }
  #
  tbl2 <- tbl[,grepl(pattern.match, cols, ignore.case = T)]
  colnames(tbl2) <- cols2
  #
  return(list(data.out = tbl2, err.val = err.val))
  #
}
