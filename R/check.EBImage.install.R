#################################check.EBImage.install##########################

#' check that EBImage installed correctly
#'Created By: Benjamin Green
#'Last Edited 09/24/2019
#'
#'Description
#'This function was designed to check for the EBImage installation, and install 
#' any updates necessary. The package does not seem to able to be installed with
#' these depencies I think since EBImage is a Bioc package and not a CRAN 
#' package. EBImage is only used when removing objects less than the 
#' 'connected.pixel' values, spefically for the C level bwlabel function it
#' makes use of.
#'
#' @param connected.pixels con pixels value
#' @return exports multiple variables for use in the main titration codes
#' @export
#'
check.EBImage.install <- function(connected.pixels) {
  #
  # check if the EBImage package is installed or not
  #
  a<-installed.packages()
  packages<-a[,1]
  #
  if (!is.element("EBImage", packages)){
    tryCatch({
      BiocManager::install("EBImage", ask=FALSE)
    }, warning = function(cond) {
      #
      #  on first warning check if BiocManager is installed\ needs updating, then 
      # try to install EBImage 
      #
      tryCatch({
        install.packages(
          'BiocManager', ask = FALSE, quiet = TRUE, verbose = FALSE)
        BiocManager::install("EBImage", ask=FALSE)
        #
      }, warning = function(cond) {
        #
        # on second warn or error exit with no connected.pixel value
        # 
        modal_out <- shinyalert::shinyalert(
          title = "Error installing EBImage from BiocManager.",
          text = paste0(
            "Please attempt to update\ install BiocManager separately using: ",
            "install.packages('BiocManager'); then attempt to update\ install ",
            "EBImage from the Bioc repo using: BiocManager::install('EBImage')."
          ),
          type = 'error',
          showConfirmButton = TRUE
        )
        connected.pixels <- 'NA'
      }, error = function(cond) {
        #
        # on second warn or error exit with no connected.pixel value
        # 
        modal_out <- shinyalert::shinyalert(
          title = "Error installing EBImage from BiocManager.",
          text = paste0(
            "Please attempt to update\ install BiocManager separately using: ",
            "install.packages('BiocManager'); then attempt to update\ install ",
            "EBImage from the Bioc repo using: BiocManager::install('EBImage')."
          ),
          type = 'error',
          showConfirmButton = TRUE
        )
        connected.pixels <- 'NA'
      })
    }, error = function(cond) {
      #
      # on first error check if BiocManager is installed\ needs updating, then 
      # try to install EBImage 
      #
      tryCatch({
        #
        install.packages(
          'BiocManager', ask = FALSE, quiet = TRUE, verbose = FALSE)
        BiocManager::install("EBImage", ask=FALSE)
        #
      }, warning = function(cond) {
        #
        # on second warn or error exit with no connected.pixel value
        # 
        modal_out <- shinyalert::shinyalert(
          title = "Error installing EBImage from BiocManager.",
          text = paste0(
            "Please attempt to update\ install BiocManager separately using: ",
            "install.packages('BiocManager'); then attempt to update\ install ",
            "EBImage from the Bioc repo using: BiocManager::install('EBImage')."
          ),
          type = 'error',
          showConfirmButton = TRUE
        )
        connected.pixels <- 'NA'
      }, error = function(cond) {
        #
        # on second warn or error exit with no connected.pixel value
        # 
        modal_out <- shinyalert::shinyalert(
          title = "Error installing EBImage from BiocManager.",
          text = paste0(
            "Please attempt to update\ install BiocManager separately using: ",
            "install.packages('BiocManager'); then attempt to update\ install ",
            "EBImage from the Bioc repo using: BiocManager::install('EBImage')."
          ),
          type = 'error',
          showConfirmButton = TRUE
        )
        connected.pixels <- 'NA'
      })
    })
  }
  #
  if (length(connected.pixels) == 1){
    if (grepl('NA',connected.pixels)){
      err.val <- 4
      return(list(err.val = err.val))
    }
  }
    err.val <- 0
    return(list(err.val = err.val, connected.pixels = connected.pixels))
}