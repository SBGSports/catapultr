#' Get CSV data file packaged with catapultR
#'
#' ofDataFileCSV() returns CSV 100 Hz data file name, packaged with catapultR package. 100 Hz data is produced in \emph{OpenField} console by raw files post-processing.
#'
#' @export
#' @return \code{ofDataFileCSV} returns an example CSV file name with 100 Hz data. 
#' @examples
#' sCSV <- ofDataFileCSV()
#' hiFreq <- read_CATcsv(sCSV)
#' df100Hz <- hiFreq$data
#'  
ofDataFileCSV <- function()
{
  system.file("extdata", "highfreq100hz.csv", package = "catapultR", mustWork = TRUE)
}

#' @describeIn ofDataFileCSV access 10 Hz sample CSV file originated from \code{\link{ofCloudGetActivitySensorData}} / \code{\link{ofCloudGetPeriodSensorData}}.\cr
#' Facilitates OpenField Cloud API vignette.
#' @export
#' @return \code{of10HzDataFileCSV} returns an example CSV file name with 10 Hz data. 
of10HzDataFileCSV <- function()
{
  system.file("extdata", "sd10Hz.csv", package = "catapultR", mustWork = TRUE)
}

#' @describeIn ofDataFileCSV access IMA events sample CSV file originated from \code{\link{ofCloudGetActivityEvents}} / \code{\link{ofCloudGetPeriodEvents}}.\cr
#' Facilitates OpenField Cloud API vignette.
#' @export
#' @return \code{ofIMAFileCSV} returns an example CSV file name with IMA events. 
ofIMAFileCSV <- function()
{
  system.file("extdata", "ima.csv", package = "catapultR", mustWork = TRUE)
}

#' @describeIn ofDataFileCSV access velocity efforts sample CSV file originated from \code{\link{ofCloudGetActivityEfforts}} / \code{\link{ofCloudGetPeriodEfforts}}.\cr
#' Facilitates OpenField Cloud API vignette.
#' @export
#' @return \code{ofVelEffortsFileCSV} returns an example CSV file name with velocity efforts. 
ofVelEffortsFileCSV <- function()
{
  system.file("extdata", "velEfforts.csv", package = "catapultR", mustWork = TRUE)
}