# The functions in this file are moved from //Analytics/R/Utility/GenericCatapultRfunctions/GenericCatapultRfunctions.R

# library(readr)
# library(tibble)

#########################################
# READ CAT CSV RAW FILES
# Function to import Catapult in R as data_tibble
# filedatapath is the path of the csv file
# col_types - column specification, keeping the default value is recommended
read_CATcsv_message <- function(filedatapath, col_types = paste0(rep("d", 26), collapse=""))
{
    stopifnot(file.exists(filedatapath))
    
	lazy <- FALSE	# the default is TRUE; in Windows it locks the file, and we experienced issues with overwriting the file when the lock is not timely released
    # validate col_types
    data <- readr::read_csv(filedatapath, skip=3, n_max=1, col_names=TRUE, col_types = NULL, lazy = lazy)
    if (NCOL(data) != nchar(col_types))
    {
      col_types <- paste0(rep("d", NCOL(data)), collapse="")
      indic <- which(colnames(data)=="TimeStamp")
      if (length(indic) > 0)
      {
        indic <- indic[1]
        col_types <- stringr::str_c(stringr::str_sub(col_types, 1, indic - 1), 
                                    "c", 
                                    stringr::str_sub(col_types, indic + 1, nchar(col_types)))
      }
    }
      
    data <- readr::read_csv(filedatapath, skip=3, col_types = col_types, lazy = lazy)
    RefTime <- readr::read_csv(filedatapath, n_max=1, col_names=FALSE, lazy = lazy)$X1
    DatesOnly1 <- substr(RefTime,17,24)
    DatesOnly <- as.Date(DatesOnly1, "%m/%d/%y")
    TimeOnly <- substr(RefTime,26,33)
    CentisecTime <- readr::read_csv(filedatapath, skip = 1, n_max=1, col_names=FALSE, lazy = lazy)$X1
    CentisecTime <- as.numeric(stringr::word(CentisecTime, 2, sep=":"))
    DeviceID1 <- readr::read_csv(filedatapath, skip=2,n_max=1, col_names=FALSE, lazy = lazy)
    DeviceID1 <- stringr::str_c(DeviceID1, collapse=",")    # tibble to string
    DeviceID <- stringr::str_trim(stringr::word(stringr::word(DeviceID1, 1, sep=","), 2, sep=":"))
    DeviceFamily <- stringr::str_trim(stringr::word(stringr::word(DeviceID1, 2, sep=","), 2, sep=":"))
    OFVersion <- stringr::str_trim(stringr::word(stringr::word(DeviceID1, 3, sep=","), 2, sep=":"))
    FirmwareVersion <- stringr::str_trim(stringr::word(stringr::word(DeviceID1, 4, sep=","), 2, sep=":"))

    out <- list("data" = data,
        "RefTime" = RefTime,
        "DatesOnly" = DatesOnly,
        "TimeOnly" = TimeOnly,
        "DeviceID" = DeviceID, 
        "DeviceFamily" = DeviceFamily,
        "OFVersion" = OFVersion,
        "FirmwareVersion" = FirmwareVersion,
        "CentisecTime" = CentisecTime   # number of centiseconds since 01.01.1970 UTC, convertible into POSIXct as
        )                               # as.POSIXct(CentisecTime/100, origin="1970-01-01", tz="UTC"); print(as.POSIXct(...)) should be identical to RefTime
    out
}
  
#' Read high frequency CSV
#'
#' Loads high frequency parameters for analytics CSV as a data_tibble.\cr
#' \code{read_CATcsv} supplements \code{\link{write_CATcsv}}.
#'
#' @export
#' @param filedatapath source CSV file.
#' @param col_types column specification for \code{readr::read_csv}, keeping the default value is recommended.
#' @param suppressMsgs \code{TRUE} to suppress messages from \code{readr::read_csv}.
#' @return a list with metadata and \code{data} tibble.
#' @examples
#' x <- read_CATcsv(ofDataFileCSV())
#' xData <- x$data
#' posixTimeStamp <- x$CentisecTime / 100   # time in seconds since the start of the epoch
#' @family read and write Catapult 100 Hz CSV files
read_CATcsv <- function(filedatapath, col_types = paste0(rep("d", 26), collapse=""), suppressMsgs = TRUE) {
	if(suppressMsgs){
		out <- suppressMessages(read_CATcsv_message(filedatapath,col_types))
	}else{
		out <- read_CATcsv_message(filedatapath,col_types)
	}
	return(out)
}

#' Write high frequency CSV, preserving the metadata
#'
#' Writes high frequency data into a CSV file, preserving the metadata.\cr
#' Use this function to save 100 Hz data including the metadata when the data was updated.\cr
#' \code{write_CATcsv} supplements \code{\link{read_CATcsv}}.
#'
#' @export
#' @param x a list with metadata and the 100 Hz data, as returned by \code{\link{read_CATcsv}}.
#' @param targetCSV target CSV file.
#' @return \code{x}, invisibly.
#' @family read and write Catapult 100 Hz CSV files
write_CATcsv <- function(x, targetCSV)
{
  line1 <- x$RefTime
  line2 <- stringr::str_c("CentisecTime: ", x$CentisecTime)
  line3 <- stringr::str_c("DeviceID: ", x$DeviceID, ", DeviceFamily: ", x$DeviceFamily, ", OFVersion: ", x$OFVersion, ", FirmwareVersion: ", x$FirmwareVersion)
  readr::write_lines(line1, targetCSV)
  readr::write_lines(line2, targetCSV, append = TRUE)
  readr::write_lines(line3, targetCSV, append = TRUE)
  readr::write_csv(x$data, targetCSV, append = TRUE, col_names = TRUE)
  invisible(x)
}
