#' convert IMA events into Focus format
#'
#' Converts specified IMA events into a Focus-friendly data frame ready for \code{jsonlite::write_json}.
#'
#' @export
#' @param dfEvents as returned by \code{\link{ofCloudGetActivityEvents}} or \code{\link{ofCloudGetPeriodEvents}}
#' @param event_name type of events in \code{dfEvents}, presumably from the set returned by \code{\link{ima_events}}
#' @param player_name name of the athlete associated with events
#' @param col color on the Focus timeline
#' @param digits the number of decimal places to round event attributes to
#' @return \code{eventsToFocus} returns a Focus-friendly data with the specified events. 
#'
#' @examples
#' \dontrun{
#'  dfActivities <- ofCloudGetActivities(token)
#'  dfEvents <- ofCloudGetActivityEvents(token, athlete_id, dfActivities$id[1], "ice_hockey_goaltender_movement")
#'  dfEvents <- dfEvents$ice_hockey_goaltender_movement
#'  dfEventsJSON <- eventsToFocus(dfEvents, "ice_hockey_goaltender_movement", "player1")
#'  jsonlite::write_json(dfEventsJSON, "iceHockeyFocus.JSON", pretty=TRUE)
#'  dfActivityJSON <- activityToFocus(dfActivities[1,])
#'  jsonlite::write_json(dfActivityJSON, "activity.JSON", pretty=TRUE)
#'  dfPeriods <- ofCloudGetPeriods(token, dfActivities$id[1])
#'  dfPeriodJSON <- periodToFocus(dfPeriods)
#'  jsonlite::write_json(dfPeriodJSON, "periods.JSON", pretty=TRUE)
#'  all.JSON <- dplyr::bind_rows(dfActivityJSON, dfPeriodJSON, dfEventsJSON) 
#'  all.JSON <- all.JSON[order(all.JSON$"tag-start"),]
#'  jsonlite::write_json(all.JSON, "all.JSON", pretty=TRUE)
#' }
eventsToFocus <- function(dfEvents, event_name, player_name, col = "blue", digits = 3)
{
	col_rgb <- col2rgb(col)
    tb <- tibble::tibble(
	        "tag-channel_id" = 1,
	        "tag-start" = as.numeric(dfEvents$start_time)*1000,	# POSIX time in ms
	        "tag-end" = as.numeric(dfEvents$end_time)*1000,		# POSIX time in ms
	        "tag-position" = round(((as.numeric(dfEvents$start_time) + as.numeric(dfEvents$end_time))/2)*1000),
	        "tag-colour_r" = col_rgb[1,] ,"tag-colour_g" = col_rgb[2,],"tag-colour_b" = col_rgb[3,],
	        "tag-name" = player_name,
	        "tag-short_name" = player_name,
	        "tag-extended_text" = ""
        )

	dfEvents <- dfEvents %>% dplyr::select(-dplyr::one_of(c("start_time", "end_time", "version"))) 

	taglabels <- lapply(1:NROW(dfEvents), 
						function(i){
									dfEventsLong <- tidyr::gather(round(dfEvents[i, ], digits), "label-group",  "label-text")
									dfEventsLong <- as.data.frame(rbind(c(label.group = "Type", label.text = event_name), dfEventsLong))
									colnames(dfEventsLong) <- c("label-group","label-text") # otherwise, it somehow returns "label.group","label.text"
									return(dfEventsLong)
								   })
    tb$"tag-labels" <- taglabels

	return(tb)
}

activityToFocusImp <- function(dfActivity, event_name, attribs, player_name, col)
{
	col_rgb <- col2rgb(col)
	startcs <- as.numeric(dfActivity$start_time)*100
	endcs <- as.numeric(dfActivity$end_time)*100
	if ("start_centiseconds" %in% colnames(dfActivity))
		startcs <- startcs + as.numeric(dfActivity$start_centiseconds)
	if ("end_centiseconds" %in% colnames(dfActivity))
		endcs <- endcs + as.numeric(dfActivity$end_centiseconds)
    tb <- tibble::tibble(
	        "tag-channel_id" = 1,
	        "tag-start" = startcs*10,	# POSIX time in ms
	        "tag-end" = endcs*10,		# POSIX time in ms
	        "tag-position" = round(((startcs + endcs)/2)*10),
	        "tag-colour_r" = col_rgb[1,] ,"tag-colour_g" = col_rgb[2,],"tag-colour_b" = col_rgb[3,],
	        "tag-name" = player_name,
	        "tag-short_name" = player_name,
	        "tag-extended_text" = ""
        )

	dfEvents <- dfActivity %>% dplyr::select(all_of(attribs))

	taglabels <- lapply(1:NROW(dfEvents), 
						function(i){
									dfEvent <- dfEvents[i, ]
									if ("tag_list" %in% attribs)
									{
										dfEvent$tag_list <- paste(dfEvent$tag_list[[1]], collapse = ", ")
									}
									dfEventsLong <- tidyr::gather(dfEvent, "label-group",  "label-text")
									dfEventsLong <- as.data.frame(rbind(c(label.group = "Type", label.text = event_name), dfEventsLong))
									colnames(dfEventsLong) <- c("label-group","label-text") # otherwise, it somehow returns "label.group","label.text"
									return(dfEventsLong)
								   })
    tb$"tag-labels" <- taglabels

	return(tb)
}

#' @describeIn eventsToFocus converts activity description into a Focus-friendly data frame ready for \code{jsonlite::write_json}.
#'
#' @export
#' @param dfActivity an activity as returned from \code{\link{ofCloudGetActivities}}
#' @return \code{activityToFocus} returns a Focus-friendly data with the specified activity. 
activityToFocus <- function(dfActivity, player_name = "", col = "blue")
{
	activityToFocusImp(dfActivity, "Activity", c("name", "venue_name"), player_name, col)
}

#' @describeIn eventsToFocus converts periods description into a Focus-friendly data frame ready for \code{jsonlite::write_json}.
#'
#' @export
#' @param dfPeriods periods as returned from \code{\link{ofCloudGetPeriods}}
#' @return \code{periodToFocus} returns a Focus-friendly data with the specified periods. 
periodToFocus <- function(dfPeriods, player_name = "", col = "blue")
{
	activityToFocusImp(dfPeriods, "Period", c("name", "tag_list"), player_name, col)
}
