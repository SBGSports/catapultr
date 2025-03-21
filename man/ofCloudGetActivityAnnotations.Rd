% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ofCloud.R
\name{ofCloudGetActivityAnnotations}
\alias{ofCloudGetActivityAnnotations}
\alias{ofCloudGetPeriodAnnotations}
\alias{ofCloudGetAthleteAnnotations}
\alias{ofCloudGetAnnotationCategories}
\title{get activity annotations}
\usage{
ofCloudGetActivityAnnotations(credentials, activity_id)

ofCloudGetPeriodAnnotations(credentials, period_id)

ofCloudGetAthleteAnnotations(credentials, athlete_id, annotation_id = NULL)

ofCloudGetAnnotationCategories(credentials)
}
\arguments{
\item{credentials}{as returned by \code{\link{ofCloudGetToken}}, \code{\link{safe_ofCloudGetToken}} or \code{\link{safe_ofCloudGetTokenEx}}}

\item{activity_id}{activity id as returned by \code{\link{ofCloudGetActivities}}}

\item{period_id}{period ID as returned by \code{\link{ofCloudGetPeriods}} or \code{\link{ofCloudGetActivities}}}

\item{athlete_id}{athlete id as returned by \code{\link{ofCloudGetAthletes}}, \code{\link{ofCloudGetAthletesInActivity}}, \code{\link{ofCloudGetAthletesInPeriod}}, \code{\link{ofCloudGetAthleteDevicesInActivity}} or \code{\link{ofCloudGetAthleteDevicesInPeriod}}}

\item{annotation_id}{as returned by \code{ofCloudGetAthleteAnnotations}}
}
\value{
\code{ofCloudGetActivityAnnotations} returns a data frame with activity annotations, including \emph{comment} and \emph{annotation category}.

\code{ofCloudGetPeriodAnnotations} returns a data frame with period annotations, including \emph{comment} and \emph{annotation category}.

\code{ofCloudGetAthleteAnnotations} returns a data frame with athlete annotations, including \emph{comment} and \emph{annotation category}. If \emph{annotation_id} is specified then the data frame returned contains details for the given annotation only.

\code{ofCloudGetAnnotationCategories} returns a data frame with annotation categories.
}
\description{
\code{ofCloudGetActivityAnnotations} returns a data frame with activity annotations.
}
\section{Functions}{
\itemize{
\item \code{ofCloudGetPeriodAnnotations()}: returns a data frame with period annotations.

\item \code{ofCloudGetAthleteAnnotations()}: returns a data frame with athlete annotations.

\item \code{ofCloudGetAnnotationCategories()}: returns a data frame with annotation categories.

}}
