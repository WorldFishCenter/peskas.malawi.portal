% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{update_validation_status}
\alias{update_validation_status}
\title{Update validation status}
\usage{
update_validation_status(
  submission_id = NULL,
  validation_status = NULL,
  asset_id = NULL,
  token = NULL,
  debug = TRUE
)
}
\arguments{
\item{submission_id}{Character. ID of the submission to update}

\item{validation_status}{Character. New validation status to set}

\item{asset_id}{Character. KoboToolbox asset ID}

\item{token}{Character. Authentication token}

\item{debug}{Logical. Whether to print debug information}
}
\value{
Character. Status message indicating success or failure
}
\description{
Updates the validation status of a KoboToolbox submission
}
