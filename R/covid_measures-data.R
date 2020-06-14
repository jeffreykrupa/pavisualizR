#' @docType data
#' @name covid_measures
#'
#' @description Date and times for COVID-19 Response Measures going into place.
#'   Information collected colloquially from partners (needs to be updated using
#'   researched db) about the implementation of different types of COVID-19
#'   response measures in several global locations.
#'
#' @format A tibble with 13 rows and 5 variables: \describe{
#'   \item{site}{Operation Healthy Air Site.} \item{program}{Operation Healthy
#'   Air program where site is located}
#'   \item{response_measures}{Description/Name of COVID-19 REsponse Measure}
#'   \item{start_date}{POSIXCT in UTC when response measure went into place.}
#'   \item{end_date}{POSIXCT in UTC when response measure was ended.}}
#' @source Information collected colloquially from partners.
"covid_measures"
