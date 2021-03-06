% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/covid_measuresPlot.R
\name{covid_measuresPlot}
\alias{covid_measuresPlot}
\title{Visualize changes in PM2.5 reading distributions for each hour the
day.}
\usage{
covid_measuresPlot(sensor, facet_workweek = FALSE)
}
\arguments{
\item{sensor}{AirSensor object.}

\item{facet_workweek}{Split plot into two facets, one for workweek days and
one for weekend days.}
}
\value{
A ggplot object.
}
\description{
Create boxplots without whiskers for before and after COVID
response measures go into place.
}
