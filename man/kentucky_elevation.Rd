% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{kentucky_elevation}
\alias{kentucky_elevation}
\title{kentucky_elevation data}
\format{
Data frame with columns:
\describe{
  \item{x}{numeric longitude of the elevation}
  \item{y}{numeric latitude of the elevation}
  \item{elevation}{the elevation in meters}
}
}
\source{
\code{elevatr::get_elev_raster()} and the
Amazon Web Services Terrain Tiles service.
}
\usage{
kentucky_elevation
}
\description{
Tiled raster elevation data(in meters) converted to a data frame,
covering the southeast region of Kentucky.
}
\keyword{datasets}
