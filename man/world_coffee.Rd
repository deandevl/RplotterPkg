% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{world_coffee}
\alias{world_coffee}
\title{world_coffee data}
\format{
An object of class \code{sf} (inherits from \code{data.table}, \code{data.frame}) with 34 rows and 3 columns.
}
\source{
\code{spData::coffee_data} and \code{spData::world}
}
\usage{
world_coffee
}
\description{
World coffee production by country (in thousands of 60 kg bags)
  is joined with a simple feature of country geometries via their
  common parameter 'name_long'.

\tabular{ll}{
  \strong{name_long} \tab character long name of country \cr
  \strong{coffee_production_2017} \tab numeric of coffee production of 60 kg bags \cr
  \strong{geom} \tab the sfc_MULTIPOLYGON country geometries
}
}
\keyword{datasets}
