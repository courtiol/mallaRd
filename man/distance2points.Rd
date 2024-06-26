% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{distance2points}
\alias{distance2points}
\alias{distance2points_vec}
\title{Functions to compute the distance between geographic locations}
\usage{
distance2points(lat1, long1, lat2, long2)

distance2points_vec(lat1, long1, lat2, long2)
}
\arguments{
\item{lat1}{the latitude of point 1}

\item{long1}{the longitude of point 1}

\item{lat2}{the latitude of point 2}

\item{long2}{the longitude of point 2}
}
\value{
\code{distance2points()} returns an object of class \code{units} providing the distance between geographic locations

\code{distance2points_vec()} returns a vector of objects of class \code{units} providing the distance between geographic locations
}
\description{
Functions to compute the distance between geographic locations
}
\section{Functions}{
\itemize{
\item \code{distance2points()}: simple version to compute distances between two geographic locations

\item \code{distance2points_vec()}: vectorised version to compute distances between two sets of coordinates

}}
\examples{
# Compute the distance between Paris and Berlin
distance2points(lat1 = 48.856667, long1 = 2.352222, lat2 = 52.52, long2 = 13.405)

# Compute the distance between Paris and Berlin, and between Berlin and Paris
distance2points_vec(lat1 = c(48.856667, 52.52),
                    long1 = c(2.352222, 13.405),
                    lat2 = c(52.52, 48.856667),
                    long2 = c(13.405, 2.352222))
}
