% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/duplicate_deleter.R
\name{nearest_gridpoint}
\alias{nearest_gridpoint}
\title{Nearest point in a grid}
\usage{
nearest_gridpoint(point, grid_list)
}
\arguments{
\item{point}{the point to map}

\item{grid_list}{the grid to map to}
}
\description{
Finds the nearest point in a grid
This is a little tricky- it expand.grid the grid list and provides index there
}
