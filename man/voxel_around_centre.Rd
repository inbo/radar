% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/voxel_around_centre.R
\name{voxel_around_centre}
\alias{voxel_around_centre}
\title{Calculate the number of tracks through a voxel}
\usage{
voxel_around_centre(
  local,
  centre,
  max_distance = 200,
  voxel_size = rep(max_distance/5, 3),
  voxel_box = matrix(c(-10, -10, 0, 10, 10, 20), nrow = 3)
)
}
\arguments{
\item{local}{the connection to the local database}

\item{centre}{a data frame with a single row and x and y coordinates of the
centre.
Coordinates in CRS 31370}

\item{max_distance}{Use all equal time tracks where the minimum distance in
meters between the track and the centre is smaller than \code{max_distance}.}

\item{voxel_size}{Size in meters of the voxel.}

\item{voxel_box}{Defines the area around the centre for to set of voxels to
use.
Defined using the index number of the voxels.
Defaults to 10 voxels on both side of the centre in x and y and 20 voxels
above 0 in the z direction.}
}
\description{
A voxel is the 3D equivalent of a pixel.
Counts the number of unique tracks going through a voxel.
The function rotates the tracks according to the average wind direction of
the track.
The x direction is parallel to the wind direction.
}
