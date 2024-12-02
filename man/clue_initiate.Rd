% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clue_initiate.R
\name{clue_initiate}
\alias{clue_initiate}
\title{Prepare the CLUE model directory}
\usage{
clue_initiate(scene, clue.exe, modify = NULL)
}
\arguments{
\item{scene}{\code{\link[=scene]{scene(1)}}\cr the CLUE scene for which to prepare the
model directory.}

\item{clue.exe}{\code{\link[=character]{character(1)}}\cr R packages cannot contain
executable files, such as the CLUMondo main model executable. Therefore,
you need to download this
\href{https://www.environmentalgeography.nl/site/data-models/models/clumondo-model/}{CLUMondo*.exe}
from the official model website and provide the path to your local copy
here.}

\item{modify}{description}
}
\value{
no return value, called for the side-effect of creating directories
and initial files.
}
\description{
This creates directories and initial files from a CLUE scene.
}
\details{
Additional details...
}
