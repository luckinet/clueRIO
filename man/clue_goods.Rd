% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clue_goods.R
\name{clue_goods}
\alias{clue_goods}
\title{Set goods (or services) and their attributes}
\usage{
clue_goods(scene, name, excluded = NULL, match = "exact", demand = NULL)
}
\arguments{
\item{scene}{\code{\link[=scene]{scene(1)}}\cr the CLUE scene to which to add a good.}

\item{name}{\code{\link[=character]{character(1)}}\cr name of the good.}

\item{excluded}{\code{\link[=character]{character(.)}}\cr land types from which the
good is excluded to be produced.}

\item{match}{\code{\link[=character]{character(1)}}\cr the type of match for this
good, either \code{"exact"} (the default), \code{"minimum"} (the
demand indicated is the minimum demand that needs to be achieved, any
overshoot is accepted) or \code{"maximum"} (the demand indicated is the
maximum that may be reached, any undershoot is accepted).}

\item{demand}{\code{\link{data.frame}}\cr table of when, where and how much
of the good is produced. Must have names \code{year}, \code{region}
and \code{amount}.}
}
\description{
Set goods (or services) and their attributes
}
