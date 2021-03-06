% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_dia_funs.R
\name{dia_names}
\alias{dia_names}
\title{Add variable names to diagonal facets}
\usage{
dia_names(y_pos = 0.15, mapping = NULL, ..., inherit.aes = FALSE)
}
\arguments{
\item{y_pos}{numeric between 0 and 1 specifying the relative position of the
text labels along the x axis (defaults to 0.2).}

\item{mapping}{Set of aesthetic mappings created by \link[ggplot2:aes]{aes}. \code{x}
and \code{y} are set automatically and must not be changed,  but all other
aesthetics may be manipulated. Defaults to \code{NULL} (use standard \code{ggcorrm}
mapping).}

\item{...}{Additional parameters passed to \code{\link[=stat_dia_names]{stat_dia_names()}}.}

\item{inherit.aes}{If \code{FALSE}, overrides the default aesthetics,
rather than combining with them. This is most useful for helper functions
that define both data and aesthetics and shouldn't inherit behaviour from
the default plot specification, e.g. \code{\link[ggplot2:borders]{borders()}}.}
}
\value{
A \code{ggplot2} layer containing text labels on the diagonal facets.
}
\description{
\code{dia_names()} is a wrapper around \code{\link[=stat_dia_names]{stat_dia_names()}} which plots
the names of variables in a \code{ggcorrm} plot at appropriate positions in the
diagonal facets.
}
\details{
\code{dia_names()} plots text labels on the plot diagonal and takes care
of the often complicated positioning in plots with different x and y
ranges.

On default, \code{inherit.aes} is set to \code{FALSE} as in most cases this is the
preferred behavior for the labels for variable names.
}
\seealso{
\code{\link[=stat_dia_names]{stat_dia_names()}}
}
