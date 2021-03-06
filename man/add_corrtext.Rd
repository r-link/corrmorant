% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_corrtext.R
\name{add_corrtext}
\alias{add_corrtext}
\alias{lotri_corrtext}
\alias{utri_corrtext}
\title{Text labels for correlations in off-diagonal ggcorrm facets}
\usage{
lotri_corrtext(
  mapping = NULL,
  nrow = NULL,
  ncol = NULL,
  digits = 2,
  corr_size = TRUE,
  corr_method = NULL,
  squeeze = 0.5,
  byrow = TRUE,
  show.legend = FALSE,
  ...
)

utri_corrtext(
  mapping = NULL,
  nrow = NULL,
  ncol = NULL,
  digits = 2,
  corr_size = TRUE,
  corr_method = NULL,
  squeeze = 0.5,
  byrow = TRUE,
  show.legend = FALSE,
  ...
)
}
\arguments{
\item{mapping}{Set of aesthetic mappings created by \link[ggplot2:aes]{aes}. \code{x}
and \code{y} are set automatically and must not be changed,  but all other
aesthetics may be manipulated. Defaults to \code{NULL} (use standard \code{ggcorrm}
mapping).}

\item{nrow}{integer - the number of rows for the correlation labels if
grouping aesthetics are present (defaults to \code{NULL} - automatic setting of
\code{nrow}).}

\item{ncol}{integer - the number of columns for the correlation labels if
grouping aesthetics are present (defaults to \code{NULL} - automatic setting of
\code{ncol}).}

\item{digits}{integer: Number of digits the correlations are rounded to
(defaults to 2).}

\item{corr_size}{logical - should the \code{size} aesthetic be expressed as a
function of correlation strength? \code{corr_size = TRUE} is a shorthand for
setting \code{aes(size = abs(..corr..))}. Similar expressions can be used to
access the correlation calculated by \code{stat_corrtext} manually in \code{aes()}.
Defaults to \code{TRUE}.}

\item{corr_method}{character string with the correlation method passed to
\link[stats:cor]{cor()}. Can be one of \code{"pearson"}, \code{"kendall"} and
\code{"spearman"}. Defaults to \code{"pearson"} (or is inherited from the setting in
\code{\link[=ggcorrm]{ggcorrm()}}).}

\item{squeeze}{numeric between 0 an 1. Proportion of the facet width/height
the facet labels are restricted to when multiple labels are present
(defaults to 0.7 - labels extend over 70\% of the extent of the plot).}

\item{byrow}{logical. Should the correlation labels in plots with multiple
groups be filled by rows (\code{byrow = TRUE}) or by columns (\code{byrow = FALSE})?
Note that the actual number of rows or columns that are filled with values
can be below the specified value of \code{nrow} or \code{ncol} when less rows/columns
than specified are needed to reach the total number of groups. Defaults to
\code{TRUE}.}

\item{show.legend}{logical. Should this layer be included in the legends?
FALSE (the default) never includes, TRUE always includes, and NA includes
only if aesthetics are mapped. It can also be a named logical vector
to finely select the aesthetics to display.}

\item{...}{Additional arguments to \link{stat_corrtext}.}
}
\value{
A \code{ggplot2} layer with text labels for correlation strength.
}
\description{
\code{lotri_corrtext()} and \code{utri_corrtext()} are used to display
text labels for the strength of bivariate correlations in the off-diagonal
facets of \link{ggcorrm} plots.
}
\details{
\code{lotri_corrtext()} and \code{utri_corrtext()} can be used to display the
correlation  between variables in the lower or upper triangular facets of
\code{ggcorrm} plots, respectively. Correlations can be calculated for single
groups by using grouping aesthetics such as \code{aes(color = group)}.

\code{lotri_corrtext()} and \code{utri_corrtext()} are wrappers around
\code{\link[=stat_corrtext]{stat_corrtext()}} that additionally take  care of the right specification
of aesthetics and allows to easily adjust size by correlation strength via
\code{corr_size}.
}
\seealso{
\link{stat_corrtext}
}
