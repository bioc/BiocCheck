% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Context.R
\name{Context}
\alias{Context}
\title{Report context of events to user with a data.frame of events and locations}
\usage{
Context(file = "", lines = character(), idx = logical(), offset = 0L)
}
\arguments{
\item{file}{character(1) full path (including package name) of file being
summarized.}

\item{lines}{character() vector of text lines in file}

\item{idx}{logical() same length as \code{lines} indicating lines in which
event occurs}

\item{offset}{\code{integer(1)} The number of lines to add to the 'Line' column
calculation. It is mainly used to account for the number of lines that the
YAML header occupies.}
}
\value{
Context: a data.frame() with columns File, Line, and Context
}
\description{
Report context of events to user with a data.frame of events and locations
}
