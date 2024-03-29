% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/arrange.R
\name{add_top}
\alias{add_top}
\title{Add top}
\usage{
add_top(
  g,
  top,
  heights = c(1, 9),
  fontfamily = "Comic Sans MS",
  fontsize = 16,
  hjust = NULL,
  vjust = NULL
)
}
\arguments{
\item{g}{a ggplot object}

\item{top}{a string specifying a top label}

\item{heights}{a unit vector giving the height of each row}

\item{fontfamily}{the font family}

\item{fontsize}{the font size}

\item{hjust}{A numeric vector specifying horizontal justification. If specified, overrides the just setting.}

\item{vjust}{A numeric vector specifying vertical justification. If specified, overrides the just setting.}
}
\value{
a gtable object
}
\description{
Add top labels
}
