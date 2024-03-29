% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/density.R
\name{ggdensity}
\alias{ggdensity}
\title{Simple density plot}
\usage{
ggdensity(
  data,
  x,
  facet,
  kernel = "gaussian",
  probs = 0.95,
  logscale = F,
  round_digits = 0,
  scales = "fixed",
  family = "Comic Sans MS"
)
}
\arguments{
\item{data}{a data frame}

\item{x}{a name specifying numeric column.}

\item{facet}{a name of columns for applying \code{facet_wrap()} function.}

\item{kernel}{a character string giving the smoothing kernel to be used.
This must partially match one of "gaussian", "rectangular", "triangular",
"epanechnikov", "biweight", "cosine" or "optcosine", with default "gaussian",
and may be abbreviated to a unique prefix (single letter).
"cosine" is smoother than "optcosine", which is the usual ‘cosine’ kernel in
the literature and almost MSE-efficient. However, "cosine" is the version
used by S.}

\item{probs}{probability for a density function.}

\item{logscale}{a boolean value that determines whether or not to make the x
variable logscale.}

\item{round_digits}{rounding digits}

\item{scales}{Should scales be fixed ("fixed", the default), free ("free"),
or free in one dimension ("free_x", "free_y")?}

\item{family}{label font-family}
}
\value{
A ggplot object
}
\description{
Show a simple density plot.
}
\examples{
\donttest{set.seed(123)
x <- sample(1:10, size = 1000, replace = TRUE)
facet1 <- sample(c("A", "B", "C"), size = 1000, replace = TRUE)
facet2 <- sample(c("X", "Y", "Z"), size = 1000, replace = TRUE)
data <- data.frame(x, facet1, facet2)
ggdensity(data, x = x, facet = list(facet1, facet2), family = NA) +
 theme_view(family = NA)}

}
