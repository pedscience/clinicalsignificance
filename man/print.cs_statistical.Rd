% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cs_statistical.R
\name{print.cs_statistical}
\alias{print.cs_statistical}
\title{Print Method for the Statistical Approach}
\usage{
\method{print}{cs_statistical}(x, ...)
}
\arguments{
\item{x}{An object of class \code{cs_distribution}}

\item{...}{Additional arguments}
}
\value{
No return value, called for side effects
}
\description{
Print Method for the Statistical Approach
}
\examples{
cs_results <- claus_2020 |>
  cs_statistical(
    id,
    time,
    hamd,
    pre = 1,
    post = 4,
    m_functional = 8,
    sd_functional = 7
  )

cs_results
}
