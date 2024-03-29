% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_rci.R
\name{calc_rci.cs_jt}
\alias{calc_rci.cs_jt}
\title{RCI for the Jacobson & Truax method}
\usage{
\method{calc_rci}{cs_jt}(data, sd_pre, reliability, direction = 1, critical_value = 1.96, ...)
}
\arguments{
\item{sd_pre}{Pre measurement SD}

\item{reliability}{Instrument reliability}

\item{direction}{Beneficial intervention effect for given instrument. 1 =
higher is better, -1 = lower is better}

\item{critical_value}{Critical RCI value, typically 1.96}
}
\description{
RCI for the Jacobson & Truax method
}
\keyword{internal}
