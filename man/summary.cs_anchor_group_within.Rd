% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cs_anchor.R
\name{summary.cs_anchor_group_within}
\alias{summary.cs_anchor_group_within}
\title{Summary Method for the Anchor-Based Approach for Groups (Within)}
\usage{
\method{summary}{cs_anchor_group_within}(object, ...)
}
\arguments{
\item{object}{An object of class \code{cs_anchor_group_within}}

\item{...}{Additional arguments}
}
\value{
No return value, called for side effects only
}
\description{
Summary Method for the Anchor-Based Approach for Groups (Within)
}
\examples{
cs_results <- claus_2020 |>
  cs_anchor(
    id,
    time,
    bdi,
    pre = 1,
    post = 4,
    mid_improvement = 8,
    target = "group"
  )

summary(cs_results)
}
