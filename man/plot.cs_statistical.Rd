% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{plot.cs_statistical}
\alias{plot.cs_statistical}
\title{Plot an Object of Class cs_statistical}
\usage{
\method{plot}{cs_statistical}(
  x,
  x_lab = "Pre",
  y_lab = "Post",
  color_lab = "Group",
  include_cutoff = TRUE,
  lower_limit,
  upper_limit,
  show,
  point_alpha = 1,
  overplotting = 0.02,
  ...
)
}
\arguments{
\item{x}{An object of class \code{cs_statistical}}

\item{x_lab}{String, x axis label. Default is \code{"Pre"}.}

\item{y_lab}{String, x axis label. Default is \code{"Post"}.}

\item{color_lab}{String, color label (if colors are displayed). Default is
\code{"Group"}}

\item{include_cutoff}{Logical, whether to include the population cutoff.
Default is \code{TRUE}.}

\item{lower_limit}{Numeric, lower plotting limit. Defaults to 2\% smaller than
minimum instrument score}

\item{upper_limit}{Numeric, upper plotting limit. Defaults to 2\% larger than
maximum instrument score}

\item{show}{Unquoted category name. You have several options to color
different features. Available are
\itemize{
\item \code{category} (shows all categories at once)
\item \code{clinical_pre} (shows participants with clinical scores pre intervention)
\item \code{functional_post} (shows participants with functional scores post
intervention)
\item \code{unchanged} (shows unchanged participants)
}}

\item{point_alpha}{Numeric, transparency adjustment for points. A value
between 0 and 1 where 1 corresponds to not transparent at all and 0 to
fully transparent.}

\item{overplotting}{Numeric, control amount of overplotting. Defaults to 0.02
(i.e., 2\% of range between lower and upper limit).}

\item{...}{Additional arguments}
}
\value{
A ggplot2 plot
}
\description{
This function creates a generic clinical significance plot by
plotting the patients' pre intervention value on the x-axis and the post
intervention score on the y-axis.
}
\examples{
cs_results <- antidepressants |>
  cs_statistical(
    patient,
    measurement,
    pre = "Before",
    mom_di,
    m_functional = 15,
    sd_functional = 8,
    cutoff_type = "c"
  )


# Plot the results "as is"
plot(cs_results)


# Change the axis labels
plot(cs_results, x_lab = "Before Intervention", y_lab = "After Intervention")


# Show the individual categories
plot(cs_results, show = category)


# Show groups as specified in the data
cs_results_grouped <- antidepressants |>
  cs_statistical(
    patient,
    measurement,
    pre = "Before",
    mom_di,
    m_functional = 15,
    sd_functional = 8,
    cutoff_type = "c",
    group = condition
  )

plot(cs_results_grouped)


# To avoid overplotting, generic ggplot2 code can be used to facet the plot
library(ggplot2)
plot(cs_results_grouped) +
  facet_wrap(~ group)


# Adjust the transparency of individual data points
plot(cs_results, point_alpha = 0.3)


# Control the overplotting
plot(cs_results, overplotting = 0.1)


# Or adjust the axis limits by hand
plot(cs_results, lower_limit = 0, upper_limit = 80)
}
