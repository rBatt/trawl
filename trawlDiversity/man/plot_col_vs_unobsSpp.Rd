% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_col_vs_unobsSpp.R
\name{plot_col_vs_unobsSpp}
\alias{plot_col_vs_unobsSpp}
\title{Plot colonizers and unobserved species}
\usage{
plot_col_vs_unobsSpp(prn, Figures, FUN = "dev.new", ...)
}
\arguments{
\item{prn}{the p object (processed msom; output from \code{process_msomStatic})}

\item{Figures}{option list to which the figure and its information should be added}

\item{FUN}{graphical device function}

\item{...}{arguments passed to plot_device}
}
\value{
Returns the Figure object
}
\description{
Colonizers from data this year versus MSOM estimates of last year's unobserved species
}
\details{
The function uses \code{unpack_p} to get much of the information it needs. The \code{Figures} object is a list whose first level is intended to be the region. The second level is specific to each figure. The third level has 3 elements: 'figure', 'name', and 'dim'. The 'figure' element is the result of using \code{\link{recordPlot}} on what is plotted. The name of the figure is, e.g., what the saved figure would be called. The dim is the width and height (in that order) in inches.

I'm not sure how valuable this figure is. But the original idea was that if the survey didn't detect a species last year (but it was present), maybe the MSOM would have estimated it as a present but unobserved species. And if that unobserved species was observed this year, it could look like a colonization. In that case, observed colonizations might just be correlated with the number of unobserved species from last year. Obviously unobserved species from last year could have gone extinct, and, more importantly, the MSOM isn't perfect. So I'm hesitant to read too far into the results of this figure. However, I do think it is useful to note the relative magnitude of unobserved species in a given year, and the number of colonizers in a given year.
}
\seealso{
There are several functions that are run through the process_msom_figures script. Richness and temperature plots are \code{\link{plot_btemp_map}}, \code{\link{plot_rich_bt_scatter}}, and \code{\link{plot_rich_bt_ts}}. Figures for colonization, extinction, and the species and places associated with those processes are \code{\link{plot_ce_wrap}}, \code{\link{plot_col_vs_unobsSpp}}, \code{\link{plot_colExt_perStrat}}, and \code{\link{plot_rank_temp}}. Figures for diagnostics are \code{\link{plot_traceplot}} and \code{\link{plot_post_corr}}.
}

