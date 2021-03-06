% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/post.R
\name{PlotLinePnls}
\alias{PlotLinePnls}
\title{Line Plot for PnL xts object using ggplot2::geom_line}
\usage{
PlotLinePnls(pnls, title = "PnL")
}
\arguments{
\item{pnls}{xts object with columns being strategies, generated
from DRIVS::OutputWrapper(...)[[2]]}
}
\value{
ggplot object
}
\description{
Line Plot for PnL xts object using ggplot2::geom_line
}
\examples{
  png(filename = "./images/Signal_PnLs.png", width = 800, height = 500)
  DRIVS::PlotLinePnls(outputs.DRI[[2]])
  invisible(dev.off())
}
