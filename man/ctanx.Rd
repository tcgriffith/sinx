% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sinx.R
\name{ctanx}
\alias{ctanx}
\title{Create TANX when starting R}
\usage{
ctanx(method = c("add", "remove"), lib = "sinxs")
}
\arguments{
\item{method}{(character) add or remove `sinx::tanx()` in ~/.Rprofile}

\item{lib}{library name of the sayings.See `?read.sinxs()`.}
}
\value{
a new ~/.Rprofile
}
\description{
Create TANX when starting R
}
\examples{
ctanx()
ctanx(lib = "jinyong")
ctanx("remove")
}
