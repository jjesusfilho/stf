% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stf_read_rtf.R
\name{stf_read_rtf}
\alias{stf_read_rtf}
\title{Reads rtf files}
\usage{
stf_read_rtf(files = NULL, path = ".")
}
\arguments{
\item{files}{of paths to the rtf files to be read.}

\item{path}{Path to rtf files if files are not informed}
}
\value{
a tibble with tree columns: incidente, texto, and doc_id.
}
\description{
Reads rtf files
}
\examples{
\dontrun{
stf_read_rtf(files = "")
}
}
