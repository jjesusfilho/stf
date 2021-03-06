% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_stf_rtf.R
\name{download_stf_rtf}
\alias{download_stf_rtf}
\title{Downloads rtf documents based on doc_url column from docket sheet data.}
\usage{
download_stf_rtf(sheet, path = ".")
}
\arguments{
\item{sheet}{provide tibble read by function read_stf_sheet.}

\item{path}{where to download the texts.}
}
\value{
files with rtf texts.
}
\description{
Downloads rtf documents based on doc_url column from docket sheet data.
}
\details{
You don't need to inform which urls are rtfs.
}
\examples{
\dontrun{
download_stf_rtf(sheet = andamento)
}
}
