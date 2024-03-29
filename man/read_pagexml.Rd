% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/alto.R
\name{read_pagexml}
\alias{read_pagexml}
\title{Read Page-XML file}
\usage{
read_pagexml(x, type = c("transkribus"), ...)
}
\arguments{
\item{x}{path to the xml file}

\item{type}{character string with the type of xml structure. Defaults to 'transkribus' and is currently the only option.}

\item{...}{further arguments currently not used}
}
\value{
a data.frame with columns file, id, coords and baseline
}
\description{
Import Page-XML file as a data.frame. Examples where this can be used
is for importing page-xml files from Transkribus
}
\note{
the function only handles single-page XML's
}
\examples{
f <- system.file(package = "madoc.utils", "extdata", "pagexml-example.xml")
x <- read_pagexml(f)
f <- system.file(package = "madoc.utils", "extdata", "multiregion-page.xml")
x <- read_pagexml(f)
}
