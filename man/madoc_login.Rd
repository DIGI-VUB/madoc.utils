% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/madoc-api-with-token.R
\name{madoc_login}
\alias{madoc_login}
\title{Log in with user/password at Madoc}
\usage{
madoc_login(site, email, password)
}
\arguments{
\item{site}{character string with the site}

\item{email}{character string with your email address of the Madoc user}

\item{password}{character string with the password used by the user of the provided \code{email}}
}
\value{
invisibly a data.frame with the cookies from the call to {site}/madoc/login
}
\description{
Logging in allows to get non-public data from Madoc
}
\examples{
x  <- madoc_login("https://www.madoc.ugent.be/s/brugse-vrije",
                  email = "jan.wijffels@vub.be",
                  password = Sys.getenv("MADOC_PWD"))
x
}
