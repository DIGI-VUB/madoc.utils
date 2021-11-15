.madoc <- new.env()

#' @title Log in with user/password at Madoc
#' @description Logging in allows to get non-public data from Madoc
#' @param site character string with the site
#' @param email character string with your email address of the Madoc user
#' @param password character string with the password used by the user of the provided \code{email}
#' @export
#' @return invisibly a data.frame with the cookies from the call to {site}/madoc/login
#' @examples 
#' x  <- madoc_login("https://www.madoc.ugent.be/s/brugse-vrije",
#'                   email = "jan.wijffels@vub.be",
#'                   password = Sys.getenv("MADOC_PWD"))
#' x
madoc_login <- function(site, email, password){
  sitename <- tail(unlist(strsplit(site, "/")), n = 1)
  msg      <- httr::POST(url    = sprintf("%s/madoc/login", site), 
                         body   = list(email = email, password = password, submit = "Log+in", redir = ""), 
                         encode = "form")
  info     <- httr::cookies(msg)
  token    <- info[which(grepl(pattern = sprintf("/%s$", sitename), info$path)), ]
  token    <- token[which(grepl(pattern = sprintf("/%s$", sitename), token$name)), ]
  .madoc$token       <- token$value
  .madoc$tokenheader <- httr::add_headers("Authorization" = sprintf("Bearer %s", token$value))
  .madoc$url         <- gsub(site, pattern = token$path, replacement = "")
  invisible(info)
}

#' @title Get the list of Madoc users
#' @description Get the list of Madoc users. This requires logging in using \code{\link{madoc_login}}
#' @export
#' @return a data.frame with the Madoc users with columns id, name, role, site_role, email
#' @examples 
#' madoc_login("https://www.madoc.ugent.be/s/brugse-vrije", 
#'             email = "jan.wijffels@vub.be", password = Sys.getenv("MADOC_PWD"))
#' x <- madoc_users()
madoc_users <- function(){
  url <- "https://www.madoc.ugent.be/api/madoc/manage-site/users"
  url <- sprintf("%s/api/madoc/manage-site/users", .madoc$url)
  msg      <- httr::GET(url, 
                        .madoc$tokenheader,
                        encode = "json")
  response <- httr::content(msg, as = "text")
  info     <- jsonlite::fromJSON(response)
  info$users
}


#'/s/:slug/madoc/api/projects/:projectSlug/manifest-tasks/:manifestId'
#'/s/:slug/madoc/api/projects/:projectSlug/canvas-tasks/:canvasId'