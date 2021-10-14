
madoc_users <- function(site, token){
  ## Get the projects
  msg      <- GET(sprintf("%s/madoc/api/projects", site), encode = "json")
  response <- content(msg, as = "text")
  info     <- fromJSON(response, simplifyVector = FALSE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
  info
}