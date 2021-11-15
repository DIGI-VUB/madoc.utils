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


#' @title TODO
#' @description TODO
#' @param site TODO
#' @param project TODO
#' @param id TODO
#' @param type TODO
#' @export
#' @return TODO
#' @examples 
#' site    <- "https://www.madoc.ugent.be/s/brugse-vrije"
#' project <- "brugse-vrije-gebruikerstest"
#' id      <- 407
#' x       <- madoc_tasks(site = site, project = project, id = id, type = "manifest")
#' 
#' projects  <- madoc_projects(site)
#' projects  <- subset(projects, slug == "brugse-vrije-gebruikerstest")
#' manifests <- madoc_collection(site = site, id = projects$collection_id, tidy_metadata = TRUE)
#' x <- madoc_tasks(site = site, project = project, id = head(manifests$manifest_id), type = "manifest")
madoc_tasks <- function(site, project, id, type = c("manifest", "canvas")){
  #'/s/:slug/madoc/api/projects/:projectSlug/manifest-tasks/:manifestId'
  #'/s/:slug/madoc/api/projects/:projectSlug/canvas-tasks/:canvasId'
  type <- match.arg(type)
  if(length(id) > 1){
    out <- lapply(id, FUN = function(id, site, project, type) madoc_tasks(site = site, project = project, id = id, type = type), 
                  site = site, project = project, type = type)
    names(out) <- id
    out <- rbindlist(out, fill = TRUE, use.names = TRUE, idcol = "manifest_id")
    out <- setDF(out)
    return(out)
  }
  if(type == "manifest"){
    url <- sprintf("%s/madoc/api/projects/%s/manifest-tasks/%s", site, project, id)  
  }else if(type == "canvas"){
    url <- sprintf("%s/madoc/api/projects/%s/canvas-tasks/%s", site, project, id)  
  }
  msg      <- httr::GET(url, encode = "json")
  response <- httr::content(msg, as = "text")
  info     <- jsonlite::fromJSON(response, simplifyVector = FALSE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
  if(type == "manifest"){
    info$manifestTask$created_at  <- as.POSIXct(info$manifestTask$created_at/1000, origin = "1970-01-01")
    info$manifestTask$modified_at <- as.POSIXct(info$manifestTask$modified_at/1000, origin = "1970-01-01")
    task <- info$manifestTask
    task$isManifestComplete <- info$isManifestComplete
    task$canClaimManifest   <- info$canClaimManifest
    task$totalContributors  <- info$totalContributors
    task$maxContributors    <- info$maxContributors
    task$canUserSubmit      <- info$canUserSubmit
    exclude <- c("pagination", "metadata", "events", "context")
    task$creator           <- task$creator$id
    task$subtasks <- lapply(task$subtasks, FUN = function(x){
      x$assignee <- x$assignee$id
      x
    })
    task$subtasks <- suppressWarnings(setDF(rbindlist(task$subtasks, fill = TRUE, use.names = TRUE)))
    fields <- intersect(names(task), setdiff(c("id", "name", "description", "type", "subject", "status", "status_text", 
                              "state", "created_at", "parameters", "context", "modified_at", 
                              "root_task", "subject_parent", "delegated_owners", "delegated_task", 
                              "creator", "assignee", "parent_task", "events", "metadata", "subtasks", 
                              "pagination", "isManifestComplete", "canClaimManifest", "totalContributors", 
                              "maxContributors", "canUserSubmit"), exclude))
    task <- task[fields]
    return(task)
  }
  info
}


