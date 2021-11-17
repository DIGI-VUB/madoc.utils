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


#' @title Get task details
#' @description Get details on tasks. This requires logging in using \code{\link{madoc_login}}
#' @param site character string with the main site (not project-specific) 
#' @param id character string with the id of the task
#' @export
#' @return a data.frame with the Madoc task details
#' @examples 
#' madoc_login("https://www.madoc.ugent.be/s/brugse-vrije", 
#'             email = "jan.wijffels@vub.be", password = Sys.getenv("MADOC_PWD"))
#' projects  <- madoc_projects("https://www.madoc.ugent.be/s/brugse-vrije")
#' projects  <- subset(projects, slug == "brugse-vrije-gebruikerstest")
#' manifests <- madoc_collection(site = "https://www.madoc.ugent.be/s/brugse-vrije", 
#'                               id = projects$collection_id, tidy_metadata = TRUE)
#' canvasses <- madoc_manifest(site = "https://www.madoc.ugent.be/s/brugse-vrije", 
#'                             id = manifests$manifest_id)
#' ids       <- canvasses$canvas_id                   
#' ids       <- sample(canvasses$canvas_id, size = 5)
#' x         <- madoc_tasks(site = "https://www.madoc.ugent.be/s/brugse-vrije", 
#'                          project = "brugse-vrije-gebruikerstest", 
#'                          id = canvasses$canvas_id, type = "canvas")
#' x         <- subset(x, !is.na(id))
#' tk        <- madoc_taskdetails("https://www.madoc.ugent.be", x$id)
madoc_taskdetails <- function(site, id){
  ## https://github.com/digirati-co-uk/tasks-api/blob/main/src/router.ts
  if(length(id) > 1){
    out <- lapply(id, FUN = function(id, site){
      madoc_taskdetails(site = site, id = id) 
    }, site = site)
    names(out) <- id
    out <- suppressWarnings(setDF(rbindlist(out, fill = TRUE, use.names = TRUE, idcol = "task_id")))
    return(out)
  }
  msg      <- httr::GET(sprintf("%s/api/tasks/%s", site, id), 
                        .madoc$tokenheader,
                        encode = "json")
  response <- httr::content(msg, as = "text")
  info     <- jsonlite::fromJSON(response, simplifyVector = FALSE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
  exclude  <- c("pagination", "metadata")
  task <- info[intersect(names(info), setdiff(c("id", "name", "description", "type", "subject", "status", "status_text", 
                                 "state", "created_at", "parameters", "context", "modified_at", 
                                 "root_task", "subject_parent", "delegated_owners", "delegated_task", 
                                 "creator", "assignee", "parent_task", "events", "metadata", "subtasks", 
                                 "pagination"), exclude))]
  if("created_at" %in% names(task)){
    task$created_at  <- as.POSIXct(task$created_at / 1000, origin = "1970-01-01")
  }
  if("modified_at" %in% names(task)){
    task$modified_at <- as.POSIXct(task$modified_at / 1000, origin = "1970-01-01")  
  }
  task$context <- txt_collapse(task$context, collapse = ";")
  task$creator <- task$creator$id
  task$events  <- txt_collapse(task$events, collapse = ";")
  task$state   <- list(task$state)
  task$subtasks <- lapply(task$subtasks, FUN = function(x){
    fields <- c("id", "type", "name", "status", "subject", "status_text", "state", "metadata")
    fields <- setdiff(fields, c("metadata"))
    x <- x[intersect(names(x), fields)]
    x$state <- list(x$state)
    
    x <- as.data.table(x)
    x
  })
  task$subtasks <- rbindlist(task$subtasks, use.names = TRUE, fill = TRUE)
  task$subtasks <- setDF(task$subtasks)
  task$subtasks <- list(task$subtasks)
  task <- as.data.table(task)
  task
}
