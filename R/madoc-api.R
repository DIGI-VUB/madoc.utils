#' @title Retrieve projects from Madoc
#' @description Get a data.frame of all projects on a Madoc site by using the Madoc API's.\cr
#' This function performs a GET request to /madoc/api/projects. 
#' @param site character string with the site
#' @export
#' @return a data.frame with columns project_id, collection_id, slug, label and summary
#' @note TODO: perform pagination
#' @examples 
#' projects  <- madoc_projects("https://www.madoc.ugent.be/s/brugse-vrije")
#' \donttest{
#' projects  <- madoc_projects("https://www.madoc.ugent.be/s/brugse-vrije-gebruikerstest")
#' }
madoc_projects <- function(site){
  ## Get the projects
  msg      <- httr::GET(sprintf("%s/madoc/api/projects", site), encode = "json")
  response <- httr::content(msg, as = "text")
  info     <- jsonlite::fromJSON(response, simplifyVector = FALSE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
  info     <- info$projects
  info <- lapply(info, FUN = function(x){
    list(project_id    = x$id, 
         collection_id = x$collection_id, 
         slug          = x$slug, 
         label         = udpipe::txt_collapse(unlist(x$label, use.names = FALSE), "\n"),
         summary       = udpipe::txt_collapse(unlist(x$summary, use.names = FALSE), "\n"))
  })
  info <- data.table::rbindlist(info)
  info <- data.table::setDF(info)
  info
}

#' @title Retrieve a collection from Madoc
#' @description Get a data.frame of all manifests which are part of a collection by using the Madoc API's.\cr
#' This function performs a GET request to /madoc/api/collections and handles pagination 
#' @param site character string with the site
#' @param id the id of the collection
#' @param tidy_metadata logical indicating to add the metadata in a tidy data.frame instead of a list column
#' @export
#' @return a data.frame with columns collection_id, manifest_id, manifest_type, manifest_label, manifest_canvasCount, manifest_thumbnail and manifest_metadata
#' @examples 
#' projects  <- madoc_projects("https://www.madoc.ugent.be/s/brugse-vrije")
#' manifests <- madoc_collection(site = "https://www.madoc.ugent.be/s/brugse-vrije", 
#'                               id = projects$collection_id)
#' manifests <- madoc_collection(site = "https://www.madoc.ugent.be/s/brugse-vrije", 
#'                               id = projects$collection_id, tidy_metadata = TRUE)
madoc_collection <- function(site, id, tidy_metadata = FALSE){
  parse_collection <- function(response){
    info     <- fromJSON(response, simplifyDataFrame = FALSE, simplifyVector = FALSE, simplifyMatrix = FALSE)
    collectie <- data.frame(manifest_id = sapply(info$collection$items, FUN = function(x) x$id),
                            manifest_type = sapply(info$collection$items, FUN = function(x) x$type),
                            manifest_label = sapply(info$collection$items, FUN = function(x) txt_collapse(unlist(x$label), collapse = " - ")), 
                            manifest_canvasCount = sapply(info$collection$items, FUN = function(x) x$canvasCount),
                            manifest_thumbnail = txt_collapse(sapply(info$collection$items, FUN = function(x) x$thumbnail)),
                            manifest_metadata = I(lapply(info$collection$items, FUN = function(item){
                              if(!"metadata" %in% names(item)){
                                item$metadata <- list(list(label = character(), value = character()))
                              }
                              metadata <- lapply(item$metadata, FUN = function(x){
                                out <- list(key = unlist(x$label, use.names = FALSE, recursive = TRUE), value = unlist(x$value, use.names = FALSE, recursive = TRUE))
                                if(length(out) == 0){
                                  out <- list(key = character(), value = character())
                                }
                                out
                              })
                              metadata <- rbindlist(metadata)
                              #metadata <- dcast.data.table(metadata, formula = key ~ value)
                              metadata
                              #
                            })),
                            row.names = NULL, stringsAsFactors = FALSE)
    collectie
  }
  info <- list()
  page <- 1
  page_max <- +Inf
  while(page < page_max){
    msg         <- GET(sprintf("%s/madoc/api/collections/%s", site, id), encode = "json", query = list(page = page))
    response    <- content(msg, as = "text")
    pagination  <- fromJSON(response)$pagination
    page_max    <- pagination$totalPages
    page        <- pagination$page + 1
    info[[page]] <- parse_collection(response)
  }
  info  <- data.table::rbindlist(info)
  info$collection_id <- rep(id, nrow(info))
  first <- "collection_id"
  info <- data.table::setcolorder(info, neworder = c(first, setdiff(colnames(info), first)))
  info <- data.table::setDF(info)
  if(tidy_metadata){
    metadata <- madoc_manifest_metadata(info)
    info     <- merge(info[, c("collection_id", "manifest_id", "manifest_type", "manifest_label", "manifest_canvasCount")], 
                      metadata, by = "manifest_id", all.x = TRUE, all.y = TRUE)
  }
  
  
  info
}

#' @title Retrieve canvasses from Madoc
#' @description Get a data.frame of all canvasses which are part of a manifest by using the Madoc API's.\cr
#' This function performs a GET request to /madoc/api/manifests
#' @param site character string with the site
#' @param id a vector of id's of manifests
#' @export
#' @return a data.frame with columns manifest_id, manifest_label, manifest_thumbnail, canvas_id, canvas_thumbnail, is_published
#' @examples 
#' projects  <- madoc_projects("https://www.madoc.ugent.be/s/brugse-vrije")
#' manifests <- madoc_collection(site = "https://www.madoc.ugent.be/s/brugse-vrije", 
#'                               id = projects$collection_id)
#' 
#' canvasses <- madoc_manifest(site = "https://www.madoc.ugent.be/s/brugse-vrije", 
#'                             id = sample(manifests$manifest_id, size = 1))
#' ids       <- sample(manifests$manifest_id, size = 10)
#' canvasses <- madoc_manifest(site = "https://www.madoc.ugent.be/s/brugse-vrije", 
#'                             id = ids)
madoc_manifest <- function(site, id){
  if(length(id) > 1){
    out <- lapply(id, FUN = function(id) madoc_manifest(site, id))
    out <- data.table::rbindlist(out)
    out <- data.table::setDF(out)
    return(out)
  }
  msg      <- GET(sprintf("%s/madoc/api/manifests/%s", site, id), encode = "json")
  response <- content(msg, as = "text")
  info     <- fromJSON(response)
  if(length(info$manifest$items) == 0){
    return(data.frame(manifest_id = integer(), manifest_label = character(), manifest_thumbnail = character(), canvas_id = integer(), canvas_thumbnail = integer(), is_published = logical(), stringsAsFactors = FALSE))
  }
  out <- data.frame(canvas_id = info$manifest$items$id, 
                    canvas_thumbnail = info$manifest$items$thumbnail, 
                    is_published = info$manifest$items$published, 
                    stringsAsFactors = FALSE)
  out$manifest_id <- rep(info$manifest$id, nrow(out))
  out$manifest_thumbnail <- rep(info$manifest$thumbnail, nrow(out))
  out$manifest_label <- rep(txt_collapse(unlist(info$manifest$label, use.names = FALSE)), nrow(out))
  out <- out[, c("manifest_id", "manifest_label", "manifest_thumbnail", "canvas_id", "canvas_thumbnail", "is_published")]
  out
}

#' @title Put manifest metadata in a data.frame
#' @description Convert manifest metadata to a data.frame
#' @param x a data.frame with the output of \code{\link{madoc_manifest}}
#' @export
#' @return a data.frame with columns manifest_id and the columns available in the metadata, lowercased
#' @examples 
#' projects  <- madoc_projects("https://www.madoc.ugent.be/s/brugse-vrije")
#' manifests <- madoc_collection(site = "https://www.madoc.ugent.be/s/brugse-vrije", 
#'                               id = projects$collection_id)
#' metadata  <- madoc_manifest_metadata(manifests)
madoc_manifest_metadata <- function(x){
  stopifnot(is.data.frame(x) && all(c("manifest_id", "manifest_metadata") %in% colnames(x)))
  manifests <- x
  x <- stats::setNames(manifests$manifest_metadata, manifests$manifest_id)
  x <- data.table::rbindlist(x, idcol = "manifest_id")
  x <- data.table::dcast.data.table(data = x, formula = manifest_id ~ key)
  x <- data.table::setnames(x, old = colnames(x), new = tolower(colnames(x)))
  x <- data.table::setDF(x)
  mani <- data.frame(manifest_id = manifests$manifest_id, stringsAsFactors = FALSE)
  mani <- merge(mani, x, by = "manifest_id", all.x = TRUE, all.y = FALSE, sort = FALSE)
  mani
}

#' @title Retrieve canvas image details from Madoc 
#' @description Get a data.frame of canvas information (image height/width + image_url) by using the Madoc API's.\cr
#' This function performs a GET request to /madoc/api/canvases
#' @param site character string with the site
#' @param id a vector of id's of canvasses
#' @export
#' @return a data.frame with columns canvas_id, height, width, image_url
#' @examples 
#' projects  <- madoc_projects("https://www.madoc.ugent.be/s/brugse-vrije")
#' manifests <- madoc_collection(site = "https://www.madoc.ugent.be/s/brugse-vrije", 
#'                               id = projects$collection_id)
#' 
#' ids               <- sample(manifests$manifest_id, size = 10)
#' canvasses         <- madoc_manifest(site = "https://www.madoc.ugent.be/s/brugse-vrije", 
#'                                     id = ids)
#' canvasses_urls    <- madoc_canvas_image("https://www.madoc.ugent.be/s/brugse-vrije", 
#'                                         id = canvasses$canvas_id)
#' canvasses         <- merge(canvasses, canvasses_urls, by = "canvas_id")
#' 
#' library(magick)
#' image_read(unlist(canvasses$image_url[[1]]))
madoc_canvas_image <- function(site, id){
  if(length(id) > 1){
    out <- lapply(id, FUN = function(id) madoc_canvas_image(site, id))
    out <- data.table::rbindlist(out)
    out <- data.table::setDF(out)
    return(out)
  }
  msg      <- GET(sprintf("%s/madoc/api/canvases/%s", site, id), encode = "json")
  response <- content(msg, as = "text")
  info     <- fromJSON(response)
  x <- data.frame(
    canvas_id = info$canvas$id, 
    height = info$canvas$height,
    width = info$canvas$width,
    image_url = I(lapply(info$canvas$items$items, FUN = function(x) x$body$id)),
    stringsAsFactors = FALSE)
  x$image_url <- txt_collapse(x$image_url, collapse = ";")
  x
}

#' @title Retrieve canvas model annotations from Madoc 
#' @description Get a data.frame of annotations by using the Madoc API's.\cr
#' This function performs a GET request to /madoc/api/canvases/{canvas_id}/models
#' @param site character string with the site
#' @param id an id of a canvas
#' @export
#' @return in case 
#' \itemize{
#' \item{id is of length 1: }{a list with elements canvas_id and annotations where annotations is a data.frame with columns document_id, document_type, document_label, model_id, status, authors, id, type, value, label, id_revision, id_revises, selector_state, selector_type, selector_id. }
#' \item{id is vector of length > 1: }{a data.frame with columns canvas_id, document_id, document_type, document_label, model_id, status, authors, id, type, value, label, id_revision, id_revises, selector_state, selector_type, selector_id. Containing only rows for canvasses which have annotations.}
#' }
#' @examples 
#' projects     <- madoc_projects("https://www.madoc.ugent.be/s/brugse-vrije")
#' manifests    <- madoc_collection(site = "https://www.madoc.ugent.be/s/brugse-vrije", 
#'                                  id = projects$collection_id)
#' 
#' ids          <- sample(manifests$manifest_id, size = 10)
#' canvasses    <- madoc_manifest(site = "https://www.madoc.ugent.be/s/brugse-vrije", 
#'                                id = ids)
#' annotations  <- madoc_canvas_model(site = "https://www.madoc.ugent.be/s/brugse-vrije", 
#'                                    id = ids)
madoc_canvas_model <- function(site, id){
  if(length(id) > 1){
    out <- lapply(id, FUN = function(id) madoc_canvas_model(site, id))
    out <- lapply(out, FUN = function(x){
      x$annotations$canvas_id <- rep(x$canvas_id, nrow(x$annotations))
      x$annotations
    })
    out   <- data.table::rbindlist(out, use.names = TRUE, fill = TRUE)
    first <- intersect(c("canvas_id", "document_id", "document_type", "document_label", "model_id", "status", "authors"), colnames(out))
    out   <- data.table::setcolorder(out, neworder = c(first, setdiff(colnames(out), first)))
    out   <- data.table::setDF(out)
    return(out)
  }
  msg      <- GET(sprintf("%s/madoc/api/canvases/%s/models", site, id), encode = "json")
  response <- content(msg, as = "text")
  info     <- fromJSON(response, simplifyVector = FALSE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
  info     <- info$models
  
  details <- lapply(info, FUN = function(x){
    annotations <- lapply(x$document$properties, FUN = function(x){
      property <- lapply(x, FUN = function(x){
        data.frame(id = x$id, 
                   type = x$type, 
                   value = x$value, 
                   label = x$label,
                   id_revision = txt_collapse(x$revision), 
                   id_revises = txt_collapse(x$revises),
                   selector_state = txt_collapse(x$selector$state),
                   selector_type = txt_collapse(x$selector$type),
                   selector_id = txt_collapse(x$selector$id))  
      })
      property <- data.table::rbindlist(property)
      property <- data.table::setDF(property)
      property
    }) 
    annotations                <- data.table::rbindlist(annotations)
    revisions <- lapply(x$revisions, FUN = function(x){
      revision <- data.frame(id_revision = x$id, 
                             status = x$status,
                             authors = udpipe::txt_collapse(x$authors, collapse = ";"), stringsAsFactors = FALSE)
      revision
    })
    revisions                 <- data.table::rbindlist(revisions)
    if(ncol(revisions) > 0 && ncol(annotations) > 0){
      annotations$authors       <- txt_recode(annotations$id_revision, from = revisions$id_revision, to = revisions$authors)
      annotations$status        <- txt_recode(annotations$id_revision, from = revisions$id_revision, to = revisions$status)  
    }
    annotations                <- data.table::setDF(annotations)
    annotations$document_id    <- rep(x$document$id, nrow(annotations))
    annotations$document_type  <- rep(x$document$type, nrow(annotations))
    annotations$document_label <- rep(x$document$label, nrow(annotations))
    annotations$model_id       <- rep(x$structure$id, nrow(annotations))
    first                      <- intersect(c("document_id", "document_type", "document_label", "model_id", "status", "authors"), colnames(annotations))
    annotations                <- data.table::setcolorder(annotations, neworder = c(first, setdiff(colnames(annotations), first)))
    annotations
  })
  details <- data.table::rbindlist(details)
  details <- data.table::setDF(details)
  if(ncol(details) == 0){
    details <- data.frame()
  }
  list(canvas_id = id, 
       annotations = details)
}




#' @title Get Madoc tasks 
#' @description Retrieve tasks from Madoc, namely manifest and canvas tass
#' @param site character string with the site
#' @param project character string with the project name of the site
#' @param id a vector of manifest or task identifiers
#' @param type character string with the type of task, either 'manifest' or 'canvas'
#' @export
#' @return a data.frame with task information
#' @examples 
#' projects  <- madoc_projects("https://www.madoc.ugent.be/s/brugse-vrije")
#' projects  <- subset(projects, slug == "brugse-vrije-gebruikerstest")
#' manifests <- madoc_collection(site = "https://www.madoc.ugent.be/s/brugse-vrije", 
#'                               id = projects$collection_id, tidy_metadata = TRUE)
#' canvasses <- madoc_manifest(site = "https://www.madoc.ugent.be/s/brugse-vrije", 
#'                             id = manifests$manifest_id)
#' ids       <- sample(manifests$manifest_id, size = 5)
#' x         <- madoc_tasks(site = "https://www.madoc.ugent.be/s/brugse-vrije", 
#'                          project = "brugse-vrije-gebruikerstest", 
#'                          id = ids, type = "manifest")
#' ids       <- sample(canvasses$canvas_id, size = 5)
#' x         <- madoc_tasks(site = "https://www.madoc.ugent.be/s/brugse-vrije", 
#'                          project = "brugse-vrije-gebruikerstest", 
#'                          id = canvasses$canvas_id, type = "canvas")
madoc_tasks <- function(site, project, id, type = c("manifest", "canvas")){
  # /s/:slug/madoc/api/projects/:projectSlug/manifest-tasks/:manifestId
  # /s/:slug/madoc/api/projects/:projectSlug/canvas-tasks/:canvasId
  type <- match.arg(type)
  if(length(id) > 1){
    out <- lapply(id, FUN = function(id, site, project, type){
      madoc_tasks(site = site, project = project, id = id, type = type) 
    }, site = site, project = project, type = type)
    names(out) <- id
    out <- suppressWarnings(setDF(rbindlist(out, fill = TRUE, use.names = TRUE, idcol = sprintf("%s_id", type))))
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
    task <- tidy_manifest(info)
    return(task)
  }else if(type == "canvas"){
    task_manifest           <- tidy_manifest(info)
    task                    <- info$canvasTask
    if("created_at" %in% names(task)){
      task$created_at  <- as.POSIXct(task$created_at / 1000, origin = "1970-01-01")
    }
    if("modified_at" %in% names(task)){
      task$modified_at <- as.POSIXct(task$modified_at / 1000, origin = "1970-01-01")  
    }
    task$manifest           <- list(task_manifest)
    task$isManifestComplete <- info$isManifestComplete
    task$canClaimManifest   <- info$canClaimManifest
    task$totalContributors  <- info$totalContributors
    task$maxContributors    <- info$maxContributors
    task$canUserSubmit      <- info$canUserSubmit
    task$parameters         <- list(task$parameters)
    task$state              <- list(task$state)
    exclude  <- c("metadata")
    fields   <- intersect(names(task), 
                          setdiff(c("id", "name", "status", "status_text", "type", "parameters", 
                                    "subject", "subject_parent", "root_task", "modified_at", "state", 
                                    "creator", "assignee", "parent_task", "metadata", "manifest",
                                    "isManifestComplete", "canClaimManifest", "totalContributors", 
                                    "maxContributors", "canUserSubmit"), 
                                  exclude))
    task <- task[fields]
    return(task)
  }
  info
}


tidy_manifest <- function(info){
  task                    <- info$manifestTask
  if("created_at" %in% names(task)){
    task$created_at  <- as.POSIXct(task$created_at / 1000, origin = "1970-01-01")
  }
  if("modified_at" %in% names(task)){
    task$modified_at <- as.POSIXct(task$modified_at / 1000, origin = "1970-01-01")  
  }
  task$isManifestComplete <- info$isManifestComplete
  task$canClaimManifest   <- info$canClaimManifest
  task$totalContributors  <- info$totalContributors
  task$maxContributors    <- info$maxContributors
  task$canUserSubmit      <- info$canUserSubmit
  task$creator            <- task$creator$id
  task$subtasks           <- lapply(task$subtasks, FUN = function(x){
    x$assignee <- x$assignee$id
    x
  })
  task$subtasks           <- suppressWarnings(setDF(rbindlist(task$subtasks, fill = TRUE, use.names = TRUE)))
  task$subtasks           <- list(task$subtasks)
  task$parameters         <- list(task$parameters)
  task$state              <- list(task$state)
  exclude  <- c("pagination", "metadata", "events", "context")
  fields   <- intersect(names(task), 
                        setdiff(c("id", "name", "description", "type", "subject", "status", "status_text", 
                                  "state", "created_at", "parameters", "context", "modified_at", 
                                  "root_task", "subject_parent", "delegated_owners", "delegated_task", 
                                  "creator", "assignee", "parent_task", "events", "metadata", "subtasks", 
                                  "pagination", "isManifestComplete", "canClaimManifest", "totalContributors", 
                                  "maxContributors", "canUserSubmit"), 
                                exclude))
  task <- task[fields]
  task
}



