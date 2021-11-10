

#' @title Transkribus API
#' @description Connect to Transkribus, inspect collections, documents, pages, perform handwritten text recognition
#' @field JSESSIONID character string with the JSESSIONID to use once logged in
#' @param user character string with your Transkribus user in order to connect
#' @param password character string with your Transkribus password in order to connect
#' @param url character string with the url to use in the call to the Transkribus API
#' @param collection id of the collection
#' @param document id of the document
#' @section Methods:
#' \describe{
#'   \item{\code{login(url, user, password)}}
#'   \item{\code{list_collections(url)}}
#'   \item{\code{list_collection(url, collection)}}
#'   \item{\code{list_document(url, collection, document)}}
#'   \item{\code{list_dictionaries(url)}}
#'   \item{\code{list_models(url, collection)}}
#'   \item{\code{list_jobs(url)}}
#'   \item{\code{transcribe(url, collection, document, page, model, dictionary)}}
#' }
#' @export
#' @examples 
#' library(madoc.utils)
#' api <- Transkribus$new(user     = "jan.wijffels@vub.ac.be", 
#'                        password = Sys.getenv("TRANSKRIBUS_PWD"))
#' 
#' ## Get pages of a collection
#' collections   <- api$list_collections()
#' collections
#' id_collection <- sample(collections$colId, size = 1)
#' documents     <- api$list_collection(collection = id_collection)
#' documents
#' id_document   <- sample(documents$docId, size = 1)
#' pages         <- api$list_document(collection = id_collection, document = id_document)
#' pages
#' 
#' ## Look at relevant models and dictionaries
#' dicts    <- api$list_dictionaries()
#' grep(dicts, pattern = "Dutch", ignore.case = TRUE, value = TRUE)
#' models   <- api$list_models(collection = id_collection)
#' str(models)
#' dutch    <- grep(models$language, pattern = "Dutch", ignore.case = TRUE, value = TRUE)
#' dutch    <- subset(models, language %in% dutch)
#' dutch    <- c("Dutch Mountains (18th Century)", "IJsberg", "Dutch Notarial Model 18th Century")
#' dutch    <- subset(models, name %in% dutch)
#' dutch    <- subset(models, name %in% "Dutch Mountains (18th Century)" & provider == "CITlabPlus")
#' str(dutch)
#' id_model <- dutch$htrId
#' 
#' ## Inspect jobs
#' jobs   <- api$list_job()
#' jobs
#' 
#' 
#' \dontrun{
#' ##
#' ## This section shows how to transcribe using the API
#' ##    >> note that this consumes Transkribus credits
#' 
#' ##
#' ## Inspect one image and transcribe it
#' ##
#' ##  - id_collection <- 123580 ## test collection
#' ##  - id_document   <- 817369 ## test document in that test collection
#' ##  - id_model      <- 21683  ## Dutch Mountains HTR+
#' pages         <- api$list_document(collection = 123580, document = 817369)
#' page          <- head(pages, n = 1)
#' id_job        <- api$transcribe(collection = 123580, document = 817369, 
#'                                 page = page$pageId,
#'                                 model = 21683, 
#'                                 dictionary = "Combined_Dutch_Model_M1.dict")
#' x             <- read_pagexml(page$page_xml) 
#' 
#' ##
#' ## A random document from a collection
#' ##
#' library(magick)
#' page <- tail(pages, n = 1)
#' page
#' img  <- image_read(page$thumbUrl)
#' img  <- image_read(page$url)
#' image_resize(img, "x600")
#' id_job <- api$transcribe(collection = id_collection, document = id_document, page = page$pageNr, 
#'                          model = id_model, dictionary = "Combined_Dutch_Model_M1.dict")
#' api$list_job(job = id_job)   
#'  
#' ## After the job has finished, we have a Page-XML file which we can read in
#' pages  <- api$list_document(collection = id_collection, document = id_document)  
#' page   <- tail(pages, n = 1)
#' img    <- image_read(page$url)
#' x      <- read_pagexml(page$page_xml)  
#' bl     <- image_draw_baselines(img, x = x$baseline, col = "darkgreen", lwd = 4)
#' image_resize(bl, "x900")
#' bl    <- image_crop_baselineareas(img, 
#'                                   x = setNames(x$baseline, x$id), 
#'                                   textregion = x$points, 
#'                                   extend = FALSE, overview = FALSE)
#' bl    <- image_rbind(bl, color = "red", geometry = "2x2")   
#' image_resize(bl, "x900")
#' }
Transkribus <- R6Class("Transkribus",
                  public = list(
                    JSESSIONID = NULL,
                    #' @description Log in with your Transkribus user and password
                    initialize = function(url = "https://transkribus.eu/TrpServer/rest/auth/login", user, password) {
                      res  <- httr::POST(url = url, body = list(user = user, pw = password), encode = "form")
                      msg  <- httr::content(res, as = "text")
                      info <- jsonlite::fromJSON(msg)
                      self$JSESSIONID <- info$sessionId
                      invisible(info)
                    },
                    #' @description List all collections you have access to
                    list_collections = function(url = "https://transkribus.eu/TrpServer/rest/collections/list"){
                      res  <- httr::GET(url = url, httr::add_headers(JSESSIONID = self$JSESSIONID))
                      msg  <- httr::content(res, as = "text")
                      info <- jsonlite::fromJSON(msg)
                      info
                    },
                    #' @description List the content (the documents) of a collection
                    list_collection = function(url = "https://transkribus.eu/TrpServer/rest/collections/%s/list", collection){
                      res  <- httr::GET(url = sprintf(url, collection), httr::add_headers(JSESSIONID = self$JSESSIONID))
                      msg  <- httr::content(res, as = "text")
                      info <- jsonlite::fromJSON(msg)
                      if("uploadTimestamp" %in% names(info)){
                        info$uploadTimestamp <- as.POSIXct(info$uploadTimestamp / 1000, origin = "1970-01-01")
                      }
                      info
                    },
                    #' @description List the content (the pages) of a document
                    #' @param type character string with the type of extraction, either 'pages' or 'raw'. Defaults to 'pages'
                    list_document = function(url = "https://transkribus.eu/TrpServer/rest/collections/%s/%s/fulldoc", collection, document, type = c("pages", "raw")){
                      type <- match.arg(type)
                      res  <- httr::GET(url = sprintf(url, collection, document), httr::add_headers(JSESSIONID = self$JSESSIONID))
                      msg  <- httr::content(res, as = "text")
                      info <- jsonlite::fromJSON(msg)
                      if(type == "pages"){
                        info <- info$pageList$pages
                        
                        try({
                          page_xml <- lapply(info$tsList$transcripts, FUN = function(x){
                            x$timestamp <- as.POSIXct(x$timestamp / 1000, origin = "1970-01-01")
                            x <- x[order(x$timestamp, decreasing = TRUE), ]
                            x <- head(x, n = 1)
                            x
                          })
                          info$page_xml <- udpipe::txt_collapse(lapply(page_xml, FUN = function(x) x$url))
                        })
                      }
                      info
                    },
                    #' @description Retrieve the set of dictionaries containing possible letters as output
                    list_dictionaries = function(url = "https://transkribus.eu/TrpServer/rest/recognition/dicts"){
                      res  <- httr::GET(url = "https://transkribus.eu/TrpServer/rest/recognition/dicts", httr::add_headers(JSESSIONID = self$JSESSIONID))
                      msg  <- httr::content(res, as = "text")
                      info <- strsplit(msg, "\n")
                      info <- unlist(info)
                      info
                    },
                    #' @description Retrieve all HTR/OCR models you have access to within a collection
                    list_models = function(url = "https://transkribus.eu/TrpServer/rest/recognition/%s/list", collection){
                      res  <- httr::GET(url = sprintf(url, collection), httr::add_headers(JSESSIONID = self$JSESSIONID), httr::timeout(5*60))
                      msg  <- httr::content(res, as = "text")
                      info <- jsonlite::fromJSON(msg)
                      info
                    },
                    #' @description List all jobs or get the information of one specific job
                    #' @param job id of the job
                    list_job = function(url = "https://transkribus.eu/TrpServer/rest/jobs/list", job){
                      if(!missing(job)){
                        url <- gsub(pattern = "list$", replacement = job, url)
                      }
                      res  <- httr::GET(url = url, httr::add_headers(JSESSIONID = self$JSESSIONID), httr::timeout(5*60))
                      msg  <- httr::content(res, as = "text")
                      info <- jsonlite::fromJSON(msg)
                      if("createTime" %in% names(info)){
                        info$createTime <- as.POSIXct(info$createTime / 1000, origin = "1970-01-01")
                        if(is.data.frame(info)){
                          info <- info[order(info$createTime, decreasing = TRUE), ]  
                        }
                      }
                      if("startTime" %in% names(info)){
                        info$startTime <- as.POSIXct(info$startTime / 1000, origin = "1970-01-01")
                      }
                      if("endTime" %in% names(info)){
                        info$endTime <- as.POSIXct(info$endTime / 1000, origin = "1970-01-01")
                      }
                      info
                    },
                    #' @description Transcribe a set of pages with a model
                    #' @param page id of the page to transcribe
                    #' @param model id of the Transkribus model to use
                    #' @param dictionary character string with the dictionary (set of letters) to use
                    transcribe = function(url = "https://transkribus.eu/TrpServer/rest/recognition/{collection}/{model}/htrCITlab?id={document}&pages={page}&dict={dictionary}",
                                          collection,
                                          document,
                                          page,
                                          model,
                                          dictionary){
                      if(missing(page)){
                        url <- gsub(url, pattern = "&pages=\\{page\\}", replacement = "")
                      }
                      #collection_id <- 123580
                      #htr_id <- 37851
                      #htr_id <- 15708
                      #doc_id <- 817369
                      #dictionary_filename <- "Combined_Dutch_Model_M1.dict"
                      #page_string <- "30554336"
                      #page_string <- 1
                      #qry <- glue("https://transkribus.eu/TrpServer/rest/recognition/{collection_id}/{htr_id}/htrCITlab?id={doc_id}&pages={page_string}&dict={dictionary_filename}")
                      qry  <- glue(url)
                      res  <- httr::POST(url = qry, httr::add_headers(JSESSIONID = self$JSESSIONID))
                      msg  <- httr::content(res, as = "text")
                      msg
                    }
                  )
)
