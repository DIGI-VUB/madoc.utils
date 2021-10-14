

#' @title Read Alto-XML file 
#' @description Import Alto-XML file as a data.frame. Examples where this can be used
#' is for importing alto-xml files from Transkribus
#' @param x path to the xml file
#' @param type character string with the type of xml structure. Defaults to 'transkribus' and is currently the only option.
#' @param ... further arguments currently not used
#' @return a data.frame
#' @export
#' @examples 
#' f <- system.file(package = "madoc.utils", "extdata", "alto-example.xml")
#' x <- read_alto(f)
#' f <- system.file(package = "madoc.utils", "extdata", "altoxml-example.xml")
#' x <- read_alto(f)
read_alto <- function(x, type = c("transkribus"), ...){
  type <- match.arg(type)
  path <- x
  x    <- read_xml(path, as_html = FALSE)
  info <- xml_children(x)
  info <- info[xml_name(info) %in% "Layout"]
  info <- xml_children(info)
  info <- lapply(info, FUN = function(x){
    content <- as_list(x)
    content <- content[["PrintSpace"]]
    content <- content[grepl(names(content), pattern = "TextBlock")]
    content <- lapply(content, FUN=function(textblock){
      if(FALSE){
        textblock <- unlist(textblock, recursive = FALSE, use.names = TRUE)
        textblock <- textblock[grepl(names(textblock), pattern = "TextLine")]
        textblock <- lapply(textblock, attributes)
        textblock <- data.table::rbindlist(textblock)  
        return(textblock)
      }
      att <- lapply(textblock, FUN = function(x){
        stringlines    <- lapply(x, attributes)
        stringlines    <- data.table::rbindlist(stringlines, fill = TRUE)
        line           <- attributes(x)
        line           <- setDF(line)
        colnames(line) <- paste(colnames(line), ".LINE", sep = "")
        line$.i        <- rep(TRUE, nrow(line))
        stringlines$.i <- rep(TRUE, nrow(stringlines))
        out            <- merge(stringlines, line, by = c(".i"), all = TRUE, suffixes = c("", ".line"))
        out$.i         <- NULL
        out
      })
      att <- data.table::rbindlist(att, fill = T)
      att <- data.table::setDF(att)
      f <- c("ID", "HEIGHT", "WIDTH", "VPOS", "HPOS", "CONTENT")
      f <- intersect(f, colnames(att))
      if(length(f) > 0){
        att <- att[, c(f, setdiff(colnames(att), f))]  
      }
      return(att)
    })
    content <- data.table::rbindlist(content)  
    content
  })
  info <- data.table::rbindlist(info)
  info
}







#' @title Read Page-XML file 
#' @description Import Page-XML file as a data.frame. Examples where this can be used
#' is for importing page-xml files from Transkribus
#' @param x path to the xml file
#' @param type character string with the type of xml structure. Defaults to 'transkribus' and is currently the only option.
#' @param ... further arguments currently not used
#' @return a data.frame with columns file, id, coords and baseline
#' @export
#' @examples 
#' f <- system.file(package = "madoc.utils", "extdata", "pagexml-example.xml")
#' x <- read_pagexml(f)
read_pagexml <- function(x, type = c("transkribus"), ...){
  type <- match.arg(type)
  path <- x
  x    <- read_xml(path, as_html = FALSE)
  info <- xml_children(x)
  info <- info[xml_name(info) %in% "Page"]
  info <- xml_children(info)
  info <- info[xml_name(info) %in% "TextRegion"]
  info <- xml_children(info)
  info <- lapply(info, FUN = function(x){
    content <- as_list(x)
    d <- list(id       = attributes(content)$id,
              coords   = attributes(content$Coords)$points,
              baseline = attributes(content$Baseline)$points)
  })
  info        <- data.table::rbindlist(info)
  info$coords <- strsplit(info$coords, " ")
  info$coords <- lapply(info$coords, FUN = function(x){
    x <- strsplit(x, ",") 
    x <- data.frame(x = as.numeric(sapply(x, FUN = function(x) x[1])),
                    y = as.numeric(sapply(x, FUN = function(x) x[2])), stringsAsFactors = FALSE)
    x
  })
  info$baseline <- strsplit(info$baseline, " ")
  info$baseline <- lapply(info$baseline, FUN = function(x){
    x <- strsplit(x, ",") 
    x <- data.frame(x = as.numeric(sapply(x, FUN = function(x) x[1])),
                    y = as.numeric(sapply(x, FUN = function(x) x[2])), stringsAsFactors = FALSE)
    x
  })
  info$file <- rep(basename(path), nrow(info))
  info <- data.table::setDF(info)
  info <- info[, c("file", "id", "coords", "baseline")]
  info
}


