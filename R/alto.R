

#' @title Read Alto-XML file 
#' @description Import Alto-XML file as a data.frame. Examples where this can be used
#' is for importing alto-xml files from Transkribus
#' @param x path to the xml file
#' @param type character string with the type of xml structure. Defaults to 'transkribus' and is currently the only option.
#' @param ... further arguments currently not used
#' @return a data.frame
#' @export
#' @note the function only handles single-page XML's
#' @examples 
#' f <- system.file(package = "madoc.utils", "extdata", "alto-example.xml")
#' x <- read_alto(f)
#' f <- system.file(package = "madoc.utils", "extdata", "altoxml-example.xml")
#' x <- read_alto(f)
#' f <- system.file(package = "madoc.utils", "extdata", "multiregion-alto.xml")
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
    content <- setNames(content, sapply(content, attr, which = "ID"))
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
    content <- data.table::rbindlist(content, fill = TRUE, idcol = "textblock")  
    content
  })
  info <- data.table::rbindlist(info, fill = TRUE)
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
#' @note the function only handles single-page XML's
#' @examples 
#' f <- system.file(package = "madoc.utils", "extdata", "pagexml-example.xml")
#' x <- read_pagexml(f)
#' f <- system.file(package = "madoc.utils", "extdata", "multiregion-page.xml")
#' x <- read_pagexml(f)
read_pagexml <- function(x, type = c("transkribus"), ...){
  type <- match.arg(type)
  path <- x
  x    <- read_xml(path, as_html = FALSE)
  info <- xml_children(x)
  info <- info[xml_name(info) %in% "Page"]
  info <- xml_children(info)
  info <- info[xml_name(info) %in% "TextRegion"]
  info <- lapply(info, FUN = function(x){
    textregion <- xml_attr(x, "id")
    info <- xml_children(x)
    info <- lapply(info, FUN = function(x){
      content <- as_list(x)
      if("id" %in% names(attributes(content))){
        d <- list(id       = attributes(content)$id,
                  coords   = attributes(content$Coords)$points,
                  baseline = attributes(content$Baseline)$points)  
      }else{
        d <- attributes(content)  
      }
      d
    })
    info          <- data.table::rbindlist(info, fill = TRUE)
    info$coords   <- coords_xy(info$coords)
    info$baseline <- coords_xy(info$baseline)
    if("points" %in% colnames(info)){
      info$points <- coords_xy(info$points)
    }
    info$file <- rep(basename(path), nrow(info))
    info <- data.table::setDF(info)
    info$textregion <- rep(textregion, nrow(info))
    info
  })
  info <- data.table::rbindlist(info, fill = TRUE)
  info <- setDF(info)
  f <- c("file", "textregion", "id", "coords", "baseline")
  f <- intersect(f, colnames(info))
  if(length(f) > 0){
    info <- info[, c(f, setdiff(colnames(info), f))]  
  }
  info
}



#' @title Parse coords of points
#' @description Parse coords of points where the list of coordinates is available in a string
#' as follows: x1,y1 x2,y2, x3,y3
#' @param data a character vector or a list of character data
#' @param split which split character to use between x and y. Defaults to ','
#' @return a list of data.frames with columns x and y
#' @keywords internal
#' @export
#' @examples 
#' x <- c("0.1,0.2 0.3,0.15", "", NA, "1,2 3,4 5,6 1,0")
#' coords_xy(x)
#' x <- c("0.1,0.2 0.3,0.15", "", NA, "1,2 3,4 5,6 1,0")
#' x <- strsplit(x, " ")
#' coords_xy(x)
coords_xy <- function(data, split = ","){
  if(is.character(data)){
    data <- strsplit(data, " ")
  }
  out <- lapply(data, FUN = function(x){
    x <- strsplit(x, split = split) 
    x <- data.frame(x = as.numeric(sapply(x, FUN = function(x) x[1])),
                    y = as.numeric(sapply(x, FUN = function(x) x[2])), stringsAsFactors = FALSE)
    x <- na.exclude(x)
    x
  })
  out
}


