

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
    content <- lapply(content, FUN=function(content){
      content <- unlist(content, recursive = FALSE, use.names = TRUE)
      content <- content[grepl(names(content), pattern = "TextLine")]
      content <- lapply(content, attributes)
      content <- data.table::rbindlist(content)  
      content
    })
    content <- data.table::rbindlist(content)  
    content
  })
  info <- data.table::rbindlist(info)
  info
}