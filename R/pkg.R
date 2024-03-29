#' @importFrom data.table rbindlist setnames setDF dcast.data.table as.data.table
#' @importFrom xml2 read_xml xml_children xml_name as_list xml_attr
#' @importFrom magick geometry_area image_read image_crop image_append image_border image_draw image_info image_resize
#' @importFrom udpipe txt_collapse txt_recode
#' @importFrom httr GET POST PUT content upload_file DELETE
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom stats setNames lm predict
#' @importFrom graphics lines polygon
#' @importFrom grDevices dev.off
#' @importFrom utils head tail
#' @importFrom zoo na.locf
#' @importFrom rgeos gIntersection
#' @importFrom sp Polygon Polygons SpatialPolygons coordinates
#' @importFrom sf st_as_sf st_make_valid st_intersection st_convex_hull st_buffer as_Spatial 
#' @importFrom R6 R6Class
#' @importFrom glue glue
NULL