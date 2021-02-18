
#' @title Crop out areas with text lines from an image
#' @description Crop out areas with text lines from an image
#' @param image \code{image} either an object of class \code{magick-image} or a path to an image file on disk
#' @param geometries a data.frame with columns width, height, x_left, y_top indicating the areas to extract from the image
#' @param color color to use for adding a border in the overview image. Defaults to 'royalblue'.
#' @param border border pixels to using in the overview image. Defaults to 10x10 pixel borders.
#' @return a list with elements areas and overview where \code{overview} is a \code{magick-image} with stacked image lines
#' and \code{areas} is a list of \code{magick-image}'s, one for each text line 
#' @export
#' @examples
#' library(magick) 
#' library(data.table)
#' path     <- system.file(package = "madoc.utils", "extdata", "alto-example.xml")
#' x        <- read_alto(path)
#' x$width  <- as.integer(x$WIDTH)
#' x$height <- as.integer(x$HEIGHT)
#' x$x_left <- as.integer(x$HPOS)
#' x$y_top  <- as.integer(x$VPOS)
#' 
#' img      <- system.file(package = "madoc.utils", "extdata", "alto-example.jpg")
#' img      <- image_read(img)
#' areas    <- image_crop_textlines(img, x, color = "red")
#' areas$overview
#' areas$areas
image_crop_textlines <- function(image, geometries, color = "royalblue", border = "10x10"){
  stopifnot(is.data.frame(geometries) && all(c("width", "height", "x_left", "y_top") %in% colnames(geometries)))
  db <- geometries
  if(inherits(image, "magick-image")){
    img <- image
  }else{
    image <- as.character(image)
    stopifnot(file.exists(image))
    img <- image_read(image)
  }
  txtlines  <- db
  areas_img <- lapply(seq_len(nrow(txtlines)), FUN=function(i){
    location <- txtlines[i, ]
    areas <- geometry_area(width = location$width, height = location$height, 
                           x_off = location$x_left, y_off = location$y_top)
    image_crop(img, geometry = areas)
  })
  #image_append(do.call(c, lapply(areas_img,image_border, "white", "10x10")), stack = TRUE)
  #image_append(do.call(c, lapply(areas_img,image_border, "#000080", "10x10")), stack = TRUE)
  #image_append(do.call(c, lapply(areas_img,image_border, "royalblue", "10x10")), stack = TRUE)
  list(areas = areas_img, 
       overview = image_append(do.call(c, lapply(areas_img, image_border, color, border)), stack = TRUE))
}


#' @title Stack images below one another
#' @description Stack images below one another
#' @param image either an object of class \code{magick-image} or a character vector of files
#' @return an object of class \code{magick-image} where all images are put below one another
#' @export
#' @examples
#' library(magick) 
#' library(data.table)
#' path     <- system.file(package = "madoc.utils", "extdata", "alto-example.xml")
#' x        <- read_alto(path)
#' x$width  <- as.integer(x$WIDTH)
#' x$height <- as.integer(x$HEIGHT)
#' x$x_left <- as.integer(x$HPOS)
#' x$y_top  <- as.integer(x$VPOS)
#' 
#' img      <- system.file(package = "madoc.utils", "extdata", "alto-example.jpg")
#' img      <- image_read(img)
#' areas    <- image_crop_textlines(img, x, color = "red")
#' areas$overview
#' areas$areas
#' 
#' all      <- image_rbind(areas$areas)
#' all
#' all      <- do.call(c, areas$areas)
#' all
image_rbind <- function(image){
  x <- image
  if(inherits(x, "magick-image")){
    image_append(x, stack = TRUE)
  }else if(is.list(x)){
    image_append(do.call(c, x), stack = TRUE)
  }else{
    stopifnot(all(file.exists(x)))
    image_append(image_read(x), stack = TRUE)
  }
}