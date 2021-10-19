
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
  txtlines  <- txtlines[, c("width", "height", "x_left", "y_top")]
  txtlines  <- stats::na.exclude(txtlines)
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







#' @title Crop out areas with text polygons from an image
#' @description Crop out areas with text polygons from an image
#' @param image \code{image} either an object of class \code{opencv-image} or a path to an image file on disk
#' @param geometries a data.frame with columns width, height, x_left, y_top indicating the areas to extract from the image
#' @param color color to use for adding a border in the overview image. Defaults to 'royalblue'.
#' @param border border pixels to using in the overview image. Defaults to 10x10 pixel borders.
#' @return a list with elements areas and overview where \code{overview} is a \code{magick-image} with stacked image lines
#' and \code{areas} is a list of \code{magick-image}'s, one for each text line 
#' @export
#' @examples
#' library(opencv) 
#' library(data.table)
#' path     <- system.file(package = "madoc.utils", "extdata", "pagexml-example.xml")
#' x        <- read_pagexml(path)
#' 
#' img      <- system.file(package = "madoc.utils", "extdata", "pagexml-example.jpg")
#' img      <- ocv_read(img)
#' area     <- ocv_polygon(img, pts = x$coords[[5]])
#' areas    <- image_crop_textpolygons(img, x, color = "red")
#' areas$overview
#' areas$areas
image_crop_textpolygons <- function(image, geometries, color = "royalblue", border = "10x10"){
  if(!requireNamespace("opencv")){
    stop("In order to use image_crop_textpolygons, install R package opencv from CRAN")
  }
  stopifnot(is.data.frame(geometries) && all(c("coords", "baseline") %in% colnames(geometries)))
  db <- geometries
  if(inherits(image, "opencv-image")){
    img <- image
  }else{
    image <- as.character(image)
    stopifnot(file.exists(image))
    img <- opencv::ocv_read(image)
  }
  txtlines  <- db
  txtlines  <- txtlines[which(sapply(txtlines$coords, nrow) > 0), ]
  areas_img <- lapply(seq_len(nrow(txtlines)), FUN=function(i){
    location <- txtlines[i, ]
    pts      <- location$coords[[1]]
    area     <- opencv::ocv_polygon(img, pts)
    area     <- opencv::ocv_bbox(area, pts)
    area     <- opencv::ocv_bitmap(area)
    area     <- magick::image_read(area)
    area
  })
  #image_append(do.call(c, lapply(areas_img,image_border, "white", "10x10")), stack = TRUE)
  #image_append(do.call(c, lapply(areas_img,image_border, "#000080", "10x10")), stack = TRUE)
  #image_append(do.call(c, lapply(areas_img,image_border, "royalblue", "10x10")), stack = TRUE)
  list(areas = areas_img, 
       overview = image_append(do.call(c, lapply(areas_img, image_border, color, border)), stack = TRUE))
}



#' @title Draw baselines on an image
#' @description Draw baselines on an image
#' @param image \code{image} either an object of class \code{magick-image} or a path to an image file on disk
#' @param x a list vector where each list element contains columns x and y indicating the positions of the baseline
#' @param ... further arguments passed on to \code{\link{lines}}
#' @return a \code{magick-image}
#' @export
#' @examples
#' library(magick)
#' path     <- system.file(package = "madoc.utils", "extdata", "pagexml-example.xml")
#' x        <- read_pagexml(path)
#' x
#' 
#' img      <- system.file(package = "madoc.utils", "extdata", "pagexml-example.jpg")
#' img      <- image_read(img)
#' plt      <- image_draw_baselines(img, x$baseline)
#' plt
#' plt      <- image_draw_baselines(img, x$baseline, col = "red", lwd = 10, lty = 2)
#' plt
image_draw_baselines <- function(image, x, ...){
  if(inherits(image, "magick-image")){
    img <- image
  }else{
    image <- as.character(image)
    stopifnot(file.exists(image))
    img <- image_read(image)
  }
  
  plt <- image_draw(img)
  lapply(x, FUN = function(l){
    if("x" %in% names(l) & length(l$x) > 0){
      lines(l$x, l$y, ...) 
    }
  })
  invisible(dev.off())
  plt
}





#' @title Extract areas between baselines
#' @description Extract areas between baselines
#' @param image \code{image} either an object of class \code{opencv-image} or a path to an image file on disk
#' @param x a list vector where each list element contains columns x and y indicating the positions of the baseline
#' @param extend logical indicating to extend the baseline to the left and right of the image. Defaults to TRUE.
#' @param color color to use for adding a border in the overview image. Defaults to 'royalblue'.
#' @param border border pixels to using in the overview image. Defaults to 10x10 pixel borders.
#' @param ... further arguments currently not used
#' @return a list with elements areas and overview where \code{overview} is a \code{magick-image} with stacked image lines
#' and \code{areas} is a list of \code{magick-image}'s, one for each text line 
#' @export
#' @examples 
#' library(opencv)
#' library(magick)
#' path     <- system.file(package = "madoc.utils", "extdata", "pagexml-example.xml")
#' x        <- read_pagexml(path)
#' x
#' img      <- system.file(package = "madoc.utils", "extdata", "pagexml-example.jpg")
#' img      <- ocv_read(img)
#' areas    <- image_crop_baselineareas(img, x = x$baseline, extend = FALSE)
#' areas$areas
#' image_resize(areas$overview, "x600")
#' areas    <- image_crop_baselineareas(img, x = x$baseline, extend = TRUE, color = "red")
#' image_resize(areas$overview, "x600")
image_crop_baselineareas <- function(image, x, extend = TRUE, color = "royalblue", border = "10x10", ...){
  if(!requireNamespace("opencv")){
    stop("In order to use image_crop_baselineareas, install R package opencv from CRAN")
  }
  if(inherits(image, "opencv-image")){
    img <- image
  }else{
    image <- as.character(image)
    stopifnot(file.exists(image))
    img <- opencv::ocv_read(image)
  }
  width  <- opencv::ocv_info(img)$width
  height <- opencv::ocv_info(img)$height
  if(extend){
    x <- lapply(x, extend_baselines, width = width - 1, height = height - 1)
  }
  polylines <- x
  polylines <- polylines[which(sapply(polylines, FUN = function(x) is.data.frame(x) && nrow(x) > 0))]
  for(i in rev(seq_len(length(polylines)))){
    pts <- polylines[[i]]
    if(i == 1){
      left  <- 0
      right <- width - 1
      left  <- min(pts$x)
      right <- max(pts$x)
      previous <- data.frame(x = c(left, right),
                             y = c(0, 0))
    }else{
      previous <- polylines[[i-1]]
    }
    pts <- rbind(previous, pts[order(rev(seq_len(nrow(pts)))), ])
    pts$y <- round(pts$y, digits = 0)
    polylines[[i]] <- pts
  }
  
  areas_img <- lapply(seq_len(length(polylines)), FUN=function(i){
    pts      <- polylines[[i]]
    area     <- opencv::ocv_polygon(img, pts)
    area     <- opencv::ocv_bbox(area, pts)
    area     <- opencv::ocv_bitmap(area)
    area     <- magick::image_read(area)
    area
  })
  #image_append(do.call(c, lapply(areas_img,image_border, "white", "10x10")), stack = TRUE)
  #image_append(do.call(c, lapply(areas_img,image_border, "#000080", "10x10")), stack = TRUE)
  #image_append(do.call(c, lapply(areas_img,image_border, "royalblue", "10x10")), stack = TRUE)
  list(areas = areas_img, 
       overview = image_append(do.call(c, lapply(areas_img, image_border, color, border)), stack = TRUE))
  
}

# 
# path     <- system.file(package = "madoc.utils", "extdata", "pagexml-example.xml")
# x        <- read_pagexml(path)
# img      <- system.file(package = "madoc.utils", "extdata", "pagexml-example.jpg")
# img      <- ocv_read(img)
# width  = ocv_info(img)$width
# height = ocv_info(img)$height
# x$baseline2 <- lapply(x$baseline, extend_baselines, width = width, height = height)
# 
# img      <- system.file(package = "madoc.utils", "extdata", "pagexml-example.jpg")
# img      <- image_read(img)
# plt      <- image_draw_baselines(img, x$baseline2, col = "red", lwd = 10, lty = 2)
# plt

extend_baselines <- function(pts, width, height){
  if(nrow(pts) < 2){
    extended <- data.frame(x = integer(), y = integer())
    extended <- rbind(extended, pts)
    return(extended)
  }
  horizontaal <- pts$x
  vertikaal   <- pts$y
  m           <- lm(y ~ x, data = pts)
  extended    <- data.frame(x = c(0, width))
  extended$y  <- predict(m, newdata = extended)
  extended$y  <- ifelse(extended$y < 0, 0, extended$y)
  extended$y  <- ifelse(extended$y > height, height, extended$y)
  extended    <- rbind(extended, pts)
  extended    <- extended[order(extended$x, decreasing = FALSE), ]
  extended
}


#' @title Extract a polygonal region 
#' @description Extract a polygonal region, optionally limiting the extracting to the bounding box
#' @param image \code{image} either an object of class \code{opencv-image} or a path to an image file on disk
#' @param x a list vector where each list element contains columns x and y indicating the positions of the points of the polygon
#' or a single data.frame with columns \code{x} and \code{y}
#' @param bbox logical indicating to limit the result to the bounding box of the polygon
#' @param ... further arguments currently not used
#' @return a \code{magick-image} or a list of \code{magick-image} objects
#' @export
#' @examples
#' library(opencv)
#' path     <- system.file(package = "madoc.utils", "extdata", "pagexml-example.xml")
#' x        <- read_pagexml(path)
#' x
#' img      <- system.file(package = "madoc.utils", "extdata", "pagexml-example.jpg")
#' img      <- ocv_read(img)
#' pts      <- x$points[[1]]
#' area     <- image_crop_area(img, pts, bbox = FALSE)
#' area
#' area     <- image_crop_area(img, pts, bbox = TRUE)
#' area
#' areas    <- image_crop_area(img, x$points, bbox = FALSE)
image_crop_area <- function(image, x, bbox = FALSE, ...){
  if(!requireNamespace("opencv")){
    stop("In order to use image_crop_area, install R package opencv from CRAN")
  }
  #stopifnot(is.data.frame(x) && all(c("x", "y") %in% colnames(x)))
  if(inherits(image, "opencv-image")){
    img <- image
  }else{
    image <- as.character(image)
    stopifnot(file.exists(image))
    img <- opencv::ocv_read(image)
  }
  pts <- x
  if(!is.data.frame(pts)){
    area <- lapply(pts, FUN = function(ptset){
      image_crop_area(img, ptset, bbox = bbox, ...)
    })
  }else{
    if(nrow(pts) > 0){
      area     <- opencv::ocv_polygon(img, pts)
      if(bbox){
        area     <- opencv::ocv_bbox(area, pts) 
      }
      area     <- opencv::ocv_bitmap(area)
      area     <- magick::image_read(area)   
    }else{
      area     <- NULL
    }
  }
  area
}