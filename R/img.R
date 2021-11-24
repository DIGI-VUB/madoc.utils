
#' @title Crop out areas with text lines from an image
#' @description Crop out areas with text lines from an image
#' @param image \code{image} either an object of class \code{magick-image} or a path to an image file on disk
#' @param geometries a data.frame with columns width, height, x_left, y_top indicating the areas to extract from the image
#' @param color color to use for adding a border in the overview image. Defaults to 'royalblue'.
#' @param border border pixels to using in the overview image. Defaults to 10x10 pixel borders.
#' @param overview logical indicating to add the overview image of all area's below each other. Defaults to TRUE.
#' @param max_width maximum width of the overview image. Defaults to +Inf
#' @param trace logical indicating to trace progress
#' @return a list with elements areas and overview where \code{overview} is a \code{magick-image} with stacked image lines
#' and \code{areas} is a list of \code{magick-image}'s, one for each text line. \cr
#' In case overview is set to \code{FALSE} the return value is only the list of stacked image lines.
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
#' x           <- subset(x, grepl(ID, pattern = "line"))
#' rownames(x) <- x$ID
#' 
#' img      <- system.file(package = "madoc.utils", "extdata", "alto-example.jpg")
#' img      <- image_read(img)
#' areas    <- image_crop_textlines(img, x, color = "red")
#' areas$overview
#' areas$areas
image_crop_textlines <- function(image, geometries, color = "royalblue", border = "10x10", overview = TRUE, max_width = +Inf, trace = FALSE){
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
  txtlines  <- txtlines[!is.na(txtlines$width) & !is.na(txtlines$height) & !is.na(txtlines$x_left) & !is.na(txtlines$y_top), ]
  #txtlines  <- stats::na.exclude(txtlines)
  areas_img <- lapply(seq_len(nrow(txtlines)), FUN=function(i){
    location <- txtlines[i, ]
    areas <- geometry_area(width = location$width, height = location$height, 
                           x_off = location$x_left, y_off = location$y_top)
    image_crop(img, geometry = areas)
  })
  if(!is.null(rownames(txtlines))){
    names(areas_img) <- rownames(txtlines)  
  }
  
  #image_append(do.call(c, lapply(areas_img,image_border, "white", "10x10")), stack = TRUE)
  #image_append(do.call(c, lapply(areas_img,image_border, "#000080", "10x10")), stack = TRUE)
  #image_append(do.call(c, lapply(areas_img,image_border, "royalblue", "10x10")), stack = TRUE)
  if(!overview){
    return(areas_img)
  }
  #overview <- image_append(do.call(c, lapply(areas_img, image_border, color, border)), stack = TRUE)
  overview <- image_merge_to_one(areas_img, color = color, border = border, trace = trace, max_width = max_width)
  
  list(areas = areas_img, 
       overview = overview)
}


#' @title Stack images below one another
#' @description Stack images below one another
#' @param image either an object of class \code{magick-image} or a character vector of files
#' @param ... further arguments passed on to image_border in case you want to add a border around the image
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
#' all      <- image_rbind(areas$areas, color = "red", geometry = "10x10")
#' all
#' all      <- do.call(c, areas$areas)
#' all      <- image_rbind(all, color = "blue", geometry = "10x10")
#' all
image_rbind <- function(image, ...){
  x <- image
  ldots <- list(...)
  if(length(ldots) > 0){
    x <- lapply(x, FUN = image_border, ...)
  }
  if(inherits(x, "magick-image")){
    image_append(x, stack = TRUE)
  }else if(is.list(x)){
    image_append(do.call(c, x), stack = TRUE)
  }else{
    stopifnot(all(file.exists(x)))
    x <- image_read(x)
    image_rbind(x, ...)
  }
}







#' @title Crop out areas with text polygons from an image
#' @description Crop out areas with text polygons from an image
#' @param image \code{image} either an object of class \code{opencv-image} or a path to an image file on disk
#' @param geometries a data.frame with columns width, height, x_left, y_top indicating the areas to extract from the image
#' @param color color to use for adding a border in the overview image. Defaults to 'royalblue'.
#' @param border border pixels to using in the overview image. Defaults to 10x10 pixel borders.
#' @param overview logical indicating to add the overview image of all area's below each other. Defaults to TRUE.
#' @param max_width maximum width of the overview image. Defaults to +Inf
#' @param trace logical indicating to trace progress
#' @return a list with elements areas and overview where \code{overview} is a \code{magick-image} with stacked image lines
#' and \code{areas} is a list of \code{magick-image}'s, one for each text line \cr
#' In case overview is set to \code{FALSE} the return value is only the list of stacked image lines. 
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
image_crop_textpolygons <- function(image, geometries, color = "royalblue", border = "10x10", overview = TRUE, max_width = +Inf, trace = FALSE){
  if(!requireNamespace("opencv")){
    stop("In order to use image_crop_textpolygons, install R package opencv from CRAN")
  }
  stopifnot(is.data.frame(geometries) && all(c("coords", "baseline") %in% colnames(geometries)))
  db <- geometries
  if(inherits(image, "magick-image")){
    image <- magick_to_opencv(image)
  }
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
    if(trace){
      cat(sprintf("%s extracting area %s/%s", Sys.time(), i, length(txtlines)), sep = "\n")
    }
    location <- txtlines[i, ]
    pts      <- location$coords[[1]]
    area     <- opencv::ocv_polygon(img, pts, crop = TRUE)
    #area     <- opencv::ocv_bbox(area, pts)
    area     <- opencv::ocv_bitmap(area)
    area     <- magick::image_read(area)
    area
  })
  if(!is.null(rownames(txtlines))){
    names(areas_img) <- rownames(txtlines)  
  }
  #image_append(do.call(c, lapply(areas_img,image_border, "white", "10x10")), stack = TRUE)
  #image_append(do.call(c, lapply(areas_img,image_border, "#000080", "10x10")), stack = TRUE)
  #image_append(do.call(c, lapply(areas_img,image_border, "royalblue", "10x10")), stack = TRUE)
  if(!overview){
    return(areas_img)
  }
  #overview <- image_append(do.call(c, lapply(areas_img, image_border, color, border)), stack = TRUE)
  overview <- image_merge_to_one(areas_img, color = color, border = border, trace = trace, max_width = max_width)
  list(areas = areas_img, 
       overview = overview)
}


magick_to_opencv <- function(img){
  p <- tempfile()
  on.exit({
    if(file.exists(p)){
      file.remove(p) 
    }
  })
  magick::image_write(img, path = p)
  image <- opencv::ocv_read(p)
  image
}

image_merge_to_one <- function(areas_img, color = "royalblue", border = "10x10", trace = FALSE, max_width = +Inf){
  add_border <- function(image){
    if(image_info(image)$width > max_width){
      image <- image_resize(image, geometry = max_width) 
    }
    out <- image_border(image, color = color, geometry = border)
    out
  }
  for(i in seq_along(areas_img)){
    if(trace){
      cat(sprintf("%s combining area %s/%s", Sys.time(), i, length(areas_img)), sep = "\n")
    }
    if(i == 1){
      overview <- add_border(areas_img[[i]])    
    }else{
      overview <- image_append(c(overview, add_border(areas_img[[i]])), stack = TRUE)
    }
  }
  overview
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
      graphics::lines(l$x, l$y, ...) 
    }
  })
  invisible(dev.off())
  plt
}



#' @title Draw polygons on an image
#' @description Draw polygons on an image
#' @param image \code{image} either an object of class \code{magick-image} or a path to an image file on disk
#' @param x a list vector where each list element contains columns x and y indicating the positions of the polygon
#' @param ... further arguments passed on to \code{\link{polygon}}
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
#' plt      <- image_draw_polygons(img, x$coords, col = "#FF000080", border = "blue", lwd = 2)
#' plt
image_draw_polygons <- function(image, x, ...){
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
      graphics::lines(l$x, l$y, ...) 
    }
  })
  invisible(dev.off())
  plt
}





#' @title Extract areas between baselines
#' @description Extract areas between baselines
#' @param image \code{image} either an object of class \code{opencv-image} or a path to an image file on disk
#' @param x a list vector where each list element contains columns x and y indicating the positions of the baseline
#' @param textregion a list vector of the same length of \code{x} where each list element contains columns x and y indicating the positions of textregion. 
#' The extracted areas can not pass these boundaries
#' @param extend logical indicating to extend the baseline to the left and right of the image. Defaults to TRUE.
#' @param color color to use for adding a border in the overview image. Defaults to 'royalblue'.
#' @param border border pixels to using in the overview image. Defaults to 10x10 pixel borders.
#' @param overview logical indicating to add the overview image of all area's below each other. Defaults to TRUE.
#' @param max_width maximum width of the overview image. Defaults to +Inf
#' @param trace logical indicating to trace progress
#' @param ... further arguments currently not used
#' @return a list with elements areas and overview where \code{overview} is a \code{magick-image} with stacked image lines
#' and \code{areas} is a list of \code{magick-image}'s, one for each text line \cr
#' In case overview is set to \code{FALSE} the return value is only the list of stacked image lines. 
#' @export
#' @examples 
#' library(opencv)
#' library(magick)
#' path     <- system.file(package = "madoc.utils", "extdata", "pagexml-example.xml")
#' x        <- read_pagexml(path)
#' x
#' img      <- system.file(package = "madoc.utils", "extdata", "pagexml-example.jpg")
#' img      <- ocv_read(img)
#' areas    <- image_crop_baselineareas(img, x = x$baseline, extend = FALSE, trace = TRUE)
#' areas$areas
#' image_resize(areas$overview, "x600")
#' areas    <- image_crop_baselineareas(img, x = x$baseline, extend = TRUE, color = "red")
#' image_resize(areas$overview, "x600")
#' 
#' ## Multiple regions
#' path     <- system.file(package = "madoc.utils", "extdata", "multiregion-page.xml")
#' x        <- read_pagexml(path)
#' x
#' img      <- system.file(package = "madoc.utils", "extdata", "multiregion.jpg")
#' img      <- ocv_read(img)
#' areas    <- image_crop_baselineareas(img, 
#'                                      x = x$baseline, textregion = x$points, 
#'                                      extend = TRUE, overview = FALSE)
#' overview <- image_rbind(areas, color = "grey", geometry = "5x5")
#' image_resize(overview, "600")                                      
image_crop_baselineareas <- function(image, x, textregion, extend = TRUE, color = "royalblue", border = "10x10", overview = TRUE, max_width = +Inf, trace = FALSE, ...){
  if(!requireNamespace("opencv")){
    stop("In order to use image_crop_baselineareas, install R package opencv from CRAN")
  }
  if(inherits(image, "magick-image")){
    image <- magick_to_opencv(image)
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
  msg <- lapply(x, FUN = function(pts){
    pts$outofrange <- pts$x < 0 | pts$x >= width | pts$y < 0 | pts$y >= height
    pts
  })
  idx <- which(sapply(msg, FUN = function(pts) any(pts$x < 0 | pts$x >= width | pts$y < 0 | pts$y >= height)))
  if(length(idx) > 0){
    msg <- msg[idx]
    msg <- mapply(msg, idx, FUN = function(x, i){
      x <- x[which(x$outofrange), ]
      paste(sprintf("%s:(%s, %s)", i, x$x, x$y), collapse = " ")
    }, SIMPLIFY = TRUE, USE.NAMES = TRUE)
    msg <- paste(msg, collapse = "; ")
    warning(sprintf("Found unexpected baseline x/y values not within expected range (0 - %sx%s)\n %s", width, height, msg))
    x <- lapply(x, FUN = function(pts){
      pts$x <- ifelse(pts$x < 0, 0, pts$x)
      pts$x <- ifelse(pts$x >= width, width - 1, pts$x)
      pts$y <- ifelse(pts$y < 0, 0, pts$y)
      pts$y <- ifelse(pts$y >= height, height - 1, pts$y)
      pts
    })
  }
  
  if(extend){
    x <- lapply(x, extend_baselines, width = width - 1, height = height - 1)
  }
  polylines <- x
  idx_ok <- which(sapply(polylines, FUN = function(x) is.data.frame(x) && nrow(x) > 0))
  polylines <- polylines[idx_ok]
  missing_textregion <- missing(textregion)
  if(!missing_textregion){
    textregion <- textregion[idx_ok]
  }
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
  coords <- function(obj){
    all <- lapply(obj@polygons, FUN = function(x){
      co <- lapply(x@Polygons, FUN = sp::coordinates)
      co <- do.call(rbind, co)
      co
    })
    all <- do.call(rbind, all)
    colnames(all) <- c("x", "y")
    all
  }  
  areas_img <- lapply(seq_len(length(polylines)), FUN=function(i){
    if(trace){
      cat(sprintf("%s area %s/%s", Sys.time(), i, length(polylines)), sep = "\n")
    }
    pts      <- polylines[[i]]
    if(!missing_textregion){
      textpolygon <- textregion[[i]]
      #a <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(coords = pts)), ID = "baseline")))
      #b <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(coords = textpolygon)), ID = "textregion")))
      #area <- rgeos::gIntersection(a, b)
      #a    <- sf::st_as_sf(a)
      #b    <- sf::st_as_sf(b)
      # a    <- sf::st_polygon(list(as.matrix(pts)))
      # b    <- sf::st_polygon(list(as.matrix(textpolygon)))
      # a    <- sf::st_make_valid(a)
      # b    <- sf::st_make_valid(b)
      # area <- sf::st_intersection(a, b)
      # area <- sf::as_Spatial(area)
      
      a <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(coords = pts)), ID = "baseline")))
      b <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(coords = textpolygon)), ID = "textregion")))
      #area <- rgeos::gIntersection(a, b)
      a    <- sf::st_as_sf(a)
      a    <- sf::st_make_valid(a)
      b    <- sf::st_as_sf(b)
      b    <- sf::st_make_valid(b)
      area <- sf::st_intersection(a, b)
      area <- sf::st_convex_hull(area)
      area <- sf::as_Spatial(area)
      pts  <- coords(area)
      pts  <- list(x = pts[, "x"], y = pts[, "y"])
    }
    area     <- opencv::ocv_polygon(img, pts, crop = TRUE)
    #area     <- opencv::ocv_bbox(area, pts)
    area     <- opencv::ocv_bitmap(area)
    area     <- magick::image_read(area)
    area
  })
  names(areas_img) <- names(polylines)
  #image_append(do.call(c, lapply(areas_img,image_border, "white", "10x10")), stack = TRUE)
  #image_append(do.call(c, lapply(areas_img,image_border, "#000080", "10x10")), stack = TRUE)
  #image_append(do.call(c, lapply(areas_img,image_border, "royalblue", "10x10")), stack = TRUE)
  if(!overview){
    return(areas_img)
  }
  #overview <- image_append(do.call(c, lapply(areas_img, image_border, color, border)), stack = TRUE)
  overview <- image_merge_to_one(areas_img, color = color, border = border, trace = trace, max_width = max_width)
  list(areas = areas_img, 
       overview = overview)
  
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
    if(nrow(pts) == 1){
      extended <- pts
      extended$y  <- ifelse(extended$y < 0, 0, extended$y)
      extended$y  <- ifelse(extended$y > height, height, extended$y)
      extended$x  <- ifelse(extended$x < 0, 0, extended$x)
      extended$x  <- ifelse(extended$x > width, width, extended$x)
    }else{
      extended <- data.frame(x = integer(), y = integer())
      #extended <- rbind(extended, pts)  
    }
    return(extended)
  }
  horizontaal <- pts$x
  vertikaal   <- pts$y
  m           <- lm(y ~ x, data = pts)
  extended    <- data.frame(x = c(0, width))
  extended$y  <- predict(m, newdata = extended)
  extended    <- rbind(head(extended, n = 1),
                       pts,
                       tail(extended, n = 1))
  extended$y  <- ifelse(extended$y < 0, 0, extended$y)
  extended$y  <- ifelse(extended$y > height, height, extended$y)
  extended$x  <- ifelse(extended$x < 0, 0, extended$x)
  extended$x  <- ifelse(extended$x > width, width, extended$x)
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
      area     <- opencv::ocv_polygon(img, pts, crop = bbox)
      # if(bbox){
      #   area     <- opencv::ocv_bbox(area, pts) 
      # }
      area     <- opencv::ocv_bitmap(area)
      area     <- magick::image_read(area)   
    }else{
      area     <- NULL
    }
  }
  area
}

