
#' Insert your own image and make it animated.
#'
#' @param src A url or path of image/gif to animate.
#' @param size Size of the icon relative to font size. Options are lg (133 percent), xs (75 percent), sm (87.5 percent) or a positive number.
#' @param height Height of the image that can be specified in percentage or pixels. This cannot be defined if size or grow is defined.
#' @param width Width of the image that can be specified in percentage or pixels. This cannot be defined if size or grow is defined.
#' @param position A numerical vector of length 4 specifying the value to move up, down, left and right. NOT IMPLEMENTED YET.
#' @param grow Numerical value to grow (or shrink for negative values) without changing or moving the container.
#' @param animate 'bounce', 'burst', 'falling', 'flash', 'float', 'horizontal',
#' 'passing', 'passing-reverse', 'pulse', 'ring', 'shake', 'spin',
#' 'tada', 'vertical', 'wrench', or use FALSE not to animate.
#' @param speed 'normal', 'fast', or 'slow'
#' @param anitype 'repeat', 'hover', or 'parent-hover'
#' @param rtext The text to be inserted on the right side.
#' @param ltext The text to be inserted on the left side.
#' @param rotate Numerical value for degree of rotation.
#' @param flip 'none', 'horizontal', 'vertical'.
#' @param border If TRUE, draws a border around the icon.
#' @param bgcolor,bgcolour Colour to be given to the background
#' @param sother Character vector of other parameters directly added to the style classes.
#'
#' @references [Font awesome animation](https://l-lin.github.io/font-awesome-animation/)
#'
#' @export
cia <- function(src, height = NULL, width = NULL, size = 1, position = c(0, 0, 0, 0),
                grow = 0, animate = "flash", speed = "normal", anitype = "repeat",
                rotate = 0, flip = "none", rtext = "", ltext = "", border = FALSE,
                bgcolor = NULL, bgcolour = bgcolor, iother = NULL, sother = NULL, dother = NULL) {

  d1 <- htmltools::htmlDependency("fontawesome", "5.0.13", src = system.file("fontawesome/font-awesome-5.0.13",
                                                                             package = "anicon"), stylesheet = "css/fontawesome-all.min.css")
  d2 <- htmltools::htmlDependency("fontawesome", "5.0.13", src = system.file("fontawesome",
                                                                             package = "anicon"), stylesheet = "fa-svg-with-js.css")
  d3 <- htmltools::htmlDependency("fontawesome", "5.0.13", src = system.file("fontawesome/font-awesome-5.0.13",
                                                                             package = "anicon"), script = "js/fontawesome-all.min.js")
  d4 <- htmltools::htmlDependency("font-awesome-animation", "1.0", src = system.file("animation",
                                                                                     package = "anicon"), stylesheet = "font-awesome-animation-emi.css")

  if (!missing(size) & (!is.null(height) | !is.null(width)))
    warning("Arguments height and width are ignored if size is defined.")
  if (!missing(size) & !missing(grow))
    warning("Arguments size is ignored if growth is defined.")

  img_height <- "1em"
  img_width <- "auto"
  if (!is.null(height))
    img_height <- height
  if (!is.null(width))
    img_width <- width
  if (!missing(size)) {
    img_width <- "auto"
    img_height <- switch(as.character(size), lg = "1.33em",
                         xs = "0.75em", sm = "0.875em", paste0(size, "em"))
  }
  if (!missing(grow)) {
    img_width <- "auto"
    img_height <- paste0(1 + grow * 0.0625, "em")
  }

  x <- structure(list(src = src, options = list(position = position, rtext=rtext, ltext=ltext,
                                                animate = animate, anitype = anitype, grow = !missing(grow), rotate = rotate,
                                                height = img_height, width = img_width, flip = flip, border = border,
                                                speed = speed, bgcolour = bgcolour, sother=sother)),
                 class = c("animg", "icon"))

  header <- htmltools::tags$head(d4, d3, d1, d2)  # order matters here

  out <- htmltools::tagList(header, get_imgtag(x))
  class(out) <- c("anicon", class(out))
  out
}

img_style <- function(x) {
  out <- paste0("height:", x$options$height, ";", " width:", x$options$width, "; ")
  if (x$options$flip != "none") {
    out <- paste0(out, switch(x$options$flip, horizontal = "transform: scaleX(-1); -webkit-transform: scaleX(-1); ",
                              vertical = "transform: scaleY(-1); -webkit-transform: scaleY(-1); "))
  }
  out
}


str_img <- function(x) {
  out <- NULL
  if (x$options$animate != FALSE) {
    anim_append <- paste0(" faa-", x$options$animate)
    out <- paste0(out, switch(x$options$anitype,
                              `repeat` = paste(anim_append, "animated "),
                              hover = paste(anim_append, "animated-hover "),
                              `parent-hover` = paste0(anim_append," ")))

    out <- paste0(out, switch(x$options$speed, normal = "", fast = "faa-fast ",
                              slow = "faa-slow "))
  }

  if (x$options$border)
    out <- paste0(out, "fa-border")
  if (!is.null(x$options$iother)) {
    out <- paste(out, paste(x$options$iother, collapse = " "))
  }
  out
}

get_imgtag <- function(x) {
  if(x$options$grow) {
    imgtag <- htmltools::img(src = x$src, style = img_style(x), align = "middle")
  } else {
    imgtag <- htmltools::img(src = x$src, style = img_style(x))
  }

  if (x$options$anitype == "parent-hover") {
    icon <- htmltools::span(imgtag,
                                 class = str_img(x),
                                 style = str_style(x))
    icontag <- htmltools::span(paste0(x$options$ltext, icon, x$options$rtext),
                                    class = "faa-parent animated-hover",
                                    style = "display:inline-block; display: -moz-inline-stack;")
  } else {
    icontag <- htmltools::span(paste0(x$options$ltext, imgtag, x$options$rtext),
                                    class = str_img(x),
                                    style = str_style(x))
  }
  icontag
}
