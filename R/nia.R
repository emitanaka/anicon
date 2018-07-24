
#' Insert animated text
#'
#' @param text A string to animate.
#' @param size Size of the icon relative to font size. Options are 1, lg (133 percent), xs (75 percent), sm (87.5 percent), 2, 3, 4, 5, 7 or 10.
#' @param position A numerical vector of length 4 specifying the value to move up, down, left and right.
#' @param grow Numerical value to grow (or shrink for negative values) without changing or moving the container.
#' @param fixed_width If TRUE, the icon is set to a fixed width
#' @param animate 'bounce', 'burst', 'falling', 'flash', 'float', 'horizontal',
#' 'passing', 'passing-reverse', 'pulse', 'ring', 'shake', 'spin',
#' 'tada', 'vertical', or 'wrench'
#' @param speed 'normal', 'fast', or 'slow'
#' @param anitype 'repeat' or 'hover'
#' @param rotate Numerical value for degree of rotation.
#' @param flip 'none', 'horizontal', 'vertical'.
#' @param border If TRUE, draws a border around the icon.
#' @param iother Character vector of other parameters directly added to the icon classes, e.g. fa-pull-left, fa-pull-right.
#' @param sother Character vector of other parameters directly added to the style classes.
#' @param dother Character vector of other parameters directly added to the data-fa-transform classes, e.g. shrink-8, grow-2
#' @param color,colour Colour to be given to the icon
#' @param bgcolor,bgcolour Colour to be given to the background
#'
#' @references [Font awesome animation](https://l-lin.github.io/font-awesome-animation/)
#'
#' @export
nia <- function(text = "anicon", size = 1, position = c(0, 0, 0, 0), grow = 0,
                fixed_width = FALSE, animate = "flash", speed = "normal", anitype = "repeat",
                rotate = 0, flip = "none", border = FALSE, color = NULL, colour = color,
                bgcolor = NULL, bgcolour = bgcolor, iother = NULL, sother = NULL, dother = NULL) {

  d1 <- htmltools::htmlDependency("fontawesome", "5.0.13", src = system.file("fontawesome/font-awesome-5.0.13",
                                                                             package = "anicon"), stylesheet = "css/fontawesome-all.min.css")
  d2 <- htmltools::htmlDependency("fontawesome", "5.0.13", src = system.file("fontawesome",
                                                                             package = "anicon"), stylesheet = "fa-svg-with-js.css")
  d3 <- htmltools::htmlDependency("fontawesome", "5.0.13", src = system.file("fontawesome/font-awesome-5.0.13",
                                                                             package = "anicon"), script = "js/fontawesome-all.min.js")
  d4 <- htmltools::htmlDependency("font-awesome-animation", "1.0", src = system.file("animation",
                                                                                     package = "anicon"), stylesheet = "font-awesome-animation-emi.css")


  x <- structure(list(text = text, options = list(size = size, fixed_width = fixed_width,
                                                  position = position, animate = animate, anitype = anitype, grow = grow,
                                                  rotate = rotate, flip = flip, border = border, speed = speed, iother = iother,
                                                  sother = sother, dother = dother, colour = colour, bgcolour = bgcolour)),
                 class = c("anitext", "icon"))

  header <- htmltools::tags$head(d4, d3, d1, d2)  # order matters here

  icontag <- htmltools::tags$span(x$text, class = str_text(x), `data-fa-transform` = str_dft(x),
                                  style = str_style(x))

  out <- htmltools::tagList(header, icontag)
  class(out) <- c("anicon", class(out))
  out
}

str_text <- function(x) {
  out <- NULL
  out <- paste0(out, switch(as.character(x$options$size), `1` = "", lg = "fa-lg",
                            xs = "fa-xs", sm = "fa-sm", `2` = "fa-2x", `3` = "fa-3x", `4` = "fa-4x",
                            `5` = "fa-5x", `7` = "fa-7x", `10` = "fa-10x"))

  if (x$options$fixed_width) {
    out <- paste0(out, "fa-fw")
  }

  anim_append <- paste0(" faa-", x$options$animate)
  out <- paste0(out, switch(x$options$anitype, `repeat` = paste(anim_append,
                                                                "animated "), hover = paste(anim_append, "animated-hover ")))

  out <- paste0(out, switch(x$options$speed, normal = "", fast = "faa-fast ",
                            slow = "faa-slow "))

  if (x$options$border) {
    out <- paste0(out, "fa-border")
  }

  if (!is.null(x$options$iother)) {
    out <- paste(out, paste(x$options$iother, collapse = " "))
  }

  out
}
