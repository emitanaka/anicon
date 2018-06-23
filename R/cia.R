
#' Insert animated image. This is still in production.
#'
#' @param src A url or path of image to animate.
#' @param alt Alternative text to the image.
#' @param align top, bottom, middle, left, right
#' @param height height of the image that can be specified in percentage or pixels.
#' @param width width of the image that can be specified in percentage or pixels.
#' @param position A numerical vector of length 4 specifying the value to move up, down, left and right.
#' @param grow Numerical value to grow (or shrink for negative values) without changing or moving the container.
#' @param animate 'bounce', 'burst', 'falling', 'flash', 'float', 'horizontal',
#' 'passing', 'passing-reverse', 'pulse', 'ring', 'shake', 'spin',
#' 'tada', 'vertical', or 'wrench'
#' @param speed 'normal', 'fast', or 'slow'
#' @param anitype 'repeat' or 'hover'
#' @param rotate Numerical value for degree of rotation. This is not working.
#' @param flip 'none', 'horizontal', 'vertical'.
#' @param border If TRUE, draws a border around the icon.
#' @param iother Character vector of other parameters directly added to the icon classes, e.g. fa-pull-left, fa-pull-right.
#' @param sother Character vector of other parameters directly added to the style classes.
#' @param dother Character vector of other parameters directly added to the data-fa-transform classes, e.g. shrink-8, grow-2
#' @param bgcolor,bgcolour Colour to be given to the background
#'
#' @references [Font awesome animation](https://l-lin.github.io/font-awesome-animation/)
#'
#' @export
#' @importFrom utils adist
cia <- function(src, alt="", height="100%", width="100%", size=1,
                position=c(0,0,0,0), grow=0, animate="flash",
                speed="normal", anitype="repeat",
                rotate=0, flip="none",
                border=FALSE,
                bgcolor=NULL, bgcolour=bgcolor,
                iother=NULL, sother=NULL, dother=NULL) {

  d1 <- htmltools::htmlDependency("fontawesome", "5.0.13", src=system.file("fontawesome/font-awesome-5.0.13", package="anicon"),
                                  stylesheet="css/fontawesome-all.min.css")
  d2 <- htmltools::htmlDependency("fontawesome", "5.0.13", src=system.file("fontawesome", package="anicon"),
                                  stylesheet="fa-svg-with-js.css")
  d3 <- htmltools::htmlDependency("fontawesome", "5.0.13", src=system.file("fontawesome/font-awesome-5.0.13", package="anicon"),
                                  script="js/fontawesome-all.min.js")
  d4 <- htmltools::htmlDependency("font-awesome-animation", "1.0", src=system.file("animation", package="anicon"),
                                  stylesheet="font-awesome-animation-emi.css")
  d5 <- htmltools::htmlDependency("font-awesome-animation", "1.0", src=system.file("animation", package="anicon"),
                                  script="custom.js")
  #d5 <- htmltools::htmlDependency("font-awesome-animation", "1.0", src=system.file("animation", package="anicon"),
  #                                stylesheet="custom.css")


  x <- structure(list(src=src,
                      options=list(size=size, position=position,
                                   animate=animate, anitype=anitype, grow=grow,
                                   rotate=rotate,
                                   flip=flip, border=border, speed=speed,
                                   iother=iother, sother=sother, dother=dother,
                                   bgcolour=bgcolour)),
                 class=c("animg", "icon"))

  header <- htmltools::tags$head(d4, d3, d1, d2, d5) # order matters here

  icontag <- htmltools::tags$div(htmltools::tags$img(src=x$src, "data-rotate"=rotate, height=height, width=width),
                                 class=str_img(x), "data-fa-transform"=str_dft(x),
                                 style=str_style(x))

  out <- htmltools::tagList(header, icontag)
  class(out) <- c("anicon", class(out))
  out
}
