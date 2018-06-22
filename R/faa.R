
#' Insert animated icon from font awesome
#'
#' @param name A string indicating the icon name.
#' @param size Size of the icon relative to font size. Options are 1, lg (133 percent), xs (75 percent), sm (87.5 percent), 2, 3, 4, 5, 7 or 10.
#' @param position A numerical vector of length 4 specifying the value to move up, down, left and right.
#' @param grow Numerical value to grow (or shrink for negative values) without changing or moving the container.
#' @param fixed_width If TRUE, the icon is set to a fixed width
#' @param animate 'bounce', 'burst', 'falling', 'flash', 'float', 'horizontal',
#' 'passing', 'passing-reverse', 'pulse', 'ring', 'shake', 'spin',
#' 'tada', 'vertical', or 'wrench'
#' @param speed 'normal', 'fast', or 'slow'
#' @param anitype 'repeat', 'hover', or 'parent-hover'
#' @param rtext The text to be inserted on the right side.
#' @param ltext The text to be inserted on the left side.
#' @param rotate Numerical value for degree of rotation.
#' @param flip 'none', 'horizontal', 'vertical'.
#' @param border If TRUE, draws a border around the icon.
#' @param iother Character vector of other parameters directly added to the icon classes, e.g. fa-pull-left, fa-pull-right.
#' @param sother Character vector of other parameters directly added to the style classes.
#' @param dother Character vector of other parameters directly added to the data-fa-transform classes, e.g. shrink-8, grow-2
#' @param color,colour Colour to be given to the icon
#' @param bgcolor,bgcolour Colour to be given to the background
#'
#' @references [Font awesome](http://fontawesome.io/icons/)
#' @references [Font awesome animation](https://l-lin.github.io/font-awesome-animation/)
#' @references [icon R-package](https://github.com/ropenscilabs/icon)
#'
#' @export
#' @importFrom utils adist
faa <- function(name="font-awesome", size=1, position=c(0,0,0,0), grow=0,
                fixed_width=FALSE, animate="flash", speed="normal", anitype="repeat",
                rtext="", ltext="", rotate=0, flip="none",
                border=FALSE, color=NULL, colour=color,
                bgcolor=NULL, bgcolour=bgcolor,
                iother=NULL, sother=NULL, dother=NULL) {
  if(!(name %in% icon:::fa_iconList)){
    stop(paste0("Icon '", name, "' not found in font awesome. Did you mean '", icon:::fa_iconList[which.min(adist(name, icon:::fa_iconList))], "'?"))
  }

  d1 <- htmltools::htmlDependency("fontawesome", "5.0.13", src=system.file("fontawesome/font-awesome-5.0.13", package="anicon"),
                            stylesheet="css/fontawesome-all.min.css")
  d2 <- htmltools::htmlDependency("fontawesome", "5.0.13", src=system.file("fontawesome", package="anicon"),
                                  stylesheet="fa-svg-with-js.css")
  d3 <- htmltools::htmlDependency("fontawesome", "5.0.13", src=system.file("fontawesome/font-awesome-5.0.13", package="anicon"),
                                  script="js/fontawesome-all.min.js")
  d4 <- htmltools::htmlDependency("font-awesome-animation", "1.0", src=system.file("animation", package="anicon"),
                            stylesheet="font-awesome-animation-emi.css")
  #d5 <- htmltools::htmlDependency("font-awesome-animation", "1.0", src=system.file("animation", package="anicon"),
  #                                stylesheet="custom.css")


  x <- structure(list(name=name,
                           options=list(size=size, fixed_width=fixed_width, position=position,
                                          animate=animate, anitype=anitype, grow=grow,
                                          rtext=rtext, ltext=ltext, rotate=rotate,
                                          flip=flip, border=border, speed=speed,
                                          iother=iother, sother=sother, dother=dother,
                                          colour=colour, bgcolour=bgcolour)),
                      class=c("icon_faa", "icon"))

  header <- htmltools::tags$head(d4, d3, d1, d2) # order matters here

  icontag <- get_icontag(x)

  out <- htmltools::tagList(header, icontag)
  class(out) <- c("anicon", class(out))
  out
}
