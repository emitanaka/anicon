#' Insert icon from academicons v1.8.0
#'
#' @inheritParams faa
#'
#' @references [Academicons](http://jpswalsh.github.io/academicons/)
#' @export
#' @importFrom utils adist
aia <- function(name="academia", size=1, position=c(0,0,0,0), grow=0,
                fixed_width=FALSE, animate="flash", speed="normal", anitype="repeat",
                rtext="", ltext="", rotate=0, flip="none",
                border=FALSE, color=NULL, colour=color,
                bgcolor=NULL, bgcolour=bgcolor,
                iother=NULL, sother=NULL, dother=NULL) {
  if(!(name %in% icon:::ai_iconList)){
    stop(paste0("Icon '", name, "' not found in academicons. Did you mean '", icon:::ai_iconList[which.min(adist(name, icon:::ai_iconList))], "'?"))
  }

  d1 <- icon:::html_dependency_academicons()
  d2 <- htmltools::htmlDependency("font-awesome-animation", "1.0", src=system.file("animation", package="anicon"),
                                  stylesheet="font-awesome-animation.min.css")


  x <- structure(list(name=name,
                      options=list(size=size, fixed_width=fixed_width, position=position,
                                   animate=animate, anitype=anitype, grow=grow,
                                   rtext=rtext, ltext=ltext, rotate=rotate,
                                   flip=flip, border=border, speed=speed,
                                   iother=iother, sother=sother, dother=dother,
                                   colour=colour, bgcolour=bgcolour)),
                 class=c("icon_aia", "icon"))

  header <- htmltools::tags$head(d1, d2) # order matters here

  if(x$options$animate=="parent-hover") {
    icon <- htmltools::tags$i(class=str_icon(x), "data-fa-transform"=str_dft(x),
                              style=str_style(x))
    icontag <- htmltools::tags$span(ltext, icon, rtext, class="faa-parent animated-hover")
  } else {
    icon <- htmltools::tags$i(class=str_icon(x), "data-fa-transform"=str_dft(x),
                              style=str_style(x))
    icontag <- htmltools::tags$span(ltext, icon, rtext)
  }
  out <- htmltools::tagList(header, icontag)
  class(out) <- c("anicon", class(out))
  out
}



