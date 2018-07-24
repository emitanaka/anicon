str_style <- function(x) {
  out <- NULL
  if (!is.null(x$options$colour)) {
    out <- paste0(out, " color:", x$options$colour, ";")
  }
  if (!is.null(x$options$bgcolour)) {
    out <- paste0(out, " background:", x$options$bgcolour, ";")
  }
  if (!is.null(x$options$sother)) {
    out <- paste(out, paste(paste0(x$options$sother, ";"), collapse = " "))
  }
  if (any(attr(x, "class") %in% c("anitext", "animg"))) {
    out <- paste0(out, " display: -moz-inline-stack; display: inline-block;")
    if(!is.null(x$options$rotate)) {
      out <- paste0(out, " transform: rotate(", x$options$rotate, "deg);")
    }
  }
  out
}





str_icon <- function(x) {
  UseMethod("str_icon")
}

str_icon.icon_faa <- function(x) {
  icon_string(x, icon = "fa", icon_class = ifelse(x$name %in% icon:::fab_iconList,
                                                  "fab", "fas"))
}

str_icon.icon_aia <- function(x) {
  icon_string(x, icon = "ai")
}

get_icontag <- function(x) {
  icon <- htmltools::tags$i(class = str_icon(x), `data-fa-transform` = str_dft(x),
                            style = str_style(x))
  if (x$options$anitype == "parent-hover") {
    icontag <- htmltools::tags$span(paste0(x$options$ltext, icon, x$options$rtext),
                                      class = "faa-parent animated-hover")
  } else {
    icontag <- htmltools::tags$span(paste0(x$options$ltext, icon, x$options$rtext))

  }
  icontag
}


str_dft <- function(x) {
  out <- NULL
  if (x$options$rotate != 0) {
    out <- paste0(out, paste0("rotate-", x$options$rotate, " "))
  }
  if (x$options$grow != 0) {
    txt <- ifelse(x$options$grow > 0, "grow-", "shrink-")
    out <- paste0(out, paste0(txt, abs(x$options$grow), " "))
  }
  if (x$options$flip != "none") {
    out <- paste0(out, switch(x$options$flip, horizontal = "flip-h ",
                              vertical = "flip-v "))
  }
  if (x$options$position[1] != 0) {
    out <- paste0(out, paste0("up-", x$options$position[1], " "))
  }
  if (x$options$position[2] != 0) {
    out <- paste0(out, paste0("down-", x$options$position[2], " "))
  }
  if (x$options$position[3] != 0) {
    out <- paste0(out, paste0("left-", x$options$position[3], " "))
  }
  if (x$options$position[4] != 0) {
    out <- paste0(out, paste0("right-", x$options$position[4], " "))
  }
  out
}

icon_string <- function(x, icon = "fas", icon_class = icon) {
  out <- paste0(icon_class, " ", icon:::paste_icon(icon, x$name))
  out <- paste0(out, switch(as.character(x$options$size), `1` = "", lg = icon:::paste_icon(icon,
                                                                                           "lg"), xs = icon:::paste_icon(icon, "xs"), sm = icon:::paste_icon(icon,
                                                                                                                                                             "sm"), `2` = icon:::paste_icon(icon, "2x"), `3` = icon:::paste_icon(icon,
                                                                                                                                                                                                                                 "3x"), `4` = icon:::paste_icon(icon, "4x"), `5` = icon:::paste_icon(icon,
                                                                                                                                                                                                                                                                                                     "5x"), `7` = icon:::paste_icon(icon, "7x"), `10` = icon:::paste_icon(icon,
                                                                                                                                                                                                                                                                                                                                                                          "10x")))

  if (x$options$fixed_width) {
    out <- paste0(out, icon:::paste_icon(icon, "fw"))
  }

  anim_append <- paste0(" faa-", x$options$animate)
  out <- paste0(out, switch(x$options$anitype, `repeat` = paste(anim_append,
                                                                "animated "), hover = paste(anim_append, "animated-hover "), `parent-hover` = paste0(anim_append,
                                                                                                                                                     " ")))

  out <- paste0(out, switch(x$options$speed, normal = "", fast = "faa-fast ",
                            slow = "faa-slow "))

  if (x$options$border) {
    out <- paste0(out, icon:::paste_icon(icon, "border"))
  }

  if (!is.null(x$options$iother)) {
    out <- paste(out, paste(x$options$iother, collapse = " "))
  }

  out
}

print.anicon <- function(x) {
  cat(gsub("&gt;", ">", gsub("&lt;", "<", x, fixed=T), fixed=T))
}
