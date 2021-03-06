% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cia.R
\name{cia}
\alias{cia}
\title{Insert your own image and make it animated.}
\usage{
cia(src, height = NULL, width = NULL, size = 1, position = c(0, 0, 0,
  0), grow = 0, animate = "flash", speed = "normal", anitype = "repeat",
  rotate = 0, flip = "none", rtext = "", ltext = "", border = FALSE,
  bgcolor = NULL, bgcolour = bgcolor, iother = NULL, sother = NULL,
  dother = NULL)
}
\arguments{
\item{src}{A url or path of image/gif to animate.}

\item{height}{Height of the image that can be specified in percentage or pixels. This cannot be defined if size or grow is defined.}

\item{width}{Width of the image that can be specified in percentage or pixels. This cannot be defined if size or grow is defined.}

\item{size}{Size of the icon relative to font size. Options are lg (133 percent), xs (75 percent), sm (87.5 percent) or a positive number.}

\item{position}{A numerical vector of length 4 specifying the value to move up, down, left and right. NOT IMPLEMENTED YET.}

\item{grow}{Numerical value to grow (or shrink for negative values) without changing or moving the container.}

\item{animate}{'bounce', 'burst', 'falling', 'flash', 'float', 'horizontal',
'passing', 'passing-reverse', 'pulse', 'ring', 'shake', 'spin',
'tada', 'vertical', 'wrench', or use FALSE not to animate.}

\item{speed}{'normal', 'fast', or 'slow'}

\item{anitype}{'repeat', 'hover', or 'parent-hover'}

\item{rotate}{Numerical value for degree of rotation.}

\item{flip}{'none', 'horizontal', 'vertical'.}

\item{rtext}{The text to be inserted on the right side.}

\item{ltext}{The text to be inserted on the left side.}

\item{border}{If TRUE, draws a border around the icon.}

\item{bgcolor, bgcolour}{Colour to be given to the background}

\item{sother}{Character vector of other parameters directly added to the style classes.}
}
\description{
Insert your own image and make it animated.
}
\references{
\href{https://l-lin.github.io/font-awesome-animation/}{Font awesome animation}
}
