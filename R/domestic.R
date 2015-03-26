
#' Vertical flip
#'
#' Vertical flip on a single matrix or on an afaces object
#'
#' @param x matrix or afaces object
#' @export
vflip <- function(x){
  UseMethod("vflip")
}

#' @export
vflip.default <- function(x){
  x[nrow(x):1,]
}

#' @export
vflip.afaces <- function(x){
  for (i in 1:dim(x)[3]) x[,,i] <- vflip(x[,,i])
  x
}

#' Horizontal flip
#'
#' Horizontal flip on a single matrix or on an afaces object
#'
#' @param x matrix or afaces object
#' @export
hflip <- function(x){
  UseMethod("hflip")
}

#' @export
hflip.default <- function(x){
  x[, ncol(x):1,]
}

#' @export
hflip.afaces <- function(x){
  for (i in 1:dim(x)[3]) x[,,i] <- hflip(x[,,i])
  x
}

#' Shaves a matrix
#'
#' Ie, everything above max is set to max; everything below min is set to min.
#' Used to avoid grey levels below 0 or above 1 when reconstructing faces,
#' and consequently avoid errors with plot method.
#' That explains white or dark areas on extreme PC ranges.
#'
#' @param m matrix to shave
#' @param max the ceiling
#' @param min the floor
#' @export
shave <- function(m, max=1, min=0){
  m[m>max] <- max
  m[m<min] <- min
  m
}

# normalize
# clockwise

# end domestic #################################################################
