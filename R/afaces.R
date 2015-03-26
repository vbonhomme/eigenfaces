# afaces methods ###############################################################

#' Convert an object to an afaces object
#'
#' Simply adds 'afaces' to the object, if not already set. No more check so far.
#' @param x an object that should be an array
#' @examples
#' a <- array(1:27, dim=rep(3, 3))
#' a
#' class(a)
#' a2 <- as.afaces(a) # which is equivalent to afaces(a)
#' a2
#' class(as.afaces(a2))
#' @rdname as.afaces
#' @export
as.afaces <- function(x){
  if (!"afaces" %in% class(x)) {
    class(x) <- c("afaces", class(x)) }
    return(x)
  }

#' @rdname as.afaces
#' @export
afaces <- as.afaces

# Print method (no helf file)
#' @export
print.afaces <- function(x, ...){
  cat("An array of", dim(x)[3], "faces (", dim(x)[1], "x", dim(x)[2], ")\n")
}

#' Plot method for afaces objects
#'
#' Plots a particular face from an afaces object
#' @param x afaces object
#' @param y the id of the face to plot (i-th slice of the array). 1 by default
#' @param ... only used to maintain generic plot
#' @examples
#' a <- array(runif(27), dim=rep(3, 3))
#' a <- afaces(a)
#' plot(a, 1) # not very sexy ; same as plot(a)
#' plot(a, 2)
#' @export
plot.afaces <- function(x, y=1, ...){
  img_plot0(x[,,y])
}


#' Converts an afaces object to a wide matrix
#'
#' If the array comes as an object with (i, j, k) pixels, returns
#' a matrix of (k, i*j) pixels. Basically takes all the pixels from
#' top to bottom and left to right, convert them as a vector and turns them into
#' rows of the wide matrix.
#'
#' @param x afaces object
#' @param colnames logical, whether to retain rows/cols names of the afaces object
#' @param ... only used to maintain generic as.matrix
#' @examples
#' a <- array(1:27, dim=rep(3, 3))
#' a <- afaces(a)
#' a[,,1]
#' as.matrix(a)
#' @export
as.matrix.afaces <- function(x, colnames=FALSE, ...){
  m <- apply(x, 3, as.numeric)
  m <- t(m)
  if (colnames) {
    # cosmetics
    xs <- dimnames(x)[[1]]
    ys <- dimnames(x)[[2]]
    colnames(m) <- apply(expand.grid(xs, ys), 1, paste, collapse=".")
  }
  rownames(m) <- dimnames(x)[[3]]
  m
}

#' Principal component analysis on afaces objects
#'
#' Takes an afaces objet, uses as.matrix.afaces to convert it into a wide matrix,
#' then call \link{prcomp} to perform the PCA. Returns a \code{PCA} object ala Momocs
#' on which \code{plot}, \code{MANOVA}, etc. are defined.
#'
#' @param x an afaces object
#' @param scale. logical, whether to scale the matrix
#' @param center logical, whether to center the matrix
#' @param fac data.frame (optionnal) that can be use with Momocs' PCA facilities.
#' @return a list with the same components as \link{prcomp} plus
#' \code{$scale}, \code{$center}, \code{$mshape} and \code{$dim}.
#' @export
PCA.afaces <- function(x, scale.=TRUE, center=TRUE, fac=data.frame()){
  xm <- as.matrix(x)
  if (scale.){
    xm <- xm - min(xm)
    xm <- xm / max(xm)
  }
  mshape <- apply(xm, 2, mean)
  if (center){
    xm <- apply(xm, 2, function(x) x - mean(x))
  }
  xp <- PCA(xm, scale.=FALSE, center=FALSE, fac=fac)
  xp$scale <- scale.
  xp$center <- center
  xp$mshape <- mshape
  xp$dim    <- dim(x[,,1])
  class(xp) <- c("pfaces", class(xp))
  xp
}

# end afaces- ##################################################################
