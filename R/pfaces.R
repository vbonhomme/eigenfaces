# afaces methods ###############################################################

# print margely recycled from Momocs'
#' @export
print.pfaces <- function(x, ...){
  cat("A PCA object\n")
  cat(rep("-", 20), "\n", sep = "")
  cat(" -", nrow(x$x), "faces (", x$dim[1], "x", x$dim[2], ")\n")
  # scale/center details
  cat(" - $scale:",  x$scale, "\n")
  cat(" - $center:", x$center, "\n")
  # we print the fac (if any)
  .print.fac(x$fac)
  cat(" - All components: ",  paste(names(x), collapse=", "), ".\n", sep="")
}

#' Given a pfaces object, reconstruct faces
#'
#' Given a pfaces object, reconstruct faces and returns an afaces object.
#'
#' @param PCA a pfaces object
#' @param pos a vector of length 2 or a 2-col matrix specifying the position
#' on the factorail place (xax, yax) to use to reconstruct faces
#' (\code{(c(0, 0)} by default)
#' @param amp numeric an amplification factor (1 by default)
#' @param xax integer which PC to use as the 1st axis (1st by default)
#' @param yax integer which PC to use as the 2nd axis (2nd by default)
#' @export
PCA2face <- function(PCA, pos=c(0, 0), amp=1, xax=1, yax=2){
  xy <- PCA$x[, c(xax, yax)]
  rot <- PCA$rotation[, c(xax, yax)]
  mshape <- PCA$mshape
  if (is.null(nrow(pos))) pos <- matrix(pos, nrow=1)
  if (ncol(pos) != ncol(rot))
    stop("'mshape' and ncol(rot) lengths differ")
  nx <- PCA$dim[1]
  ny <- PCA$dim[2]
  nz <- nrow(pos)
  res <- array(NA, dim=c(nx, ny, nz))
  for (i in 1:nz) {
    ax.contrib <- Momocs:::.mprod(rot, pos[i, ]) * amp
    vec <- mshape + apply(ax.contrib, 1, sum)
    m <- matrix(vec, nrow=nx, ncol=ny, byrow=FALSE)
    res[,,i] <- shave(m, min=0, max=1)
  }
  class(res) <- c("afaces", class(res))
  return(res)
}

# #' A shortcut to reconstruct faces
#
# face <- function(x, ...){
#   UseMethod("face")
# }
#
# face.default <- function(x, ...){
#   cat(" * Only defined for pfaces objects")
# }
#
# face.pfaces <- function(x, )

# end pfaces ###################################################################
