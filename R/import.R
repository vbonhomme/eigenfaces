#' Import a set of .jpg images to an afaces object
#'
#' Import a set of .jpg images to an afaces object
#' @param path the path to .jpg (working directory by default)
#' @export
import_faces <- function(path=getwd()){
  jpg.paths <- list.files(path, full.names=TRUE)
  # we create a list to store the results
  IMG <- list()
  # if it's gonna be long we set a progess bar
  if (length(jpg.paths) > 10) {
    pb <- txtProgressBar(1, length(jpg.paths))
    t <- TRUE
  } else {
    t <- FALSE
  }
  # we loop over jpg.paths
  for (i in seq(along=jpg.paths)){
    img <- readJPEG(jpg.paths[i])
    # if required, we convert rvb to grey
    if (length(dim(img))>2) img <- apply(img, 1:2, mean)
    IMG[[i]] <- img
    if (t) setTxtProgressBar(pb, i)
  }
  # we rename the list
  names(IMG) <- .trim.path(.trim.ext(jpg.paths))

  # we define an internal function
  normalize <- function(m){
    m <- m - min(m)
    m /max(m)
  }
  # and apply it on out list
  l <- lapply(IMG, normalize)

  # we check a bit
  l.dim <- sapply(l, dim)
  if (nrow(l.dim) != 2)
    stop(" * images should be grey matrices")
  l.dim.u <- apply(l.dim, 1, unique)
  if (!is.numeric(l.dim.u))
    stop(" * images must have the same dimension")
  if (length(l.dim.u) != nrow(l.dim))
    stop(" * images must have the same dimension")
  # we deduce some dims
  nr <- nrow(l[[1]])
  nc <- ncol(l[[1]])
  ns <- length(l)
  # we create an array
  arr <- array(NA, dim = c(nr, nc, ns),
               dimnames=list(paste0("x", 1:nr), paste0("y", 1:nc), names(l)))
  # we store every slice of l and return the results
  for (i in seq(along=l)) arr[,,i] <- l[[i]]
  class(arr) <- c("afaces", class(arr))
  return(arr)
}


# extracts lf structure and preserves Momocs'
#' Extracts grouping structure from filenames
#'
#' Can be called either on an object returned by \link{list.files} or directly
#' on an afaces object. In the latter case, \code{dimanmes(afaces)[3]} is used.
#' See Momocs' lf_structure.
#' @param lf a list of files
#' @param ... useless, used for genericity.
#' @export
lf_structure <- function(lf, ...){
  UseMethod("lf_structure")
}

#' @export
lf_structure.default <- function(lf, ...){
  Momocs::lf_structure(lf, ...)
}

#' @export
lf_structure.afaces <- function(lf, ...){
  lf_structure(dimnames(lf)[[3]], ...)
}

# end import ###################################################################

