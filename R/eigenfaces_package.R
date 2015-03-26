##### Package documentation and NAMESPACE import

#' eigenfaces
#'
#' Eigenfaces are mainly used for face recognition and face morphometrics,
#' using dimensionality reduction via eigenvectors (principal component analysis)
#' on a set of faces, coming as .jpg images.
#'
#' In many aspects, it is a plugin of the Momocs package: it both takes profits and extends it.
#' To cite eigenfaces in publications: \code{citation('eigenfaces')}. (Paper in prep.)
#'
#' @docType package
#' @name eigenfaces
#' @import Momocs
#' @importFrom jpeg readJPEG
NULL

#' Pain expression subset from Stirling (PICS)
#'
#' Converted as an afaces object
#' @docType data
#' @name pain
#' @rdname data_pain
#' @keywords Datasets
#' @format 84 face images, fixed eye location, 7 expressions (not sad) from each of 12 women.
#' Resolution: 181x241 monochrome
#' \describe{an afaces object}
#' @source "Pain expression subset" found there \url{http://pics.psych.stir.ac.uk/2D_face_sets.htm}
NULL

