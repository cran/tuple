##################################################################################
## CODE FOR PACKAGE tuple
##   (Tools for finding unique or replicated matches)

#######
## REQUIREMENTS LOG
#
#    Packages:
#    Projects:

###########################################################################
## CONSTANTS

###########################################################################
## FUNCTIONS

#' Match All Values
#'
#' Extends the functionality of \code{\link[base]{match}} to identify all matching
#' values, instead of just the first one.
#'
#' Returns an integer vector of the index in \code{table} for all
#' the matches. The result is not sorted in numerical index order when
#' more than one value is sought to be matched.
#' Instead, the matches of the first value in \code{x} are listed first,
#' followed by matches to the second value in \code{x} and so on.
#' Values of \code{NA} are treated as data.
#'
#' @param x
#'   A vector.
#' @param table
#'   The lookup table as a vector.
#' @examples
#' matchAll(3, c(1:3, 3, 4:6, 3, NA, 4))
#' matchAll(3:4, c(1:3, 3, 4:6, 3, NA, 4))
#' matchAll(c(NA, 3:4), c(NA, 1:3, 3, 4:6, 3, NA, 4))
#' @keywords match
#' @seealso \code{\link[base]{match}}
#' @export
#' @author Emmanuel Lazaridis <emmanuel@@lazaridis.eu>
matchAll <- function(x, table) {
  myMatch <- match(x, table)
  myRecords <- integer()
  while(length(myMatch)>0) {
    myRecords <- c(myRecords, myMatch)
    table[myMatch] <- ""
    myMatch <- match(x, table)
    if(any(is.na(myMatch))) {
      x <- x[-which(is.na(myMatch))]
      myMatch <- myMatch[-which(is.na(myMatch))]
    }
  }
  if(all(is.na(myRecords))) myRecords <- NA
  if(length(myRecords)>1 && any(is.na(myRecords)))
    myRecords <- myRecords[!is.na(myRecords)]
  return(myRecords)
}

#' Find Orphan Values
#'
#' Finds values that occur exactly once in a vector.
#'
#' Returns the unique values in the same order that they would be
#' returned in a call to \code{\link[base]{unique}}.
#'
#' @param x
#'   A vector.
#' @examples
#' orphan(c(NA, 1:3, 3, 4:6, 3, NA, 4))
#' @keywords orphan, unique
#' @seealso \code{\link[base]{unique}}
#' @export
#' @author Emmanuel Lazaridis <emmanuel@@lazaridis.eu>
orphan <- function(x) {
  return(x[!(duplicated(x) | duplicated(x, fromLast = TRUE))])
}

#' Find Duplicate Values
#'
#' Finds values that occur exactly twice in a vector.
#'
#' Returns the duplicated values in the same order that they would be
#' returned in a call to \code{\link[tuple]{orphan}}.  This fundamentally
#' differs from \code{\link[base]{duplicated}}, which returns
#' a logical vector that is \code{TRUE} when it runs into any but
#' the first occurrence of a value (and is therefore dependent on
#' the direction of testing of the vector).
#'
#' @param x
#'   A vector.
#' @examples
#' duplicate(c(NA, 1:3, 3, 4:6, 3, NA, 4))
#' @keywords duplicate, duplicated, repeat, repeated
#' @seealso \code{\link[base]{duplicated}}
#' @export
#' @author Emmanuel Lazaridis <emmanuel@@lazaridis.eu>
duplicate <- function(x) {
  return(x[duplicated(x)][!(duplicated(x[duplicated(x)]) |
             duplicated(x[duplicated(x)], fromLast=TRUE))])
}

#' Find Values That Are Repeated At Least Thrice
#'
#' Finds values that are repeated at least three times in a vector.
#'
#' Returns a logical vector that is \code{TRUE} when it runs into
#' any but the first or second occurrences of a value, analogous
#' to \code{\link[base]{duplicated}}.
#'
#' @param x
#'   A vector.
#' @param fromLast
#'   A logical indicating if triplication should be considered from
#'   the reverse side, i.e., the two last (or rightmost) of identical
#'   elements would return \code{FALSE}
#' @param ...
#'   Other optional arguments are ignored.
#' @examples
#' triplicated(c(NA, 1:3, 3, 4:6, 3, NA, 4, 3))
#' @keywords triplicate, triplicated, repeat, repeated
#' @seealso \code{\link[base]{duplicated}}
#' @export
#' @author Emmanuel Lazaridis <emmanuel@@lazaridis.eu>
triplicated <- function(x, ..., fromLast = FALSE) {
  retVec <- rep(FALSE, length(x))
  retVec[which(duplicated(x, fromLast = fromLast))[
      which(duplicated(x[which(duplicated(x, fromLast = fromLast))],
            fromLast = fromLast))]] <- TRUE
  return(retVec)
}

#' Find Triplicate Values
#'
#' Finds values that occur exactly three times in a vector.
#'
#' Returns the triplicated values in the same order that they would be
#' returned in a call to \code{\link[tuple]{orphan}}.  This fundamentally
#' differs from \code{\link[tuple]{triplicated}}, which returns
#' a logical vector that is \code{TRUE} when it runs into any but
#' the first or second occurrences of a value (and is therefore
#' dependent on the direction of testing of the vector).
#'
#' @param x
#'   A vector.
#' @examples
#' triplicate(c(NA, 1:3, 3, 4:6, 3, NA, 4))
#' triplicate(c(NA, 1:3, 3, 4:6, 3, NA, 4, 3))
#' @keywords triplicate, triplicated, repeat, repeated
#' @seealso \code{\link[base]{duplicated}}
#' @export
#' @author Emmanuel Lazaridis <emmanuel@@lazaridis.eu>
triplicate <- function(x) {
  return(x[triplicated(x)][!(duplicated(x[triplicated(x)]) |
             duplicated(x[triplicated(x)], fromLast = TRUE))])
}

