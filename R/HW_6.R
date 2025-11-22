# HW_6.R

#------------------------
# sparse_numeric class
#------------------------

#' Sparse numeric vector class
#'
#' This S4 class represents a numeric vector stored in sparse format,
#' storing only non-zero values and their positions.
#'
#' @slot value Numeric vector of non-zero values.
#' @slot pos Integer vector of positions corresponding to `value`.
#' @slot length Integer of total vector length.
#'
#' @export
setClass(
  Class = "sparse_numeric",
  slots = c(
    value = "numeric",
    pos = "integer",
    length = "integer"
  )
)

#------------------------
# validity
#------------------------

#' Validate sparse_numeric object
#'
#' Checks that the slots are consistent and valid.
#' @param object sparse_numeric object
#' @keywords internal
setValidity("sparse_numeric", function(object) {
  msgs <- character()

  if (!(is.integer(object@length) && length(object@length) == 1L && !is.na(object@length))) {
    msgs <- c(msgs, "'length' must be a single integer")
  } else if (object@length < 0L) {
    msgs <- c(msgs, "'length' must be non-negative")
  }

  if (length(object@value) != length(object@pos)) msgs <- c(msgs, "'value' and 'pos' must have same length")
  if (!is.integer(object@pos)) msgs <- c(msgs, "'pos' must be integer vector")

  if (length(object@pos) > 0L) {
    if (any(is.na(object@pos))) msgs <- c(msgs, "'pos' contains NA")
    if (any(object@pos < 1L)) msgs <- c(msgs, "all 'pos' entries must be >= 1")
    if (any(object@pos > object@length)) msgs <- c(msgs, "'pos' entries cannot exceed 'length'")
    if (any(duplicated(object@pos))) msgs <- c(msgs, "'pos' must not contain duplicates")
  }

  if (length(object@value) > 0L) {
    if (any(is.na(object@value))) msgs <- c(msgs, "'value' contains NA")
    if (any(object@value == 0)) msgs <- c(msgs, "'value' must not contain zeros")
  }

  if (length(msgs) == 0L) TRUE else msgs
})

#------------------------
# coercion
#------------------------

#' Convert sparse_numeric to numeric
#'
#' Converts a sparse_numeric object to a full numeric vector.
#'
#' @param x sparse_numeric object
#'
#' @return numeric vector
#' @export
#' @examples
#' x <- as(c(0,2,0,3), "sparse_numeric")
#' as(x, "numeric")
setAs("numeric", "sparse_numeric", function(from) {
  n <- length(from)
  nz_idx <- which(from != 0)
  new("sparse_numeric",
      value = if (length(nz_idx) > 0) as.numeric(from[nz_idx]) else numeric(0),
      pos = if (length(nz_idx) > 0) as.integer(nz_idx) else integer(0),
      length = as.integer(n)
  )
})

#' @rdname sparse_numeric
#' @export
setAs("sparse_numeric", "numeric", function(from) {
  n <- as.integer(from@length)
  out <- numeric(n)
  if (length(from@pos) > 0L) out[from@pos] <- from@value
  out
})

#------------------------
# internal helper
#------------------------

#' Sanitize sparse_numeric vector
#'
#' Removes zeros, orders positions
#' @keywords internal
.sanitize_sparse <- function(vals, poss, len) {
  if (length(vals) == 0L) return(new("sparse_numeric", value=numeric(0), pos=integer(0), length=as.integer(len)))
  keep <- vals != 0 & !is.na(vals)
  vals <- vals[keep]; poss <- poss[keep]
  o <- order(poss); vals <- vals[o]; poss <- as.integer(poss[o])
  new("sparse_numeric", value=as.numeric(vals), pos=poss, length=as.integer(len))
}

#------------------------
# generics
#------------------------

#' @name sparse_add
#' @export
setGeneric("sparse_add", function(x, y, ...) standardGeneric("sparse_add"))

#' @name sparse_sub
#' @export
setGeneric("sparse_sub", function(x, y, ...) standardGeneric("sparse_sub"))

#' @name sparse_mult
#' @export
setGeneric("sparse_mult", function(x, y, ...) standardGeneric("sparse_mult"))

#' @name sparse_crossprod
#' @export
setGeneric("sparse_crossprod", function(x, y, ...) standardGeneric("sparse_crossprod"))

#' @name sparse_norm
#' @export
setGeneric("sparse_norm", function(x, ...) standardGeneric("sparse_norm"))

#' @name standardize
#' @export
setGeneric("standardize", function(x, ...) standardGeneric("standardize"))

#' @name sparse_sum
#' @export
setGeneric("sparse_sum", function(x, ...) standardGeneric("sparse_sum"))

#------------------------
# methods
#------------------------

#' Add two sparse_numeric objects
#'
#' Performs element-wise addition of two sparse_numeric objects.
#'
#' @param x sparse_numeric object
#' @param y sparse_numeric object
#'
#' @return sparse_numeric object
#' @export
#' @examples
#' x <- as(c(0,2,0,3), "sparse_numeric")
#' y <- as(c(1,0,4,0), "sparse_numeric")
#' sparse_add(x, y)
setMethod("sparse_add", signature(x="sparse_numeric", y="sparse_numeric"), function(x, y, ...) {
  if (x@length != y@length) stop("vectors must have same length")
  if (length(x@pos)==0L) return(y)
  if (length(y@pos)==0L) return(x)
  allpos <- sort(unique(c(x@pos, y@pos)))
  vals_x <- numeric(length(allpos))
  vals_y <- numeric(length(allpos))
  match_x <- match(allpos, x@pos, nomatch=0L)
  match_y <- match(allpos, y@pos, nomatch=0L)
  vals_x[match_x!=0L] <- x@value[match_x[match_x!=0L]]
  vals_y[match_y!=0L] <- y@value[match_y[match_y!=0L]]
  res_vals <- vals_x + vals_y
  .sanitize_sparse(res_vals, as.integer(allpos), x@length)
})


#' Subtract two sparse_numeric objects
#'
#' Performs element-wise subtraction of y from x.
#'
#' @param x sparse_numeric object
#' @param y sparse_numeric object
#'
#' @return sparse_numeric object
#' @export
#' @examples
#' x <- as(c(1,2,0,3), "sparse_numeric")
#' y <- as(c(1,1,0,4), "sparse_numeric")
#' sparse_sub(x, y)
setMethod("sparse_sub", signature(x="sparse_numeric", y="sparse_numeric"), function(x, y, ...) {
  if (x@length != y@length) stop("vectors must have same length")
  if (length(x@pos)==0L && length(y@pos)==0L) return(new("sparse_numeric", value=numeric(0), pos=integer(0), length=x@length))
  if (length(x@pos)==0L) return(new("sparse_numeric", value=-y@value, pos=y@pos, length=x@length))
  if (length(y@pos)==0L) return(x)
  allpos <- sort(unique(c(x@pos, y@pos)))
  vals_x <- numeric(length(allpos)); vals_y <- numeric(length(allpos))
  match_x <- match(allpos, x@pos, nomatch=0L)
  match_y <- match(allpos, y@pos, nomatch=0L)
  vals_x[match_x!=0L] <- x@value[match_x[match_x!=0L]]
  vals_y[match_y!=0L] <- y@value[match_y[match_y!=0L]]
  res_vals <- vals_x - vals_y
  .sanitize_sparse(res_vals, as.integer(allpos), x@length)
})


#------------------------
# sparse_mult
#------------------------
#' Multiply two sparse_numeric objects
#'
#' Performs element-wise multiplication of two sparse_numeric objects.
#'
#' @param x sparse_numeric object
#' @param y sparse_numeric object
#'
#' @return sparse_numeric object
#' @export
#' @examples
#' x <- as(c(0,2,0,3), "sparse_numeric")
#' y <- as(c(1,0,5,2), "sparse_numeric")
#' sparse_mult(x, y)
setMethod("sparse_mult", signature(x="sparse_numeric", y="sparse_numeric"), function(x, y, ...) {
  if (x@length != y@length) stop("vectors must have same length")
  if (length(x@pos)==0L || length(y@pos)==0L) return(new("sparse_numeric", value=numeric(0), pos=integer(0), length=x@length))
  common <- intersect(x@pos, y@pos)
  if (length(common)==0L) return(new("sparse_numeric", value=numeric(0), pos=integer(0), length=x@length))
  ix <- match(common, x@pos)
  iy <- match(common, y@pos)
  prod_vals <- x@value[ix]*y@value[iy]
  .sanitize_sparse(prod_vals, as.integer(common), x@length)
})

#------------------------
# sparse_crossprod
#------------------------
#' Cross product of two sparse_numeric objects
#'
#' Computes the dot product of two sparse_numeric objects.
#'
#' @param x sparse_numeric object
#' @param y sparse_numeric object
#'
#' @return numeric scalar
#' @export
#' @examples
#' x <- as(c(1,2,0,3), "sparse_numeric")
#' y <- as(c(0,2,5,1), "sparse_numeric")
#' sparse_crossprod(x, y)
setMethod("sparse_crossprod", signature(x="sparse_numeric", y="sparse_numeric"), function(x, y, ...) {
  if (x@length != y@length) stop("vectors must have same length")
  if (length(x@pos)==0L || length(y@pos)==0L) return(0)
  common <- intersect(x@pos, y@pos)
  if (length(common)==0L) return(0)
  ix <- match(common, x@pos); iy <- match(common, y@pos)
  sum(x@value[ix]*y@value[iy])
})


#------------------------
# sparse_norm
#------------------------
#' Euclidean norm of sparse_numeric object
#'
#' Computes the Euclidean norm of a sparse_numeric object.
#'
#' @param x sparse_numeric object
#'
#' @return numeric scalar
#' @export
#' @examples
#' x <- as(c(3,0,4), "sparse_numeric")
#' sparse_norm(x)
setMethod("sparse_norm", "sparse_numeric", function(x, ...) {
  if (length(x@value)==0L) return(0)
  sqrt(sum(x@value^2))
})


#------------------------
# sparse_sum
#------------------------
#' Sum of sparse_numeric object
#'
#' Computes the sum of stored values in a sparse_numeric object.
#'
#' @param x sparse_numeric object
#'
#' @return numeric scalar
#' @export
#' @examples
#' x <- as(c(1,0,2,0,3), "sparse_numeric")
#' sparse_sum(x)
setMethod("sparse_sum", "sparse_numeric", function(x, ...) sum(x@value))


#------------------------
# standardize
#------------------------
#' Standardize a sparse_numeric object
#'
#' Standardizes a sparse_numeric object to mean 0 and standard deviation 1.
#'
#' @param x sparse_numeric object
#'
#' @return sparse_numeric object
#' @export
#' @examples
#' x <- as(c(1,0,1), "sparse_numeric")
#' standardize(x)
setMethod("standardize", "sparse_numeric", function(x, ...) {
  x_num <- as(x, "numeric")  # convert to numeric
  m <- mean(x_num)
  s <- sqrt(mean(x_num^2) - m^2)
  if (s == 0) stop("Cannot standardize: zero standard deviation")
  as((x_num - m)/s, "sparse_numeric")
})


#' Mean for sparse_numeric
#'
#' Computes the mean of a sparse_numeric vector including zeros.
#'
#' @param x sparse_numeric object
#' @param ... additional arguments (ignored)
#'
#' @return numeric scalar
#' @export
setMethod("mean", "sparse_numeric", function(x, ...) {
  if (x@length == 0L) return(NaN)
  sum(x@value) / x@length
})

#' Sort sparse_numeric by position
#'
#' Returns a sparse_numeric object with entries sorted by the `pos` slot.
#'
#' @param x sparse_numeric object
#' @param decreasing logical, whether to sort in decreasing order
#' @param ... additional arguments (ignored)
#'
#' @return sparse_numeric object with sorted pos and value
#' @export
setMethod("sort", "sparse_numeric", function(x, decreasing = FALSE, ...) {
  o <- order(x@pos, decreasing = decreasing)
  new("sparse_numeric",
      value = x@value[o],
      pos = x@pos[o],
      length = x@length)
})


# show method
#' @rdname sparse_numeric
#' @export
setMethod("show", "sparse_numeric", function(object) {
  nnz <- length(object@pos)
  cat("An object of class 'sparse_numeric'\n")
  cat("  length:", as.integer(object@length), "\n")
  cat("  non-zero entries:", nnz, "\n")
  if (nnz>0L) {
    show_n <- min(10L, nnz)
    df <- data.frame(pos=object@pos[seq_len(show_n)], value=object@value[seq_len(show_n)])
    print(df, row.names=FALSE)
    if (nnz>show_n) cat("  ... (", nnz-show_n, "more non-zero entries)\n", sep="")
  } else cat("  (all zeros)\n")
  invisible(NULL)
})

# plot method
#' @rdname sparse_numeric
#' @export
setMethod("plot", signature(x="sparse_numeric", y="sparse_numeric"), function(x, y, xlab="Position", ylab="Value", main="Non-zero overlap", ...) {
  if (x@length != y@length) stop("vectors must have same length")
  allpos <- sort(unique(c(x@pos, y@pos)))
  rngx <- range(c(x@value, y@value), finite=TRUE)
  if (length(rngx)==0L || any(is.infinite(rngx))) rngx <- c(-1,1)
  plot(NA, xlim=range(allpos, na.rm=TRUE), ylim=rngx, xlab=xlab, ylab=ylab, main=main, ...)
  if (length(x@pos)>0L) points(x@pos, x@value, pch=17)
  if (length(y@pos)>0L) points(y@pos, y@value, pch=19)
  common <- intersect(x@pos, y@pos)
  if (length(common)>0L) {
    ix <- match(common, x@pos); iy <- match(common, y@pos)
    for (k in seq_along(common)) segments(x0=common[k], y0=x@value[ix[k]], x1=common[k], y1=y@value[iy[k]])
  }
  legend("topright", legend=c("x non-zero","y non-zero"), pch=c(17,19), bty="n")
  invisible(NULL)
})
