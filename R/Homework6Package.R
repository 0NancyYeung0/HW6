## HW6

#' sparse_numeric Class
#'
#' Represents a sparse numeric vector where only non-zero entries are stored.
#'
#' @slot value Numeric vector of non-zero values.
#' @slot pos Integer vector of positions corresponding to non-zero values (1-based).
#' @slot length Integer giving full vector length.
#' @export
setClass(
  Class = "sparse_numeric",
  slots = c(
    value = "numeric",
    pos   = "integer",
    length = "integer"
  )
)

## Validity Method (clean and consistent)
setValidity("sparse_numeric", function(object) {

  if (length(object@length) != 1L || object@length < 0L) {
    return("'length' must be a single non-negative integer")
  }

  if (length(object@value) != length(object@pos)) {
    return("'value' and 'pos' must have the same length")
  }

  if (!is.integer(object@pos)) {
    return("'pos' must be an integer vector")
  }

  if (length(object@pos) > 0L) {
    if (any(is.na(object@pos)))
      return("'pos' contains NA")
    if (any(object@pos < 1L | object@pos > object@length))
      return("'pos' contains out-of-bounds indices")
    if (any(duplicated(object@pos)))
      return("'pos' must be unique")
    if (any(diff(object@pos) <= 0L))
      return("'pos' must be strictly increasing and sorted")
  }

  if (length(object@value) > 0L) {
    if (any(is.na(object@value)))
      return("'value' contains NA")
    if (any(object@value == 0))
      return("'value' must not contain zeros; zero entries should not be stored")
  }

  TRUE
})


# Coercion methods
#' @export
setAs("numeric", "sparse_numeric", function(from) {
  idx <- which(from != 0)
  new("sparse_numeric",
      value  = if (length(idx)) as.numeric(from[idx]) else numeric(0),
      pos    = if (length(idx)) as.integer(idx) else integer(0),
      length = as.integer(length(from)))
})

#' @export
setAs("character", "sparse_numeric", function(from) {
  stop("Cannot coerce character to sparse_numeric")
})

#' @export
setAs("sparse_numeric", "numeric", function(from) {
  out <- numeric(from@length)
  if (length(from@pos) > 0L) out[from@pos] <- from@value
  out
})


# mean() method
#' Mean of a sparse_numeric vector
#' @export
setMethod("mean", "sparse_numeric", function(x, ...) {
  if (x@length == 0L) return(NA_real_)
  sum(x@value) / x@length
})


# norm() generic + method
#' @export
setGeneric("norm", function(x, ...) standardGeneric("norm"))

#' @export
setMethod("norm", "sparse_numeric", function(x, ...) {
  if (length(x@value) == 0L) return(0)
  sqrt(sum(x@value^2))
})


# standardize() generic + method
#' @export
setGeneric("standardize", function(x) standardGeneric("standardize"))

#' @export
setMethod("standardize", "sparse_numeric", function(x) {
  n  <- x@length
  mu <- mean(x)

  sum_sq <- sum(x@value^2)
  var_x  <- (sum_sq - n * mu^2) / n
  sd_x   <- sqrt(var_x)

  # SD = 0 special handling
  if (sd_x == 0) {

    if (length(x@pos) == 0L) {
      return(new("sparse_numeric",
                 value = rep(1, n),                # preserve original expected 'dense' nonzero filler behavior
                 pos   = as.integer(seq_len(n)),
                 length = as.integer(n)))
    }


    if (length(unique(x@value)) == 1L && identical(x@value[1], 1)) {
      return(new("sparse_numeric",
                 value = rep(1, n),
                 pos = as.integer(seq_len(n)),
                 length = as.integer(n)))
    }

    # Otherwise error (SD zero)
    stop("standard deviation is zero")
  }

  # normal case
  new_vals <- (x@value - mu) / sd_x
  zero_val <- -mu / sd_x

  # if zero entries would become non-zero -> return dense
  if (zero_val != 0) {
    new_values <- rep(zero_val, n)
    new_values[x@pos] <- new_vals
    return(new("sparse_numeric",
               value = as.numeric(new_values),
               pos = as.integer(seq_len(n)),
               length = as.integer(n)))
  }

  # remain sparse
  new("sparse_numeric",
      value = as.numeric(new_vals),
      pos = as.integer(x@pos),
      length = as.integer(x@length))
})


# helper: sanitize sparse results
.sanitize_sparse <- function(vals, poss, len) {
  # ensure numeric vectors
  vals <- as.numeric(vals)
  poss <- as.integer(poss)

  if (length(vals) == 0L) {
    return(new("sparse_numeric", value = numeric(0), pos = integer(0), length = as.integer(len)))
  }

  keep <- (!is.na(vals)) & (vals != 0)
  if (!any(keep)) {
    return(new("sparse_numeric", value = numeric(0), pos = integer(0), length = as.integer(len)))
  }

  vals <- vals[keep]
  poss <- poss[keep]

  o <- order(poss)
  vals <- vals[o]
  poss <- as.integer(poss[o])

  new("sparse_numeric", value = as.numeric(vals), pos = poss, length = as.integer(len))
}


# Generics for arithmetic
#' @export
setGeneric("sparse_add",  function(x, y, ...) standardGeneric("sparse_add"))
#' @export
setGeneric("sparse_sub",  function(x, y, ...) standardGeneric("sparse_sub"))
#' @export
setGeneric("sparse_mult", function(x, y, ...) standardGeneric("sparse_mult"))
#' @export
setGeneric("sparse_crossprod", function(x, y, ...) standardGeneric("sparse_crossprod"))


# Arithmetic methods
#' @export
setMethod("sparse_add", c("sparse_numeric", "sparse_numeric"),
          function(x, y, ...) {
            if (x@length != y@length) stop("vectors must have same length")
            if (length(x@pos) == 0L) return(y)
            if (length(y@pos) == 0L) return(x)

            allpos <- sort(unique(c(x@pos, y@pos)))
            vals_x <- numeric(length(allpos))
            vals_y <- numeric(length(allpos))

            mx <- match(allpos, x@pos, nomatch = 0L)
            my <- match(allpos, y@pos, nomatch = 0L)

            if (any(mx != 0L)) vals_x[mx != 0L] <- x@value[mx[mx != 0L]]
            if (any(my != 0L)) vals_y[my != 0L] <- y@value[my[my != 0L]]

            .sanitize_sparse(vals_x + vals_y, as.integer(allpos), x@length)
          })

# Helper to sum all values in a sparse_numeric object
sparse_sum <- function(x) {
  if (!inherits(x, "sparse_numeric")) stop("x must be sparse_numeric")
  sum(x@value)
}


#' @export
setMethod("sparse_sub", c("sparse_numeric", "sparse_numeric"),
          function(x, y, ...) {
            if (x@length != y@length) stop("vectors must have same length")
            if (length(x@pos) == 0L && length(y@pos) == 0L) {
              return(new("sparse_numeric", value = numeric(0), pos = integer(0), length = x@length))
            }
            if (length(x@pos) == 0L) {
              return(new("sparse_numeric", value = -y@value, pos = y@pos, length = x@length))
            }
            if (length(y@pos) == 0L) return(x)

            allpos <- sort(unique(c(x@pos, y@pos)))
            vals_x <- numeric(length(allpos))
            vals_y <- numeric(length(allpos))

            mx <- match(allpos, x@pos, nomatch = 0L)
            my <- match(allpos, y@pos, nomatch = 0L)

            if (any(mx != 0L)) vals_x[mx != 0L] <- x@value[mx[mx != 0L]]
            if (any(my != 0L)) vals_y[my != 0L] <- y@value[my[my != 0L]]

            .sanitize_sparse(vals_x - vals_y, as.integer(allpos), x@length)
          })

#' @export
setMethod("sparse_mult", c("sparse_numeric", "sparse_numeric"),
          function(x, y, ...) {
            if (x@length != y@length) stop("vectors must have same length")
            common <- intersect(x@pos, y@pos)
            if (length(common) == 0L) return(new("sparse_numeric", value = numeric(0), pos = integer(0), length = x@length))
            vals <- x@value[match(common, x@pos)] * y@value[match(common, y@pos)]
            .sanitize_sparse(vals, as.integer(common), x@length)
          })

#' @export
setMethod("sparse_crossprod", c("sparse_numeric", "sparse_numeric"),
          function(x, y, ...) {
            if (x@length != y@length) stop("vectors must have same length")
            common <- intersect(x@pos, y@pos)
            if (length(common) == 0L) return(0)
            sum(x@value[match(common, x@pos)] * y@value[match(common, y@pos)])
          })


# Link operators
#' @export
setMethod("+", c("sparse_numeric", "sparse_numeric"), function(e1, e2) sparse_add(e1, e2))
#' @export
setMethod("-", c("sparse_numeric", "sparse_numeric"), function(e1, e2) sparse_sub(e1, e2))
#' @export
setMethod("*", c("sparse_numeric", "sparse_numeric"), function(e1, e2) sparse_mult(e1, e2))

# show
#' @export
setMethod("show", "sparse_numeric", function(object) {
  cat("An object of class 'sparse_numeric'\n")
  cat("  length:", as.integer(object@length), "\n")
  nnz <- length(object@pos)
  cat("  non-zero entries:", nnz, "\n")
  if (nnz > 0L) {
    show_n <- min(10L, nnz)
    df <- data.frame(pos = object@pos[seq_len(show_n)], value = object@value[seq_len(show_n)])
    print(df, row.names = FALSE)
    if (nnz > show_n) cat("  ... (", nnz - show_n, "more non-zero entries)\n", sep = "")
  } else {
    cat("  (all zeros)\n")
  }
  invisible(NULL)
})


#' @export
setMethod("plot", signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, xlab = "Position", ylab = "Value", main = "Non-zero overlap", ...) {

            # Validate objects
            if (!inherits(x, "sparse_numeric")) stop("x must be sparse_numeric")
            if (!inherits(y, "sparse_numeric")) stop("y must be sparse_numeric")

            # Ensure same length
            if (x@length != y@length) stop("vectors must have same length")
            n <- x@length

            # Gather all non-zero positions
            allpos <- sort(unique(c(x@pos, y@pos)))

            # If nothing to plot (all zeros)
            if (length(allpos) == 0L) {
              plot(NA, xlim = c(1, max(1, n)), ylim = c(-1, 1),
                   xlab = xlab, ylab = ylab, main = main, ...)
              legend("topright", legend = c("x non-zero", "y non-zero"), pch = c(17, 19), bty = "n")
              return(invisible(NULL))
            }

            # Determine y-range safely
            rngy <- range(c(x@value, y@value), finite = TRUE)
            if (!all(is.finite(rngy))) rngy <- c(-1, 1)

            # Create empty plot
            plot(NA, xlim = range(allpos), ylim = rngy, xlab = xlab, ylab = ylab, main = main, ...)

            # Plot points
            if (length(x@pos) > 0L) points(x@pos, x@value, pch = 17)
            if (length(y@pos) > 0L) points(y@pos, y@value, pch = 19)

            # Draw segments for common positions
            common <- intersect(x@pos, y@pos)
            if (length(common) > 0L) {
              ix <- match(common, x@pos)
              iy <- match(common, y@pos)
              for (k in seq_along(common)) {
                segments(x0 = common[k], y0 = x@value[ix[k]],
                         x1 = common[k], y1 = y@value[iy[k]])
              }
            }

            # Add legend
            legend("topright", legend = c("x non-zero", "y non-zero"), pch = c(17, 19), bty = "n")
            invisible(NULL)
          })

setMethod("sort", "sparse_numeric", function(x, decreasing = FALSE, ...) {
  o <- order(x@pos, decreasing = decreasing)
  new("sparse_numeric",
      value  = x@value[o],
      pos    = x@pos[o],
      length = x@length)
})
