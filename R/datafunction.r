#' Prime Factorization
#'
#' This function performs prime factorization of an integer.
#' @param n An integer to factorize.
#' @return A vector of prime factors.
#' @export
#' @examples
#' prime_factors(98)
prime_factors <- function(n) {
  if (n <= 1) {
    stop("n must be greater than 1")
  }
  factors <- c()
  divisor <- 2
  while (n > 1) {
    while (n %% divisor == 0) {
      factors <- c(factors, divisor)
      n <- n / divisor
    }
    divisor <- divisor + 1
  }
  return(factors)
}

#' Most Frequent Element
#'
#' This function finds the most frequent element in a vector.
#' @param data A vector of elements (numbers or words).
#' @return The most frequent element.
#' @export
#' @examples
#' most_frequent(c(1, 2, 2, 3, 3, 3, 4))
most_frequent <- function(data) {
  if (length(data) == 0) {
    stop("data vector is empty")
  }
  freq_table <- table(data)
  most_freq <- names(freq_table[freq_table == max(freq_table)])
  return(most_freq)
}

#' Simple Data Plot
#'
#' This function creates a simple plot of the given data.
#' @param data A numeric vector or data frame to plot.
#' @param plot_type The type of plot to create ("histogram", "boxplot", "scatter").
#' @param ... Additional arguments passed to the plotting functions.
#' @return A plot of the data.
#' @export
#' @examples
#' simple_plot(c(1, 2, 2, 3, 3, 3, 4), "histogram")
simple_plot <- function(data, plot_type = "histogram", ...) {
  if (!is.numeric(data) && !is.data.frame(data)) {
    stop("data must be a numeric vector or data frame")
  }
  if (plot_type == "histogram") {
    hist(data, ...)
  } else if (plot_type == "boxplot") {
    boxplot(data, ...)
  } else if (plot_type == "scatter" && is.data.frame(data) && ncol(data) == 2) {
    plot(data[,1], data[,2], ...)
  } else {
    stop("invalid plot type or data format")
  }
}
