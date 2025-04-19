if(!require('fourPNO')) {
  install.packages('fourPNO')
  library('fourPNO')
}
if(!require('dplyr')) {
  install.packages('dplyr')
  library('dplyr')
}

#' Gets the cluster labels of the synthetic dataset of size 4n
#'
#' @param n the number of points per class
#'
#' @returns list of the cluster labels
#' @export
#'
#' @examples
get_cluster_labels <- function(n) {
  return(rep(1:4, each=n))
}

#' Gets the time series prototype labels of the synthetic dataset of size 4n,
#' being the first n  and last n instances of the first prototype and the remaining
#' of the second.
#'
#' @param n the number of points per class
#'
#' @returns list of the cluster labels
#' @export
#'
#' @examples
get_dynamic_cluster_labels <- function(n) {
  return(append(append(rep(1, each=n), rep(2, each=2*n)),  rep(1, each=n)))
}

#' Gets a dataframe of two real number columns and 4n data points
#'
#' @param n the number of points per class
#'
#' @returns dataframe
#' @export
#'
#' @examples
get_static_points <- function(n) {
  X <- as.data.frame(rmvnorm(n, c(-4,-4), diag(2)))
  names(X) <- c('x1', 'x2')
  
  Xt <- as.data.frame(rmvnorm(n, c(-3,-3), diag(2)))
  names(Xt) <- c('x1', 'x2')
  X <- bind_rows(X, Xt)
  
  Xt <- as.data.frame(rmvnorm(n, c(3,3), diag(2)))
  names(Xt) <- c('x1', 'x2')
  X <- bind_rows(X, Xt)

  Xt <- as.data.frame(rmvnorm(n, c(4,4), diag(2)))
  names(Xt) <- c('x1', 'x2')
  X <- bind_rows(X, Xt)
  
  return(X)
}

#' Gets a list of time series drawn from two prototypes
#'
#' @param n parameter to return 4n instances
#' @param T amount of timestamps of the time series
#' @param shift_window amount of shift in the drawn time series
#' @param mean pointwise mean shift in the noise introduced in the time series
#' @param sd pointwise SD introduced in the noise of the time series
#' 
#'
#' @returns list of time series, each represented as a list
#' @export
#'
#' @examples
get_dynamic_points <- function(n, T, shift_window, mean, sd) {
  ts <- list()
  for(i in 1:n) {
    ts[[length(ts)+1]] <- prot1(T, i %% shift_window, 60) + rnorm(n, mean = mean, sd = sd)
  }
  for(i in 1:(2*n)) {
    ts[[length(ts)+1]] <- prot2(T, i %% shift_window, 60) + rnorm(n, mean = mean, sd = sd)
  }
  for(i in 1:n) {
    ts[[length(ts)+1]] <- prot1(T, i %% shift_window, 60) + rnorm(n, mean = mean, sd = sd)
  }
  return(ts)
}

#' First time series prototype
#'
#' @param T number of points
#' @param shift shifting (positive moves to the left)
#' @param p periodicity
#'
#' @returns time series, represented as a list of length T 
#' @export
#'
#' @examples
prot1 <- function(T, shift, p){
  time <- 1:T
  values <- sapply((time + shift) %% p, function(x){
    if (x < p /6) {
      x
    } else if (x < p/3){
      x-p/3
    } else {
      0
    }
  })
  return(values)
}

#' Second time series prototype
#'
#' @param T number of points
#' @param shift shifting (positive moves to the left)
#' @param p periodicity
#'
#' @returns time series, represented as a list of length T 
#' @export
#'
#' @examples
prot2 <- function(T, shift, p){
  time <- 1:T
  values <- cos(time/n*p)+.1*cos(time/n*p*10)+time**3/(n**3)
  # values <- time
  return(values)
}


