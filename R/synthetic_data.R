if(!require('fourPNO')) {
  install.packages('fourPNO')
  library('fourPNO')
}
if(!require('dplyr')) {
  install.packages('dplyr')
  library('dplyr')
}

get_cluster_labels <- function(n) {
  return(rep(1:4, each=n))
}

get_dynamic_cluster_labels <- function(n) {
  return(append(append(rep(1, each=n), rep(2, each=2*n)),  rep(1, each=n)))
}

#' Gets synthetic data
#'
#' @param n the number of points per class
#' @param seed seed for reproducible results
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

get_dynamic_points <- function(n, t, shift_window, mean, sd) {
  ts <- list()
  for(i in 1:n) {
    ts[[length(ts)+1]] <- prot1(t, i %% shift_window, 60) + rnorm(n, mean = mean, sd = sd)
  }
  for(i in 1:(2*n)) {
    ts[[length(ts)+1]] <- prot2(t, i %% shift_window, 60) + rnorm(n, mean = mean, sd = sd)
  }
  for(i in 1:n) {
    ts[[length(ts)+1]] <- prot1(t, i %% shift_window, 60) + rnorm(n, mean = mean, sd = sd)
  }
  return(ts)
}

#' Title
#'
#' @param n number of points
#' @param shift shifting (positive moves to the left)
#' @param p periodicity
#'
#' @returns
#' @export
#'
#' @examples
prot1 <- function(n, shift, p){
  time <- 1:n
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

#'
prot2 <- function(n, shift, p){
  time <- 1:n
  values <- cos(time/n*p)+.1*cos(time/n*p*10)+time**3/(n**3)
  # values <- time
  return(values)
}


