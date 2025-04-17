if(!require('fourPNO')) {
  install.packages('fourPNO')
  library('fourPNO')
}
if(!require('dplyr')) {
  install.packages('dplyr')
  library('dplyr')
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
  df <- as.data.frame(rmvnorm(n, c(-4,-4), diag(2)))
  names(df) <- c('x1', 'x2')
  
  dft <- as.data.frame(rmvnorm(n, c(-3,-3), diag(2)))
  names(dft) <- c('x1', 'x2')
  df <- bind_rows(df, dft)
  
  dft <- as.data.frame(rmvnorm(n, c(3,3), diag(2)))
  names(dft) <- c('x1', 'x2')
  df <- bind_rows(df, dft)

  dft <- as.data.frame(rmvnorm(n, c(4,4), diag(2)))
  names(dft) <- c('x1', 'x2')
  df <- bind_rows(df, dft)
  
  retu <- list(df[c('x1', 'x2')], rep(1:4, each=n))
  names(retu) <- c('X', 'y')
  return(retu)
}

get_dynamic_points <- function(n, t, shift_window, mean, sd) {
  y <- rep(1:4, each=n)
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
  retu <- list(ts, y)
  names(retu) <- c('X', 'y')
  return(retu)
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


