library(fourPNO)
library(dplyr)

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
  df <- as.data.frame(rmvnorm(n, c(0,0), diag(2)))
  names(df) <- c('x', 'y')
  df['label'] <- 1
  
  dft <- as.data.frame(rmvnorm(n, c(-1,-1), diag(2)))
  names(dft) <- c('x', 'y')
  dft['label'] <- 2
  df <- bind_rows(df, dft)
  
  dft <- as.data.frame(rmvnorm(n, c(1,1), diag(2)))
  names(dft) <- c('x', 'y')
  dft['label'] <- 3
  df <- bind_rows(df, dft)
  
  return (df)
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
  retu <- list(time, values)
  names(retu) <- c('time', 'values')
  return(retu)
}

#'
prot2 <- function(n, shift, p){
  time <- 1:n
  values <- cos(time/n*p)+.1*cos(time/n*p*10)+time**3/(n**3)
  retu <- list(time,values)
  names(retu) <- c('time', 'values')
  return(retu)
}


get_dynamic_points <- function(n, seed = 1) {
  set.seed(seed)
  df <- as.data.frame(rmvnorm(n, c(0,0), diag(2)))
  names(df) <- c('x', 'y')
  df['label'] <- 1
  
  dft <- as.data.frame(rmvnorm(n, c(-1,-1), diag(2)))
  names(dft) <- c('x', 'y')
  dft['label'] <- 2
  df <- bind_rows(df, dft)
  
  dft <- as.data.frame(rmvnorm(n, c(1,1), diag(2)))
  names(dft) <- c('x', 'y')
  dft['label'] <- 3
  df <- bind_rows(df, dft)
  
  return (df)
}