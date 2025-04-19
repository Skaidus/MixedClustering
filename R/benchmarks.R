if(!require('factoextra')) {
  install.packages('factoextra')
  library('factoextra')
}
if(!require('NbClust')) {
  install.packages('NbClust')
  library('NbClust')
}

if(!require('dtwclust')) {
  install.packages('dtwclust')
  library('dtwclust')
}

if(!require("memtime")) {
  devtools::install_github("tdhock/memtime")
  library("memtime")
}

#' Wrapper to measure time and memory of a block of code
#'
#' @param expr the code block to run.
#'
#' @returns a named vector of the elapsed time and max memory used.
#' @export
#'
#' @examples
get_resources <- function(expr) {
    res <- memtime(expr)
    retr <- c(as.numeric(res$time[3]), as.numeric(res$memory[3,2])) 
    names(retr) <- c('elapsed.time.S', 'max.memory.MB')
    return(retr)
}

#' Random static data builder for benchmark purposes
#'
#' @param N the amount of instances
#' @param d the amount of columns
#'
#' @returns a dataset of `N` rows and `d` columns
#' @export
#'
#' @examples
get_static_data <- function(N, d) {
    return(as.data.frame(matrix(rnorm(N*d,1,1), ncol=d)))
}

#' Random dynamic data builder for benchmark purposes
#'
#' @param N the amount of time series
#' @param T the amount of timestamps
#'
#' @returns a list of `N` time series represented as lists of length `T`
#' @export
#'
#' @examples
get_dynamic_data <- function(N, T) {
    return(apply(matrix(rnorm(N*T,1,1), ncol=T), 1, as.list))
}

#' Wrapper around the static clustering methodology for the parameters 
#' detailed in the paper.
#'
#' @param X the static data set
#' @param k the desired amount of static clusters
#'
#' @returns the obtained clusters
#' @export
#'
#' @examples
static_clustering <- function(X, k) {
    return(hcut(X, k = k, 
    stand = TRUE, 
    hc_func = "hclust",
    hc_method = "ward.D2",
    hc_metric = "euclidean"))
}

#' Wrapper around the dynamic clustering methodology for the parameters 
#' detailed in the paper.
#'
#' @param X the dynamic data set
#' @param k the desired amount of dynamic clusters
#'
#' @returns the obtained clusters
#' @export
#'
#' @examples
dynamic_clustering <- function(X, k) {
    window.size <- as.integer(length(X[[1]])/10)
    return(
        tsclust(X,
        k = k,
        type = "hierarchical",
        preproc=zscore,
        distance = "dtw_basic",
        seed = 2973L,
        centroid=shape_extraction,
        trace = FALSE,
        control = hierarchical_control(method = 'ward.D2'),
        args = tsclust_args(preproc = list(center = FALSE),
            dist = list(window.size = window.size, norm="L1"),
            )
        )
    )
}

#' Wrapper around the mixed clustering methodology for the parameters 
#' detailed in the paper.
#'
#' @param X.static the static data set
#' @param dynamic the dynamic data set
#' @param k.static the desired amount of static clusters
#' @param k.dynamic the desired amount of dynamic clusters
#'
#' @returns the obtained clusters
#' @export
#'
#' @examples
mixed_clustering <- function(X.static, X.dynamic, k.static, k.dynamic) {
    window.size <- as.integer(length(X.dynamic[[1]])/10)
    X.dyn.clust <- tsclust(X.dynamic,
        k = k.dynamic,
        type = "hierarchical",
        preproc=zscore,
        distance = "dtw_basic",
        centroid=shape_extraction,
        seed = 2973L,
        trace = FALSE,
        control = hierarchical_control(method = 'ward.D2'),
        args = tsclust_args(preproc = list(center = FALSE),
            dist = list(window.size =window.size, norm="L1")
        )
    )
    X.static$dyn.clust <- X.dyn.clust@cluster
    return(hcut(X.static, k = k.static, 
    stand = TRUE, 
    hc_func = "hclust",
    hc_method = "ward.D2",
    hc_metric = "euclidean"))
}