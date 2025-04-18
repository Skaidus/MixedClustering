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

get_resources <- function(expr) {
    res <- memtime(expr)
    retr <- c(as.numeric(res$time[3]), as.numeric(res$memory[3,2])) 
    names(retr) <- c('elapsed.time.S', 'max.memory.MB')
    return(retr)
}

get_static_data <- function(N, d) {
    return(as.data.frame(matrix(rnorm(N*d,1,1), ncol=d)))
}

get_dynamic_data <- function(N, T) {
    return(apply(matrix(rnorm(N*T,1,1), ncol=T), 1, as.list))
}


static_clustering <- function(X, k) {
    return(hcut(X, k = k, 
    stand = TRUE, 
    hc_func = "hclust",
    hc_method = "ward.D2",
    hc_metric = "euclidean"))
}

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