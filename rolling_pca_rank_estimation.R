if(!require(pacman)){
  install.packages("pacman")
}

#' Title
#'
#' @param dataset_cleaned  the dataset
#' @param window_size window size
#' @param num_pca obv
#' @param forget_factor see the f argument in online'PCA::incRpca 
#' @param initial_window - the initial window- we'll have 0s there...
#'
#' @return
#' @export
#'
#' @examples
get_list_ranks <- function(
  dataset_cleaned,
  window_size=30,
  num_pca=3,
  forget_factor=1/256,# check this re:sampling rate
  initial_window=30
) {
  pacman::p_load("onlinePCA","data.table")
  
  
numeric_columns <- colnames(dataset_cleaned)[
  sapply(dataset_cleaned,class)%in%c("numeric","integer")]
setDT(dataset_cleaned)

x <- as.matrix(
  dataset_cleaned[,.SD,.SDcols=numeric_columns]
)

n <- nrow(x)

n0 <- initial_window
q <- num_pca
# dim(x) # (100,50)
## Incremental PCA (IPCA, centered)
pca_obj <- prcomp(x[1:n0,],rank. = q) # initialization
# pca <- prcomp(x) # initialization

xbar <- pca_obj$center
pca <- list(values=pca_obj$sdev[1:q]^2, vectors=pca_obj$rotation[,1:q])

i <- n0+1
# x[i,]

# pca[1:(n0+i)]


projections_old <- scale(x[1:n0,], pca_obj$center, pca_obj$scale) %*% pca_obj$rotation 
projections_new <- scale(x[n0+1:2,], pca_obj$center, pca_obj$scale) %*% pca_obj$rotation 
# rank(2,1:10)
# rank(c(projections_new[,1],projections_old[,1]))->ranks
# ranks[c(1,2)]/length(unique(ranks))
# plot(density(projections_old[,1]))

# fill the matrix of ranks
ranks <- matrix(data = 0,nrow=n,ncol=q)
i <- n0+1
for (i in (n0+1):n){
  projections_old <- scale(x[1:(i-1),], pca_obj$center, 
                         pca_obj$scale) %*% pca_obj$rotation 
  projections_new <- scale(matrix(x[i,],nrow=1), pca_obj$center, 
                         pca_obj$scale) %*% pca_obj$rotation 
  ranks_new_normalized <- apply(rbind(projections_new,projections_old),2,function(x){
    rnks <- rank(x)
    rnks/length(unique(rnks))
  })[1,]
  # ranks_new_normalized
  ranks[i,] <- ranks_new_normalized
  xbar <- updateMean(xbar, x[i,], i-1)
  pca <- incRpca(pca$values, pca$vectors, x[i,], i-1, q = q,
                 center = xbar,f=forget_factor)
}

ranks %>% as.data.table()->ranks_dt
ranks
# likelyhood <- 2*apply(ranks[ranks[,1]!=0,],2,function(x)abs(x-0.5))[,1:3] %>% 
#   apply(1,prod)

# 0.4^3
  
}

ranks_dt <- get_list_ranks(
  dataset[,.SD,.SDcols=diffed_cols] 
)
