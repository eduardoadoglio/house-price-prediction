# Helper functions

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cor_mat){
  cor_mat[upper.tri(cor_mat)] <- NA
  return(cor_mat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cor_mat){
  cor_mat[lower.tri(cor_mat)]<- NA
  return(cor_mat)
}

reorder_cormat <- function(cor_mat){
  # Use correlation between variables as distance
  dd <- as.dist((1 - cor_mat) / 2)
  hc <- hclust(dd)
  cor_mat <-cor_mat[hc$order, hc$order]
}