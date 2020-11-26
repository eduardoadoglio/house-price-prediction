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


lm_eqn <- function(df, dependent, independent){
  m <- lm(dependent ~ independent, df);
  eq <- substitute(y == a + b %.% x*","~~r^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}
