calc_pvalues = function(G, xss_t, xss_perm_mat, x0=NULL){
  
  print("Calculating P Values...")
  nodes_network = vertex_attr(G)$name
  num_perms = nrow(xss_perm_mat)
  rep_mat =  t(matrix(rep(xss_t, num_perms), ncol=num_perms))
  p_mat = xss_perm_mat >= rep_mat
  pvals = colSums(as.matrix(p_mat))/num_perms
  fdrs = p.adjust(pvals, method = "fdr")
  
  Y = data.frame(nodes=as.array(nodes_network),
                 pvals=pvals,fdrs=fdrs,
                 x0=as.numeric(x0_1), xss_t=xss_t)
  
  return(Y)
  
}