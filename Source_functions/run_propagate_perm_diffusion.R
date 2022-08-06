run_propagate_perm_diffusion <- function(G,L_time_exp,nodes2label,nodes2labelvalues,num_perms=100000,flag_PermuteInLabeled=FALSE) { 
  
  start_time <- Sys.time()
  
  # Get list of nodes in network
  nodes_network <- vertex_attr(G)
  nodes_network = as.matrix(nodes_network$name)
  
  # Match your list to network list of nodes
  inds_network <- match(nodes2label,nodes_network,nomatch=0)
  
  # Creating x0
  x0 = matrix(0,ncol=length(nodes_network),nrow=1)
  inds_label = inds_network != 0
  x0[inds_network] = nodes2labelvalues[inds_label]
  
  # Creating Randomly Permuted Matrix
  print('Creating Randomly Permuted Matrix ...')
  x0_perm_mat = matrix(0,nrow=num_perms,ncol=length(nodes_network))
  
  if (flag_PermuteInLabeled){
    for(i in 1:num_perms) {
    labels = nodes2labelvalues[inds_label]
    new_labels = labels[sample(sum(inds_label))]
    x0_row = x0
    x0_row[inds_network] <- new_labels
    x0_perm_mat[i,]=x0_row
    }
  }
  else {
    for(i in 1:num_perms) {
      x0_perm_mat[i,]=x0[sample(length(nodes_network))]
    }
  }

  # Do heat diffusion
  print('Performing diffused-based network propagation ...',quote=FALSE)
  #xss_perm_mat = as.matrix(x0_perm_mat %*% expm(-L*time))
  xss_perm_mat = as.matrix(x0_perm_mat %*% L_time_exp)
  
  finish_time <- Sys.time()
  print(finish_time-start_time)
  
  return(as.matrix(xss_perm_mat))
}