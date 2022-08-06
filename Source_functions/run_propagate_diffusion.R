run_propagate_diffusion = function(G,L_time_exp,nodes2label,nodes2labelvalues){
  
  # Info:
  # k is number of time steps
  
  # Load libraries
  library(Matrix)
  
  # Get list of nodes in network
  nodes_network = vertex_attr(G)
  nodes_network = nodes_network$name
  
  # Match your list to network list of nodes
  inds_network = match(nodes2label,nodes_network,nomatch=0)
  
  # Creating x0
  x0 = matrix(0,ncol=length(nodes_network),nrow=1)
  inds_label = inds_network!=0
  x0[inds_network]=nodes2labelvalues[inds_label]
  
  # Do heat diffusion
  print('Performing diffused-based network propagation ...',quote=FALSE)
  #xss_t = as.numeric(x0 %*% expm(-L*time))
  xss_t = as.numeric(x0 %*% L_time_exp)
  
  print('Done',quote=FALSE)
  return(list(xss_t,x0))
  
}