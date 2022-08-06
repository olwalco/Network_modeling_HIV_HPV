calc_laplacian = function(G,time=0.1){
  
# Load libraries
library(Matrix)

# Calculate Laplacian matrix
print('Calculate network Laplacian ...',quote=FALSE)
D_vec = Matrix::as.matrix(igraph::degree(G, mode="all"))
D0 = diag(as.vector(D_vec),nrow=length(D_vec),ncol=length(D_vec))
A = as_adjacency_matrix(G,type="both",names=FALSE)
L = D0 - A

# Matrix exponential
L_time_exp = expm(-L*time)

return(list(L,L_time_exp))

}

