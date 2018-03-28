basic_variables <- function(U, V, G,...){ # calculate intermediate variables from SUTs and emission tables 
  
  #Input variables
  #U: Use table[com, ind]
  #S: Supply table[com, ind]
  #G : Unallocated emissions [ext, ind] (default= 0)
  
  #output 
  #com : number of commodities (products)
  #ind : number of industries (activities)
  #org : number of origin industries (for traceable flows)
  #traceable : boolean, are use flows traceable, true or false?
  #e_com : vertical vector of ones [com, 1]
  #e_ind : vertical vector of ones [ind, 1]
  #q : total production volume of each commodity
  #g : total production volume of each industry
  #ext : number of environmental stressors/factors of production
  
  #Requirement functions
  ndim <- function(x){length(dim(x))} # number of dimensions
  dimsize <- function(x){ prod(dim(x))} # number of elements e.g. 0 - vector , 1 or >1 - 2d or 3d matrix
  
  
  #Step 1: assigning G variable
  ext <- vector(mode="numeric", length=0)
  
  #Step2 : Traceability and dimension
  com <- nrow(V)
  ind <- ncol(V)
  if (ndim(U) == ndim(V)){ # Untraceable
    traceable <- FALSE
    org <- 1}
  else if (ndim(U) == ndim(V)+1){ #Traceable
    traceable <- TRUE
    org <- nrow(U)}
  else {warning("Incompatible dimensions")}


  if(dimsize(G)!= 0) {ext = nrow(G)}

  #Step 3 - summation vectors
  e_com <- array(1, com)
  e_ind <- array(1, ind)
  
  #Step4 - Totals
  #industry total
  g <- t(V) %*% e_com
  q <- V %*% e_ind
  
  #Step 5
  bv_out <-  list(com=com, ind=ind, org=org, traceable=traceable, e_com=e_com, e_ind=e_ind, q=q, g=g, ext=ext)
  bv_out
}
  
