collapse_dimensions <- function(X){
  # collapse 3d or 4d array into matrices
  # significant changes in the way 4d matrices are handled
  #Input
  ----------
  #X : 3d or 4d array to be collapsed
  #first2dimensions : Boolean : For 3d array, should the last two dimensions
  #be flattened together (default) or should the first two be
  #flattened together instead (=true)?
    
  #Output
    #Z : Flatened 2d array
  
  #Required functions
  #ndim 
  #arraysize 
  
  # For a 4d array convert to 3d array...
    if (ndim(X) == 4){
    org <-arraysize(X, 1)
    com <- arraysize(X, 2)
    ind <- nrow(X) 
    X3d <- array(X, dim=c(com, ind, org*com))  #dropping the dimensions into 3 with nrow:com, ncol:ind
    Y <- array(0, dim=c(ind, com,org * com))  #creating an empty array with nrow:ind, ncol:com, arraysize=org*com
    org_3d <- arraysize(X3d, 1)
    for (ORG  in seq_along(1:org_3d)){
      Y[, ,ORG] = aperm(X3d[ , ,ORG])
                        }
    }
    else:
  
}