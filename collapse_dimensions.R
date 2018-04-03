collapse_dimensions <- function(X){
  # For a 4d array, array rows and array columns are combined to a 3d array
  #Further, the 3d arrays , the rows and columns are further combined to a 3d array
  # significant changes in the way 4d matrices are handled compared to python and matlab scripts
  
  #Input
  #X : 3d or 4d array to be collapsed
    
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
    X3d <- array(X, dim=c(com, ind, org*com))  #converting 4d into  3d with nrow:com, ncol:ind, arraysize = org*com
    Y <- array(0, dim=c(ind, com,org * com))  #creating an empty array with nrow:ind, ncol:com, arraysize=org*com
    org_3d <- arraysize(X3d, 1)
    for (ORG  in seq_along(1:org_3d)){
      Y[, ,ORG] = aperm(X3d[ , ,ORG])                     }
    }
    else {Y = X}
  # check the results for Z
  
  
}