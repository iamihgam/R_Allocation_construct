matrixNormalizer <- function(Z, V){
  # normalizes a flow matrix when some rows and columns are null
  
  # Input parameters
  # ----------
  # Z : Flow matrix to be normalized dimensions 
  #     : [com, com] or  [ind,com,com] | [ind,com,ind,com]
  # V : Production volume with which flows are normalized [com, ind]
  # 
  # Output
  # --------
  # A : Normalized flow matrix, without null rows and columns
  # nn_in : filter applied to rows (0 for removed rows, 1 for kept rows)
  # nn_out : filter applied to cols (0 for removed cols, 1 for kept cols)
  
  # Collapse dimensions
  if(ndim(Z)>2){
    Z= collapse_dimensions(Z)
    
  # Basic variables estimation
    com <- nrow(V)
    ind <- ncol(V)
    comZ <- nrow(Z)
  
  # Total production, both aggregate and traceable
    q <- colSums(V)
    u <- colSums(Z)
    q_tr <- array(0, dim=ind*com)
    for (i in seq_along(1:ind)){
      q_tr[i*com:(i+1)*com] <- V[,i]
    }
    
    
  }

  
  
}