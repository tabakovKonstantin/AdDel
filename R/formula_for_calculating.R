#' Creates object of class "Formula" which can be used for a "Random forest" algorithm, "Linear regression", etc.
#'
#' Creates object of class "Formula" which can be used for a "Random forest" algorithm, "Linear regression", etc. Format: "predicted_sign ~ signs + signs + .... signs".
#'
#'@param signs Vector or list of input features.
#'@param predicted_sign  Target attribute.
#'
#'@return An object of class formula. 
#'
#'@author Tabakov Konstantin
#'
#'@examples formula_for_calculating(c("one", "two", "three"), "predict")
#'
formula_for_calculating = function( signs, predicted_sign ) {
  
  ## For list  ##
  if( is.list(signs) ) {
    signs = setdiff( signs[[1]], predicted_sign )
    string = predicted_sign
    string = paste ( string, "~",  sep = " " )
    
    if ( length( signs[[1]] ) > 0 ) {
      string = paste ( string, signs[[1]][1],  sep = " " )
      if ( length( signs[[1]] ) > 1) {
        for( i in 2 : length( signs[[1]] ) ) { 
          string = paste ( string, signs[[1]][i],  sep = " + " )      
        } 
      }
      return ( formula( string ) )
    }
  }
  
  ## For vector ##
  if( is.vector(signs) ) {
    signs = setdiff( signs, predicted_sign )
    string = predicted_sign
    string = paste ( string, "~",  sep = " " )
    
    if ( length( signs ) > 0 ) {
      string = paste ( string, signs[1],  sep = " " )
      if ( length( signs ) > 1) {
        for( i in 2 : length( signs ) ) { 
          string = paste ( string, signs[i],  sep = " + " )      
        } 
      }
      return ( formula( string ) )
    }
  }
  return ( 0 )
}