#' Calculates error for classification tasks.
#'
#'Calculates error for classification tasks as the ratio of incorrectly predicted lines to all lines.
#'
#'@param real Vector of real values.
#'@param predicted Vector of the predicted values.
#'
#'@return Calculated error.
#'
#'@author Tabakov Konstantin
#'
#'@seealso error_for_classification_equal_rights, std_dev, get_error
#'
#'@examples error_for_classification(c("1","2"), c("5","2")) 
#'
error_for_classification = function( real, predicted ) {
  
  if( length( real ) != length( predicted ) ) { 
    
    print("Error size diff")
    return( 0 )
    
  }
  else {
    
    count = 0
    count_na = 0
    
    for(i in 1 : length( real ) ) {
      if( is.na( real[i] ) || is.na(predicted[i]) ) {
        count_na = count_na + 1
        next
      }
      if( real[i] != predicted[i] ) {
        count = count + 1
      }
    }
    return( count / ( length( real ) - count_na ) )
    
  }
  
}