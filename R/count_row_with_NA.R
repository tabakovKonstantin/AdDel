#'Determines the execution of the specified conditions in the column of the table: is the maximum amount less than half? Also displays information about the number of "NA" rows. 
#'
#'Determines the execution of the specified conditions in the column of the table: is the maximum amount less than half? Also displays information about the number of "NA" rows. 
#'
#'@param signs Vector name column of the table, need to check.
#'@param data Source table format: data.table
#'@param max_row_with_NA  Maximum number rows with NA
#'
#'@return  True if the number of "NA" rows is less than maximum. False otherwise.
#'
#'@author Tabakov Konstantin
#'
#'@seealso deleting_rows
#'
count_row_with_NA = function( signs, data, max_row_with_NA ) {
  
  ## Initialization ##
  row_with_NA = c()
  
  ## Count the number of rows ##
  for ( i in signs ) {
    tmp_section = data[ , get( i )]
    for ( j in 1 : length( tmp_section ) ) {
      if( is.na( tmp_section[j] ) ) {
        row_with_NA = unique( c( row_with_NA, j ))
      }
    }
  }
  
  print( "************************************" )
  print( "Set size:" )
  print( nrow( data ) - length( row_with_NA ) )
  
  ## Return result ##
  if ( length( row_with_NA )  >=  max_row_with_NA ) {
    return ( FALSE )
  }
  
  return ( TRUE )
} 
