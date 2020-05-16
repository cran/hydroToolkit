#' Remove spikes
#' 
#' @description Removes spikes, and sets their value to \code{NA_real_}.
#'
#' @param df data frame with date and numeric variable in the first and second column respectively (from \code{read_XXX} functions).
#' @param tolerance numeric with maximum tolerance between a number and its successor.
#'
#' @return The same data frame but without peaks.
#' 
#' @export
#' 
#' @examples
#' # Relative path to raw data
#' full_path <- system.file('extdata', package = "hydroToolkit")
#' 
#' # Read IANIGLA file
#' cuevas <- read_IANIGLA(file = 'Cuevas.csv', path = full_path)             
#' 
#' # Remove spikes from air temperature series
#' tair_rm_spikes <- rm_spikes(df = cuevas, tolerance = 10)
#' 
rm_spikes <- function(df, tolerance){
  #*********************************
  # Condicionales
  #*********************************
    # data.frame
  if(is.data.frame(df) == FALSE){
    return('df argument must be a data frame')
  }
  
    # numeric
  if(is.numeric(df[ , 2]) == FALSE){
    return('df column 2 must be numeric')
  }
  
    # tolerance
  if(is.numeric(tolerance) == FALSE){
    return('tolerance argument must be numeric')
  }
  
  if(length(tolerance) != 1){
    return('tolerance argument must be of length one')
  }
  
  if(is.na(tolerance) == TRUE){
    return('NA_real_ is not a valid tolerance argument')
  }
  
  #*********************************
  #*********************************
  x <- df[ , 2]
  
  diferencia <- abs(diff(x))
  
  ind <- which(diferencia >= tolerance)
  pos <- ind + 1
  
  x[pos] <- NA_real_
  
  df_out <- data.frame(df[ , 1], x)
  colnames(df_out) <- colnames(df)[1:2]
  
  return(df_out)
}