#' Cumulative sum of precipitation series
#'
#' @description Returns a data frame with two columns: the date and the cumulative sum of the chosen \code{col_number}. This function can deal with \code{NA_real_}.
#'
#' @param df data frame with \code{Date} (or \code{POSIXct}) in the first column and \code{numeric} variables on the others.
#' @param col_number numeric. The column number of the series where to apply the cumulative sum. 
#' @param out_name optional. String value with the column output name. Default is 'cumsum_' plus the original name.
#'
#' @return A data frame with two columns: date and the cumulative sum of the series.
#' @export
#'
#' @examples
#' # Load daily precipitation data-set from BDHI
#' load( paste0(system.file('extdata', package = "hydroToolkit"), '/bdhi_p.rda') )
#' 
#' # Get compact slot
#' p_bdhi <- get_hydroMet(obj = bdhi_p, name = 'compact')[[1]]
#' 
#' # Apply cumulative precipitation function
#' p_cum <- precip_cumsum(df = p_bdhi, col_number = 2, out_name = 'cum_guido')
#' 
precip_cumsum <- function(df, col_number = 2, out_name = NULL){
  
  #****************************************
  # Condicionales
  #****************************************
  
  ## df
  # es data frame?
  if( is.data.frame(df) == FALSE){
    return('df argument must be of class data frame')
  }
  
  # columna uno POSIXct o Date
  if( class(df[ , 1])[1] != 'Date' & class(df[ , 1])[1] != 'POSIXct' ){
    return('First column must be of class Date or POSIXct')
  }
  
  # numericos el resto?
  if( is.numeric( as.matrix(df[ , -1]) ) == FALSE){
    return('All column (except the first one) should be of class numeric')
  }
  
  
  ## col_number
  # es numerico?
  if( is.numeric(col_number) == FALSE){
    return('col_number argument must be of class numeric')
  }
  
  # es de longitud uno?
  if( length(col_number) != 1 ){
    return('col_number should be of length one')
  }
  
  # es mayor que 1? 
  if(col_number <= 1){
    return('col_number argument must be > 1')
  }
  
  # es menor que el numero de columnas?
  if(col_number > ncol(df)){
    return('col_number should not be greater than the number of columns in df')
  }
  
  # es NA_real_
  if(is.na(col_number) == TRUE){
    return('col_number should not be a NA value')
  }
  
  
  ## out_name
  if( is.null(out_name) == TRUE ){
    aux_name <- colnames(df)[col_number]
    out_name <- paste0('cumsum_', aux_name)
    
  } else {
    # es caracter?
    if( is.character(out_name) == FALSE ){
      return('out_name must be of class character')
    }
    
    # es uno solo?
    if( length(out_name) != 1 ){
      return('out_name must be of length one')
    }
    
    # es NA?
    if( is.na(out_name) == TRUE){
      return('A NA value is not valid as out_name argument')
    }
  }
  
  #****************************************
  #****************************************
  # Comienzo funcion
  
  n_it  <- nrow(df)
  x_var <- df[ , col_number]
  
  cumVar <- 0
  out    <- rep(NA, n_it)
  for (i in 1:n_it){
    if(is.na(x_var[i]) == FALSE){
      cumVar <- cumVar + x_var[i]
      out[i] <- cumVar
    }
  }#end for
  
  df_out <- data.frame(df[ , 1], out)
  
  colnames(df_out) <- c('Date', out_name)
  
  return(df_out)
  
}# fin funcion