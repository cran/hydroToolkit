#' Fill a time interval in a data frame with a specific numeric value
#' 
#' @description Assign specific values to a time interval. 
#'
#' @param df data frame with the first column being the date and the others numeric variables. 
#' @param col numeric vector with column(s) number(s) to be filled.
#' @param value numeric or \code{NA_real_}. This numeric vector contains the elements to be fill in. 
#' @param from character, Date or POSIXct with the first date to be filled. 
#' @param to character, Date or POSIXct with the last date to be filled. 
#'
#' @return A data frame filled with the \option{value} in the specified time period. 
#' 
#' @export
#'
#' @examples
#' # Create a data frame
#' dates   <- seq.Date(from = as.Date('1990-01-01'), to = as.Date('1990-12-01'), by = 'm')
#' met_var <- runif(n = 12, 0, 10)
#' 
#' met_serie <- data.frame(dates, met_var)
#' 
#' # Fill serie
#' met_fill <- fill_serie(df = met_serie, colName = 'Temp', timeStep = 'day')
#' 
#' # Now fill value
#' met_fill <- fill_value(df = met_fill, col = 2, value = 10, from = '1990-02-01', to = '1990-02-15')
#' 
fill_value <- function(df, col, value, from, to){
  
  # Condicionales
  if(is.data.frame(df) == FALSE){
    return('df must be of class data frame')
  }
  
  if(is.numeric(col) == FALSE){
    return('col must be numeric')
  }
  
  aux <- which(is.na(value) == TRUE)
  
  if(is.numeric(value) == FALSE & length(aux) >= 1){
    return('value must be numeric or NA_real_')
  }
  
  if(class(from)[1] != 'Date' & class(from)[1] != 'character' & class(from)[1] != 'POSIXct'){
    return('from must be: character, Date or POSIXct class')
  }
  
  if(class(to)[1] != 'Date' & class(to)[1] != 'character' & class(to)[1] != 'POSIXct'){
    return('to must be a character, Date or POSIXct class')
  }
  
  if(col == 1 | col > ncol(df)){
    return('Please insert a valid col argument value/s')
  }
  
  clase <- class(df[ , 1])[1]
  if(clase != 'Date' & clase != c('POSIXct') ){
    return('Invalid format. Valid classes are Date or POSIXct')
  }
  
  ###
  
  N  <- nrow(df)
  i1 <- which(df[ , 1] == from)
  i2 <- which(df[ , 1] == to)
  
  df[i1:i2, col] <- value
  return(df)
}

