#'  Snow water equivalent to snowfall
#' 
#' @description Converts a snow water equivalent series (from snow pillow) to a snowfall series. 
#'
#' @param df data frame with 'swe' series in the second column. See \code{'read_XXX'} functions. 
#'
#' @return Data frame containing the numeric vector with inferred snowfall. 
#' 
#' @export
#'
#' @examples
#' # Relative path to raw data
#' full_path <- system.file('extdata', package = "hydroToolkit")
#' 
#' # Read swe sheet
#' toscas_swe <- read_DGI(file = 'Toscas.xlsx', sheet = 'swe',
#'                 colName = 'swe(mm)', path = full_path)
#'
#' # swe to snowfall
#' toscas_snfall <- swe_to_precip(df = toscas_swe)    
#' 
swe_to_precip <- function(df) {
  #**************************
  # Condicionales
  #**************************
  # data.frame
  if(is.data.frame(df) == FALSE){
    return('df argument must be a data frame')
  }
  
  # numeric
  if(is.numeric(df[ , 2]) == FALSE){
    return('df column 2 must be numeric')
  }
  
  #**************************
  #**************************
  x <- df[ , 2]
  
  aux <- diff(x)
  aux <- ifelse(aux > 0 , aux, 0)
  
  out <- c(0, aux)
  
  df_out <- data.frame(df[ , 1], out)
  colnames(df_out) <- c('Date', 'Snowfall')
  
  return(df_out)
}