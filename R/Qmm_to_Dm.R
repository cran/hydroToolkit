#' River discharge [m3/s] to  volume [hm3]
#' 
#' @description Converts mean monthly river discharge [m3/s] to total volume discharge [hm3].
#'
#' @param df data frame with class Date in the first column. By default the function converts the second column only. If you have daily or hourly data see \link{agg_serie}.
#'
#' @return A data frame with two columns: Date and total volume discharge.
#' 
#' @importFrom lubridate month month<-
#' 
#' @export
#' 
#' @examples
#' # Create BDHI hydro-met station
#' guido <- create_hydroMet(class_name = 'BDHI')
#' 
#' # List with meteorological variables (slots in BDHI's object)
#' cargar <- list('precip', 'Qmd', 'Qmm')
#' 
#' # Now assign as names the files
#' hydro_files   <- list.files( system.file('extdata', package = "hydroToolkit"), pattern = 'Guido' )
#' names(cargar) <- hydro_files
#' 
#' # Build the object with the met records
#' guido <- build_hydroMet(obj = guido, slot_list = cargar, 
#'                path = system.file('extdata', package = "hydroToolkit") )
#'                
#' # Now get mean monthly discharge
#' Qmm <- get_hydroMet(obj = guido, name = 'Qmm')[[1]]
#' 
#' # Get the monthly water volume
#' Qmm_vol <- Qmm_to_Dm(df = Qmm)
#' 
Qmm_to_Dm <- function(df){
  #**************************************
  # Condicionales
  #**************************************
    # data frame
  if(is.data.frame(df) == FALSE){
    return('df must be of class data frame')
    
  }
  
    # reviso class date
  if(class(df[ , 1]) != 'Date' ){
    return('First df column class must be Date')
  }
  
    # reviso class numeric
  if(class(df[ , 2]) != 'numeric'){
    return('Column two must be of class numeric')
    
  }
  
  #**************************************
  #**************************************
  ###
  N <- nrow(df)
  
  # Obtengo vector con cantidad de días de cada en cada mes
  meses_plus <- df[ , 1]
  
  # lubridate::month(meses_plus) <- lubridate::month(meses_plus) + 1
  month(meses_plus) <- month(meses_plus) + 1
  
  dias <- as.integer( format(meses_plus - 1, format = '%d') )
  
  # Calculo el derrame mensual en hm3
  out <- df[ , 2] * dias * 0.0864
  
  # Salida
  df_out <- data.frame(Date = df[ , 1], `Vol(hm3)` = out, check.names = FALSE)
  
  return(df_out)
}