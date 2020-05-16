#' Hydrological year classification
#'
#' @description This function allows you to get the hydrological year. The criteria is consistent with the one of \href{http://www.irrigacion.gov.ar/dgi/}{Departamento General de Irrigacion} (Mendoza - Argentina).
#'
#' @param df a data frame with total annual volumes discharges created with \link{agg_serie} function. 
#'
#' @return A data frame containing the hydrological classification for each year.
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
#' # Aggregate data frame to get total annual discharges
#' AD  <- agg_serie(df = Qmm_vol, fun = 'sum', period = 'annual', out_name = 'Ann_vol',
#'          start_month = 7, end_month = 6, allow_NA = 2)
#'          
#' # Get hydrological year classification
#' AD_class <- hydro_year_DGI(df = AD)
#'                
hydro_year_DGI <- function(df){
  
  # Condicionales 
    # data frame
  if(is.data.frame(df) == FALSE){
    return('df must be of class data frame')
    
  }
  
    # reviso class date
  if(class(df[ , 1]) != 'Date' ){
    return('First df column class must be Date')
  }
  if(class(df[ , 2]) != 'Date' ){
    return('Second df column class must be Date')
  }
  
    # reviso class numeric
  if(class(df[ , 3]) != 'numeric'){
    return('Column two must be of class numeric')
    
  }
  
  ###
  
  N  <- nrow(df)
  mu <- mean(df[ , 3], na.rm = TRUE)
  
  # Obtengo el ano hidrologico
  anoH <- rep('NA', N)
  for(i in 1:N){
    # Verifico si es NA
    
    if(is.na(df[i, 3]) == FALSE){
      # Clasifico
      if(df[i, 3] < 0.65 * mu){
        anoH[i] <- 'Seco'
        
      } else if(df[i, 3] < 0.85 * mu){
        anoH[i] <- 'Pobre'
        
      } else if(df[i, 3] < 0.95 * mu){
        anoH[i] <- 'Medianamente_pobre'
        
      } else if(df[i, 3] < 1.05 * mu){
        anoH[i] <- 'Medio'
        
      } else if(df[i, 3] < 1.15 * mu){
        anoH[i] <- 'Medianamente_rico'
        
      } else if(df[i, 3] < 1.35 * mu){
        anoH[i] <- 'Rico'
        
      } else {
        anoH[i] <- 'Extraordinario'
        
      }
      
    } 
      
  }# fin loop for
  
  # Salida
  df_out <- data.frame(df, Hydro_Year = anoH, check.names = FALSE) 
  
  return(df_out)
  
} # fin funcion