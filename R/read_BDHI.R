#' Reads data from Base de Datos Hidrológica Integrada (BDHI) - Argentina
#' 
#' @description Reads files downloaded from the Base de Datos Hidrológica Integrada (BDHI) as a data frame.
#'
#' @param file string with the name (including extension) of the file.
#' @param colName string with variable name. E.g.: Qmd(m3/s)
#' @param timeStep string with time step: 'month', 'day', 'day/3', '4h' or 'hour'. 
#' \itemize{
#' \item 'day': data recorded once a day 
#' \item 'month': data recorded monthly
#' \item '4h': applies to atmospheric pressure time series only
#' \item 'day/3': applies to wind related variables, relative humidity, and dry bulb temperature'
#' \item 'hour': in case you have to deal with hourly data.   
#' }    
#' @param is.Wdir a logical value indicating if the variable is wind direction. Default value is set to FALSE. 
#'
#' @return A data frame with two columns: date and variable. Gaps between dates are filled with \code{NA_real_} and duplicated rows are eliminated automatically.
#' 
#' @export
#'
#' @examples
#' # Relative path to raw data
#' full_path <- system.file('extdata', package = "hydroToolkit")
#' 
#' # Apply function
#' guido_Qmd <- read_BDHI(file = paste0(full_path, '/Qmd_Mendoza_Guido'), 
#'                 colName = 'Q(m3/s)', timeStep = 'day')
#' 
read_BDHI <- function(file, colName, timeStep, is.Wdir = FALSE){
  # NOTAS: Agregar opción para el archivo aforos 
  
  #********************************
  # Condicionales
  #********************************
  if(is.character(file) == FALSE ){
    return('file argument must be a character with file name')
  }
  
  if(is.character(colName) == FALSE){
    return('colName argument must be of character type')
    }
  
  if(timeStep != 'day' & timeStep != 'month' & timeStep != '4h' & timeStep != 'day/3' & timeStep != 'hour'){
    return('timeStep must be one of the following: day - month - 4h - day/3' - 'hour')
  }
  
  if(is.logical(is.Wdir) == FALSE){
    return('is.Wdir argument must be either TRUE or FALSE')
  }
  
  #********************************
  #********************************
  ###
  
  # Escaneo archivo en forma de lista 
  Lista <- scan(file, what = 'list', sep = '\n', quiet = TRUE)
  linea <- grep(pattern = '<', x = Lista)[1] # para terminar de leer antes de el meatdato que por defecto traen los archivos del BDHI
  
  # Genero  variable 
  if(is.Wdir == TRUE){
    Variable <- rep('NA', linea - 10) 
    
    } else{
    Variable <- rep(NA, linea - 10) 
    
  }
  
  # Extraigo datos
  if(timeStep != '4h' & timeStep != 'day/3' & timeStep != 'hour'){
    # series con resolución diaria o mensual
    Fecha  <- rep('NA', linea - 10)
    
    for(i in 1:(linea - 10)){
      Fecha[i]    <- strsplit(x = Lista[i+3], split = ' ')[[1]][1] 
      Variable[i] <- gsub(',', '.', strsplit(x = Lista[i+3], split = '\t')[[1]][2] )
      rm(i)
    }
    
  } else{
    # series con resolución subdiaria
    # Fecha <- .POSIXct(character(linea - 10), tz = 'ART')
    Fecha <- rep(.POSIXct(xx = NA_real_, tz = 'UTC'), (linea - 10) )
      
    for(i in 1:(linea - 10)){
      # doy formato am - pm
      momento <- substring( strsplit(x = Lista[i+3], split = '\t')[[1]][1], first = 21, last = 24)
      if(momento == 'a.m.'){
        momento <- 'am'
        
      } else{
        momento <- 'pm'
        
      }
      
      # armo nuevamente la fecha
      aux_fecha <- substring( strsplit(x = Lista[i+3], split = '\t')[[1]][1] , first = 1, last = 19)
      
      # asigno fecha y variable || SOLUCIONAR ESTE TEMA....VER PAQUETE CRHMr!!!
      Fecha[i]    <- as.POSIXct( paste(aux_fecha, momento, sep = ' '), format = "%d/%m/%Y %I:%M:%S %p", tz = 'UTC' ) 
      Variable[i] <- gsub(',', '.', strsplit(x = Lista[i+3], split = '\t')[[1]][2] )
      rm(i, momento, aux_fecha)
    }
  }
  
  # Doy formato a serie diaria o mensual
  if(timeStep != '4h' & timeStep != 'day/3' & timeStep != 'hour'){
    Fecha <- as.Date(Fecha, format = '%d/%m/%Y')
    
  } 
  
  
  # Descarto líneas sin valor
  Fecha    <- Fecha[!is.na(Fecha)]
  Variable <- Variable[!is.na(Variable)]
  
  # Armo data.frame de salida
  out <- data.frame(Fecha, Variable)
  colnames(out) <- c('Date', colName)
  
  # Relleno la serie en cuestión
  out <- fill_serie(df = out, colName = colName, timeStep = timeStep)
  
  if(is.Wdir == TRUE){
    out[ , 2] <- as.character(as.character(out[ , 2]))
    
  } else {
    out[ , 2] <- round(as.numeric(as.character(out[ , 2])), digits = 2)
    
  }
  
  rm(Fecha, Variable, Lista, linea)
  
  # me aseguro de sacar filas duplicadas en archivo original (caso EAN_Atuel_Laguna_Atuel)
  out <- unique.data.frame(out)
  
  # me aseguro que las fechas sean unicas (caso tmin en uspallata-uspallata: misma fecha con valroes de tmin distintos) 
  # tomo el primer valor
  out_date <- unique(out[ , 1])
  
  if(length(out_date) == nrow(out)){
    return(out)
    
  } else {
    aux_ind   <- which(diff.Date(out$Date) == 0)
    out_final <- out[-aux_ind, ]
    
    return(out_final)
    
  }
  
  
    
  
  
}