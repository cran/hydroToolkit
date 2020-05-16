#' Reads data provided by IANIGLA
#' 
#' @description  Reads the data provided by IANIGLA (Instituto Argentino de Nivologia, Glaciologia y Ciencias Ambientales). 
#'
#' @param file string with the name of the '.csv' file downloaded from the meteo-stations web page.
#' @param all logical value indicating whether the returned data frame contain all the original columns or just the date and data. 
#' @param path  string with the files directory. If not provided, the function will use the current working directory. 
#'
#' @note In this package version we only provide functionality for a specific data-set generated in the institute.  
#'
#' @return A data frame containing the hourly data measured by the automatic weather stations. Gaps between dates are filled with NA_real_ and duplicated rows are eliminated automatically.
#' 
#' @importFrom utils read.csv
#' 
#' @export
#'
#' @examples 
#' # Relative path to raw data
#' full_path <- system.file('extdata', package = "hydroToolkit")
#' 
#' # Apply function
#' cuevas <- read_IANIGLA(file = 'Cuevas.csv', path = full_path)             
#' 
read_IANIGLA <- function(file, all = FALSE, path = NULL) {
  #******************************
  ## Condicionales
  #******************************
  
  # file: caracter
  if( is.character(file) == FALSE ){
    return('file argument must be a character string')
  }
  
  # file: un solo string
  if( length(file) > 1 ){
    return('file argument must be a string of length one')
  }
  
  # file: existe el archivo
  if( is.null(path) == TRUE){
    archivos <- list.files(pattern = '.csv')
    existe   <- grep(pattern = paste0('^', file, '$'), x = archivos)
    
    if(length(existe) == 0){
      return('The file does not exist or is misspell')
    }
    
    if(length(existe) > 1){
      return('Multiple files have the same name')
    }
    
  } else{
    # path: caracter
    if( is.character(path) == FALSE ){
      return('path must be of class character')
    }
    
    # path: longitud
    if( length(path) != 1 ){
      return('You should provide a single path')
    }
    
    # path: direccion valida
    if( length( dir(path = path) ) == 0 ){
      return('You provide an invalid directory path. Please check the path!')
    }
    
    archivos <- list.files(path = path, pattern = '.csv')
    existe   <- grep(pattern = paste0('^', file, '$'), x = archivos)
    
    if(length(existe) == 0){
      return('The file does not exist or is misspell')
    }
    
    if(length(existe) > 1){
      return('Multiple files have the same name')
    }
  }
  # all: es logico
  if( is.logical(all) == FALSE){
    return('all argument must be TRUE or FALSE')
  }
  
  # all: uno solo
  if( length(all) > 1 ){
    return('all argument must be of length one')
  }
  
  #******************************
  #******************************
  if( is.null(path) == TRUE ){
    file_path <- file
    
  } else{
    file_path <- paste0(path, '/', file)
  }
  
  
  if (all == TRUE) {
    out <- read.csv(file = file_path, header = TRUE, stringsAsFactors = FALSE,  
                    col.names = c('Station', 'Date', 'Jul_day', 'Hour', 'Bat(V)', 
                                  'Tair(\u00B0C)', 'HR(%)', 'Patm(mbar)', 'P(mm)', 'Wspd(km/hr)',
                                  'Wdir(\u00B0)', 'Kin(kW/m2)', 'Hsnow(cm)', 'Tsoil(\u00B0C)' ), check.names = FALSE )
    
    out[ , 2] <- as.POSIXct(out[ , 2], tz = 'UTC')
    
    ## Por las dudas reviso que no haya baches en la serie
    # Obtenemos el tamano de la serie
    N <- length(out[ , 1])
    
    # Primera y ultima fecha
    time.min <- out[1, 2]
    time.max <- out[N, 2]
    
    # Genero secuencia completa
    all.dates <- seq(from = time.min, to = time.max, by = 'hour')
    
    # Convierto las fechas a formato data.frame para hacer el merge
    all.dates.frame <- data.frame(Date = all.dates)
    
    # Uno los dos data.frame
    merged.data <- merge(all.dates.frame, out, all = TRUE)
    
    
  } else {
    out <- read.csv(file = file_path, header = TRUE,  stringsAsFactors = FALSE,
                    col.names = c('Station', 'Date', 'Jul_day', 'Hour', 'Bat(V)', 
                                  'Tair(\u00B0C)', 'HR(%)', 'Patm(mbar)', 'P(mm)', 'Wspd(km/hr)',
                                  'Wdir(\u00B0)', 'Kin(kW/m2)', 'Hsnow(cm)', 'Tsoil(\u00B0C)' ), check.names = FALSE)[c(2, 6:14)]
    
    out[ , 1] <- as.POSIXct(out[ , 1], tz = 'UTC')
    
    ## Por las dudas reviso que no haya baches en la serie
    # Obtenemos el tamano de la serie
    N <- length(out[ , 1])
    
    # Primera y ultima fecha
    time.min <- out[1, 1]
    time.max <- out[N, 1]
    
    # Genero secuencia completa
    all.dates <- seq(from = time.min, to = time.max, by = 'hour')
    
    # Convierto las fechas a formato data.frame para hacer el merge
    all.dates.frame <- data.frame(Date = all.dates)
    
    # Uno los dos data.frame
    merged.data <- merge(all.dates.frame, out, all = TRUE)
    
   
  }
  
  # Elimino filas duplicadas
  merged.data <- unique.data.frame(merged.data)
  
  return(merged.data)
}


              


