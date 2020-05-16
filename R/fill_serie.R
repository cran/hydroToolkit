#' Find non-reported dates and fill them with \code{NA_real_}
#' 
#' @description This function complete non-reported dates and assign \code{NA_real_} as their value.
#'
#' @param df data frame with date and numeric vector as first and second column respectively.
#' @param colName output colname of the numeric variable, e.g.: 'Qmd(m3/s)'.
#' @param timeStep character with a valid time step: 'day', 'month', '4h', 'day/3', 'hour'.
#'
#' @return A data frame with missing time steps filled with NA's.
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
#' met_fill  <- fill_serie(df = met_serie, colName = 'Temp', timeStep = 'day')
#' 
fill_serie <- function(df, colName,  timeStep){
  
  # Condicionantes
  if(is.data.frame(df) == FALSE){
    return('df must be a data frame object')
  }
  
  if(timeStep != 'day' & timeStep != 'month' & timeStep != '4h' & timeStep != 'day/3' & timeStep != 'hour'){
    return('timeStep must be one of the following: day - month - 4h - day/3 - hour')
  }
  
  # Asigno nombre a columnas del data.frame
  colnames(df) <- c('Date', colName)
  
  # Obtenemos el tamaño de la serie
  N <- length(df[ , 1])
  
  # Primera y última fecha
  time.min <- df[1, 1]
  time.max <- df[N, 1]
  
  # Genro una secuencia con todos los días a rellenar
  if(timeStep == 'day' | timeStep == 'month'){
    all.dates <- seq(from = time.min, to = time.max, by = timeStep)
    
  } else if(timeStep == 'hour'){
    all.dates <- seq(from = time.min, to = time.max, by = timeStep)
    
  } else if(timeStep == '4h'){
    all.dates <- seq(from = as.POSIXct( as.character(time.min), tz = 'ART' ), to = as.POSIXct( as.character(time.max), tz = 'ART' ), by = '4 hour')
    
  } else {
    all.dates <- seq(from = as.POSIXct( as.character(time.min), tz = 'ART' ), to = as.POSIXct( as.character(time.max), tz = 'ART' ), by = '6 hour')
    aux_index <- which(format(all.dates, '%H') == '03')
    all.dates <- all.dates[-aux_index]
    rm(aux_index)
  }
  
  
  # Convierto las fechas a formato data.frame para hacer el merge
  all.dates.frame <- data.frame(Date = all.dates)
  
  # Uno los dos data.frame
  merged.data <- merge(all.dates.frame, df, all = TRUE)
  return(merged.data)
}