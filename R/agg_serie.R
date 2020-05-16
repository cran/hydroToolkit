#' Aggregates a data frame to a larger time period
#' 
#' @description This is a useful function to easily aggregate your data.
#' 
#' @param df data frame with class \code{Date} or \code{POSIXct} in the first column. The function always aggregates the second column.
#' @param fun string containing one of the following functions: \option{mean}, \option{min}, \option{max} or \option{sum}.
#' @param period string with the period of aggregation: \option{hourly}, \option{daily}, \option{monthly}, \option{annual} or \option{climatic}. \bold{NOTE}: the 'climatic' option returns the all series annual statistics ('fun').
#' @param out_name string with the output column name of the variable to aggregate.
#' @param start_month optional. Numeric value of the first month. It only makes sense if the \option{period} is \option{annual}.   
#' @param end_month optional. Numeric value of the last month. It only makes sense if the \option{period} is \option{annual}.   
#' @param allow_NA optional. Numeric value with the maximum allowed number of \code{NA_real_} values.  By default the function will not tolerate any \code{NA_real_} in an aggregation period (and will return \code{NA_real_} instead).
#'
#' @return A data frame with to columns: the date and the aggregated variable. 
#' 
#' @export
#'
#' @examples
#' # Path to file
#' dgi_path  <- system.file('extdata', package = "hydroToolkit")
#' 
#' toscas <- read_DGI(file = 'Toscas.xlsx', sheet = 'tmean', path = dgi_path)
#' 
#' # Monthly mean temperature
#' m_toscas <- agg_serie(df = toscas, fun = 'mean', period = 'monthly', out_name = 'T_month')
#' 
agg_serie <- function(df, fun, period, out_name, start_month = NULL, end_month = NULL, allow_NA = NULL){
  
  #************************************************
  # Condicionales
  #************************************************
  
    # data frame
  if(is.data.frame(df) == FALSE){
    return('df must be of class data frame')
    
  }
  
    # reviso class date
  if(class(df[ , 1])[1] != 'Date' & class(df[ , 1])[1] != 'POSIXct' ){
    return('First df column class must be Date or POSIXct POSIXt')
  }
  
    # fun
  if(fun != 'mean' & fun != 'min' & fun != 'max' & fun != 'sum' ){
    return('Valid functions are: mean, min, max or sum')
  }
  
    # period
  if(period != 'hourly' & period != 'daily' & period != 'monthly' & period != 'annual' & period != 'climatic') {
    return('Valid periods are: hourly, daily, monthly,  annual or climatic')
  }
  
    # out_name
  if( is.character(out_name) == FALSE ){
    return('stop must be of class character')
  }
  
    # start_month and end_month
  if(period == 'annual' | period == 'climatic'){
    
    if( is.null(start_month) == TRUE & is.null(end_month) == TRUE ){
      start_month <- 1
      end_month   <- 12
      
    } else if(is.numeric(start_month) ==  FALSE | is.numeric(end_month) == FALSE){
      return('Both, start_month and end_month arguments, must be either NULL or numeric')
      
    } else if(start_month < 1 | start_month > 12 | end_month < 1 | end_month > 12){
      return('start_month and end_month arguments must be between 1 and 12' )
      
    }
    
    
  }
  
  
    # start_month y end_month relacionados con climatic
  if(period == 'climatic'){
    if(start_month == 1 & end_month != 12){
      return('If start month is 1, end_month argument must be 12. When using climatic as period you must round off a year')
    }
    
    if(start_month != 1 & end_month != (start_month - 1) ){
      return('When start_month argument is not 1 (January), end_month should be the previous month.')
    }
    
    
  }
  
    # Acondiciono allow_NA
  if(is.null(allow_NA) == TRUE) {
    allow_NA <- 0
    
  } else if(is.numeric(allow_NA) == FALSE){
    return('allow_NA must be either NULL or numeric')
    
  }
  
  #************************************************
  ###
  
  # Formato de fecha
  if(period == 'hourly'){
    formato_fecha <- '%Y-%m-%d %H'
    
  } else if(period == 'daily'){
    formato_fecha <- '%Y-%m-%d'
    
  } else if(period == 'monthly'){
    formato_fecha <- '%Y-%m'
    
  } else if(period == 'climatic'){
    formato_fecha <- '%m'
    
    } else { # annual
    formato_fecha <- '%Y'
    formato_mes    <- '%Y-%m'
    
  }
  
  # Agrego fecha 
  if(period == 'annual'){
    agg_Date  <- format(df[ , 1], format = formato_fecha)
    agg_month <- format(df[ , 1], format = formato_mes)
    
    # Encuentro primer y último año
    Y_first <- agg_Date[1]
    Y_last  <- agg_Date[length(agg_Date)]
    
    # Verifico que existan primer y última fecha en esos periodos
    start_month  <- ifelse(start_month < 10, paste0('0', start_month), start_month ) # doy formato 
    end_month    <- ifelse(end_month < 10, paste0('0', end_month), end_month ) # doy formato
    
    target_first <- paste0(Y_first, '-', start_month)
    target_last  <- paste0(Y_last, '-', end_month)
    
    exist_first <- which(agg_month == target_first)
    exist_last  <- which(agg_month == target_last)
    
      # En caso de que no existan esas fechas sumo y resto un año para la primer y última
      # fecha respectivamente. 
    if(length(exist_first) == 0){
      target_first <- paste0( (as.numeric(Y_first) + 1), '-', start_month )
    }
    
    if(length(exist_last) == 0){
      target_last <- paste0( (as.numeric(Y_last) - 1), '-', end_month )
    }
    
      # Creo un flag para saber si se trata o no del mismo año
    same_year <- ifelse( as.numeric(start_month) < as.numeric(end_month), TRUE , FALSE )
    
  } else {
    agg_Date  <- format(df[ , 1], format = formato_fecha)
    
  }
  
  # Fechas unicas 
  if(period == 'annual'){
    aux_year_first <- as.numeric (substr(x = target_first, start = 1, stop = 4) )
    aux_year_last  <- as.numeric (substr(x = target_last, start = 1, stop = 4) )
    
    # Evaluo segun se trate o no del mismo año
    if(same_year == FALSE){ # años distintos
      # Vectores con fecha de inicio y finalización
      Start_Date <- seq.Date(from = as.Date( paste0(target_first, '-01'), format = '%Y-%m-%d'), 
                             to = as.Date( paste0( (aux_year_last - 1), '-', start_month, '-01'), format = '%Y-%m-%d'),
                             by = 'year' )
      
      End_Date   <- seq.Date(from = as.Date( paste0( (aux_year_first + 1), '-', end_month, '-01'), format = '%Y-%m-%d'), 
                             to = as.Date( paste0(target_last, '-01'), format = '%Y-%m-%d'),
                             by = 'year' )
      
      
    } else { # años iguales
      # Vectores con fecha de inicio y finalización
      Start_Date <- seq.Date(from = as.Date( paste0(target_first, '-01'), format = '%Y-%m-%d'), 
                             to = as.Date( paste0( (aux_year_last), '-', start_month, '-01'), format = '%Y-%m-%d'),
                             by = 'year' )
      
      End_Date   <- seq.Date(from = as.Date( paste0( (aux_year_first), '-', end_month, '-01'), format = '%Y-%m-%d'), 
                             to = as.Date( paste0(target_last, '-01'), format = '%Y-%m-%d'),
                             by = 'year' )
      
      
    }
    
  } else {
    unique_date <- sort( unique(agg_Date) )
    
    # Creo el vector con formato de fecha para la salida 
    if(period == 'hourly'){
      out_Date <- as.POSIXct( paste0(unique_date, ':00:00') )
      
    } else if (period == 'daily') {
      out_Date <- as.Date(unique_date )
      
    } else if (period == 'monthly'){
      out_Date <- as.Date( paste0(unique_date, '-01'), format = '%Y-%m-%d')
      
    } else if( period == 'climatic'){
      out_Date <- seq.Date(from = as.Date('2020-01-01'), to = as.Date('2020-12-01'), by = 'month')
    }
      
    
  }
  
  # Obtengo filas y calculo el valor de interés
    
  if(period != 'annual'){
    # Caso no annual
    
    N   <- length(unique_date)
    out <- rep(NA_real_, N) # creo vector con valores a completar
    
    for(i in 1:N){
      # obtengo el índice
      indice <- which(agg_Date == unique_date[i])
      
      # aplico función
      if(allow_NA == 0){
        # no hay tolerancia
        if(fun == 'mean'){
          out[i] <- mean(df[indice, 2])
          
        } else if(fun == 'max'){
          out[i] <- max(df[indice, 2])
          
        } else if(fun == 'min'){
          out[i] <- min(df[indice, 2])
          
        } else if(fun == 'sum'){
          out[i] <- sum(df[indice, 2])
        }
        
      } else{
        # admito un número de NA's por intervalo de agregación
        num_NA <- length( which(is.na(df[indice, 2]) == TRUE ) )
        
        if(fun == 'mean'){
          out[i] <- ifelse(num_NA <= allow_NA, mean(df[indice, 2], na.rm = TRUE), NA_real_ )
          
        } else if(fun == 'max'){
          out[i] <- ifelse(num_NA <= allow_NA, max(df[indice, 2], na.rm = TRUE), NA_real_ )
          
        } else if(fun == 'min'){
          out[i] <- ifelse(num_NA <= allow_NA, min(df[indice, 2], na.rm = TRUE), NA_real_ )
          
        } else if(fun == 'sum'){
          out[i] <- ifelse(num_NA <= allow_NA, sum(df[indice, 2], na.rm = TRUE), NA_real_ )
          
        }
        
        
        
      }
      
    } # fin loop for
    
    
  } else {
    # Caso anual
    
    N   <- length(Start_Date)
    out <- rep(NA_real_, N)
    
    aux_Start_Date <- as.character( as.Date(Start_Date, format = formato_mes) )
    aux_End_Date   <- as.character( as.Date(End_Date, format = formato_mes) )
    
    for(i in 1:N){
      # obtengo índices
      indice <- which(agg_month >= aux_Start_Date[i] & agg_month <= aux_End_Date[i])
      
      # aplico función
      if(allow_NA == 0){
        # no hay tolerancia
        if(fun == 'mean'){
          out[i] <- mean(df[indice, 2])
          
        } else if(fun == 'max'){
          out[i] <- max(df[indice, 2])
          
        } else if(fun == 'min'){
          out[i] <- min(df[indice, 2])
          
        } else if(fun == 'sum'){
          out[i] <- sum(df[indice, 2])
        }
        
      } else {
        # admito un número de NA's por intervalo de agregación
        num_NA <- length( which(is.na(df[indice, 2]) == TRUE ) )
        
        if(fun == 'mean'){
          out[i] <- ifelse(num_NA <= allow_NA, mean(df[indice, 2], na.rm = TRUE), NA_real_ )
          
        } else if(fun == 'max'){
          out[i] <- ifelse(num_NA <= allow_NA, max(df[indice, 2], na.rm = TRUE), NA_real_ )
          
        } else if(fun == 'min'){
          out[i] <- ifelse(num_NA <= allow_NA, min(df[indice, 2], na.rm = TRUE), NA_real_ )
          
        } else if(fun == 'sum'){
          out[i] <- ifelse(num_NA <= allow_NA, sum(df[indice, 2], na.rm = TRUE), NA_real_ )
          
        }
        
      }
      
    }# fin loop for
    
  } # fin condicional caso annual
  
  # Armo data frame de salida
  if(period == 'annual'){
    df_out <- data.frame(Start_Date, End_Date, out)
    
    colnames(df_out) <- c('Start', 'End', out_name)
  
  } else if(period == 'climatic'){
    df_out <- data.frame(out_Date, out)
    
    if(start_month == 1 & end_month == 12){
      row_order <- 1:12
      
    } else {
      row_order <- c(start_month:12, 1:end_month )
    }
    
    df_out <- df_out[row_order, ]
    
    colnames(df_out) <- c('Date', out_name)
    
  } else {
    df_out <- data.frame(out_Date, out)
    
    colnames(df_out) <- c('Date', out_name)
    
  }
  
 # Salida
  return(df_out)
  
  
} # fin de la función



