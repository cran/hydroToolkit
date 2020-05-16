#' Moving average windows
#' 
#' @description Smooth a numeric serie with a moving average windows
#'
#' @param df data frame with the serie that you want to smooth. By default, t he function uses column 2.
#' @param k numeric value with windows size., e.g.: 5 
#' @param pos string with the position of the window: \itemize{
#'      \item 'izq': left aligned. The output value is on the left, so the function weights the (k - 1) values at the right side.
#'      \item 'der': right aligned. The output value is on the right, so the function weights the (k - 1) values at the left side.
#'      \item 'cen': center. The output value is in the middle of the window.
#' }
#'        
#' @return data frame with the smooth serie. 
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
#' # Get air temperature
#' cuevas_tair <- cuevas[ , 1:2]
#' 
#' # Create a moving average serie of Tair
#' Tair_mov <- movAvg(df = cuevas_tair, k = 10, pos = 'izq')
#' 
movAvg <- function(df, k, pos){
  #***************************************
  # Condicionales
  #***************************************
    # data frame
  if(is.data.frame(df) == FALSE){
    return('df argument must be a data frame')
  }
  
    # Vector numérico
  x <- df[ , 2] # para compatibilizar con la funcion original
  
  if(is.numeric(x) == FALSE){
    return('x argument must be a numeric vector')
  }
  
    # largo de la serie
  if(length(x) < 3 ){
    return('x length must be greater than 3')
    }
    
    # Tamaño de la ventana
  if(k < 2){return('k must be >= 2')}
  
    # verifico posición
  if(pos != 'izq' & pos != 'der' & pos != 'cen'){
    return('Please insert a valid pos value')
  }
  
  #***************************************
  #***************************************
  # Longitud de la serie
  N <- length(x)
  
  out  <- rep(NA, N)
  
  if(pos == 'cen'){
    flag <- k %% 2
    
    if(flag == 1){# caso ventana impar
      der <- izq <- trunc(k / 2)
      
      for(i in 1:N){
        WinSize <- (i - izq):(i + der)
        
        indices <- which(WinSize > 0 & WinSize <= N)
        pos     <- WinSize[indices]
        
        out[i] <- mean(x[pos], na.rm = TRUE)
        
        rm(i, WinSize, pos)
      } # end loop for
      
      
    } else { # caso ventana par
      der <- k / 2
      izq <- k / 2 - 1
      
      for(i in 1:N){
        WinSize <- (i - izq):(i + der)
        
        indices <- which(WinSize > 0 & WinSize <= N)
        pos     <- WinSize[indices]
        
        out[i] <- mean(x[pos], na.rm = TRUE)
        
        rm(i, WinSize, pos)
      }# end for
    }
    
  } else if(pos == 'izq'){
    WinSize <- k - 1
    
    for(i in 1:N){
      der <- i:(i + WinSize)
      
      indices <- which(der <= N)
      pos     <- c(der)[indices]
      
      out[i] <- mean(x[pos], na.rm = TRUE)
      
      rm(der, indices, pos, i)
    }# end for
    
  } else if(pos == 'der'){
    WinSize <- k - 1
    
    for(i in 1:N){
      izq <- i:(i - WinSize)
      
      indices <- which(izq > 0)
      pos     <- c(izq)[indices]
      
      out[i] <- mean(x[pos], na.rm = TRUE)
      
      rm(izq, indices, pos, i)
    }# end for
    
    
  } # end if else conditions
  
  out <- round(out, 2)
  
  df_out <- data.frame(Date = df[ , 1], out)
  colnames(df_out) <- colnames(df)
  return(df_out)
}

