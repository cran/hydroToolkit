#' Set a threshold
#' 
#' @description Set tolerable extreme values (maximum or minimum). Records greater or equal than ('>=') or lesser or equal than ('<=') 'threshold' argument are set to \code{NA_real_}.
#'
#' @param x numeric vector or data frame with a numeric series in the second column. 
#' @param threshold numeric value with threshold.
#' @param case string with either '>=' (greater or equal than) or '<=' (lesser or equal than) symbol.
#'
#' @return Numeric vector or data frame with values greater (or lesser) or equal than 'threshold' set as \code{NA_real_}.
#' 
#' @export
#'
#' @examples
#' # Relative path to raw data
#' full_path <- system.file('extdata', package = "hydroToolkit")
#' 
#' # Read IANIGLA file
#' cuevas <- read_IANIGLA(file = 'Cuevas.csv', path = full_path)             
#' 
#' # Set threshold from air temperature series
#' tair_thres <- set_threshold(x = cuevas, threshold = 40)
#' 
set_threshold <- function(x, threshold, case = '>='){
  
  #**************************************
  # Condicionales
  #**************************************
  # clases en general
  if(class(x) != 'data.frame' & class(x) != 'numeric'){
    return('x argument must be either data frame or numeric')
  }
  
  # x como data frame
  if(class(x) == 'data.frame'){
    if(ncol(x) < 2){
      return('x data frame should have unless two columns. This function always works on the second column.')
    }
    
    if(class(x[ , 2]) != 'numeric'){
      return('x[ , 2] must be of class numeric')
    }
  }
  
  # threshold
  if(class(threshold) != 'numeric'){
    return('threshold argument should be numeric')
  }
  
  if( is.na(threshold) == TRUE ){
    return('NA_real_ is not a valid threshold argument')
  }
  
  
  if(length(threshold) != 1){
    return('threshold argument must be of length one')
  }
  
  # case
  if(is.character(case) == FALSE){
    return('case class should be character')
  }
  
  if(length(case) != 1){
    return('case should be of length one')
  }
  
  if(case != '>=' & case != '<='){
    return('case argument should be either >= or <=')
  }
  
  #*************************************
  #*************************************
  
  if(class(x) == 'numeric'){
    
    if(case == '>='){
      indice <- which(x >= threshold)
      
    } else if(case == '<='){
      indice <- which(x <= threshold)
      
    }
    
    
    x[indice] <- NA_real_
    return(x)
    
  } else {
    # no coloco que sea data frame porque eso iría restringido en 
    # los condicionales
    aux_x <- x[ , 2]
    
    if(case == '>='){
      indice <- which(aux_x >= threshold)
      
    } else if(case == '<='){
      indice <- which(aux_x <= threshold)
      
    }
    
    aux_x[indice] <- NA_real_
    
    df_out <- data.frame(x[ , 1], aux_x)
    
    colnames(df_out) <- colnames(x)[1:2]
    return(df_out)
    
  }
  
}