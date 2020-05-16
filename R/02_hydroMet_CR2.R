# **********************************************************
# Author       : Ezequiel Toum
# Licence      : GPL V3 
# Institution  : IANIGLA-CONICET 
# e-mail       : etoum@mendoza-conicet.gob.ar
# **********************************************************
#
#' \code{hydroMet} subclass for CR2 (Explorador Climático) data
#'
#' @description A suitable object for store hydro-meteorological data from CR2.
#'
#' @slot precip data.frame from \link{read_CR2} containing daily precipitation [mm].
#' @slot tmean data.frame from \link{read_CR2} containing daily mean air temperature [ºC].
#' @slot tmax data.frame from \link{read_CR2} containing daily maximum air temperature [ºC].
#' @slot tmin data.frame from \link{read_CR2} containing daily minimum air temperature [ºC].
#'
#' @return A hydroMet_CR2 class object.
#' 
#' @export
#'
hydroMet_CR2 <- setClass(
  # Nombre de la clase
  'hydroMet_CR2',
  
  # Definino slots nuevos
  slots = c(
    precip = 'data.frame', # precipitación diaria (mm)
    tmean  = 'data.frame', # temperatura media diaria (ºC)
    tmax   = 'data.frame', # temperatura máxima diaria (ºC)
    tmin   = 'data.frame' # temperatura mínima diaria (ºC)
  ),
  
  # Valores por defecto (opcional)
  prototype = list(),
  
  # Controles
  validity = function(object)
  {
    # precip
    if( dim(object@precip)[1]  != 0 ){
      if(class(object@precip) != 'data.frame'){return('precip class must be data.frame')}
      if(class(object@precip[ , 1]) != 'Date'){return('precip[ , 1] class must be Date')}
      if(class(object@precip[ , 2]) != 'numeric'){return('precip[ , 2] class must be numeric')}  
    }
    
    
    # tmean
    if( dim(object@tmean)[1]  != 0 ){
      if(class(object@tmean) != 'data.frame'){return('tmean class must be data.frame')}
      if(class(object@tmean[ , 1])[1] != 'Date'){return('tmean[ , 1] class must be Date')}
      if(class(object@tmean[ , 2]) != 'numeric'){return('tmean[ , 2] class must be numeric')}  
    }
    
    
    # tmax
    if( dim(object@tmax)[1]  != 0 ){
      if(class(object@tmax) != 'data.frame'){return('tmax class must be data.frame')}
      if(class(object@tmax[ , 1]) != 'Date'){return('tmax[ , 1] class must be Date')}
      if(class(object@tmax[ , 2]) != 'numeric'){return('tmax[ , 2] class must be numeric')}  
    }
    
    
    # tmin
    if( dim(object@tmin)[1]  != 0 ){
      if(class(object@tmin) != 'data.frame'){return('tmin class must be data.frame')}
      if(class(object@tmin[ , 1]) != 'Date'){return('tmin[ , 1] class must be Date')}
      if(class(object@tmin[ , 2]) != 'numeric'){return('tmin[ , 2] class must be numeric')}  
    }
    
   
    return(TRUE)
  },
  
  # Set the inheritance for this class
  contains = 'hydroMet'
)