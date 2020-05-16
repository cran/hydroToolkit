# **********************************************************
# Author       : Ezequiel Toum
# Licence      : GPL V3 
# Institution  : IANIGLA-CONICET 
# e-mail       : etoum@mendoza-conicet.gob.ar
# **********************************************************
#
#' \code{hydroMet} subclass for DGI (Departamento General de Irrigación) data
#'
#' @description A suitable object for store hydro-meteorological data from DGI.
#'
#' @slot hsnow data.frame from \link{read_DGI} containing daily snow height [m].
#' @slot swe data.frame from \link{read_DGI} containing daily snow water equivalent [mm].
#' @slot tmean data.frame from \link{read_DGI} containing daily mean air temperature [ºC].
#' @slot tmax data.frame from \link{read_DGI} containing daily max. air temperature [ºC]. 
#' @slot tmin data.frame from \link{read_DGI} containing daily min. air temperature [ºC]. 
#' @slot hr data.frame from \link{read_DGI} containing daily mean relative humidity [\%].
#' @slot patm data.frame from \link{read_DGI} containing daily mean atmospheric pressure [hPa]. 
#'
#' @return A hydroMet_DGI class object. 
#' 
#' @export
#'
hydroMet_DGI <- setClass(
  # Nombre de la clase
  'hydroMet_DGI',
  
  # Definino slots nuevos
  slots = c(
    hsnow  = 'data.frame', # altura de nieve (m)
    swe    = 'data.frame', # equivalente agua nieve (mm)
    tmean  = 'data.frame', # temperatura media diaria (ºC)
    tmax   = 'data.frame', # temperatura máxima diaria (ºC)
    tmin   = 'data.frame', # temperatura mínima diaria (ºC)
    hr     = 'data.frame', # temperatura mínima diaria (ºC)
    patm   = 'data.frame'  # presión atmosférica media diaria (hPa)
  ),
  
  # Valores por defecto (opcional)
  prototype = list(),
  
  # Controles
  validity = function(object)
  {
    # hsnow 
    if( dim(object@hsnow)[1] != 0 ){
      if(class(object@hsnow) != 'data.frame'){return('hsnow class must be data.frame')}
      if(class(object@hsnow[ , 1]) != 'Date'){return('hsnow[ , 1] class must be Date')}
      if(class(object@hsnow[ , 2]) != 'numeric'){return('hsnow[ , 2] class must be numeric')}  
    }
    
    # swe
    if( dim(object@swe)[1] != 0 ){
      if(class(object@swe) != 'data.frame'){return('swe class must be data.frame')}
      if(class(object@swe[ , 1]) != 'Date'){return('swe[ , 1] class must be Date')}
      if(class(object@swe[ , 2]) != 'numeric'){return('swe[ , 2] class must be numeric')}  
    }
    
    
    # tmean
    if( dim(object@tmean)[1] != 0 ){
      if(class(object@tmean) != 'data.frame'){return('tmean class must be data.frame')}
      if(class(object@tmean[ , 1])[1] != 'Date'){return('tmean[ , 1] class must be Date')}
      if(class(object@tmean[ , 2]) != 'numeric'){return('tmean[ , 2] class must be numeric')}  
    }
    
    
    # tmax
    if( dim(object@tmax)[1] != 0 ){
      if(class(object@tmax) != 'data.frame'){return('tmax class must be data.frame')}
      if(class(object@tmax[ , 1]) != 'Date'){return('tmax[ , 1] class must be Date')}
      if(class(object@tmax[ , 2]) != 'numeric'){return('tmax[ , 2] class must be numeric')}  
    }
    
    
    # tmin
    if( dim(object@tmin)[1] != 0 ){
      if(class(object@tmin) != 'data.frame'){return('tmin class must be data.frame')}
      if(class(object@tmin[ , 1]) != 'Date'){return('tmin[ , 1] class must be Date')}
      if(class(object@tmin[ , 2]) != 'numeric'){return('tmin[ , 2] class must be numeric')}  
    }
    
    
    # hr
    if( dim(object@hr)[1] != 0 ){
      if(class(object@hr) != 'data.frame'){return('hr class must be data.frame')}
      if(class(object@hr[ , 1]) != 'Date'){return('hr[ , 1] class must be Date')}
      if(class(object@hr[ , 2]) != 'numeric'){return('hr[ , 2] class must be numeric')}  
    }
   
   
    # patm
    if( dim(object@patm)[1] != 0 ){
      if(class(object@patm) != 'data.frame'){return('patm class must be data.frame')}
      if(class(object@patm[ , 1])[1] != 'Date'){return('patm[ , 1] class must be Date')}
      if(class(object@patm[ , 2]) != 'numeric'){return('patm[ , 2] class must be numeric')}  
    }
    
    
    return(TRUE)
  },
  
  # Set the inheritance for this class
  contains = 'hydroMet'
)