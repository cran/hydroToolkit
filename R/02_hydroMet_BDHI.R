# **********************************************************
# Author       : Ezequiel Toum
# Licence      : GPL V3 
# Institution  : IANIGLA-CONICET 
# e-mail       : etoum@mendoza-conicet.gob.ar
# **********************************************************
#
#' \code{hydroMet} subclass for BDHI (Base de Datos Hidrologica Integrada) data
#'
#' @description An suitable object for store hydro-meteorological data from BDHI.
#'
#' @slot Qmd data.frame from \link{read_BDHI} containing daily mean river discharge [m3/s].
#' @slot Qmm data.frame from \link{read_BDHI} containing monthly mean river discharge [m3/s].
#' @slot precip data.frame from \link{read_BDHI} containing daily liquid precipitation [mm].
#' @slot tdb data.frame from \link{read_BDHI} containing subdaily dry bulb temperature [ºC].
#' @slot tmax data.frame from \link{read_BDHI} containing daily maximum air temperature [ºC].
#' @slot tmin data.frame from \link{read_BDHI} containing daily minimum air temperature [ºC].
#' @slot swe data.frame from \link{read_BDHI} containing daily snow water equivalent [mm].
#' @slot hr data.frame from \link{read_BDHI} containing subdaily relative humidity [\%].
#' @slot wspd data.frame from \link{read_BDHI} containing subdaily wind speed [km/hr].
#' @slot wdir data.frame from \link{read_BDHI} containing subdaily wind direction [º].
#' @slot evap data.frame from \link{read_BDHI} containing daily pan-evaporation [mm].
#' @slot anem data.frame from \link{read_BDHI} containing daily wind speed above the evap tank [km/hr].
#' @slot patm data.frame from \link{read_BDHI} containing subdaily atmospheric pressure [mbar].
#'
#' @return A hydroMet_BDHI class object. 
#' 
#' @export
#'
hydroMet_BDHI <- setClass(
  # Nombre de la clase
  'hydroMet_BDHI',
  
  # Definino slots nuevos
  slots = c(
    Qmd    = 'data.frame', # caudal medio diario (m3/s)
    Qmm    = 'data.frame', # caudal medio mensual (m3/s)
    precip = 'data.frame', # precipitación diaria (mm/d)
    tdb    = 'data.frame', # temperatura de bulbo seco (ºC)
    tmax   = 'data.frame', # temperatura máxima diaria (ºC)
    tmin   = 'data.frame', # temperatura mínima diaria (ºC)
    swe    = 'data.frame', # equivalente agua nieve (mm)
    hr     = 'data.frame', # humedad relativa (%)
    wspd   = 'data.frame', # velocidad del viento (km/hr)
    wdir   = 'data.frame', # dirección del viento (º)
    evap   = 'data.frame', # evaporación (mm)
    anem   = 'data.frame', # velocidad del viento encima del tanque de evap. (km/hr)
    patm   = 'data.frame'  # presión atmosférica (mbar)
    #misc   = 'list'
  ),
  
  # Valores por defecto (opcional)
  prototype = list(),
  
  # Controles
  validity = function(object)
  {
    # Qmd
    if( dim(object@Qmd)[1]  != 0 ){
      if(class(object@Qmd) != 'data.frame'){return('Qmd class must be data.frame')}
      if(class(object@Qmd[ , 1]) != 'Date'){return('Qmd[ , 1] class must be Date')}
      if(class(object@Qmd[ , 2]) != 'numeric'){return('Qmd[ , 2] class must be numeric')}
    }
    
    
    # Qmm
    if( dim(object@Qmm)[1]  != 0 ){
      if(class(object@Qmm) != 'data.frame'){return('Qmm class must be data.frame')}
      if(class(object@Qmm[ , 1]) != 'Date'){return('Qmm[ , 1] class must be Date')}
      if(class(object@Qmm[ , 2]) != 'numeric'){return('Qmm[ , 2] class must be numeric')}  
    }
    
    
    # precip
    if( dim(object@precip)[1]  != 0 ){
      if(class(object@precip) != 'data.frame'){return('precip class must be data.frame')}
      if(class(object@precip[ , 1]) != 'Date'){return('precip[ , 1] class must be Date')}
      if(class(object@precip[ , 2]) != 'numeric'){return('precip[ , 2] class must be numeric')}  
    }
    
    
    # tdb
    if( dim(object@tdb)[1]  != 0 ){
      if(class(object@tdb) != 'data.frame'){return('tdb class must be data.frame')}
      if(class(object@tdb[ , 1])[1] != 'POSIXct' & class(object@tdb[ , 1])[1] != 'Date') { 
        return('tdb[ , 1] class must be POSIXct or Date')}
      if(class(object@tdb[ , 2]) != 'numeric'){return('tdb[ , 2] class must be numeric')}  
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
    
    
    # swe
    if( dim(object@swe)[1]  != 0 ){
      if(class(object@swe) != 'data.frame'){return('swe class must be data.frame')}
      if(class(object@swe[ , 1])[1] != 'Date' & class(object@swe[ , 1])[1] != 'POSIXct'){
        return('swe[ , 1] class must be Date or POSIXct')}
      if(class(object@swe[ , 2]) != 'numeric'){return('swe[ , 2] class must be numeric')}  
    }
    
    
    # hr
    if( dim(object@hr)[1]  != 0 ){
      if(class(object@hr) != 'data.frame'){return('hr class must be data.frame')}
      if(class(object@hr[ , 1])[1] != 'POSIXct' & class(object@hr[ , 1])[1] != 'Date' ) {
        return('hr[ , 1] class must be POSIXct or Date')}
      if(class(object@hr[ , 2]) != 'numeric'){return('hr[ , 2] class must be numeric')}  
    }
    
    
    # wspd
    if( dim(object@wspd)[1]  != 0 ){
      if(class(object@wspd) != 'data.frame'){return('wspd class must be data.frame')}
      if(class(object@wspd[ , 1])[1] != 'POSIXct' & class(object@wspd[ , 1])[1] != 'Date') {
        return('wspd[ , 1] class must be POSIXct or Date')}
      if(class(object@wspd[ , 2]) != 'numeric'){return('wspd[ , 2] class must be numeric')}  
    }
    
    
    # wdir
    if( dim(object@wdir)[1]  != 0 ){
      if(class(object@wdir) != 'data.frame'){return('wdir class must be data.frame')}
      if(class(object@wdir[ , 1])[1] != 'POSIXct' & class(object@wdir[ , 1])[1] != 'Date') {
        return('wdir[ , 1] class must be POSIXct or Date')}
      if(class(object@wdir[ , 2]) != 'character'){return('wdir[ , 2] class must be character')}  
    }
    
    
    # evap
    if( dim(object@evap)[1]  != 0 ){
      if(class(object@evap) != 'data.frame'){return('evap class must be data.frame')}
      if(class(object@evap[ , 1]) != 'Date'){return('evap[ , 1] class must be Date')}
      if(class(object@evap[ , 2]) != 'numeric'){return('evap[ , 2] class must be numeric')}  
    }
    
    
    # anem
    if( dim(object@anem)[1]  != 0 ){
      if(class(object@anem) != 'data.frame'){return('anem class must be data.frame')}
      if(class(object@anem[ , 1]) != 'Date'){return('anem[ , 1] class must be Date')}
      if(class(object@anem[ , 2]) != 'numeric'){return('anem[ , 2] class must be numeric')}  
    }
    
    
    # patm
    if( dim(object@patm)[1]  != 0 ){
      if(class(object@patm) != 'data.frame'){return('patm class must be data.frame')}
      if(class(object@patm[ , 1])[1] != 'POSIXct' & class(object@patm[ , 1])[1] != 'Date') {
        return('patm[ , 1] class must be POSIXct or Date')}
      if(class(object@patm[ , 2]) != 'numeric'){return('patm[ , 2] class must be numeric')}  
    }
    
    # misc 
    # if(class(misc) != 'list'){
    #   return('misc argument must be class list')
    # }
       
       
    return(TRUE)
  },
  
  # Set the inheritance for this class
  contains = 'hydroMet'
)
