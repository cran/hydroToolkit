# **********************************************************
# Author       : Ezequiel Toum
# Licence      : GPL V3 
# Institution  : IANIGLA-CONICET 
# e-mail       : etoum@mendoza-conicet.gob.ar
# **********************************************************
#
#' \code{hydroMet} subclass for IANIGLA (Instituto Argentino de Nivología, Glaciología y Ciencias Ambientales) data
#'
#' @description A suitable object for store hydro-meteorological data provided by IANIGLA.
#'
#' @slot date time serie of dates (class \code{POSIXct} or \code{Date}).
#' @slot tair numeric matrix with air temperature. 
#' @slot hr numeric matrix with relative humidity.
#' @slot patm numeric matrix with atmospheric pressure. 
#' @slot precip numeric matrix with precipitacion. 
#' @slot wspd numeric matrix with wind speed.
#' @slot wdir numeric matrix with wind direction.
#' @slot kin  numeric matrix with incoming short-wave radiation. 
#' @slot hsnow numeric matrix with snow height.
#' @slot tsoil numeric matrix with soil temperature.
#' @slot hwat numeric matrix with stream water level.
#'
#' @return A hydroMet_IANIGLA class object. 
#' @export
#'
hydroMet_IANIGLA <- setClass(
  # Nombre de la clase
  'hydroMet_IANIGLA',
  
  # Definino slots nuevos
  slots = c(
    date   = 'ANY', # serie de fechas 
    tair   = 'matrix',
    hr     = 'matrix',
    patm   = 'matrix',
    precip = 'matrix',
    wspd   = 'matrix',
    wdir   = 'matrix',
    kin    = 'matrix',
    hsnow  = 'matrix',
    tsoil  = 'matrix',
    hwat   = 'matrix'
    ),
  
  # Valores por defecto (opcional)
  prototype = list(
    date   = ISOdate(year = 2020, month = 01, day = 15, tz = 'UTC'),
    tair   = matrix(NA_real_),
    hr     = matrix(NA_real_),
    patm   = matrix(NA_real_),
    precip = matrix(NA_real_),
    wspd   = matrix(NA_real_),
    wdir   = matrix(NA_real_),
    kin    = matrix(NA_real_),
    hsnow  = matrix(NA_real_),
    tsoil  = matrix(NA_real_),
    hwat   = matrix(NA_real_)
  ),
  
  # Controles
  validity = function(object)
  {
    # date
      # date: clase de los datos
    if(class(object@date)[1] != 'POSIXct' & class(object@date)[1] != 'Date'){
      return('date class must be POSIXct or Date')}
      
    # tair 
      # tair: clase de los datos
    if(typeof(object@tair) != 'double' & typeof(object@tair) != 'integer'){return('tair should contain double or integer')}
    
      # tair: nombre de las columnas
    # if( is.null( colnames(object@tair) ) == T){
    #   colnames(object@tair) <- paste0('tair_', 1:ncol(object@tair)) 
    # }
    
    # hr
      # hr: clase de los datos
    if(typeof(object@hr) != 'double' & typeof(object@hr) != 'integer'){return('hr should contain double or integer')}
    
      # hr: nombre de las columnas
    # if( is.null( colnames(object@hr) ) == T){
    #   colnames(object@hr) <- paste0('hr_', 1:ncol(object@hr))
    # }
    
    # patm
      # patm: clase de los datos
    if(typeof(object@patm) != 'double' & typeof(object@patm) != 'integer'){return('patm should contain double or integer')}
    
      # patm: nombre de las columnas
    # if( is.null( colnames(object@patm) ) == T){
    #   colnames(object@patm) <- paste0('patm_', 1:ncol(object@patm))
    # }
    
    # precip
      # precip: clase de los datos
    if(typeof(object@precip) != 'double' & typeof(object@precip) != 'integer'){return('precip should contain double or integer')}
    
      # precip: nombre de las columnas
    # if( is.null( colnames(object@precip) ) == T){
    #   colnames(object@precip) <- paste0('precip_', 1:ncol(object@precip))
    # }
    
    # wspd
      # wspd: clase de los datos
    if(typeof(object@wspd) != 'double' & typeof(object@wspd) != 'integer'){return('wspd should contain double or integer')}
    
      # wspd: nombre de las columnas
    # if( is.null( colnames(object@wspd) ) == T){
    #   colnames(object@wspd) <- paste0('wspd_', 1:ncol(object@wspd))
    # }
    
    # wdir
      # wdir: clase de los datos
    if(typeof(object@wdir) != 'double' & typeof(object@wdir) != 'integer'){return('wdir should contain double or integer')}
   
      # wdir: nombre de las columnas
    # if( is.null( colnames(object@wdir) ) == T){
    #   colnames(object@wdir) <- paste0('wdir_', 1:ncol(object@wdir))
    # }
    
    
    # kin
      # kin: clase de los datos
    if(typeof(object@kin) != 'double' & typeof(object@kin) != 'integer'){return('kin should contain double or integer')}
    
      # kin: nombre de las columnas
    # if( is.null( colnames(object@kin) ) == T){
    #   colnames(object@kin) <- paste0('kin', 1:ncol(object@kin))
    # }
    
    # hsnow
      # hsnow: clase de los datos
     if(typeof(object@hsnow) != 'double' & typeof(object@hsnow) != 'integer'){return('hsnow should contain double or integer')}
    
      # hsnow: nombre de las columnas
    # if( is.null( colnames(object@hsnow) ) == T){
    #   colnames(object@hsnow) <- paste0('tair_', 1:ncol(object@hsnow))
    # }
    
    # tsoil
      # tsoil: clase de los datos
    if(typeof(object@tsoil) != 'double' & typeof(object@tsoil) != 'integer'){return('tsoil should contain double or integer')}
    
      # tsoil: nombre de las columnas
    # if( is.null( colnames(object@tsoil) ) == T){
    #   colnames(object@tsoil) <- paste0('tsoil_', 1:ncol(object@tsoil))
    # }
    
    # hwat
      # hwat: clase de los datos
    if(typeof(object@hwat) != 'double' & typeof(object@hwat) != 'integer'){return('hwat should contain double or integer')}
    
      # hwat: nombre de las columnas
    # if( is.null( colnames(object@hwat) ) == T){
    #   colnames(object@hwat) <- paste0('hwat_', 1:ncol(object@hwat))
    # }
    
    
    
    return(TRUE)
  },
  
  # Set the inheritance for this class
  contains = 'hydroMet'
)