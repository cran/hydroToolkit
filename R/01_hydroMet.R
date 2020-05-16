# **********************************************************
# Author       : Ezequiel Toum
# Licence      : GPL V3 
# Institution  : IANIGLA-CONICET 
# e-mail       : etoum@mendoza-conicet.gob.ar
# **********************************************************
#
#' \code{hydroMet} superclass object
#' 
#' @description A suitable object for store basic information about an hydro-meteorological station. 
#'
#' @slot id numeric. This is the ID assigned by the agency.  
#' @slot agency character. The name of the agency (or institution) that provides the data of the station. 
#' @slot station character. The name of the (hydro)-meteorological station. 
#' @slot lat numeric. Latitude of the station. 
#' @slot long numeric. Longitude of the station
#' @slot alt numeric. Altitude of the station. 
#' @slot country character. Country where the station is located. Argentina is set as default value. 
#' @slot province character. Name of the province where the station is located. Mendoza is set as default value. 
#' @slot river character. Basin river's name. 
#' @slot active logical. It indicates whether or not the station is currently operated. Default value is \code{TRUE}.
#'
#' @return A basic hydroMet class object. 
#' 
#' @importFrom methods new
#' 
#' @export
#'
hydroMet <- setClass(
  # Nombre de la clase
  'hydroMet',
  
  # Defino los slots
  slots = c(
    id       = 'numeric',
    agency   = 'character',
    station  = 'character',
    lat      = 'numeric',
    long     = 'numeric',
    alt      = 'numeric',
    country  = 'character',
    province = 'character',
    river    = 'character',
    active   = 'logical'
    
  ), # fin slot
  
  # Valores por defecto
  prototype = list(
    id       = NA_real_,
    agency   = NA_character_,
    station  = NA_character_,
    lat      = NA_real_,
    long     = NA_real_,
    alt      = NA_real_,
    country  = 'Argentina',
    province = 'Mendoza',
    river    = NA_character_,
    active   = TRUE
    
  ),# fin prototype
  
  # Condicionales 
  validity = function(object){
    if(length(object@id) > 1){return('id should be of length one')}
    
    if(length(object@lat) > 1){return('lat should be of length one')}
    
    if(length(object@long) > 1){return('long should be of length one')}
    
    if(length(object@alt) > 1){return('alt should be of length one')}
    
    return(TRUE)
    
  }# fin validity
  
)# fin clase
  