# **********************************************************
# Author       : Ezequiel Toum
# Licence      : GPL V3 
# Institution  : IANIGLA-CONICET 
# e-mail       : etoum@mendoza-conicet.gob.ar
# **********************************************************
#
#' Set the data of an \code{hydroMet} object or its subclasses
#' 
#' @description With this method you can set (or change) an specific slot value. 
#'
#' @param obj an \code{hydroMet} or \code{hydroMet_XXX} class object. 
#' @param id numeric. This is the ID assigned by the agency.
#' @param agency character. The name of the agency (or institution) that provides the data of the station.  
#' @param station character. The name of the (hydro)-meteorological station. 
#' @param lat numeric. Latitude of the station. 
#' @param long numeric. Longitude of the station
#' @param alt numeric. Altitute of the station. 
#' @param country character. Country where the station is located. Argentina is set as default value. 
#' @param province character. Name of the province where the station is located. Mendoza is set as default value. 
#' @param river character. Basin river's name. 
#' @param active logical. It indicates whether or not the station is currently operated. Default value is \code{TRUE}.
#' @param ... arguments to be passed to methods. They rely on the slots of the \code{obj} subclass. 
#' @param Qmd daily mean river discharge. 
#' @param Qmm monthly mean river discharge.
#' @param date time serie with dates.
#' @param precip precipitation.
#' @param tdb dry bulb temperature.
#' @param tmax daily maximum air temperature. 
#' @param tmin daily minimum air temperature.
#' @param tmean daily mean air temperature. 
#' @param tair air temperature.
#' @param swe snow water equivalent. 
#' @param hr relative humidity. 
#' @param wspd wind speed. 
#' @param wdir wind direction.
#' @param evap evaporation. 
#' @param anem wind speed above the pan-evaporation.
#' @param patm atmospheric pressure.
#' @param kin incoming shortwave radiation. 
#' @param hsnow snow height.
#' @param tsoil soil temperature.
#' @param hwat stream water level.
#' @param compact data frame with Date as first column. All other columns are hydro-meteorological variables.
#'
#' @return The hydroMet object with the slots setted.
#' 
#' @importFrom methods callNextMethod validObject
#' 
#' @export
#'
#' @examples
#' # Create BDHI hydro-met station
#' guido <- create_hydroMet(class_name = 'BDHI')
#' 
#' # Assign altitude
#' guido <- set_hydroMet(obj = guido, alt = 2480)
#' 
## Generico
setGeneric(name = 'set_hydroMet',
           def = function(obj = NULL, id = NULL, agency = NULL, station = NULL,
                          lat = NULL, long = NULL, alt = NULL, country = NULL,
                          province = NULL, river = NULL, active = NULL, ...) 
           {
             standardGeneric('set_hydroMet')
           }
)


#' @describeIn set_hydroMet set method for generic object
## hydroMet
setMethod(f = 'set_hydroMet',
          signature = 'hydroMet',
          definition = function(obj = NULL, id = NULL, agency = NULL, station = NULL,
                                lat = NULL, long = NULL, alt = NULL, country = NULL,
                                province = NULL, river = NULL, active = NULL) 
          {
            # obj
            if(is.null(obj) == TRUE){
              return('You must provide a valid obj argument')
            }
            
            # id
            if(is.null(id) == FALSE){
              obj@id <- id
            }
            
            # agency
            if(is.null(agency) == FALSE){
              obj@agency <- agency
            }
            
            # station
            if(is.null(station) == FALSE){
              obj@station <- station
            }
            
            # latitude
            if(is.null(lat) == FALSE){
              obj@lat <- lat
            }
            
            # longitude
            if(is.null(long) == FALSE){
              obj@long <- long
            }
            
            # altitude
            if(is.null(alt) == FALSE){
              obj@alt <- alt
            }
            
            # country
            if(is.null(country) == FALSE){
              obj@country <- country  
            }
            
            # province 
            if(is.null(province) == FALSE){
              obj@province <- province  
            }
            
            # river
            if(is.null(river) == FALSE){
              obj@river <- river
            }
            
            # active
            if(is.null(active) == FALSE){
              obj@active <- active
            }
            
            
            # Validación de los nuevos valores asignados
            validObject(obj)
            
            # Lo que debe regresar el método
            return(obj)
          }
)


#' @describeIn set_hydroMet set method for BDHI object
## BDHI
setMethod(f = 'set_hydroMet',
          signature = 'hydroMet_BDHI',
          definition = function(obj = NULL, id = NULL, agency = NULL, station = NULL,
                                lat = NULL, long = NULL, alt = NULL, country = NULL,
                                province = NULL, river = NULL, active = NULL,
                                Qmd = NULL, Qmm = NULL, precip = NULL, 
                                tdb = NULL, tmax = NULL, tmin = NULL, swe = NULL,
                                hr = NULL, wspd = NULL, wdir = NULL, evap = NULL, 
                                anem = NULL, patm = NULL) 
          {
            # obj
            if(is.null(obj) == TRUE){
              return('You must provide a valid obj argument')
            }
            
            obj <- callNextMethod(obj)
            
            # id
            if(is.null(id) == FALSE){
              obj@id <- id
            }
            
            # agency
            if(is.null(agency) == FALSE){
              obj@agency <- agency
            }
            
            # station
            if(is.null(station) == FALSE){
              obj@station <- station
            }
            
            # latitude
            if(is.null(lat) == FALSE){
              obj@lat <- lat
            }
            
            # longitude
            if(is.null(long) == FALSE){
              obj@long <- long
            }
            
            # altitude
            if(is.null(alt) == FALSE){
              obj@alt <- alt
            }
            
            # country
            if(is.null(country) == FALSE){
              obj@country <- country  
            }
            
            # province 
            if(is.null(province) == FALSE){
              obj@province <- province  
            }
            
            # river
            if(is.null(river) == FALSE){
              obj@river <- river
            }
            
            # active
            if(is.null(active) == FALSE){
              obj@active <- active
            }
            
            
            # Qmd
            if( is.null(Qmd) == FALSE ){
              obj@Qmd <- Qmd
            }
            
            # Qmm 
            if( is.null(Qmm) == FALSE ){
              obj@Qmm <- Qmm
            }
            
            # precip
            if( is.null(precip) == FALSE ){
              obj@precip <- precip
            }
            
            # tdb
            if( is.null(tdb) == FALSE ){
              obj@tdb <- tdb
            }
            
            # tmax
            if( is.null(tmax) == FALSE ){
              obj@tmax <- tmax
            }
            
            # tmin
            if( is.null(tmin) == FALSE ){
              obj@tmin <- tmin
            }
            
            # swe
            if( is.null(swe) == FALSE ){
              obj@swe <- swe
            }
            
            # hr
            if( is.null(hr) == FALSE ){
              obj@hr <- hr
            }
            
            # wspd
            if( is.null(wspd) == FALSE ){
              obj@wspd <- wspd
            }
            
            # wdir
            if( is.null(wdir) == FALSE ){
              obj@wdir <- wdir
            }
            
            # evap
            if( is.null(evap) == FALSE ){
              obj@evap <- evap
            }
            
            # anem
            if( is.null(anem) == FALSE ){
              obj@anem <- anem
            }
            
            # patm
            if( is.null(patm) == FALSE ){
              obj@patm <- patm
            }
            
            # misc
            # if( is.null(misc) == F ){
            #   # agrego un nuevo misc
            #   N <- length(obj@misc)
            #   
            #   obj@misc[(N+1)] <- misc
            # }
            
            # Validación de los nuevos valores asignados
            validObject(obj)
            
            # Lo que debe regresar el método
            return(obj)
          }
)


#' @describeIn set_hydroMet set method for DGI object
## DGI
setMethod(f = 'set_hydroMet',
          signature = 'hydroMet_DGI',
          definition = function(obj = NULL, id = NULL, agency = NULL, station = NULL,
                                lat = NULL, long = NULL, alt = NULL, country = NULL,
                                province = NULL, river = NULL, active = NULL,
                                swe = NULL, tmean = NULL, tmax = NULL, tmin = NULL,
                                hr = NULL, patm = NULL, hsnow = NULL )
          {
            # obj
            if(is.null(obj) == TRUE){
              return('You must provide a valid obj argument')
            }
            
            obj <- callNextMethod(obj)
            
            # id
            if(is.null(id) == FALSE){
              obj@id <- id
            }
            
            # agency
            if(is.null(agency) == FALSE){
              obj@agency <- agency
            }
            
            # station
            if(is.null(station) == FALSE){
              obj@station <- station
            }
            
            # latitude
            if(is.null(lat) == FALSE){
              obj@lat <- lat
            }
            
            # longitude
            if(is.null(long) == FALSE){
              obj@long <- long
            }
            
            # altitude
            if(is.null(alt) == FALSE){
              obj@alt <- alt
            }
            
            # country
            if(is.null(country) == FALSE){
              obj@country <- country  
            }
            
            # province 
            if(is.null(province) == FALSE){
              obj@province <- province  
            }
            
            # river
            if(is.null(river) == FALSE){
              obj@river <- river
            }
            
            # active
            if(is.null(active) == FALSE){
              obj@active <- active
            }
            
            
            # swe
            if( is.null(swe) == FALSE ){
              obj@swe <- swe
            }
            
            # tmean
            if( is.null(tmean) == FALSE ){
              obj@tmean <- tmean
            }
            
            # tmax
            if( is.null(tmax) == FALSE ){
              obj@tmax <- tmax
            }
            
            # tmin
            if( is.null(tmin) == FALSE ){
              obj@tmin <- tmin
            }
            
            # hr
            if( is.null(hr) == FALSE ){
              obj@hr <- hr
            }
            
            # patm
            if( is.null(patm) == FALSE ){
              obj@patm <- patm
            }
            
            # hsnow
            if( is.null(hsnow) == FALSE ){
              obj@hsnow <- hsnow
            }
            
            # Validación de los nuevos valores asignados
            validObject(obj)
            
            # Lo que debe regresar el método
            return(obj)
          }
)


#' @describeIn set_hydroMet set method for IANIGLA object
## IANIGLA
setMethod(f = 'set_hydroMet',
          signature = 'hydroMet_IANIGLA',
          definition = function(obj = NULL,  id = NULL, agency = NULL, station = NULL,
                                lat = NULL, long = NULL, alt = NULL, country = NULL,
                                province = NULL, river = NULL, active = NULL,
                                date = NULL, tair = NULL, hr = NULL, patm = NULL, 
                                precip = NULL, wspd = NULL, wdir = NULL, kin = NULL, 
                                hsnow = NULL, tsoil = NULL, hwat = NULL)
            {
            # obj
            if(is.null(obj) == TRUE){
              return('You must provide a valid obj argument')
            }
            
            obj <- callNextMethod(obj)
            
            # id
            if(is.null(id) == FALSE){
              obj@id <- id
            }
            
            # agency
            if(is.null(agency) == FALSE){
              obj@agency <- agency
            }
            
            # station
            if(is.null(station) == FALSE){
              obj@station <- station
            }
            
            # latitude
            if(is.null(lat) == FALSE){
              obj@lat <- lat
            }
            
            # longitude
            if(is.null(long) == FALSE){
              obj@long <- long
            }
            
            # altitude
            if(is.null(alt) == FALSE){
              obj@alt <- alt
            }
            
            # country
            if(is.null(country) == FALSE){
              obj@country <- country  
            }
            
            # province 
            if(is.null(province) == FALSE){
              obj@province <- province  
            }
            
            # river
            if(is.null(river) == FALSE){
              obj@river <- river
            }
            
            # active
            if(is.null(active) == FALSE){
              obj@active <- active
            }
            
            
            # date
            if(is.null(date) == FALSE){
              obj@date <- date
            }
            
            # tair 
            if(is.null(tair) == FALSE){
              obj@tair <- tair
              
              if( is.null(colnames(obj@tair) ) ){
                colnames(obj@tair) <- paste0('tair', '_', 1:ncol(obj@tair) )
              }
              
            }
            
            # hr 
            if(is.null(hr) == FALSE){
              obj@hr <- hr
              
              if( is.null(colnames(obj@hr) ) ){
                colnames(obj@hr) <- paste0('hr', '_', 1:ncol(obj@hr) )
              }
            }
            
            # patm 
            if(is.null(patm) == FALSE){
              obj@patm <- patm
              
              if( is.null(colnames(obj@patm) ) ){
                colnames(obj@patm) <- paste0('patm', '_', 1:ncol(obj@patm) )
              }
            }
            
            # precip
            if(is.null(precip) == FALSE){
              obj@precip <- precip
              
              if( is.null(colnames(obj@precip) ) ){
                colnames(obj@precip) <- paste0('precip', '_', 1:ncol(obj@precip) )
              }
            }
            
            # wspd
            if(is.null(wspd) == FALSE){
              obj@wspd <- wspd
              
              if( is.null(colnames(obj@wspd) ) ){
                colnames(obj@wspd) <- paste0('wspd', '_', 1:ncol(obj@wspd) )
              }
            }
            
            # wdir
            if(is.null(wdir) == FALSE){
              obj@wdir <- wdir
              
              if( is.null(colnames(obj@wdir) ) ){
                colnames(obj@wdir) <- paste0('wdir', '_', 1:ncol(obj@wdir) )
              }
            }
            
            # kin
            if(is.null(kin) == FALSE){
              obj@kin <- kin
              
              if( is.null(colnames(obj@kin) ) ){
                colnames(obj@kin) <- paste0('kin', '_', 1:ncol(obj@kin) )
              }
            }
            
            # hsnow
            if(is.null(hsnow) == FALSE){
              obj@hsnow <- hsnow
              
              if( is.null(colnames(obj@hsnow) ) ){
                colnames(obj@hsnow) <- paste0('hsnow', '_', 1:ncol(obj@hsnow) )
              }
            }
            
            # tsoil
            if(is.null(tsoil) == FALSE){
              obj@tsoil <- tsoil
              
              if( is.null(colnames(obj@tsoil) ) ){
                colnames(obj@tsoil) <- paste0('tsoil', '_', 1:ncol(obj@tsoil) )
              }
            }
            
            # hwat
            if(is.null(hwat) == FALSE){
              obj@hwat <- hwat
              
              if( is.null(colnames(obj@hwat) ) ){
                colnames(obj@hwat) <- paste0('hwat', '_', 1:ncol(obj@hwat) )
              }
            }
            
            
            # Validación de los nuevos valores asignados
            validObject(obj)
            
            # Lo que debe regresar el método
            return(obj)
            
          }
)


#' @describeIn set_hydroMet set method for CR2 object
# clase hydroMet_CR2
setMethod(f = 'set_hydroMet', 
          signature = 'hydroMet_CR2', 
          definition = function(obj = NULL, id = NULL, agency = NULL, station = NULL,
                                lat = NULL, long = NULL, alt = NULL, country = NULL,
                                province = NULL, river = NULL, active = NULL,
                                precip = NULL, tmean = NULL, tmax = NULL, tmin = NULL)
          {
            # obj
            if(is.null(obj) == TRUE){
              return('You must provide a valid obj argument')
            }
            
            obj <- callNextMethod(obj)
            
            # id
            if(is.null(id) == FALSE){
              obj@id <- id
            }
            
            # agency
            if(is.null(agency) == FALSE){
              obj@agency <- agency
            }
            
            # station
            if(is.null(station) == FALSE){
              obj@station <- station
            }
            
            # latitude
            if(is.null(lat) == FALSE){
              obj@lat <- lat
            }
            
            # longitude
            if(is.null(long) == FALSE){
              obj@long <- long
            }
            
            # altitude
            if(is.null(alt) == FALSE){
              obj@alt <- alt
            }
            
            # country
            if(is.null(country) == FALSE){
              obj@country <- country  
            }
            
            # province 
            if(is.null(province) == FALSE){
              obj@province <- province  
            }
            
            # river
            if(is.null(river) == FALSE){
              obj@river <- river
            }
            
            # active
            if(is.null(active) == FALSE){
              obj@active <- active
            }
            
            # precip 
            if(is.null(precip) == FALSE){
              obj@precip <- precip
            }
            
            # tmean 
            if(is.null(tmean) == FALSE){
              obj@tmean <- tmean
            }
            
            # tmax
            if(is.null(tmax) == FALSE){
              obj@tmax <- tmax
            }
            
            # tmin
            if(is.null(tmin) == FALSE){
              obj@tmin <- tmin
            }
            
            # Validación de los nuevos valores asignados
            validObject(obj)
            
            # Lo que debe regresar el método
            return(obj)
            
          }
)


#' @describeIn set_hydroMet set method for \code{compact} object
# clase hydroMet_compact
setMethod(f = 'set_hydroMet', 
          signature = 'hydroMet_compact', 
          definition = function(obj = NULL, id = NULL, agency = NULL, station = NULL,
                                lat = NULL, long = NULL, alt = NULL, country = NULL,
                                province = NULL, river = NULL, active = NULL,
                                compact = NULL)
          {
            # obj
            if(is.null(obj) == TRUE){
              return('You must provide a valid obj argument')
            }
            
            obj <- callNextMethod(obj)
            
            # id
            if(is.null(id) == FALSE){
              obj@id <- id
            }
            
            # agency
            if(is.null(agency) == FALSE){
              obj@agency <- agency
            }
            
            # station
            if(is.null(station) == FALSE){
              obj@station <- station
            }
            
            # latitude
            if(is.null(lat) == FALSE){
              obj@lat <- lat
            }
            
            # longitude
            if(is.null(long) == FALSE){
              obj@long <- long
            }
            
            # altitude
            if(is.null(alt) == FALSE){
              obj@alt <- alt
            }
            
            # country
            if(is.null(country) == FALSE){
              obj@country <- country  
            }
            
            # province 
            if(is.null(province) == FALSE){
              obj@province <- province  
            }
            
            # river
            if(is.null(river) == FALSE){
              obj@river <- river
            }
            
            # active
            if(is.null(active) == FALSE){
              obj@active <- active
            }
            
            # compact 
            if(is.null(compact) == FALSE){
              obj@compact <- compact
            }
            
           
            # Validación de los nuevos valores asignados
            validObject(obj)
            
            # Lo que debe regresar el método
            return(obj)
            
          }
)
