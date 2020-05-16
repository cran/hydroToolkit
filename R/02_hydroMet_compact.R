# **********************************************************
# Author       : Ezequiel Toum
# Licence      : GPL V3 
# Institution  : IANIGLA-CONICET 
# e-mail       : etoum@mendoza-conicet.gob.ar
# **********************************************************
#
#' \code{hydroMet} subclass for compact data
#'
#' @description This subclass is useful for storing in a single data frame ready to use hydro-meteorological series or many variables of the same kind (e.g. lets say precipitacion series).
#'
#' @slot compact data.frame with Date as first column (class 'Date' or 'POSIXct'). All other columns are the numeric hydro-meteorological variables (double). This subclass was though to join in a single table ready to use data (e.g. in modelling). You can also use it to put together variables of the same kind (e.g. precipitation records) to make some regional analysis.
#'
#' @return A hydroMet_compact class object.
#' 
#' @export
#'
hydroMet_compact <- setClass(
  # Nombre de la clase
  'hydroMet_compact',
  
  # Definino slots nuevos
  slots = c(
    compact  = 'data.frame' # Date | ...(todas las variables de interes)
    ),
  
  # Valores por defecto (opcional)
  prototype = list(),
  
  # Controles
  validity = function(object)
  {
    # compact
    if( dim(object@compact)[1] != 0 ){
      if(class(object@compact) != 'data.frame'){return('compact class must be data.frame')}
      if(class(object@compact[ , 1])[1] != 'Date'){
        if(class(object@compact[ , 1])[1] != 'POSIXct'){
          return('compact[ , 1] class must be Date or POSIXct')
        }
      }
      if(typeof( as.matrix(object@compact[ , -1])  ) != 'double'){return('compact[ , -1] elements must be of double type')}
    }
    
    
    return(TRUE)
  },
  
  # Set the inheritance for this class
  contains = 'hydroMet'
)