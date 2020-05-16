# **********************************************************
# Author       : Ezequiel Toum
# Licence      : GPL V3 
# Institution  : IANIGLA-CONICET 
# e-mail       : etoum@mendoza-conicet.gob.ar
# **********************************************************
#
#' Get the slot(s) content(s)
#' 
#' @description Extract the slots that you want from an \code{hydroMet} or \code{hydroMet_XXX} class.
#'
#' @param obj an \code{hydroMet} or \code{hydroMet_XXX} class object.
#' @param name a valid single string or vector string with the required slot name(s).
#'
#' @return A list with the slot's data.
#' 
#' @importFrom methods getSlots
#' 
#' @export
#'
#' @examples
#' # Create an IANIGLA object
#' cuevas <- create_hydroMet(class_name = 'IANIGLA')
#' 
#' # Extract one of its slots
#' tair <- get_hydroMet(obj = cuevas, name = 'tair')
#' 
setGeneric(name = 'get_hydroMet',
           def = function(obj, name = NA_character_)
           {
             standardGeneric('get_hydroMet')
           }
)

#' @describeIn get_hydroMet get method for generic hydroMet object 
# hydroMet
setMethod(f = 'get_hydroMet',
          signature = 'hydroMet',
          definition = function(obj, name = NA_character_)
          {
            # Largo de name
            N <- length(name)
            
            # Hay valor(es)?
            if(N == 1){
              if(is.na(name) == TRUE){'Please supply a valid slot(s) name(s) as character(character vector)'}  
            }
            
            
            # Verifico existencia de los slots
            nom_slots <- names(getSlots('hydroMet'))
            coincid   <- match(x = nom_slots, table = name) 
            posicion  <- which( is.na(coincid) == FALSE ) # posición dentro de los slots
            N_coincid <- length( posicion == FALSE )  # cantidad de caracteres que coinciden con los slots
            
            if(N > N_coincid){
              return('Unless one name character is not well specified')
            }
            
            # Obtengo los slots que me interesan como lista
            out <- list()
            
            for(i in 1:N){
              out[[i]] <- eval( parse(text = paste0('obj', '@', name[i]) ) )
            }
            
            names(out) <- name
            
            return(out)
          }
)


#' @describeIn get_hydroMet get method for BDHI class
# hydroMet_BDHI
setMethod(f = 'get_hydroMet',
          signature = 'hydroMet_BDHI',
          definition = function(obj, name = NA_character_)
          {
            #obj <- callNextMethod(obj)
            
            # Largo de name
            N <- length(name)
            
            # Hay valor(es)?
            if(N == 1){
              if(is.na(name) == TRUE){'Please supply a valid slot(s) name(s) as character(character vector)'}  
            }
            
            
            # Verifico existencia de los slots
            nom_slots <- names(getSlots('hydroMet_BDHI'))
            coincid   <- match(x = nom_slots, table = name) 
            posicion  <- which( is.na(coincid) == FALSE ) # posición dentro de los slots
            N_coincid <- length( posicion == FALSE )  # cantidad de caracteres que coinciden con los slots
            
            if(N > N_coincid){
              return('Unless one name character is not well specified')
            }
            
            # Obtengo los slots que me interesan como lista
            out <- list()
            
            for(i in 1:N){
              out[[i]] <- eval( parse(text = paste0('obj', '@', name[i]) ) )
            }
            
            names(out) <- name
            
            return(out)
          }
)


#' @describeIn get_hydroMet get method for DGI class
# hydroMet_DGI
setMethod(f = 'get_hydroMet',
          signature = 'hydroMet_DGI',
          definition = function(obj, name = NA_character_)
          {
            #obj <- callNextMethod(obj)
            
            # Largo de name
            N <- length(name)
            
            # Hay valor(es)?
            if(N == 1){
              if(is.na(name) == TRUE){'Please supply a valid slot(s) name(s) as character(character vector)'}  
            }
            
            
            # Verifico existencia de los slots
            nom_slots <- names(getSlots('hydroMet_DGI'))
            coincid   <- match(x = nom_slots, table = name) 
            posicion  <- which( is.na(coincid) == FALSE ) # posición dentro de los slots
            N_coincid <- length( posicion == FALSE )  # cantidad de caracteres que coinciden con los slots
            
            if(N > N_coincid){
              return('Unless one name character is not well specified')
            }
            
            # Obtengo los slots que me interesan como lista
            out <- list()
            
            for(i in 1:N){
              out[[i]] <- eval( parse(text = paste0('obj', '@', name[i]) ) )
            }
            
            names(out) <- name
            
            return(out)
          }
)


#' @describeIn get_hydroMet get method for IANIGLA class
# hydroMet_IANIGLA
setMethod(f = 'get_hydroMet',
          signature = 'hydroMet_IANIGLA',
          definition = function(obj, name = NA_character_)
          {
            #obj <- callNextMethod(obj)
            
            # Largo de name
            N <- length(name)
            
            # Hay valor(es)?
            if(N == 1){
              if(is.na(name) == TRUE){'Please supply a valid slot(s) name(s) as character(character vector)'}  
            }
            
            
            # Verifico existencia de los slots
            nom_slots <- names(getSlots('hydroMet_IANIGLA'))
            coincid   <- match(x = nom_slots, table = name) 
            posicion  <- which( is.na(coincid) == FALSE ) # posición dentro de los slots
            N_coincid <- length( posicion == FALSE )  # cantidad de caracteres que coinciden con los slots
            
            if(N > N_coincid){
              return('Unless one name character is not well specified')
            }
            
            # Obtengo los slots que me interesan como lista
            out <- list()
            
            for(i in 1:N){
              out[[i]] <- eval( parse(text = paste0('obj', '@', name[i]) ) )
            }
            
            names(out) <- name
            
            return(out)
          }
)


#' @describeIn get_hydroMet get method for CR2 class
# hydroMet_CR2
setMethod(f = 'get_hydroMet',
          signature = 'hydroMet_CR2',
          definition = function(obj, name = NA_character_)
          {
            #obj <- callNextMethod(obj)
            
            # Largo de name
            N <- length(name)
            
            # Hay valor(es)?
            if(N == 1){
              if(is.na(name) == TRUE){'Please supply a valid slot(s) name(s) as character(character vector)'}  
            }
            
            
            # Verifico existencia de los slots
            nom_slots <- names(getSlots('hydroMet_CR2'))
            coincid   <- match(x = nom_slots, table = name) 
            posicion  <- which( is.na(coincid) == FALSE ) # posición dentro de los slots
            N_coincid <- length( posicion == FALSE )  # cantidad de caracteres que coinciden con los slots
            
            if(N > N_coincid){
              return('Unless one name character is not well specified')
            }
            
            # Obtengo los slots que me interesan como lista
            out <- list()
            
            for(i in 1:N){
              out[[i]] <- eval( parse(text = paste0('obj', '@', name[i]) ) )
            }
            
            names(out) <- name
            
            return(out)
          }
)


#' @describeIn get_hydroMet get method for \code{compact} class
# hydroMet_compact
setMethod(f = 'get_hydroMet',
          signature = 'hydroMet_compact',
          definition = function(obj, name = NA_character_)
          {
            
            # Largo de name
            N <- length(name)
            
            # Hay valor(es)?
            if(N == 1){
              if(is.na(name) == TRUE){'Please supply a valid slot(s) name(s) as character(character vector)'}  
            }
            
            
            # Verifico existencia de los slots
            nom_slots <- names(getSlots('hydroMet_compact'))
            coincid   <- match(x = nom_slots, table = name) 
            posicion  <- which( is.na(coincid) == FALSE ) # posición dentro de los slots
            N_coincid <- length( posicion == FALSE )  # cantidad de caracteres que coinciden con los slots
            
            if(N > N_coincid){
              return('Unless one name character is not well specified')
            }
            
            # Obtengo los slots que me interesan como lista
            out <- list()
            
            for(i in 1:N){
              out[[i]] <- eval( parse(text = paste0('obj', '@', name[i]) ) )
            }
            
            names(out) <- name
            
            return(out)
          }
)