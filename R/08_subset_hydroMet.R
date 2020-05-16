# **********************************************************
# Author       : Ezequiel Toum
# Licence      : GPL V3 
# Institution  : IANIGLA-CONICET 
# e-mail       : etoum@mendoza-conicet.gob.ar
# **********************************************************
#
#' Subset your data
#' 
#' @description This method allows you to easily cut the data stored in an \code{hydroMet_XXX} class object by dates.
#'
#' @param obj an \code{hydroMet_XXX} class object.
#' @param slot_name string vector with the slot(s) name(s) to subset. \strong{NOTE}: in case you want to subset a \code{hydroMet_IANIGLA} object is recommended to consider all the slots with data.
#' @param from string (or \code{POSIXct} - valid only in 'BDHI' and  'IANIGLA') with the starting \code{Date}. You can use \code{from} without \code{to}. In this case you will subset your data 'from' till the end.
#' @param to string (or \code{POSIXct} - valid only in 'BDHI' and  'IANIGLA') with the ending \code{Date}. You can use \code{to} without \code{from}. In this case you will subset your data from the beginning till 'to'. 
#'
#' @return The same \code{hydroMet_XXX} class provided in \code{obj} but subsetted. 
#' 
#' @importFrom lubridate is.POSIXct
#' 
#' @export
#'
#' @examples
#' # Create BDHI hydro-met station
#' guido <- create_hydroMet(class_name = 'BDHI')
#' 
#' # List with meteorological variables (slots in BDHI's object)
#' cargar <- list('precip', 'Qmd', 'Qmm')
#' 
#' # Assign as names the files
#' hydro_files   <- list.files( system.file('extdata', package = "hydroToolkit"), pattern = 'Guido' )
#' names(cargar) <- hydro_files
#' 
#' # Build the object with the met records
#' guido <- build_hydroMet(obj = guido, slot_list = cargar, 
#'                path = system.file('extdata', package = "hydroToolkit") )
#'                
#' # Subset daily mean discharge
#' guido <- subset_hydroMet(obj = guido, slot_name = 'Qmd', from = '2005-01-01',
#'             to = '2010-12-31')
#' 
## Generico
setGeneric(name = 'subset_hydroMet', 
           def = function(obj, slot_name, from = NULL, to = NULL) 
           {
             standardGeneric('subset_hydroMet')
           })

#' @describeIn subset_hydroMet subset method for BDHI data
## BDHI
setMethod(f = 'subset_hydroMet', 
          signature = 'hydroMet_BDHI', 
          definition = function(obj, slot_name, from = NULL, to = NULL) 
          {
            #*********************************************************************
            # Condicionales
            #*********************************************************************
            # slot_name
            # slot_name: nombre como caracter
            if(is.character(slot_name) == FALSE){
              return('slot_name argument must be of class character')
            }
            
            # slot_name: que sea un slot valido
            aux <- match(x = slot_name, table = slotNames('hydroMet_BDHI')[1:13])
            if( is.na( sum(aux) ) == TRUE  ){
              return('Unless one of the slot_name arguments is incorrect')
            }
            rm(aux)
            
            
            # from y to
            if( is.null(from) & is.null(to) ){
              return('Unless one of the following argument must exist: from and to')
            }
            
            
            # from
            if( is.null(from) == FALSE ){
              # from: caracter
              if( is.character(from) == FALSE  & is.POSIXct(from) == FALSE){
                return('from must be of class character or POSIXct')
              }
              
              # from: uno solo
              if( length(from) != 1){
                return('from must be of length one')
              }
              
            }
            
            
            
            # to
            if( is.null(to) == FALSE){
              # to: caracter
              if( is.character(to) == FALSE  & is.POSIXct(to) == FALSE){
                return('to must be of class character or POSIXct')
              }
              
              # to: uno solo
              if( length(to) != 1){
                return('to must be of length one')
              }
              
            }
            
            
            #********************************************************************** 
            
            #***************
            # Binding
            #***************
            Date <- NULL
            #***************
            
            # Comienzo con la funcion subset
            
            #01# Obtengo los slots de interes
            all_slots <- get_hydroMet(obj = obj, name = slot_name)
            
            # Verificar que existan las fechas
            # from
            if( is.null(from) == FALSE ){
            check_from <- sum(sapply(X = all_slots, FUN = function(x){
                flag_from <- which(x[ , 1] == from)
                
                out <- ifelse( length(flag_from) > 0, 1, NA_real_) 
                return(out)
                })
                )
            
            if(is.na(check_from) == TRUE){ return('from argument does not exist unless in one of the slots') }
            }
            
            # to
            if( is.null(to) == FALSE ){
              check_to <- sum(sapply(X = all_slots, FUN = function(x){
                flag_to <- which(x[ , 1] == to)
                
                out <- ifelse( length(flag_to) > 0, 1, NA_real_) 
                return(out)
              })
              )
              
              if(is.na(check_to) == TRUE){ return('to argument does not exist unless in one of the slots') }
            }
            
            #02# Bucle para recortar los data frame
            
            # Recortar
            if( is.null(from) == FALSE & is.null(to) == FALSE){
              out_list <- lapply(X = all_slots, FUN = function(x){
                out <- subset(x = x, subset = Date >= from & Date <= to)
              })
              
            } else if( is.null(from) == FALSE){
              out_list <- lapply(X = all_slots, FUN = function(x){
                out <- subset(x = x, subset = Date >= from )
              })
              
            } else {
              out_list <- lapply(X = all_slots, FUN = function(x){
                out <- subset(x = x, subset = Date <= to )
              })
              
            }
            
            # Asignar los elementos de la lista al objeto de salida
            N_slot <- length(slot_name)
            
            for(j in 1:N_slot){
              texto <- paste0('set_hydroMet(obj = obj,', slot_name[j], '=', 'out_list[[', j, ']]', ')')
              obj   <- eval( parse(text = texto) )
              
            }
            
        
            #********************************************************************** 
            
            # Valido objeto
            validObject(obj)
            
            # Lo que debe regresar el método
            return(obj)
            
            
          } # fin funcion
)


#' @describeIn subset_hydroMet subset method for DGI data
# DGI
setMethod(f = 'subset_hydroMet', 
          signature = 'hydroMet_DGI', 
          definition = function(obj, slot_name, from = NULL, to = NULL) 
          {
            #*********************************************************************
            # Condicionales
            #*********************************************************************
            # slot_name
            # slot_name: nombre como caracter
            if(is.character(slot_name) == FALSE){
              return('slot_name argument must be of class character')
            }
            
            # slot_name: que sea un slot valido
            aux <- match(x = slot_name, table = slotNames('hydroMet_DGI')[1:7])
            if( is.na( sum(aux) ) == TRUE  ){
              return('Unless one of the slot_name arguments is incorrect')
            }
            rm(aux)
            
            
            # from y to
            if( is.null(from) & is.null(to) ){
              return('Unless one of the following argument must exist: from and to')
            }
            
            
            # from
            if( is.null(from) == FALSE ){
              # from: caracter
              if( is.character(from) == FALSE  & is.POSIXct(from) == FALSE){
                return('from must be of class character or POSIXct')
              }
              
              # from: uno solo
              if( length(from) != 1){
                return('from must be of length one')
              }
              
            }
            
            
            
            # to
            if( is.null(to) == FALSE){
              # to: caracter
              if( is.character(to) == FALSE  & is.POSIXct(to) == FALSE){
                return('to must be of class character or POSIXct')
              }
              
              # to: uno solo
              if( length(to) != 1){
                return('to must be of length one')
              }
              
            }
            
            
            #********************************************************************** 
            
            #***************
            # Binding
            #***************
            Date <- NULL
            #***************
            
            # Comienzo con la funcion subset
            
            #01# Obtengo los slots de interes
            all_slots <- get_hydroMet(obj = obj, name = slot_name)
            
            # Verificar que existan las fechas
            # from
            if( is.null(from) == FALSE ){
              check_from <- sum(sapply(X = all_slots, FUN = function(x){
                flag_from <- which(x[ , 1] == from)
                
                out <- ifelse( length(flag_from) > 0, 1, NA_real_) 
                return(out)
              })
              )
              
              if(is.na(check_from) == TRUE){ return('from argument does not exist unless in one of the slots') }
            }
            
            # to
            if( is.null(to) == FALSE ){
              check_to <- sum(sapply(X = all_slots, FUN = function(x){
                flag_to <- which(x[ , 1] == to)
                
                out <- ifelse( length(flag_to) > 0, 1, NA_real_) 
                return(out)
              })
              )
              
              if(is.na(check_to) == TRUE){ return('to argument does not exist unless in one of the slots') }
            }
            
            #02# Bucle para recortar los data frame
            
            # Recortar
            if( is.null(from) == FALSE & is.null(to) == FALSE){
              out_list <- lapply(X = all_slots, FUN = function(x){
                out <- subset(x = x, subset = Date >= from & Date <= to)
              })
              
            } else if( is.null(from) == FALSE){
              out_list <- lapply(X = all_slots, FUN = function(x){
                out <- subset(x = x, subset = Date >= from )
              })
              
            } else {
              out_list <- lapply(X = all_slots, FUN = function(x){
                out <- subset(x = x, subset = Date <= to )
              })
              
            }
            
            # Asignar los elementos de la lista al objeto de salida
            N_slot <- length(slot_name)
            
            for(j in 1:N_slot){
              texto <- paste0('set_hydroMet(obj = obj,', slot_name[j], '=', 'out_list[[', j, ']]', ')')
              obj   <- eval( parse(text = texto) )
              
            }
            
            
            #********************************************************************** 
            
            # Valido objeto
            validObject(obj)
            
            # Lo que debe regresar el método
            return(obj)
            
            
          } # fin funcion
)


#' @describeIn subset_hydroMet subset method for CR2 data
# CR2
setMethod(f = 'subset_hydroMet', 
          signature = 'hydroMet_CR2', 
          definition = function(obj, slot_name, from = NULL, to = NULL) 
          {
            #*********************************************************************
            # Condicionales
            #*********************************************************************
            # slot_name
            # slot_name: nombre como caracter
            if(is.character(slot_name) == FALSE){
              return('slot_name argument must be of class character')
            }
            
            # slot_name: que sea un slot valido
            aux <- match(x = slot_name, table = slotNames('hydroMet_CR2')[1:4])
            if( is.na( sum(aux) ) == TRUE  ){
              return('Unless one of the slot_name arguments is incorrect')
            }
            rm(aux)
            
            
            # from y to
            if( is.null(from) & is.null(to) ){
              return('Unless one of the following argument must exist: from and to')
            }
            
            
            # from
            if( is.null(from) == FALSE ){
              # from: caracter
              if( is.character(from) == FALSE  & is.POSIXct(from) == FALSE){
                return('from must be of class character or POSIXct')
              }
              
              # from: uno solo
              if( length(from) != 1){
                return('from must be of length one')
              }
              
            }
            
            
            
            # to
            if( is.null(to) == FALSE){
              # to: caracter
              if( is.character(to) == FALSE  & is.POSIXct(to) == FALSE){
                return('to must be of class character or POSIXct')
              }
              
              # to: uno solo
              if( length(to) != 1){
                return('to must be of length one')
              }
              
            }
            
            
            #********************************************************************** 
            
            #***************
            # Binding
            #***************
            Date <- NULL
            #***************
            
            # Comienzo con la funcion subset
            
            #01# Obtengo los slots de interes
            all_slots <- get_hydroMet(obj = obj, name = slot_name)
            
            # Verificar que existan las fechas
            # from
            if( is.null(from) == FALSE ){
              check_from <- sum(sapply(X = all_slots, FUN = function(x){
                flag_from <- which(x[ , 1] == from)
                
                out <- ifelse( length(flag_from) > 0, 1, NA_real_) 
                return(out)
              })
              )
              
              if(is.na(check_from) == TRUE){ return('from argument does not exist unless in one of the slots') }
            }
            
            # to
            if( is.null(to) == FALSE ){
              check_to <- sum(sapply(X = all_slots, FUN = function(x){
                flag_to <- which(x[ , 1] == to)
                
                out <- ifelse( length(flag_to) > 0, 1, NA_real_) 
                return(out)
              })
              )
              
              if(is.na(check_to) == TRUE){ return('to argument does not exist unless in one of the slots') }
            }
            
            #02# Bucle para recortar los data frame
            
            # Recortar
            if( is.null(from) == FALSE & is.null(to) == FALSE){
              out_list <- lapply(X = all_slots, FUN = function(x){
                out <- subset(x = x, subset = Date >= from & Date <= to)
              })
              
            } else if( is.null(from) == FALSE){
              out_list <- lapply(X = all_slots, FUN = function(x){
                out <- subset(x = x, subset = Date >= from )
              })
              
            } else {
              out_list <- lapply(X = all_slots, FUN = function(x){
                out <- subset(x = x, subset = Date <= to )
              })
              
            }
            
            # Asignar los elementos de la lista al objeto de salida
            N_slot <- length(slot_name)
            
            for(j in 1:N_slot){
              texto <- paste0('set_hydroMet(obj = obj,', slot_name[j], '=', 'out_list[[', j, ']]', ')')
              obj   <- eval( parse(text = texto) )
              
            }
            
            
            #********************************************************************** 
            
            # Valido objeto
            validObject(obj)
            
            # Lo que debe regresar el método
            return(obj)
            
            
          } # fin funcion
)


#' @describeIn subset_hydroMet subset method for IANIGLA data
# IANIGLA
setMethod(f = 'subset_hydroMet', 
          signature = 'hydroMet_IANIGLA', 
          definition = function(obj, slot_name, from = NULL, to = NULL) 
          {
            #*********************************************************************
            # Condicionales
            #*********************************************************************
            # slot_name
            # slot_name: nombre como caracter
            if(is.character(slot_name) == FALSE){
              return('slot_name argument must be of class character')
            }
            
            # slot_name: que sea un slot valido
            aux <- match(x = slot_name, table = slotNames('hydroMet_IANIGLA')[2:11])
            if( is.na( sum(aux) ) == TRUE  ){
              return('Unless one of the slot_name arguments is incorrect')
            }
            rm(aux)
            
            
            # from y to
            if( is.null(from) & is.null(to) ){
              return('Unless one of the following argument must exist: from and to')
            }
            
            
            # from
            if( is.null(from) == FALSE ){
              # from: caracter
              if( is.character(from) == FALSE  & is.POSIXct(from) == FALSE){
                return('from must be of class character or POSIXct')
              }
              
              # from: uno solo
              if( length(from) != 1){
                return('from must be of length one')
              }
              
            }
            
            
            
            # to
            if( is.null(to) == FALSE){
              # to: caracter
              if( is.character(to) == FALSE  & is.POSIXct(to) == FALSE){
                return('to must be of class character or POSIXct')
              }
              
              # to: uno solo
              if( length(to) != 1){
                return('to must be of length one')
              }
              
            }
            
            
            #********************************************************************** 
            
            #***************
            # Binding
            #***************
            Date <- NULL
            #***************
            
            # Comienzo con la funcion subset
            
            #01# Obtengo los slots de interes
            all_slots <- get_hydroMet(obj = obj, name = slot_name)
            
            # Verificar que existan las fechas
            # from
            if( is.null(from) == FALSE ){
              check_from <- which(obj@date == from)
              
              if(length(check_from) == 0){ return('Date in from argument does not exist') }
            }
            
            # to
            if( is.null(to) == FALSE ){
              check_to <-  which(obj@date == to)
              
              if(length(check_to) == 0){ return('Date in to argument does not exist') }
            }
            
            #02# Bucle para recortar los data frame
            
            # Recortar datos
            if( is.null(from) == FALSE & is.null(to) == FALSE){
              out_list <- lapply(X = all_slots, FUN = function(x){
                inicio <- which(obj@date == from)
                final  <- which(obj@date == to)
                
                out <- x[inicio:final, , drop = FALSE]
              })
              
            } else if( is.null(from) == FALSE){
              out_list <- lapply(X = all_slots, FUN = function(x){
                inicio <- which(obj@date == from)
                
                out <- x[-c( 1:(inicio-1) ), , drop = FALSE]
              })
              
            } else {
              out_list <- lapply(X = all_slots, FUN = function(x){
                final <- which(obj@date == to)
                
                out <- x[1:final, , drop = FALSE]
              })
              
            }
            
            # Asignar los elementos de la lista al objeto de salida
            N_slot <- length(slot_name)
            
            for(j in 1:N_slot){
              texto <- paste0('set_hydroMet(obj = obj,', slot_name[j], '=', 'out_list[[', j, ']]', ')')
              obj   <- eval( parse(text = texto) )
              
            }
            
            # Recortar fechas
            if( is.null(from) == FALSE & is.null(to) == FALSE){
                inicio <- which(obj@date == from)
                final  <- which(obj@date == to)
                
                out <- obj@date[inicio:final]
             
            } else if( is.null(from) == FALSE){
                inicio <- which(obj@date == from)
                
                out <- obj@date[ -c( 1:(inicio-1) ) ]
             
            } else {
                final <- which(obj@date == to)
                
                out <- obj@date[1:final]
              
            }
            
            obj <- set_hydroMet(obj = obj, date = out)
            
            #********************************************************************** 
            
            # Valido objeto
            validObject(obj)
            
            # Lo que debe regresar el método
            return(obj)
            
            
          } # fin funcion
)


#' @describeIn subset_hydroMet subset method for \code{compact} data
# compact
setMethod(f = 'subset_hydroMet', 
          signature = 'hydroMet_compact', 
          definition = function(obj, slot_name, from = NULL, to = NULL) 
          {
            #*********************************************************************
            # Condicionales
            #*********************************************************************
            # slot_name
            # slot_name: nombre como caracter
            if(is.character(slot_name) == FALSE){
              return('slot_name argument must be of class character')
            }
            
            # slot_name: que sea un slot valido
            aux <- match(x = slot_name, table = slotNames('hydroMet_compact')[1])
            if( is.na( sum(aux) ) == TRUE  ){
              return('Unless one of the slot_name arguments is incorrect')
            }
            rm(aux)
            
            
            # from y to
            if( is.null(from) & is.null(to) ){
              return('Unless one of the following argument must exist: from and to')
            }
            
            
            # from
            if( is.null(from) == FALSE ){
              # from: caracter
              if( is.character(from) == FALSE  & is.POSIXct(from) == FALSE){
                return('from must be of class character or POSIXct')
              }
              
              # from: uno solo
              if( length(from) != 1){
                return('from must be of length one')
              }
              
            }
            
            
            
            # to
            if( is.null(to) == FALSE){
              # to: caracter
              if( is.character(to) == FALSE  & is.POSIXct(to) == FALSE){
                return('to must be of class character or POSIXct')
              }
              
              # to: uno solo
              if( length(to) != 1){
                return('to must be of length one')
              }
              
            }
            
            
            #********************************************************************** 
            
            #***************
            # Binding
            #***************
            Date <- NULL
            #***************
            
            # Comienzo con la funcion subset
            
            #01# Obtengo los slots de interes
            all_slots <- get_hydroMet(obj = obj, name = slot_name)
            
            # Verificar que existan las fechas
            # from
            if( is.null(from) == FALSE ){
              check_from <- sum(sapply(X = all_slots, FUN = function(x){
                flag_from <- which(x[ , 1] == from)
                
                out <- ifelse( length(flag_from) > 0, 1, NA_real_) 
                return(out)
              })
              )
              
              if(is.na(check_from) == TRUE){ return('from argument does not exist unless in one of the slots') }
            }
            
            # to
            if( is.null(to) == FALSE ){
              check_to <- sum(sapply(X = all_slots, FUN = function(x){
                flag_to <- which(x[ , 1] == to)
                
                out <- ifelse( length(flag_to) > 0, 1, NA_real_) 
                return(out)
              })
              )
              
              if(is.na(check_to) == TRUE){ return('to argument does not exist unless in one of the slots') }
            }
            
            #02# Bucle para recortar los data frame
            
            # Recortar
            if( is.null(from) == FALSE & is.null(to) == FALSE){
              out_list <- lapply(X = all_slots, FUN = function(x){
                out <- subset(x = x, subset = Date >= from & Date <= to)
              })
              
            } else if( is.null(from) == FALSE){
              out_list <- lapply(X = all_slots, FUN = function(x){
                out <- subset(x = x, subset = Date >= from )
              })
              
            } else {
              out_list <- lapply(X = all_slots, FUN = function(x){
                out <- subset(x = x, subset = Date <= to )
              })
              
            }
            
            # Asignar los elementos de la lista al objeto de salida
            N_slot <- length(slot_name)
            
            for(j in 1:N_slot){
              texto <- paste0('set_hydroMet(obj = obj,', slot_name[j], '=', 'out_list[[', j, ']]', ')')
              obj   <- eval( parse(text = texto) )
              
            }
            
            
            #********************************************************************** 
            
            # Valido objeto
            validObject(obj)
            
            # Lo que debe regresar el método
            return(obj)
            
            
          } # fin funcion
)
