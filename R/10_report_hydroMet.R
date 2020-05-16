# **********************************************************
# Author       : Ezequiel Toum
# Licence      : GPL V3 
# Institution  : IANIGLA-CONICET 
# e-mail       : etoum@mendoza-conicet.gob.ar
# **********************************************************
#
#' Object summaries
#' 
#' @description This method returns a list with two elements: the first one is a \code{data frame} with miss data (see also \link{report_miss_data}) and the second one is also a \code{data frame} with the \code{mean}, \code{sd}, \code{max} and \code{min} values.
#'
#' @param obj an \code{hydroMet_XXX} object. 
#' @param slot_name a single or vector string containing the slot(s) to report. 
#' @param col_name a single or vector string with the name of the column to report in \code{slot_name}.
#' @param start_date optional (default is the first \code{Date}). Single string or \code{POSIXct} with the starting \code{Date} to report. 
#' @param end_date optional (default is the last \code{Date}). Single string or \code{POSIXct} with the last \code{Date} to report. 
#' @param Lang optional (default value is \code{spanish}). Single string with the language to report results: \code{spanish} or \code{english}.
#'
#' @return A list containing two \code{data frames}: the first one with miss data and the second with the \code{mean}, \code{sd}, \code{max} and \code{min} values of the series. 
#' 
#' @importFrom stats sd 
#' 
#' @export
#'
#' @examples
#' # Create IANIGLA class
#' cuevas <- create_hydroMet(class_name = 'IANIGLA')
#'  
#' # List with meteorological variables (slots in BDHI's object)
#' cargar <- list( slotNames(x = 'hydroMet_IANIGLA')[2:11] )
#' 
#' # Assign as names the files
#' hydro_files   <- list.files( system.file('extdata', package = "hydroToolkit"), pattern = 'Cuevas' )
#' names(cargar) <- hydro_files
#' 
#' # Build met-station
#' cuevas <- build_hydroMet(obj = cuevas, slot_list = cargar, 
#'            path = system.file('extdata', package = "hydroToolkit") )
#' 
#' # Get report
#' report_hydroMet(obj = cuevas, slot_name = 'kin', col_name = 'kin_1')
#' 
# Generico
setGeneric(name = 'report_hydroMet',
           def = function(obj, slot_name, col_name, start_date = NULL, end_date = NULL, Lang = 'spanish')
           {
             standardGeneric('report_hydroMet')
           }
)

#' @describeIn report_hydroMet report method for BDHI class
# hydroMet_BDHI
setMethod(f = 'report_hydroMet',
          signature = 'hydroMet_BDHI',
          definition = function(obj, slot_name, col_name, start_date = NULL, end_date = NULL, Lang = 'spanish')
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
            
            
            ## col_name
            # col_name: que sea caracter
            if( is.character(col_name) == FALSE ){
              return('col_name must be of class character')
            }
            
            # col_name: que sea unico
            if( length(col_name) != length(slot_name)){
              return('col_name should have the same length as slot_name argument. You have to express which column you want to aggregate')
            }
            
            
            ## start_date
            if(is.null(start_date) == FALSE){
              # start_date: es caracter o POSIXct
              if( is.character(start_date) == FALSE  & is.POSIXct(start_date) == FALSE){
                return('start_date must be of class character or POSIXct')
              }
              
              # start_date: uno solo
              if( length(start_date) != 1){
                return('start_date must be of length one')
              }
              
            }
            
            
            ## end_date
            if(is.null(end_date) == FALSE){
              # end_date: es caracter o POSIXct
              if( is.character(end_date) == FALSE  & is.POSIXct(end_date) == FALSE){
                return('end_date must be of class character or POSIXct')
              }
              
              # end_date: uno solo
              if( length(end_date) != 1){
                return('end_date must be of length one')
              }
              
            }
            
            
            ## idioma
            # idioma: que sea uno
            if(length(Lang) != 1){
              return('Lang argument must be of length one')
            }
              
            # idioma: que sea el correcto 
            if(Lang != 'spanish' & Lang != 'english') {
              stop('spanish and english are the only valid languages')
            }
            
            #********************************************************************** 
            # Comienzo con el metodo report
            #********************************************************************** 
            
            #***************
            # Binding
            #***************
            Date <- NULL
            #***************
            
            #01# Extraigo los slots de interes
            all_slots <- get_hydroMet(obj = obj, name = slot_name)
            
            # verifico que exista la fecha en cada slot
            if(is.null(start_date) == FALSE){
              for(i in 1:length(all_slots)){
                fecha <- all_slots[[i]]$Date
                flag  <- which(fecha == start_date)
                if(length(flag) == 0){
                  return(paste0('start_date argument is not present in ', slot_name[i] ) )
                }
                
                rm(fecha, i, flag)
                
              } # fin bucle
            } # fin condicional
            
            if(is.null(end_date) == FALSE){
              for(i in 1:length(all_slots)){
                fecha <- all_slots[[i]]$Date
                flag  <- which(fecha == end_date)
                if(length(flag) == 0){
                  return(paste0('end_date argument is not present in ', slot_name[i] ) )
                }
                
                rm(fecha, i, flag)
                
              } # fin bucle
            } # fin condicional
              
            #02# Genero lista de salida
              # lista vacía a completar
              salida <- list()
              
              for(i in 1:length(all_slots)){
                # aplico subset 
                if( is.null(start_date) == FALSE & is.null(end_date) == TRUE){
                  df <- subset(x = all_slots[[i]], subset = Date >= start_date, select = c('Date', col_name[i]) )
                  
                } else if( is.null(start_date) == TRUE &  is.null(end_date) == FALSE ){
                  df <- subset(x = all_slots[[i]], subset = Date <= end_date, select = c('Date', col_name[i]) )
                  
                } else if( is.null(start_date) == FALSE &  is.null(end_date) == FALSE){
                  df <- subset(x = all_slots[[i]], subset = Date >= start_date & Date <= end_date, select = c('Date', col_name[i]) )
                  
                } else {
                  df <- subset(all_slots[[i]], select = c('Date', col_name[i]) )
                }
                
                # report_miss_data
                faltantes <- report_miss_data(df = df, Lang = Lang)
                
                # mean | sd | max | min
                if(Lang == 'spanish'){
                  estadist <- data.frame(Medidas = c('media', 's', 'max', 'min'), 
                                         Valor = c( mean(x = df[ , 2], na.rm = TRUE),
                                                    sd(x = df[ , 2], na.rm = TRUE), 
                                                    max(x = df[ , 2], na.rm = TRUE),
                                                    min(x = df[ , 2], na.rm = TRUE) ) 
                                         )
                } else {
                  estadist <- data.frame(Measure = c('mean', 'sd', 'max', 'min'), 
                                         Value = c( mean(x = df[ , 2], na.rm = TRUE),
                                                    sd(x = df[ , 2], na.rm = TRUE), 
                                                    max(x = df[ , 2], na.rm = TRUE),
                                                    min(x = df[ , 2], na.rm = TRUE) ) 
                  )
                  
                }
                
                # salida
                salida[[i]] <- list(miss_data = faltantes, Stats = estadist)
                
                
              }
              
            #03# Salida del metodo 
            return(salida)  
            
            
          } # fin funcion
)


#' @describeIn report_hydroMet report method for CR2 class
# hydroMet_CR2
setMethod(f = 'report_hydroMet',
          signature = 'hydroMet_CR2',
          definition = function(obj, slot_name, col_name, start_date = NULL, end_date = NULL, Lang = 'spanish')
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
            
            
            ## col_name
            # col_name: que sea caracter
            if( is.character(col_name) == FALSE ){
              return('col_name must be of class character')
            }
            
            # col_name: que sea unico
            if( length(col_name) != length(slot_name)){
              return('col_name should have the same length as slot_name argument. You have to express which column you want to aggregate')
            }
            
            
            ## start_date
            if(is.null(start_date) == FALSE){
              # start_date: es caracter o POSIXct
              if( is.character(start_date) == FALSE  & is.POSIXct(start_date) == FALSE){
                return('start_date must be of class character or POSIXct')
              }
              
              # start_date: uno solo
              if( length(start_date) != 1){
                return('start_date must be of length one')
              }
              
            }
            
            
            ## end_date
            if(is.null(end_date) == FALSE){
              # end_date: es caracter o POSIXct
              if( is.character(end_date) == FALSE  & is.POSIXct(end_date) == FALSE){
                return('end_date must be of class character or POSIXct')
              }
              
              # end_date: uno solo
              if( length(end_date) != 1){
                return('end_date must be of length one')
              }
              
            }
            
            
            ## idioma
            # idioma: que sea uno
            if(length(Lang) != 1){
              return('Lang argument must be of length one')
            }
            
            # idioma: que sea el correcto 
            if(Lang != 'spanish' & Lang != 'english') {
              stop('spanish and english are the only valid languages')
            }
            
            #********************************************************************** 
            # Comienzo con el metodo report
            #********************************************************************** 
            
            #***************
            # Binding
            #***************
            Date <- NULL
            #***************
            
            #01# Extraigo los slots de interes
            all_slots <- get_hydroMet(obj = obj, name = slot_name)
            
            # verifico que exista la fecha en cada slot
            if(is.null(start_date) == FALSE){
              for(i in 1:length(all_slots)){
                fecha <- all_slots[[i]]$Date
                flag  <- which(fecha == start_date)
                if(length(flag) == 0){
                  return(paste0('start_date argument is not present in ', slot_name[i] ) )
                }
                
                rm(fecha, i, flag)
                
              } # fin bucle
            } # fin condicional
            
            if(is.null(end_date) == FALSE){
              for(i in 1:length(all_slots)){
                fecha <- all_slots[[i]]$Date
                flag  <- which(fecha == end_date)
                if(length(flag) == 0){
                  return(paste0('end_date argument is not present in ', slot_name[i] ) )
                }
                
                rm(fecha, i, flag)
                
              } # fin bucle
            } # fin condicional
            
            #02# Genero lista de salida
            # lista vacía a completar
            salida <- list()
            
            for(i in 1:length(all_slots)){
              # aplico subset 
              if( is.null(start_date) == FALSE & is.null(end_date) == TRUE){
                df <- subset(x = all_slots[[i]], subset = Date >= start_date, select = c('Date', col_name[i]) )
                
              } else if( is.null(start_date) == TRUE &  is.null(end_date) == FALSE ){
                df <- subset(x = all_slots[[i]], subset = Date <= end_date, select = c('Date', col_name[i]) )
                
              } else if( is.null(start_date) == FALSE &  is.null(end_date) == FALSE){
                df <- subset(x = all_slots[[i]], subset = Date >= start_date & Date <= end_date, select = c('Date', col_name[i]) )
                
              } else {
                df <- subset(all_slots[[i]], select = c('Date', col_name[i]) )
              }
              
              # report_miss_data
              faltantes <- report_miss_data(df = df, Lang = Lang)
              
              # mean | sd | max | min
              if(Lang == 'spanish'){
                estadist <- data.frame(Medidas = c('media', 's', 'max', 'min'), 
                                       Valor = c( mean(x = df[ , 2], na.rm = TRUE),
                                                  sd(x = df[ , 2], na.rm = TRUE), 
                                                  max(x = df[ , 2], na.rm = TRUE),
                                                  min(x = df[ , 2], na.rm = TRUE) ) 
                )
              } else {
                estadist <- data.frame(Measure = c('mean', 'sd', 'max', 'min'), 
                                       Value = c( mean(x = df[ , 2], na.rm = TRUE),
                                                  sd(x = df[ , 2], na.rm = TRUE), 
                                                  max(x = df[ , 2], na.rm = TRUE),
                                                  min(x = df[ , 2], na.rm = TRUE) ) 
                )
                
              }
              
              # salida
              salida[[i]] <- list(miss_data = faltantes, Stats = estadist)
              
              
            }
            
            #03# Salida del metodo 
            return(salida)  
            
            
          } # fin funcion
)


#' @describeIn report_hydroMet report method for DGI class
# hydroMet_DGI
setMethod(f = 'report_hydroMet',
          signature = 'hydroMet_DGI',
          definition = function(obj, slot_name, col_name, start_date = NULL, end_date = NULL, Lang = 'spanish')
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
            
            
            ## col_name
            # col_name: que sea caracter
            if( is.character(col_name) == FALSE ){
              return('col_name must be of class character')
            }
            
            # col_name: que sea unico
            if( length(col_name) != length(slot_name)){
              return('col_name should have the same length as slot_name argument. You have to express which column you want to aggregate')
            }
            
            
            ## start_date
            if(is.null(start_date) == FALSE){
              # start_date: es caracter o POSIXct
              if( is.character(start_date) == FALSE  & is.POSIXct(start_date) == FALSE){
                return('start_date must be of class character or POSIXct')
              }
              
              # start_date: uno solo
              if( length(start_date) != 1){
                return('start_date must be of length one')
              }
              
            }
            
            
            ## end_date
            if(is.null(end_date) == FALSE){
              # end_date: es caracter o POSIXct
              if( is.character(end_date) == FALSE  & is.POSIXct(end_date) == FALSE){
                return('end_date must be of class character or POSIXct')
              }
              
              # end_date: uno solo
              if( length(end_date) != 1){
                return('end_date must be of length one')
              }
              
            }
            
            
            ## idioma
            # idioma: que sea uno
            if(length(Lang) != 1){
              return('Lang argument must be of length one')
            }
            
            # idioma: que sea el correcto 
            if(Lang != 'spanish' & Lang != 'english') {
              stop('spanish and english are the only valid languages')
            }
            
            #********************************************************************** 
            # Comienzo con el metodo report
            #********************************************************************** 
            
            #***************
            # Binding
            #***************
            Date <- NULL
            #***************
            
            #01# Extraigo los slots de interes
            all_slots <- get_hydroMet(obj = obj, name = slot_name)
            
            # verifico que exista la fecha en cada slot
            if(is.null(start_date) == FALSE){
              for(i in 1:length(all_slots)){
                fecha <- all_slots[[i]]$Date
                flag  <- which(fecha == start_date)
                if(length(flag) == 0){
                  return(paste0('start_date argument is not present in ', slot_name[i] ) )
                }
                
                rm(fecha, i, flag)
                
              } # fin bucle
            } # fin condicional
            
            if(is.null(end_date) == FALSE){
              for(i in 1:length(all_slots)){
                fecha <- all_slots[[i]]$Date
                flag  <- which(fecha == end_date)
                if(length(flag) == 0){
                  return(paste0('end_date argument is not present in ', slot_name[i] ) )
                }
                
                rm(fecha, i, flag)
                
              } # fin bucle
            } # fin condicional
            
            #02# Genero lista de salida
            # lista vacía a completar
            salida <- list()
            
            for(i in 1:length(all_slots)){
              # aplico subset 
              if( is.null(start_date) == FALSE & is.null(end_date) == TRUE){
                df <- subset(x = all_slots[[i]], subset = Date >= start_date, select = c('Date', col_name[i]) )
                
              } else if( is.null(start_date) == TRUE &  is.null(end_date) == FALSE ){
                df <- subset(x = all_slots[[i]], subset = Date <= end_date, select = c('Date', col_name[i]) )
                
              } else if( is.null(start_date) == FALSE &  is.null(end_date) == FALSE){
                df <- subset(x = all_slots[[i]], subset = Date >= start_date & Date <= end_date, select = c('Date', col_name[i]) )
                
              } else {
                df <- subset(all_slots[[i]], select = c('Date', col_name[i]) )
              }
              
              # report_miss_data
              faltantes <- report_miss_data(df = df, Lang = Lang)
              
              # mean | sd | max | min
              if(Lang == 'spanish'){
                estadist <- data.frame(Medidas = c('media', 's', 'max', 'min'), 
                                       Valor = c( mean(x = df[ , 2], na.rm = TRUE),
                                                  sd(x = df[ , 2], na.rm = TRUE), 
                                                  max(x = df[ , 2], na.rm = TRUE),
                                                  min(x = df[ , 2], na.rm = TRUE) ) 
                )
              } else {
                estadist <- data.frame(Measure = c('mean', 'sd', 'max', 'min'), 
                                       Value = c( mean(x = df[ , 2], na.rm = TRUE),
                                                  sd(x = df[ , 2], na.rm = TRUE), 
                                                  max(x = df[ , 2], na.rm = TRUE),
                                                  min(x = df[ , 2], na.rm = TRUE) ) 
                )
                
              }
              
              # salida
              salida[[i]] <- list(miss_data = faltantes, Stats = estadist)
              
              
            }
            
            #03# Salida del metodo 
            return(salida)  
            
            
          } # fin funcion
)


#' @describeIn report_hydroMet report method for IANIGLA class
# hydroMet_IANIGLA
setMethod(f = 'report_hydroMet',
          signature = 'hydroMet_IANIGLA',
          definition = function(obj, slot_name, col_name, start_date = NULL, end_date = NULL, Lang = 'spanish')
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
            
            
            ## col_name
            # col_name: que sea caracter
            if( is.character(col_name) == FALSE ){
              return('col_name must be of class character')
            }
            
            # col_name: que sea unico
            if( length(col_name) != length(slot_name)){
              return('col_name should have the same length as slot_name argument. You have to express which column you want to aggregate')
            }
            
            
            ## start_date
            if(is.null(start_date) == FALSE){
              # start_date: es caracter o POSIXct
              if( is.character(start_date) == FALSE  & is.POSIXct(start_date) == FALSE){
                return('start_date must be of class character or POSIXct')
              }
              
              # start_date: uno solo
              if( length(start_date) != 1){
                return('start_date must be of length one')
              }
              
            }
            
            
            ## end_date
            if(is.null(end_date) == FALSE){
              # end_date: es caracter o POSIXct
              if( is.character(end_date) == FALSE  & is.POSIXct(end_date) == FALSE){
                return('end_date must be of class character or POSIXct')
              }
              
              # end_date: uno solo
              if( length(end_date) != 1){
                return('end_date must be of length one')
              }
              
            }
            
            
            ## idioma
            # idioma: que sea uno
            if(length(Lang) != 1){
              return('Lang argument must be of length one')
            }
            
            # idioma: que sea el correcto 
            if(Lang != 'spanish' & Lang != 'english') {
              stop('spanish and english are the only valid languages')
            }
            
            #********************************************************************** 
            # Comienzo con el metodo report
            #********************************************************************** 
            
            #***************
            # Binding
            #***************
            Date <- NULL
            #***************
            
            #01# Extraigo los slots de interes
            var_slots  <- get_hydroMet(obj = obj, name = slot_name)
            date_serie <- get_hydroMet(obj = obj, name = 'date')[[1]]
            
            # armo data frame con fecha en primer columna para que pueda aplicar el resto
            # del algoritmo
            all_slots <- lapply(X = var_slots, FUN = function(x){
              # verifico que el la fecha tenga el mismo paso y duracion temporal
              # que las series
              if(nrow(x) != length(date_serie)){stop('Variables should have the same length as Date')}
              
              # extraigo nombre de variables para usarlos más adelante
              var_name <- colnames(x)
              
              # armo data frame y asigno nombre a las columnas
              df <- data.frame(Date = date_serie, x)
              colnames(df) <- c('Date', var_name)
              
              return(df)
            })
            
            # verifico que exista la fecha en cada slot
            if(is.null(start_date) == FALSE){
              for(i in 1:length(all_slots)){
                fecha <- all_slots[[i]]$Date
                flag  <- which(fecha == start_date)
                if(length(flag) == 0){
                  return(paste0('start_date argument is not present in ', slot_name[i] ) )
                }
                
                rm(fecha, i, flag)
                
              } # fin bucle
            } # fin condicional
            
            if(is.null(end_date) == FALSE){
              for(i in 1:length(all_slots)){
                fecha <- all_slots[[i]]$Date
                flag  <- which(fecha == end_date)
                if(length(flag) == 0){
                  return(paste0('end_date argument is not present in ', slot_name[i] ) )
                }
                
                rm(fecha, i, flag)
                
              } # fin bucle
            } # fin condicional
            
            #02# Genero lista de salida
            # lista vacía a completar
            salida <- list()
            
            for(i in 1:length(all_slots)){
              # aplico subset 
              if( is.null(start_date) == FALSE & is.null(end_date) == TRUE){
                df <- subset(x = all_slots[[i]], subset = Date >= start_date, select = c('Date', col_name[i]) )
                
              } else if( is.null(start_date) == TRUE &  is.null(end_date) == FALSE ){
                df <- subset(x = all_slots[[i]], subset = Date <= end_date, select = c('Date', col_name[i]) )
                
              } else if( is.null(start_date) == FALSE &  is.null(end_date) == FALSE){
                df <- subset(x = all_slots[[i]], subset = Date >= start_date & Date <= end_date, select = c('Date', col_name[i]) )
                
              } else {
                df <- subset(all_slots[[i]], select = c('Date', col_name[i]) )
              }
              
              # report_miss_data
              faltantes <- report_miss_data(df = df, Lang = Lang)
              
              # mean | sd | max | min
              if(Lang == 'spanish'){
                estadist <- data.frame(Medidas = c('media', 's', 'max', 'min'), 
                                       Valor = c( mean(x = df[ , 2], na.rm = TRUE),
                                                  sd(x = df[ , 2], na.rm = TRUE), 
                                                  max(x = df[ , 2], na.rm = TRUE),
                                                  min(x = df[ , 2], na.rm = TRUE) ) 
                )
              } else {
                estadist <- data.frame(Measure = c('mean', 'sd', 'max', 'min'), 
                                       Value = c( mean(x = df[ , 2], na.rm = TRUE),
                                                  sd(x = df[ , 2], na.rm = TRUE), 
                                                  max(x = df[ , 2], na.rm = TRUE),
                                                  min(x = df[ , 2], na.rm = TRUE) ) 
                )
                
              }
              
              # salida
              salida[[i]] <- list(miss_data = faltantes, Stats = estadist)
              
              
            }
            
            #03# Salida del metodo 
            return(salida)  
            
            
          } # fin funcion
)
