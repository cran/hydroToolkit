# **********************************************************
# Author       : Ezequiel Toum
# Licence      : GPL V3 
# Institution  : IANIGLA-CONICET 
# e-mail       : etoum@mendoza-conicet.gob.ar
# **********************************************************
#
#' Aggregate slot data
#' 
#' @description This method provides common functions to aggregate the data inside a slot.
#'
#' @param obj an \code{hydroMet_XXX} class object. This method is not allowed for \code{hydroMet_compact} class. This is because this class was thought as \emph{ready to use}, so when building this class you should have already aggregated your data.
#' @param slot_name a single or vector string containing the slot(s) to aggregate. 
#' @param col_name a single or vector string with the name of the column to aggregate in \code{slot_name}.
#' @param fun a single or vector string containing one of the following functions: \option{mean}, \option{min}, \option{max} or \option{sum}.
#' @param period a single or vector string with the period of aggregation: \option{hourly}, \option{daily}, \option{monthly},  \option{annual}  or \option{climatic}. \bold{NOTE_1}: the 'climatic' option returns the all series annual statistics ('fun'). \bold{NOTE_2}: if the object is of class \code{hydroMet_IANIGLA} you must provide a single \code{period} value. 
#' @param out_name optional. Single or vector string with the output column name of the variable to aggregate.
#' @param start_month optional. Numeric (or numeric vector) value of the first month. It only makes sense if the \option{period} is \option{annual}. \bold{NOTE}: as an example, in case you have just two slots (out of five) that you want to aggregate annually you must provide a vector of length two. Default value is January. \bold{NOTE*}: if the object is of class \code{hydroMet_IANIGLA} you must provide a single \code{start_month} value.
#' @param end_month optional. Numeric (or numeric vector) value of the last month. It only makes sense if the \option{period} is \option{annual}. \bold{NOTE}: as an example, in case you have just two slots (out of five) that you want to aggregate annually you must provide a vector of length two. Default value es December. \bold{NOTE*}: if the object is of class \code{hydroMet_IANIGLA} you must provide a single \code{end_month} value.
#' @param allow_NA optional. Numeric (or numeric vector) value with the maximum allowed number of \code{NA_real_} values.  By default the function will not tolerate any \code{NA_real_} in an aggregation period (and will return \code{NA_real_} instead).
#'
#' @return An \code{hydroMet_XXX} class object with the required slot(s) aggregated. 
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
#' # Aggregrate precipitation serie
#' guido <- agg_hydroMet(obj = guido, slot_name = 'precip', col_name = 'precip', fun = 'sum',
#'        period = 'monthly', out_name = 'P_month', allow_NA = 3)
#'                
## Generico
setGeneric(name = 'agg_hydroMet', 
           def = function(obj, slot_name, col_name, fun, period, out_name = NULL,
                          start_month = NULL, end_month = NULL, allow_NA = NULL) 
           {
             standardGeneric('agg_hydroMet')
           })


#' @describeIn agg_hydroMet aggregation method for BDHI data
## BDHI
setMethod(f = 'agg_hydroMet', 
          signature = 'hydroMet_BDHI', 
          definition = function(obj, slot_name, col_name, fun, period, out_name = NULL,
                                start_month = NULL, end_month = NULL, allow_NA = NULL) 
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
            
            
            ## fun
            # fun: caracter
            if( is.character(fun) == FALSE ){
              return('fun argument must be of character class')
            }
            
            # fun: misma longitud que slot_name
            if(length(fun) != length(slot_name)){
              return('fun should have the same length as slot_name')
            }
            
            # fun: son correctas las funciones?
            allow_fun <- c('mean', 'min', 'max', 'sum')
            match_fun <- match(x = fun, table = allow_fun)
            
            if( is.na( sum(match_fun) ) == TRUE ){
              return('Unless one of the fun arguments is wrong. Allowed values are: mean, min, max, sum')
            }
            
            
            ## period
            # period: es caracter
            if( is.character(period) == FALSE ){
              return('period argument must be of character class')
            }
            
            # period: misma longitud que slot_name
            if(length(period) != length(slot_name)){
              return('period should have the same length as slot_name')
            }
            
            # perdiod: son correctos los periodos?
            allow_period <- c('hourly', 'daily', 'monthly', 'annual', 'climatic')
            match_period <-  match(x = period, table = allow_period)
            
            if( is.na( sum(match_period) ) == TRUE ){
              return('Unless one of the perdiod arguments is wrong. Allowed values are: hourly, daily, monthly, annual')
            }
            
            
            ## out_name
            if( is.null(out_name) == FALSE ){
              # out_name: es caracter
              if( is.character(out_name) == FALSE ){
                return('out_name argument must be of character class')
              }
              
              # out_name: es de la misma longitud que slot_name
              if( length(out_name) != length(slot_name) ){
                return('out_name should have the same length as slot_name argument')
              }
              
            } else {
              out_name <- paste0(slot_name, '_', fun)
            }
            
            
            ## start_month y end_month
            annual_flag <- which(period == 'annual' | period == 'climatic')  # obtengo la posicion de los valores anuales
            N_annual    <- length(annual_flag) # cantidad de slots con perido anual
            
            # start_month: es nulo?
            if( is.null(start_month) == TRUE ){
              start_month <- rep(1, N_annual)
              
            } else {
              # es numerico
              if( is.numeric(start_month) == FALSE) {
                return('start_month argument must be numeric or NULL')
              }
              
              # es NA_real_
              if( is.na(start_month) == TRUE) {
                return('start_month argument NA_real_ is not allowed. Provide a month as number')
              }
              
              # tiene la misma longitud que N_annual
              if( length(start_month) != N_annual ){
                return('start_month length must be the same as the number of annual periods')
              }
              
            }
            
            # end_month: es nulo?
            if( is.null(end_month) == TRUE ){
              end_month <- rep(12, N_annual)
              
            } else {
              # es numerico
              if( is.numeric(end_month) == FALSE) {
                return('end_month argument must be numeric or NULL')
              }
              
              # es NA_real_
              if( is.na(end_month) == TRUE) {
                return('end_month argument NA_real_ is not allowed. Provide a month as number')
              }
              
              # tiene la misma longitud que N_annual
              if( length(end_month) != N_annual ){
                return('end_month length must be the same as the number of annual periods')
              }
              
            }
           
            
            ## allow_NA
            # allow_NA: es nulo?
            if( is.null(allow_NA) == TRUE){
              allow_NA <- rep(0, length(slot_name))
              
            } else {
              # numerico
              if(is.numeric(allow_NA) == FALSE){
                return('allow_NA argument must be numeric')
              }
              
              # misma longitud que slot_names
              if( length(allow_NA) != length(slot_name)){
                return('allow_NA should have the same length as slot_name argument')
              }
              
            }
            
            
            #********************************************************************** 
            # Comienzo con el metodo aggregate
            #********************************************************************** 
            
            #01# Extraigo los slots de interes
            slot_out <- get_hydroMet(obj = obj, name = slot_name)
            
            #02# Corro la funcion agg_serie para cada slot
            N_it <- length(slot_name)
            
            out_list <- list()
            for(i in 1:N_it){
              df_to_agg <- subset(x = slot_out[[i]], select = c('Date', col_name[i]) )
              df_out    <- agg_serie(df = df_to_agg, fun = fun[i], period = period[i], out_name = out_name[i], 
                                     start_month = start_month[i], end_month = end_month[i], allow_NA = allow_NA[i])
              
              # si es periodo anual acondiciono salida
              if(period[i] == 'annual'){
                df_out <- df_out[ , -2]
                
                colnames(df_out) <- c('Date', out_name[i])
                
              }
              
              
              out_list[[i]] <- df_out
              rm(i, df_to_agg, df_out)
            }
            
            #03# Asigno las series agregadas al objeto
            for(j in 1:N_it){
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


#' @describeIn agg_hydroMet aggregation method for DGI data
## DGI
setMethod(f = 'agg_hydroMet', 
          signature = 'hydroMet_DGI', 
          definition = function(obj, slot_name, col_name, fun, period, out_name = NULL,
                                start_month = NULL, end_month = NULL, allow_NA = NULL) 
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
            
            
            ## fun
            # fun: caracter
            if( is.character(fun) == FALSE ){
              return('fun argument must be of character class')
            }
            
            # fun: misma longitud que slot_name
            if(length(fun) != length(slot_name)){
              return('fun should have the same length as slot_name')
            }
            
            # fun: son correctas las funciones?
            allow_fun <- c('mean', 'min', 'max', 'sum')
            match_fun <- match(x = fun, table = allow_fun)
            
            if( is.na( sum(match_fun) ) == TRUE ){
              return('Unless one of the fun arguments is wrong. Allowed values are: mean, min, max, sum')
            }
            
            
            ## period
            # period: es caracter
            if( is.character(period) == FALSE ){
              return('period argument must be of character class')
            }
            
            # period: misma longitud que slot_name
            if(length(period) != length(slot_name)){
              return('period should have the same length as slot_name')
            }
            
            # perdiod: son correctos los periodos?
            allow_period <- c('hourly', 'daily', 'monthly', 'annual', 'climatic')
            match_period <-  match(x = period, table = allow_period)
            
            if( is.na( sum(match_period) ) == TRUE ){
              return('Unless one of the perdiod arguments is wrong. Allowed values are: hourly, daily, monthly, annual')
            }
            
            
            ## out_name
            if( is.null(out_name) == FALSE ){
              # out_name: es caracter
              if( is.character(out_name) == FALSE ){
                return('out_name argument must be of character class')
              }
              
              # out_name: es de la misma longitud que slot_name
              if( length(out_name) != length(slot_name) ){
                return('out_name should have the same length as slot_name argument')
              }
              
            } else {
              out_name <- paste0(slot_name, '_', fun)
            }
            
            
            ## start_month y end_month
            annual_flag <- which(period == 'annual' | period == 'climatic')  # obtengo la posicion de los valores anuales
            N_annual    <- length(annual_flag) # cantidad de slots con perido anual
            
            # start_month: es nulo?
            if( is.null(start_month) == TRUE ){
              start_month <- rep(1, N_annual)
              
            } else {
              # es numerico
              if( is.numeric(start_month) == FALSE) {
                return('start_month argument must be numeric or NULL')
              }
              
              # es NA_real_
              if( is.na(start_month) == TRUE) {
                return('start_month argument NA_real_ is not allowed. Provide a month as number')
              }
              
              # tiene la misma longitud que N_annual
              if( length(start_month) != N_annual ){
                return('start_month length must be the same as the number of annual periods')
              }
              
            }
            
            # end_month: es nulo?
            if( is.null(end_month) == TRUE ){
              end_month <- rep(12, N_annual)
              
            } else {
              # es numerico
              if( is.numeric(end_month) == FALSE) {
                return('end_month argument must be numeric or NULL')
              }
              
              # es NA_real_
              if( is.na(end_month) == TRUE) {
                return('end_month argument NA_real_ is not allowed. Provide a month as number')
              }
              
              # tiene la misma longitud que N_annual
              if( length(end_month) != N_annual ){
                return('end_month length must be the same as the number of annual periods')
              }
              
            }
            
            
            ## allow_NA
            # allow_NA: es nulo?
            if( is.null(allow_NA) == TRUE){
              allow_NA <- rep(0, length(slot_name))
              
            } else {
              # numerico
              if(is.numeric(allow_NA) == FALSE){
                return('allow_NA argument must be numeric')
              }
              
              # misma longitud que slot_names
              if( length(allow_NA) != length(slot_name)){
                return('allow_NA should have the same length as slot_name argument')
              }
              
            }
            
            
            #********************************************************************** 
            # Comienzo con el metodo aggregate
            #********************************************************************** 
            
            #01# Extraigo los slots de interes
            slot_out <- get_hydroMet(obj = obj, name = slot_name)
            
            #02# Corro la funcion agg_serie para cada slot
            N_it <- length(slot_name)
            
            out_list <- list()
            for(i in 1:N_it){
              df_to_agg <- subset(x = slot_out[[i]], select = c('Date', col_name[i]) )
              df_out    <- agg_serie(df = df_to_agg, fun = fun[i], period = period[i], out_name = out_name[i], 
                                     start_month = start_month[i], end_month = end_month[i], allow_NA = allow_NA[i])
              
              # si es periodo anual acondiciono salida
              if(period[i] == 'annual'){
                df_out <- df_out[ , -2]
                
                colnames(df_out) <- c('Date', out_name[i])
                
              }
              
              
              out_list[[i]] <- df_out
              rm(i, df_to_agg, df_out)
            }
            
            #03# Asigno las series agregadas al objeto
            for(j in 1:N_it){
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


#' @describeIn agg_hydroMet aggregation method for CR2 data
## CR2
setMethod(f = 'agg_hydroMet', 
          signature = 'hydroMet_CR2', 
          definition = function(obj, slot_name, col_name, fun, period, out_name = NULL,
                                start_month = NULL, end_month = NULL, allow_NA = NULL) 
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
            
            
            ## fun
            # fun: caracter
            if( is.character(fun) == FALSE ){
              return('fun argument must be of character class')
            }
            
            # fun: misma longitud que slot_name
            if(length(fun) != length(slot_name)){
              return('fun should have the same length as slot_name')
            }
            
            # fun: son correctas las funciones?
            allow_fun <- c('mean', 'min', 'max', 'sum')
            match_fun <- match(x = fun, table = allow_fun)
            
            if( is.na( sum(match_fun) ) == TRUE ){
              return('Unless one of the fun arguments is wrong. Allowed values are: mean, min, max, sum')
            }
            
            
            ## period
            # period: es caracter
            if( is.character(period) == FALSE ){
              return('period argument must be of character class')
            }
            
            # period: misma longitud que slot_name
            if(length(period) != length(slot_name)){
              return('period should have the same length as slot_name')
            }
            
            # perdiod: son correctos los periodos?
            allow_period <- c('hourly', 'daily', 'monthly', 'annual', 'climatic')
            match_period <-  match(x = period, table = allow_period)
            
            if( is.na( sum(match_period) ) == TRUE ){
              return('Unless one of the perdiod arguments is wrong. Allowed values are: hourly, daily, monthly, annual')
            }
            
            
            ## out_name
            if( is.null(out_name) == FALSE ){
              # out_name: es caracter
              if( is.character(out_name) == FALSE ){
                return('out_name argument must be of character class')
              }
              
              # out_name: es de la misma longitud que slot_name
              if( length(out_name) != length(slot_name) ){
                return('out_name should have the same length as slot_name argument')
              }
              
            } else {
              out_name <- paste0(slot_name, '_', fun)
            }
            
            
            ## start_month y end_month
            annual_flag <- which(period == 'annual' | period == 'climatic')  # obtengo la posicion de los valores anuales
            N_annual    <- length(annual_flag) # cantidad de slots con perido anual
            
            # start_month: es nulo?
            if( is.null(start_month) == TRUE ){
              start_month <- rep(1, N_annual)
              
            } else {
              # es numerico
              if( is.numeric(start_month) == FALSE) {
                return('start_month argument must be numeric or NULL')
              }
              
              # es NA_real_
              if( is.na(start_month) == TRUE) {
                return('start_month argument NA_real_ is not allowed. Provide a month as number')
              }
              
              # tiene la misma longitud que N_annual
              if( length(start_month) != N_annual ){
                return('start_month length must be the same as the number of annual periods')
              }
              
            }
            
            # end_month: es nulo?
            if( is.null(end_month) == TRUE ){
              end_month <- rep(12, N_annual)
              
            } else {
              # es numerico
              if( is.numeric(end_month) == FALSE) {
                return('end_month argument must be numeric or NULL')
              }
              
              # es NA_real_
              if( is.na(end_month) == TRUE) {
                return('end_month argument NA_real_ is not allowed. Provide a month as number')
              }
              
              # tiene la misma longitud que N_annual
              if( length(end_month) != N_annual ){
                return('end_month length must be the same as the number of annual periods')
              }
              
            }
            
            
            ## allow_NA
            # allow_NA: es nulo?
            if( is.null(allow_NA) == TRUE){
              allow_NA <- rep(0, length(slot_name))
              
            } else {
              # numerico
              if(is.numeric(allow_NA) == FALSE){
                return('allow_NA argument must be numeric')
              }
              
              # misma longitud que slot_names
              if( length(allow_NA) != length(slot_name)){
                return('allow_NA should have the same length as slot_name argument')
              }
              
            }
            
            
            #********************************************************************** 
            # Comienzo con el metodo aggregate
            #********************************************************************** 
            
            #01# Extraigo los slots de interes
            slot_out <- get_hydroMet(obj = obj, name = slot_name)
            
            #02# Corro la funcion agg_serie para cada slot
            N_it <- length(slot_name)
            
            out_list <- list()
            for(i in 1:N_it){
              df_to_agg <- subset(x = slot_out[[i]], select = c('Date', col_name[i]) )
              df_out    <- agg_serie(df = df_to_agg, fun = fun[i], period = period[i], out_name = out_name[i], 
                                     start_month = start_month[i], end_month = end_month[i], allow_NA = allow_NA[i])
              
              # si es periodo anual acondiciono salida
              if(period[i] == 'annual'){
                df_out <- df_out[ , -2]
                
                colnames(df_out) <- c('Date', out_name[i])
                
              }
              
              
              out_list[[i]] <- df_out
              rm(i, df_to_agg, df_out)
            }
            
            #03# Asigno las series agregadas al objeto
            for(j in 1:N_it){
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


#' @describeIn agg_hydroMet aggregation method for IANIGLA data
## IANIGLA
setMethod(f = 'agg_hydroMet', 
          signature = 'hydroMet_IANIGLA', 
          definition = function(obj, slot_name, col_name, fun, period, out_name = NULL,
                                start_month = NULL, end_month = NULL, allow_NA = NULL) 
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
            
            
            ## fun
            # fun: caracter
            if( is.character(fun) == FALSE ){
              return('fun argument must be of character class')
            }
            
            # fun: misma longitud que slot_name
            if(length(fun) != length(slot_name)){
              return('fun should have the same length as slot_name')
            }
            
            # fun: son correctas las funciones?
            allow_fun <- c('mean', 'min', 'max', 'sum')
            match_fun <- match(x = fun, table = allow_fun)
            
            if( is.na( sum(match_fun) ) == TRUE ){
              return('Unless one of the fun arguments is wrong. Allowed values are: mean, min, max, sum')
            }
            
            
            ## period
            # period: es caracter
            if( is.character(period) == FALSE ){
              return('period argument must be of character class')
            }
            
            # period: longitud unitaria
            if(length(period) != 1){
              return('period should be of length one')
            }
            
            # perdiod: son correctos los periodos?
            allow_period <- c('hourly', 'daily', 'monthly', 'annual', 'climatic')
            match_period <-  match(x = period, table = allow_period)
            
            if( is.na( sum(match_period) ) == TRUE ){
              return('The perdiod argument is wrong. Allowed values are: hourly, daily, monthly, annual')
            }
            
            period <- rep(period, length(slot_name)) # para que pueda usar el bucle sin inconvenientes
            
            ## out_name
            if( is.null(out_name) == FALSE ){
              # out_name: es caracter
              if( is.character(out_name) == FALSE ){
                return('out_name argument must be of character class')
              }
              
              # out_name: es de la misma longitud que slot_name
              if( length(out_name) != length(slot_name) ){
                return('out_name should have the same length as slot_name argument')
              }
              
            } else {
              out_name <- paste0(slot_name, '_', fun)
            }
            
            
            ## start_month y end_month
            annual_flag <- which(period == 'annual' | period == 'climatic')  # obtengo la posicion de los valores anuales
            N_annual    <- length(annual_flag) # cantidad de slots con perido anual
            
            # start_month: es nulo?
            if( is.null(start_month) == TRUE ){
              start_month <- rep(1, N_annual)
              
            } else {
              # que sea unico
              if(length(start_month) != 1 ){
                return('start_month should be of length one')
              }
              
              start_month <- rep(start_month, length(slot_name))  
              
              # es numerico
              if( is.numeric(start_month) == FALSE) {
                return('start_month argument must be numeric or NULL')
              }
              
              # es NA_real_
              if( is.na(start_month) == TRUE) {
                return('start_month argument NA_real_ is not allowed. Provide a month as number')
              }
              
              # tiene la misma longitud que N_annual
              if( length(start_month) != N_annual ){
                return('start_month length must be the same as the number of annual periods')
              }
              
            }
            
            # end_month: es nulo?
            if( is.null(end_month) == TRUE ){
              end_month <- rep(12, N_annual)
              
            } else {
              # que sea unico
              if(length(end_month) != 1 ){
                return('end_month should be of length one')
              }
              
              end_month <- rep(end_month, length(slot_name))  
              
              # es numerico
              if( is.numeric(end_month) == FALSE) {
                return('end_month argument must be numeric or NULL')
              }
              
              # es NA_real_
              if( is.na(end_month) == TRUE) {
                return('end_month argument NA_real_ is not allowed. Provide a month as number')
              }
              
              # tiene la misma longitud que N_annual
              if( length(end_month) != N_annual ){
                return('end_month length must be the same as the number of annual periods')
              }
              
            }
            
            
            ## allow_NA
            # allow_NA: es nulo?
            if( is.null(allow_NA) == TRUE){
              allow_NA <- rep(0, length(slot_name))
              
            } else {
              # numerico
              if(is.numeric(allow_NA) == FALSE){
                return('allow_NA argument must be numeric')
              }
              
              # misma longitud que slot_names
              if( length(allow_NA) != length(slot_name)){
                return('allow_NA should have the same length as slot_name argument')
              }
              
            }
            
            
            #********************************************************************** 
            # Comienzo con el metodo aggregate
            #********************************************************************** 
            
            #01# Extraigo los slots de interes
            date_out <- get_hydroMet(obj = obj, name = 'date')
            var_out  <- get_hydroMet(obj = obj, name = slot_name)
            
            # armo los data frames
            slot_out <- lapply(X = var_out, FUN = function(x){
                         out <- data.frame(date_out, x)
                         return(out) })
            
            # les asigno los nombres a las columnas
            # for(i in 1:length(slot_name)){
            #   colnames(slot_out[[i]]) <- c('Date', col_name[i])
            # }
            # rm(i)
            
            #02# Corro la funcion agg_serie para cada slot
            N_it <- length(slot_name)
            
            out_list <- list()
            for(i in 1:N_it){
              df_to_agg <- subset(x = slot_out[[i]], select = c('date', col_name[i]) )
              df_out    <- agg_serie(df = df_to_agg, fun = fun[i], period = period[i], out_name = out_name[i], 
                                     start_month = start_month[i], end_month = end_month[i], allow_NA = allow_NA[i])
              
              # si es periodo anual acondiciono salida
              if(period[i] == 'annual'){
                df_out <- df_out[ , -2]
                
                colnames(df_out) <- c('Date', out_name[i])
                
              }
              
              
              out_list[[i]] <- df_out
              rm(i, df_to_agg, df_out)
            }
            
            #03# Asigno las series agregadas al objeto
            # lo hago para las variables
            for(j in 1:N_it){
              texto <- paste0('set_hydroMet(obj = obj,', slot_name[j], '=',
                              'as.matrix( out_list[[', j, ']][ , 2, drop = FALSE] )', ')')
              obj   <- eval( parse(text = texto) )
              
            }
            
            # lo aplico al slot con las fechas
            texto <- paste0('set_hydroMet(obj = obj,', 'date' , '=',
                            'out_list[[', j, ']][ , 1]', ')')
            
            obj   <- eval( parse(text = texto) )
            
            #********************************************************************** 
            
            # Valido objeto
            validObject(obj)
            
            # Lo que debe regresar el método
            return(obj)
            
            
          } # fin funcion
)