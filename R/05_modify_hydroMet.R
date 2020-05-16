# **********************************************************
# Author       : Ezequiel Toum
# Licence      : GPL V3 
# Institution  : IANIGLA-CONICET 
# e-mail       : etoum@mendoza-conicet.gob.ar
# **********************************************************
#
#' Modify data inside a specific slot
#'
#' @description Apply a pre-defined (e.g.: \link{movAvg}, \link{fill_value} or \link{Qmm_to_Dm}) or user defined function to an existing series inside a slot.
#' 
#' @param obj hydroMet_XXX subclass object. See \link{hydroMet_BDHI}, \link{hydroMet_DGI}, \link{hydroMet_IANIGLA} or \link{hydroMet_CR2}.
#' @param name string with the slot name of the data frame. 
#' @param colName string with the new column name (from \code{FUN}). 
#' @param colNum numeric value with the data frame column where to apply \code{FUN}. It must be > 1 (except in 'IANIGLA' subclass).
#' @param FUN the function name.
#' @param ... \code{FUN} arguments to pass.
#'
#' @return The same hydroMet subclass provided in \code{obj} with an extra column. 
#' 
#' @importFrom methods slotNames
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
#' # Now assign as names the files
#' hydro_files   <- list.files( system.file('extdata', package = "hydroToolkit"), pattern = 'Guido' )
#' names(cargar) <- hydro_files
#' 
#' # Build the object with the met records
#' guido <- build_hydroMet(obj = guido, slot_list = cargar, 
#'                path = system.file('extdata', package = "hydroToolkit") )
#' 
## Generico
setGeneric(name = 'modify_hydroMet', 
           def = function(obj, name = NA_character_, colName = NA_character_, colNum = 2, FUN = NULL, ...) 
{
  standardGeneric('modify_hydroMet')
})


#' @describeIn modify_hydroMet modify method for BDHI class 
## BDHI
setMethod(f = 'modify_hydroMet', 
          signature = 'hydroMet_BDHI', 
          definition = function(obj, name = NA_character_, colName = NA_character_, colNum = 2, FUN = NULL, ...) 
            {
            
            # Condicionales
              # name
            # name: nombre como caracter
            if(is.na(name) == TRUE){
              return('name argument should have a slot name')
            }
            
            # name: que sea una sola palabra
            N <- length(name)
            if(N > 1){
              return('name argument must be of length one')
            }
            
            # name: que sea un slot valido
            if(length( which(slotNames('hydroMet_BDHI') == name) ) != 1){
              return('Invalid name argument; it must be one of the slots names')
            }
            
              # colName
            # colName: que sea caracter
            if(is.na(colName) == TRUE){
              return('You must provide a colName character')
            }
            
            # colName: que sea una sola palabra
            if( length(colName) > 1 ){
              return('colName length must be one')
            }
            
            # colName: que sea caracter
            if( is.character(colName) == FALSE ){
              return('colName should be of character class')
            }
            
              # colNum = numero de la columna sobre el que aplico la funcion
            # colNum: que sea un numeric
            if(is.numeric(colNum) == FALSE){
              return('colNum argument must be numeric')
            }
            
            # colNum: que sea un solo numero
            if(length(colNum) > 1){
              return('colNum length must be one')
            }
            
            # colNum: que sea mayor a 1
            if(colNum <= 1){
              return('colNum must be > 1')
            }
            
            # FUN
            # FUN: que haya funcion
            if(is.null(FUN) == TRUE){
              return('FUN argument must be non-NULL')
            }
            
            # FUN: que exista una clase function
            if(class(FUN) != 'function'){
              return('FUN argument must be of class function')
            }
            
            ## 
            
            # Extraigo el slot y aplico la funcion
            allSerie <- get_hydroMet(obj = obj, name = name)
            
              # ¿esta vacia?
            if( nrow(allSerie[[1]]) == 0 ){
              return('You cannot apply a function on an empty data frame')
            }
            
              # ¿es coherente el colNum?
            if( ncol(allSerie[[1]]) < colNum){
              return('colNum arguments exceeds number of colmns in data frame')
            }
            
            series   <- allSerie[[1]][ , c(1, colNum)]
            newSerie <- FUN(series, ...) # aplico funcion
            
            # chequeo que la salida de la función sea un data frame 
            if( is.data.frame(newSerie) == FALSE ){
              return('The output of FUN must be a data.frame with | Date | NewSerie | as first and second column')
              }
            
            if(class(newSerie[ , 1])[1] != 'Date' & class(newSerie[ , 1])[1] != 'POSIXct' ){
              return('FUN output must be a data.frame with Date or POSIXct class as first column')
            }  
              
            if( nrow(series) != nrow(newSerie) ){
              return('FUN output data frame should have the same number of rows as the original data.frame')
            }
            
            # Hago la unión entre los data frame
            origin_name <- colnames( allSerie[[1]] ) # nombres del data frame original
            
            colnames(newSerie) <- c(origin_name[1], colName)
            
            out_df <- merge(x = allSerie[[1]], y = newSerie, all = TRUE)
            
            
            # Asigno el nuevo data.frame al objeto hydroMet_XXX
            eval( parse(text = paste0('obj', '@', name, '<-', 'out_df') ) )
            
            # Valido objeto
            validObject(obj)
            
            # Lo que debe regresar el método
            return(obj)
              
              
            } # fin funcion
)


#' @describeIn modify_hydroMet modify method for CR2 class 
## CR2
setMethod(f = 'modify_hydroMet', 
          signature = 'hydroMet_CR2', 
          definition = function(obj, name = NA_character_, colName = NA_character_, colNum = 2, FUN = NULL, ...) 
          {
            
            # Condicionales
              # name
            # name: nombre como caracter
            if(is.na(name) == TRUE){
              return('name argument should have a slot name')
            }
            
            # name: que sea una sola palabra
            N <- length(name)
            if(N > 1){
              return('name argument must be of length one')
            }
            
            # name: que sea un slot valido
            if(length( which(slotNames('hydroMet_CR2') == name) ) != 1){
              return('Invalid name argument; it must be one of the slots names')
            }
            
              # colName
            # colName: que sea caracter
            if(is.na(colName) == TRUE){
              return('You must provide a colName character')
            }
            
            # colName: que sea una sola palabra
            if( length(colName) > 1 ){
              return('colName length must be one')
            }
            
            # colName: que sea caracter
            if( is.character(colName) == FALSE ){
              return('colName should be of character class')
            }
            
              # colNum = numero de la columna sobre el que aplico la funcion
            # colNum: que sea un numeric
            if(is.numeric(colNum) == FALSE){
              return('colNum argument must be numeric')
            }
            
            # colNum: que sea un solo numero
            if(length(colNum) > 1){
              return('colNum length must be one')
            }
            
            # colNum: que sea mayor a 1
            if(colNum <= 1){
              return('colNum must be > 1')
            }
            
            # FUN
            # FUN: que haya funcion
            if(is.null(FUN) == TRUE){
              return('FUN argument must be non-NULL')
            }
            
            # FUN: que exista una clase function
            if(class(FUN) != 'function'){
              return('FUN argument must be of class function')
            }
            
            ## 
            # Extraigo el slot y aplico la funcion
            allSerie <- get_hydroMet(obj = obj, name = name)
            
            # ¿esta vacia?
            if( nrow(allSerie[[1]]) == 0 ){
              return('You cannot apply a function on an empty data frame')
            }
            
            # ¿es coherente el colNum?
            if( ncol(allSerie[[1]]) < colNum){
              return('colNum arguments exceeds number of colmns in data frame')
            }
            
            
            # Extraigo el slot y aplico la funcion
            series   <- allSerie[[1]][ , c(1, colNum)]
            newSerie <- FUN(series, ...) # aplico funcion
            
            # chequeo que la salida de la función sea un data frame 
            if( is.data.frame(newSerie) == FALSE ){
              return('The output of FUN must be a data.frame with | Date | NewSerie | as first and second column')
            }
            
            if(class(newSerie[ , 1])[1] != 'Date' & class(newSerie[ , 1])[1] != 'POSIXct' ){
              return('FUN output must be a data.frame with Date or POSIXct class as first column')
            }  
            
            if( nrow(series) != nrow(newSerie) ){
              return('FUN output data frame should have the same number of rows as the original data.frame')
            }
            
            # Hago la unión entre los data frame
            origin_name <- colnames( allSerie[[1]] ) # nombres del data frame original
            
            colnames(newSerie) <- c(origin_name[1], colName)
            
            out_df <- merge(x = allSerie[[1]], y = newSerie, all = TRUE)
            
            
            # Asigno el nuevo data.frame al objeto hydroMet_XXX
            eval( parse(text = paste0('obj', '@', name, '<-', 'out_df') ) )
            
            # Valido objeto
            validObject(obj)
            
            # Lo que debe regresar el método
            return(obj)
            
            
          } # fin funcion
)


#' @describeIn modify_hydroMet modify method for DGI class 
## DGI
setMethod(f = 'modify_hydroMet', 
          signature = 'hydroMet_DGI', 
          definition = function(obj, name = NA_character_, colName = NA_character_, colNum = 2, FUN = NULL, ...) 
          {
            
            # Condicionales
            # name
            # name: nombre como caracter
            if(is.na(name) == TRUE){
              return('name argument should have a slot name')
            }
            
            # name: que sea una sola palabra
            N <- length(name)
            if(N > 1){
              return('name argument must be of length one')
            }
            
            # name: que sea un slot valido
            if(length( which(slotNames('hydroMet_DGI') == name) ) != 1){
              return('Invalid name argument; it must be one of the slots names')
            }
            
            # colName
            # colName: que sea caracter
            if(is.na(colName) == TRUE){
              return('You must provide a colName character')
            }
            
            # colName: que sea una sola palabra
            if( length(colName) > 1 ){
              return('colName length must be one')
            }
            
            # colName: que sea caracter
            if( is.character(colName) == FALSE ){
              return('colName should be of character class')
            }
            
            # colNum = numero de la columna sobre el que aplico la funcion
            # colNum: que sea un numeric
            if(is.numeric(colNum) == FALSE){
              return('colNum argument must be numeric')
            }
            
            # colNum: que sea un solo numero
            if(length(colNum) > 1){
              return('colNum length must be one')
            }
            
            # colNum: que sea mayor a 1
            if(colNum <= 1){
              return('colNum must be > 1')
            }
            
            # FUN
            # FUN: que haya funcion
            if(is.null(FUN) == TRUE){
              return('FUN argument must be non-NULL')
            }
            
            # FUN: que exista una clase function
            if(class(FUN) != 'function'){
              return('FUN argument must be of class function')
            }
            
            ## 
            # Extraigo el slot y aplico la funcion
            allSerie <- get_hydroMet(obj = obj, name = name)
            
            # ¿esta vacia?
            if( nrow(allSerie[[1]]) == 0 ){
              return('You cannot apply a function on an empty data frame')
            }
            
            # ¿es coherente el colNum?
            if( ncol(allSerie[[1]]) < colNum){
              return('colNum arguments exceeds number of colmns in data frame')
            }
            
            # Extraigo el slot y aplico la funcion
            series   <- allSerie[[1]][ , c(1, colNum)]
            newSerie <- FUN(series, ...) # aplico funcion
            
            # chequeo que la salida de la función sea un data frame 
            if( is.data.frame(newSerie) == FALSE ){
              return('The output of FUN must be a data.frame with | Date | NewSerie | as first and second column')
            }
            
            if(class(newSerie[ , 1])[1] != 'Date' & class(newSerie[ , 1])[1] != 'POSIXct' ){
              return('FUN output must be a data.frame with Date or POSIXct class as first column')
            }  
            
            if( nrow(series) != nrow(newSerie) ){
              return('FUN output data frame should have the same number of rows as the original data.frame')
            }
            
            # Hago la unión entre los data frame
            origin_name <- colnames( allSerie[[1]] ) # nombres del data frame original
            
            colnames(newSerie) <- c(origin_name[1], colName)
            
            out_df <- merge(x = allSerie[[1]], y = newSerie, all = TRUE)
            
            
            # Asigno el nuevo data.frame al objeto hydroMet_XXX
            eval( parse(text = paste0('obj', '@', name, '<-', 'out_df') ) )
            
            # Valido objeto
            validObject(obj)
            
            # Lo que debe regresar el método
            return(obj)
            
            
          } # fin funcion
)


#' @describeIn modify_hydroMet modify method for IANIGLA class 
## IANIGLA
setMethod(f = 'modify_hydroMet', 
          signature = 'hydroMet_IANIGLA', 
          definition = function(obj, name = NA_character_, colName = NA_character_, colNum = 1, FUN = NULL, ...) 
          {
            
            # Condicionales
            # name
            # name: nombre como caracter
            if(is.na(name) == TRUE){
              return('name argument should have a slot name')
            }
            
            # name: que sea una sola palabra
            N <- length(name)
            if(N > 1){
              return('name argument must be of length one')
            }
            
            # name: que sea un slot valido
            if(length( which(slotNames('hydroMet_IANIGLA') == name) ) != 1){
              return('Invalid name argument; it must be one of the slots names')
            }
            
            # name: que sea uno que contenga matrices
            # tair ocupa el segundo lugar y hwat el lugar 11
            if( which(slotNames('hydroMet_IANIGLA') == name) < 2 | which(slotNames('hydroMet_IANIGLA') == name) > 11){
              return('name argument must be one of: tair - hr - patm - precip - wspd - wdir - kin - hsnow - tsoil - hwat')
            }
            
            # colName
            # colName: que sea caracter
            if(is.na(colName) == TRUE){
              return('You must provide a colName character')
            }
            
            # colName: que sea una sola palabra
            if( length(colName) > 1 ){
              return('colName length must be one')
            }
            
            # colName: que sea caracter
            if( is.character(colName) == FALSE ){
              return('colName should be of character class')
            }
            
            # colNum = numero de la columna sobre el que aplico la funcion
            # colNum: que sea un numeric
            if(is.numeric(colNum) == FALSE){
              return('colNum argument must be numeric')
            }
            
            # colNum: que sea un solo numero
            if(length(colNum) > 1){
              return('colNum length must be one')
            }
            
            # colNum: que sea mayor a 1
            if(colNum < 1){
              return('colNum must be >= 1')
            }
            
            # FUN
            # FUN: que haya funcion
            if(is.null(FUN) == TRUE){
              return('FUN argument must be non-NULL')
            }
            
            # FUN: que exista una clase function
            if(class(FUN) != 'function'){
              return('FUN argument must be of class function')
            }
            
            ## 
            
            # Extraigo fechas
            fechas <- get_hydroMet(obj = obj, name = 'date')[[1]]
            
            # Extraigo matriz de interes
            m_data <- get_hydroMet(obj = obj, name = name)[[1]]
            
              # ¿colNum es mayor que el maximo numero de columnas de la matriz?
            if( ncol(m_data) < colNum ){
              return('colNum argument cannot exceed the number of matrix columns')
            }
            
              # ¿matriz tiene datos?
            if( dim(m_data)[1] == 1L ){
              return('You cannot apply a function on an empty matrix')
            }
            
              # ¿tienen nombre las columnas?
                # Si: siga
                # No: asigno el nombre(s) del slot, e.g.: hr_i
            
            nom_m_data <- colnames(m_data) # nombre de las columnas
            if( is.null(nom_m_data) == TRUE){
              Ncol_m_data <- ncol(m_data)
              
              colnames(m_data) <- c( paste0(name, '_', 1:Ncol_m_data) )
              
              nom_m_data <- colnames(m_data)
              
              }
              
            # Armo el data frame 
            df_data <- data.frame(fechas, m_data[ , colNum]) # con columna de interes
            
            # Aplico la funcion
            df_new  <- FUN(df_data, ...)
            
            # validaciones de salida
              # ¿salida de funcion es data frame?
            if( is.data.frame(df_new) == FALSE){
              return('The output of FUN must be a data.frame with | Date | NewSerie | as first and second column')
            }
            
              # ¿primer columna Date o POSIXct?
            if(class(df_new[ , 1])[1] != 'Date' & class(df_new[ , 1])[1] != 'POSIXct' ){
              return('FUN output must be a data.frame with Date or POSIXct class as first column')
            } 
            
              # ¿mismo numero de filas?
            if( nrow(df_data) != nrow(df_new) ){
              return('FUN output data frame should have the same number of rows as the original data.frame')
            }
            
            
            # Uno las matrices
            m_out <- cbind(m_data, df_new[ , 2])
            
            colnames(m_out) <- c(nom_m_data, colName)
            
            
            # Asigno la nuev matriz al slot
            eval( parse(text = paste0('obj', '@', name, '<-', 'm_out') ) )
            
            # Valido objeto
            validObject(obj)
            
            # Lo que debe regresar el método
            return(obj)
            
            
          } # fin funcion
)