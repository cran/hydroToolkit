# **********************************************************
# Author       : Ezequiel Toum
# Licence      : GPL V3 
# Institution  : IANIGLA-CONICET 
# e-mail       : etoum@mendoza-conicet.gob.ar
# **********************************************************
#
#' Automatically load native data files 
#' 
#' @description This method is the recommended one for loading your data-sets (as provided by the agency). 
#' 
#' @param obj an \code{hydroMet_XXX} class object (see \link{create_hydroMet}).
#' @param slot_list a list containing (in each element) a vector string with the slot names. The name of the list elements are the native file names (e.g.: \emph{Qmd_Guido_BDHI.txt}). \bold{NOTE}: when the \code{obj} argument is of class \code{hydroMet_compact}, \code{slot_list} allows to build from multiple objects. So, in this case you have to provide a list of list: the top \code{list} contains as \code{names} the objects names (as you read them from \emph{Global Environment}); then every object (top level) contains another \code{list} with \code{slot} names as \code{names} and the column(s) number(s) to extract as numeric value. E.g.: \code{list(bdhi_obj = list(Qmd = 2, Qmm = c(2, 5)), cr2_obj = list(precip = 4) )}.
#' @param path string with the files directory. If not provided, the method will use the current working directory. \bold{NOTE}: this argument is harmless for an object of class \code{hydroMet_compact}.
#' @param col_names it just make sense if \code{'obj'} argument is of \code{hydroMet_compact} class. String vector with the names of the column output. Default value (NULL) will return expressive column names.
#' @param start_date it just make sense if \code{'obj'} argument is of \code{hydroMet_compact} class. String or POSIXct with the starting date to extract. You can use \code{start_date} without \code{end_date}. In this case you will subset your data from \code{start_date} till the end.
#' @param end_date it just make sense if \code{'obj'} argument is of \code{hydroMet_compact} class. String or POSIXct with the last date to extract. You can use \code{end_date} without \code{start_date}. In this case you will subset your data from the beginning till \code{end_date}.
#'
#' @return An S4 object of class \code{hydroMet_XXX} with the data loaded in each slot. 
#' 
#' @importFrom utils setTxtProgressBar txtProgressBar
#' 
#' @export
#'
#' @examples
#' # Path to file
#' dgi_path  <- system.file('extdata', package = "hydroToolkit")
#' file_name <- list.files(path = dgi_path, pattern = 'Toscas')
#' 
#' # Read Toscas
#' var_nom <- list(slotNames(x = 'hydroMet_DGI')[2:7])
#' names(var_nom) <- file_name
#'
#' # Load Toscas meteo station data
#' toscas_dgi <- create_hydroMet(class_name = 'DGI')
#' toscas_dgi <- build_hydroMet(obj = toscas_dgi, slot_list = var_nom,
#'                  path = dgi_path)
#' 
## Generico
setGeneric(name = 'build_hydroMet', 
           def = function(obj, slot_list, path = NULL,
                          col_names = NULL, start_date = NULL, end_date = NULL) 
           {
             standardGeneric('build_hydroMet')
           })


#' @describeIn build_hydroMet build up method for BDHI class
## BDHI
setMethod(f = 'build_hydroMet', 
          signature = 'hydroMet_BDHI', 
          definition = function(obj, slot_list, path = NULL) 
            {
            ## Condicionales
            
            ## path
            # path: directorio actual o nuevo
            if( is.null(path) == FALSE){
              # path: caracter
              if( is.character(path) == FALSE ){
                return('path must be of class character')
              }
              
              # path: longitud
              if( length(path) != 1 ){
                return('You should provide a single path')
              }
              
              # path: direccion valida
              if( length( dir(path = path) ) == 0 ){
                return('You provide an invalid directory path. Please check the path!')
              }
              
              
            } else {
              path <- getwd()
            }
            
            # asigno directorio de trabajo
            actual_path <- getwd()
            setwd(path)
            
            ## slot_list
            # slot_list: lista 
            if( is.list(slot_list) == FALSE ) {
              return('slot_list must be of class list')
            }
            
           # slot_list: que existan los nombres
            if( length( which( is.null( names(slot_list) ) == TRUE) ) != 0  ){
              return('All slot_list elements must have a character name')
            }
            
            # slot_list: nombres de archivos existentes
            archivos    <- list.files(path = path) # vector con archivos en el directorio
            file_names  <- names(slot_list)
            
            files_match <- match(x = file_names, table = archivos)
            if( is.na( sum(files_match) ) == TRUE){
              aux <- which( is.na(files_match)  )
              
              return( paste0(file_names[aux], ' file does not exist in the directory') )
            }
            
            # slot_list: que existan los slots
            all_slots    <- slotNames(x = 'hydroMet_BDHI')[1:13] # slots disponibles
            target_slots <- Reduce(c, slot_list) # slots que quiero levantar
            
            slot_match   <- match(x = target_slots, table = all_slots)
            if( is.na( sum(slot_match) ) == TRUE ){
              aux <- which( is.na(slot_match) == TRUE )
              
              return( paste0(target_slots[aux], ' is not a valid slot name. Check valid ones with slotNames() function') )
              
            }
          
          
            ## fin condicionales
            
            # Agrupo slots por resolucion temporal
            diarios <- c('Qmd', 'precip', 'tmax', 'tmin', 'swe', 'evap', 'anem')
            mensual <- 'Qmm'
            cuatroH <- 'patm'
            tresDia <- c('wspd', 'hr', 'tdb')
            
            # Levanto archivos 
            # itero sobre los archivos
            n_files <- length(slot_list) # cantidad de archivos
            for(i in 1:n_files){
              slot_names <- slot_list[[i]]
              n_slots    <- length( slot_names )
              
              # armo contador para mostrar barra
              counter   <- 0
              max_count <- n_files * n_slots
              
              pb <- txtProgressBar(min = 0, max = max_count, style = 3)
              
              # itero sobre los slots
              for(j in 1:n_slots){
                ##
                counter <- counter + 1
                setTxtProgressBar(pb, i)
                ##
                
                if( length( grep(pattern = slot_names[j], x = diarios) ) == 1 ){
                  read_aux <- read_BDHI(file = file_names[i], colName = slot_names[j], timeStep = 'day')
                  
                } else if( length( grep(pattern = slot_names[j], x = mensual) ) == 1  ) {
                  read_aux <- read_BDHI(file = file_names[i], colName = slot_names[j], timeStep = 'month')
                  
                } else if( length( grep(pattern = slot_names[j], x = cuatroH) ) == 1 ) {
                  read_aux <- read_BDHI(file = file_names[i], colName = slot_names[j], timeStep = '4h')
                  
                } else if( length( grep(pattern = slot_names[j], x = tresDia) ) == 1 ) {
                  read_aux <- read_BDHI(file = file_names[i], colName = slot_names[j], timeStep = 'day/3')
                  
                } else {
                  read_aux <- read_BDHI(file = file_names[i], colName = slot_names[j], timeStep = 'day/3', is.Wdir = TRUE)
                  
                }
                
                
                # asigo la variable al slot
                texto <- paste0('set_hydroMet(obj = obj,', slot_names[j], '=', 'read_aux', ')')
                obj   <- eval( parse(text = texto) )
                
                
                rm(read_aux, j)
                  
              }# fin loop for j
              
              
            }# fin loop for i
            
            setwd(actual_path)
            
            return(obj)
            
          } # fin funcion
          
)


#' @describeIn build_hydroMet build up method for CR2 class
## CR2
setMethod(f = 'build_hydroMet', 
          signature = 'hydroMet_CR2', 
          definition = function(obj, slot_list, path = NULL) 
          {
            ## Condicionales
            
            ## path
            # path: directorio actual o nuevo
            if( is.null(path) == FALSE){
              # path: caracter
              if( is.character(path) == FALSE ){
                return('path must be of class character')
              }
              
              # path: longitud
              if( length(path) != 1 ){
                return('You should provide a single path')
              }
              
              # path: direccion valida
              if( length( dir(path = path) ) == 0 ){
                return('You provide an invalid directory path. Please check the path!')
              }
              
              
            } else {
              path <- getwd()
            }
            
            # asigno directorio de trabajo
            actual_path <- getwd()
            setwd(path)
            
            ## slot_list
            # slot_list: lista 
            if( is.list(slot_list) == FALSE ) {
              return('slot_list must be of class list')
            }
            
            # slot_list: que existan los nombres
            if( length( which( is.null( names(slot_list) ) == TRUE) ) != 0  ){
              return('All slot_list elements must have a character name')
            }
            
            # slot_list: nombres de archivos existentes
            archivos    <- list.files(path = path) # vector con archivos en el directorio
            file_names  <- names(slot_list)
            
            files_match <- match(x = file_names, table = archivos)
            if( is.na( sum(files_match) ) == TRUE){
              aux <- which( is.na(files_match)  )
              
              return( paste0(file_names[aux], ' file does not exist in the directory') )
            }
            
            # slot_list: que existan los slots
            all_slots    <- slotNames(x = 'hydroMet_CR2')[1:4] # slots disponibles
            target_slots <- Reduce(c, slot_list) # slots que quiero levantar
            
            slot_match   <- match(x = target_slots, table = all_slots)
            if( is.na( sum(slot_match) ) == TRUE ){
              aux <- which( is.na(slot_match) == TRUE )
              
              return( paste0(target_slots[aux], ' is not a valid slot name. Check valid ones with slotNames() function') )
              
            }
            
            
            ## fin condicionales
            ###
            
            # Agrupo slots por resolucion temporal
            diarios <- c('precip', 'tmean', 'tmax', 'tmin')
            # mensual <- 'Qmm'
            # cuatroH <- 'patm'
            # tresDia <- c('wspd', 'hr', 'tdb')
            
            # Levanto archivos 
            # itero sobre los archivos
            n_files <- length(slot_list) # cantidad de archivos
            for(i in 1:n_files){
              slot_names <- slot_list[[i]]
              n_slots    <- length( slot_names )
              
              # armo contador para mostrar barra
              counter   <- 0
              max_count <- n_files * n_slots
              
              pb <- txtProgressBar(min = 0, max = max_count, style = 3)
              
              # itero sobre los slots
              for(j in 1:n_slots){
                ##
                counter <- counter + 1
                setTxtProgressBar(pb, i)
                ##
                
                read_aux <- read_CR2(file = file_names[i], colName = slot_names[j] )
                  
                
                # asigo la variable al slot
                texto <- paste0('set_hydroMet(obj = obj,', slot_names[j], '=', 'read_aux', ')')
                obj   <- eval( parse(text = texto) )
                
                
                rm(read_aux, j)
                
                
              }# fin loop for j
              
              
            }# fin loop for i
            
            setwd(actual_path)
            
            return(obj)
            
          } # fin funcion
          
)


#' @describeIn build_hydroMet build up method for DGI class
## DGI
setMethod(f = 'build_hydroMet', 
          signature = 'hydroMet_DGI', 
          definition = function(obj, slot_list, path = NULL) 
          {
            ## Condicionales
            
            ## path
            # path: directorio actual o nuevo
            if( is.null(path) == FALSE){
              # path: caracter
              if( is.character(path) == FALSE ){
                return('path must be of class character')
              }
              
              # path: longitud
              if( length(path) != 1 ){
                return('You should provide a single path')
              }
              
              # path: direccion valida
              if( length( dir(path = path) ) == 0 ){
                return('You provide an invalid directory path. Please check the path!')
              }
              
              
            } else {
              path <- getwd()
            }
            
            # asigno directorio de trabajo
            actual_path <- getwd()
            setwd(path)
            
            ## slot_list
            # slot_list: lista 
            if( is.list(slot_list) == FALSE ) {
              return('slot_list must be of class list')
            }
            
            # slot_list: que existan los nombres
            if( length( which( is.null( names(slot_list) ) == TRUE) ) != 0  ){
              return('All slot_list elements must have a character name')
            }
            
            # slot_list: nombres de archivos existentes
            archivos    <- list.files(path = path) # vector con archivos en el directorio
            file_names  <- names(slot_list)
            
            files_match <- match(x = file_names, table = archivos)
            if( is.na( sum(files_match) ) == TRUE){
              aux <- which( is.na(files_match)  )
              
              return( paste0(file_names[aux], ' file does not exist in the directory') )
            }
            
            # slot_list: que existan los slots
            all_slots    <- slotNames(x = 'hydroMet_DGI')[1:7] # slots disponibles
            target_slots <- Reduce(c, slot_list) # slots que quiero levantar
            
            slot_match   <- match(x = target_slots, table = all_slots)
            if( is.na( sum(slot_match) ) == TRUE ){
              aux <- which( is.na(slot_match) == TRUE )
              
              return( paste0(target_slots[aux], ' is not a valid slot name. Check valid ones with slotNames() function') )
              
            }
            
            
            ## fin condicionales
            
            # Agrupo slots por resolucion temporal
            diarios <- all_slots
            # mensual <- 'Qmm'
            # cuatroH <- 'patm'
            # tresDia <- c('wspd', 'hr', 'tdb')
            
            # Levanto archivos 
            # itero sobre los archivos
            n_files <- length(slot_list) # cantidad de archivos
            for(i in 1:n_files){
              slot_names <- slot_list[[i]]
              n_slots    <- length( slot_names )
              
              # armo contador para mostrar barra
              counter   <- 0
              max_count <- n_files * n_slots
              
              pb <- txtProgressBar(min = 0, max = max_count, style = 3)
              
              # itero sobre los slots
              for(j in 1:n_slots){
                ##
                counter <- counter + 1
                setTxtProgressBar(pb, i)
                ##
                
               read_aux <- read_DGI(file = file_names[i], sheet = slot_names[j], colName = slot_names[j], path = path)
                  
               # asigo la variable al slot
                texto <- paste0('set_hydroMet(obj = obj,', slot_names[j], '=', 'read_aux', ')')
                obj   <- eval( parse(text = texto) )
                
                rm(read_aux, j)
                
                
              }# fin loop for j
              
              
            }# fin loop for i
            
            setwd(actual_path)
            
            return(obj)
            
          } # fin funcion
          
)


#' @describeIn build_hydroMet build up method for IANIGLA class
## IANIGLA
setMethod(f = 'build_hydroMet', 
          signature = 'hydroMet_IANIGLA', 
          definition = function(obj, slot_list, path = NULL) 
          {
            ## Condicionales
            
            ## path
            # path: directorio actual o nuevo
            if( is.null(path) == FALSE){
              # path: caracter
              if( is.character(path) == FALSE ){
                return('path must be of class character')
              }
              
              # path: longitud
              if( length(path) != 1 ){
                return('You should provide a single path')
              }
              
              # path: direccion valida
              if( length( dir(path = path) ) == 0 ){
                return('You provide an invalid directory path. Please check the path!')
              }
              
              
            } else {
              path <- getwd()
            }
            
            # asigno directorio de trabajo
            actual_path <- getwd()
            setwd(path)
            
            ## slot_list
            # slot_list: lista 
            if( is.list(slot_list) == FALSE ) {
              return('slot_list must be of class list')
            }
            
            # slot_list: que existan los nombres
            if( length( which( is.null( names(slot_list) ) == TRUE) ) != 0  ){
              return('All slot_list elements must have a character name')
            }
            
            # slot_list: nombres de archivos existentes
            archivos    <- list.files(path = path) # vector con archivos en el directorio
            file_names  <- names(slot_list)
            
            files_match <- match(x = file_names, table = archivos)
            if( is.na( sum(files_match) ) == TRUE){
              aux <- which( is.na(files_match)  )
              
              return( paste0(file_names[aux], ' file does not exist in the directory') )
            }
            
            # slot_list: que existan los slots
            all_slots    <- slotNames(x = 'hydroMet_IANIGLA')[1:11] # slots disponibles
            target_slots <- Reduce(c, slot_list) # slots que quiero levantar
            
            slot_match   <- match(x = target_slots, table = all_slots)
            if( is.na( sum(slot_match) ) == TRUE ){
              aux <- which( is.na(slot_match) == TRUE )
              
              return( paste0(target_slots[aux], ' is not a valid slot name. Check valid ones with slotNames() function') )
              
            }
            
            
            ## fin condicionales
            
            # Agrupo slots por resolucion temporal
            horario <- all_slots
            # diarios <- c('Qmd', 'precip', 'tmax', 'tmin', 'swe', 'evap', 'anem')
            # mensual <- 'Qmm'
            # cuatroH <- 'patm'
            # tresDia <- c('wspd', 'hr', 'tdb')
            
            # Levanto archivos 
            # itero sobre los archivos
            n_files <- length(slot_list) # cantidad de archivos
            
            ##
            if(n_files != 1){
              return('In this version of hydroToolKit package just one file is allowed')
            }
            ##
            
            # levanto archivo
            read_aux <- read_IANIGLA(file = file_names[1])
            
            # extraigo el nombre de los slots
            slot_names <- slot_list[[1]]
            n_slots    <- ncol( read_aux )
            
            # armo contador para mostrar barra
            counter   <- 0
            max_count <- n_files * n_slots
            
            pb <- txtProgressBar(min = 0, max = max_count, style = 3)
            
            
            for(j in 1:n_slots){
              ##
              counter <- counter + 1
              setTxtProgressBar(pb, j)
              ##
              
              if(j == 1){
                # asigo la variable al slot
                texto <- paste0('set_hydroMet(obj = obj,', all_slots[j], '=', 'read_aux[ ,', j, '] ' , ')')
                obj   <- eval( parse(text = texto) )
                
              } else {
                # asigo la variable al slot
                texto <- paste0('set_hydroMet(obj = obj, ', all_slots[j], ' = ', 'matrix( read_aux[ ,', j, '], ' , 'ncol = 1)', ' )')
                obj   <- eval( parse(text = texto) )
                
              }
              
              
            }# fin loop for j
            
            
            setwd(actual_path)
            
            return(obj)
            
          } # fin funcion
          
)


#' @describeIn build_hydroMet build up method for \code{compact} class
## compact
setMethod(f = 'build_hydroMet', 
          signature = 'hydroMet_compact', 
          definition = function(obj, slot_list, 
                                col_names = NULL, start_date = NULL, end_date = NULL) 
          {
          #*******************************************
          ## Condicionales
          #*******************************************
           ## slot_list
            # slot_list: lista 
            if( is.list(slot_list) == FALSE ) {
              return('slot_list must be of class list')
            }
            
            # slot_list: que existan los nombres
            if( length( which( is.null( names(slot_list) ) == TRUE) ) != 0  ){
              return('All slot_list elements must have a character name')
            }
            
            # slot_list: nombres de archivos existentes en ambiente global
            archivos    <- ls(name = '.GlobalEnv') # vector con archivos ambiente global
            file_names  <- names(slot_list)
            
            files_match <- match(x = file_names, table = archivos)
            if( is.na( sum(files_match) ) == TRUE){
              aux <- which( is.na(files_match)  )
              
              return( paste0(file_names[aux], ' object does not exist in the global environment') )
            }
            
            # slot_list: que sean clases hydroMet_XXX validas
              # obtengo la clase de cada objeto
            target_class  <- c('hydroMet_BDHI', 'hydroMet_CR2', 'hydroMet_DGI', 'hydroMet_IANIGLA',
                               'hydroMet_compact') # clase formal (con nombre completo)
            compact_class <- c('bdhi', 'cr2', 'dgi', 'ian', 'comp') # nombres resumidos
            
            object_class  <- sapply(X = file_names, FUN = function(x){
              clase <- class( eval( parse(text = x) ) )[1]
              
              return(clase)
            }) # obtengo clase de objetos
            
            class_match <- match(x = object_class, table = target_class)
            if( is.na( sum(class_match) ) == TRUE ){
              aux <- which( is.na(class_match) )
              
              return( paste0(class_match[aux], ' object is not a hydroMet class. Allowed classes are: ', target_class) )
            }
            
            # slot_list: que existan los slots para cada objeto
            for(i in 1:length(slot_list)){
              all_slots    <- slotNames(x = object_class[i]) # slots disponibles en objeto
              target_slots <- names(slot_list[[i]])
              
              slot_match <- match(x = target_slots, table = all_slots)
              if( is.na( sum(slot_match) ) == TRUE ){
                aux <- which( is.na(slot_match) )
                
                return(paste0(slot_match[aux], ' slot in ', file_names[i], ' is not a valid slot name for an ', object_class[i], ' object.') )
              }
              
              rm(i, all_slots, target_slots, slot_match)
            }# fin bucle for
            
            # slot_list: que los numeros de columna sean numeros
            numeros_columna <- Reduce(f = c, x = Reduce(c, slot_list))
            
            if( is.numeric(numeros_columna) == FALSE ){
              return('The column numbers in slot_list argument must be of class numeric')
            }
            
            
            ## col_names
            if( is.null(col_names) == FALSE ){
              # col_names: que sean caracteres
              if( is.character(col_names) == FALSE ){
                return('col_names argument must be of class character')
              }
              
              # col_names: que sean la misma cantidad que numeros de columna
              if(length(col_names) != length(numeros_columna)){
                return('col_names length should equal to the ammount of columns to build')
              }
              
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
            
            #*******************************************
            #*******************************************
            # AYUDA: nombres utiles
            # file_names    => nombre de los objetos de clase hydroMet_XXX
            # target_class  => vector con los nombres formales de las clases
            # compact_class => vector con el nombre resumido de las clases
            # object_class  => la clase de cada uno de los file_names
            
            ###
            Date <- NULL
            ###
            
            # Flujo de trabajo
            #A# Repito para cada objeto
            n_it <- length(file_names)
            
            out_list <- list()
            for (i in 1:n_it) {
              #01# Extraigo los slots del elemento
              aux_nombres  <- paste0(names(slot_list[[i]]), collapse = ',')
              aux_nombres2 <- gsub(pattern = ',', replacement = '\",\"', x = aux_nombres)
              nombres_slot <- paste0('\"', aux_nombres2, '\"')
                
              texto    <- paste0('get_hydroMet(obj =', file_names[i], ', name = c(', nombres_slot, ') )')
              obj_list <- eval( parse(text = texto) ) # lista con los elementos del objeto
              max_col  <- lapply(X = obj_list, FUN = ncol) # numero de columna para comparar
                
              #02# Verifico que existan las columnas para cada variable
              for(j in 1:length(max_col)){
                flag <- which(slot_list[[i]][[j]] > max_col[[j]] )
                
                if(length(flag) >= 1){
                  return(paste0('One of the column numbers in ', names(slot_list[[i]][j]), '=',
                                slot_list[[i]][j], ' is out of range.') )
                }
                
                rm(flag, j)
              }
              
              #03# Recorto segun fechas y uno los data frames, o solamente uno data frames
              if(object_class[i] != 'hydroMet_IANIGLA'){
                # no IANIGLA class
                
                flag      <- which(target_class  == object_class[i])
                nom_class <- compact_class[flag]
                
                if( is.null(start_date) == FALSE & is.null(end_date) == TRUE ){
                  # solo start_date
                  
                  list_recortados <- list()
                  for(j in 1:length(obj_list) ){
                    list_recortados[[j]] <- subset(obj_list[[j]], subset = `Date` >= start_date,
                                                   select = c(1, slot_list[[i]][[j]]) )
                    
                    colnames( list_recortados[[j]] ) <- c('Date',  
                                                          paste0(file_names[i], '_', nom_class, '_', names(slot_list[[i]])[j],
                                                                  '_', slot_list[[i]][[j]]) )
                    
                  }# fin bucle for
                  
                  if(j > 1){
                    df_out <- Reduce(f = function(x, y){merge(x, y, all = TRUE)}, x = list_recortados)
                    
                  } else {
                    df_out <- list_recortados[[1]]
                  }
                  
                } else if( is.null(start_date) == TRUE & is.null(end_date) == FALSE ){
                  # solo end_date
                 
                  list_recortados <- list()
                  for(j in 1:length(obj_list) ){
                    list_recortados[[j]] <- subset(obj_list[[j]], subset = `Date` <= end_date,
                                                   select = c(1, slot_list[[i]][[j]]) )
                    
                    colnames( list_recortados[[j]] ) <- c('Date',  
                                                          paste0(file_names[i], '_', nom_class, '_', names(slot_list[[i]])[j],
                                                                 '_', slot_list[[i]][[j]]) )
                    
                  }# fin bucle for
                  
                  if(j > 1){
                    df_out <- Reduce(f = function(x, y){merge(x, y, all = TRUE)}, x = list_recortados)
                    
                  } else {
                    df_out <- list_recortados[[1]]
                  }
                  
                  
                  
                } else if( is.null(start_date) == FALSE & is.null(end_date) == FALSE ){
                  # ambos
                  
                  list_recortados <- list()
                  for(j in 1:length(obj_list) ){
                    list_recortados[[j]] <- subset(obj_list[[j]], subset = `Date` >= start_date & `Date` <= end_date,
                                                   select = c(1, slot_list[[i]][[j]]) )
                    
                    colnames( list_recortados[[j]] ) <- c('Date',  
                                                          paste0(file_names[i], '_', nom_class, '_', names(slot_list[[i]])[j],
                                                                 '_', slot_list[[i]][[j]]) )
                    
                  }# fin bucle for
                  
                  if(j > 1){
                    df_out <- Reduce(f = function(x, y){merge(x, y, all = TRUE)}, x = list_recortados)
                    
                  } else {
                    df_out <- list_recortados[[1]]
                  }
                  
                  
                } else {
                  # ninguno de los dos
                  
                  list_recortados <- list()
                  for(j in 1:length(obj_list) ){
                    list_recortados[[j]] <- subset(obj_list[[j]], select = c(1, slot_list[[i]][[j]]) )
                    
                    colnames( list_recortados[[j]] ) <- c('Date',  
                                                          paste0(file_names[i], '_', nom_class, '_', names(slot_list[[i]])[j],
                                                                 '_', slot_list[[i]][[j]]) )
                    
                  }# fin bucle for
                  
                  if(j > 1){
                    df_out <- Reduce(f = function(x, y){merge(x, y, all = TRUE)}, x = list_recortados)
                    
                  } else {
                    df_out <- list_recortados[[1]]
                  }
                  
                }
                
                
              } else {
                # IANIGLA class
                
                flag      <- which(target_class  == object_class[i])
                nom_class <- compact_class[flag]
                
                # a cada slot lo convierto en data frame con la fecha en la primer columna
                aux_text <- paste0('get_hydroMet(obj =', file_names[i], ', name = \"date\" )')
                aux_date <- eval( parse(text = aux_text) )[[1]]
                aux_obj  <- lapply(X = obj_list, function(x){
                  aux_name <- c('Date', colnames(x))
                  df_aux   <- data.frame(aux_date, x)
                  
                  colnames(df_aux) <- aux_name
                  return(df_aux)
                })
                
                obj_list <- aux_obj
                
                if( is.null(start_date) == FALSE & is.null(end_date) == TRUE ){
                  # solo start_date
                  
                  list_recortados <- list()
                  for(j in 1:length(obj_list) ){
                    list_recortados[[j]] <- subset(obj_list[[j]], subset = `Date` >= start_date,
                                                   select = c(1, slot_list[[i]][[j]] + 1) )
                    
                    colnames( list_recortados[[j]] ) <- c('Date',  
                                                          paste0(file_names[i], '_', nom_class, '_', names(slot_list[[i]])[j],
                                                                 '_', slot_list[[i]][[j]]) )
                    
                  }# fin bucle for
                  
                  if(j > 1){
                    df_out <- Reduce(f = function(x, y){merge(x, y, all = TRUE)}, x = list_recortados)
                    
                  } else {
                    df_out <- list_recortados[[1]]
                  }
                  
                } else if( is.null(start_date) == TRUE & is.null(end_date) == FALSE ){
                  # solo end_date
                  
                  list_recortados <- list()
                  for(j in 1:length(obj_list) ){
                    list_recortados[[j]] <- subset(obj_list[[j]], subset = `Date` <= end_date,
                                                   select = c(1, slot_list[[i]][[j]] + 1) )
                    
                    colnames( list_recortados[[j]] ) <- c('Date',  
                                                          paste0(file_names[i], '_', nom_class, '_', names(slot_list[[i]])[j],
                                                                 '_', slot_list[[i]][[j]]) )
                    
                  }# fin bucle for
                  
                  if(j > 1){
                    df_out <- Reduce(f = function(x, y){merge(x, y, all = TRUE)}, x = list_recortados)
                    
                  } else {
                    df_out <- list_recortados[[1]]
                  }
                  
                  
                  
                } else if( is.null(start_date) == FALSE & is.null(end_date) == FALSE ){
                  # ambos
                  
                  list_recortados <- list()
                  for(j in 1:length(obj_list) ){
                    list_recortados[[j]] <- subset(obj_list[[j]], subset = `Date` >= start_date & `Date` <= end_date,
                                                   select = c(1, slot_list[[i]][[j]] + 1) )
                    
                    colnames( list_recortados[[j]] ) <- c('Date',  
                                                          paste0(file_names[i], '_', nom_class, '_', names(slot_list[[i]])[j],
                                                                 '_', slot_list[[i]][[j]]) )
                    
                  }# fin bucle for
                  
                  if(j > 1){
                    df_out <- Reduce(f = function(x, y){merge(x, y, all = TRUE)}, x = list_recortados)
                    
                  } else {
                    df_out <- list_recortados[[1]]
                  }
                  
                  
                } else {
                  # ninguno de los dos
                  
                  list_recortados <- list()
                  for(j in 1:length(obj_list) ){
                    list_recortados[[j]] <- subset(obj_list[[j]], select = c(1, slot_list[[i]][[j]] + 1) )
                    
                    colnames( list_recortados[[j]] ) <- c('Date',  
                                                          paste0(file_names[i], '_', nom_class, '_', names(slot_list[[i]])[j],
                                                                 '_', slot_list[[i]][[j]]) )
                    
                  }# fin bucle for
                  
                  if(j > 1){
                    df_out <- Reduce(f = function(x, y){merge(x, y, all = TRUE)}, x = list_recortados)
                    
                  } else {
                    df_out <- list_recortados[[1]]
                  }
                  
                }
                
              }
              
              #04# Guardo en lista
              out_list[[i]] <- df_out
              
              
            }
              
            #B# Uno los data frame de la lista en uno solo
            if( length(out_list) > 1 ){
              df_final <- Reduce(f = function(x, y){merge(x, y, all = TRUE)}, x = out_list)  
              
            } else {
              df_final <- out_list[[1]]
              
            }
            
            # doy nombre a las columnas
            if(is.null(col_names) == FALSE){
              colnames(df_final) <- c('Date', col_names)
            }
            
            #C# Asigno data frame al slot compact
            obj <- set_hydroMet(obj = obj, compact = df_final)
            
            #D# Devuelvo resultado
            return(obj)
            
          } # fin funcion
          
)

