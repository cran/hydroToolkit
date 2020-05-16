#' Interpolation 
#'
#' @description This functions applies interpolation to fill in missing (or non-recorded) values. 
#'
#' @param df data frame with two columns: 'Date' or 'POSIXct' class in the first column and a numeric variable in the second one.
#' @param miss_table data frame with three columns: first and last date of interpolation (first and second column respectively). The last and third column, is a numeric with the number of steps to interpolate. See \link{report_miss_data}.
#' @param threshold numeric variable with the maximum number of dates in which to apply the interpolation.
#' @param method string with the interpolation method. In this version only 'linear' method is allowed.
#'
#' @return A data frame with date and the interpolated numeric variable. 
#' 
#' @importFrom stats approx na.omit
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
#' # Get mean daily discharge and report miss data
#' Qmd  <- get_hydroMet(obj = guido, name = 'Qmd')[[1]]
#' miss <- report_miss_data(df = Qmd)
#' 
#' # Now interpolate miss values 
#' Qmd_fill <- interpolate(df = Qmd, miss_table = miss, threshold = 5, method = "linear")
#' 
# Mirar en curso Metodos Matematicos => U1: interpolacion => cuadratica, Lagrange y Newton (hay script de R)
interpolate <- function(df, miss_table, threshold, method = 'linear'){
  #******************************************
  # Condicionales
  #******************************************
  # df: es data frame?
  if( is.data.frame(df) == FALSE ){
    return('df must be of class data frame')
  }
  
  # df: tiene dos columnas
  if(ncol(df) != 2){
    return('df should have two columns')
  }
  
  # df: es Date o POSIXct la primera?
  if( class(df[ , 1])[1] != 'Date' &  class(df[ , 1])[1] != 'POSIXct'){
    return('df[ , 1] must be of class Date or POSIXct')
  }
  
  # df: es numerica la segunda
  if( is.numeric(df[ , 2]) == FALSE ){
    return('df[ , 2] must be of class numeric')
  }
  
  #*****
  # Remuevo la ultima fila de report_miss_data()
  miss_table <- na.omit(miss_table)
  
  # miss_table: es data frame?
  if( is.data.frame(miss_table) == FALSE ){
    return('miss_table must be of class data frame')
  }
  
  # miss_table: col_1 y col_2 Date o POSIXct?
  if( class(miss_table[ , 1])[1] != 'Date' &  class(miss_table[ , 1])[1] != 'POSIXct'){
    return('miss_table[ , 1] must be of class Date or POSIXct')
  }
  
  if( class(miss_table[ , 2])[1] != 'Date' &  class(miss_table[ , 2])[1] != 'POSIXct'){
    return('miss_table[ , 2] must be of class Date or POSIXct')
  }
  
  # miss_table: col_3 numeric
  if( is.numeric(miss_table[ , 3]) == FALSE ){
    return('miss_table[ , 3] must be of class numeric')
  }
  
  #*****
  # threshold: es unico
  if(length(threshold) != 1){
    return('threshold should be of length one')
  }
  
  # threshold: es numérico?
  if( is.numeric(threshold) == FALSE ){
    return('threshold should be of class numeric')
  }
  
  #*****
  # method: solo lineal
  if(method != 'linear'){
    return('In this version only linear interpolation is allowed')
  }
  
  
  
  #******************************************
  #******************************************
  # Comienzo funcion
  # que filas uso?
  filas <- which(miss_table[ , 3] <= threshold)
  
  if( length(filas) == 0){
    return('There are no gaps where to interpolate. Check threshold argument!')
  }
  n_it <- length(filas)
  out  <- df[ , 2]
  for(i in 1:n_it){
    i1 <- which( df[ , 1] == miss_table[filas[i], 1] ) # primera posicion valor a interpolar
    i2 <- which( df[ , 1] == miss_table[filas[i], 2] ) # ultima posicion a interpolar
    
    # salvo el caso de que uno de los extremos de la interpolación sea el primer o ultimo valor de la serie
    if(i1 != 1 & i2 != nrow(df)){
      
      j1 <- i1 - 1 # primer valor a extraer
      j2 <- i2 + 1 # ultimo valor a extraer
      
      var_aux <- df[j1:j2, 2]  
      
      aux <- approx(x = j1:j2, y = var_aux, xout = i1:i2)[[2]]
      
      out[i1:i2] <- aux
      
      rm(i, i1, i2, j1, j2, var_aux, aux)
      
    }
    
    
    
  }# fin bucle
  
  df_out <- data.frame(df[ , 1], out)
  colnames(df_out) <- c('Date', 'out')
  
  return(df_out)
}