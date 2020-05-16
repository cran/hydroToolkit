#' Reads data from Explorador Climático de Chile
#' 
#' @description Reads data downloaded from Explorador Climatico de Chile (\href{http://explorador.cr2.cl/}{CR2}) as a data frame.
#'
#' @param file string with the file name (include extension). The only accepted format is '.csv'.
#' @param colName string with the name of the variable. 
#' @param path  string with the files directory. If not provided, the function will use the current working directory. 
#'
#' @return A two column data frame with date and variable. Gaps between dates are filled with \code{NA_real_} and duplicated rows are eliminated automatically.
#'
#' @importFrom utils read.csv
#' 
#' @export
#'
#' @examples
#' # Relative path to raw data
#' full_path <- system.file('extdata', package = "hydroToolkit")
#' 
#' # Apply function
#' yeso_tmed <- read_CR2(file = 'Tmed_Yeso_Embalse.csv', 
#'                 colName = 'T(ºC)', path = full_path)
#' 
#' 
read_CR2 <- function(file, colName, path = NULL){
  #*******************************
  # Condicionales
  #*******************************
  
  # file
  # file: un solo archivo
  if(length(file) > 1){
    return('Please insert a single file as argument')
  }
  
  # file: caracter
  if( is.character(file) == FALSE ){
    return('file argument must be of character type')
  }
  
  # file: existe el archivo (tambien evaluo path)
  if( is.null(path) == TRUE){
    archivos <- list.files(pattern = '.csv')
    existe   <- grep(pattern = paste0('^', file, '$'), x = archivos)
    
    if(length(existe) == 0){
      return('The file does not exist or is misspell')
    }
    
    if(length(existe) > 1){
      return('Multiple files have the same name')
    }
    
  } else{
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
    
    archivos <- list.files(path = path, pattern = '.csv')
    existe   <- grep(pattern = paste0('^', file, '$'), x = archivos)
    
    if(length(existe) == 0){
      return('The file does not exist or is misspell')
    }
    
    if(length(existe) > 1){
      return('Multiple files have the same name')
    }
  }
  
  
  
  # colName: string
    if( is.character(colName) == FALSE){
      return('colName argument must be of character class')
    }
    
  # colName: longitud
    if( length(colName) > 1){
      return('colName argument accepts just one string')
    }
    
  #*******************************
  #*******************************
  
  ## 
  # Leer archivo
  if(is.null(path) == TRUE){
    df_cr2 <- read.csv(file = file, header = TRUE, sep = ',', dec = '.')  
    
  } else{
    df_cr2 <- read.csv(file = paste0(path, '/', file), header = TRUE, sep = ',', dec = '.')  
  }
  
  
  
  # Fechas
  anio <- df_cr2[ , 1]
  mes  <- df_cr2[ , 2]
  dia  <- df_cr2[ , 3]
  
  fechas <- as.Date( paste0(anio, '-', mes, '-', dia) )
  
  # Armar data frame
  df_r <- data.frame(fechas, df_cr2[ , 4])
  
  # Rellenar serie
  df_out <- fill_serie(df = df_r, colName = colName, timeStep = 'day')
  
  # Elimno filas duplicadas
  df_out <- unique.data.frame(x = df_out)
  
  # Salida
  return(df_out)
}