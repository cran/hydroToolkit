#' Reads data from Departamento General de Irrigación (Mendoza - Argentina)
#' 
#' @description Reads the Departamento General de Irrigacion(Mendoza - Argentina) excel sheet. 
#'
#' @param file string with the file name ('xlsx' excel files).
#' @param sheet sheet to read. Either a string (the name of a sheet), or an integer (the position of the sheet). Default value is sheet one.
#' @param colName string with the name of the second column (as default first column is Date). If ignored first row excel names are used. 
#' @param range string providing cell range to read. E.g.: 'A1:B75'.
#' @param path  string with the files directory. If not provided, the function will use the current working directory. 
#'
#' @return A data frame with two columns: date and variable. Gaps between dates are filled with \code{NA_real_} and duplicated rows are eliminated automatically.
#' 
#' @importFrom readxl read_xlsx
#' 
#' @export
#'
#' @examples
#' # Relative path to raw data
#' full_path <- system.file('extdata', package = "hydroToolkit")
#' 
#' # Apply function
#' toscas_hr <- read_DGI(file = 'Toscas.xlsx', sheet = 'hr',
#'                 colName = 'RH(%)', path = full_path)
#' 
read_DGI <- function(file, sheet = NULL, colName = NULL, range = NULL, path = NULL){
  #***********************
  # Condicionales
  #***********************
  
    # file
  # file: un solo archivo
  if(length(file) > 1){
    return('Please insert a single file as argument')
  }
  
  # file: caracter
  if( is.character(file) == FALSE ){
    return('file argument must be of character type')
  }
  
  # file: existe el archivo
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
    
    archivos <- list.files(path = path, pattern = '.xlsx')
    existe   <- grep(pattern = paste0('^', file, '$'), x = archivos)
    
    if(length(existe) == 0){
      return('The file does not exist or is misspell')
    }
    
    if(length(existe) > 1){
      return('Multiple files have the same name')
    }
  }
    
    # sheet 
  if(is.null(sheet) == FALSE){
    # sheet: numeric o string
    if(is.numeric(sheet) == FALSE & is.character(sheet) == FALSE){
      return('sheet argument must be either numeric or string')
    }
    
    # sheet: longitud
    if(length(sheet) > 1){
      return('sheet argument accepts just one string')
    }
    
    
  }
  
  
    # colName
  if( is.null(colName) == FALSE){
    # colName: string
    if( is.character(colName) == FALSE){
      return('colName argument must be of character class')
    }
    
    # colName: longitud
    if( length(colName) > 1){
      return('colName argument accepts just one string')
    }
    
  }
  
  
    # range 
  if( is.null(range) == FALSE){
    # range: string
    if( is.character(range) == FALSE){
      return('range argument must be of character class')
    }
    
    # range: longitud
    if( length(range) > 1){
      return('range argument accepts just one string')
    }
    
  }
 
  #***********************
  #***********************
  
  ##
  # Leer archivo excel con paquete readxl
  if(is.null(path) == TRUE){
    tibble_xlsx <- read_xlsx(path = file, sheet = sheet, range = range)
    
  } else {
    tibble_xlsx <- read_xlsx(path = paste0(path, '/', file), sheet = sheet, range = range)
    
  }
  
  
  
  # Transformar a data frame
  df_xlsx <- as.data.frame(tibble_xlsx)
  
    # asigno nombre a columnas
  if( is.null(colName) == FALSE){
    colnames(df_xlsx) <- c('Date', colName)
  }
  
    # transformo columna 1 a fecha
  df_xlsx[ , 1] <- as.Date(df_xlsx[ , 1])
  
    # verifico que la columna 2 sea numeric sino coerciono
  if( class(df_xlsx[ , 2]) != 'numeric'){
    df_xlsx[ , 2] <- as.numeric( df_xlsx[ , 2] )
  }
  
  # Relleno serie en caso de ser necesario
  if( is.null(colName) == TRUE){
    col_name <- colnames(df_xlsx)[2]
    
  } else{
    col_name <- colName
    
  }
    
  df_out <- fill_serie(df = df_xlsx, colName = col_name, timeStep = 'day')
  
  # Elimino filas duplicadas
  df_out <- unique.data.frame(x = df_out)
  
  # Salida funcion
  return(df_out)
  
}