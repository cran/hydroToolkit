#' Report \code{NA_real_} values
#' 
#' @description  Creates a data frame with reported dates and number of times-step of missing or not recorded data.
#'
#' @param df data frame with hydro-meteo data. First column is date and the second the numeric vector to be reported. 
#' @param Lang string with output column name language: 'spanish' (default) or 'english'.
#'
#' @return A data frame with three columns: start-date, end-date and number of missing time steps. 
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
report_miss_data <- function(df, Lang = 'spanish'){
  #**********************************
  # Condicionales
  #**********************************
    # data frame
  if(is.data.frame(df) == FALSE){
    return('df class must be data frame')
  }
  
    # Idioma
  if(Lang != 'spanish' & Lang != 'english') {
    return('spanish and english are the only valid languages')
  }
  
  #**********************************
  #**********************************
  
  # Posición de datos faltantes
  NA_pos   <- which( is.na(df[ , 2]) ) 
  if(length(NA_pos) ==  0){
    return('There are no missing values')
  }
    
  # Contiguos
  contiguo <- c(1, diff(NA_pos))
  
  # Comienzo y final de cada intervalo
  comienzo_pos <- which(contiguo > 1)
  
  if(length(contiguo) == 1 ){
    comienzo <- NA_pos[1]
    final    <- NA_pos[1]
    
  } else {
    comienzo <- c(NA_pos[1], NA_pos[comienzo_pos]) 
    final    <- c(NA_pos[comienzo_pos - 1], NA_pos[length(NA_pos)])
  }
  
  # Fecha de inicio y finalización
  fecha_inicio <- c( df[comienzo, 1], NA_character_ )
  fecha_final  <- c( df[final, 1], NA_character_)
  
  # Cantidad de intervalos de tiempo
  delta_t <- final - comienzo + 1
  delta_t <- c(delta_t, sum(delta_t))
  
  # Genero data frame para reporte
  out <- data.frame(fecha_inicio, fecha_final, delta_t)
  
  # Nombre de columnas
  if(Lang == 'spanish'){
    colnames(out) <- c('Inicio', 'Fin', 'Pasos de tiempo')
    
  } else if (Lang == 'english'){
    colnames(out) <- c('First', 'Last', 'Time steps')
  }
  
  return(out)
  
}