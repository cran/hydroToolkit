#' Creates an hydroMet class or subclass.
#' 
#' @description This function is the constructor of \code{hydroMet} class and its subclasses. 
#'
#' @param class_name string with the name of the class. Valid arguments are: \code{hydroMet}, \code{BDHI}, \code{CR2}, \code{DGI}, \code{IANIGLA} or \code{compact}.
#'
#' @return an S4 object of class \code{hydroMet}
#' 
#' @importFrom methods new
#' 
#' @export
#' 
#' @examples
#' # Create class 'hydroMet'
#' met_station <- create_hydroMet(class_name = 'hydroMet')
#' 
#' # Subclass 'BDHI'
#' bdhi_station <- create_hydroMet(class_name = 'BDHI')
#' 
#' # Subclass 'DGI'
#' dgi_station <- create_hydroMet(class_name = 'DGI')
#' 
#' # Subclass 'CR2'
#' cr2_station <- create_hydroMet(class_name = 'CR2')
#' 
#' # Subclass 'IANIGLA'
#' ianigla_station <- create_hydroMet(class_name = 'IANIGLA')
#'
create_hydroMet <- function(class_name = 'hydroMet'){
  ## Condicionales
  # class_name: es caracter
  if( is.character(class_name) == FALSE){
    return('class_name argument must be a string')
  }
  
  # class_name: longitud
  if(length(class_name) > 1) {
    return('class_name must be of length 1')
  }
  
  # class_name: nombre valido
  valid_names <- c('hydroMet', 'BDHI', 'CR2', 'DGI', 'IANIGLA', 'compact')
  if( is.na( match(class_name, valid_names) )  == TRUE){
    return('class_name possible values are: hydroMet - BDHI - CR2 - DGI - IANIGLA - compact')
  }
  
  # Funcion generadora
  if(class_name != 'hydroMet'){
    out <- new(Class = paste0('hydroMet_', class_name) )
    
  } else {
    out <- new(Class = 'hydroMet')
  }
  
  # Salida
  return(out)
  
}