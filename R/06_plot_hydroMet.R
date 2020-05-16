# **********************************************************
# Author       : Ezequiel Toum
# Licence      : GPL V3 
# Institution  : IANIGLA-CONICET 
# e-mail       : etoum@mendoza-conicet.gob.ar
# **********************************************************
#
#' Methods to easily use \code{ggplot2} or \code{plotly} (interactive) 
#' 
#' @description This method allows you to make plots (using simple and expressive arguments) of the variables contained inside an \code{hydroMet_XXX} object. The plot outputs can be static (\code{ggplot2}) or interactive (\code{plotly}).
#'
#' @param obj a valid \code{hydroMet_XXX} object. 
#' @param slot_name string(s) with the name of the slot(s) to use in plotting. 
#' @param col_number numeric (vector) with the column's variable to plot. In case you decide to merge slots you must provide a list in which each element contains the column numbers of the variable to plot.  
#' @param interactive logical. Default value, \code{FALSE}, will return a \code{ggplot2} class object. Otherwise you will get a \code{plotly} one.
#' @param line_type string  with line dash type (\code{ggplot2}) or mode in \code{plotly} case. \code{ggplot2}: \code{'solid'} (default value), \code{'twodash'}, \code{'longdash'}, \code{'dotted'}, \code{'dotdash'}, \code{'dashed'} or \code{'blank'}. \code{plotly}: \code{'lines'} (default value), \code{'lines+markers'} or \code{'markers'}.
#' @param line_color string with a valid \code{color}. See 'colors()' or \href{http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf}{Rcolor document}.
#' @param x_lab string with \code{x axis} label. 
#' @param y_lab string with \code{y axis} label. In case you use \code{double_yaxis} argument you must supply both \code{c('ylab', 'y2lab')}.
#' @param title_lab string with the title of the plot. Default is a plot without title. 
#' @param legend_lab string with plot label(s) name(s). \bold{NOTE}: \code{ggplot2} double_yaxis does not support \code{legend_lab} in this package version, so giving values to this argument will be harmfulness.
#' @param double_yaxis numeric vector with either \code{1} (= main axis - left) or \code{2} (= secondary axis - right) indicating whether the variable should be plotted in either left or right axis. \bold{NOTE}: in this package version \code{ggplot2} supports just one line plot for each 'y' axis.
#' @param list_extra list with the \code{ggplot2} argument to pass. This argument was design to allow the user to modify \code{ggplot2} arguments (you can find nice examples in \href{http://www.sthda.com/english/wiki/ggplot2-title-main-axis-and-legend-titles}{ggplot2 - Essentials}) \bold{NOTE}: in this package version this argument doesn't make sense for \code{plotly} (except for \code{scatter} plot in \code{hydroMet_compact} class).
#' @param from string (or \code{POSIXct} - valid only in 'BDHI' and  'IANIGLA') with the starting \code{Date}. You can use \code{'from'} without \code{'to'}. In this case you will subset your data 'from' till the end.
#' @param to string (or \code{POSIXct} - valid only in 'BDHI' and  'IANIGLA') with the ending \code{Date}. You can use \code{'to'} without \code{'from'}. In this case you will subset your data from the beginning till 'to'.
#' @param scatter numeric vector of length two with the column number to plot as scatter. The first variable (column number) will be the \code{'x'} variable and the second one the \code{'y'} variable. This argument will work just for class \code{hydroMet_compact}.
#'
#' @return A \code{ggplot2} or \code{plotly} objects to analyze your data.
#'
#' @import ggplot2
#' @importFrom plotly plot_ly add_trace layout %>% ggplotly 
#' @importFrom reshape2 melt
#' @importFrom grDevices colors
#' @importFrom lubridate is.POSIXct
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
#' toscas_dgi <- build_hydroMet(obj = toscas_dgi, slot_list = var_nom, path = dgi_path)
#' 
#' # Plot mean air temperature
#' plot_hydroMet(obj = toscas_dgi, col_number = 2, slot_name = 'tmean',
#'  legend_lab = 'Tmean(ºC)' )
#' 
#' # Now let's plot an interactive graph
#' plot_hydroMet(obj = toscas_dgi, col_number = 2, slot_name = 'tmean',
#'  interactive = TRUE, y_lab = 'Tmean(ºC)' ) 
#' 
## Generico
setGeneric(name = 'plot_hydroMet', 
           def = function(obj, slot_name, col_number, interactive = FALSE,
                          line_type = NULL, line_color = 'dodgerblue', 
                          x_lab = 'Date', y_lab = 'y', title_lab = NULL,
                          legend_lab = NULL, double_yaxis = NULL,
                          list_extra = NULL, from = NULL, to = NULL, scatter = NULL) 
           {
             standardGeneric('plot_hydroMet')
           })


#' @describeIn plot_hydroMet plot method for BDHI class
## BDHI
setMethod(f = 'plot_hydroMet', 
          signature = 'hydroMet_BDHI', 
          definition = function(obj, slot_name, col_number, interactive = FALSE,
                                line_type = NULL, line_color = 'dodgerblue', 
                                x_lab = 'Date', y_lab = 'y', title_lab = NULL,
                                legend_lab = NULL, double_yaxis = NULL,
                                list_extra = NULL, from = NULL, to = NULL) 
          {
            ### 
            # Datos de prueba
            # setwd('/home/ezequiel/Documentos/CONICET/08-R_Packages/hydroToolsKit/Datos_prueba/BDHI')
            # list.files()
            # qmd  <- read_BDHI(file = 'Qmd_Atuel_La_Angostura', colName = 'Qmd', timeStep = 'day' )
            # hr   <- read_BDHI(file = 'HR_Grande_Los_Mayines', colName = 'HR', timeStep = 'day/3' )
            # patm <- read_BDHI(file = 'Patm_Laguna_Fea_Laguna_Fea', colName = 'Patm', timeStep = '4h')
            # swe  <- read_BDHI(file = 'EAN_Atuel_Laguna_Atuel', colName = 'swe', timeStep = 'day')
            # 
            # prueba <- create_hydroMet(class_name = 'BDHI')
            # obj    <- set_hydroMet(obj = prueba, Qmd = qmd, hr = hr, patm = patm, swe = swe)
            # 
            # slot_name  <- c('Qmd')
            # col_number <- 2
            # 
            # double_yaxis <- NULL
            # line_type    <- 'lines'
            # line_color   <- 'dodgerblue'
            # title_lab        <- NULL
            # legend_lab   <- 'Qmd(m3/s)' # no tiene sentido en double axis
            # x_lab        <- 'Date'
            # y_lab        <- 'Q(m3/s)'
            # from         <- '1990-01-01'
            # to           <- '1995-12-31'
            # list_extra   <- list(
            #   theme(
            #     axis.title.x = element_text(color="blue", size=14, face="bold"),
            #     axis.title.y = element_text(color="#993333", size=14, face="bold")  )
            # )
            
            ###
            
            # Condicionales
            
            ## slot_name
            n_slot_name <- length(slot_name) # lo genero para comparar con long de otros argumentos
            
            # slot_name: caracter
            if( is.character(slot_name) == FALSE ){
              return('slot_name argument must be of class character')
            }
            
            # slot_name: se corresponden con los slots
            aux <- match(x = slot_name, table = slotNames('hydroMet_BDHI')[1:13])
            if( is.na( sum(aux) ) == TRUE  ){
              return('Unless one of the slot_name arguments is incorrect')
            }
            rm(aux)
            
            
            ## col_number
            col_position <- Reduce(f = c, x = col_number) # posicion de las columnas a plotear
            n_col_number <- length( col_position ) # cantidad
            
            # col_number: numerico o lista
            if(n_slot_name == 1){
              # col_number como vector numerico
              if( is.numeric(col_number) == FALSE ){
                return('col_number argument must be of class numeric')
              }
              
            } else {
              # col_number como lista que contiene numeros
              if( is.list(col_number) == FALSE ){
                return('col_number must be of list class')
              }
              
              # contiene numeros?
              if( is.numeric(Reduce(f = c, x = col_number) ) == FALSE ){
                return('Each list element should contain numeric vectors')
              }
              
            }
            
            # col_number: mayor o igual a uno
            col_position  <- as.integer(col_position) # coerciono a entero para evitar columnas decimales
            
            if(length( which(col_position <= 1) ) >= 1){
              return('col_number arguments to plot must be >= 1')
            }
            
            
            ## interactive
            # interactive: logico
            if( is.logical(interactive) == FALSE){
              return('interactive must be either TRUE or FALSE')
            }
            
            # interactive: uno solo
            if( length(interactive) > 1 ){
              return('interactive accepts a single value')
            }
            
            ## line_type
            n_line_type <- length(line_type)
            
            # line_type: asigno valores / compruebo sus valores
            if(n_line_type == 0) {
              # asigno valores por defecto
              if(interactive == FALSE){
                # ggplot2
                line_type <- rep('solid', n_col_number)
                
              } else{
                # plotly
                line_type <- rep('lines', n_col_number)
                
              }
              
            } else {
              # misma longitud que col_number
              if( n_line_type != n_col_number ){
                return('line_type must have the same length as col_number')
              }
              
              # long_type valido
              if(interactive == FALSE){
                # ggplot2
                valid_line_type <- c('solid', 'twodash', 'longdash', 'dotted', 'dotdash', 'dashed', 'blank')
                correspondencia <- match(x = line_type, table = valid_line_type)
                
                if( is.na( sum(correspondencia) ) == TRUE ){
                  aux_var <- line_type[ which(is.na(correspondencia) ) ]
                  return( paste0(aux_var, ' ', 'is not a valid line_type for ggplot2 graph') )
                }
                
                
              } else {
                # plotly
                valid_line_type <- c('lines', 'lines+markers', 'markers')
                correspondencia <- match(x = line_type, table = valid_line_type)
                
                if( is.na( sum(correspondencia) ) == TRUE ){
                  aux_var <- line_type[ which(is.na(correspondencia) ) ]
                  return( paste0(aux_var, ' ', 'is not a valid line_type for plotly graph') )
                }
                
              }
              
              
            }
            
            
            ## line_color
            n_line_color <- length(line_color)
            
            # line_color: misma longitud que col_number
            if( n_line_color != n_col_number ){
              return('line_color must be of the same length as col_number')
            }
            
            # line_color: caracter
            if( is.character(line_color) == FALSE ){
              return('line_color must be of character class')
            }
            
            # line_color: color valido
            # paleta_match <- match(x = line_color, table = colors())
            # 
            # if( is.na(sum(paleta_match) ) == T ) {
            #   aux_var <- line_color[ which(is.na(paleta_match) ) ]
            #   return( paste0(aux_var, ' ', 'is not a valid line_color. Find valid color names with colors()') )
            #   
            # }
              
            
            ## x_lab
            # x_lab: caracter
            if( is.character(x_lab) == FALSE ){
              return('x_lab must be of class character')
            }
            
            # x_lab: uno solo
            if( length(x_lab) != 1){
              return('x_lab must be of length one')
            }
            
            
            ## y_lab
            # y_lab: caracter
            if( is.character(y_lab) == FALSE ){
              return('y_lab must be of class character')
            }
            
            # y_lab: uno o dos
            if( is.null(double_yaxis) == TRUE){
              # eje simple
              if( length(y_lab) != 1){
                return('y_lab must be of length one')
              } 
                
            } else {
              # eje doble
              if( length(y_lab) != 2){
                return('y_lab must be of length two')
              }
              
            }
              
            
            ## title_lab
            # title_lab: caracter unico
            if( is.null(title_lab) == FALSE){
              # caracter
              if( is.character(title_lab) == FALSE ){
                return('title_lab argument must be of character class')
              }
              
              # unico
              if( length(title_lab) != 1 ){
                return('title_lab length must be one')
              }
              
            }
            
            
            ## legend_lab 
            if( is.null(legend_lab) == FALSE ){
              n_legend_lab <- length(legend_lab)
              
              # legend_lab: caracter
              if( is.character(legend_lab) == FALSE ){
                return('legend_lab must be of class character')
              }
              
              # legend_lab: cantidad
              if( n_col_number != n_legend_lab){
                return('You must provide as many legend_lab strings as line plots')
              }
              
              
            }
            
            
            ## double_yaxis
            if( is.null(double_yaxis) == FALSE){
              n_double_yaxis <- length(double_yaxis)
              
              # double_yaxis: numerico
              if( is.numeric(double_yaxis) == FALSE){
                return('double_axis argument must be of numeric class')
              }
              
              # double_yaxis: cantidad
              if( interactive == FALSE){
                # ggplot2
                if( n_double_yaxis != 2 ){
                  return('In interactive = FALSE double_yaxis arguments only allows a numeric vector of length two')
                }
                
              } else {
                # plotly
                if(n_double_yaxis != n_col_number){
                  return('double_yaxis numeric vector argument must be of the same length as col_number')
                }
                
              }
              
              # double_yaxis: 1 y 2
              target_nums <- c(1, 2)
              match_nums  <- match(x = double_yaxis, table = target_nums)
              
              if( is.na( sum(match_nums) ) == TRUE ){
                return('Only 1 and 2 are allow as arguments in double_yaxis')
              }
              
              
              
            }
            
            ## list_extra
            if( is.null(list_extra) == FALSE ){
              if( interactive == FALSE){
                # ggplot2
                
                # list_extra: lista
                if( is.list(list_extra) == FALSE){
                  return('list_extra argument must be of list class')
                }
                
                # list_extra: longitud
                # if(length(list_extra) != 1){
                #   return('list_extra should contain a single element list')
                # }
              
                
              } else {
                  # plotly
                print('list_extra argument does not make sense if interactive = TRUE')
                
                }
              
            }# fin list_extra
            
            ## from
            if( is.null(from) == FALSE){
              # from: caracter
              if( is.character(from) == FALSE  & is.POSIXct(from) == FALSE){
                return('from must be of class character or POSIXct')
              }
              
              # from: uno solo
              if( length(from) != 1){
                return('from must be of length one')
              }
              
            }
            
            ## to
            if( is.null(to) == FALSE){
              # to: caracter
              if( is.character(to) == FALSE & is.POSIXct(from) == FALSE){
                return('to must be of class character or POSIXct')
              }
              
              # to: uno solo
              if( length(to) != 1){
                return('to must be of length one')
              }
              
            }
            
            
            # fin condicionales
            ### 
            
            #********************
            # Binding variables
            #********************
            Date <- value <- NULL
            #********************
            
            
            ## Obtener los slots de interes
            all_slots <- get_hydroMet(obj = obj, name = slot_name)
            
            # condicionar que el numero(s) de columna exista dentro de cada slot
            target_max_col <- sapply(X = all_slots, FUN = ncol)
            
            if(n_slot_name == 1){
              # un slot
              if(max(col_number) > target_max_col){
                return('Unless one of the col_number does not exist in the slot')
                
              }
              
              
            } else {
              # varios slots
              for(i in 1:n_slot_name){
                aux_col_num <- col_number[[i]]
                
                if(max(aux_col_num) > target_max_col[i]){
                  return( paste0('Unless one of the col_number (', slot_name[i], ') does not exist in the slot') )
                }
                
              }# fin for
              
            }
            
            ## Armar el un data frame para graficar (df_plot) de acuerdo a las columnas seleccionadas
            # Verifico resolucion temporal
            N_all_slots <- length(all_slots)
            
            if(N_all_slots > 1){
              
              unidades  <- rep(NA_character_, N_all_slots) # que las unidades temporales sean las mismas
              paso_tpo  <- rep(NA_character_, N_all_slots) # que el paso de tiempo sea el mismo
              for(i in 1:N_all_slots){
                
                unidades[i] <- units( diff.Date( all_slots[[i]][ , 1] ) )
                paso_tpo[i] <- length(unique( diff.Date( all_slots[[i]][ , 1] ) ) )
                  
              }# fin for
              
              if( length( unique(unidades)) != 1 ){
                return('the variables must have the same temporal resolution')
              }
              
              if( unique(paso_tpo) != 1 ){
                return('the variables must have the same temporal resolution')
              }
              
              
            } # fin if
            
            # Extraigo las variables de interes de cada slot
            if(N_all_slots > 1){
              # en este caso col_number es una lista
              df_plot <- all_slots[[1]][ , c(1, col_number[[1]] )]
              
              for(i in 2:N_all_slots){
                df_aux <- all_slots[[i]][ , c(1, col_number[[i]] )]
                
                df_plot <- merge(df_plot, df_aux, all = TRUE)
              }
              
            } else { 
              # solo un slot 
              
              df_plot <- all_slots[[1]][ , c(1, col_number)]
              
            }
            
            # En caso de ser necesario aplico subset al data frame 
            if( is.null(from) == FALSE & is.null(to) == FALSE){
              df_plot <- subset(df_plot, subset = Date >= from & Date <= to)
            
            } else if( is.null(from) == FALSE ) {
              df_plot <- subset(df_plot, subset = Date >= from)
              
            } else if( is.null(to) == FALSE) {
              df_plot <- subset(df_plot, subset = Date <= to)
              
            }
            
            ### 
            
            ## ggplot2 o plotly? => esto define la sintaxis a usar
            if( interactive == FALSE ){
              ## ggplot2
             
              # Doble eje y?
              if( is.null(double_yaxis) == TRUE){
                # un solo eje y
                
                # Armo df_plot2 con las columnas
                N_plot <- nrow(df_plot)
                N_var  <- ncol(df_plot) - 1
                
                if( is.null(legend_lab) == FALSE ){
                  tipo_linea  <- list()
                  color_linea <- list()
                  leyen_linea <- list()
                  for(i in 1:N_var){
                    tipo_linea[[i]]  <- rep(line_type[i], N_plot)
                    color_linea[[i]] <- rep(line_color[i], N_plot)
                    leyen_linea[[i]] <- rep(legend_lab[i], N_plot)
                    
                  }
                  
                  linea <- c(sapply(X = tipo_linea, '['))
                  color <-  c(sapply(X = color_linea, '['))
                  leyen <- c(sapply(X = leyen_linea, '['))
                  
                  df_plot2 <- melt(data = df_plot, id.vars = 'Date')
                  df_plot2 <- cbind(df_plot2, linea, color, leyen)
                  
                } else {
                  tipo_linea  <- list()
                  color_linea <- list()
                  
                  for(i in 1:N_var){
                    tipo_linea[[i]]  <- rep(line_type[i], N_plot)
                    color_linea[[i]] <- rep(line_color[i], N_plot)
                    
                  }
                  
                  linea <- c(sapply(X = tipo_linea, '['))
                  color <- c(sapply(X = color_linea, '['))
                  
                  df_plot2 <- melt(data = df_plot, id.vars = 'Date')
                  leyen    <- df_plot2$variable
                  df_plot2 <- cbind(df_plot2, linea, color, leyen)
                }
                
                # Grafico
                ggout <- 
                  ggplot(data = df_plot2, aes(x = Date, y = value, color = leyen) ) +
                  geom_line(aes(linetype = leyen) ) +
                  scale_color_manual(values = line_color) +
                  scale_linetype_manual(values = line_type) +
                  theme(legend.title = element_blank()) +
                  ggtitle(label = title_lab) +
                  xlab(x_lab) + ylab(y_lab)
                
                # agrego list_extra en caso de ser necesario
                if( is.null(list_extra) == FALSE){
                  ggout <- ggout +
                    Reduce(f = c, x = list_extra)
                }
                
                # Salida
                return(ggout)
               
               
              } else {
                # doble eje
                # Fuente: http://rstudio-pubs-static.s3.amazonaws.com/329613_f53e84d1a18840d5a1df55efb90739d9.html
                
                # obtengo posicion de las series en eje principal
                main_pos <- which(double_yaxis == 1) + 1 # vector con numero de columna
                seco_pos <- which(double_yaxis == 2) + 1 # idem
                
                # especifico nombre de las variables para escalar
                y1 <- colnames(df_plot)[main_pos[1]] # principal 
                y2 <- colnames(df_plot)[seco_pos[1]] # secundario
                
                # extraigo nombres para plotear
                y1_plot <- colnames(df_plot)[main_pos]
                y2_plot <- colnames(df_plot)[seco_pos]
                
                # genero matriz
                m_plot <- as.matrix(x = df_plot[ , -1])
                
                # reescalo el eje y secundario:
                #   - substraigo su valor minimo (para que empiece en cero)
                #   - escalo para que tenga el mismo rango del eje y principal
                #   - sumo el minimo valor de y1 
                
                a            <- range(df_plot[[y1]], na.rm = TRUE)
                b            <- range(df_plot[[y2]], na.rm = TRUE)
                scale_factor <- diff(a)/diff(b)
                
                m_plot[ , (seco_pos - 1)] <- ( (m_plot[ , (seco_pos - 1)] - b[1]) * scale_factor) + a[1]
                
                # formula para transformar el eje y secundario
                trans <- ~ ((. - a[1]) / scale_factor) + b[1]
                
                # genero un df_plot2 con los valores reescalados
                df_plot2 <- data.frame(df_plot[ , 1], m_plot)
                
                colnames(df_plot2) <- colnames(df_plot)
                
                # grafico
                ggout <- 
                  ggplot(df_plot2) +
                  geom_line(aes_string('Date', y1_plot), col = line_color[ (main_pos - 1) ], lty = line_type[ (main_pos - 1) ] ) + 
                  geom_line(aes_string('Date', y2_plot), col = line_color[ (seco_pos - 1) ], lty = line_type[ (seco_pos - 1) ] ) +
                  ggtitle(label = title_lab) +
                  xlab(x_lab) + ylab(y_lab[1]) +
                  scale_y_continuous(sec.axis = sec_axis(trans = trans, name = y_lab[2]))
                
                # agrego list_extra en caso de ser necesario
                if( is.null(list_extra) == FALSE){
                  ggout <- ggout +
                    Reduce(f = c, x = list_extra)
                }
                
                # Salida
                return(ggout)
                
             }
             
            } else {
              ## plotly
              
              # Doble eje y?
              if( is.null(double_yaxis) == TRUE ){
                # y simple
                
                # Armo sentencia basica
                ppout   <- plot_ly(df_plot, x = ~Date)
                N_plots <- ncol(df_plot) - 1
                
                # genero graficos sin etiquetas en los ejes
                for(i in 1:N_plots){
                  ppout <- ppout %>% 
                    add_trace(y = df_plot[ , (i + 1)], name = legend_lab[i], type = 'scatter', mode = line_type[i], color = I(line_color[i]) )
                  
                }# fin for
                
                # agrego etiquetas
                ppout <- 
                   ppout %>% 
                     layout(title = title_lab,
                            xaxis = list(title = x_lab),
                            yaxis = list(title = y_lab) )
              
                # Salida 
                return(ppout)
                
              } else {
                # y doble
                
                # Armo sentencia basica
                ppout   <- plot_ly(df_plot, x = ~Date)
                N_plots <- ncol(df_plot) - 1
                
                # genero graficos sin etiquetas en los ejes
                for(i in 1:N_plots){
                  
                  if(double_yaxis[i] == 1){
                    # a eje principal
                    ppout <- ppout %>% 
                      add_trace(y = df_plot[ , (i + 1)], name = legend_lab[i], type = 'scatter', mode = line_type[i], color = I(line_color[i]) )
                    
                  } else if (double_yaxis[i] == 2){
                    # a eje secundario
                    ppout <- ppout %>% 
                      add_trace(y = df_plot[ , (i + 1)], name = legend_lab[i], type = 'scatter', mode = line_type[i], color = I(line_color[i]), yaxis = 'y2')
                    
                  }
                  
                  
                }# fin for
                
                # agrego etiquetas
                ppout <- 
                ppout %>% 
                  layout(title  = title_lab,
                         xaxis  = list(title = x_lab),
                         yaxis  = list(title = y_lab[1]),
                         yaxis2 = list(title = y_lab[2],
                                       overlaying = 'y',
                                       side = 'right')  )
                
                # Salida 
                return(ppout)
                
              } 
              
              
            } # fin plotly
            
            
          } # fin funcion
)


#' @describeIn plot_hydroMet plot method for CR2 class
## CR2
setMethod(f = 'plot_hydroMet', 
          signature = 'hydroMet_CR2', 
          definition = function(obj, slot_name, col_number, interactive = FALSE,
                                line_type = NULL, line_color = 'dodgerblue', 
                                x_lab = 'Date', y_lab = 'y', title_lab = NULL,
                                legend_lab = NULL, double_yaxis = NULL,
                                list_extra = NULL, from = NULL, to = NULL) 
          {
            
            # Condicionales
            
            ## slot_name
            n_slot_name <- length(slot_name) # lo genero para comparar con long de otros argumentos
            
            # slot_name: caracter
            if( is.character(slot_name) == FALSE ){
              return('slot_name argument must be of class character')
            }
            
            # slot_name: se corresponden con los slots
            aux <- match(x = slot_name, table = slotNames('hydroMet_CR2')[1:4])
            if( is.na( sum(aux) ) == TRUE  ){
              return('Unless one of the slot_name arguments is incorrect')
            }
            rm(aux)
            
            
            ## col_number
            col_position <- Reduce(f = c, x = col_number) # posicion de las columnas a plotear
            n_col_number <- length( col_position ) # cantidad
            
            # col_number: numerico o lista
            if(n_slot_name == 1){
              # col_number como vector numerico
              if( is.numeric(col_number) == FALSE ){
                return('col_number argument must be of class numeric')
              }
              
            } else {
              # col_number como lista que contiene numeros
              if( is.list(col_number) == FALSE ){
                return('col_number must be of list class')
              }
              
              # contiene numeros?
              if( is.numeric(Reduce(f = c, x = col_number) ) == FALSE ){
                return('Each list element should contain numeric vectors')
              }
              
            }
            
            # col_number: mayor o igual a uno
            col_position  <- as.integer(col_position) # coerciono a entero para evitar columnas decimales
            
            if(length( which(col_position <= 1) ) >= 1){
              return('col_number arguments to plot must be >= 1')
            }
            
            
            ## interactive
            # interactive: logico
            if( is.logical(interactive) == FALSE){
              return('interactive must be either TRUE or FALSE')
            }
            
            # interactive: uno solo
            if( length(interactive) > 1 ){
              return('interactive accepts a single value')
            }
            
            ## line_type
            n_line_type <- length(line_type)
            
            # line_type: asigno valores / compruebo sus valores
            if(n_line_type == 0) {
              # asigno valores por defecto
              if(interactive == FALSE){
                # ggplot2
                line_type <- rep('solid', n_col_number)
                
              } else{
                # plotly
                line_type <- rep('lines', n_col_number)
                
              }
              
            } else {
              # misma longitud que col_number
              if( n_line_type != n_col_number ){
                return('line_type must have the same length as col_number')
              }
              
              # long_type valido
              if(interactive == FALSE){
                # ggplot2
                valid_line_type <- c('solid', 'twodash', 'longdash', 'dotted', 'dotdash', 'dashed', 'blank')
                correspondencia <- match(x = line_type, table = valid_line_type)
                
                if( is.na( sum(correspondencia) ) == TRUE ){
                  aux_var <- line_type[ which(is.na(correspondencia) ) ]
                  return( paste0(aux_var, ' ', 'is not a valid line_type for ggplot2 graph') )
                }
                
                
              } else {
                # plotly
                valid_line_type <- c('lines', 'lines+markers', 'markers')
                correspondencia <- match(x = line_type, table = valid_line_type)
                
                if( is.na( sum(correspondencia) ) == TRUE ){
                  aux_var <- line_type[ which(is.na(correspondencia) ) ]
                  return( paste0(aux_var, ' ', 'is not a valid line_type for plotly graph') )
                }
                
              }
              
              
            }
            
            
            ## line_color
            n_line_color <- length(line_color)
            
            # line_color: misma longitud que col_number
            if( n_line_color != n_col_number ){
              return('line_color must be of the same length as col_number')
            }
            
            # line_color: caracter
            if( is.character(line_color) == FALSE ){
              return('line_color must be of character class')
            }
            
            # line_color: color valido
            # paleta_match <- match(x = line_color, table = colors())
            # 
            # if( is.na(sum(paleta_match) ) == T ) {
            #   aux_var <- line_color[ which(is.na(paleta_match) ) ]
            #   return( paste0(aux_var, ' ', 'is not a valid line_color. Find valid color names with colors()') )
            #   
            # }
            
            
            ## x_lab
            # x_lab: caracter
            if( is.character(x_lab) == FALSE ){
              return('x_lab must be of class character')
            }
            
            # x_lab: uno solo
            if( length(x_lab) != 1){
              return('x_lab must be of length one')
            }
            
            
            ## y_lab
            # y_lab: caracter
            if( is.character(y_lab) == FALSE ){
              return('y_lab must be of class character')
            }
            
            # y_lab: uno o dos
            if( is.null(double_yaxis) == TRUE){
              # eje simple
              if( length(y_lab) != 1){
                return('y_lab must be of length one')
              } 
              
            } else {
              # eje doble
              if( length(y_lab) != 2){
                return('y_lab must be of length two')
              }
              
            }
            
            
            ## title_lab
            # title_lab: caracter unico
            if( is.null(title_lab) == FALSE){
              # caracter
              if( is.character(title_lab) == FALSE ){
                return('title_lab argument must be of character class')
              }
              
              # unico
              if( length(title_lab) != 1 ){
                return('title_lab length must be one')
              }
              
            }
            
            
            ## legend_lab 
            if( is.null(legend_lab) == FALSE ){
              n_legend_lab <- length(legend_lab)
              
              # legend_lab: caracter
              if( is.character(legend_lab) == FALSE ){
                return('legend_lab must be of class character')
              }
              
              # legend_lab: cantidad
              if( n_col_number != n_legend_lab){
                return('You must provide as many legend_lab strings as line plots')
              }
              
              
            }
            
            
            ## double_yaxis
            if( is.null(double_yaxis) == FALSE){
              n_double_yaxis <- length(double_yaxis)
              
              # double_yaxis: numerico
              if( is.numeric(double_yaxis) == FALSE){
                return('double_axis argument must be of numeric class')
              }
              
              # double_yaxis: cantidad
              if( interactive == FALSE){
                # ggplot2
                if( n_double_yaxis != 2 ){
                  return('In interactive = FALSE double_yaxis arguments only allows a numeric vector of length two')
                }
                
              } else {
                # plotly
                if(n_double_yaxis != n_col_number){
                  return('double_yaxis numeric vector argument must be of the same length as col_number')
                }
                
              }
              
              # double_yaxis: 1 y 2
              target_nums <- c(1, 2)
              match_nums  <- match(x = double_yaxis, table = target_nums)
              
              if( is.na( sum(match_nums) ) == TRUE ){
                return('Only 1 and 2 are allow as arguments in double_yaxis')
              }
              
              
              
            }
            
            ## list_extra
            if( is.null(list_extra) == FALSE ){
              if( interactive == FALSE){
                # ggplot2
                
                # list_extra: lista
                if( is.list(list_extra) == FALSE){
                  return('list_extra argument must be of list class')
                }
                
                # list_extra: longitud
                # if(length(list_extra) != 1){
                #   return('list_extra should contain a single element list')
                # }
                
                
              } else {
                # plotly
                print('list_extra argument does not make sense if interactive = TRUE')
                
              }
              
            }# fin list_extra
            
            ## from
            if( is.null(from) == FALSE){
              # from: caracter
              if( is.character(from) == FALSE ){
                return('from must be of class character')
              }
              
              # from: uno solo
              if( length(from) != 1){
                return('from must be of length one')
              }
              
            }
            
            ## to
            if( is.null(to) == FALSE){
              # to: caracter
              if( is.character(to) == FALSE ){
                return('to must be of class character')
              }
              
              # to: uno solo
              if( length(to) != 1){
                return('to must be of length one')
              }
              
            }
            
            
            # fin condicionales
            ### 
            
            #********************
            # Binding variables
            #********************
            Date <- value <- NULL
            #********************
            
            ## Obtener los slots de interes
            all_slots <- get_hydroMet(obj = obj, name = slot_name)
            
            # condicionar que el numero(s) de columna exista dentro de cada slot
            target_max_col <- sapply(X = all_slots, FUN = ncol)
            
            if(n_slot_name == 1){
              # un slot
              if(max(col_number) > target_max_col){
                return('Unless one of the col_number does not exist in the slot')
                
              }
              
              
            } else {
              # varios slots
              for(i in 1:n_slot_name){
                aux_col_num <- col_number[[i]]
                
                if(max(aux_col_num) > target_max_col[i]){
                  return( paste0('Unless one of the col_number (', slot_name[i], ') does not exist in the slot') )
                }
                
              }# fin for
              
            }
            
            ## Armar el un data frame para graficar (df_plot) de acuerdo a las columnas seleccionadas
            # Verifico resolucion temporal
            N_all_slots <- length(all_slots)
            
            if(N_all_slots > 1){
              
              unidades  <- rep(NA_character_, N_all_slots) # que las unidades temporales sean las mismas
              paso_tpo  <- rep(NA_character_, N_all_slots) # que el paso de tiempo sea el mismo
              for(i in 1:N_all_slots){
                
                unidades[i] <- units( diff.Date( all_slots[[i]][ , 1] ) )
                paso_tpo[i] <- length(unique( diff.Date( all_slots[[i]][ , 1] ) ) )
                
              }# fin for
              
              if( length( unique(unidades)) != 1 ){
                return('the variables must have the same temporal resolution')
              }
              
              if( unique(paso_tpo) != 1 ){
                return('the variables must have the same temporal resolution')
              }
              
              
            } # fin if
            
            # Extraigo las variables de interes de cada slot
            if(N_all_slots > 1){
              # en este caso col_number es una lista
              df_plot <- all_slots[[1]][ , c(1, col_number[[1]] )]
              
              for(i in 2:N_all_slots){
                df_aux <- all_slots[[i]][ , c(1, col_number[[i]] )]
                
                df_plot <- merge(df_plot, df_aux, all = TRUE)
              }
              
            } else { 
              # solo un slot 
              
              df_plot <- all_slots[[1]][ , c(1, col_number)]
              
            }
            
            # En caso de ser necesario aplico subset al data frame 
            if( is.null(from) == FALSE & is.null(to) == FALSE){
              df_plot <- subset(df_plot, subset = Date >= from & Date <= to)
              
            } else if( is.null(from) == FALSE ) {
              df_plot <- subset(df_plot, subset = Date >= from)
              
            } else if( is.null(to) == FALSE) {
              df_plot <- subset(df_plot, subset = Date <= to)
              
            }
            
            ### 
            
            ## ggplot2 o plotly? => esto define la sintaxis a usar
            if( interactive == FALSE ){
              ## ggplot2
              
              # Doble eje y?
              if( is.null(double_yaxis) == TRUE){
                # un solo eje y
                
                # Armo df_plot2 con las columnas
                N_plot <- nrow(df_plot)
                N_var  <- ncol(df_plot) - 1
                
                if( is.null(legend_lab) == FALSE ){
                  tipo_linea  <- list()
                  color_linea <- list()
                  leyen_linea <- list()
                  for(i in 1:N_var){
                    tipo_linea[[i]]  <- rep(line_type[i], N_plot)
                    color_linea[[i]] <- rep(line_color[i], N_plot)
                    leyen_linea[[i]] <- rep(legend_lab[i], N_plot)
                    
                  }
                  
                  linea <- c(sapply(X = tipo_linea, '['))
                  color <-  c(sapply(X = color_linea, '['))
                  leyen <- c(sapply(X = leyen_linea, '['))
                  
                  df_plot2 <- melt(data = df_plot, id.vars = 'Date')
                  df_plot2 <- cbind(df_plot2, linea, color, leyen)
                  
                } else {
                  tipo_linea  <- list()
                  color_linea <- list()
                  
                  for(i in 1:N_var){
                    tipo_linea[[i]]  <- rep(line_type[i], N_plot)
                    color_linea[[i]] <- rep(line_color[i], N_plot)
                    
                  }
                  
                  linea <- c(sapply(X = tipo_linea, '['))
                  color <- c(sapply(X = color_linea, '['))
                  
                  df_plot2 <- melt(data = df_plot, id.vars = 'Date')
                  leyen    <- df_plot2$variable
                  df_plot2 <- cbind(df_plot2, linea, color, leyen)
                }
                
                # Grafico
                ggout <- 
                  ggplot(data = df_plot2, aes(x = Date, y = value, color = leyen) ) +
                  geom_line(aes(linetype = leyen) ) +
                  scale_color_manual(values = line_color) +
                  scale_linetype_manual(values = line_type) +
                  theme(legend.title = element_blank()) +
                  ggtitle(label = title_lab) +
                  xlab(x_lab) + ylab(y_lab)
                
                # agrego list_extra en caso de ser necesario
                if( is.null(list_extra) == FALSE){
                  ggout <- ggout +
                    Reduce(f = c, x = list_extra)
                }
                
                # Salida
                return(ggout)
                
                
              } else {
                # doble eje
                # Fuente: http://rstudio-pubs-static.s3.amazonaws.com/329613_f53e84d1a18840d5a1df55efb90739d9.html
                
                # obtengo posicion de las series en eje principal
                main_pos <- which(double_yaxis == 1) + 1 # vector con numero de columna
                seco_pos <- which(double_yaxis == 2) + 1 # idem
                
                # especifico nombre de las variables para escalar
                y1 <- colnames(df_plot)[main_pos[1]] # principal 
                y2 <- colnames(df_plot)[seco_pos[1]] # secundario
                
                # extraigo nombres para plotear
                y1_plot <- colnames(df_plot)[main_pos]
                y2_plot <- colnames(df_plot)[seco_pos]
                
                # genero matriz
                m_plot <- as.matrix(x = df_plot[ , -1])
                
                # reescalo el eje y secundario:
                #   - substraigo su valor minimo (para que empiece en cero)
                #   - escalo para que tenga el mismo rango del eje y principal
                #   - sumo el minimo valor de y1 
                
                a            <- range(df_plot[[y1]], na.rm = TRUE)
                b            <- range(df_plot[[y2]], na.rm = TRUE)
                scale_factor <- diff(a)/diff(b)
                
                m_plot[ , (seco_pos - 1)] <- ( (m_plot[ , (seco_pos - 1)] - b[1]) * scale_factor) + a[1]
                
                # formula para transformar el eje y secundario
                trans <- ~ ((. - a[1]) / scale_factor) + b[1]
                
                # genero un df_plot2 con los valores reescalados
                df_plot2 <- data.frame(df_plot[ , 1], m_plot)
                
                colnames(df_plot2) <- colnames(df_plot)
                
                # grafico
                ggout <- 
                  ggplot(df_plot2) +
                  geom_line(aes_string('Date', y1_plot), col = line_color[ (main_pos - 1) ], lty = line_type[ (main_pos - 1) ] ) + 
                  geom_line(aes_string('Date', y2_plot), col = line_color[ (seco_pos - 1) ], lty = line_type[ (seco_pos - 1) ] ) +
                  ggtitle(label = title_lab) +
                  xlab(x_lab) + ylab(y_lab[1]) +
                  scale_y_continuous(sec.axis = sec_axis(trans = trans, name = y_lab[2]))
                
                # agrego list_extra en caso de ser necesario
                if( is.null(list_extra) == FALSE){
                  ggout <- ggout +
                    Reduce(f = c, x = list_extra)
                }
                
                # Salida
                return(ggout)
                
              }
              
            } else {
              ## plotly
              
              # Doble eje y?
              if( is.null(double_yaxis) == TRUE ){
                # y simple
                
                # Armo sentencia basica
                ppout   <- plot_ly(df_plot, x = ~Date)
                N_plots <- ncol(df_plot) - 1
                
                # genero graficos sin etiquetas en los ejes
                for(i in 1:N_plots){
                  ppout <- ppout %>% 
                    add_trace(y = df_plot[ , (i + 1)], name = legend_lab[i], type = 'scatter', mode = line_type[i], color = I(line_color[i]) )
                  
                }# fin for
                
                # agrego etiquetas
                ppout <- 
                  ppout %>% 
                  layout(title = title_lab,
                         xaxis = list(title = x_lab),
                         yaxis = list(title = y_lab) )
                
                # Salida 
                return(ppout)
                
              } else {
                # y doble
                
                # Armo sentencia basica
                ppout   <- plot_ly(df_plot, x = ~Date)
                N_plots <- ncol(df_plot) - 1
                
                # genero graficos sin etiquetas en los ejes
                for(i in 1:N_plots){
                  
                  if(double_yaxis[i] == 1){
                    # a eje principal
                    ppout <- ppout %>% 
                      add_trace(y = df_plot[ , (i + 1)], name = legend_lab[i], type = 'scatter', mode = line_type[i], color = I(line_color[i]) )
                    
                  } else if (double_yaxis[i] == 2){
                    # a eje secundario
                    ppout <- ppout %>% 
                      add_trace(y = df_plot[ , (i + 1)], name = legend_lab[i], type = 'scatter', mode = line_type[i], color = I(line_color[i]), yaxis = 'y2')
                    
                  }
                  
                  
                }# fin for
                
                # agrego etiquetas
                ppout <- 
                  ppout %>% 
                  layout(title  = title_lab,
                         xaxis  = list(title = x_lab),
                         yaxis  = list(title = y_lab[1]),
                         yaxis2 = list(title = y_lab[2],
                                       overlaying = 'y',
                                       side = 'right')  )
                
                # Salida 
                return(ppout)
                
              } 
              
              
            } # fin plotly
            
            
          } # fin funcion
)


#' @describeIn plot_hydroMet plot method for DGI class
## DGI
setMethod(f = 'plot_hydroMet', 
          signature = 'hydroMet_DGI', 
          definition = function(obj, slot_name, col_number, interactive = FALSE,
                                line_type = NULL, line_color = 'dodgerblue', 
                                x_lab = 'Date', y_lab = 'y', title_lab = NULL,
                                legend_lab = NULL, double_yaxis = NULL,
                                list_extra = NULL, from = NULL, to = NULL) 
          {
            
            # Condicionales
            
            ## slot_name
            n_slot_name <- length(slot_name) # lo genero para comparar con long de otros argumentos
            
            # slot_name: caracter
            if( is.character(slot_name) == FALSE ){
              return('slot_name argument must be of class character')
            }
            
            # slot_name: se corresponden con los slots
            aux <- match(x = slot_name, table = slotNames('hydroMet_DGI')[1:7])
            if( is.na( sum(aux) ) == TRUE  ){
              return('Unless one of the slot_name arguments is incorrect')
            }
            rm(aux)
            
            
            ## col_number
            col_position <- Reduce(f = c, x = col_number) # posicion de las columnas a plotear
            n_col_number <- length( col_position ) # cantidad
            
            # col_number: numerico o lista
            if(n_slot_name == 1){
              # col_number como vector numerico
              if( is.numeric(col_number) == FALSE ){
                return('col_number argument must be of class numeric')
              }
              
            } else {
              # col_number como lista que contiene numeros
              if( is.list(col_number) == FALSE ){
                return('col_number must be of list class')
              }
              
              # contiene numeros?
              if( is.numeric(Reduce(f = c, x = col_number) ) == FALSE ){
                return('Each list element should contain numeric vectors')
              }
              
            }
            
            # col_number: mayor o igual a uno
            col_position  <- as.integer(col_position) # coerciono a entero para evitar columnas decimales
            
            if(length( which(col_position <= 1) ) >= 1){
              return('col_number arguments to plot must be >= 1')
            }
            
            
            ## interactive
            # interactive: logico
            if( is.logical(interactive) == FALSE){
              return('interactive must be either TRUE or FALSE')
            }
            
            # interactive: uno solo
            if( length(interactive) > 1 ){
              return('interactive accepts a single value')
            }
            
            ## line_type
            n_line_type <- length(line_type)
            
            # line_type: asigno valores / compruebo sus valores
            if(n_line_type == 0) {
              # asigno valores por defecto
              if(interactive == FALSE){
                # ggplot2
                line_type <- rep('solid', n_col_number)
                
              } else{
                # plotly
                line_type <- rep('lines', n_col_number)
                
              }
              
            } else {
              # misma longitud que col_number
              if( n_line_type != n_col_number ){
                return('line_type must have the same length as col_number')
              }
              
              # long_type valido
              if(interactive == FALSE){
                # ggplot2
                valid_line_type <- c('solid', 'twodash', 'longdash', 'dotted', 'dotdash', 'dashed', 'blank')
                correspondencia <- match(x = line_type, table = valid_line_type)
                
                if( is.na( sum(correspondencia) ) == TRUE ){
                  aux_var <- line_type[ which(is.na(correspondencia) ) ]
                  return( paste0(aux_var, ' ', 'is not a valid line_type for ggplot2 graph') )
                }
                
                
              } else {
                # plotly
                valid_line_type <- c('lines', 'lines+markers', 'markers')
                correspondencia <- match(x = line_type, table = valid_line_type)
                
                if( is.na( sum(correspondencia) ) == TRUE ){
                  aux_var <- line_type[ which(is.na(correspondencia) ) ]
                  return( paste0(aux_var, ' ', 'is not a valid line_type for plotly graph') )
                }
                
              }
              
              
            }
            
            
            ## line_color
            n_line_color <- length(line_color)
            
            # line_color: misma longitud que col_number
            if( n_line_color != n_col_number ){
              return('line_color must be of the same length as col_number')
            }
            
            # line_color: caracter
            if( is.character(line_color) == FALSE ){
              return('line_color must be of character class')
            }
            
            # line_color: color valido
            # paleta_match <- match(x = line_color, table = colors())
            # 
            # if( is.na(sum(paleta_match) ) == T ) {
            #   aux_var <- line_color[ which(is.na(paleta_match) ) ]
            #   return( paste0(aux_var, ' ', 'is not a valid line_color. Find valid color names with colors()') )
            #   
            # }
            
            
            ## x_lab
            # x_lab: caracter
            if( is.character(x_lab) == FALSE ){
              return('x_lab must be of class character')
            }
            
            # x_lab: uno solo
            if( length(x_lab) != 1){
              return('x_lab must be of length one')
            }
            
            
            ## y_lab
            # y_lab: caracter
            if( is.character(y_lab) == FALSE ){
              return('y_lab must be of class character')
            }
            
            # y_lab: uno o dos
            if( is.null(double_yaxis) == TRUE){
              # eje simple
              if( length(y_lab) != 1){
                return('y_lab must be of length one')
              } 
              
            } else {
              # eje doble
              if( length(y_lab) != 2){
                return('y_lab must be of length two')
              }
              
            }
            
            
            ## title_lab
            # title_lab: caracter unico
            if( is.null(title_lab) == FALSE){
              # caracter
              if( is.character(title_lab) == FALSE ){
                return('title_lab argument must be of character class')
              }
              
              # unico
              if( length(title_lab) != 1 ){
                return('title_lab length must be one')
              }
              
            }
            
            
            ## legend_lab 
            if( is.null(legend_lab) == FALSE ){
              n_legend_lab <- length(legend_lab)
              
              # legend_lab: caracter
              if( is.character(legend_lab) == FALSE ){
                return('legend_lab must be of class character')
              }
              
              # legend_lab: cantidad
              if( n_col_number != n_legend_lab){
                return('You must provide as many legend_lab strings as line plots')
              }
              
              
            }
            
            
            ## double_yaxis
            if( is.null(double_yaxis) == FALSE){
              n_double_yaxis <- length(double_yaxis)
              
              # double_yaxis: numerico
              if( is.numeric(double_yaxis) == FALSE){
                return('double_axis argument must be of numeric class')
              }
              
              # double_yaxis: cantidad
              if( interactive == FALSE){
                # ggplot2
                if( n_double_yaxis != 2 ){
                  return('In interactive = FALSE double_yaxis arguments only allows a numeric vector of length two')
                }
                
              } else {
                # plotly
                if(n_double_yaxis != n_col_number){
                  return('double_yaxis numeric vector argument must be of the same length as col_number')
                }
                
              }
              
              # double_yaxis: 1 y 2
              target_nums <- c(1, 2)
              match_nums  <- match(x = double_yaxis, table = target_nums)
              
              if( is.na( sum(match_nums) ) == TRUE ){
                return('Only 1 and 2 are allow as arguments in double_yaxis')
              }
              
              
              
            }
            
            ## list_extra
            if( is.null(list_extra) == FALSE ){
              if( interactive == FALSE){
                # ggplot2
                
                # list_extra: lista
                if( is.list(list_extra) == FALSE){
                  return('list_extra argument must be of list class')
                }
                
                # list_extra: longitud
                # if(length(list_extra) != 1){
                #   return('list_extra should contain a single element list')
                # }
                
                
              } else {
                # plotly
                print('list_extra argument does not make sense if interactive = TRUE')
                
              }
              
            }# fin list_extra
            
            ## from
            if( is.null(from) == FALSE){
              # from: caracter
              if( is.character(from) == FALSE ){
                return('from must be of class character')
              }
              
              # from: uno solo
              if( length(from) != 1){
                return('from must be of length one')
              }
              
            }
            
            ## to
            if( is.null(to) == FALSE){
              # to: caracter
              if( is.character(to) == FALSE ){
                return('to must be of class character')
              }
              
              # to: uno solo
              if( length(to) != 1){
                return('to must be of length one')
              }
              
            }
            
            # fin condicionales
            ### 
            
            #********************
            # Binding variables
            #********************
            Date <- value <- NULL
            #********************
            
            ## Obtener los slots de interes
            all_slots <- get_hydroMet(obj = obj, name = slot_name)
            
            # condicionar que el numero(s) de columna exista dentro de cada slot
            target_max_col <- sapply(X = all_slots, FUN = ncol)
            
            if(n_slot_name == 1){
              # un slot
              if(max(col_number) > target_max_col){
                return('Unless one of the col_number does not exist in the slot')
                
              }
              
              
            } else {
              # varios slots
              for(i in 1:n_slot_name){
                aux_col_num <- col_number[[i]]
                
                if(max(aux_col_num) > target_max_col[i]){
                  return( paste0('Unless one of the col_number (', slot_name[i], ') does not exist in the slot') )
                }
                
              }# fin for
              
            }
            
            ## Armar el un data frame para graficar (df_plot) de acuerdo a las columnas seleccionadas
            # Verifico resolucion temporal
            N_all_slots <- length(all_slots)
            
            if(N_all_slots > 1){
              
              unidades  <- rep(NA_character_, N_all_slots) # que las unidades temporales sean las mismas
              paso_tpo  <- rep(NA_character_, N_all_slots) # que el paso de tiempo sea el mismo
              for(i in 1:N_all_slots){
                
                unidades[i] <- units( diff.Date( all_slots[[i]][ , 1] ) )
                paso_tpo[i] <- length(unique( diff.Date( all_slots[[i]][ , 1] ) ) )
                
              }# fin for
              
              if( length( unique(unidades)) != 1 ){
                return('the variables must have the same temporal resolution')
              }
              
              if( unique(paso_tpo) != 1 ){
                return('the variables must have the same temporal resolution')
              }
              
              
            } # fin if
            
            # Extraigo las variables de interes de cada slot
            if(N_all_slots > 1){
              # en este caso col_number es una lista
              df_plot <- all_slots[[1]][ , c(1, col_number[[1]] )]
              
              for(i in 2:N_all_slots){
                df_aux <- all_slots[[i]][ , c(1, col_number[[i]] )]
                
                df_plot <- merge(df_plot, df_aux, all = TRUE)
              }
              
            } else { 
              # solo un slot 
              
              df_plot <- all_slots[[1]][ , c(1, col_number)]
              
            }
            
            # En caso de ser necesario aplico subset al data frame 
            if( is.null(from) == FALSE & is.null(to) == FALSE){
              df_plot <- subset(df_plot, subset = Date >= from & Date <= to)
              
            } else if( is.null(from) == FALSE ) {
              df_plot <- subset(df_plot, subset = Date >= from)
              
            } else if( is.null(to) == FALSE) {
              df_plot <- subset(df_plot, subset = Date <= to)
              
            }
            
            ### 
            
            ## ggplot2 o plotly? => esto define la sintaxis a usar
            if( interactive == FALSE ){
              ## ggplot2
              
              # Doble eje y?
              if( is.null(double_yaxis) == TRUE){
                # un solo eje y
                
                # Armo df_plot2 con las columnas
                N_plot <- nrow(df_plot)
                N_var  <- ncol(df_plot) - 1
                
                if( is.null(legend_lab) == FALSE ){
                  tipo_linea  <- list()
                  color_linea <- list()
                  leyen_linea <- list()
                  for(i in 1:N_var){
                    tipo_linea[[i]]  <- rep(line_type[i], N_plot)
                    color_linea[[i]] <- rep(line_color[i], N_plot)
                    leyen_linea[[i]] <- rep(legend_lab[i], N_plot)
                    
                  }
                  
                  linea <- c(sapply(X = tipo_linea, '['))
                  color <-  c(sapply(X = color_linea, '['))
                  leyen <- c(sapply(X = leyen_linea, '['))
                  
                  df_plot2 <- melt(data = df_plot, id.vars = 'Date')
                  df_plot2 <- cbind(df_plot2, linea, color, leyen)
                  
                } else {
                  tipo_linea  <- list()
                  color_linea <- list()
                  
                  for(i in 1:N_var){
                    tipo_linea[[i]]  <- rep(line_type[i], N_plot)
                    color_linea[[i]] <- rep(line_color[i], N_plot)
                    
                  }
                  
                  linea <- c(sapply(X = tipo_linea, '['))
                  color <- c(sapply(X = color_linea, '['))
                  
                  df_plot2 <- melt(data = df_plot, id.vars = 'Date')
                  leyen    <- df_plot2$variable
                  df_plot2 <- cbind(df_plot2, linea, color, leyen)
                }
                
                # Grafico
                ggout <- 
                  ggplot(data = df_plot2, aes(x = Date, y = value, color = leyen) ) +
                  geom_line(aes(linetype = leyen) ) +
                  scale_color_manual(values = line_color) +
                  scale_linetype_manual(values = line_type) +
                  theme(legend.title = element_blank()) +
                  ggtitle(label = title_lab) +
                  xlab(x_lab) + ylab(y_lab)
                
                # agrego list_extra en caso de ser necesario
                if( is.null(list_extra) == FALSE){
                  ggout <- ggout +
                    Reduce(f = c, x = list_extra)
                }
                
                # Salida
                return(ggout)
                
                
              } else {
                # doble eje
                # Fuente: http://rstudio-pubs-static.s3.amazonaws.com/329613_f53e84d1a18840d5a1df55efb90739d9.html
                
                # obtengo posicion de las series en eje principal
                main_pos <- which(double_yaxis == 1) + 1 # vector con numero de columna
                seco_pos <- which(double_yaxis == 2) + 1 # idem
                
                # especifico nombre de las variables para escalar
                y1 <- colnames(df_plot)[main_pos[1]] # principal 
                y2 <- colnames(df_plot)[seco_pos[1]] # secundario
                
                # extraigo nombres para plotear
                y1_plot <- colnames(df_plot)[main_pos]
                y2_plot <- colnames(df_plot)[seco_pos]
                
                # genero matriz
                m_plot <- as.matrix(x = df_plot[ , -1])
                
                # reescalo el eje y secundario:
                #   - substraigo su valor minimo (para que empiece en cero)
                #   - escalo para que tenga el mismo rango del eje y principal
                #   - sumo el minimo valor de y1 
                
                a            <- range(df_plot[[y1]], na.rm = TRUE)
                b            <- range(df_plot[[y2]], na.rm = TRUE)
                scale_factor <- diff(a)/diff(b)
                
                m_plot[ , (seco_pos - 1)] <- ( (m_plot[ , (seco_pos - 1)] - b[1]) * scale_factor) + a[1]
                
                # formula para transformar el eje y secundario
                trans <- ~ ((. - a[1]) / scale_factor) + b[1]
                
                # genero un df_plot2 con los valores reescalados
                df_plot2 <- data.frame(df_plot[ , 1], m_plot)
                
                colnames(df_plot2) <- colnames(df_plot)
                
                # grafico
                ggout <- 
                  ggplot(df_plot2) +
                  geom_line(aes_string('Date', y1_plot), col = line_color[ (main_pos - 1) ], lty = line_type[ (main_pos - 1) ] ) + 
                  geom_line(aes_string('Date', y2_plot), col = line_color[ (seco_pos - 1) ], lty = line_type[ (seco_pos - 1) ] ) +
                  ggtitle(label = title_lab) +
                  xlab(x_lab) + ylab(y_lab[1]) +
                  scale_y_continuous(sec.axis = sec_axis(trans = trans, name = y_lab[2]))
                
                # agrego list_extra en caso de ser necesario
                if( is.null(list_extra) == FALSE){
                  ggout <- ggout +
                    Reduce(f = c, x = list_extra)
                }
                
                # Salida
                return(ggout)
                
              }
              
            } else {
              ## plotly
              
              # Doble eje y?
              if( is.null(double_yaxis) == TRUE ){
                # y simple
                
                # Armo sentencia basica
                ppout   <- plot_ly(df_plot, x = ~Date)
                N_plots <- ncol(df_plot) - 1
                
                # genero graficos sin etiquetas en los ejes
                for(i in 1:N_plots){
                  ppout <- ppout %>% 
                    add_trace(y = df_plot[ , (i + 1)], name = legend_lab[i], type = 'scatter', mode = line_type[i], color = I(line_color[i]) )
                  
                }# fin for
                
                # agrego etiquetas
                ppout <- 
                  ppout %>% 
                  layout(title = title_lab,
                         xaxis = list(title = x_lab),
                         yaxis = list(title = y_lab) )
                
                # Salida 
                return(ppout)
                
              } else {
                # y doble
                
                # Armo sentencia basica
                ppout   <- plot_ly(df_plot, x = ~Date)
                N_plots <- ncol(df_plot) - 1
                
                # genero graficos sin etiquetas en los ejes
                for(i in 1:N_plots){
                  
                  if(double_yaxis[i] == 1){
                    # a eje principal
                    ppout <- ppout %>% 
                      add_trace(y = df_plot[ , (i + 1)], name = legend_lab[i], type = 'scatter', mode = line_type[i], color = I(line_color[i]) )
                    
                  } else if (double_yaxis[i] == 2){
                    # a eje secundario
                    ppout <- ppout %>% 
                      add_trace(y = df_plot[ , (i + 1)], name = legend_lab[i], type = 'scatter', mode = line_type[i], color = I(line_color[i]), yaxis = 'y2')
                    
                  }
                  
                  
                }# fin for
                
                # agrego etiquetas
                ppout <- 
                  ppout %>% 
                  layout(title  = title_lab,
                         xaxis  = list(title = x_lab),
                         yaxis  = list(title = y_lab[1]),
                         yaxis2 = list(title = y_lab[2],
                                       overlaying = 'y',
                                       side = 'right')  )
                
                # Salida 
                return(ppout)
                
              } 
              
              
            } # fin plotly
            
            
          } # fin funcion
)


#' @describeIn plot_hydroMet plot method for IANIGLA class
## IANIGLA
setMethod(f = 'plot_hydroMet', 
          signature = 'hydroMet_IANIGLA', 
          definition = function(obj, slot_name, col_number, interactive = FALSE,
                                line_type = NULL, line_color = 'dodgerblue', 
                                x_lab = 'Date', y_lab = 'y', title_lab = NULL,
                                legend_lab = NULL, double_yaxis = NULL,
                                list_extra = NULL, from = NULL, to = NULL) 
          {
            
            # Condicionales
            
            ## slot_name
            n_slot_name <- length(slot_name) # lo genero para comparar con long de otros argumentos
            
            # slot_name: caracter
            if( is.character(slot_name) == FALSE ){
              return('slot_name argument must be of class character')
            }
            
            # slot_name: se corresponden con los slots
            aux <- match(x = slot_name, table = slotNames('hydroMet_IANIGLA')[2:11])
            if( is.na( sum(aux) ) == TRUE  ){
              return('Unless one of the slot_name arguments is incorrect')
            }
            rm(aux)
            
            
            ## col_number
            col_position <- Reduce(f = c, x = col_number) # posicion de las columnas a plotear
            n_col_number <- length( col_position ) # cantidad
            
            # col_number: numerico o lista
            if(n_slot_name == 1){
              # col_number como vector numerico
              if( is.numeric(col_number) == FALSE ){
                return('col_number argument must be of class numeric')
              }
              
            } else {
              # col_number como lista que contiene numeros
              if( is.list(col_number) == FALSE ){
                return('col_number must be of list class')
              }
              
              # contiene numeros?
              if( is.numeric(Reduce(f = c, x = col_number) ) == FALSE ){
                return('Each list element should contain numeric vectors')
              }
              
            }
            
            # col_number: mayor o igual a uno
            col_position  <- as.integer(col_position) # coerciono a entero para evitar columnas decimales
            
            if(length( which(col_position < 1) ) >= 1){
              return('col_number arguments to plot must be > 1')
            }
            
            
            ## interactive
            # interactive: logico
            if( is.logical(interactive) == FALSE){
              return('interactive must be either TRUE or FALSE')
            }
            
            # interactive: uno solo
            if( length(interactive) > 1 ){
              return('interactive accepts a single value')
            }
            
            ## line_type
            n_line_type <- length(line_type)
            
            # line_type: asigno valores / compruebo sus valores
            if(n_line_type == 0) {
              # asigno valores por defecto
              if(interactive == FALSE){
                # ggplot2
                line_type <- rep('solid', n_col_number)
                
              } else{
                # plotly
                line_type <- rep('lines', n_col_number)
                
              }
              
            } else {
              # misma longitud que col_number
              if( n_line_type != n_col_number ){
                return('line_type must have the same length as col_number')
              }
              
              # long_type valido
              if(interactive == FALSE){
                # ggplot2
                valid_line_type <- c('solid', 'twodash', 'longdash', 'dotted', 'dotdash', 'dashed', 'blank')
                correspondencia <- match(x = line_type, table = valid_line_type)
                
                if( is.na( sum(correspondencia) ) == TRUE ){
                  aux_var <- line_type[ which(is.na(correspondencia) ) ]
                  return( paste0(aux_var, ' ', 'is not a valid line_type for ggplot2 graph') )
                }
                
                
              } else {
                # plotly
                valid_line_type <- c('lines', 'lines+markers', 'markers')
                correspondencia <- match(x = line_type, table = valid_line_type)
                
                if( is.na( sum(correspondencia) ) == TRUE ){
                  aux_var <- line_type[ which(is.na(correspondencia) ) ]
                  return( paste0(aux_var, ' ', 'is not a valid line_type for plotly graph') )
                }
                
              }
              
              
            }
            
            
            ## line_color
            n_line_color <- length(line_color)
            
            # line_color: misma longitud que col_number
            if( n_line_color != n_col_number ){
              return('line_color must be of the same length as col_number')
            }
            
            # line_color: caracter
            if( is.character(line_color) == FALSE ){
              return('line_color must be of character class')
            }
            
            # line_color: color valido
            # paleta_match <- match(x = line_color, table = colors())
            # 
            # if( is.na(sum(paleta_match) ) == T ) {
            #   aux_var <- line_color[ which(is.na(paleta_match) ) ]
            #   return( paste0(aux_var, ' ', 'is not a valid line_color. Find valid color names with colors()') )
            #   
            # }
            
            
            ## x_lab
            # x_lab: caracter
            if( is.character(x_lab) == FALSE ){
              return('x_lab must be of class character')
            }
            
            # x_lab: uno solo
            if( length(x_lab) != 1){
              return('x_lab must be of length one')
            }
            
            
            ## y_lab
            # y_lab: caracter
            if( is.character(y_lab) == FALSE ){
              return('y_lab must be of class character')
            }
            
            # y_lab: uno o dos
            if( is.null(double_yaxis) == TRUE){
              # eje simple
              if( length(y_lab) != 1){
                return('y_lab must be of length one')
              } 
              
            } else {
              # eje doble
              if( length(y_lab) != 2){
                return('y_lab must be of length two')
              }
              
            }
            
            
            ## title_lab
            # title_lab: caracter unico
            if( is.null(title_lab) == FALSE){
              # caracter
              if( is.character(title_lab) == FALSE ){
                return('title_lab argument must be of character class')
              }
              
              # unico
              if( length(title_lab) != 1 ){
                return('title_lab length must be one')
              }
              
            }
            
            
            ## legend_lab 
            if( is.null(legend_lab) == FALSE ){
              n_legend_lab <- length(legend_lab)
              
              # legend_lab: caracter
              if( is.character(legend_lab) == FALSE ){
                return('legend_lab must be of class character')
              }
              
              # legend_lab: cantidad
              if( n_col_number != n_legend_lab){
                return('You must provide as many legend_lab strings as line plots')
              }
              
              
            }
            
            
            ## double_yaxis
            if( is.null(double_yaxis) == FALSE){
              n_double_yaxis <- length(double_yaxis)
              
              # double_yaxis: numerico
              if( is.numeric(double_yaxis) == FALSE){
                return('double_axis argument must be of numeric class')
              }
              
              # double_yaxis: cantidad
              if( interactive == FALSE){
                # ggplot2
                if( n_double_yaxis != 2 ){
                  return('In interactive = FALSE double_yaxis arguments only allows a numeric vector of length two')
                }
                
              } else {
                # plotly
                if(n_double_yaxis != n_col_number){
                  return('double_yaxis numeric vector argument must be of the same length as col_number')
                }
                
              }
              
              # double_yaxis: 1 y 2
              target_nums <- c(1, 2)
              match_nums  <- match(x = double_yaxis, table = target_nums)
              
              if( is.na( sum(match_nums) ) == TRUE ){
                return('Only 1 and 2 are allow as arguments in double_yaxis')
              }
              
              
              
            }
            
            ## list_extra
            if( is.null(list_extra) == FALSE ){
              if( interactive == FALSE){
                # ggplot2
                
                # list_extra: lista
                if( is.list(list_extra) == FALSE){
                  return('list_extra argument must be of list class')
                }
                
                # list_extra: longitud
                # if(length(list_extra) != 1){
                #   return('list_extra should contain a single element list')
                # }
                
                
              } else {
                # plotly
                print('list_extra argument does not make sense if interactive = TRUE')
                
              }
              
            }# fin list_extra
            
            ## from
            if( is.null(from) == FALSE){
              # from: caracter
              if( is.character(from) == FALSE  & is.POSIXct(from) == FALSE){
                return('from must be of class character or POSIXct')
              }
              
              # from: uno solo
              if( length(from) != 1){
                return('from must be of length one')
              }
              
            }
            
            ## to
            if( is.null(to) == FALSE){
              # to: caracter
              if( is.character(to) == FALSE & is.POSIXct(from) == FALSE){
                return('to must be of class character or POSIXct')
              }
              
              # to: uno solo
              if( length(to) != 1){
                return('to must be of length one')
              }
              
            }
            
            
            # fin condicionales
            ### 
            
            #********************
            # Binding variables
            #********************
            Date <- value <- NULL
            #********************
            
            ## Obtener los slots de interes
            all_slots <- get_hydroMet(obj = obj, name = slot_name)
            
            # condicionar que el numero(s) de columna exista dentro de cada slot
            target_max_col <- sapply(X = all_slots, FUN = ncol)
            
            if(n_slot_name == 1){
              # un slot
              if(max(col_number) > target_max_col){
                return('Unless one of the col_number does not exist in the slot')
                
              }
              
              
            } else {
              # varios slots
              for(i in 1:n_slot_name){
                aux_col_num <- col_number[[i]]
                
                if(max(aux_col_num) > target_max_col[i]){
                  return( paste0('Unless one of the col_number (', slot_name[i], ') does not exist in the slot') )
                }
                
              }# fin for
              
            }
            
            ## Armar el un data frame para graficar (df_plot) de acuerdo a las columnas seleccionadas
            # Verifico resolucion temporal (no hace falta porque todos estan en la misma resolucion)
            N_all_slots <- length(all_slots)
            
            # if(N_all_slots > 1){
            #   
            #   unidades  <- rep(NA_character_, N_all_slots) # que las unidades temporales sean las mismas
            #   paso_tpo  <- rep(NA_character_, N_all_slots) # que el paso de tiempo sea el mismo
            #   for(i in 1:N_all_slots){
            #     
            #     unidades[i] <- units( diff.Date( all_slots[[i]][ , 1] ) )
            #     paso_tpo[i] <- length(unique( diff.Date( all_slots[[i]][ , 1] ) ) )
            #     
            #   }# fin for
            #   
            #   if( length( unique(unidades)) != 1 ){
            #     return('the variables must have the same temporal resolution')
            #   }
            #   
            #   if( unique(paso_tpo) != 1 ){
            #     return('the variables must have the same temporal resolution')
            #   }
            #   
            #   
            # } # fin if
            
            # Extraigo las variables de interes de cada slot
            date_serie <- get_hydroMet(obj = obj, name = 'date')[[1]] # fechas 
            
            if(N_all_slots > 1){
              # en este caso col_number es una lista
              aux_nom <- colnames(all_slots[[1]])[ c( col_number[[1]] ) ]
              df_plot <- data.frame(date_serie, all_slots[[1]][ , c(col_number[[1]] )] )
              
              colnames(df_plot) <- c('Date', aux_nom)
              
              for(i in 2:N_all_slots){
                aux_nom <- c('Date', aux_nom, colnames(all_slots[[i]])[ c( col_number[[i]] ) ] )
                df_aux  <- data.frame(Date = date_serie, all_slots[[i]][ , c(col_number[[i]] )] )
                
                df_plot <- merge(df_plot, df_aux, all = TRUE)
                
                colnames(df_plot) <- aux_nom
              }
              
            } else { 
              # solo un slot 
              aux_nom <- colnames(all_slots[[1]])[ c(col_number) ]
              df_plot <- data.frame(date_serie, all_slots[[1]][ , c(col_number)] )
              
              colnames(df_plot) <- c('Date', aux_nom)
            }
            
            # En caso de ser necesario aplico subset al data frame 
            if( is.null(from) == FALSE & is.null(to) == FALSE){
              df_plot <- subset(df_plot, subset = Date >= from & Date <= to)
              
            } else if( is.null(from) == FALSE ) {
              df_plot <- subset(df_plot, subset = Date >= from)
              
            } else if( is.null(to) == FALSE) {
              df_plot <- subset(df_plot, subset = Date <= to)
              
            }
            
            ### 
            
            ## ggplot2 o plotly? => esto define la sintaxis a usar
            if( interactive == FALSE ){
              ## ggplot2
              
              # Doble eje y?
              if( is.null(double_yaxis) == TRUE){
                # un solo eje y
                
                # Armo df_plot2 con las columnas
                N_plot <- nrow(df_plot)
                N_var  <- ncol(df_plot) - 1
                
                if( is.null(legend_lab) == FALSE ){
                  tipo_linea  <- list()
                  color_linea <- list()
                  leyen_linea <- list()
                  for(i in 1:N_var){
                    tipo_linea[[i]]  <- rep(line_type[i], N_plot)
                    color_linea[[i]] <- rep(line_color[i], N_plot)
                    leyen_linea[[i]] <- rep(legend_lab[i], N_plot)
                    
                  }
                  
                  linea <- c(sapply(X = tipo_linea, '['))
                  color <-  c(sapply(X = color_linea, '['))
                  leyen <- c(sapply(X = leyen_linea, '['))
                  
                  df_plot2 <- melt(data = df_plot, id.vars = 'Date')
                  df_plot2 <- cbind(df_plot2, linea, color, leyen)
                  
                } else {
                  tipo_linea  <- list()
                  color_linea <- list()
                  
                  for(i in 1:N_var){
                    tipo_linea[[i]]  <- rep(line_type[i], N_plot)
                    color_linea[[i]] <- rep(line_color[i], N_plot)
                    
                  }
                  
                  linea <- c(sapply(X = tipo_linea, '['))
                  color <- c(sapply(X = color_linea, '['))
                  
                  df_plot2 <- melt(data = df_plot, id.vars = 'Date')
                  leyen    <- df_plot2$variable
                  df_plot2 <- cbind(df_plot2, linea, color, leyen)
                }
                
                # Grafico
                ggout <- 
                  ggplot(data = df_plot2, aes(x = Date, y = value, color = leyen) ) +
                  geom_line(aes(linetype = leyen) ) +
                  scale_color_manual(values = line_color) +
                  scale_linetype_manual(values = line_type) +
                  theme(legend.title = element_blank()) +
                  ggtitle(label = title_lab) +
                  xlab(x_lab) + ylab(y_lab)
                
                # agrego list_extra en caso de ser necesario
                if( is.null(list_extra) == FALSE){
                  ggout <- ggout +
                    Reduce(f = c, x = list_extra)
                }
                
                # Salida
                return(ggout)
                
                
              } else {
                # doble eje
                # Fuente: http://rstudio-pubs-static.s3.amazonaws.com/329613_f53e84d1a18840d5a1df55efb90739d9.html
                
                # obtengo posicion de las series en eje principal
                main_pos <- which(double_yaxis == 1) + 1 # vector con numero de columna
                seco_pos <- which(double_yaxis == 2) + 1 # idem
                
                # especifico nombre de las variables para escalar
                y1 <- colnames(df_plot)[main_pos[1]] # principal 
                y2 <- colnames(df_plot)[seco_pos[1]] # secundario
                
                # extraigo nombres para plotear
                y1_plot <- colnames(df_plot)[main_pos]
                y2_plot <- colnames(df_plot)[seco_pos]
                
                # genero matriz
                m_plot <- as.matrix(x = df_plot[ , -1])
                
                # reescalo el eje y secundario:
                #   - substraigo su valor minimo (para que empiece en cero)
                #   - escalo para que tenga el mismo rango del eje y principal
                #   - sumo el minimo valor de y1 
                
                a            <- range(df_plot[[y1]], na.rm = TRUE)
                b            <- range(df_plot[[y2]], na.rm = TRUE)
                scale_factor <- diff(a)/diff(b)
                
                m_plot[ , (seco_pos - 1)] <- ( (m_plot[ , (seco_pos - 1)] - b[1]) * scale_factor) + a[1]
                
                # formula para transformar el eje y secundario
                trans <- ~ ((. - a[1]) / scale_factor) + b[1]
                
                # genero un df_plot2 con los valores reescalados
                df_plot2 <- data.frame(df_plot[ , 1], m_plot)
                
                colnames(df_plot2) <- colnames(df_plot)
                
                # grafico
                ggout <- 
                  ggplot(df_plot2) +
                  geom_line(aes_string('Date', y1_plot), col = line_color[ (main_pos - 1) ], lty = line_type[ (main_pos - 1) ] ) + 
                  geom_line(aes_string('Date', y2_plot), col = line_color[ (seco_pos - 1) ], lty = line_type[ (seco_pos - 1) ] ) +
                  ggtitle(label = title_lab) +
                  xlab(x_lab) + ylab(y_lab[1]) +
                  scale_y_continuous(sec.axis = sec_axis(trans = trans, name = y_lab[2]))
                
                # agrego list_extra en caso de ser necesario
                if( is.null(list_extra) == FALSE){
                  ggout <- ggout +
                    Reduce(f = c, x = list_extra)
                }
                
                # Salida
                return(ggout)
                
              }
              
            } else {
              ## plotly
              
              # Doble eje y?
              if( is.null(double_yaxis) == TRUE ){
                # y simple
                
                # Armo sentencia basica
                ppout   <- plot_ly(df_plot, x = ~Date)
                N_plots <- ncol(df_plot) - 1
                
                # genero graficos sin etiquetas en los ejes
                for(i in 1:N_plots){
                  ppout <- ppout %>% 
                    add_trace(y = df_plot[ , (i + 1)], name = legend_lab[i], type = 'scatter', mode = line_type[i], color = I(line_color[i]) )
                  
                }# fin for
                
                # agrego etiquetas
                ppout <- 
                  ppout %>% 
                  layout(title = title_lab,
                         xaxis = list(title = x_lab),
                         yaxis = list(title = y_lab) )
                
                # Salida 
                return(ppout)
                
              } else {
                # y doble
                
                # Armo sentencia basica
                ppout   <- plot_ly(df_plot, x = ~Date)
                N_plots <- ncol(df_plot) - 1
                
                # genero graficos sin etiquetas en los ejes
                for(i in 1:N_plots){
                  
                  if(double_yaxis[i] == 1){
                    # a eje principal
                    ppout <- ppout %>% 
                      add_trace(y = df_plot[ , (i + 1)], name = legend_lab[i], type = 'scatter', mode = line_type[i], color = I(line_color[i]) )
                    
                  } else if (double_yaxis[i] == 2){
                    # a eje secundario
                    ppout <- ppout %>% 
                      add_trace(y = df_plot[ , (i + 1)], name = legend_lab[i], type = 'scatter', mode = line_type[i], color = I(line_color[i]), yaxis = 'y2')
                    
                  }
                  
                  
                }# fin for
                
                # agrego etiquetas
                ppout <- 
                  ppout %>% 
                  layout(title  = title_lab,
                         xaxis  = list(title = x_lab),
                         yaxis  = list(title = y_lab[1]),
                         yaxis2 = list(title = y_lab[2],
                                       overlaying = 'y',
                                       side = 'right')  )
                
                # Salida 
                return(ppout)
                
              } 
              
              
            } # fin plotly
            
            
          } # fin funcion
)


#' @describeIn plot_hydroMet plot method for \code{compact} class
## compact
setMethod(f = 'plot_hydroMet', 
          signature = 'hydroMet_compact', 
          definition = function(obj, slot_name, col_number, interactive = FALSE,
                                line_type = NULL, line_color = 'dodgerblue', 
                                x_lab = 'x', y_lab = 'y', title_lab = NULL,
                                legend_lab = NULL, double_yaxis = NULL,
                                list_extra = NULL, from = NULL, to = NULL, scatter = NULL) 
          {
            #**********************************************************************
            # Condicionales
            #**********************************************************************
            
            ## slot_name
            n_slot_name <- length(slot_name) # lo genero para comparar con long de otros argumentos
            
            # slot_name: caracter
            if( is.character(slot_name) == FALSE ){
              return('slot_name argument must be of class character')
            }
            
            # slot_name: se corresponden con los slots
            aux <- match(x = slot_name, table = slotNames('hydroMet_compact')[1])
            if( is.na( sum(aux) ) == TRUE  ){
              return('Unless one of the slot_name arguments is incorrect')
            }
            rm(aux)
            
            
            ## col_number
            col_position <- Reduce(f = c, x = col_number) # posicion de las columnas a plotear
            n_col_number <- length( col_position ) # cantidad
            
            # col_number: numerico o lista
            if(n_slot_name == 1){
              # col_number como vector numerico
              if( is.numeric(col_number) == FALSE ){
                return('col_number argument must be of class numeric')
              }
              
            } else {
              # col_number como lista que contiene numeros
              if( is.list(col_number) == FALSE ){
                return('col_number must be of list class')
              }
              
              # contiene numeros?
              if( is.numeric(Reduce(f = c, x = col_number) ) == FALSE ){
                return('Each list element should contain numeric vectors')
              }
              
            }
            
            # col_number: mayor o igual a uno
            col_position  <- as.integer(col_position) # coerciono a entero para evitar columnas decimales
            
            if(length( which(col_position <= 1) ) >= 1){
              return('col_number arguments to plot must be >= 1')
            }
            
            
            ## interactive
            # interactive: logico
            if( is.logical(interactive) == FALSE){
              return('interactive must be either TRUE or FALSE')
            }
            
            # interactive: uno solo
            if( length(interactive) > 1 ){
              return('interactive accepts a single value')
            }
            
            ## line_type
            n_line_type <- length(line_type)
            
            # line_type: asigno valores / compruebo sus valores
            if(n_line_type == 0) {
              # asigno valores por defecto
              if(interactive == FALSE){
                # ggplot2
                line_type <- rep('solid', n_col_number)
                
              } else{
                # plotly
                line_type <- rep('lines', n_col_number)
                
              }
              
            } else {
              # misma longitud que col_number
              if( n_line_type != n_col_number ){
                return('line_type must have the same length as col_number')
              }
              
              # long_type valido
              if(interactive == FALSE){
                # ggplot2
                valid_line_type <- c('solid', 'twodash', 'longdash', 'dotted', 'dotdash', 'dashed', 'blank')
                correspondencia <- match(x = line_type, table = valid_line_type)
                
                if( is.na( sum(correspondencia) ) == TRUE ){
                  aux_var <- line_type[ which(is.na(correspondencia) ) ]
                  return( paste0(aux_var, ' ', 'is not a valid line_type for ggplot2 graph') )
                }
                
                
              } else {
                # plotly
                valid_line_type <- c('lines', 'lines+markers', 'markers')
                correspondencia <- match(x = line_type, table = valid_line_type)
                
                if( is.na( sum(correspondencia) ) == TRUE ){
                  aux_var <- line_type[ which(is.na(correspondencia) ) ]
                  return( paste0(aux_var, ' ', 'is not a valid line_type for plotly graph') )
                }
                
              }
              
              
            }
            
            
            ## line_color
            n_line_color <- length(line_color)
            
            if( is.null(scatter) == TRUE ){
              # line_color: misma longitud que col_number
              if( n_line_color != n_col_number ){
              return('line_color must be of the same length as col_number')
            }
            
              # line_color: caracter
              if( is.character(line_color) == FALSE ){
              return('line_color must be of character class')
            }
            
              # line_color: color valido
            #   paleta_match <- match(x = line_color, table = colors())
            # 
            #   if( is.na(sum(paleta_match) ) == T ) {
            #   aux_var <- line_color[ which(is.na(paleta_match) ) ]
            #   return( paste0(aux_var, ' ', 'is not a valid line_color. Find valid color names with colors()') )
            #   
            # }
            }
            
            ## x_lab
            # x_lab: caracter
            if( is.character(x_lab) == FALSE ){
              return('x_lab must be of class character')
            }
            
            # x_lab: uno solo
            if( length(x_lab) != 1){
              return('x_lab must be of length one')
            }
            
            
            ## y_lab
            # y_lab: caracter
            if( is.character(y_lab) == FALSE ){
              return('y_lab must be of class character')
            }
            
            # y_lab: uno o dos
            if( is.null(double_yaxis) == TRUE){
              # eje simple
              if( length(y_lab) != 1){
                return('y_lab must be of length one')
              } 
              
            } else {
              # eje doble
              if( length(y_lab) != 2){
                return('y_lab must be of length two')
              }
              
            }
            
            
            ## title_lab
            # title_lab: caracter unico
            if( is.null(title_lab) == FALSE){
              # caracter
              if( is.character(title_lab) == FALSE ){
                return('title_lab argument must be of character class')
              }
              
              # unico
              if( length(title_lab) != 1 ){
                return('title_lab length must be one')
              }
              
            }
            
            
            ## legend_lab 
            if( is.null(legend_lab) == FALSE ){
              n_legend_lab <- length(legend_lab)
              
              # legend_lab: caracter
              if( is.character(legend_lab) == FALSE ){
                return('legend_lab must be of class character')
              }
              
              # legend_lab: cantidad
              if( n_col_number != n_legend_lab){
                return('You must provide as many legend_lab strings as line plots')
              }
              
              
            }
            
            
            ## double_yaxis
            if( is.null(double_yaxis) == FALSE){
              n_double_yaxis <- length(double_yaxis)
              
              # double_yaxis: numerico
              if( is.numeric(double_yaxis) == FALSE){
                return('double_axis argument must be of numeric class')
              }
              
              # double_yaxis: cantidad
              if( interactive == FALSE){
                # ggplot2
                if( n_double_yaxis != 2 ){
                  return('In interactive = FALSE double_yaxis arguments only allows a numeric vector of length two')
                }
                
              } else {
                # plotly
                if(n_double_yaxis != n_col_number){
                  return('double_yaxis numeric vector argument must be of the same length as col_number')
                }
                
              }
              
              # double_yaxis: 1 y 2
              target_nums <- c(1, 2)
              match_nums  <- match(x = double_yaxis, table = target_nums)
              
              if( is.na( sum(match_nums) ) == TRUE ){
                return('Only 1 and 2 are allow as arguments in double_yaxis')
              }
              
              
              
            }
            
            ## list_extra
            if( is.null(list_extra) == FALSE ){
              if( interactive == FALSE){
                # ggplot2
                
                # list_extra: lista
                if( is.list(list_extra) == FALSE){
                  return('list_extra argument must be of list class')
                }
                
                }
              
            }# fin list_extra
            
            ## from
            if( is.null(from) == FALSE){
              # from: caracter
              if( is.character(from) == FALSE ){
                return('from must be of class character')
              }
              
              # from: uno solo
              if( length(from) != 1){
                return('from must be of length one')
              }
              
            }
            
            ## to
            if( is.null(to) == FALSE){
              # to: caracter
              if( is.character(to) == FALSE ){
                return('to must be of class character')
              }
              
              # to: uno solo
              if( length(to) != 1){
                return('to must be of length one')
              }
              
            }
            
            
            ## scatter
            if( is.null(scatter) == FALSE ){
              # es numerico?
              if( is.numeric(scatter) == FALSE ){
                return('scatter argument must be of class numeric')
              }
              
              # es de longitud 2?
              if( length(scatter) != 2){
                return('scatter supports just two variables. Please provide a numeric vector of length two.')
              }
              
              # esta dentro de col_num?
              aux_sacatter <- match(x = scatter, table = col_number)
              if( is.na( sum(aux_sacatter) ) == TRUE ){
                return('scatter numbers must be included in col_number argument.')
                
              }
              
            }
            
            # fin condicionales
            #**********************************************************************
            #**********************************************************************
            
            #**************
            # Binding 
            #**************
            Date <- value <- NULL
            #**************
            
            ## COMIENZO CON EL METODO
            
            ## Obtener los slots de interes
            all_slots <- get_hydroMet(obj = obj, name = slot_name)
            
            # condicionar que el numero(s) de columna exista dentro de cada slot
            target_max_col <- sapply(X = all_slots, FUN = ncol)
            
            if(n_slot_name == 1){
              # un slot
              if(max(col_number) > target_max_col){
                return('Unless one of the col_number does not exist in the slot')
                
              }
              
              
            } else {
              # varios slots
              for(i in 1:n_slot_name){
                aux_col_num <- col_number[[i]]
                
                if(max(aux_col_num) > target_max_col[i]){
                  return( paste0('Unless one of the col_number (', slot_name[i], ') does not exist in the slot') )
                }
                
              }# fin for
              
            }
            
            ## Armar el un data frame para graficar (df_plot) de acuerdo a las columnas seleccionadas
            # Verifico resolucion temporal
            N_all_slots <- length(all_slots)
            
            if(N_all_slots > 1){
              
              unidades  <- rep(NA_character_, N_all_slots) # que las unidades temporales sean las mismas
              paso_tpo  <- rep(NA_character_, N_all_slots) # que el paso de tiempo sea el mismo
              for(i in 1:N_all_slots){
                
                unidades[i] <- units( diff.Date( all_slots[[i]][ , 1] ) )
                paso_tpo[i] <- length(unique( diff.Date( all_slots[[i]][ , 1] ) ) )
                
              }# fin for
              
              if( length( unique(unidades)) != 1 ){
                return('the variables must have the same temporal resolution')
              }
              
              if( unique(paso_tpo) != 1 ){
                return('the variables must have the same temporal resolution')
              }
              
              
            } # fin if
            
            # Extraigo las variables de interes de cada slot
            if(N_all_slots > 1){
              # en este caso col_number es una lista
              df_plot <- all_slots[[1]][ , c(1, col_number[[1]] )]
              
              for(i in 2:N_all_slots){
                df_aux <- all_slots[[i]][ , c(1, col_number[[i]] )]
                
                df_plot <- merge(df_plot, df_aux, all = TRUE)
              }
              
            } else { 
              # solo un slot 
              
              df_plot <- all_slots[[1]][ , c(1, col_number)]
              
            }
            
            # En caso de ser necesario aplico subset al data frame 
            if( is.null(from) == FALSE & is.null(to) == FALSE){
              df_plot <- subset(df_plot, subset = Date >= from & Date <= to)
              
            } else if( is.null(from) == FALSE ) {
              df_plot <- subset(df_plot, subset = Date >= from)
              
            } else if( is.null(to) == FALSE) {
              df_plot <- subset(df_plot, subset = Date <= to)
              
            }
            
            ### 
            
            # Series de tiempo o nube de puntos
            if( is.null(scatter) == TRUE){
            # series de tiempo
              
            ## ggplot2 o plotly? => esto define la sintaxis a usar
            if( interactive == FALSE ){
              ## ggplot2
              
              # Doble eje y?
              if( is.null(double_yaxis) == TRUE){
                # un solo eje y
                
                # Armo df_plot2 con las columnas
                N_plot <- nrow(df_plot)
                N_var  <- ncol(df_plot) - 1
                
                if( is.null(legend_lab) == FALSE ){
                  tipo_linea  <- list()
                  color_linea <- list()
                  leyen_linea <- list()
                  for(i in 1:N_var){
                    tipo_linea[[i]]  <- rep(line_type[i], N_plot)
                    color_linea[[i]] <- rep(line_color[i], N_plot)
                    leyen_linea[[i]] <- rep(legend_lab[i], N_plot)
                    
                  }
                  
                  linea <- c(sapply(X = tipo_linea, '['))
                  color <-  c(sapply(X = color_linea, '['))
                  leyen <- c(sapply(X = leyen_linea, '['))
                  
                  df_plot2 <- melt(data = df_plot, id.vars = 'Date')
                  df_plot2 <- cbind(df_plot2, linea, color, leyen)
                  
                } else {
                  tipo_linea  <- list()
                  color_linea <- list()
                  
                  for(i in 1:N_var){
                    tipo_linea[[i]]  <- rep(line_type[i], N_plot)
                    color_linea[[i]] <- rep(line_color[i], N_plot)
                    
                  }
                  
                  linea <- c(sapply(X = tipo_linea, '['))
                  color <- c(sapply(X = color_linea, '['))
                  
                  df_plot2 <- melt(data = df_plot, id.vars = 'Date')
                  leyen    <- df_plot2$variable
                  df_plot2 <- cbind(df_plot2, linea, color, leyen)
                }
                
                # Grafico
                ggout <- 
                  ggplot(data = df_plot2, aes(x = Date, y = value, color = leyen) ) +
                  geom_line(aes(linetype = leyen) ) +
                  scale_color_manual(values = line_color) +
                  scale_linetype_manual(values = line_type) +
                  theme(legend.title = element_blank()) +
                  ggtitle(label = title_lab) +
                  xlab(x_lab) + ylab(y_lab)
                
                # agrego list_extra en caso de ser necesario
                if( is.null(list_extra) == FALSE){
                  ggout <- ggout +
                    Reduce(f = c, x = list_extra)
                }
                
                # Salida
                return(ggout)
                
                
              } else {
                # doble eje
                # Fuente: http://rstudio-pubs-static.s3.amazonaws.com/329613_f53e84d1a18840d5a1df55efb90739d9.html
                
                # obtengo posicion de las series en eje principal
                main_pos <- which(double_yaxis == 1) + 1 # vector con numero de columna
                seco_pos <- which(double_yaxis == 2) + 1 # idem
                
                # especifico nombre de las variables para escalar
                y1 <- colnames(df_plot)[main_pos[1]] # principal 
                y2 <- colnames(df_plot)[seco_pos[1]] # secundario
                
                # extraigo nombres para plotear
                y1_plot <- colnames(df_plot)[main_pos]
                y2_plot <- colnames(df_plot)[seco_pos]
                
                # genero matriz
                m_plot <- as.matrix(x = df_plot[ , -1])
                
                # reescalo el eje y secundario:
                #   - substraigo su valor minimo (para que empiece en cero)
                #   - escalo para que tenga el mismo rango del eje y principal
                #   - sumo el minimo valor de y1 
                
                a            <- range(df_plot[[y1]], na.rm = TRUE)
                b            <- range(df_plot[[y2]], na.rm = TRUE)
                scale_factor <- diff(a)/diff(b)
                
                m_plot[ , (seco_pos - 1)] <- ( (m_plot[ , (seco_pos - 1)] - b[1]) * scale_factor) + a[1]
                
                # formula para transformar el eje y secundario
                trans <- ~ ((. - a[1]) / scale_factor) + b[1]
                
                # genero un df_plot2 con los valores reescalados
                df_plot2 <- data.frame(df_plot[ , 1], m_plot)
                
                colnames(df_plot2) <- colnames(df_plot)
                
                # grafico
                ggout <- 
                  ggplot(df_plot2) +
                  geom_line(aes_string('Date', y1_plot), col = line_color[ (main_pos - 1) ], lty = line_type[ (main_pos - 1) ] ) + 
                  geom_line(aes_string('Date', y2_plot), col = line_color[ (seco_pos - 1) ], lty = line_type[ (seco_pos - 1) ] ) +
                  ggtitle(label = title_lab) +
                  xlab(x_lab) + ylab(y_lab[1]) +
                  scale_y_continuous(sec.axis = sec_axis(trans = trans, name = y_lab[2]))
                
                # agrego list_extra en caso de ser necesario
                if( is.null(list_extra) == FALSE){
                  ggout <- ggout +
                    Reduce(f = c, x = list_extra)
                }
                
                # Salida
                return(ggout)
                
              }
              
            } else {
              ## plotly
              
              # Doble eje y?
              if( is.null(double_yaxis) == TRUE ){
                # y simple
                
                # Armo sentencia basica
                ppout   <- plot_ly(df_plot, x = ~Date)
                N_plots <- ncol(df_plot) - 1
                
                # genero graficos sin etiquetas en los ejes
                for(i in 1:N_plots){
                  ppout <- ppout %>% 
                    add_trace(y = df_plot[ , (i + 1)], name = legend_lab[i], type = 'scatter', mode = line_type[i], color = I(line_color[i]) )
                  
                }# fin for
                
                # agrego etiquetas
                ppout <- 
                  ppout %>% 
                  layout(title = title_lab,
                         xaxis = list(title = x_lab),
                         yaxis = list(title = y_lab) )
                
                # Salida 
                return(ppout)
                
              } else {
                # y doble
                
                # Armo sentencia basica
                ppout   <- plot_ly(df_plot, x = ~Date)
                N_plots <- ncol(df_plot) - 1
                
                # genero graficos sin etiquetas en los ejes
                for(i in 1:N_plots){
                  
                  if(double_yaxis[i] == 1){
                    # a eje principal
                    ppout <- ppout %>% 
                      add_trace(y = df_plot[ , (i + 1)], name = legend_lab[i], type = 'scatter', mode = line_type[i], color = I(line_color[i]) )
                    
                  } else if (double_yaxis[i] == 2){
                    # a eje secundario
                    ppout <- ppout %>% 
                      add_trace(y = df_plot[ , (i + 1)], name = legend_lab[i], type = 'scatter', mode = line_type[i], color = I(line_color[i]), yaxis = 'y2')
                    
                  }
                  
                  
                }# fin for
                
                # agrego etiquetas
                ppout <- 
                  ppout %>% 
                  layout(title  = title_lab,
                         xaxis  = list(title = x_lab),
                         yaxis  = list(title = y_lab[1]),
                         yaxis2 = list(title = y_lab[2],
                                       overlaying = 'y',
                                       side = 'right')  )
                
                # Salida 
                return(ppout)
                
              } 
              
              
            } # fin plotly
            
            } else {
            # Nube de puntos
              ## ggplot2 o plotly? => esto define la sintaxis a usar
              if( interactive == FALSE){
                # ggplot2
                df_col  <- c(1, scatter)
                pos_col <- match(x = scatter, table = df_col)
                
                if( is.null(list_extra) == TRUE){
                  ggout <- 
                    ggplot(data = df_plot, aes(x = df_plot[ , pos_col[1]], y = df_plot[ , pos_col[2]] ) ) +
                    geom_point() +
                    ggtitle(label = title_lab) +
                    xlab(x_lab) + ylab(y_lab)
                  
                } else {
                  ggout <- 
                    ggplot(data = df_plot, aes(x = df_plot[ , pos_col[1]], y = df_plot[ , pos_col[2]] ) ) +
                    Reduce(f = c, x = list_extra) +
                    ggtitle(label = title_lab) +
                    xlab(x_lab) + ylab(y_lab)
                }
                
                
                # Salida
                return(ggout)
                
                
                
                
              } else {
               # plotly
                df_col  <- c(1, scatter)
                pos_col <- match(x = scatter, table = df_col)
                
                if( is.null(list_extra) == TRUE){
                  ggout <- 
                    ggplot(data = df_plot, aes(x = df_plot[ , pos_col[1]], y = df_plot[ , pos_col[2]] ) ) +
                    geom_point() +
                    ggtitle(label = title_lab) +
                    xlab(x_lab) + ylab(y_lab)
                  
                } else {
                  ggout <- 
                    ggplot(data = df_plot, aes(x = df_plot[ , pos_col[1]], y = df_plot[ , pos_col[2]] ) ) +
                    Reduce(f = c, x = list_extra) +
                    ggtitle(label = title_lab) +
                    xlab(x_lab) + ylab(y_lab)
                }
                
                
                # Salida
                plotly_out <- ggplotly( p = ggout )
                
                return(plotly_out)
                
                
              } # fin plotly
              
          } # fin scatter
            
            
          } # fin funcion
)
