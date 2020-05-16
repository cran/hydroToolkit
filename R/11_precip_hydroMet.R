# **********************************************************
# Author       : Ezequiel Toum
# Licence      : GPL V3 
# Institution  : IANIGLA-CONICET 
# e-mail       : etoum@mendoza-conicet.gob.ar
# **********************************************************
#
#' Make homogeneity test or fill gaps in a series
#'
#' @description This method can do both: test homogeneity in precipitation series or fill data gaps using regional analysis. 
#' 
#' @param obj an \code{hydroMet_compact} class object. 
#' @param col_target numeric. The column number of the target series (either to test homogeneity or to fill gaps) in \code{compact} slot.
#' @param fill logical. By default value (\code{FALSE}) you will make an homogeneity test to your target series. 
#' @param method string (default is \code{spearman} - possible values are: \code{spearman}, \code{pearson} or \code{kendall}). When creating the regional (or master series) the method uses a weighted mean. The weighted values are the correlations coefficients.
#' @param min_value numeric. Series with a correlation value less than \code{min_value} are thrown away. 
#'
#' @return If \code{fill = FALSE} the method will return a list with three elements: a data frame with all necessary values to correct your target serie, a plot with \code{p-values} and the correlation matrix. When \code{fill = TRUE} the list will contain: the data frame with the target series gaps filled and the correlation matrix.
#' 
#' @import ggplot2
#' @importFrom stats cor sd t.test weighted.mean
#' @export
#'
#' @examples
#' # Load daily precipitation data-set from BDHI
#' load( paste0(system.file('extdata', package = "hydroToolkit"), '/bdhi_p.rda') )
#' 
#' # Fill gaps in Tupungato station
#' relleno <- precip_hydroMet(obj = bdhi_p, col_target = 5, fill = TRUE)
#'
## Generico
setGeneric(name = 'precip_hydroMet', 
           def = function(obj, col_target = 2, fill = FALSE, method = 'spearman',
                          min_value = 0.2) 
           {
             standardGeneric('precip_hydroMet')
           })


#' @describeIn precip_hydroMet homogeneity test applied to precipitation data stored in \code{compact} class.
## compact
setMethod(f = 'precip_hydroMet', 
          signature = 'hydroMet_compact', 
          definition = function(obj, col_target = 2, fill = FALSE, method = 'spearman',
                                min_value = 0.2) 
          {
            #*********************************************************************
            # Condicionales
            #*********************************************************************
            ## col_target
            # col_target: es numerico?
            if( is.numeric(col_target) == FALSE){
              return('col_taget argument must be of class numeric')
            }
            
            # col_target: es NA_real_?
            if( is.na(col_target) == TRUE ){
              return('col_target cannot be NA. You must provide a valid column number.')
            }
            
            # col_target: es de longitud uno?
            if( length(col_target) != 1 ){
              return('col_target should contain a single numeric value')
            }
            
            # col_target: existe la columna?
            max_col <- ncol(obj@compact)
            if(col_target > max_col){
              return('col_target is greater than the maximum number of columns in the data frame')
            }
            
            # col_target: no debe ser la columna uno
            if( col_target <= 1){
              return('col_target value must be > 1')
            }
            
            
            ## fill
            # fill: es T o F?
            if( fill != TRUE & fill != FALSE){
              return('fill argument must be either TRUE or FALSE')
            }
            
            # fill: es uno solo?
            if( length(fill) != 1){
              return('fill argument must be of length one')
            }
            
            
            ## method
            # method: de longitud uno
            if(length(method) != 1){
              return('method argument must be of length one')
            }
            
            # method: es alguno de los permitidos?
            method_target <- c('spearman', 'kendall', 'pearson')
            method_match  <- match(x = method, table = method_target)
            
            if( is.na( sum(method_match) ) == TRUE ){
              return('Valid method values are: spearman, kendall or pearson')
            }
            
            
            ## min_value
            # min_value: es numerico?
            if( is.numeric(min_value) == FALSE ){
              return('min_value argument must be of class numeric')
            }
            
            # min_value: es unico?
            if( length(min_value) != 1 ){
              return('min_value argument must be of length one')
            }
            
            # min_value: es NA_real_?
            if( is.na(min_value) == TRUE ){
              return('NA is not allowed as min_value argument')
            }
            
            # min_value: no puede ser mayor que 1 ni menor que -1
            if( min_value > 1 | min_value < -1 ){
              return('min_value must be between [-1; 1]')
            }
            
            
            #********************************************************************** 
            # Comienzo con el metodo precip
            #********************************************************************** 
            
            #*********************
            # Binding variables
            #*********************
            Date <- p_value <- NULL
            #*********************
            
            # test de homogeneidad o completar serie?
            if(fill == FALSE){
              # test de homogeneidad
              # ver: http://www.sthda.com/english/wiki/unpaired-two-samples-t-test-in-r
              
              #01# imprimir los supuestos del metodo
              message(
                'This homogeneity test was thought for precipitaction series (monthly or annual)
                 Summary: the target series is divided in two samples, nA and nB (nB = n - nA).
                 Welch t-statistic for unpaired two-samples test is applied.
                 Hypothesis: 
                 *H0: mu_A = mu_B  
                 *H1: mu_A != mu_B 
                 Assumption: the series have normal distribution.'
                )
              
              #02# extraer el df
              df_precip <- get_hydroMet(obj = obj, name = 'compact')[[1]]
              
              #03# que las columnas sean tres o mas
              row_num <- nrow(df_precip)
              col_num <- ncol(df_precip) - 1
              if(col_num < 3){
                return('You should provide 3 or more precipitation series to build a regional series')
              }
              
              #04# calcular pik = Pik / Pk_mu para cada columna
              P_mu  <- rep(NA_real_, col_num)
              m_pik <- matrix(NA_real_, ncol = col_num, nrow = row_num)
              for(i in 1:col_num){
                P_mu[i] <- mean(df_precip[ , i + 1], na.rm = TRUE)
                
                m_pik[ , i] <- df_precip[ , i + 1] / P_mu[i]
              }
              
              #05# obtener la matriz de correlacion
              m_cor <- cor(x = as.matrix(df_precip[ , -1]), use = 'pairwise.complete.obs', method = method)
              
              non_col   <- col_target - 1 # columna (fila) objetivo
              
              # selecciono valores de correlacion menores a min_value
              flag_row <- which(m_cor[ , non_col] < min_value)
              
              #06# calcular la serie maestra pi_master = w_mean(pik, cor)
              pesos     <- m_cor[-c(non_col, flag_row), non_col]
              
              pi_master <- apply(X =  m_pik[ , -c(non_col, flag_row)], MARGIN = 1, FUN = function(x){
                weighted.mean(x = x, w = pesos, na.rm = TRUE)
              })
                
              
              #07# obtengo las razones qi_o = pi_o / pi_master
              pi_o <-  m_pik[ , non_col] # serie objetivo normalizada
              
              qi_o <- ifelse(pi_master != 0, pi_o / pi_master, 0)
              
              #08# armo los intervalos para obtener las series
              #    restriccion: siempre deben quedar de una lado y del otro al menos 5 valores
              p_val <- rep(NA_real_, row_num)
              
              qA_mean <- rep(NA_real_, row_num)
              qB_mean <-  rep(NA_real_, row_num)
                
              for(i in 1:row_num){
                n_A <- i
                n_B <- row_num - i
                if(n_A >= 5 & n_B >= 5){
                  # aplico el test-t (uso el de Welch) y extraigo (para cada n el p-valor)
                  x_A <- qi_o[1:i]
                  x_B <- qi_o[(i+1): row_num]
                  
                  p_val[i] <- t.test(x = x_A, y = x_B, alternative = 'two.sided', var.equal = FALSE)[['p.value']]
                  
                  qA_mean[i] <- mean(x_A, na.rm = TRUE)
                  qB_mean[i] <- mean(x_B, na.rm = TRUE)
                  
                } 
                
              }
              
              #09# tabla de salida con: Date | p_value | qA_mean | qB_mean 
              df_out <- data.frame(Date = df_precip[ , 1], p_value = p_val, 
                                   q1_mean = qA_mean, q2_mean = qB_mean)
              
              #10# grafico con el p-valor en ordenadas y fecha en abscisas
              ggout  <- ggplot(data = df_out, aes(x = Date, y = p_value)) + 
                geom_point(col = 'dodgerblue') + 
                geom_hline(yintercept = 0.05, col = 'red') + 
                xlab('Date') + ylab('p-value')
              
              #11# salida
              list_out <- list(result_table = df_out, plot = ggout, cor_matrix = m_cor)
              
            } else{
              # completar serie objetivo
              
              #01# extraer el df
              df_precip <- get_hydroMet(obj = obj, name = 'compact')[[1]]
              
              #02# que las columnas sean tres o mas
              row_num <- nrow(df_precip)
              col_num <- ncol(df_precip) - 1
              if(col_num < 3){
                return('You should provide 3 or more precipitation series to build a regional series')
              }
              
              #03# calcular pik = Pik / Pk_mu para cada columna
              P_mu  <- rep(NA_real_, col_num)
              m_pik <- matrix(NA_real_, ncol = col_num, nrow = row_num)
              for(i in 1:col_num){
                P_mu[i] <- mean(df_precip[ , i + 1], na.rm = TRUE)
                
                m_pik[ , i] <- df_precip[ , i + 1] / P_mu[i]
              }
              
              #04# obtener la matriz de correlacion
              m_cor <- cor(x = as.matrix(df_precip[ , -1]), use = 'pairwise.complete.obs', method = method)
              
              non_col   <- col_target - 1 # columna (fila) objetivo
              
              # selecciono valores de correlacion menores a min_value
              flag_row <- which(m_cor[ , non_col] < min_value)
              
              #05# calcular la serie maestra pi_master = w_mean(pik, cor)
              pesos     <- m_cor[-c(non_col, flag_row), non_col]
              
              pi_master <- apply(X =  m_pik[ , -c(non_col, flag_row)], MARGIN = 1, FUN = function(x){
                weighted.mean(x = x, w = pesos, na.rm = TRUE)
              })
              
              #06# completo valores faltantes serie objetivo
              P_target <- df_precip[ , col_target]
              
              P_aux <- pi_master * P_mu[non_col] 
              P_out <- ifelse(is.na(P_target) == TRUE, P_aux, P_target)
              
              #07# Salida
              out <- df_precip
              aux_names <- colnames(df_precip)
                
              out[ , col_target] <- P_out
              colnames(out) <- aux_names
              
              list_out <- list(serie_fill = out, cor_matrix = m_cor)
            }
            
            #********************************************************************** 
            
            # Lo que debe regresar el método
            return(list_out)
            
            
          } # fin funcion
)