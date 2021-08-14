setwd("/home/julian/Desktop/Tesis/discapacidades-intelectuales/ML/2020/")
#==========================================================================================
#TRASFORMACIONES
#==========================================================================================
#MinMax
min_max_norm= function(datos){
  #Esto no es lo más eficiente pero no importa
  datos_est= datos
  #Para todas las filas de datos, es decir, ni la primera ni la última
  for (columna in 1:(dim(datos)[2]-1)) {
    if(columna != 1){
      minimo= min(datos[,columna])
      maximo= max(datos[,columna])
      for (fila in 1:dim(datos)[1]) {
        datos_est[fila,columna]= (datos[fila,columna] - minimo)/(maximo - minimo)
      }
    }
  }
  return(datos_est)
}
#=========================================================================================
#LOG2
log2_matrix= function(datos){
  #Esto no es lo más eficiente pero no importa
  datos_est= datos
  #Para todas las filas de datos, es decir, ni la primera ni la última
  for (columna in 1:(dim(datos)[2]-1)) {
    if(columna != 1){
      for (fila in 1:dim(datos)[1]) {
        if(datos_est[fila,columna]== 0){
          datos_est[fila,columna]= 0.00001
        }
        datos_est[fila,columna]= log2(datos[fila,columna])
      }
    }
  }
  
  return(datos_est)
}
#=========================================================================================
#ESTANDARIZACIÓN
estandarizar= function(datos){
  #Esto no es lo más eficiente pero no importa
  datos_est= datos
  #Para todas las filas de datos, es decir, ni la primera ni la última
  for (columna in 1:(dim(datos)[2]-1)){
    if(columna != 1){
      promedio= mean(datos[,columna])
      desviacion= sd(datos[,columna])
      for (fila in 1:dim(datos)[1]) {
        datos_est[fila,columna]= ((promedio-datos[fila,columna])/desviacion)
      }
    }
  }
  
  return(datos_est)
}

#=========================================================================================