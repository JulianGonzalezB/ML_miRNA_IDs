library(randomForest)
library(caret)
library(DMwR)

#=================================================================================================
#=================================================================================================
#Funci?n para hacer los bosques indicando 
#la base a utilizar, el porcentaje de los datos para el entrenamiento,
#el n?mero de ?rboles en el bosque, la semilla y si se desea que las clases se balanceen (T o F).
#Retorna el bosque.
hacer_bosque <- function(base, porcentaje, num_arboles, semilla, balanceo)
{
  set.seed(semilla)
  ######caret split para que sea balanceado.
  trainIndex <- createDataPartition(base$Class, p = (porcentaje/100), list = FALSE)
  
  train <- base[trainIndex,]
  train$Class <- as.factor(train$Class)
  test  <- base[-trainIndex,]
  test$Class <- as.factor(test$Class)
  
  #Balanceado con SMOTE
  if(balanceo){
    train= SMOTE(Class~. , train, k=3, perc.over = 190)
    train=as.data.frame(train)
  }
  
  rf= randomForest(train[, -dim(train)[2]], 
                   y= train$Class, xtest= test[, -dim(train)[2]] , 
                   ytest= test$Class, ntree = num_arboles, importance = TRUE, 
                   replace= FALSE, keep.forest = TRUE)
  return(rf)
}

#=================================================================================================
#=================================================================================================
imprimir_conf <- function(confusion_matrix, archivo){
  #encabezado
  encabezado= "  -1  1     class_error"
  write(encabezado, file = archivo, append = TRUE)
  for (fila in 1:2) {
    if(fila == 1){
      linea= "-1"
    }
    else{
      linea= "1 "
    }
    
    for (columna in 1:3) {
      linea= paste(linea, confusion_matrix[fila, columna], sep="|")
    }
    write(linea, file = archivo, append = TRUE)
  }
}
#=================================================================================================
#=================================================================================================
imprimir_importancia <- function(matriz_importancia, archivo){
  #encabezado
  encabezado= "variable             -1                       1                    MDA                    MDG"
  write(encabezado, file = archivo, append = TRUE)
  
  for (fila in 1:dim(matriz_importancia)[1]) {
    
    linea= row.names(bosque_importancia)[fila]
    
    for (columna in 1:dim(matriz_importancia)[2]) {
      
      linea= paste(linea, matriz_importancia[fila, columna], sep = "     ")
    }
    
    write(linea, file = archivo, append = TRUE)
  }
}
#=================================================================================================
#=================================================================================================
#Se crea el archivo
archivo_salida= "RandomForest_resultados.txt"
write('',file=archivo_salida, append = FALSE)
separador= "============================================================"
miniseparador= "_________________________________________________"
ruta= "../../datos/"
nombre_archivo=c("log2_minmax_expression_matrix_transcripts.csv","log2_expression_matrix_transcripts.csv")
num_modelo= 0

random_seeds= sample.int(1000000, 5)

for(datos in 1:2){
  #Adecuacion de la base
  base <- read.csv(paste(ruta, nombre_archivo[datos], sep=''), sep = ',', check.names = FALSE)
  ensemble_gene_id= base[,1]
  base <- base[,-1]
  
  for(seed in 1:length(random_seeds)){
    #Crea el bosque
    bosque= hacer_bosque(base, 70, 1500, random_seeds[seed], TRUE)
    
    write(separador, file=archivo_salida, append= TRUE)
    write(paste("Seed:", random_seeds[seed]),file= archivo_salida, append= TRUE)
   
     #Imprimir el performance
    conf= bosque$test$confusion
    imprimir_conf(conf, archivo_salida)
    
    write(miniseparador, file=archivo_salida, append= TRUE)
    
    #Imprimir los detalles de las variables
    bosque_importancia= as.data.frame(bosque$importance)
    imprimir_importancia(bosque_importancia, archivo_salida)
    
    #Exportar el bosque
    num_modelo= num_modelo + 1
    archivo_modelo= paste("rf_model", num_modelo, sep = '_')
    save(bosque,file = paste(archivo_modelo, "Rdata", sep = '.'))
  }
}