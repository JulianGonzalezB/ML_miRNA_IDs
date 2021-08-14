#Paquetes necesarios para correr el modelo
library(e1071)
library(caret)
library(DMwR)

#Se crea el archivo
archivo_salida= "prueba"
write('',file=archivo_salida, append = FALSE)
separador= "============================================================"
miniseparador= "_________________________________________________"
ruta= "../../datos/"
nombre_archivo=c("log2_minmax_expression_matrix_transcripts.csv","log2_expression_matrix_transcripts.csv")

random_seeds= sample.int(1000000, 1)
kernels= c("sigmoid")

num_modelo= 0

for(datos in 1:1){
  #Adecuaci?n de la base
  base <- read.csv(paste(ruta, nombre_archivo[datos], sep=''), sep = ',')
  ensemble_gene_id= base[,1]
  base <- base[1:400,-1]
  
  for(seed in 1:length(random_seeds)){
    #Definici?n de la semilla
    set.seed(random_seeds[seed])
    
    #Toma de una muestra de la base original balanceadas por la funci?n en proporci?n
    sample <-  createDataPartition(base$Class, p = 0.70, list = FALSE)
    
    #Definici?n de las bases de entrenamiento y de prueba
    train_base <-  base[sample,]
    test_base <- base[-sample,]
    test_X= test_base[, -dim(test_base)[2]]
    test_Y= as.factor(test_base$Class)
    
    ###Modelo Balanceado internamente
    #Ajuste necesario para balancear internamente las clases en la base de entrenamiento
    train_base$Class <-  as.factor(train_base$Class)
    
    for(kernel in 1:length(kernels)){
      svm_model <-  svm(Class~. , data= train_base, kernel = kernels[kernel],
                        type= "C-classification", probability= TRUE)
      
      #Testeo
      predictions <-  predict(svm_model, test_X)
      confMatrix= confusionMatrix(predictions, test_Y)
      
      #Escritura del resultado
      write(paste("Seed:", random_seeds[seed]),file=archivo_salida, append= TRUE)
      write(paste("Kernel:",kernels[kernel]), file=archivo_salida, append= TRUE)
      write(miniseparador, file=archivo_salida, append= TRUE)
      write("TN FP FN TP", file=archivo_salida, append= TRUE)
      write(confMatrix$table, file=archivo_salida, append= TRUE)
      write(miniseparador, file=archivo_salida, append= TRUE)
      write(paste("Positive:",confMatrix$positive), file=archivo_salida, append= TRUE)
      write(miniseparador, file=archivo_salida, append= TRUE)
      write("Accuracy Kappa AccuracyLower AccuracyUpper AccuracyNull AccuracyPValue McnemarPValue", file=archivo_salida, append= TRUE)
      write(confMatrix$overall, file=archivo_salida, append= TRUE)
      write(miniseparador, file=archivo_salida, append= TRUE)
      write("Sensitivity Specificity Pos_Pred_Value Neg_Pred_Value Precision Recall F1 Prevalence Detection_Rate_Detection Prevalence Balanced_Accuracy", file=archivo_salida, append= TRUE)
      write(confMatrix$byClass, file=archivo_salida, append= TRUE)
      write(separador, file=archivo_salida, append= TRUE)
    }
  }
}
