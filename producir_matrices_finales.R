datos= read.csv("non_normalized_expression_matrix_duplicated.csv", sep = ",", check.names = FALSE)
head(datos[,1:7])
normalizados= log2_matrix(datos)
head(normalizados[,1:7])
write.csv(normalizados, file = "log2_expression_matrix_transcripts.csv", row.names = FALSE)

normalizados= read.csv("log2_expression_matrix_transcripts.csv", sep = ",", check.names = FALSE)
min(normalizados[,2])

for (columna in 2:(dim(normalizados)[2]-1)) {
  minimo= min(normalizados[,columna])
  maximo= max(normalizados[,columna])
  for (fila in 1:dim(normalizados)[1]) {
    normalizados[fila,columna]= (normalizados[fila,columna] + abs(minimo))/(maximo + abs(minimo))
  }
}

min(normalizados[,2])
max(normalizados[,2])
normalizados[,2]

mm_datos= min_max_norm(normalizados)
head(mm_datos[,1:7])
min(mm_datos[2])

write.csv(normalizados, file = "log2_minmax_expression_matrix_transcripts.csv", row.names = FALSE)

###Para probar que este proceso es correcto
base= read.csv("../datos/log2_minmax_expression_matrix_transcripts.csv", sep = ",", check.names = FALSE)
