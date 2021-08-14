#================================================================================================
#PREPARACIÓN INICIAL
#Cargar los datos de expresión, de las filas y de las columnas
expression_matrix= read.csv("expression_matrix.csv", dec = ".")
metadata_gene_ids= read.csv("rows_metadata.csv")
metadata_variables= read.csv("columns_metadata.csv")

#Ya que es necesario tener la matriz espaciotemporalmente
#Agregamos los gene IDs
all_ensembl_gene_ids= metadata_gene_ids[2:dim(metadata_gene_ids)[1],3]
expression_matrix= cbind(all_ensembl_gene_ids, expression_matrix)
#creamos la columna de clase
class_col= array(dim = dim(expression_matrix)[1])
expression_matrix= cbind(expression_matrix, class_col)
#===============================================================================================
#POSITIVOS
positivos= read.csv("ensembl_gene_ids_nuevos_positivos.csv")

gen= 1
for (positivo in positivos[,3]) {
  pos= 1
  for (reference in expression_matrix[,1]) {
    if(positivo == reference){
      expression_matrix[pos, dim(expression_matrix)[2]]= 1
      print(gen)
    }
    pos= pos + 1
  }
  gen= gen + 1
}
#==============================================================================================
#NEGATIVOS
negativos= read.csv("ensembl_gene_ids_negativos.csv")

gen= 1
for (negativo in negativos[,2]) {
  pos= 1
  for (reference in expression_matrix[,1]) {
    if(negativo == reference){
      expression_matrix[pos, dim(expression_matrix)[2]]= -1
      print(gen)
    }
    pos= pos + 1
  }
  gen= gen + 1
}
#==============================================================================================
#Seleccionamos solo los positivos y los negativos
final_matrix= expression_matrix[!is.na(expression_matrix[,dim(expression_matrix)[2]]),]
#==============================================================================================
#Exportamos la matriz final a un archivo
write.csv(final_matrix, file = "non_normalized_expression_matrix_duplicated.csv")
