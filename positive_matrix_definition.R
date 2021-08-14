#################################
##################################Quedé en que la matriz de expresión tiene los duplicados
#y hay que agregarle los ensembl gene ids
#si se eliminan, hay que hacer la positive_matrix
setwd("/home/julian/Desktop/Tesis/discapacidades-intelectuales/ML/2020/")
#================================================================================================
#PREPARACIÓN INICIAL
#Cargar los datos de expresión, de las filas y de las columnas
expression_matrix= read.csv("expression_matrix.csv", dec = ".")
metadata_gene_ids= read.csv("rows_metadata.csv")
metadata_variables= read.csv("columns_metadata.csv")

#En la metadata de los genes, las filas vienen duplicadas y esto hay que corregirlo
noDup_gene_ids= metadata_gene_ids[!duplicated(metadata_gene_ids[,3]),]
#Se obtiene la lista de gene ids
all_gene_ids= noDup_gene_ids[,3]
#Ya que es necesario tener la matriz espaciotemporalmente
#Agregamos los gene IDs
expression_matrix= cbind(all_gene_ids, expression_matrix)
#===============================================================================================
#POSITIVOS
#Búsqueda de la posición en la que se encuentran todos los positivos
positivos= read.csv("ensembl_gene_ids_nuevos_positivos.csv")
positions= array(dim= dim(positivos)[1])

gene= 1

for (positivo in positivos[,3]) {
  position= 1
  found= F
  end= F
  
  while(!found && !end){
    if(positivo == all_gene_ids[position]){
      found= T
    }
    else{
      if(position < length(all_gene_ids)){
        position= position + 1
      }
      else{
        end= T
      }
    }
  }
  
  if(found == T){
    positions[gene]= position
  }
  print(gene)
  gene= gene + 1
}

#Quitamos los NAs
positions_no_NAs= positions[!is.na(positions)]
#Montamos la matriz de positivos
positive_matrix= expression_matrix[positions_no_NAs,]
#Agregamos la etiqueta de positivos
clase_positiva= array(c(1), dim = length(positions_no_NAs))
positive_expression_matrix= cbind(positive_matrix, clase_positiva)
#==============================================================================================
#NEGATIVOS
#Búsqueda de la posición en la que se encuentran todos los negativos
negativos= read.csv("ensembl_gene_ids_negativos.csv")
negative_positions= array(dim= dim(negativos)[1])

negative_gene= 1

for (negativo in negativos[,3]) {
  position= 1
  found= F
  end= F
  
  while(!found && !end){
    if(negativo == all_gene_ids[position]){
      found= T
    }
    else{
      if(position < length(all_gene_ids)){
        position= position + 1
      }
      else{
        end= T
      }
    }
  }
  
  if(found == T){
    negative_positions[negative_gene]= position
  }
  print(negative_gene)
  negative_gene= negative_gene + 1
}

#Montamos la matriz de positivos
negative_matrix= expression_matrix[negative_positions,]
#Agregamos la etiqueta de positivos
clase_negativa= array(c(-1), dim = length(negative_positions))
negative_expression_matrix= cbind(negative_matrix, clase_negativa)
#==============================================================================================
#Creamos la matriz final y la exportamos a un archivo
final_matrix= rbind(positive_expression_matrix, negative_expression_matrix)
write.csv(final_matrix, file = "non_normalized_expression_matrix.csv")
