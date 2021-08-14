base <- read.csv("log2_full_feature_set_unique_ids_and_nidgs.csv", sep = ',')
colnames(base)[1] <- "ensemble_gene_id"
colnames(base)[526] <- "Class"
positivos <- subset(base, Class== 1)
negativos <- subset(base, Class== -1)
ensamble_IDs_Negatives <- negativos[,1]

library("biomaRt")

new_mart <- useMart(biomart = "ENSEMBL_MART_ENSEMBL", dataset = "hsapiens_gene_ensembl")
attrS <- listAttributes(new_mart)
getBM(attributes = c("mim_gene_description","ensembl_gene_id"), filters = "ensembl_gene_id",
      values = ensamble_IDs_Negatives, mart = new_mart)
###Esta es la buena
getBM(attributes = "mim_morbid_description", filters = "ensembl_gene_id",
      values = ensamble_IDs_Negatives, mart = new_mart)

getBM(attributes = "description", filters = "ensembl_gene_id",
      values = ensamble_IDs_Negatives, mart = new_mart)

write.csv(ensamble_IDs_Negatives, file = "ensembl_gene_ids_negativos.csv")
