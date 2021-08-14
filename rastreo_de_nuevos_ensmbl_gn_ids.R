geneIDs= read.table("positivosNuevos", header= T)
geneIDs= as.data.frame(geneIDs)

library("biomaRt")
ensembl = useMart("ensembl", dataset="hsapiens_gene_ensembl")
atributos= listAttributes(ensembl)
names=getBM(attributes=c('external_gene_name', 'ensembl_gene_id'),
      mart = ensembl)

namesLen= dim(names)[1]
geneIDLen= dim(geneIDs)[1]
results= array(dim = c(dim(geneIDs)[1],3))
encontrados= 0
buscados= 0

for (id in geneIDs[1:geneIDLen,1]) {
  buscados= buscados + 1
  revisados= 0
  for (i in names[1:namesLen,1]){
    revisados= revisados + 1
    if(id == i){
      encontrados= encontrados + 1
      results[buscados,1]= id
      results[buscados,2]= names[revisados,2]
      results[buscados,3]= revisados
    }
  }
}

toPrint= results[complete.cases(results[,1]),]
colnames(toPrint)<-c("external_gene_name","ensembl_gene_id","position_in_BM")

write.csv(toPrint, file = "ensembl_gene_ids_nuevos_positivos.csv")
