#BiocManager::install("mirbase.db")
library(mirbase.db)

#CARGAR LA BASE
base= read.csv("../discapacidades-intelectuales/ML/2020/microarns.csv")
#Contenedores
starts= array(dim = dim(base)[1])
ends= array(dim = dim(base)[1])

##############################    INMADUROS   ###################################
#CARGAR LOS DATOS DE INICIO DE LOS MIRNAS
x <- mirbaseCHRLOC
mapped_keys <- mappedkeys(x)
xx <- as.list(x[mapped_keys])

#CARGAR LOS DATOS DE FINAL DE MIRNAS
y= mirbaseCHRLOCEND
y_mapped_keys= mappedkeys(y)
yy= as.list(y[y_mapped_keys])

#Exploración en inmaduros
mirnas= array(dim = dim(base)[1], 0)

for (mirna in 1:dim(base)[1]) {
  for (key in 1:length(mapped_keys)) {
    if(mapped_keys[key] == base[mirna,1]){
      mirnas[mirna]= key
      starts[mirna]= get(mapped_keys[key], x)
      ends[mirna]= get(y_mapped_keys[key], y)
    }
  } 
}

##############################    MADUROS   #####################################
#CARGAR LOS DATOS DE INICIO Y FINAL DE LOS MIRNAS
alfa= mirbaseMATURE
alfa_mapped_keys= mappedkeys(alfa)
#Exploración en maduros

#Para cada mirna maduro
for (mirna in 1701:1861) {
  #Para cada mirna maduro en la base
  for (maduro in 1:length(alfa_mapped_keys)) {
    #tomar la tupla de mirnas maduros
    tupla= attr(get(alfa_mapped_keys[maduro], alfa), "matureName")
    #Para cada mirna maduro en la tupla
    for (nombre_en_tupla in 1:length(tupla)) {
      #Si el mirna maduro es igual al mirna maduro de la tupla
      if(base[mirna,1] == tupla[nombre_en_tupla]){
        #Saca el inicio
        starts[mirna]= attr(get(alfa_mapped_keys[maduro], alfa), "matureFrom")[nombre_en_tupla]
        #Saca el final
        ends[mirna]= attr(get(alfa_mapped_keys[maduro], alfa), "matureTo")[nombre_en_tupla]
        #Escribe la posición pero en negativo
        mirnas[mirna]= -maduro
      }
    }
  }
}

#677 datos
parciales1= matrix(nrow = 677, ncol = 3)
colnames(parciales1) = c("mirna","start","end")
parciales1= as.data.frame(parciales1)
parciales1[,1]= base[1:677,1]
parciales1[,2]= starts[1:677]
parciales1[,3]= ends[1:677]

write.csv(parciales1, file = "parciales_677.csv", row.names = FALSE)

#678-800
parciales2= matrix(nrow = (800-677), ncol = 3)
colnames(parciales2) = c("mirna","start","end")
parciales2= as.data.frame(parciales2)
parciales2[,1]= base[678:800,1]
parciales2[,2]= starts[678:800]
parciales2[,3]= ends[678:800]

write.csv(parciales2, file = "parciales_678-800.csv", row.names = FALSE)

#801-1000
parciales3= matrix(nrow = (200), ncol = 3)
colnames(parciales3) = c("mirna","start","end")
parciales3= as.data.frame(parciales3)
parciales3[,1]= base[801:1000,1]
parciales3[,2]= starts[801:1000]
parciales3[,3]= ends[801:1000]

write.csv(parciales3, file = "parciales_801-1000.csv", row.names = FALSE)

#1001-1225
parciales4= matrix(nrow = (225), ncol = 3)
colnames(parciales4) = c("mirna","start","end")
parciales4= as.data.frame(parciales4)
parciales4[,1]= base[1001:1225,1]
parciales4[,2]= starts[1001:1225]
parciales4[,3]= ends[1001:1225]

write.csv(parciales4, file = "parciales_1001-1225.csv", row.names = FALSE)

#1226-1460
parciales5= matrix(nrow = (235), ncol = 3)
colnames(parciales5) = c("mirna","start","end")
parciales5= as.data.frame(parciales5)
parciales5[,1]= base[1226:1460,1]
parciales5[,2]= starts[1226:1460]
parciales5[,3]= ends[1226:1460]

write.csv(parciales5, file = "parciales_1226-1460.csv", row.names = FALSE)

#1461-1700
parciales6= matrix(nrow = (240), ncol = 3)
colnames(parciales6) = c("mirna","start","end")
parciales6= as.data.frame(parciales6)
parciales6[,1]= base[1461:1700,1]
parciales6[,2]= starts[1461:1700]
parciales6[,3]= ends[1461:1700]

write.csv(parciales6, file = "parciales_1461-1700.csv", row.names = FALSE)

#1701-1861
parciales7= matrix(nrow = (161), ncol = 3)
colnames(parciales7) = c("mirna","start","end")
parciales7= as.data.frame(parciales7)
parciales7[,1]= base[1701:1861,1]
parciales7[,2]= starts[1701:1861]
parciales7[,3]= ends[1701:1861]

write.csv(parciales7, file = "parciales_1701-1861.csv", row.names = FALSE)
##########################################################################
##########################################################################

p1= read.csv("parciales_677.csv")
p2= read.csv("parciales_678-800.csv")
p3= read.csv("parciales_801-1000.csv")
p4= read.csv("parciales_1001-1225.csv")
p5= read.csv("parciales_1226-1460.csv")
p6= read.csv("parciales_1461-1700.csv")
p7= read.csv("parciales_1701-1861.csv")

mirnas_data= rbind(p1,p2)
mirnas_data= rbind(mirnas_data,p3)
mirnas_data= rbind(mirnas_data,p4)
mirnas_data= rbind(mirnas_data,p5)
mirnas_data= rbind(mirnas_data,p6)
mirnas_data= rbind(mirnas_data,p7)

diferencias= array(dim = dim(mirnas_data)[1])

for (mirna in 1:dim(mirnas_data)[1]) {
  diferencias[mirna] = mirnas_data[mirna,3] - mirnas_data[mirna,2]
}

mirnas_data_completed= cbind(mirnas_data, diferencias)

write.csv(mirnas_data_completed, file = "miRNAs_starts-ends-length.csv", row.names = FALSE)

#######################################################################################
#######################################################################################
counts = read.csv("../discapacidades-intelectuales/ML/2020/microarns.csv")
featureLengths= read.csv("miRNAs_starts-ends-length.csv")

faltantes= is.na(featureLengths[,4])
counts_found= counts[!faltantes,]
featureLengths_found= featureLengths[!faltantes,4]