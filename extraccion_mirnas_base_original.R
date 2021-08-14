library(tidyverse)

dummy= read.csv("Desktop/Tesis/discapacidades-intelectuales/ML/2020/llaves/resultados1.csv")
dummy2= read.csv("Desktop/Tesis/discapacidades-intelectuales/ML/2020/llaves/resultados10.csv")

dummy= rbind(dummy,dummy2)

write.csv(dummy, file = "complete_results.csv", row.names = FALSE)

dummy3= unique(dummy[,1])

write.csv(dummy3, file = "complete_results.csv", row.names = FALSE)

base= read.csv("expression_matrix.csv")

base_mirnas= base[dummy3,]

write.csv(base_mirnas, file = "./llaves/mirnas_expression_matrix.csv", row.names = FALSE)
