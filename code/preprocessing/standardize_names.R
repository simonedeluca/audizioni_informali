library(rio)
library(stringr)

setwd("C:/Users/pc/Desktop/notes/Progetto Audizioni")
camera <- import("Mappatura audizioni informali Camera (definitiva e con PNRR).xlsx")

camera <- camera[-1,]

enti_camera <- camera$`Ente/Organizzazione`
nomecogn_camera <- camera$`Nome e cognome`

df <- data.frame(
  enti_camera = enti_camera,
  nomecogn_camera = nomecogn_camera
)

df$nomi_camera <- ifelse(is.na(df$enti_camera), nomecogn_camera,
                         enti_camera)

nomi_camera <- df$nomi_camera
nomi_camera <- str_to_upper(nomi_camera)
nomi_camera <- unique(sort(nomi_camera))

setwd("C:/Users/pc/Desktop/Progetto Audizioni/data/preprocessed_data")
senato <- import("dataset_senato.xlsx")

nomi_senato <- senato$NOMI
nomi_senato <- nomi_senato[!is.na(nomi_senato)]

nomi_senato <- str_to_upper(nomi_senato)
nomi_senato <- unique(sort(nomi_senato))

intersect(nomi_camera,nomi_senato) #346 nomenclature uguali

# TASK: standardize the nomenclature of the collected names.

library("xlsx")
write.xlsx (nomi_camera, "C:/Users/pc/Desktop/Progetto Audizioni/data/preprocessed_data/Camera.xlsx", sheetName="Camera", col.names=TRUE, row.names=FALSE, append=FALSE, showNA=TRUE, password=NULL)
write.xlsx (nomi_senato, "C:/Users/pc/Desktop/Progetto Audizioni/data/preprocessed_data/Senato.xlsx", sheetName="Senato", col.names=TRUE, row.names=FALSE, append=FALSE, showNA=TRUE, password=NULL)

