library(readxl)
library(dplyr)
library(stringr)

setwd("C:/Users/pc/Desktop/notes/Progetto Audizioni")
camera <- read_excel("Mappatura audizioni informali Camera (definitiva e con PNRR).xlsx", sheet = "Audizioni 2018_2020")

camera <- camera[-1,]
camera <- camera[,-c(8:12)]

camera$nomi_camera <- ifelse(is.na(camera$`Ente/Organizzazione`), camera$`Nome e cognome`,
                             camera$`Ente/Organizzazione`)
sum(is.na(camera$nomi_camera))

camera %>% count(`Tipologia soggetto`)

camera$`Tipologia soggetto` <- ifelse(camera$`Tipologia soggetto`=="Sindacati e associazioni di categorie", "Sindacati e associazioni di categoria",
                                      ifelse(camera$`Tipologia soggetto`=="Aziende a partecipazione pubblica", "Aziende e società a partecipazione pubblica",
                                             camera$`Tipologia soggetto`))

camera %>% count(`Tipologia soggetto`)

aziende <- camera %>%
  filter(`Tipologia soggetto`=="Aziende") %>%
  select(nomi_camera)

table(aziende)
aziende$nomi_camera <- str_to_upper(aziende$nomi_camera)

aziende$nomi_camera <- ifelse(aziende$nomi_camera=="ARCELORMITTAL ITALIA", "ARCELORMITTAL",
                              ifelse(aziende$nomi_camera=="ARCELOR MITTAL", "ARCELORMITTAL",
                                     ifelse(aziende$nomi_camera=="SAMSUNG ELECTONICS SPA", "SAMSUNG ELECTRONICS",
                                            ifelse(aziende$nomi_camera=="SAMSUNG ELECTRONIC SPA", "SAMSUNG ELECTRONICS",
                                                   aziende$nomi_camera))))

table(aziende)

to_remove <- "SRL|SPA|S.R.L.|S.P.A.|ITALY|ITALIA"
aziende$nomi_camera2 <- gsub(to_remove, "", aziende$nomi_camera)








camera$nomi_camera <- gsub("\\.", "", camera$nomi_camera)

camera <- str_split_fixed(camera$nomi_camera, ",", 2)
camera$nomi_camera <- str_to_upper(camera$nomi_camera)


































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

