library(readxl)
library(dplyr)
library(stringr)

setwd("C:/Users/pc/Desktop/Progetto Audizioni/data/preprocessed_data")

senato <- read_excel("dataset_senato.xlsx")

sum(is.na(senato$NOMI))
which(is.na(senato$NOMI))
senato <- senato[!is.na(senato$NOMI),]

senato$nomi_senato <- str_to_upper(senato$NOMI)


setwd("C:/Users/pc/Desktop/notes/Progetto Audizioni")
camera <- read_excel("Mappatura audizioni informali Camera (definitiva e con PNRR).xlsx", sheet = "Audizioni 2018_2020")

camera <- camera[-1,]
camera <- camera[,-c(8:12)]

camera$nomi_camera <- ifelse(is.na(camera$`Ente/Organizzazione`), camera$`Nome e cognome`,
                             camera$`Ente/Organizzazione`)
sum(is.na(camera$nomi_camera))
camera$nomi_camera <- str_to_upper(camera$nomi_camera)

intersect(camera$nomi_camera,senato$nomi_senato) #346 nomenclature uguali

# TASK: standardize the nomenclature of the collected names.

camera %>% count(`Tipologia soggetto`)

camera$`Tipologia soggetto` <- ifelse(camera$`Tipologia soggetto`=="Sindacati e associazioni di categorie", "Sindacati e associazioni di categoria",
                                      ifelse(camera$`Tipologia soggetto`=="Aziende a partecipazione pubblica", "Aziende e società a partecipazione pubblica",
                                             camera$`Tipologia soggetto`))

camera %>% count(`Tipologia soggetto`)

aziende <- camera %>%
  filter(`Tipologia soggetto`=="Aziende") %>%
  select(nomi_camera)

table(aziende)

aziende$nomi_camera <- ifelse(aziende$nomi_camera=="ARCELORMITTAL ITALIA", "ARCELORMITTAL",
                              ifelse(aziende$nomi_camera=="ARCELOR MITTAL", "ARCELORMITTAL",
                                     ifelse(aziende$nomi_camera=="SAMSUNG ELECTONICS SPA", "SAMSUNG ELECTRONICS",
                                            ifelse(aziende$nomi_camera=="SAMSUNG ELECTRONIC SPA", "SAMSUNG ELECTRONICS",
                                                   aziende$nomi_camera))))

table(aziende)

to_remove <- "SRL|SPA|S.R.L.|S.P.A.|ITALY|ITALIA"
aziende$nomi_camera2 <- gsub(to_remove, "", aziende$nomi_camera)


a <- "WIND TRE"
b <- "WINDTRE"
stringdist(a,b, method = "lv")

telos <- stringdist(senato$nomi_senato, aziende$nomi_camera2[8], method = "lv")
table(telos)
which(telos<6)
senato$nomi_senato[607] #TELOS
senato$nomi_senato[4228] #TELT S.A.S.

# Dovrei provare un metodo diverso? Questo non sembra funzionare con which.min in quanto non c'è garanzia che si tratti dello stesso soggetto.


