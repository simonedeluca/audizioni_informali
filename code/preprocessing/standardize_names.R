library(readxl)
library(dplyr)
library(stringr)
library(stringdist)

setwd("C:/Users/pc/Desktop/Progetto Audizioni/data/preprocessed_data")

senato <- read_excel("dataset_senato.xlsx")

#Remove NA values in NOMI (missing info)
sum(is.na(senato$NOMI))
which(is.na(senato$NOMI))
senato <- senato[!is.na(senato$NOMI),]

#Create var + cleaning
senato$subject <- str_to_upper(senato$NOMI) %>% str_squish()

# Check for duplicated rows
sum(duplicated(senato)) #940 duplicati in senato
senato <- unique(senato)


setwd("C:/Users/pc/Desktop/notes/Progetto Audizioni")
camera <- read_excel("Mappatura audizioni informali Camera (definitiva e con PNRR).xlsx", sheet = "Audizioni 2018_2020")

#Delete noise
camera <- camera[-1,]
camera <- camera[,-c(8:12)]

#Check for identified category
camera %>% count(`Tipologia soggetto`) #9 categories

# Correct mispelling
camera$`Tipologia soggetto` <- ifelse(camera$`Tipologia soggetto`=="Sindacati e associazioni di categorie", "Sindacati e associazioni di categoria",
                                      ifelse(camera$`Tipologia soggetto`=="Aziende a partecipazione pubblica", "Aziende e società a partecipazione pubblica",
                                             camera$`Tipologia soggetto`))

camera %>% count(`Tipologia soggetto`) #7 categories

#Create var
camera$subject <- ifelse(is.na(camera$`Ente/Organizzazione`), camera$`Nome e cognome`,
                             camera$`Ente/Organizzazione`)
#Check for NA values
sum(is.na(camera$subject))

#Cleaning
camera$subject <- str_to_upper(camera$subject) %>% str_squish()

# Checking for duplicated rows
sum(duplicated(camera)) #6 duplicati in camera
camera <- unique(camera)

#Check for common subjects
intersect(camera$subject,senato$subject) #346 nomenclature uguali

#Create a new dataframe with all the subjects heard by the Camera + their identified category
cat_camera <- data.frame(subject = camera$subject,
                         category = camera$`Tipologia soggetto`) %>%
  distinct(.keep_all = TRUE)

#Create a new dataframe with all the subjects heard by the Senato
cat_senato <- data.frame(subject = senato$subject) %>%
  distinct(.keep_all = TRUE)

intersect(cat_camera$subject, cat_senato$subject)

z <- merge(cat_senato, cat_camera) #Since it returns more obs than expected, it might be that the same subject is associated to more than one category. 

mis_cat <- cat_camera %>% group_by(subject) %>% filter(n()>1) #mis-categorization in the camera dataset

###

# TASK: standardize the nomenclature of the collected names.


# STANDARDIZING CATEGORY "AZIENDE"

aziende <- camera %>%
  filter(`Tipologia soggetto`=="Aziende") %>%
  select(subject)

table(aziende)

aziende$subject <- ifelse(aziende$subject=="ARCELORMITTAL ITALIA", "ARCELORMITTAL",
                              ifelse(aziende$subject=="ARCELOR MITTAL", "ARCELORMITTAL",
                                     ifelse(aziende$subject=="SAMSUNG ELECTONICS SPA", "SAMSUNG ELECTRONICS",
                                            ifelse(aziende$subject=="SAMSUNG ELECTRONIC SPA", "SAMSUNG ELECTRONICS",
                                                   aziende$subject))))

table(aziende)

to_remove <- "SRL|SPA|S.R.L.|S.P.A.|ITALY|ITALIA"
aziende$subject2 <- gsub(to_remove, "", aziende$subject) %>% str_squish()

