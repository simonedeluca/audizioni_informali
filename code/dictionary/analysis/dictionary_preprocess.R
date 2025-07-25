library(tidyverse)
library(quanteda)
library(quanteda.textstats)

# PREPROCESSING DIZIONARIO

# SOGGETTI ISTITUZIONALI

ist_list <- readRDS("C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/institutions/institution_list.RData")

# 1. Staccare gli acronimi 

# Crea una tibble con due colonne: nomi e valori (per verificare quali segni di punteggiatura precedono gli acronimi)
unlist_vec <- unlist(ist_list) # Unisci la lista mantenendo i nomi
my_tibble <- tibble(name = names(unlist_vec), values = unlist_vec)

# trattino (hyphen), trattino medio (en dash), aperta parentesi

#Loop through each vector in the list
for (i in names(ist_list)) {
  # Extract the vector
  vec <- ist_list[[i]]
  # Split each element of the vector by hyphen or en dash + flat the split components
  new_vec <- unlist(strsplit(vec, " [-–\\(]"))
  # Update the list with the modified vector
  ist_list[[i]] <- new_vec
}

# 2. Rimuovere punteggiatura (eccetto segno *) e stopwords
ist_list <- lapply(ist_list, function(x) gsub("\\.", "", x))
ist_list <- lapply(ist_list, function(x) gsub("[,;:'\"()\\-]", " ", x))

vec_pulizia <- c("spa", "in liquidazione", "coatta amministrativa", "in breve", "società per azioni", "in sigla", "scpa", "arl")

for (i in vec_pulizia) {
  ist_list <- lapply(ist_list, function(x) {
    gsub(paste0("\\b", i, "\\b"), "", x)
  })
}

stp_wrd <- stopwords('it')
stp_wrd <- setdiff(stp_wrd, c("una", "nostra"))

# Pulire ogni vettore della lista per ogni elemento del vettore di pulizia come parola a sé stante
for (i in stp_wrd) {
  ist_list <- lapply(ist_list, function(x) {
    gsub(paste0("\\b", i, "\\b"), "", x)
  })
}

ist_list <- lapply(ist_list, function(x) gsub("\\s+", " ", trimws(x)))

# Function to remove empty elements
ist_list <- lapply(ist_list, function(x) x[x != ""])


# DIZIONARIO

# Aggiungiamo al dizionario i vettori usati per descrivere le categorie.

dict <- dictionary(list(v1=ist_list[["v1"]],
                        v2=ist_list[["v2"]],
                        v3=ist_list[["v3"]],
                        v4=ist_list[["v4"]],
                        v5=ist_list[["v5"]],
                        v6=ist_list[["v6"]],
                        v7=ist_list[["v7"]],
                        v8=ist_list[["v8"]],
                        v9=ist_list[["v9"]],
                        v10=ist_list[["v10"]],
                        v11=ist_list[["v11"]],
                        v12=ist_list[["v12"]],
                        v13=ist_list[["v13"]],
                        v15=ist_list[["v15"]],
                        v20=ist_list[["v20"]],
                        v22=ist_list[["v22"]],
                        v23=ist_list[["v23"]],
                        v24=ist_list[["v24"]],
                        v25=ist_list[["v25"]],
                        v27=ist_list[["v27"]],
                        v30=ist_list[["v30"]],
                        v31=ist_list[["v31"]],
                        v33=ist_list[["v33"]],
                        v36=ist_list[["v36"]],
                        v37=ist_list[["v37"]],
                        v38=ist_list[["v38"]],
                        org_enti_local=ist_list[["org_enti_local"]],
                        forze_sicurezza=ist_list[["forze_sicurezza"]],
                        gradi_militari=ist_list[["gradi_militari"]],
                        carica_ist=ist_list[["carica_ist"]],
                        eu=ist_list[["eu"]],
                        fond_ist=ist_list[["fond_ist"]],
                        comit_ist=ist_list[["comit_ist"]],
                        new_vec=ist_list[["new_vec"]]))


# PARTECIPATE STATALI (clean)

part_state <- readRDS("C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/part_state.RData")

# DIZIONARIO
dict[["part_state"]] <- part_state


# CONFINDUSTRIA

confind_list <- readRDS("C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/confindustria/confind_list.RData")

# Eliminiamo specifici segni di punteggiatura, non quelli facenti parte dei nomi degli enti (come #vita, FB&Associati)
confind_list <- lapply(confind_list, function(x) gsub("\\.", "", x))

confind_list <- lapply(confind_list, function(x) gsub("[,;:'\"()\\-]", " ", x))

stp_wrd <- stopwords('it')
stp_wrd <- setdiff(stp_wrd, c("una", "nostra"))

# Pulire ogni vettore della lista per ogni elemento del vettore di pulizia come parola a sé stante
for (i in stp_wrd) {
  confind_list <- lapply(confind_list, function(x) {
    gsub(paste0("\\b", i, "\\b"), "", x)
  })
}

confind_list <- lapply(confind_list, function(x) gsub("\\s+", " ", trimws(x)))

# DIZIONARIO

dict[["conf_soci"]] <- confind_list[["conf_soci"]]
dict[["conf_asso"]] <- confind_list[["conf_asso"]]
dict[["conf_district"]] <- confind_list[["conf_district"]]
dict[["ass_subsect"]] <- confind_list[["ass_subsect"]]
dict[["fed_sect"]] <- confind_list[["fed_sect"]]
dict[["rappr_sect"]] <- confind_list[["rappr_sect"]]


# ORDINI PROFESSIONALI (clean)
ord_prof <- readRDS("C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/ordini_pro.RData")

# DIZIONARIO
dict[["ord_prof"]] <- ord_prof


# ORGANIZZAZIONI DI RAPPRESENTANZA
org_rappr <- readRDS("C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/org_rappresentanza.RData")

org_rappr <- tolower(org_rappr)

org_rappr <- gsub("\\.", "", org_rappr)
org_rappr <- gsub("[,;:'\"()\\-]", " ", org_rappr)

for (i in stp_wrd) {
  org_rappr <- lapply(org_rappr, function(x) {
    gsub(paste0("\\b", i, "\\b"), "", x)
  })
}

org_rappr <- gsub("\\s+", " ", trimws(org_rappr))

# DIZIONARIO
dict[["org_rappr"]] <- org_rappr


# ORGANIZZAZIONI SOCIETÀ CIVILE
org_civile <- readRDS("C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/org_società-civile.RData")

org_civile <- gsub("\\.", "", org_civile)
org_civile <- gsub("[,;:'\"()\\-]", " ", org_civile)

for (i in stp_wrd) {
  org_civile <- lapply(org_civile, function(x) {
    gsub(paste0("\\b", i, "\\b"), "", x)
  })
}

org_civile <- gsub("\\s+", " ", trimws(org_civile))

# DIZIONARIO
dict[["org_civile"]] <- org_civile


# ESPERTI
experts <- readRDS("C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/experts.RData")

# DIZIONARIO
dict[["experts"]] <- experts


# CENTRI DI RICERCA
centri_ricerca <- readRDS("C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/centri_ricerca.RData")

# DIZIONARIO
dict[["centri_ricerca"]] <- centri_ricerca


# AZIENDE PRIVATE
privati <- readRDS("C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/privati.RData")

# DIZIONARIO
dict[["privati"]] <- privati




# TEXT ANALYSIS

senato <- read.csv("C:/Users/SImone/Desktop/audizioni_informali/data/senato/clean_data/dataset_senato.csv")

# Creating a quanteda corpus
corpus <- corpus(senato, text_field = "NOMI_clean")
tok <- tokens(corpus)

dfm_result <- tokens_lookup(tok, dictionary = dict, nested_scope = "dictionary", exclusive = FALSE) %>% dfm()

result <- as.tibble(dfm_result)

result <- result %>% mutate(sogg_istituz = v1+v2+v3+v4+v5+v6+v7+v8+v9+v10+v11+
                              v12+v13+v15+v20+v22+v23+v24+v25+v27+v30+v31+v33+
                              v36+v37+v38+org_enti_local+forze_sicurezza+gradi_militari+
                              carica_ist+eu+fond_ist+comit_ist+new_vec,
                            part_state = part_state,
                            confindustria = conf_soci+conf_asso+conf_district+ass_subsect+
                              fed_sect+rappr_sect,
                            ord_prof = ord_prof,
                            org_rappr = org_rappr,
                            org_civile = org_civile,
                            esperti = experts,
                            centri_ricerca = centri_ricerca,
                            privati = privati,
                            length = ntoken(dfm_result),
                            sum = sogg_istituz+part_state+confindustria+ord_prof+org_rappr+org_civile+esperti+
                              centri_ricerca+privati) %>%
  select(sogg_istituz, part_state, confindustria, ord_prof, org_rappr, org_civile, esperti, centri_ricerca, privati, length, sum)


# Categorie aggregate
result <- result %>% mutate(sogg_istituz = v1+v2+v3+v4+v5+v6+v7+v8+v9+v10+v11+
                              v12+v13+v15+v20+v22+v23+v24+v25+v27+v30+v31+v33+
                              v36+v37+v38+org_enti_local+forze_sicurezza+gradi_militari+
                              carica_ist+eu+fond_ist+comit_ist+new_vec,
                            part_state = part_state,
                            org_rappr = org_rappr+ord_prof+                                      #ordini professionali
                              conf_soci+conf_asso+conf_district+ass_subsect+fed_sect+rappr_sect, #confindustria
                            org_civile = org_civile,
                            esperti = experts,
                            centri_ricerca = centri_ricerca,
                            privati = privati,
                            length = ntoken(dfm_result),
                            sum = sogg_istituz+part_state+org_rappr+org_civile+esperti+
                              centri_ricerca+privati) %>%
  select(sogg_istituz, part_state, org_rappr, org_civile, esperti, centri_ricerca, privati, length, sum)

testo_ricomposto <- sapply(tok, function(x) paste(x, collapse = " "))
result$text <- testo_ricomposto

# 3812 of 7089 = 53,7%
# 4507 of 7089 = 63,5%

result <- result %>%
  rowwise() %>%
  mutate(
    values = list(c(sogg_istituz, part_state, org_rappr, org_civile, esperti, centri_ricerca, privati)),
    labels = list(c("sogg_istituz", "part_state", "org_rappr", "soc_civile", "esperti", "centri_ricerca", "privati")),
    
    max_val = max(values), #quante corrispondenze ha avuto la categoria più rappresentata nella riga
    n_max = sum(values == max_val), #quante volte compare il numero massimo nella riga
    
    label = if (max_val == 0) {
      "NC"
    } else if (n_max > 1) {
      "pareggio"
    } else {
      labels[which.max(values)]
    }
  ) %>%
  ungroup() %>%
  select(sogg_istituz, part_state, org_rappr, org_civile, esperti, centri_ricerca, privati, length, sum, text, label) 

library(ggplot2)

ggplot(result, aes(label)) +
  geom_bar()


ggplot(result) +
  geom_bar(aes(y = label))
