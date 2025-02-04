library(tidyverse)
library(quanteda)

# PREPROCESSING DIZIONARIO

# SOGGETTI ISTITUZIONALI

ist_list <- load("~/.../audizioni_informali/data/clean_data/istitution_list.RData")

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
stp_wrd <- setdiff(stp_wrd, "una")

# Pulire ogni vettore della lista per ogni elemento del vettore di pulizia come parola a sé stante
for (i in stp_wrd) {
  ist_list <- lapply(ist_list, function(x) {
    gsub(paste0("\\b", i, "\\b"), "", x)
  })
}

ist_list <- lapply(ist_list, function(x) gsub("\\s+", " ", trimws(x)))

# Function to remove empty elements
ist_list <- lapply(ist_list, function(x) x[x != ""])


## DIZIONARIO

Aggiungiamo al dizionario i vettori usati per descrivere le categorie.

```{r dictionary}
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
                        eu=ist_list[["eu"]]))


# CONFINDUSTRIA

confind_list <- readRDS("~/.../audizioni_informali/data/clean_data/confind_list.RData")

# Eliminiamo specifici segni di punteggiatura, non quelli facenti parte dei nomi degli enti (come #vita, FB&Associati)
confind_list <- lapply(confind_list, function(x) gsub("\\.", "", x))

confind_list <- lapply(confind_list, function(x) gsub("[,;:'\"()\\-]", " ", x))

stp_wrd <- stopwords('it')
stp_wrd <- setdiff(stp_wrd, "una")

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
