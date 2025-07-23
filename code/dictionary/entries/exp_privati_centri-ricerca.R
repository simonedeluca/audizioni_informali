#######################################
#             DIZIONARIO              #
# Aziende private , centri di ricerca #
#             ed esperti              #
#######################################


# CENTRI DI RICERCA

# Da script: società_civile

fond_senato <- senato %>% filter(str_detect(NOMI_clean, "fondazione")) %>% pull(NOMI_clean) %>% unique()

fond_ricerca <- fond_senato[c(2,4,10,24,29,30,38)]
fond_ricerca <- c(fond_ricerca, "fondazione ricerca salute")

comit_ricerca <- c("accademia georgofili", "comitato glaciologico italiano")

centri_ricerca <- c(fond_ricerca, comit_ricerca, "censis")


# PRIVATI

# Da script: società_civile

privati <- "forum pa"


# ESPERTI

experts <- c("prof*", "dott*", "avv*", "scrittore", "proto", "arch*", "ing", "ingegnere", "signor", "esperto")

# Save data
saveRDS(centri_ricerca, file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/centri_ricerca.RData")
saveRDS(privati, file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/privati.RData")
saveRDS(experts, file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/experts.RData")

