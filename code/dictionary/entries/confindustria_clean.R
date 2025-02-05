
# Pulizia dati Confindustria

# Carichiamo i dati e partiamo dalle sezioni con le sottoliste.

rappr_evolute <- read.csv("[path]/rappr_evolute.csv", header = TRUE, stringsAsFactors = FALSE)
rappr_settore <- read.csv("[path]/rappr_settore.csv", header = TRUE, stringsAsFactors = FALSE)
feder_settore <- read.csv("[path]/feder_settore.csv", header = TRUE, stringsAsFactors = FALSE)
galax_confind <- read.csv("[path]/galax_confind.csv", header = TRUE, stringsAsFactors = FALSE)

# Sulla scorta dell'organizzazione dei dati in feder_settore, vogliamo riprodurre
# la relazione tra capolista e membri della lista anche in rappr_settore e rappr_evolute.

# Correggiamo le prime due colonne: nella prima salviamo il capolista, nella seconda
# il nome e il nome esteso dell'ente separati dal trattino.

# Se dovessi dare un nome al task direi, galassie e pianeti dell'universo Confindustria.


# Packages
library(tidyverse)

# Rappresentanze evolute

rappr_evolute$nome_esteso <- sapply(rappr_evolute$nome_esteso, function(x) sub("^[^-]*- ", "", trimws(x)))

rappr_evolute[4,1] <- "UNA"

rappr_evolute <- rappr_evolute %>% mutate(nome_esteso = paste(nome, nome_esteso, sep = " - "))
rappr_evolute$nome <- "CONFINDUSTRIA PROFESSIONI E MANAGEMENT"

rappr_evolute <- rappr_evolute %>% mutate(across(everything(), ~ trimws(tolower(.))))

colnames(rappr_evolute) <- c("ente_ref", "nome", "tipologia", "ambito_interesse")


# Rappresentanze settore

x <- rappr_settore

# Aggiungiamo i valori mancanti

x[17,2] <- "ANFIDA"
x[20,2] <- "ASSOMINERARIA"
x[30,2] <- "ASSALZOO"
x[35,2] <- "ASSOCARNI"
x[41,2] <- "ITALMOPA"
x[61,2] <- "ACIMGA"
x[64,2] <- "ASSOCOMAPLAST"
x[66,2] <- "ASSOMARMOMACCHINE"
x[105,2] <- "UNCMA"
x[107,2] <- "UNACOMA"

# Eliminiamo righe attribuibili a bug nella costruzione del sito

x <- x[-78, ]
x <- x[-89, ]
x <- x[-98, ]

# La strategia per assegnare un numero ai genitori e ai rispettivi figli prende
# come riferimento la tipologia dell'ente.

table(x$tipologia)

# Sappiamo che ci sono 12 rappresentanze di settore (capolista) e che i membri di
# una lista occupano le righe sotto il capolista.

# Creiamo una variabile per registrare i valori numerici, che aumenteranno ad ogni
# rappresentanza di settore

x$main_nodo <- NA

num <- 0

for (i in 1:nrow(x)) {
  if (x$tipologia[i] == "Rappresentanza di Settore") {
    num <- num + 1
  }
  x$main_nodo[i] <- num
}

# Recupera i nomi dei genitori e metti il cognome ai figli

tag <- x %>% filter(tipologia == "Rappresentanza di Settore") %>% pull(nome)

x$tag <- tag[x$main_nodo]

x <- x %>% mutate(across(everything(), ~ trimws(tolower(.))))

# Incolla nome breve e nome esteso separati dal trattino, evitando ripetizioni

x <- x %>%
  mutate(nuova = ifelse(nome_esteso != nome & nome != tag,
                        paste(nome, nome_esteso, sep = " - "),
                        nome_esteso))

x <- x %>%
  filter(tipologia == "rappresentanza di settore") %>%
  mutate(nuova = ifelse(nome_esteso != nome & nome == tag,
                        paste(nome, nome_esteso, sep = " - "),
                        nome_esteso)) %>%
  bind_rows(filter(x, tipologia != "rappresentanza di settore"))


# Correggi ripetizioni

x$nuova2 <- ifelse(
  grepl("^[^-]*-[^-]*-", x$nuova),  # Verifica se ci sono esattamente due trattini
  sub("^[^-]*- ", "", x$nuova),      # Rimuovi tutto fino al primo trattino
  x$nuova                           # Altrimenti lascia la stringa invariata
)

x <- x %>% select(tag, nuova2, tipologia, ambito_interesse)

colnames(x) <- c("ente_ref", "nome", "tipologia", "ambito_interesse")

rappr_settore <- x


# Federazioni di settore

# Eliminiamo le parentesi e il contenuto

feder_settore$nome_esteso <- gsub("\\(.*?\\)", "", feder_settore$nome_esteso)

feder_settore <- feder_settore %>% mutate(across(everything(), ~ trimws(tolower(.))))

# Incolla nome breve e nome esteso per casi specifici

feder_settore <- feder_settore %>%
  filter(tipologia == "federazione di settore") %>%
  mutate(nome_esteso = ifelse(nome_esteso != nome,
                              paste(nome, nome_esteso, sep = " - "),
                              nome_esteso)) %>%
  bind_rows(filter(feder_settore, tipologia != "federazione di settore"))

colnames(feder_settore) <- c("ente_ref", "nome", "tipologia", "ambito_interesse")

# Correggi abbreviazione

feder_settore[12,2] <- "anie aice - associazione italiana industrie cavi e conduttori elettrici"

# Questo e pochi altri casi (soprattutto del settore ANIE), presentano il nome del
# capolista anche nel loro nome breve. Non lo cancelliamo perché considerato integrante.



# Creiamo il dataset delle sezioni con le sottoliste

conf_sublist <- rbind(feder_settore, rappr_settore, rappr_evolute)

# Ora abbiamo due dataset:

# 1. galax_confind: che comprende le sezioni "rappresentanze-regionali", "associazioni-territorio",
#                                            "associazioni-settore", "associati-aggregati",
#                                            "rappresentanze-internazionali"
# 2. conf_sublist: che comprende le sezioni "federazioni-settore", "rappresentanze-evolute",
#                                           "rappresentanze-settore"



# Vettori dizionario

# Procediamo con la creazione dei vettori per il nostro dizionario, prendendo come
# riferimento la tipologia degli enti.

# 1. Partiamo con il dataset galax_confind

table(galax_confind$tipologia)


# Confindustria Regionale e Associazione Territoriale possono essere riassunte in poche parole chiave

conf_district <- c("confindustria", "unindustria", "assolombarda", "sicindustria", "unione industrial*")


# Associazioni di categoria

ass_cat <- galax_confind %>% filter(tipologia == "Associazione di Categoria")

# Elimina le parti con il trattino

ass_cat$nome_esteso[grepl("-", ass_cat$nome_esteso)]

# Correggi valore
ass_cat[32, 2] <- "Associazione Italiana dell'Industria Olearia"

# Elimina la parte iniziale della stringa fino al trattino (perché già in nome)
ass_cat$nome_esteso <- sapply(ass_cat$nome_esteso, function(x) sub("^[^-]*-", "", x))

ass_cat <- ass_cat %>% mutate(across(everything(), ~ trimws(tolower(.))))  # minuscolo e rimuovi spazi

# Correggi valore
ass_cat[65,1] <- "elettricità futura"

# Crea vettore che comprenda entrambe le denominazioni: nome breve e nome esteso
conf_asso <- c(ass_cat$nome, ass_cat$nome_esteso) %>% unique()


# Socio aggregato

soci_agg <- galax_confind %>% filter(tipologia == "Socio Aggregato")

# Eliminiamo i nomi brevi dalla colonna dei nomi estesi

# Stringa dall'inizio al trattino
n <- c(1, 6)
for(i in n) {
  soci_agg$nome_esteso[i] <- sub("^[^-]*-", "", soci_agg$nome_esteso[i])
}

# Stringa dal trattino alla fine
n <- c(2, 11)
for(i in n) {
  soci_agg$nome_esteso[i] <- sub("-.*", "", soci_agg$nome_esteso[i])
}

# Correggi valore
soci_agg[5,2] <- "associazione trasporti"

soci_agg <- soci_agg %>% mutate(across(everything(), ~ trimws(tolower(.))))  # minuscolo e rimuovi spazi

conf_soci <- c(soci_agg$nome, soci_agg$nome_esteso) %>% unique()


# Gli enti in Rappresentanza internazionale non sono presenti nel dataset del Senato,
# quindi non creiamo il vettore.


# I vettori del dataset galax_confind sono conf_district, conf_asso, conf_soci


# 2. Procediamo con il dataset conf_sublist (pulito in precedenza)

table(conf_sublist$tipologia)

# Notiamo che ci sono dei valori mancanti (10 soggetti senza etichetta)


# Associazione di categoria

vec_ass <- conf_sublist %>% filter(tipologia == "associazione di categoria") %>% pull(nome)
vec_ass <- unlist(str_split(vec_ass, "-")) %>% trimws()

# Trattandosi di una categoria già presente in galax_confind, confrontiamo i vettori
# per vedere se condividono le stesse stringhe

setdiff(vec_ass, conf_asso) #no need

# Questo vuol dire che non c'è bisogno di riportare il vettore nel dizionario,
# basterà conf_asso


# Associazione di sottosettore

ass_subsect <- conf_sublist %>% filter(tipologia == "associazione di sottosettore") %>% pull(nome)
ass_subsect <- unlist(str_split(ass_subsect, "-")) %>% trimws()
ass_subsect <- unique(ass_subsect)

# Alcune associazioni di sottosettore in conf_sublist sono classificate come associazioni
# di categoria in galax_confind.

z <- intersect(ass_subsect, conf_asso)

# Rimuoviamole dal vettore ma questo ci spinge a pensare che siano la stessa classe
# codificata in due modi diversi.

# Prima aggiungiamo il nome esteso di aniasa
z <- c(z, "associazione noleggiatori italiani autoveicoli senza autista")

ass_subsect <- setdiff(ass_subsect, z)


# Federazione di settore

fed_sect <- conf_sublist %>% filter(tipologia == "federazione di settore") %>% pull(nome)
fed_sect <- unlist(str_split(fed_sect, "-")) %>% trimws()
fed_sect <- unique(fed_sect)


# Rappresentanze di settore

rappr_sect <- conf_sublist %>% filter(tipologia == "rappresentanza di settore") %>% pull(nome)
rappr_sect <- unlist(str_split(rappr_sect, "-")) %>% trimws()
rappr_sect <- unique(rappr_sect)


# Etichette mancanti

conf_blank <- conf_sublist %>% filter(tipologia == "") %>% pull(nome)

intersect(conf_blank, ass_subsect)

# Diamo un'etichetta a questi enti: aggiungiamoli alle associazioni di sottosettore

ass_subsect <- c(ass_subsect, conf_blank) %>% unique()


# I vettori del dataset conf_sublist sono: ass_subsect, fed_sect, rappr_sect


# Creiamo e salviamo la lista per il dizionario

confind_list <- list(conf_soci = conf_soci,
                     conf_asso = conf_asso,
                     conf_district = conf_district,
                     ass_subsect = ass_subsect,
                     fed_sect = fed_sect,
                     rappr_sect = rappr_sect)

saveRDS(confind_list, file = "[path]/confind_list.RData")
