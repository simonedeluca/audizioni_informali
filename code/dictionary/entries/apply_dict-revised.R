
library(tidyverse)
library(quanteda)
library(quanteda.textstats)

# Applichiamo il dizionario revisionato sul dataset ripulito con le etichette del primo dizionario.
# Poi confrontiamo i risultati.

dict_revised <- readRDS(file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/dict_revised.RData")

# Facciamo qualche modifica

dict_revised[["esperti"]] <- dict_revised[["esperti"]][!dict_revised[["esperti"]] %in% c("don", "padre", "suor")]
dict_revised[["neutro"]] <- c(dict_revised[["neutro"]], "don", "padre", "suor")

# Preprocessing ist

ist <- dict_revised[["istituzioni"]]

# 1. Staccare gli acronimi 

ist_new <- unlist(strsplit(ist, " [-–\\(]")) # i segni di punteggiatura sono stati cercati manualmente

ist_new <- str_trim(ist_new)
ist_new <- unique(ist_new)

# 2. Rimuovere punteggiatura (eccetto segno *) e stopwords
ist_new <- lapply(ist_new, function(x) gsub("\\.", "", x))
ist_new <- lapply(ist_new, function(x) gsub("[,;:'\"()\\-]", " ", x))

stp_wrd <- stopwords('it')
stp_wrd <- setdiff(stp_wrd, c("una", "nostra"))

# Pulire ogni vettore della lista per ogni elemento del vettore di pulizia come parola a sé stante
for (i in stp_wrd) {
  ist_new <- lapply(ist_new, function(x) {
    gsub(paste0("\\b", i, "\\b"), "", x)
  })
}

ist_new <- lapply(ist_new, function(x) gsub("\\s+", " ", trimws(x)))

# Function to remove empty elements
ist_new <- lapply(ist_new, function(x) x[x != ""]) # no need

ist_new <- unlist(ist_new)

# Preprocessing cat

cat <- dict_revised[["categoria"]]

cat <- lapply(cat, function(x) gsub("\\.", "", x))
cat <- lapply(cat, function(x) gsub("[,;:'\"()\\-]", " ", x))


# Pulire ogni vettore della lista dalle stopwords
for (i in stp_wrd) {
  cat <- lapply(cat, function(x) {
    gsub(paste0("\\b", i, "\\b"), "", x)
  })
}

cat <- lapply(cat, function(x) gsub("\\s+", " ", trimws(x)))
cat <- unlist(cat)

# Preprocessing partecipate

part <- dict_revised[["partecipate"]]

part <- lapply(part, function(x) gsub("’", " ", x))

# Pulire ogni vettore della lista dalle stopwords
for (i in stp_wrd) {
  part <- lapply(part, function(x) {
    gsub(paste0("\\b", i, "\\b"), "", x)
  })
}

part <- lapply(part, function(x) gsub("\\s+", " ", trimws(x)))
part <- unlist(part)

# Gli altri vettori sono già senza punteggiatura e stopwords

# Sostituiamo i gruppi nel dizionario

dict_revised[["istituzioni"]] <- ist_new
dict_revised[["partecipate"]] <- part
dict_revised[["categoria"]] <- cat



# text analysis


senato <- read.csv("C:/Users/SImone/Desktop/audizioni_informali/data/senato/senato_label.csv")

corpus <- corpus(senato, text_field = "NOMI_clean")
tok <- tokens(corpus)

dfm_result <- tokens_lookup(tok, dictionary = dict_revised, nested_scope = "dictionary", exclusive = FALSE) %>% dfm()

result <- as.tibble(dfm_result)

result <- result %>% mutate(length = ntoken(dfm_result),
                            sum = istituzioni + partecipate + categoria + soc_civile + privati + esperti + altro + neutro) %>%
  select(istituzioni, partecipate, categoria, soc_civile, privati, esperti, altro, neutro, length, sum)

testo_ricomposto <- sapply(tok, function(x) paste(x, collapse = " "))
result$text <- testo_ricomposto


# creo le etichette

macrocategorie <- c("istituzioni", "partecipate", "categoria", "soc_civile", "privati", "esperti", "altro")

etichette <- c("istituzioni", "partecipate", "sindacati e categorie", "società civile", "privati", "esperti", "altro")

result <- result %>%
  rowwise() %>%
  mutate(
    max_val = max(c_across(all_of(macrocategorie)), na.rm = TRUE),
    n_max = sum(c_across(all_of(macrocategorie)) == max_val, na.rm = TRUE),
    
    label = case_when(
      max_val == 0 ~ "NC",
      n_max > 1 ~ "pareggio",
      TRUE ~ etichette[which.max(c_across(all_of(macrocategorie)))]
    )
  ) %>%
  ungroup() %>%
  select(
    istituzioni, partecipate, categoria, soc_civile, privati,
    esperti, altro, neutro, length, sum, text, label
  )

# label in dataset
senato$LABEL3 <- result$label

data <- senato %>% select(NOMI_clean, LABEL, LABEL2, LABEL3)
# (LABEL2 è la classificazione con il dizionario che contiene neutro come macrocategoria)

# Dizionari a confronto: distribuzione nelle categorie
table(senato$LABEL) # prima classificazione
table(senato$LABEL2)
table(senato$LABEL3)


senato %>% filter(LABEL2=="neutro" & LABEL=="esperti") %>% select(NOMI_clean) # setacciati


# Nota: togliendo neutro dalle etichette e dal conteggio per la macrocategoria,
# abbiamo circa 190 classificazioni in più rispetto al dizionario che tiene conto
# delle cariche neutre. Questo è un vantaggio anche perché la carica neutra non è
# rilevante per l'individuazione della macrocategoria. Quando è presente l'ente
# di riferimento, prevale l'etichetta dell'ente. Il ruolo professionale o
# specialistico è catturato dalla categoria esperti.

table(senato$LABEL3)

# Ci sono 511 NC. Le altre classificazioni possono darci indizi utili.

data %>% filter(LABEL3=="NC") %>% select(LABEL2) %>% table()
data %>% filter(LABEL3=="NC") %>% select(LABEL) %>% table()

# Confrontando i risultati, spiccano le 200 istituzioni catturate dalla prima classificazione.
# Forse il lavoro di revisione ha eliminato pattern individuati dal lavoro manuale di risoluzione
# dei pareggi, post prima classificazione.

# Andiamo a vedere (trattasi di 2/5 dei casi)

# Possiamo individuare i pattern di scarto tra i due dizionari alla voce istituzioni?
dict_1 <- readRDS(file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/dict_1.RData")

ist_plus <- setdiff(dict_1[["istituzioni"]], dict_revised[["istituzioni"]]) %>% sort()
# difatti, abbiamo 277 risultati

# Non li aggiungiamo automaticamente, controlliamo.

# Istituzioni - da integrare




# Lavoro sulla macrocategoria istituzioni divisa in sottogruppi (salvata nello script institutions)
ist_sub <- readRDS(file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/institutions/institution_list-revised.RData") # no preprocessing

# La lista "ist_sub" ha il vantaggio di ospitare i sottotipi; non contiene però gli elementi aggiunti nella revisione delle altre macrocategorie.
# Teniamolo presente per dopo.


# Aggiungiamo gli NC effettivamente "istituzioni" nei rispettivi sottotipi. 


# Organi costituzionali, di rilievo costituzionale, giurisdizionali e di controllo
ist_sub[["v1"]] <- c(ist_sub[["v1"]], "tribunale sorveglianza", "tribunale minorenni", "consiglio presidenza giustizia tributaria",
                     "procura nazionale antimafia", "dna")

# Presidenza del Consiglio dei ministri e Ministeri
ist_sub[["v2"]] <- c(ist_sub[["v2"]], "direzione centrale anticrimine", "dipartimento amministrazione penitenziaria",
                     "autorità nazionale uama", "ragioneria generale stato", "dipartimento finanze", "garante sorveglianza prezzi",
                     "grande progetto pompei", "mibac", "mit", "direzione generale*", "mite", "mise")

# expertise: ragioneria generale stato, dipartimento finanze, garante sorveglianza prezzi

# Agenzie fiscali

# Enti di regolazione dell’attività economica
ist_sub[["v4"]] <- c(ist_sub[["v4"]], "agenzia italia digitale", "ansf", "ispettorato lavoro")

# expertise: agenzia italia digitale, ansf, ispettorato lavoro

# Enti produttori di servizi economici
ist_sub[["v5"]] <- c(ist_sub[["v5"]], "ismea", "stabilimento chimico farmaceutico militare")

# expertise: ismea , stabilimento chimico farmaceutico militare

# Autorità indipendenti*
ist_sub[["v6"]] <- c(ist_sub[["v6"]],
                     "garante privacy", "garante dati personali", "garante privacy dati personali",
                     "garante nazionale persone detenute private libertà personale",
                     "autorità garante infanzia adolescenza", "autorità garante infanzia",
                     "commissione garanzia diritto sciopero servizi pubblici essenziali", "commissione garanzia attuazione legge sciopero servizi pubblici essenziali")


# Enti a struttura associativa
ist_sub[["v7"]] <- c(ist_sub[["v7"]],
                     "associazione nazionale piccoli comuni", "associazione nazionale piccoli comuni d italia",
                     "acif", "associazione comuni italiani frontiera",
                     "conord")

# Reti e associazioni tematiche di enti istituzionali
ist_sub[["reti_territori"]] <- c(ist_sub[["reti_territori"]],
                                 "cunsf", "conferenza universitaria nazionale scienze formazione",
                                 "crui", "rete nazionale licei musicali coreutici",
                                 "andisu", "asmel", "associazione comuni virtuosi")

# expertise: crui, cunsf, conferenza universitaria nazionale scienze formazione

# Organi collegiali istituzionali
ist_sub[["comit_ist"]] <- c(ist_sub[["comit_ist"]],
                            "cnam", "cun", "cnsu", "consiglio nazionale studenti universitari","consulte provinciali studenti",
                            "cspi", "consiglio superiore pubblica istruzione", "consiglio generale italiani estero",
                            "cncu", "consiglio nazionale consumatori utenti", "consiglio superiore lavori pubblici",
                            "consulta nazionale sicurezza stradale mobilità sostenibile", "osservatorio asse torino lione")

# expertise: cnam, cun, cnsu, consiglio nazionale studenti universitari, consiglio superiore lavori pubblici,
# consulta nazionale sicurezza stradale mobilità sostenibile, osservatorio asse torino lione

# Organismi di raccordo istituzionale
ist_sub[["org_raccordo"]] <- c(ist_sub[["org_raccordo"]], "conper", "conferenza direttori accademie belle arti",
                               "commissione bilaterale italia francia", "consiglio nazionale sistema protezione ambiente")

# expertise: conper, commissione bilaterale italia francia, consiglio nazionale sistema protezione ambiente

# Enti pubblici culturali, ricreativi e sportivi
 
ist_sub[["v8"]] <- c(ist_sub[["v8"]], "corpo nazionale soccorso alpino speleologico", "complesso monumentale pilotta",
                     "gallerie uffizi", "museo real bosco capodimonte", "nado italia", "aeroporto civile statale pavullo")

# expertise: nado, corpo nazionale soccorso alpino speleologico?

# Enti di ricerca e organismi tecnico-scientifici + istituto zooprofilattico sperimentale*
ist_sub[["v9"]] <- c(ist_sub[["v9"]], "icr", "centro nazionale trapianti", "agenzia nazionale nuove tecnologie energia sviluppo economico sostenibile",
                     "istituto zooprofilattico")

# expertise: icr, centro nazionale trapianti, agenzia nazionale nuove tecnologie energia sviluppo economico sostenibile, istituto zooprofilattico


# Enti territoriali e forme associative degli enti locali
ist_sub[["enti_locali"]] <- c(ist_sub[["enti_locali"]], "consorzio comunale caltanissetta")

# Agenzie ed enti strumentali regionali, provinciali e interregionali + autorità di bacino
ist_sub[["v30"]] <- c(ist_sub[["v30"]], "agenzia regionale tecnologia innovazione",
                   "autorità bacino")

# expertise: agenzia regionale tecnologia innovazione, aipo, istituto regionale floricoltura, autorità bacino,
# agenzia provinciale

# Enti e strutture sanitarie, sociosanitarie e assistenziali pubbliche
ist_sub[["v20"]] <- c(ist_sub[["v20"]], ist_sub[["v23"]], ist_sub[["v24"]])

# Autorità di sistema portuale
ist_sub[["v22"]] <- c(ist_sub[["v22"]], "adsp")

# Camere di commercio


# Istituzioni universitarie, scolastiche e formative
ist_sub[["v33"]] <- c(ist_sub[["v33"]], ist_sub[["v36"]], ist_sub[["uni_non_statali"]], # expertise
                      "istituto istruzione secondaria superiore", "istituto tecnico", "liceo", "ipseoa", "itg belzoni", "itis") # no expertise


# Enti nazionali di previdenza e assistenza sociale
ist_sub[["v38"]] <- c(ist_sub[["v38"]], "inarcassa", "eppi")

# Organizzazioni internazionali
ist_sub[["org_int"]] <- c(ist_sub[["org_int"]], "oim", "unfccc", "unesco")

# Forze armate, corpi di sicurezza e strutture della difesa
ist_sub[["corpi_armati"]] <- c(ist_sub[["corpi_armati"]],  "dia", "igesan")

# expertise: igesan

# Fondazioni istituzionali
ist_sub[["fond_ist"]] <- c(ist_sub[["fond_ist"]], "comitato organizzativo milano cortina", "fondazione slala")

# expertise: fondazione slala

# eu
ist_sub[["eu"]] <- c(ist_sub[["eu"]], "acer", "cedefop")

# Istituzioni estere
ist_estere <- c("repubblica san marino", "cantone ticino", "servicio pùblico de empleo estatal sepe")
ist_sub$ist_estere <- ist_estere






# Ruoli istituzionali - commissari* straordinar*
ist_sub[["carica_ist"]] <- setdiff(ist_sub[["carica_ist"]], "commissari* straordinar*")
ist_sub[["carica_ist"]] <- c(ist_sub[["carica_ist"]], "procuratore nazionale antimafia", "questore", "prefetto")

# Gradi militari
ist_sub[["gradi_militari"]] <- c(ist_sub[["gradi_militari"]], "generale divisione aerea")


# Pattern deboli, talvolta eterogenei - sono stati verificati
pattern_deboli <- c("procura", "dogane", "ispettorato lavoro", "mercato fiori",
                    "proc", "procuratore", "on", "commissari* straordinari*")
ist_sub$pattern_deboli <- pattern_deboli


# Elimina v10, v31, v23, v24, v36, v37, uni_non_statali
ist_sub[c("v10", "v31", "v23", "v24", "v36", "v37", "uni_non_statali")] <- NULL




# Aggiungere elementi individuati nella revisione delle macrocategorie non collocati nei sottotipi

# Il calderone "istituzioni" aggiornato non processato è in dict_revised come salvato: l'oggetto ist,
# da cui siamo partiti per il preprocessing, culminato in ist_new

x <- unlist(ist_sub) %>% unique()

setdiff(ist, x) # 164 nuovi elementi da dividere nei sottogruppi

ist_sub[["v8"]] <- c(ist_sub[["v8"]], "museo egizio", "museo glauco lombardi")
ist_sub[["v20"]] <- c(ist_sub[["v20"]], "centro regionale s alessio")
ist_sub[["v4"]] <- c(ist_sub[["v4"]], "accredia")
ist_sub[["fond_ist"]] <- c(ist_sub[["fond_ist"]], "fondazione montagna sicura")
ist_sub[["v9"]] <- c(ist_sub[["v9"]], "stazione zoologica anton dohrn", "garr")

# expertise: accredia, garr, stazione zoologica anton dohrn, fondazione montagna sicura


saveRDS(ist_sub, file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/institutions/institution_list-revised2.RData") # no preprocessing


# Adesso riapplichiamo il dizionario dict_revised con gli aggiornamenti.

# Ripartiamo con il preprocessing di ist_sub

# 1. Staccare gli acronimi 
x <- unlist(ist_sub)
ist_new <- unlist(strsplit(x, " [-–\\(]")) # i segni di punteggiatura sono stati cercati manualmente
# Ripeti 24-46



# Integriamo categoria con i nuovi elementi

# Categoria / Sindacati e tutela collettiva
cat <- c(cat, "sindir vvf", "agdp", "cnu", "comitato nazionale universitario")

# Categoria / reti
cat <- c(cat, "associazione fondazioni its italia", "associazione rete fondazioni its italia", "osservatorio nazionale infrastrutture")

# expertise: osservatorio nazionale infrastrutture


# Categoria / consorzi
cat <- c(cat, "mercato fiori ercolano")



# Integriamo partecipate
part <- c(part, "aeroporto capannori", "mercato fiori sanremo", "sviluppumbria", "astral")




# Civile
dict_revised[["soc_civile"]] <- c(dict_revised[["soc_civile"]], "cng", "consiglio nazionale giovani")


# Privati
dict_revised[["privati"]] <- c(dict_revised[["privati"]], "funivie spa")


# Esperti: procuratore san marco
dict_revised[["esperti"]] <- c(dict_revised[["esperti"]], "procuratore san marco")


# Integriamo Altro
# Altro / società scientifiche, associazioni accademiche e comunità tecnico-epistemiche
dict_revised[["altro"]] <- c(dict_revised[["altro"]], "istituto nazionale urbanistica", "commissione scientifica decommissioning nucleare")

# expertise: istituto nazionale urbanistica, commissione scientifica decommissioning nucleare


# Ripeti 86-88 (aggiorna dizionario) / 100-134 (text analysis)

# label in dataset
senato$LABEL4 <- result$label
data <- senato %>% select(NOMI_clean, LABEL, LABEL2, LABEL3, LABEL4)

table(data$LABEL4)
table(data$LABEL3)



# NEW SESSION

# Setacciamo gli attori LABEL4 = NC e LABEL3 = esperti. In totale 293 su 315.
# Integriamo i volumi del dizionario (ogni volume è una macrocategoria con i propri sottotipi)



# ISTITUZIONI (aggiorniamo lista ist_sub - salvato - ma replicare il preprocessing)

# Aggiorniamo lista ist_sub (già in environment)

# Organi costituzionali, giurisdizionali e di controllo
ist_sub[["v1"]] <- c(ist_sub[["v1"]], "corte d appello", "tribunale", "corte suprema cassazione")

# Ministeri
ist_sub[["v2"]] <- c(ist_sub[["v2"]], "maeci", "dipartimento tesoro", "mef", "mur")
# expertise: dipartimento tesoro

# Autorità indipendenti*
ist_sub[["v6"]] <- c(ist_sub[["v6"]], "unità informazione finanziaria")

# Enti culturali, ricreativi e sportivi
ist_sub[["v8"]] <- c(ist_sub[["v8"]], "archivio centrale stato")
# expertise: archivio centrale stato

# Istituzioni scolastiche, universitarie, formative
ist_sub[["v33"]] <- c(ist_sub[["v33"]], "scuola superiore della magistratura", "scuola specializzazione beni archivistici librari", "istituto professionale statale")
# expertise: scuola superiore della magistratura, scuola specializzazione beni archivistici librari

# Organi collegiali istituzionali
ist_sub[["comit_ist"]] <- c(ist_sub[["comit_ist"]], "cts")
# expertise: cts

# Agenzie ed enti strumentali regionali, provinciali e interregionali
ist_sub[["v30"]] <- c(ist_sub[["v30"]], "arpac")
# expertise: arpac

# Istituzioni eu
ist_sub[["eu"]] <- c(ist_sub[["eu"]], "efsa")
# expertise: efsa

# Ruoli istituzionali
ist_sub[["carica_ist"]] <- c(ist_sub[["carica_ist"]], "prefetto", "inviato speciale")

# Aggiungo nota su sis 118
# Enti e strutture sanitarie, sociosanitarie e assistenziali pubbliche
ist_sub[["v20"]] <- c(ist_sub[["v20"]], "sistema 118 venezia")

# Sovrascrivo il file
saveRDS(ist_sub, file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/institutions/institution_list-revised2.RData")


# CATEGORIA (aggiorniamo direttamente oggetto cat ovvero dict_revised[["categoria"]] aggiornato e pulito).
# Avremmo potuto fare lo stesso con dict_revised[["istituzioni"]] ma avremmo perso gli aggiornamenti dei sottotipi.

# Aggiorniamo oggetto cat

# Non ho una lista con i sottotipi per questa categoria, dovrei recuperare gli appunti negli script.
# Integriamo la voce con i nuovi elementi, riportando i sottogruppi nelle note.

# Sindacati e tutela collettiva
cat <- c(cat, "associazione dottorandi dottori ricerca", "movimento unico dipendenti 118")

# Associazioni datoriali, imprenditoriali e settoriali
cat <- c(cat, "ico", "associazione nazionale enti formazione professionale", "assoimpredia", "associazione agricoltura biodinamica",
         "associazione veneta allevatori ava", "associazione pescatori marittimi professionali")

# Associazioni professionali, ordini e organismi professionali di settore
cat <- c(cat, "ordine avvocati", "consiglio ordine avvocati", "associazione tecnici tutela patrimonio culturale", "libera giovane avvocatura",
         "anfi - associazione nazionale familiaristi italiani", "apri - associazione professionisti risanamento imprese", "aiaf - associazione italiana avvocati famiglia minori", "unaa - unione nazionale avvocati amministrativisti",
         "associazione avvocati amministrativisti liguri", "consiglio nazionale dottori commercialisti", "consiglio nazionale dottori commercialisti dottori commercialisti esperti contabili",
         "consiglio ordine dottori commercialisti esperti contabili", "federazione europea professionisti pedagogia - fepp", "associazione nazionale educatori professionali - anep",
         "numismatici italiani professionisti", "anpri", "associazione italiana professionisti cinofili", "associazione nazionale professionisti verde",
         "aieci", "apnec", "apnocs", "ficsspro", "associazione italiana giardinieri professionisti - aigp", "collegio interprovinciale periti agrari periti agrari laureati",
         "unione professionisti infortunistica stradale - upis", "avvocati giuslavoristi italiani", "associazione italiana medici")

# Reti, alleanze, coordinamenti, movimenti e comitati categoriali

cat <- c(cat, "rete professioni tecniche", "professionisti spettacolo", "professionista beni culturali")
# il nome dell'attore è: mi riconosci? sono un professionista dei beni culturali

# ulteriori modifiche
add <- c("associazione professioni pedagogiche", "reti professioni tecniche")
cat <- c(cat, add)


# SOCIETA' CIVILE (oggetto in dict_revised[["soc_civile"]])

# Non ho una lista con i sottotipi per questa categoria, dovrei recuperare gli appunti negli script.
# Integriamo la voce con i nuovi elementi, riportando i sottogruppi nelle note.

# Legalità, diritti, trasparenza e libertà civili

civile <- "universitari vita"

# Cultura, educazione, scienza pubblica, tempo_libero
civile <- c(civile, "associazione sos archivi", "associazione europea vie francigene", "archeoclub", "gruppi archeologici d italia", "anspi")
# expertise: associazione sos archivi, archeoclub

dict_revised[["soc_civile"]] <- c(dict_revised[["soc_civile"]], civile)
dict_revised[["soc_civile"]] <- setdiff(dict_revised[["soc_civile"]], "universitari per la vita")

dict_revised[["soc_civile"]] <- unique(dict_revised[["soc_civile"]])


# PRIVATI (oggetto in dict_revised[["privati"]])

# Servizi, formazione, certificazione, operatori
pvt <- "weschool"
# expertise: weschool

# Energia, ambiente, infrastrutture, mobilità
pvt <- c(pvt, "kostruttiva")

# Consulenza, ricerca, e servizi professionali
pvt <- c(pvt, "società agrea verona")
# expertise: società agrea verona

# Imprese industriali, agricole, agroalimentari
pvt <- c(pvt, "eurolactis")

# Digitale, telecomunicazioni, piattaforme e tecnologia
ptv <- c(pvt, "inwit")

dict_revised[["privati"]] <- c(dict_revised[["privati"]], pvt)

dict_revised[["privati"]] <- c(dict_revised[["privati"]], "inwit") # ripeto
dict_revised[["privati"]] <- unique(dict_revised[["privati"]])


# ALTRO (oggetto in dict_revised[["altro"]])

# Think tank, centri studi, osservatori e istituti di ricerca non istituzionali*
altro <- c("incer", "osservatorio open data", "centre for european policy sudies", "centro studi rosario livatino")

# Società scientifiche, associazioni accademiche e comunità tecnico-epistemiche*
altro <- c(altro, "sis 118", "card italia", "simfer")

dict_revised[["altro"]] <- c(dict_revised[["altro"]], altro)

dict_revised[["altro"]] <- unique(dict_revised[["altro"]])


# ESPERTI

# Titoli professionali, scientifici o tecnici
esperti <- c("infermiere", "infermiera", "produttore", "epidemiologo", "agronoma")

dict_revised[["esperti"]] <- c(dict_revised[["esperti"]], esperti)

dict_revised[["esperti"]] <- unique(dict_revised[["esperti"]])


# Riprocessiamo istituzioni e categoria.


# Preprocessing di ist_sub

# 1. Staccare gli acronimi 
x <- unlist(ist_sub)
ist_new <- unlist(strsplit(x, " [-–\\(]")) # i segni di punteggiatura sono stati cercati manualmente
# Ripeti 24-46


# Preprocessing di cat
# Separiamo gli acronimi. I nuovi attori sono stati già puliti da stopwords e altri segni di punteggiatura.
x <- cat
x <- unlist(strsplit(x, "-")) # i segni di punteggiatura sono stati cercati manualmente
x <- trimws(x)
cat <- x

# Aggiorna dizionario.
# Le macrocateogie "società civile", "privati", "altro", "esperti" sono stati allargati direttamente.
dict_revised[["istituzioni"]] <- ist_new
dict_revised[["categoria"]] <- cat

dict_revised[["istituzioni"]] <- unique(dict_revised[["istituzioni"]])
dict_revised[["categoria"]] <- unique(dict_revised[["categoria"]])


# Salva dizionario
saveRDS(dict_revised, file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/dict_revised2.RData")


# 100-134 (text analysis)

# label in dataset
senato$LABEL5 <- result$label
data <- senato %>% select(NOMI_clean, LABEL, LABEL2, LABEL3, LABEL4, LABEL5)

table(data$LABEL5)
table(data$LABEL4)
table(data$LABEL3)


# Ora siamo abbastanza sicuri che LABEL5=NC & LABEL=esperti mostri attori senza enti di riferimento,
# al massimo con pattern deboli. In genere, sono così composti:

# Titolo + Nome + Cognome ;
# Titolo + Cognome ;
# Nome + Cognome ;
# Cognome

# A questi attori daremo la qualifica di esperti, sottotipo deboli perché dovrebbero essere guardati uno per uno.
# La scelta è difendibile da un punto di vista teorico (sono auditi per raccolta informazioni) e semantico (non contengono ulteriori info).

esperti_deboli <- data %>% filter(LABEL5=="NC" & LABEL=="esperti") %>% pull(NOMI_clean)
esperti_deboli <- unique(esperti_deboli)


# Tra gli NC, restano da classificare: LABEL non "esperti" e LABEL5 = NC

data %>% filter(LABEL5=="NC" & LABEL!="esperti") #21

# Istituzioni / Enti pubblici culturali, ricreativi e sportivi
diff <- c("gallerie uffizi", "aeroporto civile statale pavullo")
ist_sub[["v8"]] <- setdiff(ist_sub[["v8"]], diff)
ist_sub[["v8"]] <- c(ist_sub[["v8"]], "galleria uffizi")

# Istituzioni / Enti produttori di servizi tecnici ed economici.
ist_sub[["v5"]] <- c(ist_sub[["v5"]], "aeroporto civile statale g paolucci pavullo") 

# Istituzioni / Enti territoriali e forme associative degli enti locali
ist_sub[["enti_locali"]] <- c(ist_sub[["enti_locali"]], "pres giovanni toti") # manca liguria nella stringa che è nel vettore

# Istituzioni / cariche istituzionali
ist_sub[["carica_ist"]] <- c(ist_sub[["carica_ist"]], "commissario straordianario") # typo di commissario straordinario nel vettore

### MANUTENZIONE LISTA ###

# Sovrascrivo il file
saveRDS(ist_sub, file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/institutions/institution_list-revised2.RData")

# Preprocessing di ist_sub

# 1. Staccare gli acronimi 
x <- unlist(ist_sub)
ist_new <- unlist(strsplit(x, " [-–\\(]")) # i segni di punteggiatura sono stati cercati manualmente
# Ripeti 24-46

dict_revised[["istituzioni"]] <- ist_new

### FINE ###


# Esperti / Titoli professionali, scientifici o tecnici*
dict_revised[["esperti"]] <- c(dict_revised[["esperti"]], "ingegner")


# Partecipate / Società strumentali, digitali e di servizi pubblici: openfiber
dict_revised[["partecipate"]] <- c(dict_revised[["partecipate"]], "openfiber")

# Partecipate / Società di trasporto, mobilità, logistica e infrastrutture
dict_revised[["partecipate"]] <- c(dict_revised[["partecipate"]], "gruppo sea")


# Categoria / Reti, alleanze, coordinamenti, movimenti e comitati categoriali
dict_revised[["categoria"]] <- c(dict_revised[["categoria"]], "host+host", "la musica che gira")

# Categoria / Associazioni professionali, ordini e organismi professionali di settore
dict_revised[["categoria"]] <- c(dict_revised[["categoria"]], "associazione a dj", "doc/it")

# Categoria / Associazioni datoriali, imprenditoriali e settoriali
dict_revised[["categoria"]] <- c(dict_revised[["categoria"]], "figc", "federazione italiana sport equestri") # potrebbero esserci altre dello stesso tipo

# Categoria / Consorzi, filiere, distretti e cluster produttivi
dict_revised[["categoria"]] <- c(dict_revised[["categoria"]], "op daunia&bio")


# Altro / Partiti, movimenti politici e gruppi parlamentari
dict_revised[["altro"]] <- c(dict_revised[["altro"]], "liberi uguali")

# Altro / Think tank, centri studi, osservatori e istituti di ricerca non istituzionali
dict_revised[["altro"]] <- c(dict_revised[["altro"]], "endisu")


# Civile
dict_revised[["soc_civile"]] <- setdiff(dict_revised[["soc_civile"]], "registro attori attrici italiani")


# Integrazione esperti deboli
esperti_deboli <- c(esperti_deboli, "nando pagnoncelli", "anna monia alfieri", "occhetta")
# suor e padre sono in neutro / titoli deboli o ambigui, non inclusi nel calcolo della macrocategoria perché falserebbero i risultati.




dict_revised3 <- dict_revised
dict_revised3[["esperti_deboli"]] <- esperti_deboli

# text analysis

dfm_result <- tokens_lookup(tok, dictionary = dict_revised3, nested_scope = "dictionary", exclusive = FALSE) %>% dfm()

result <- as.tibble(dfm_result)

result <- result %>% mutate(length = ntoken(dfm_result),
                            sum = istituzioni + partecipate + categoria + soc_civile + privati + esperti + altro + neutro + esperti_deboli) %>%
  select(istituzioni, partecipate, categoria, soc_civile, privati, esperti, altro, neutro, esperti_deboli, length, sum)

testo_ricomposto <- sapply(tok, function(x) paste(x, collapse = " "))
result$text <- testo_ricomposto


# creo le etichette

macrocategorie <- c("istituzioni", "partecipate", "categoria", "soc_civile", "privati", "esperti", "altro")

etichette <- c("istituzioni", "partecipate", "sindacati e categorie", "società civile", "privati", "esperti", "altro")

result <- result %>%
  rowwise() %>%
  mutate(
    max_val = max(c_across(all_of(macrocategorie)), na.rm = TRUE),
    n_max = sum(c_across(all_of(macrocategorie)) == max_val, na.rm = TRUE),
    
    label = case_when(
      max_val == 0 & esperti_deboli > 0 ~ "esperti_deboli",
      max_val == 0 ~ "NC",
      TRUE ~ paste(
        etichette[c_across(all_of(macrocategorie)) == max_val],
        collapse = " ; "
      )
    )
  ) %>%
  ungroup() %>%
  select(
    istituzioni, partecipate, categoria, soc_civile, privati,
    esperti, altro, neutro, length, sum, text, label
  )

# abbiamo fatto sparire n_max, altrimenti...
# pareggi <- result %>%
# filter(n_max > 1) %>% count(LABEL6, sort = TRUE)

# label in dataset
senato$LABEL6 <- result$label
data <- senato %>% select(NOMI_clean, LABEL, LABEL2, LABEL3, LABEL4, LABEL5, LABEL6)

table(data$LABEL6)


pareggi <- data %>% filter(grepl(" ; ", LABEL6))

# Se i professori universitari sono in esperti per continuità di scelta con lo studio pilota,
# filtriamo le varianti del pattern "prof" insieme alla parola chiave "università"

prof_pattern <- "\\bprof\\b|\\bprofssa\\b|\\bprofessor"

pareggi <- pareggi %>%
  mutate(
    LABEL_win = ifelse(
      str_detect(NOMI_clean, regex(prof_pattern)) &
                   str_detect(NOMI_clean, "università"),
                 "esperti",
                 LABEL6
    )
  )


# Ci sono altre combinazioni con prof

x <- pareggi %>% filter(str_detect(NOMI_clean, prof_pattern) & LABEL_win != "esperti") %>%
  select(NOMI_clean, LABEL6)

# pareggi istituzioni ; esperti (residui) = istituzioni

x$LABEL_win <- ifelse(
  grepl("istituzioni", x$LABEL6),
  "istituzioni",
  x$LABEL6)

# eccezione prof giulio enea vigevani

x$LABEL_win <- ifelse(x$NOMI_clean=="prof giulio enea vigevani", "esperti",
                      x$LABEL_win)

x %>% filter(LABEL_win != "istituzioni")

# 4 pareggi tra categoria ; esperti = 3 categoria (1 errore)
# 2 pareggi tra società civile ; esperti = civile (1 dubbio)
# 1 pareggio tra altro ; esperti = esperti
# 1 pareggio tra esperti ; partecipate = partecipate

# la stringa 6 va splittata perché contiene due attori (riga 6440 nel dataset)

# aiutiamoci con gli indici (metodo rischioso ci si può confondere)
x %>%
  mutate(row_id = row_number()) %>%
  filter(LABEL_win != "istituzioni") %>%
  select(row_id, NOMI_clean, LABEL_win)

x$LABEL_win[c(3,4,31)] <- "sindacati e categorie"
x$LABEL_win[29] <- "società civile"
x$LABEL_win[10] <- "partecipate"


# Riportiamo i risultati in pareggi

# Prima togliamo i duplicati in x
x <- distinct(x)

pareggi <- pareggi %>%
  left_join(
    x %>% select(NOMI_clean, LABEL_win_x = LABEL_win),
    by = "NOMI_clean"
  ) %>%
  mutate(
    LABEL_win2 = if_else(!is.na(LABEL_win_x), LABEL_win_x, LABEL_win)
  ) %>%
  select(-LABEL_win_x)


# Prendiamo il pattern spa per individuare le imprese in pareggio
x <- pareggi %>% filter(grepl("\\bspa\\b", NOMI_clean)) %>% select(NOMI_clean, LABEL_win2)
x <- distinct(x)

# Su 21 risultati, cerchiamo i pareggi tra partecipate e privati

x <- x %>%
  mutate(
    LABEL_win2 = ifelse(
      LABEL_win2=="partecipate ; privati", "partecipate",
      LABEL_win2))

# la riga 2785 va splittata perché contiene due attori
# togliere acronimo "arte" da pattern categoria perché fuorviante in alcuni casi

x[6,2] <- "partecipate" # arte falsifica
x[c(7,10),2] <- "istituzioni" # commissario straordinario prevale su società privata

pareggi <- pareggi %>%
  left_join(
    x %>% select(NOMI_clean, LABEL_win2_x = LABEL_win2),
    by = "NOMI_clean"
  ) %>%
  mutate(
    LABEL_win2 = if_else(!is.na(LABEL_win2_x), LABEL_win2_x, LABEL_win2)
  ) %>%
  select(-LABEL_win2_x)

# I gruppi più numerosi da smaltire sono:
# istituzioni ; sindacati e categorie (42)
# istituzioni ; partecipate (14)
# istituzioni ; esperti (13)
# sindacati e categorie ; società civile (9)

x <- pareggi %>% filter(LABEL_win2=="istituzioni ; sindacati e categorie") %>% select(NOMI_clean) %>% distinct()

# togliere pattern c("aicc", "siae") da istituzioni
# togliere pattern c("and", ico") da categoria

# categoria / settore: "siae"
# categoria / sindacati: "uspp", "unione sindacati polizia penitenziaria"
# istituzioni / org_raccordo: "confederazione regioni province autonome"
# istituzioni / pattern debole: ico

x$LABEL_win2[c(1,2,5,7,8,11:37)] <- "sindacati e categorie"
# expertise: centro studi cna marche

x$LABEL_win2[c(4,6,9,10)] <- "istituzioni"

# replica join sui pareggi 856-864


# istituzioni ; partecipate (14)
x <- pareggi %>% filter(LABEL_win2=="istituzioni ; partecipate") %>% select(NOMI_clean) %>% distinct()

# togliere anpal da partecipate
# responsabile struttura territoriale (ruolo dirigenziale) + regione + anas (partecipata) = partecipata

# soggetto attuatore anas (partecipata) vs commissario straordinario anas (istituzione)
# funzione esercitata attraverso l'ente vs funzione di pubblico ufficiale

x$LABEL_win2[c(1,7:9)] <- "istituzioni"
x$LABEL_win2[c(2:6)] <- "partecipate"

# replica join sui pareggi 856-864


# istituzioni ; esperti (13)
x <- pareggi %>% filter(LABEL_win2=="istituzioni ; esperti") %>% select(NOMI_clean) %>% distinct()

x$LABEL_win2[c(1,4,6,13)] <- "istituzioni"
x$LABEL_win2[c(2,5)] <- "sindacati e categorie"
x$LABEL_win2[c(3,7:10,11,12)] <- "esperto"

# expertise: incrociamo pareggi con il listone di enti che portano expertise per assicurarci di individuare parti di stringhe ("anac" in "anac avv busia")

# replica join sui pareggi 856-864


# sindacati e categorie ; società civile (9)
x <- pareggi %>% filter(LABEL_win2=="sindacati e categorie ; società civile") %>% select(NOMI_clean) %>% distinct()

# via da civile: associazione italiana celiachia aic, aic, coordinamento libere associazioni professionali, coordinamento libere associazioni professionali colap, federazione italiana uso razionale energia
# aggiungi: associazione italiana celiachia, transport and environment

# via da categoria: una, act italia
# aggiungi: federazione italiana uso razionale energia*

x$LABEL_win2[c(1,6,7)] <- "società civile"
x$LABEL_win2[c(3,4,8)] <- "sindacati e categorie"

# replica join sui pareggi 856-864


# mancano gli ultimi 30 attori!

x <- pareggi %>% select(NOMI_clean, LABEL_win2) %>% filter(str_detect(LABEL_win2, ";")) %>% arrange(LABEL_win2) %>% distinct()

# carica professionale + carica organizzativa + ente : label ente + expertise.
x$LABEL_win2[c(2)] <- "altro" #expertise
x$LABEL_win2[c(16,17)] <- "partecipate"
x$LABEL_win2[c(19,20)] <- "sindacati e categorie"
x$LABEL_win2[c(27)] <- "società civile"


# amb non sempre pattern istituzionale (può riferirsi al passato - uso improprio come dott)
x$LABEL_win2[c(4)] <- "istituzioni" # amb + stato
x$LABEL_win2[c(3,6)] <- "altro" # amb + ente = label ente + expertise


x$LABEL_win2[c(1)] <- "altro"
x$LABEL_win2[c(5)] <- "altro" #expertise
x$LABEL_win2[c(11)] <- "istituzioni"
x$LABEL_win2[c(13,15)] <- "società civile"
x$LABEL_win2[c(22,23,24)] <- "sindacati e categorie" # tavolo autoconsumo (expertise)

# 10,21,25,26 to split
# 14 doc co-firmati
# 12 to remove

# togliere da privati: federmatrimoni eventi privati, associazione enti previdenziali privati
# togliere da categoria: aogoi
# aggiungi aogoi a società scientifiche

# risoluzione d'ufficio
x$LABEL_win2[c(28)] <- "esperti"
# carica ente 1 + carica ente 2: affiliazione professionale vs ruolo rappresentativo in organizzazione : prevale la rappresentanza per misurare la constituency
x$LABEL_win2[c(7,8,9)] <- "altro" # expertise: società scientifiche, centro studi

# replica join sui pareggi 856-864


# Salviamo la tabella dei pareggi
pareggi <- distinct(pareggi)
pareggi %>% select(NOMI_clean, LABEL, LABEL6, LABEL_win2) %>%
  write.csv("C:/Users/SImone/Desktop/audizioni_informali/data/senato/pareggi.csv", row.names = FALSE)


# Salviamo versione dizionario prima di risoluzione pareggi
saveRDS(dict_revised3, file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/dict_revised3.RData")

# riportiamo le label di pareggi in senato
senato$LABEL_win <- senato$LABEL6
senato2 <- senato %>%
  left_join(
    pareggi %>% select(NOMI_clean, LABEL_win2_x = LABEL_win2),
    by = "NOMI_clean"
  ) %>%
  mutate(
    LABEL_win2 = if_else(!is.na(LABEL_win2_x), LABEL_win2_x, LABEL_win)
  ) %>%
  select(-LABEL_win2_x)

table(senato2$LABEL_win2)

# salva il dataset con tutte le classificazioni
write.csv(senato2, "C:/Users/SImone/Desktop/audizioni_informali/data/senato/senato_label2.csv", row.names = FALSE)

# Registriamo le ultime modifiche al dizionario (quelle relative ai pareggi) in una nuova versione (finale)

ist_sub[["reti_territori"]] <- setdiff(ist_sub[["reti_territori"]], "aicc")
ist_sub[["v8"]] <- setdiff(ist_sub[["v8"]], "fondazione ottavio ziino orchestra di roma e del lazio")
ist_sub[["v5"]] <- setdiff(ist_sub[["v5"]], "siae")
ist_sub[["pattern_deboli"]] <- setdiff(ist_sub[["pattern_deboli"]], "commissari* straordinari*")

ist_sub[["pattern_deboli"]] <- c(ist_sub[["pattern_deboli"]], "ico")
ist_sub[["org_raccordo"]] <- c(ist_sub[["org_raccordo"]], "confederazione regioni province autonome")

saveRDS(ist_sub, file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/institutions/institution_list-revised3.RData") # no preprocessing


# Adesso riapplichiamo il dizionario dict_revised con gli aggiornamenti.

# Ripartiamo con il preprocessing di ist_sub

# 1. Staccare gli acronimi 
x <- unlist(ist_sub)
ist_new <- unlist(strsplit(x, " [-–\\(]")) # i segni di punteggiatura sono stati cercati manualmente
# Ripeti 24-46

dict_revised3[["istituzioni"]] <- ist_new

rmv <- c("and", "ico", "una", "act italia", "aogoi")
dict_revised3[["categoria"]] <- setdiff(dict_revised3[["categoria"]], rmv)
dict_revised3[["categoria"]] <- c(dict_revised3[["categoria"]], "siae", "uspp", "unione sindacati polizia penitenziaria", "federazione italiana uso razionale energia")

dict_revised3[["partecipate"]] <- setdiff(dict_revised3[["partecipate"]], "anpal")

rmv <- c("associazione italiana celiachia aic", "aic", "coordinamento libere associazioni professionali", "coordinamento libere associazioni professionali colap", "federazione italiana uso razionale energia")
dict_revised3[["soc_civile"]] <- setdiff(dict_revised3[["soc_civile"]], rmv)
dict_revised3[["soc_civile"]] <- c(dict_revised3[["soc_civile"]], "associazione italiana celiachia", "transport and environment")

rmv <- c("federmatrimoni eventi privati", "associazione enti previdenziali privati")
dict_revised3[["privati"]] <- setdiff(dict_revised3[["privati"]], rmv)

dict_revised3[["altro"]] <- c(dict_revised3[["altro"]], "aogoi")

# salviamo la versione aggiornata del dizionario
saveRDS(dict_revised3, file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/dict_revised4.RData")



# APS è pattern ambiguo. Nelle occorrenze indica associazione promozione sociale.
# In una è aconimo di LIBERA ASSOCIAZIONE PROFESSIONALE LAVORATORI SPETTACOLO - APS.
# Andremo a rinominare manualmente la stringa e cattureremo la denominazione estesa.
