library(ggplot2)

# Manutenzione dataset

# Obiettivi:
# 1. Correggere gli errori (battitura - typos), ambiguità (acronimi uguali per enti diversi).
# 2. Eliminare righe superflue
# 3. Standardizzare la nomenclacura dei singoli stakeholders (abbreviazioni, acronimo-nome esteso)

# Gli errori e le ambiguità sono state appuntate strada facendo nella classificazione.

# Disclaimer!
# Dopo la correzione, bisogna ripetere le operazioni di pulizia dei nomi.

# Strategia in aiuto per punto 3: metodo del coseno (rinviata)


senato <- read.csv("C:/Users/SImone/Desktop/audizioni_informali/data/senato/clean_data/dataset_senato.csv")


senato[5824,2] <- "associazione italiana diabetici" # rimosso acronimo in comune
senato[546,2] <- "tavolo permanente delle federazioni bandistiche italiane"
senato[4740,2] <- "associazioni dei consumatori ; altroconsumo ; (unc) unione nazionale consumatori ; associazione mo bast!"
senato[5367,2] <- "transport & environment"
senato[7004,2] <- "associazione italiana promozione scienza aperta"
senato[732,2] <- "associazione italiana promozione scienza aperta"
senato[43,2] <- "associazione docenti e dirigenti scolastici italiani"
senato[1045,2] <- "assemblee di dio in italia"
senato[237,2] <- "actionaid - ondata - transparency international italia"
senato[82,2] <- "fondazione promozione sociale onlus"
senato[2691,2] <- "comitato apprendimento pratico musica studenti (cnapm) ; conferenza direttori conservatori musica"
senato[5719,2] <- "legambiente - pro natura - comitato vigilanza sul nucleare del piemonte"
senato[7085,2] <- "associazione air italia agenti immobiliari riuniti"
senato[2967,2] <- "associazione produttori esecutivi"
senato[885,2] <- "(angdp) associazione nazionale giudici di pace ; (unagipa) unione nazionale giudici di pace"
senato[1442,2] <- "(arti) agenzia regionale tecnologia e innovazione"
senato[6302,2] <- "associazione medici diabetologi (amd) - società italiana diabetologia (sid)" #congiunta
senato[39,2] <- "associazione nazionale docenti (and)"
senato[3266,2] <- "associazione nazionale esercenti multiplex (anem) ; federazione carta grafica"
senato[4637,2] <- "nuova associazione bieticoltori italiani"
senato[3009,2] <- "associazione reti teatrali italiane (arti)"
senato[2749,2] <- "consiglio universitario nazionale (cun) ; consiglio nazionale studenti universitari (cnsu)"
senato[4480,2] <- "assoferilizzanti - federchimica"
senato[1139,2] <- "centro studi internazionali"
senato[3577,2] <- "presidente commissione tecnica tranvie e sistemi ferroviari istituita presso l'ordine degli ingegneri della provincia di roma"
senato[4640,2] <- "confederazione agromeccanici e agricoltori italiani"
senato[2035,2] <- "nestlé italiana spa"
senato[4609,2] <- "federazione apicoltori italiani"
senato[6731,2] <- "fondo ambiente italiano - inu - slow food italia - wwf"
senato[2576,2] <- "conferenza delle regioni e delle province autonome"
senato[4049,2] <- "conferenza delle regioni e delle province autonome"
senato[1149,2] <- "alessandro orsini direttore osservatorio sulla sicurezza internazionale luiss"
senato[2114,2] <- "fabrizia lapecorella direttore dipartimento finanze"
senato[1034,2] <- "maria luisa stasi, article 19"
senato[900,2] <- "avv maria sabina lembo"
senato[2874,2] <- "associazione a dj"
senato[1148,2] <- "comitato di collegamento di cattolici per una civiltà dell'amore onlus"
senato[2931,2] <- "lavoratori della danza"
senato[3092,2] <- "suor anna monia alfieri, il diritto di apprendere"
senato[2974,2] <- "giffoni film festival"
senato[5392,2] <- "(anbba) associazione nazionale bed & breakfast affittacamere case per vacanza locazioni turistiche"
senato[5462,2] <- "(anbba) associazione nazionale bed & breakfast affittacamere case per vacanza locazioni turistiche"
senato[6591,2] <- "consult@noi"
senato[6700,2] <- "rén collective"
senato[4761,2] <- "free2move esolutions"
senato[5857,2] <- "direttore generale della direzione generale dei rapporti lavoro delle relazioni industriali del ministero del lavoro e delle politiche sociali"
senato[4483,2] <- "coldiretti - fondo ambiente italiano - inu - legambiente - lipu - slow food italia - touring club italiano - wwf"
senato[7020,2] <- "100 autori"
senato[3286,2] <- "associazione professioni pedagogiche app ; associazione pedagogisti educatori italiani apei"
senato[29,2] <- "cgil"
senato[1059,2] <- "arca 2000"
senato[1266,2] <- "alleanza cooperative italiane"
senato[1296,2] <- "confindustria alberghi"
senato[1450,2] <- "marco esposito"
senato[1488,2] <- "confindustria alberghi"
senato[1766,2] <- "confagricoltura"
senato[2014,2] <- "assoviaggi confesercenti"
senato[1821,2] <- "confesercenti"
senato[1879,2] <- "anitrav"
senato[2291,2] <- "confindustria servizi hcfs"
senato[2748,2] <- "conferenza direttori conservatori musica"
senato[2860,2] <- "coopculture"
senato[2872,2] <- "parchi permanenti italiani"
senato[2873,2] <- "movimento facciamo la conta"
senato[3001,2] <- "coordinamento la musica che gira"
senato[3369,2] <- "alleanza cooperative italiane"
senato[3634,2] <- "ministro innovazione tecnologica transizione digitale vittorio colao"
senato[3637,2] <- "trasporto unito"
senato[2970,2] <- "ifc italian film commission"
senato[3362,2] <- "federazione italiana sport equestri"
senato[4147,2] <- "unci agroalimentare"
senato[4813,2] <- "movimento nazionale liberi farmacisti federazione nazionale parafarmacie italiane federazione farmacisti disabilità onlus confederazione unitaria libere parafarmacie italiane"
senato[5093,2] <- "conferenza regioni province autonome"
senato[5218,2] <- "alleanza fotovoltaico italia"
senato[5458,2] <- "base balneare donnedamare"
senato[6759,2] <- "associazione italiana compostaggio"
senato[6786,2] <- "re te imprese italia"
senato[6837,2] <- "confartigianato"
senato[7024,2] <- "agcom"
senato[7027,2] <- "iliad"
senato[7028,2] <- "cfwa coalizione fixed wireless access"
senato[3217,2] <- "professor luca serianni"
senato[4484,2] <- "fondo italiano ambiente"
senato[6988,2] <- "tavolo autoconsumo efficienza energetica"
# Rimozione righe
senato <- senato[-c(4580,3578,4484:4490,5393:5395,5463:5465,5119,7003,5858), ]
rownames(senato) <- NULL

senato <- senato %>% separate_rows(NOMI, sep = ";")

# Rimuoviamo gli altri segni di punteggiatura

senato$NOMI_clean <- gsub("\\.", "", senato$NOMI)
senato$NOMI_clean <- gsub("[,;'\"()\\-]", " ", senato$NOMI_clean) 
senato$NOMI_clean <- gsub("[/’_]", " ", senato$NOMI_clean) # aggiunto

senato$NOMI_clean <- gsub("\\s+", " ", senato$NOMI_clean) # Rimuovi spazi multipli


# Remove stopwords
stp_wrd <- stopwords('it')
stp_wrd <- setdiff(stp_wrd, c("una", "nostra"))

# Funzione di pulizia
pulisci_testo <- function(testo) {
  for (i in stp_wrd) {
    testo <- gsub(paste0("\\b", i, "\\b"), "", testo)
  }
  testo <- trimws(gsub("\\s+", " ", testo))  # Rimuove spazi multipli e all'inizio/fine della stringa
  return(testo)
}

senato$NOMI_clean <- pulisci_testo(senato$NOMI_clean)
senato <- unique(senato)

senato <- as.data.frame(senato)

senato[6579,5] <- "consult@noi"
senato[2960,5] <- "doc/it"
senato[746,5] <- "di donna"
senato[773,5] <- "siamo così"
senato[982,5] <- "la malfa"
senato[255,5] <- "lorenzo calò il mattino"
senato[3344,5] <- "movimento facciamo la conta"
senato[2875,5] <- "movimento facciamo la conta"
senato[2876,5] <- "associazione a dj"
senato[2985,5] <- "associazione i jazz"
senato[3331,5] <- "associazione i jazz"
senato[2990,5] <- "movimento spettacolo vivo" #
senato[3003,5] <- "coordinamento la musica che gira"
senato[3479,5] <- "coordinamento la musica che gira"
senato[5805,5] <- "io sono oss"

# salvo il dataset corretto
write.csv(senato, "C:/Users/SImone/Desktop/audizioni_informali/data/senato/senato_fix.csv", row.names = FALSE)




# combino i volumi del dizionario
dizionario_babele <- readRDS(file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/dizionario_babele.RData")
dizionario_pt1 <- readRDS(file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/dizionario_pt1.RData")
dict10 <- readRDS(file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/dict10.RData")

dizionario <- c(dizionario_pt1, dizionario_babele, dict10)


# text analysis

corpus <- corpus(senato, text_field = "NOMI_clean")
tok <- tokens(corpus)

dfm_result <- tokens_lookup(tok, dictionary = dizionario, nested_scope = "dictionary", exclusive = FALSE) %>% dfm()

result <- as.tibble(dfm_result)

result <- result %>% mutate(length = ntoken(dfm_result),
                            sum = privati+istituzioni+soc_civile+rappr+centr_ricerca+altro+esperti+part_statali) %>%
  select(privati, istituzioni, soc_civile, rappr, centr_ricerca, altro, esperti, part_statali, length, sum)

testo_ricomposto <- sapply(tok, function(x) paste(x, collapse = " "))
result$text <- testo_ricomposto


# creo le etichette

result <- result %>%
  rowwise() %>%
  mutate(
    values = list(c(istituzioni, part_statali, rappr, soc_civile, esperti, centr_ricerca, privati, altro)),
    labels = list(c("istituzioni", "partecipate", "sindacati_e_organiz_di_rappr", "società_civile", "esperti", "centri_ricerca", "privati", "altro")),
    
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
  select(istituzioni, part_statali, rappr, soc_civile, esperti, centr_ricerca, privati, altro, length, sum, text, label) 


# creo il grafico

ggplot(result, aes(label)) +
  geom_bar()


ggplot(result) +
  geom_bar(aes(y = label)) +
  labs(y = "9 categorie", x = "Conta degli stakeholders n°(3498)",
       title = "Distribuzione degli stakeholders nelle categorie",
       subtitle = "Audizioni informali del Senato - XVIII legislatura (n°=7076)") +
  theme(
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12))


# stats
table(result$label)
