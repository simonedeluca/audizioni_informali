library(tidyverse)
library(quanteda)

senato <- read.csv("C:/Users/SImone/Desktop/audizioni_informali/data/senato/clean_data/dataset_senato.csv")

# Correggiamo i dati inesatti
# Gli errori e le ambiguità sono state appuntate strada facendo nella classificazione.

# Disclaimer!
# Dopo la correzione, bisogna ripetere le operazioni di pulizia dei nomi.

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
senato[900,2] <- "avv Maria Sabina Lembo"
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

# Rimozione righe
senato <- senato[-c(4580,3578,3729,4484,7020,5393:5395,5463:5465,5857,5858), ]

# non salviamo le modifiche sul dataset ora!!! si rimanda allo script -grafico.

# La punteggiatura porta significato.

# Trattino "-" sta per audizione congiunta, conta per un'audizione.
# Punto e virgola ";" indica diverse audizioni. Split the strings.
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


# crea il corpus, token

# applica il dizionario: dizionario_pt1 e dizionario_babele
dizionario_babele <- readRDS(file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/dizionario_babele.RData")
dizionario_pt1 <- readRDS(file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/dizionario_pt1.RData")

dizionario <- c(dizionario_pt1, dizionario_babele)

corpus <- corpus(senato, text_field = "NOMI_clean")
tok <- tokens(corpus)

dfm_result <- tokens_lookup(tok, dictionary = dizionario, nested_scope = "dictionary", exclusive = FALSE) %>% dfm()

result <- as.tibble(dfm_result)

result <- result %>% mutate(length = ntoken(dfm_result),
                            sum = privati+istituzioni+soc_civile+rappr+centr_ricerca+altro+esperti+part_statali) %>%
  select(privati, istituzioni, soc_civile, rappr, centr_ricerca, altro, esperti, part_statali, length, sum)

testo_ricomposto <- sapply(tok, function(x) paste(x, collapse = " "))
result$text <- testo_ricomposto


babele <- result %>% filter(sum==0) %>% select(text) %>% unique() # 384 enti da classificare - nuovo obiettivo 200!


first_line <- babele$text[1:84] %>% unname()

istituzioni <- first_line[c(1,4)]
rappr <- first_line[c(2,3,6,7,8,12,13,15,16,21,22,23,24,25:30,34,35,36,39,41,48,49,50,67,68,69,72,76)]
privati <- first_line[c(9,19,42,51)]
civile <- first_line[c(10,14,17,18,32,37,38,40,44,46,47,55,66,84)]
centr_ricerca <- first_line[c(11,45)]
partecipate <- first_line[c(20,33,43)]
esperto <- first_line[c(31,52,53,54,56:65,70,71,73,75,78:83)] # di donna
media <- first_line[74]

esperto <- c(esperto, "di donna")
# convol (36), csvnet (39) rappr o civile?

# Ci restano 3 linee da 100 per prendere la città!

cavalieri <- babele$text[85:184] %>% unname()

altro <- cavalieri[c(1,19,70,71)]
esperto <- c(esperto, cavalieri[c(2,9,10,14:16,18)], "la malfa", "marco esposito")
civile <- c(civile, cavalieri[c(11,12,47,49,53,96,99)], "siamo così", "arca 2000", "facciamo la conta")
rappr <- c(rappr, cavalieri[c(4,5,6,7,8,13,26,28,29,30,36:45,48,50,51,54,59,61,62,64,66,69,72,73,75,79,91,100)])
centr_ricerca <- c(centr_ricerca, cavalieri[c(21,22,32,63)])
istituzioni <- c(istituzioni, cavalieri[c(23,31,33,57,76,77,78,82,83,84)])
privati <- c(privati, cavalieri[c(25,55,60,67,68,74,86,95,97,98)])
think_tank <- cavalieri[c(46)]
media <- c(media, cavalieri[c(80,81,92,93,94)])

# link2007 (30), csvnet, convol civile o rappresentanza? 

# correzioni
# siae in istituzioni; imaie in categoria o civile, soundreef e itsright in privati o categoria?
# forum (nazionale) terzo settore civile o rappresentanza?

# prof* prende anche i professionisti

giganti <- babele$text[185:284] %>% unname()

# aps - libera associazione professionale lavoratori spettacolo

rappr <- c(rappr, giganti[c(1,2,3,6,7,8,9,11,14,15,16,18,20,24,30,31,35,37,42,43,44,46,50,55,57,58,60,
                   61,65,68,71,73,74,79,89,97)], "i jazz", "la musica che gira" , "fise assoambiente")
centr_ricerca <- c(centr_ricerca, giganti[c(4,23,54)])
civile <- c(civile, giganti[c(5,21,28,34,63,66,67,70,72,77,78)])
media <- c(media, giganti[c(12,13,27,51,52,53)])
privati <- c(privati, giganti[c(26,38,39,36,45,48.49,64,69,80,83,84,85,86,91,92,93,94,95,96,98,100)]) # lac: scuola cucina etnica
istituzioni <- c(istituzioni, giganti[c(32,33,47)]) # scuola superiore
partecipate <- c(partecipate, giganti[c(82)])

# m° - maestro
# 3534-3540 un'audizione, sette co-firmatari di memoria
# rimuovi da rappr: coordinamento musica gira, italian film commission

# le federazioni sportive nazionali FSN aderenti al CONI - istituzioni
# fisi - federazione italiana sport invernali
# fisip - federazione italiana sport invernali paralimpici
# fin - federnuoto - federazione italiana nuoto
# federvolley - federazione italiana pallavolo
# federazione italiana rugby
# fiv - federazione italiana vela
# federazione italiana sport equestri
# fipsas - federazione italiana pesca sportiva attività subacquee


# Categoria (associazioni di rappresentanza, non FSN):
# federazione italiana caccia
# fiops - federazione italiana operatori pesca sportiva

# arci pesca fisa - federazione italiana sport ambiente (civile)
# arcicaccia

# amodo - alleanza mobilità dolce, tokyo club - rappresentanza in appr. funzionale, civile in giuridico

# enav - partecipata statale ; ispra - ente pubblico di ricerca 
# enama 76 e accredia 59 privati con funzione pubblica


titani <- babele$text[285:384] %>% unname()
# equo garantito è categoria, ma avevo scelto civile.

rappr <- c(rappr, titani[c(1,3,7,8,9,10,12,13,14,17,22,23,24,26,40,41,43,44,46,48,49,53,55,61,71,72,73,76,86,91,92,96,98,99,100)], "io sono oss")
civile <- c(civile, titani[c(2,5,16,19,20,21,28,29,30,31,32,33,34,35,36,37,38,39,42,45,47,50,52,54,56,57,58,59,60,62,64,65,74,78,82,89,90)])
privati <- c(privati, titani[c(4,27,51,67,68,69,70,75,79,81,83,85,97)])
istituzioni <- c(istituzioni, titani[c(6,18)], "agcom")
altro <- c(altro, titani[c(25)])
esperto <- c(esperto, titani[c(66)])

# FUNZIONALE
# 5 in privati
# 6,16,37,50,52,54,57,60,64,78 in centri di ricerca
# 25 in chiesa
# 42,45,68,69,70,97 in categoria


# si può creare un vettore sindacati per distinguerli dalle org di categoria
# consorzio parmigiano privato-categoria

rappr <- c(rappr, "unacma", "goal")
istituzioni <- c(istituzioni, "federazione italiana sport equestri", "conord") # categoria in funzionale
privati <- c(privati, "silt", "accredia", "enama", "mfsd") # accredia ed enama categoria in funzionale
civile <- c(civile, "associazione italiana compostaggio") # categoria in funzionale


dict10 <- dictionary(list(privati=privati,
                          istituzioni=istituzioni,
                          soc_civile=civile,
                          rappr=rappr,
                          centr_ricerca=c(centr_ricerca, media, think_tank),
                          altro=altro,
                          esperti=esperto,
                          part_statali=partecipate))


saveRDS(dict10, file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/dict10.RData")
