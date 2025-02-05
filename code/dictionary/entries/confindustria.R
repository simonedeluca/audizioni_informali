
# Scraping della galassia Confindustria

# Organizziamo le informazioni disponibili sul sito di Confindustria in tabelle
# di 4 variabili:

# nome: contiene il nome dell'ente, spesso breve o acronimo;
# nome esteso: contiene il nome per esteso dell'ente;
# tipologia: classificazione dell'ente nella galassia Confindustria;
# ambito di interesse: settore di cui l'ente si occupa.

# Ci sono 8 sezioni. Salviamo a parte le sezioni con le sottoliste per consentire
# la riproduzione delle relazioni tra enti "genitori" ed enti "figli" in fase di
# pulizia.


# 0. Packages

library(tidyverse)
library(rvest)


# 1. Rappresentanze settore

urls <- "https://www.confindustria.it/home/chi-siamo/sistema-confindustria/rappresentanze-settore#scrollAnchor" %>%
  read_html() %>%
  html_elements(".associazioni li a") %>%
  html_attr("href")

# elimina le stringhe che iniziano per #
urls <- urls[!grepl("^#", urls)]

# forma i link
urls <- paste("https://www.confindustria.it/", urls, sep = "")

results <- list()
for (i in urls) {
  page <- read_html(i)
  nome <- page %>% html_elements(".section-title h2") %>% html_text2()
  nome_esteso <- page %>% html_elements(".section-title h3") %>% html_text2()
  tipologia <- page %>% html_element(".col-lg-8 p") %>% html_text2()
  ambito_interesse <- page %>% html_elements(".bordbot:nth-child(2) .col-lg-8 p") %>% html_text2()
  
  page_tibble <- tibble(nome, nome_esteso, tipologia, ambito_interesse)
  results[[i]] <- page_tibble
  
  print(paste("Page: ", which(urls==i)))
  Sys.sleep(3)
}

rappr_settore <- bind_rows(results)


# 2. Federazioni di settore

urls <- "https://www.confindustria.it/home/chi-siamo/sistema-confindustria/federazioni-settore#scrollAnchor" %>%
  read_html() %>%
  html_elements(".associazioni li a") %>%
  html_attr("href")

# elimina le stringhe che iniziano per #
urls <- urls[!grepl("^#", urls)]

# forma i link
urls <- paste("https://www.confindustria.it/", urls, sep = "")

results <- list()

for (i in urls) {
  page <- read_html(i)
  nome <- page %>% html_elements(".section-title h2") %>% html_text2()
  nome_esteso <- page %>% html_elements(".section-title h3") %>% html_text2()
  tipologia <- page %>% html_element(".col-lg-8 p") %>% html_text2()
  ambito_interesse <- page %>% html_elements(".bordbot:nth-child(2) .col-lg-8 p") %>% html_text2()
  
  page_tibble <- tibble(nome, nome_esteso, tipologia, ambito_interesse)
  results[[i]] <- page_tibble
  
  print(paste("Page: ", which(urls==i)))
  Sys.sleep(3)
}

feder_settore <- bind_rows(results)
feder_settore <- unique(feder_settore)


# 3. Rappresentanze evolute

urls <- "https://www.confindustria.it/home/chi-siamo/sistema-confindustria/rappresentanze-evolute#scrollAnchor" %>%
  read_html() %>%
  html_elements(".associazioni li a") %>%
  html_attr("href")

# elimina le stringhe che iniziano per #
urls <- urls[!grepl("^#", urls)]

# forma i link
urls <- paste("https://www.confindustria.it/", urls, sep = "")

results <- list()

for (i in urls) {
  page <- read_html(i)
  nome <- page %>% html_elements(".section-title h2") %>% html_text2()
  nome_esteso <- page %>% html_elements(".section-title h3") %>% html_text2()
  tipologia <- page %>% html_element(".col-lg-8 p") %>% html_text2()
  ambito_interesse <- page %>% html_elements(".bordbot:nth-child(2) .col-lg-8 p") %>% html_text2()
  
  page_tibble <- tibble(nome, nome_esteso, tipologia, ambito_interesse)
  results[[i]] <- page_tibble
  
  print(paste("Page: ", which(urls==i)))
  Sys.sleep(3)
}

rappr_evolute <- bind_rows(results)


# 4. Sezioni senza sottoliste

chunk <- c("rappresentanze-regionali","associazioni-territorio", "associazioni-settore","associati-aggregati", "rappresentanze-internazionali")

link <- paste("https://www.confindustria.it/home/chi-siamo/sistema-confindustria/", chunk,"#scrollAnchor", sep="")

results <- list()

for (i in link) {
  urls <- read_html(i) %>%
    html_elements(".associazioni li a") %>%
    html_attr("href")
  
  urls <- paste("https://www.confindustria.it/", urls, sep = "")
  
  for (z in urls) {
    page <- read_html(z)
    nome <- page %>% html_elements(".section-title h2") %>% html_text2()
    nome_esteso <- page %>% html_elements(".section-title h3") %>% html_text2()
    tipologia <- page %>% html_element(".col-lg-8 p") %>% html_text2()
    ambito_interesse <- page %>% html_elements(".bordbot:nth-child(2) .col-lg-8 p") %>% html_text2()
    
    page_tibble <- tibble(nome, nome_esteso, tipologia, ambito_interesse)
    results[[z]] <- page_tibble
    
    print(paste("Page: ", which(urls==z)))
    Sys.sleep(3)
  }
  
  galax_confind <- bind_rows(results)
}

write.csv(rappr_settore, "[path]/rappr_settore.csv", row.names = FALSE)
write.csv(feder_settore, "[path]/feder_settore.csv", row.names = FALSE)
write.csv(rappr_evolute, "[path]/rappr_evolute.csv", row.names = FALSE)
write.csv(galax_confind, "[path]/galax_confind.csv", row.names = FALSE)
