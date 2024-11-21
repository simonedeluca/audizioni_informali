library("dplyr")
library("stringr")

# Carichiamo commissione13.csv in c13
c13 <- read.csv("[path]/audizioni_informali/data/raw_data/senato/commissione13.csv", header = TRUE, stringsAsFactors = FALSE)

nomi <- c13$NOMI
nomi <- str_squish(nomi)

nomi_splitted <- str_split(nomi, ";", simplify = FALSE)

m <- matrix(nrow=247, ncol=1)
for (i in 1:247) {
  m[i,] <- length(nomi_splitted[[i]])
}

c13$times <- m[,1]

new_data <- c13[rep(1:nrow(c13), times = c13$times), ]
rownames(new_data) <- 1:nrow(new_data)

single_name <- unlist(nomi_splitted)

single_name <- str_trim(side = "both", single_name)
words2 <- "^di|^del|^della|^dell'|^delle|^dei|^da|^dai|^dal|^dall'|^dalla|^dallo|^dalle"
single_name <- gsub(words2, "", single_name) %>% str_trim(side = "left")

p <- str_extract(single_name, "-")
p <- which(p == "-")
x <- str_subset(single_name, "-")

y <- c(2,4,5,10,21)
for (i in y) {
  x[i] <- gsub("-", " ", x[i])
}

output <- c()
for (i in y) {
  h <- p[i]
  output <- c(output, h)
}

for (i in y) {
  single_name[output] <- x[y]
}

single_name[162] <- "Prof.ssa Fraschetti, UNIVERSITA' DEGLI STUDI DI NAPOLI FEDERICO II"
single_name[148] <- "Assitol-Elettricità Futura-Anpeb"

single_name <- str_squish(single_name)
new_data[,2] <- single_name

x <- new_data$DATA
x <- gsub("^\\s","0", x)
x <- str_split_fixed(x, " ", 3)
x <- as.data.frame(x)

x$month <- with(x, ifelse(V2 %in% "Gennaio", 1,
                          ifelse(V2 %in% "Febbraio", 2,
                                 ifelse(V2 %in% "Marzo", 3,
                                        ifelse(V2 %in% "Aprile", 4,
                                               ifelse(V2 %in% "Maggio", 5,
                                                      ifelse(V2 %in% "Giugno", 6,
                                                             ifelse(V2 %in% "Luglio", 7,
                                                                    ifelse(V2 %in% "Agosto", 8,
                                                                           ifelse(V2 %in% "Settembre", 9,
                                                                                  ifelse(V2 %in% "Ottobre", 10,
                                                                                         ifelse(V2 %in% "Novembre", 11,
                                                                                                ifelse(V2 %in% "Dicembre", 12,
                                                                                                       "NA")))))))))))))


date <- paste(x$V1,x$month,x$V3, sep= "-")
new_data <- subset(new_data, select = -c(DATA,times))
new_data$DATA <- as.Date(date, format="%d-%m-%Y")

new_data$COMMISSIONE <- gsub("ª", "", new_data$COMMISSIONE)
new_data$COMMISSIONE <- gsub(" e ", " ; ", new_data$COMMISSIONE)
new_data$COMMISSIONE <- gsub(",", " ;", new_data$COMMISSIONE)
new_data$COMMISSIONE <- gsub("con", " ;", new_data$COMMISSIONE)
new_data$COMMISSIONE <- gsub("Senato", "", new_data$COMMISSIONE)
new_data$COMMISSIONE <- gsub("Camera", "", new_data$COMMISSIONE) %>% str_squish()

# salva new_data in C13
write.csv(new_data, "[path]/audizioni_informali/data/clean_data/senato/C13.csv", row.names = FALSE)
