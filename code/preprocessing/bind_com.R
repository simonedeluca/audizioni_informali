library("dplyr")
library("xlsx")

x <- c(1:3,5:14)

data <- list()

for (i in x) {
  data[[i]] <- read.csv(paste0("C:/Users/SImone/Desktop/audizioni_informali/data/preprocessed_data/C",i,".csv"), header = TRUE, stringsAsFactors = FALSE)
  }

df <- bind_rows(data)

write.xlsx(df,"C:/Users/SImone/Desktop/audizioni_informali/data/preprocessed_data/dataset_senato.xlsx", sheetName="Dati", col.names=TRUE, row.names=FALSE, append=FALSE, showNA=TRUE, password=NULL)

# OR

path <- paste0("C:/Users/SImone/Desktop/audizioni_informali/data/preprocessed_data/C",1:14,".csv")
path <- path[-4]

data <- lapply(path, read.csv)

df <- bind_rows(data)
