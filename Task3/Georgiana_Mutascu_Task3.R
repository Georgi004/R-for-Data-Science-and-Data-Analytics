# 1. Incarcarea bibliotecilor necesare pentru procesare si conectivitate
library(tidyverse) # Pentru manipulare date (dplyr) si citire CSV (readr)
library(readxl)    # Pentru importul fisierelor Excel
library(DBI)       # Interfata standard pentru baze de date
library(RMariaDB)  # Driver specific pentru conexiunea cu MySQL

# Resetam folderul de lucru catre locul unde sunt fisierele (Desktop)
setwd("C:/Users/Georgi/Desktop/Teme/R for Data Science and Data Analytics/Mutascu_Georgiana_Task3")
list.files()

# IMPORT SI CURATARE CSV

# Citirea listei de carti. Sarim peste primul rand(titlu decorativ) si
# definim explicit formatele de text care reprezinta valori lipsa.
books_df <- read_csv("RDSDA4_Task_03-books_3.csv", 
                     skip = 1, 
                     na = c("", "NA", "N/A"))

# Standardizam numele coloanelor pentru analiza si convertim anul in format numeric.
# Nota: Valorile non-numerice (ex: "500 BC") vor deveni NA in mod automat.
books_df <- books_df %>%
  rename(publication_year = published) %>% # redenumirea coloanei
  mutate(publication_year = as.integer(publication_year)) # transformarea in numar intreg

# Verificam inregistrarile care au generat probleme la conversia anului
books_df %>% 
  filter(is.na(publication_year))


# Vizualizam structura finala a setului de date CSV
glimpse(books_df)

# IMPORT EXCEL

# Importam evidenta imprumuturilor. R va detecta automat tipurile de date (date-time).
rentals_df <- read_excel("RDSDA4_Task_03-user_rentals_3.xlsx")
glimpse(rentals_df)

# CONEXIUNE SI IMPORT MYSQL

# Stabilim conexiunea cu serverul local MySQL pentru a accesa datele de back-end.
con <- dbConnect(RMariaDB::MariaDB(),
                 user = "root",
                 password = "python",
                 dbname = "library",
                 host = "127.0.0.1")

# Listam tabelele disponibile pentru a confirma conexiunea activa
dbListTables(con)

# Extragem tabelele necesare in obiectele R (Data Frames)
books_db <- dbReadTable(con, "book")
users_db <- dbReadTable(con, "user")
rentals_db <- dbReadTable(con, "rental")

# INCHIDERE CONEXIUNE (Esential pentru a nu lasa procese deschise pe server)
dbDisconnect(con)

# ANALIZA SI LOGICA DE BUSINESS

# Identificam utilizatorii din "East Albert" care figureaza in sistem
# dar nu au nicio carte imprumutata in prezent (rental_book_id este NA).
inactivi_east_albert <- rentals_df %>%
  filter(city == "East Albert" & is.na(rental_book_id))

print(inactivi_east_albert)

# Calculam popularitatea cartilor: Top 5 cele mai sofisticate volume
top_5_carti <- rentals_df %>%
  filter(!is.na(rental_book_id)) %>%
  group_by(rental_book_id) %>%
  summarise(nr_imprumuturi = n()) %>%
  arrange(desc(nr_imprumuturi)) %>%
  head(5)

print(top_5_carti)

# Carti unice imprumutate (din Excel)
carti_unice_excel <- rentals_df %>% filter(!is.na(rental_book_id)) %>% distinct(rental_book_id) %>% nrow()

# Carti unice imprumuatte (din SQL - tabelul rental)
carti_unice_sql <- rentals_db %>% distinct(book_id) %>% nrow()

cat("Carti unice imprumutate (Excel):", carti_unice_excel, "\n")
cat("Carti unice imprumutate (SQL):", carti_unice_sql, "\n")

# VERIFICARE CONSISTENTA

# Comparam sursele: Verificam daca exista diferente intre inventarul CSV si cel din SQL.
cat("Carti in fisierul CSV:", nrow(books_df), "\n")
cat("Carti in baza de date SQL:", nrow(books_db), "\n")

diferenta <- nrow(books_df) - nrow(books_db)
cat("Diferenta de inregistrari este de:", diferenta)

# EXPORT REZULTATE

# Salvam analizele in fisiere externe pentru a fi trimise departamentelor relevante.
write_csv(inactivi_east_albert, "Mutascu_Georgiana_Inactivi_EastAlbert.csv")
write_csv(top_5_carti, "Mutascu_Georgiana_Top_5_Imprumuturi.csv")

message("Sarcina finalizata cu succes! Fisierele au fost exportate.")

# OBSERVATII
# Am ales sa curat datele inainte de analiza pentru a evita erorile de tip 
# "NAs introduced by coercion" in timpul calculelor.
# cea mai grea parte mi s-a parut curatarea randurilor de titlu din CSV.