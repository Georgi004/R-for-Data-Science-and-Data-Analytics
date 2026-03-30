# Obiectiv: Curatarea, pivotarea si integrarea datelor pentru identificarea tiparelor de lectura

# 1. Incarcarea bibliotecilor
library(tidyverse)
library(readxl)
library(lubridate) # esential pentru manipularea datelor calendaristice (luni, ani)

# 2. Setarea directorului de lucru
setwd("C:/Users/Georgi/Desktop/Teme/R for Data Science and Data Analytics/Mutascu_Georgiana_Task4")

# 3. Incarcarea fisierelor
books_df <- read_csv("books_2.0.csv")
users_df <- read_csv("users.csv")
rentals_df <- read_csv("rentals.csv")

# STRUCTURAREA
# Problema: rentals_df are coloane book_id_1, book_id_2 etc. pe acelasi rand.
# Folosim pivot_longer pentru a avea un rand/imprumut (un rand/carte)

rentals_tidy <- rentals_df %>%
  pivot_longer(
    cols = -user_id,
    names_to = c(".value", "set"),
    names_pattern = "(.*)_(.*)",
    values_drop_na = TRUE # eliminam celulele unde nu s-a imprumutat nimic
  ) %>%
  # Presupunem ca data de inchiriere este unitara per sesiune
  mutate(return_date = dmy(return_date)) # convertim in format Date

glimpse(rentals_tidy)

users_df_clean <- users_df %>%
  distinct(user_id, .keep_all = TRUE)

if("id" %in% colnames(books_df)) {
  books_df <- books_df %>% rename(book_id = id)
}
books_df_clean <- books_df %>%
  distinct(book_id, .keep_all = TRUE)

full_data <- rentals_tidy %>%
  left_join(users_df_clean, by = "user_id") %>%
  left_join(books_df_clean, by = "book_id")
message("Unire finalizata. Numar randuri: ", nrow(full_data))

# UNIREA DATELOR

# Unim imprumuturile cu datele despre utilizatori
full_data <- rentals_tidy %>%
  left_join(users_df_clean, by = "user_id") %>%
  # Unim si cu datele despre carti pentru a afla genul
  left_join(books_df_clean, by = "book_id")
# Verificam daca avem randuri in plus fata de rentals_tidy
nrow(rentals_tidy)
nrow(full_data)

# RASPUNSURI LA INTREBARILE DE BUSINESS

# 1.Ce genuri sunt cele mai populare si in randul cui?
populartate_genuri <- full_data %>%
  group_by(genre, user_id) %>%
  summarise(nr_imprumuturi = n(), .groups = 'drop') %>%
  arrange(desc(nr_imprumuturi))

# 2.Cum variaza numarul de carti imprumutate pe parcursul lunilor?
sezonalitate <- full_data %>%
  mutate(luna = month(return_date, label = TRUE, abbr = FALSE)) %>%
  group_by(luna) %>%
  summarise(total_imprumuturi = n()) %>%
  arrange(luna)

# 3.Avem utilizatori care revin dupa carti similare? (loialitate pe gen)
utilizatori_loiali <- full_data %>%
  group_by(user_id, genre) %>%
  summarise(vizite_gen = n(), .groups = 'drop') %>%
  filter(vizite_gen > 1)

# 4.Utilizatorii Fantoma (cei care au venit o singura data)
utilizatori_fantoma <- full_data %>%
  group_by(user_id) %>%
  summarise(total_carti = n()) %>%
  filter(total_carti == 1)

# 5.Top tari care viziteaza biblioteca
top_tari <- full_data %>%
  group_by(country) %>%
  summarise(vizite = n()) %>%
  arrange(desc(vizite))

# 6.Tabel Pivot: Utilizatori (randuri) vs Genuri (coloane)
tabel_obiceiuri <- full_data %>%
  group_by(user_id, genre) %>%
  summarise(n = n(), .groups = 'drop') %>%
  filter(!is.na(genre)) %>%
  pivot_wider(names_from = genre, values_from = n, values_fill = 0)

# EXPORT REZULTATE
write_csv(tabel_obiceiuri, "Mutascu_Georgiana_Matrice_Obiceiuri.csv")
write_csv(populartate_genuri, "Mutascu_Georgiana_Top_Genuri.csv")

message("Analiza Task 4 finalizata cu succes!")

# OBSERVATIE
# Am gasit duplicatele in fiserele sursa si le-am eliminat folosind distinct() pentru a pastrta integritatea raportului.