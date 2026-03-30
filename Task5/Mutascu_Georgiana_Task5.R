# Obiectiv: Transformarea datelor in mesaje vizuale de impact
# Date utilizate: 
# books_2.0.csv (informatii despre titluri, autori si genuri)
#   - users.csv (date demografice despre cititori si tara de origine)
#   - rentals.csv (istoricul imprumuturilor)
# Se urmareste identificarea trendurilor, preferintelor si a comportamentului utilizatorilor.
library(tidyverse)
library(lubridate)

# Setarea directorului
setwd("C:/Users/Georgi/Desktop/Teme/R for Data Science and Data Analytics/Mutascu_Georgiana_Task5")

# Incarcarea si explorarea datelor
books_df <- read_csv("books_2.0.csv")
glimpse(books_df)
summary(books_df)

users_df <- read_csv("users.csv")
glimpse(users_df)
summary(users_df)

rentals_df <- read_csv("rentals.csv")
glimpse(rentals_df)
summary(rentals_df)

# Pivotare si curatare (Deduplicarea)
# Pivotare pentru a aduce imprumuturile pe randuri separate si curatarea datei
rentals_tidy <- rentals_df %>%
  pivot_longer(cols = -user_id, names_to = c(".value", "set"),
               names_pattern = "(.*)_(.*)", values_drop_na = TRUE) %>%
  mutate(return_date = dmy(return_date))

# Deduplicare pentru a asigura integritatea datelor inainte de join
users_df_clean <- users_df %>% distinct(user_id, .keep_all = TRUE)
books_df_clean <- books_df %>%
  rename(book_id = id) %>%
  distinct(book_id, .keep_all = TRUE)

# Crearea setului de date complet
full_data <- rentals_tidy %>%
  left_join(users_df_clean, by = "user_id") %>%
  left_join(books_df_clean, by = "book_id")

# VIZUALIZARI

# GRAFIC 1: MIXUL DE GENURI PER UTILIZATOR
# Arata diversitatea lecturii pentru primii 10 utilizatori.
# Barplot "dodge" pentru a compara genurile in cadrul aceluiasi utilizator.
top_users_data <- full_data %>%
  filter(user_id %in% head(unique(user_id), 10))

ggplot(top_users_data, aes(x = as.factor(user_id), fill = genre)) +
  geom_bar(position = "dodge") +
  labs(title = "Mixul de genuri citite (Esantion primii 10 utilizatori)",
       subtitle = "Pattern: Majoritatea utilizatorilor tind sa exploreze cel putin 2 genuri diferite.",
       x = "ID Utilizator", y = "Numar Carti",
       fill = "Gen Literar") +
  theme_minimal()
# Acest grafic confirma diversitatea comunitatii prin prezenta a peste 30 de genuri diferite, 
# indicand interese variate, de la Non-Fiction la Management.

# GRAFIC 2: EVOLUTIA LUNARA A IMPRUMUTURILOR
# Arata fluxul de returnari pe parcursul lunilor.
# Exista varfuri de activitate care necesita planificarea resurselor.
# Am ales "geom_line" pentru a evidentia continuitatea si trendul temporal.
sezonalitate_plot <- full_data %>%
  mutate(luna = month(return_date, label = TRUE, abbr = FALSE)) %>%
  group_by(luna) %>%
  summarise(total = n(), .groups = "drop")

ggplot(sezonalitate_plot, aes(x = luna, y = total, group = 1)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point(color = "darkblue", size = 3) +
  labs(title = "Evolutia lunara a imprumuturilor",
       subtitle = "Pattern: Se observa fluctuatii sezoniere, cu varfuri in anumite luni ale anului.",
       x = "Luna", y = "Total Carti imprumutate") +
  theme_light()
# Acest grafic ajuta biblioteca sa planifice achizitiile inainte de perioadele aglomerate.
# Linia de trend arata o activitate constanta, confirmand o comunitate activa care interactioneaza cu biblioteca lunar, nu doar in perioade izolate.

# GRAFIC 3: DISTRIBUTIA GEOGRAFICA A CITITORILOR
# Arata topul tarilor dupa volumul de imprumuturi.
# Anumite regiuni (ex. Serbia) domina consumul de carte.
# Am ales "geom_col" cu "coord_flip" pentru lizibilitatea numelor tarilor.
top_tari_plot <- full_data %>%
  group_by(country) %>%
  summarise(vizite = n(), .groups = "drop") %>%
  arrange(desc(vizite))

ggplot(top_tari_plot, aes(x = reorder(country, vizite), y = vizite, fill = vizite)) +
  geom_col() +
  coord_flip() +  # Bare orizontale pentru a citi mai usor numele tarilor
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Distributia Geografica a Cititorilor",
       subtitle = "Paatern: Anumite regiuni domina clar consumul de carte.",
       x = "Tara", y = "Numar total de imprumuturi") +
  theme_minimal()
# Concentrarea imprumuturilor in anumite regiuni (ex. Serbia) indica un pattern de consum geografic clar, util pentru campanii de marketing localizate.

# GRAFIC 4: FRECVENTA IMPRUMUTURILOR CITITORILOR
# Arata cate carti imprumuta in medie un utilizator activ.
# Majoritatea respecta limita de 2 carti din sistemul actual.
# Am ales "geom_histogram" pentru a vedea distributia volumului de cititori.
user_activity <- full_data %>%
  group_by(user_id) %>%
  summarise(carti_totale = n(), .groups = "drop")

ggplot(user_activity, aes(x = carti_totale)) +
  geom_histogram(binwidth = 1, fill = "coral", color = "white") +
  labs(title = "Frecventa imprumuturilor per utilizator",
       subtitle = "Pattern: Majoritatea utilizatorilor au imprumutat 2 carti (conform structurii rentals).",
       x = "Numar de carti per utilizator", y = "Numar de oameni") +
  theme_minimal()
# Pattern-ul arata ca utilizatorii maximizeaza resursele disponibile, majoritatea atingand limita superioara de 2 imprumuturi inregistrate.

# GRAFIC 5: TOPUL GENURILOR LITERARE
# Arata preferintele culturale ale intregii comunitati.
# Identificarea genurilor "best-seller" pentru viitoare achizitii.
# Am ales "geom_bar" ordonat descrescator pentru impact vizual imediat.
ggplot(full_data, aes(x = fct_infreq(genre), fill = genre)) +
  geom_bar() +
  labs(title = "Topul Genurilor Literare",
       subtitle = "Pattern: Genul cel mai frecvent indica profilul cultural al comunitatii.",
       x = "Gen Literar", y = "Frecventa Imprumuturi") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = "none") # Scoatem legenda pentru ca numele genurilor sunt deja pe axa X
# Diversitatea este demonstrata de dispersia imprumuturilor pe un spectru larg de genuri (Non-Fiction, Management, Sci-Fi etc.), indicand o comunitate cu interese intelectuale variate.

# CONCLUZII
# Analiza vizuala confirma o comunitate activa si diversa.
# Graficele ofera perspective clare asupra perioadelor de varf si a
# preferintelor geografice, servind drept baza predictiva pentru
# optimizarea stocurilor bibliotecii.


ggsave("Mutascu_Georgiana_Top_Genuri.png", width = 10, height = 6, dpi = 300)

       
       
  
  
  
       
       
       