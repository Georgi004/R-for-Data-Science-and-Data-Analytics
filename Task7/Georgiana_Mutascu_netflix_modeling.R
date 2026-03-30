# Proiect Final: R ca revansa – Netflix Case

library(tidyverse)
library(lubridate)

setwd("C:/Users/Georgi/Desktop/Teme/R for Data Science and Data Analytics/Mutascu_Georgiana_ProiectFinal")

# 1. IMPORTUL DATELOR
titles <- read_csv("titles.csv")
ratings <- read_csv("ratings.csv")
genres <- read_csv("genres.csv")
countries <- read_csv("countries.csv")
certifications <- read_csv("certifications.csv")

# 2. COMBINAREA SI PREGATIREA DATELOR
# Unim informatiile de baza cu notele IMDb
df_clean <- titles %>%
  left_join(ratings, by = "id") %>%
  left_join(genres, by = "id") %>%
  left_join(countries, by = "id") %>%
  left_join(certifications, by = "id") %>%
  # Filtram: Doar filme din ultimii 5 ani
  filter(type == "MOVIE", release_year >= 2021) %>%
  # Cream variabilele de interes: Durata scurta si Succesul
  mutate(
    is_short = ifelse(runtime < 90, 1, 0),
    is_high_rated = ifelse(imdb_score > 7.5, 1, 0)
  ) %>%
  drop_na(imdb_score, runtime)

# 3. ANALIZA APROFUNDATA
df_analiza_complexa <- df_clean %>%
  group_by(production_country, genre) %>%
  summarise(
    scor_mediu = mean(imdb_score, na.rm = TRUE),
    durata_medie = mean(runtime, na.rm = TRUE),
    nr_filme = n(),
    .groups = "drop"
  ) %>%
  filter(nr_filme > 2)

# 4. VIZUALIZARE REZULTATE
# Calculam rata de succes (%) pentru fiecare categorie
df_viz <- df_clean %>%
  group_by(is_short) %>%
  summarise(
    total = n(),
    succes = sum(is_high_rated),
    rata_succes = (succes / total) * 100
  ) %>%
  mutate(categorie = ifelse(is_short == 1, "Sub 90 min", "Peste 90 min"))

# Generam graficul cu bare
ggplot(df_viz, aes(x = categorie, y = rata_succes, fill = categorie)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(round(rata_succes, 1), "%")), 
            vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("Sub 90 min" = "firebrick", "Peste 90 min" = "gray60")) +
  labs(
    title = "Rata de Succes a Filmelor (Scor IMDb > 7.5)",
    subtitle = "Filmele mai lungi au o probabilitate usor mai mare de a fi bine cotate.",
    x = "Durata Filmului",
    y = "Procentajul de succes (%)",
    caption = "Sursa: Netflix Dataset | Analiza: Mutascu Georgiana"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Salvarea automata a noului grafic
ggsave("Mutascu_Georgiana_Netflix_Vizualizare.png", width = 8, height = 6)

# 4. MODELARE PREDICTIVA 
# Prezicem probabilitatea de a fi "High Rated" in functie de durata si anul lansarii
model_logit <- glm(is_high_rated ~ runtime + release_year + genre, 
                   data = df_clean, 
                   family = binomial)

summary(model_logit)

# 5. TESTARE PE CAZURI NOI
scenarii_noi <- data.frame(
  title = c("Docu Canada", "Thriller Spania", "Indie Brazilia"),
  runtime = c(45, 85, 120),
  release_year = c(2026, 2026, 2026),
  genre = c("documentation", "thriller", "drama")
)

# Calculam probabilitatea de succes
scenarii_noi$probabilitate_succes <- predict(model_logit, newdata = scenarii_noi, type = "response")
print(scenarii_noi)

# Exportul predictiilor
write_csv(scenarii_noi, "Mutascu_Georgiana_Predictii_Netflix.csv")

# 6. RECOMANDARE FINALA 
# Analiza indica faptul ca atingerea unui scor de peste 7.5 este o provocare statistica (sub 4% sanse).
# Interesant este ca modelul arata o usoara crestere a probabilitatii odata cu cresterea duratei
# (coeficient runtime 0.012), sugerand ca publicul apreciaza mai mult filmele cu desfasurare 
# completa in defavoarea celor extrem de scurte.
# Recomandare: Pentru succes garantat, nu ne bazam doar pe durata, ci pe calitatea productiei,
# mentinand filmele intr-un interval echilibrat (90-120 min).