# Obiectiv: Estimarea numarului de carti citite in functie de varsta si vechime.
# Date utilizate: library_books_read.csv

library(tidyverse)
library(broom) # pentru o vizualizare curata a modelului
setwd("C:/Users/Georgi/Desktop/Teme/R for Data Science and Data Analytics/Mutascu_Georgiana_Task6")

# 1. Incarcarea datelor
data <- read_csv("library_books_read.csv")

glimpse(data)
summary(data)

# 2. MODELAREA 
# Am ales regresia liniara deoarece variabila tinta (books_read) este numerica.
# Vrem sa vedem cum influenteaza varsta si anii de membership acest numar.

model <- lm(books_read ~ member_age + membership_years, data = data)

# Interpretarea modelului
model_summary <- summary(model)
print(model_summary)

# Coeficientii ne spun impactul:
# member_age: cu cat creste numarul de carti la fiecare an de viata?
# membership_years: cu cat creste numarul de carti pentru fiecare an de fidelitate?

# Calcularea erorii medii absolute
mae <- mean(abs(model$residuals))
print(paste("Eroarea medie a modelului este:", round(mae, 2), "carti."))

# 3. TESTARE PE CAZURI NOI
noi_membri <- data.frame(
  member_age = c(18, 25, 40, 60),
  membership_years = c(1, 3, 5, 7)
)

# Predictia
predictii <- predict(model, newdata = noi_membri)

# Combinarea datelor pentru export
rezultate <- noi_membri %>%
  mutate(books_predicted = round(predictii, 0))

# Exportul predictiilor conform cerintei
write_csv(rezultate, "Georgiana_Mutascu_Task6_predictions.csv")

# 4, VIZUALIZAREA REZULTATELOR
# Alegem sa vizualizam relatia dintre vechime si carti citite,
# fiind de obicei cel mai puternic predictor in biblioteci.

ggplot(data, aes(x = membership_years, y = books_read)) +
  geom_jitter(alpha = 0.6, color = "steelblue", size = 2) + 
  geom_smooth(method = "lm", formula = y ~ x, color = "firebrick", fill = "gray80") +
  labs(
    title = "Impactul vechimii membrilor asupra volumului de lectura",
    subtitle = paste("Fiecare an de membership aduce ~2.8 carti in plus, Fiabilitate model:",
                     round(summary(model)$r.squared * 100, 1), "%"),
    x = "Ani de fidelitate (Membership)",
    y = "Total carti citite",
    caption = "Sursa date: library_books_read.csv | Analiza: Mutascu Georgiana"
  ) +
  theme_minimal(base_size = 13)

# Salvarea graficului
ggsave("Georgiana_Mutascu_vizualizare.png", width = 8, height = 6)

# CONCLUZII
# Modelul indica faptul ca fidelitatea (membership_years) are un impact de 6 ori mai mare
# decat varsta biologica (coeficient 2.79 vs 0.46) pentru volumul de lectura.
# Modelul explica aproximativ  85.27% din variatia datelor,
# ceea ce il face o baza solida pentru planificarea investitiilor.
