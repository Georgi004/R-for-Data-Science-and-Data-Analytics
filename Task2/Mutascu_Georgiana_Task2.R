# Analiza vanzarilor online
# Obiectiv: Identificarea produsului performant si analiza fluxului de incasari

# COLECTAREA SI DEFINIREA DATELOR BRUTE
# Utilizam vectori pentru a stoca seriile de date primite pe Slack
# Aceasta structura permite calcule matematice rapide pe intreaga serie

# Date Produs A
preturi_A <- c(19.99, 24.50, 18.75, 22.00, 20.30)
cantitati_A <- c(5, 8, 4, 7, 6)

# Date Produs B
preturi_B <- c(15.00, 16.50, 14.75, 17.00, 15.80)
cantitati_B <- c(10, 9, 12, 8, 11)

# CALCULAREA INDICATORILOR FINANCIARI
# Calcularea venitului zilnic prin inmultirea vectoriala
venit_A <- preturi_A * cantitati_A
venit_B <- preturi_B * cantitati_B

# ORGANIZAREA DATELOR
# Grupam datele intr-o singura lista ierarhica (shop_data) pentru o mai buna gestiune a obiectelor
# Listele ne permit sa accesam rapid toate atributele unui produs (nume, preturi, venit total)
shop_data <- list(
  Produs_A = list(nume= "Produs A", pret = preturi_A, cant = cantitati_A, venit = venit_A),
  Produs_B = list(nume = "Produs B", pret = preturi_B, cant = cantitati_B, venit = venit_B)
)

# CONSTRUIREA TABELULUI DE ANALIZA
# Calcul pentru venitul total per produs
venit_total_A <- sum(shop_data$Produs_A$venit)
venit_total_B <- sum(shop_data$Produs_B$venit)

# Transformam datele in format tabelar (Data Frame) pentru a facilita agregarile ulterioare
# Combinam tabelele individuale folosind rbind() pentru a avea o singura sursa de adevar
df_A <- data.frame(
  produs = "Produs A",
  zi = 1:5,
  pret = preturi_A,
  cantitate = cantitati_A,
  venit = venit_A
)

df_B <- data.frame(
  produs = "Produs B",
  zi = 1:5,
  pret = preturi_B,
  cantitate = cantitati_B,
  venit = venit_B
)

# Unim cele doua tabele
df_sales <- rbind(df_A, df_B)

# Afisarea tabelului in consola
print("Tabelul complet al vanzarilor")
print(df_sales)

# ANALIZA FINALA

# Folosim aggregate() pentru a extrage perspectivele cheie cerute de management

# Venit total per produs
comparatie_venit <- aggregate(venit ~ produs, data = df_sales, sum)
print(comparatie_venit)

# Pret mediu per produs
comparatie_pret <- aggregate(pret ~ produs, data = df_sales, mean)
print(comparatie_pret)

# Performanta zilnica a magazinului
venit_pe_zile <- aggregate(venit ~ zi, data = df_sales, sum)
print("Venit total per zi (Ambele Produse)")
print(venit_pe_zile)

# Concluzie
# 1. CARE PRODUS A FOST MAI DE SUCCES SI DE CE?
# Produsul B este castigator, cu un venit total de 785.30 lei fata de 646.75 lei (Produsul A).
# Succesul Produsului B se datoreaza volumului: desi este cu ~ 25% mai ieftin decat Produsul A,
# acesta vinde constant aproape dublu ca numar de unitati (media de 10 unitati/zi).

# 2. CE M-A SURPRINS?
# M-a surprins rezilienta Produsului B in ziua 3: desi a fost o zi slaba pentru Produsul A
# (doar 4 unitati), Produsul B a atins varful de vanzari (12 unitati), salvand veniturile zilei respective.
# De asemenea, ziua 2 a fost singurul moment in care ambele produse au performat la capacitate maxima.

# 3. CARE AR FI PASUL LOGIC URMATOR?
# Recomand o analiza de stoc pentru Produsul B, fiind cel mai rulat produs, pentru a evita pierderile
# prin "out-of-stock". Pentru Produsul A, as testa o usoara reducere de pret (promotie)
# pentru a vedea daca volumul poate fi stimulat sa depaseasca pragul de rentabilitate actual.

