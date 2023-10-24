# Zbadanie wplywu Barbary na Boze Narodzenie dla Warszawy
library(httr)   
library(jsonlite)
#install.packages("tseries")
#install.packages('segmented')
library(tseries)
library(segmented)

#Ustawienie lokalizacji na angielska
Sys.setlocale("LC_ALL", "English")

# Pobieranie danych pogodowych dla Warszawy
r <- GET("https://archive-api.open-meteo.com/v1/archive?latitude=52.23&longitude=21.01&start_date=1950-01-01&end_date=2022-12-31&daily=temperature_2m_max,temperature_2m_min,temperature_2m_mean,precipitation_sum,precipitation_hours,winddirection_10m_dominant&timezone=Europe%2FWarsaw",
         Accept = "application/json")
jsonRespText <- content(r, as = "text")
aux <- fromJSON(jsonRespText)

# Tworzenie ramki danych dla danych pogodowych Warszawy
warszawa <- data.frame(time = aux$daily$time,
                       t_2m_max = aux$daily$temperature_2m_max,
                       t_2m_min = aux$daily$temperature_2m_min,
                       t_2m_mean = aux$daily$temperature_2m_mean,
                       p_sum = aux$daily$precipitation_sum,
                       p_h = aux$daily$precipitation_hours,
                       w_d = aux$daily$winddirection_10m_dominant)

warszawa$time <- as.Date(warszawa$time)

#summary(warszawa)  #check

# Wyodrebnienie dnia, miesiaca i roku z daty
day <- format(warszawa$time, format = "%d")
month <- format(warszawa$time, format = "%m")
year <- format(warszawa$time, format = "%Y")

# Konwersja dnia, miesiaca i roku na liczbe
warszawa$day <- as.numeric(day)
warszawa$month <- as.numeric(month)
warszawa$year <- as.numeric(year)

# Sprawdzenie, czy miesiac to grudzien i czy dzien to 6
logic <- warszawa$month == 12 & warszawa$day == 6
barbara <- warszawa$t_2m_mean[logic]

# Sprawdzenie, czy miesiac to Grudzien i dzien jest pomiedzy 25 a 26
logic <- warszawa$month == 12 & (warszawa$day >= 25 & warszawa$day <= 26)
bn <- warszawa$p_sum[logic]

# Obliczenie sredniej
bn <- (bn[seq(1, length(bn)-1, 2)] + bn[seq(2, length(bn), 2)]) / 2

# Sprawdzenie, czy temperatura jest poniżej 0
barbara <- barbara < 0
# Sprawdzenie, czy opady sa wieksze od 0
bn <- bn > 0

# Tworzenie tabeli kontyngencji
ct <- table(barbara, bn)

# Przeprowadzenie testu chi-kwadrat
X <- chisq.test(ct)

X

#Wyniki testu pokazuja, ze przyslowie sie sprawdza


# Inne przyslowie: Jesli snieg na Wszystkich Swietych zawiedzie, to Swiety Marcin (11.11) na bialym koniu przyjedzie. Test dla Rzeszowa
#Przyjmujac za obecnosc sniegu temp < 0

# Pobieranie danych pogodowych dla Rzeszowa
r2 <- GET("https://archive-api.open-meteo.com/v1/archive?latitude=50.04&longitude=21.73&start_date=1950-01-01&end_date=2022-12-31&daily=temperature_2m_max,temperature_2m_min,temperature_2m_mean,precipitation_sum,precipitation_hours,winddirection_10m_dominant&timezone=Europe%2FWarsaw",
         Accept = "application/json")
jsonRespText2 <- content(r2, as = "text")
aux2 <- fromJSON(jsonRespText2)

# Tworzenie ramki danych dla danych pogodowych Rzeszowa
rzeszow <- data.frame(time = aux2$daily$time,
                       t_2m_max = aux2$daily$temperature_2m_max,
                       t_2m_min = aux2$daily$temperature_2m_min,
                       t_2m_mean = aux2$daily$temperature_2m_mean,
                       p_sum = aux2$daily$precipitation_sum,
                       p_h = aux2$daily$precipitation_hours,
                       w_d = aux2$daily$winddirection_10m_dominant)

rzeszow$time <- as.Date(rzeszow$time)

#summary(rzeszow)  #check

# Wyodrebnienie dnia, miesiaca i roku z daty
day <- format(rzeszow$time, format = "%d")
month <- format(rzeszow$time, format = "%m")
year <- format(rzeszow$time, format = "%Y")

# Konwersja dnia, miesiaca i roku na liczbe
rzeszow$day <- as.numeric(day)
rzeszow$month <- as.numeric(month)
rzeszow$year <- as.numeric(year)

# Sprawdzenie, czy miesiac to listopad i czy dzien to 1
logic <- rzeszow$month == 11 & rzeszow$day == 1
ws_temp <- rzeszow$t_2m_mean[logic]

# Sprawdzenie, czy miesiac to listopad i czy dzien to 11
logic <- rzeszow$month == 11 & rzeszow$day == 11
marcin_temp <- rzeszow$t_2m_mean[logic]

# Sprawdzenie, czy temperatura jest poniżej 0
ws_temp <- ws_temp < 0
marcin_temp <- marcin_temp < 0

# Tworzenie tabeli kontyngencji
ct2 <- table(ws_temp,marcin_temp)

# Przeprowadzenie testu chi-kwadrat
X2 <- chisq.test(ct2)

X2

#Przeprowadzajac ten test mozemy stwierdzic, ze jest bardzo wysokie prawdopodobienstwo (18%) tego, ze przyslowie nie sprawdzi sie w Rzeszowie