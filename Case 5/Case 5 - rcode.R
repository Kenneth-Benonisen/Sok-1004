#-------------------------------------------------------------------------------
library(rjstat)
url <- "http://data.ssb.no/api/v0/dataset/1086.json?lang=no"
kpi_json <- fromJSONstat(url)
str(kpi_json)



tabell <- kpi_json[[1]]
str(tabell)
head(tabell)


suppressPackageStartupMessages(library(tidyverse))

tabell %>%
  group_by(statistikkvariabel) %>%
  summarise(n=n())

kpi <- 
  tabell %>%
  filter(statistikkvariabel=="Konsumprisindeks (2015=100)")
head(kpi)

library(lubridate)



kpi <-
  kpi %>%
  separate(måned, into=c("år", "måned"), sep="M") %>% 
  mutate(dato=ymd(paste(år, måned, "1"))) %>% 
  select(dato, konsumgruppe, statistikkvariabel, value)
head(kpi)


kpi %>%
  ggplot(aes(x=dato, y=value)) + 
  geom_line()


kpi %>%
  ggplot(aes(x=dato, y=value)) + 
  geom_line(col="dark blue") +
  labs(title="Konsumprisindeks - KPI \n Totalindeks (2015=100)",
       x =" ",
       y = "Totalindeks") +
  theme_bw()



#-------------------------------------------------------------------------------


# Konsumprisindeksen har som basis, 2015=100. Hva innebærer dette? La oss se på de faktiske KPI tallene for 2015.
kpi %>%
  filter(dato >= "2015-01-01" & dato <= "2015-12-01")

#Dersom vi beregner gjennomsnittet av KPI verdiene for 2015 vil de være lik 100. 
#For enkelhetens skyld lager vi først en variabel på år (year) som vi deretter filtrerer på.

kpi %>%
  mutate(year=year(dato)) %>% 
  filter(year==2015) %>%
  summarise(mean(value))


#-------------------------------------------------------------------------------

#Hva om vi ønsker å endre basis fra 2015 til et annet år? Hvordan gjør vi det? La oss endre basis til 2010.


kpi %>%
  mutate(year=year(dato)) %>% 
  filter(year==2010) %>%
  summarise(mean(value))


# Dette tallet er nå vår nye basis, og vi kan re-indeksere KPI med å dele alle KPI observasjoner med dette tallet. 
# Dette gjør vi i to steg. Først beregner vi gjennomsnittet til den nye basisen i 2010, og 
# lagrer det i objektet b2010, med variabelnavn ny_basis_2010 deretter beregner vi den nye rebaserte indeksen

b2010 <- kpi %>%
  mutate(year=year(dato)) %>% 
  filter(year==2010) %>%
  summarise(ny_basis_2010=mean(value))

kpi <- 
  kpi %>%
  mutate(KPI_2010=100*value/b2010$ny_basis_2010)

# Vi kan nå lage en ny figur med begge indeksene i. 

kpi %>%
  rename(KPI_2015=value) %>%
  select(dato, KPI_2010, KPI_2015) %>% 
  pivot_longer(-dato,
               names_to = "KPI",
               values_to = "indeks") %>% 
  ggplot(aes(x=dato, y=indeks, col=KPI)) +
  geom_line() +
  labs(title="Konsumprisindeks - KPI",
       x =" ",
       y = "Totalindeks") +
  theme_bw()


#-------------------------------------------------------------------------------


# Oppgave 1 - Rebaser konsumprisindeksen med November 2019 lik 100, og lag en figur. 
# Unngå å bruke kode der du skriver verdien for KPI i november 2019 inn som et tall.

kpi %>%
  mutate(year=year(dato)) %>% 
  filter(dato=="2019-11-01") %>%
  summarise(mean(value))

b2019_nov <- kpi %>%
  mutate(year=year(dato)) %>% 
  filter(dato=="2019-11-01") %>%
  summarise(ny_basis_2019_nov=mean(value))

kpi <- 
  kpi %>%
  mutate(KPI_2019_nov=100*value/b2019_nov$ny_basis_2019_nov)


# Lager en graf med tallene fra 2010, 2015 og Nov 2019.

kpi %>%
  rename(KPI_2015=value) %>%
  select(dato, KPI_2010, KPI_2015, KPI_2019_nov) %>% 
  pivot_longer(-dato,
               names_to = "KPI",
               values_to = "indeks") %>% 
  ggplot(aes(x=dato, y=indeks, col=KPI)) +
  geom_line() +
  labs(title="Konsumprisindeks - KPI",
       x =" ",
       y = "Totalindeks") +
  theme_bw()


#-------------------------------------------------------------------------------


# Oppgave 2 - Gå til de respektive sidene for VGs matbørs. 
# Finn tallene for matbørsen i oktober 2018 og september 2020, og repliser beregningene over.


# Gjennomsnitt av totalsum for 6 ulike butikker - Oktober 2018

Totalsum_okt_18 <- data.frame(y1 = c(3259.30, 3315.45, 3326.76, 3597.78, 3666.59, 3731.57))
mean(Totalsum_okt_18$y1)


# Gjennomsnitt av totalsum for 6 ulike butikker - September 2020

Totalsum_sep_20 <- data.frame(y2 = c(3519.72, 3520.36, 3534.44, 3821.26, 3832.89, 3944.75))
mean(Totalsum_sep_20$y2)


# kalkulerer differansen mellom 18 og 2020 tallene og ser at vi også ender opp med økt pris på 6.11%.

prisøkning = (mean(Totalsum_sep_20$y2) / mean(Totalsum_okt_18$y1)) * 100
prisøkning



# Deretter gjør vi beregning via KPI som er hentet tidligere. 

kpi18 <- kpi %>%
  mutate(year=year(dato)) %>% 
  filter(dato=="2018-10-01") %>%
  summarise(mean(value))


kpi20 <-kpi %>%
  mutate(year=year(dato)) %>% 
  filter(dato=="2020-09-01") %>%
  summarise(mean(value))


# Finner ut differansen mellom KPI fra 2018 og 2020. Ser at det er en differanse på 3.6 
# som er noe høyere enn hva som blir beskrevet i selve oppgaven. Korreksjon av SSB kanskje?

Kpiøkning = kpi20 - kpi18
Kpiøkning


#-------------------------------------------------------------------------------

# oppgave 3 - SSB har i sin API med ferdige datasett, Konsumprisindeks - undergruppenivå2. 1979M01 - siste måned.
# I denne oppgaven laster du ned denne datatabellen, velger en samsvarende kategori i VGs matbørs, 
# og sammenligner den prosentvise prisutviklingen i denne kategorien 
# fra oktober 2018 til september 2020 med den prosentvise endringen i konsumprisindeksen.


url <- "https://data.ssb.no/api/v0/dataset/1094.json?lang=no"
kpi_json_1094 <- fromJSONstat(url)
str(kpi_json_1094)



tabell_1094 <- kpi_json_1094[[1]]
str(tabell)
head(tabell)


tabell_1094 %>%
  group_by(statistikkvariabel) %>%
  summarise(n=n())

# lager for en enkeltmåned for å se hva denne inneholder av produkter.
kpi_mat_18 <- tabell_1094 %>% 
  filter(måned =="2018M10")



# lager en ny tabell, velger perioden mellom Oktober 18 og September 2020.
kpi_mat_18_20 <- tabell_1094 %>% 
  filter(måned >= "2018M10"  & måned <= "2020M09")



# Koden under fungerer ikke pga. Det er flere produkter som benytter samme tidsrom.
# lar det ligge dersom jeg er nødt til å gå tilbake for å se tankegangen. 
#-------------------------------------------------------------------------------


# Lager en ny dataFrame som skal kun inneholde kjøtt og fisk, som skal sammenlignes med listen på VG matbørs. 
#Kjott_og_fisk <- kpi_mat_18[c(9:13, 15:16, 17:18, 20:21), ]





# lager en ny tabell, velger spesifikt månedene Okt 18 til Sep 2020. 
#kpi_mat_18_10 <- tabell_1094 %>% 
#  filter(måned == "2018M10")



# Plukker ut den kategorien som vi ønsker å se på fra den nye tabellen. 
# Sorterer rows slik at det starter på 1 igjen, fremfor 289:360 også videre.
#Kjott_og_fisk_18_20 <- kpi_mat_18_20[c(289:360, 577:936, 1009:1512), ] %>% 
#  as.data.frame(row.names = 1:nrow(.)) 

# Vi er nødt til å endre navnene på de ulike produktene til et fellesnavn.
#Kjott_og_fisk_18_20$konsumgruppe <- gsub("Pizza og paier|Storfe- og kalvekjøtt|Svinekjøtt|Lamme- og geitekjøtt|Fjørfe|Annet kjøtt
#                                         |Tørket, saltet eller røkt kjøtt|Annet bearbeidet kjøtt|Fersk og kjølt fisk|Fryst fisk|Fryst sjømat
#                                         |Tørket, røkt eller saltet fisk og sjømat|Annen konservert eller bearbeidet fisk og sjømat", 
#                                         "Kjøtt og Fisk", Kjott_og_fisk_18_20$konsumgruppe) 




# Plukker ut den kategorien, som vi ønsker å se på fra den nye tabellen. -- Velger pizza og paier 
# Sorterer rows slik at det starter på 1 igjen, fremfor 289:360
#Kjott_og_fisk_18_10 <- kpi_mat_18_10[c(289:360, 577:936, 1009:1512), ] %>% 
#  as.data.frame(row.names = 1:nrow(.)) 




# Plukker ut den kategorien som vi ønsker å se på fra den nye tabellen. 
# Sorterer rows slik at det starter på 1 igjen, fremfor 289:360 også videre.
#Pizza_18_20 <- kpi_mat_18_20[c(289:360), ] %>% 
#  as.data.frame(row.names = 1:nrow(.)) 

#-------------------------------------------------------------------------------
# KPI for svinekjott.

# Lager en ny dataFrame som skal kun inneholde svinekjøtt, som skal sammenlignes med listen på VG matbørs. 
#Svinekjott_18_10 <- kpi_mat_18_20[c(649:720), ] %>% 
#  as.data.frame(row.names = 1:nrow(.)) 

# prosentvis endring i en indeks -- hentet fra selve oppgaven.
#tabell_svinekjott <- Svinekjott_18_10 %>%
#  filter(statistikkvariabel != "12-måneders endring (prosent)") %>% 
#  separate(måned, into = c("år", "måned"), sep="M") %>% 
#  mutate(dato = ymd(paste(år, måned, "1"))) %>% 
#  select(dato, statistikkvariabel, value) %>% 
#  pivot_wider(names_from = "statistikkvariabel") %>% 
#  rename(KPI = "Konsumprisindeks (2015=100)",
#         SSB_dp ="Månedsendring (prosent)") %>% 
#  mutate(dp  = 100*(KPI - lag(KPI))/lag(KPI),
#       lndp.v1 = 100*(log(KPI) - log(lag(KPI))),
#       lndp.v2 = c(NA, 100*diff(log(KPI))))

# grafisk fremstilling prosentvis endring av pizza og paier

#tabell_svinekjott %>%
#  filter(dato >= "1979-02-01") %>%
#  mutate(positiv=lndp.v2 >= 0) %>% 
#  ggplot(aes(x=dato, y=lndp.v2, fill=positiv)) +
#  geom_col(position = "identity") +
#  scale_fill_manual(values = c("dark blue", "dark red"), guide = FALSE) +
#  labs(title="Prosentvis endring i konsumprisindeksen for Svinekjøtt \n Totalindeks (2015=100)",
#       x = " ",
#       y = "Prosent") +
#  theme_bw()

#-------------------------------------------------------------------------------
# KPI for Pizza og paier

# Lager en ny dataFrame som skal kun inneholde pizza og paier, som skal sammenlignes med listen på VG matbørs. 
#pizza_18_20 <- kpi_mat_18_20[c(289:360), ] %>% 
#  as.data.frame(row.names = 1:nrow(.)) 


# prosentvis endring i en indeks -- hentet fra selve oppgaven.
#tabell_pizza <- pizza_18_20 %>%
#  filter(statistikkvariabel != "12-måneders endring (prosent)") %>% 
#  separate(måned, into = c("år", "måned"), sep="M") %>% 
#  mutate(dato = ymd(paste(år, måned, "1"))) %>% 
#  select(dato, statistikkvariabel, value) %>% 
#  pivot_wider(names_from = "statistikkvariabel") %>% 
#  rename(KPI = "Konsumprisindeks (2015=100)",
#         SSB_dp ="Månedsendring (prosent)") %>% 
#  mutate(dp  = 100*(KPI - lag(KPI))/lag(KPI),
#         lndp.v1 = 100*(log(KPI) - log(lag(KPI))),
#         lndp.v2 = c(NA, 100*diff(log(KPI))))

# grafisk fremstilling prosentvis endring av pizza og paier

#tabell_pizza %>%
#  filter(dato >= "1979-02-01") %>%
#  mutate(positiv=lndp.v2 >= 0) %>% 
#  ggplot(aes(x=dato, y=lndp.v2, fill=positiv)) +
#  geom_col(position = "identity") +
#  scale_fill_manual(values = c("dark blue", "dark red"), guide = FALSE) +
#  labs(title="Prosentvis endring i konsumprisindeksen for Pizza og paier \n Totalindeks (2015=100)",
#       x = " ",
#       y = "Prosent") +
#  theme_bw()

#-------------------------------------------------------------------------------
# KPI for Storfe og kalvekjøtt

# Lager en ny dataFrame som skal kun inneholde Storfe og kalvekjøtt, som skal sammenlignes med listen på VG matbørs. 
#storfe_18_20 <- kpi_mat_18_20[c(577:648), ] %>% 
#  as.data.frame(row.names = 1:nrow(.)) 

# prosentvis endring i en indeks -- hentet fra selve oppgaven.
#tabell_storfe <- storfe_18_20 %>%
#  filter(statistikkvariabel != "12-måneders endring (prosent)") %>% 
#  separate(måned, into = c("år", "måned"), sep="M") %>% 
#  mutate(dato = ymd(paste(år, måned, "1"))) %>% 
#  select(dato, statistikkvariabel, value) %>% 
#  pivot_wider(names_from = "statistikkvariabel") %>% 
#  rename(KPI = "Konsumprisindeks (2015=100)",
#         SSB_dp ="Månedsendring (prosent)") %>% 
#  mutate(dp  = 100*(KPI - lag(KPI))/lag(KPI),
#         lndp.v1 = 100*(log(KPI) - log(lag(KPI))),
#         lndp.v2 = c(NA, 100*diff(log(KPI))))

# grafisk fremstilling prosentvis endring av pizza og paier

#tabell_storfe %>%
#  filter(dato >= "1979-02-01") %>%
#  mutate(positiv=lndp.v2 >= 0) %>% 
#  ggplot(aes(x=dato, y=lndp.v2, fill=positiv)) +
#  geom_col(position = "identity") +
#  scale_fill_manual(values = c("dark blue", "dark red"), guide = FALSE) +
#  labs(title="Prosentvis endring i konsumprisindeksen for Storfe- og kalvekjøtt \n Totalindeks (2015=100)",
#       x = " ",
#       y = "Prosent") +
#  theme_bw()


#-------------------------------------------------------------------------------
# KPI for Fryst Fisk

# Lager en ny dataFrame som skal kun inneholde Storfe og kalvekjøtt, som skal sammenlignes med listen på VG matbørs. 
#fisk_18_20 <- kpi_mat_18_20[c(1225:1296), ] %>% 
#  as.data.frame(row.names = 1:nrow(.)) 

# prosentvis endring i en indeks -- hentet fra selve oppgaven.
#tabell_fisk <- fisk_18_20 %>%
#  filter(statistikkvariabel != "12-måneders endring (prosent)") %>% 
#  separate(måned, into = c("år", "måned"), sep="M") %>% 
#  mutate(dato = ymd(paste(år, måned, "1"))) %>% 
#  select(dato, statistikkvariabel, value) %>% 
#  pivot_wider(names_from = "statistikkvariabel") %>% 
#  rename(KPI = "Konsumprisindeks (2015=100)",
#         SSB_dp ="Månedsendring (prosent)") %>% 
#  mutate(dp  = 100*(KPI - lag(KPI))/lag(KPI),
#         lndp.v1 = 100*(log(KPI) - log(lag(KPI))),
#         lndp.v2 = c(NA, 100*diff(log(KPI))))

# grafisk fremstilling prosentvis endring av pizza og paier

#tabell_fisk %>%
#  filter(dato >= "1979-02-01") %>%
#  mutate(positiv=lndp.v2 >= 0) %>% 
#  ggplot(aes(x=dato, y=lndp.v2, fill=positiv)) +
#  geom_col(position = "identity") +
#  scale_fill_manual(values = c("dark blue", "dark red"), guide = FALSE) +
#  labs(title="Prosentvis endring i konsumprisindeksen for Fryst Fisk \n Totalindeks (2015=100)",
#       x = " ",
#       y = "Prosent") +
#  theme_bw()







#-------------------------------------------------------------------------------
# KPI for Ris

# Lager en ny dataFrame som skal kun inneholde svinekjøtt, som skal sammenlignes med listen på VG matbørs. 
ris_18_10 <- kpi_mat_18_20[c(1:72), ] %>% 
  as.data.frame(row.names = 1:nrow(.)) 

# prosentvis endring i en indeks -- hentet fra selve oppgaven.
tabell_ris <- ris_18_10 %>%
  filter(statistikkvariabel != "12-måneders endring (prosent)") %>% 
  separate(måned, into = c("år", "måned"), sep="M") %>% 
  mutate(dato = ymd(paste(år, måned, "1"))) %>% 
  select(dato, statistikkvariabel, value) %>% 
  pivot_wider(names_from = "statistikkvariabel") %>% 
  rename(KPI = "Konsumprisindeks (2015=100)",
         SSB_dp ="Månedsendring (prosent)") %>% 
  mutate(dp  = 100*(KPI - lag(KPI))/lag(KPI),
         lndp.v1 = 100*(log(KPI) - log(lag(KPI))),
         lndp.v2 = c(NA, 100*diff(log(KPI))))

# grafisk fremstilling prosentvis endring av pizza og paier

tabell_ris %>%
  filter(dato >= "1979-02-01") %>%
  mutate(positiv=lndp.v2 >= 0) %>% 
  ggplot(aes(x=dato, y=lndp.v2, fill=positiv)) +
  geom_col(position = "identity") +
  scale_fill_manual(values = c("dark blue", "dark red"), guide = FALSE) +
  labs(title="Prosentvis endring i konsumprisindeksen for Ris \n Totalindeks (2015=100)",
       x = " ",
       y = "Prosent") +
  theme_bw()



# Henter ut data fra VGmatbørs for ris
# Okt 2018 - delsum for Toro boil in bag, 960 g
delsum_okt_18 <- data.frame(ris = c(24.40, 24.40, 24.40, 26.30, 37.50, 38.90))
mean(delsum_okt_18$ris)



# Okt 2019 - delsum for Toro boil in bag, 960 g
delsum_okt_19 <- data.frame(ris = c(39.90, 39.90, 39.90, 42.90, 42.30, 43.90))
mean(delsum_okt_19$ris)



# Sep 2020 - delsum for Toro boil in bag, 960 g
delsum_sep_20 <- data.frame(ris = c(39.90, 39.90, 39.90, 42.90, 42.90, 45.90))
mean(delsum_sep_20$ris)




# kalkulerer differansen mellom 18, 19 og 2020 tallene.

# kalkulerer differansen mellom 2018 og 2019 tallene og ser at vi ender opp med en økt pris på 41.44%.
prisøkning_18_19 = (mean(delsum_okt_19$ris) / mean(delsum_okt_18$ris)) * 100
prisøkning_18_19


# kalkulerer differansen mellom 2019 og 2020 tallene og ser at vi ender opp med en økt pris på 1.045%.
prisøkning_19_20 = (mean(delsum_sep_20$ris) / mean(delsum_okt_19$ris)) * 100
prisøkning_19_20


# kalkulerer differansen mellom 2018 og 2020 tallene og ser at vi ender opp med en økt pris på 42.92%.
prisøkning_18_20 = (mean(delsum_sep_20$ris) / mean(delsum_okt_18$ris)) * 100
prisøkning_18_20







#-------------------------------------------------------------------------------
# prosentvis endring i en indeks -- hentet fra selve oppgaven.

tibble(tabell)


tabell2 <-tabell %>%
  filter(statistikkvariabel != "12-måneders endring (prosent)") %>% 
  separate(måned, into = c("år", "måned"), sep="M") %>% 
  mutate(dato = ymd(paste(år, måned, "1"))) %>% 
  select(dato, statistikkvariabel, value) %>% 
  pivot_wider(names_from = "statistikkvariabel") %>% 
  rename(KPI = "Konsumprisindeks (2015=100)",
         SSB_dp ="Månedsendring (prosent)") %>% 
  mutate(dp  = 100*(KPI - lag(KPI))/lag(KPI),
         lndp.v1 = 100*(log(KPI) - log(lag(KPI))),
         lndp.v2 = c(NA, 100*diff(log(KPI))))

head(tabell2)

# hentet fra selve oppgaven.

tabell2 %>%
  filter(dato >= "1979-02-01") %>%
  mutate(positiv=lndp.v2 >= 0) %>% 
  ggplot(aes(x=dato, y=lndp.v2, fill=positiv)) +
  geom_col(position = "identity") +
  scale_fill_manual(values = c("dark blue", "dark red"), guide = FALSE) +
  labs(title="Prosentvis endring i konsumprisindeksen \n Totalindeks (2015=100)",
       x = " ",
       y = "Prosent") +
  theme_bw()


#-------------------------------------------------------------------------------


# Oppgave 4 - Finn ut hvor mange måneder det er positive endringer, der Delta KPI t >= 0, sammenlignet med negative endringer. 
# Beregn også dette forholdet i prosent.



## Han har allerede kodet det for oss, se nedfor?



tabell2 %>%
  filter(dato >= "1979-02-01") %>%
  mutate(positiv=lndp.v2 >= 0) %>% 
  mosaic::tally(.$lndp.v2 >= 0, data=.)

tabell2 %>%
  filter(dato >= "1979-02-01") %>%
  mutate(positiv=lndp.v2 >= 0) %>% 
  mosaic::tally(.$lndp.v2 >= 0, format="percent", data=.)

# Den kumulative prosentvise endringen viser hvor mye prisene har økt siden januar 1979.

tabell2 %>%
  filter(dato >= "1979-02-01") %>%
  select(dato, lndp.v2) %>% 
  mutate(kumulativKPI=cumsum(lndp.v2)) %>% 
  ggplot(aes(x=dato, y=kumulativKPI)) +
  geom_line(col="dark green") +
  labs(title="Kumulativ endring i konsumprisindeksen \n Totalindeks (2015=100)",
       x = " ",
       y = "Prosent") +
  theme_bw()


#-------------------------------------------------------------------------------

# oppgave 5 - Finn ut hvor mange måneder det gikk før den kumulative endringen i konsumprisindeksen var henholdsvis 50, 100 og 150 prosent.

# lager en egen tabell for endringen mellom 50 og 100.

test <- tabell2 %>% 
  filter(dato >= "1979-02-01") %>%
  select(dato, lndp.v2) %>% 
  mutate(kumulativKPI=cumsum(lndp.v2))

# Benytter tibble for å se at det er 148 rows med data som tilsvarer at det er 148 måneder mellom 50 og 100%.  

filtrert_50_100 <- test %>% 
  filter(kumulativKPI >= 50 & kumulativKPI <= 100)
tibble(filtrert_50_100)


# benytter samme fremgangsmåte for å se mellom 100 og 150. Ser at det er 291 måneder i mellom 100 og 150. 

filtrert_100_150 <- test %>% 
  filter(kumulativKPI >= 100 & kumulativKPI <= 150)
tibble(filtrert_100_150)


# Til slutt kan vi se mellom 50 og 150. I denne har det gått 439 måneder fra den var på 50 til den nådde 150 som stemmer overens
# hvis vi summerer antall måneder fra de tidligere utregningene. 

filtrert_50_150 <- test %>% 
  filter(kumulativKPI >= 50 & kumulativKPI <= 150)
tibble(filtrert_50_150)
