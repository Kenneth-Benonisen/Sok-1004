## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(comment=NA)


## --------------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(rvest)
library(PxWebApiData)


## --------------------------------------------------------------------------------------------------------------------------------
webpage <- read_html("https://www.ssb.no/a/histstat/aarbok/ht-0901-bnp.html")
tabell <- html_table(html_nodes(webpage, "table")[[2]])



## --------------------------------------------------------------------------------------------------------------------------------
tabell <- tabell %>% drop_na()


## --------------------------------------------------------------------------------------------------------------------------------
names(tabell) <- c("År", "BNP", "BNP_endring",
                   "BNP_percap", "BNP_percap_endring")

tabell <- as_tibble(tabell)

tabell


## --------------------------------------------------------------------------------------------------------------------------------
tabell <-
  tabell %>% 
  mutate(BNP=str_replace_all(BNP, " ", ""),
         BNP_endring=na_if(BNP_endring, ""),
         BNP_percap_endring=na_if(BNP_percap_endring, ""),
         BNP_endring=str_replace(BNP_endring, ",","."),
         BNP_percap_endring=str_replace(BNP_percap_endring, ",",".")) %>% 
  mutate_if(is.character, as.numeric)

tabell


# oppgave 1 - lag et plot med BNP per innbygger i perioden.

#tabell %>%
#  filter(År >= 1865) %>%
#  ggplot(aes(x=År, y=BNP_percap)) +
#  scale_y_continuous(labels = scales::comma) +
#  geom_line(color="dark blue") +
#  labs(title="Bruttonasjonalprodukt per innbygger",
#       x=" ",
#       y="BNP pr innbygger") +
#  theme_bw()


tabell %>%
  ggplot(aes(x=År)) +
  geom_line(aes(y=BNP_percap), color="dark red") +
  scale_y_continuous(labels = scales::comma) +
  labs(title="BNP per innbygger fra 1865 til 2011",
       x="År",
       y="Kroner") +
  theme_bw() 


# oppgave 2 - Gitt disse dataene, er det en annen viktig variabel som vi kan beregne ut fra dem, i så fall hvilken?
# Bruk dplyr::mutate() funksjonen til å beregn denne størrelsen.

tabell %>%
  mutate(befolkningen = 1000000*(BNP /BNP_percap)) %>%
  ggplot(aes(x=År)) +
  geom_line(aes(y=befolkningen), color="dark Blue") +
  geom_line(aes(y=BNP_percap), color="dark red") +
  scale_y_continuous(labels = scales::comma) +
  labs(title="Innbyggere(blå linje) BNP per innbygger (rød linje)",
       x="År",
       y=" ") +
  theme_bw() 


# oppgave 3 - Denne tabellen inneholder årlige BNP data frem til 2011. I det forrige caset så vi på nyere månedlige BNP tall.
# I denne oppgaven skal du spleise de to BNP seriene til en lang tabell, per år.
# Vi trenger ikke justere BNP-tallene ettersom begge tabellene har 2005 som basisår.

# Benytt funksjonen dplyr::bind_rows().




## -----------------------------------------------------------------------------------------------------------------------------------------------
variabler <- ApiData("http://data.ssb.no/api/v0/no/table/09842", returnMetaFrames = TRUE)
names(variabler)


## -----------------------------------------------------------------------------------------------------------------------------------------------
verdier <- ApiData("https://data.ssb.no/api/v0/no/table/09842/", returnMetaData = TRUE)
verdier


## -----------------------------------------------------------------------------------------------------------------------------------------------
case.1.tabell <- ApiData("https://data.ssb.no/api/v0/no/table/09842/",
                         Tid = paste(1970:2019),
                         ContentsCode = "BNP")


## -----------------------------------------------------------------------------------------------------------------------------------------------
bnp <- case.1.tabell[[1]]
tibble(bnp)





## -----------------------------------------------------------------------------------------------------------------------------------------------
bnp <- bnp %>%
  mutate(år=parse_number(år)) %>%
  rename(BNP2=value)
tibble(bnp)

## -----------------------------------------------------------------------------------------------------------------------------------------------

bnp <- bnp %>%
  select(år, BNP2)
tibble(bnp)


## -----------------------------------------------------------------------------------------------------------------------------------------------
combined_df <- bind_rows(tabell, bnp)
tibble(combined_df)

## -----------------------------------------------------------------------------------------------------------------------------------------------
combined_df %>%
  mutate_if(is.character, ~as.numeric(.)) %>%
  mutate_if(is.integer, ~as.numeric(.))
tibble(combined_df)            

## -----------------------------------------------------------------------------------------------------------------------------------------------

#combined_df %>%
#  ggplot(aes(x=År, y=BNP)) +
#  geom_line(color="red") +
#  scale_y_continuous(labels = scales::comma) +
#  labs(title="Bruttonasjonalprodukt - BNP \n (kr per innbygger)",
#       x =" ",
#       y = "kr per innbygger") +
#  theme_bw()

## -----------------------------------------------------------------------------------------------------------------------------------------------

#combined_df %>%
#  ggplot(aes(x=år, y=BNP)) +
#  geom_line(color="dark blue") +
#  scale_y_continuous(labels = scales::comma) +
#  labs(title="Bruttonasjonalprodukt - BNP \n (kr per innbygger)",
#       x =" ",
#       y = "kr per innbygger") +
#  theme_bw()

## -----------------------------------------------------------------------------------------------------------------------------------------------
combined_df %>%
  ggplot(aes(x)) +
  geom_line(aes(x=År, y=BNP), color = "dark red") +
  geom_line(aes(x=År, y=BNP_percap), color="dark blue") +
  geom_line(aes(x=år, y=BNP2), color="dark green") +
  scale_y_continuous(labels = scales::comma) +
  labs(title="Bruttonasjonalprodukt - BNP \n (kr per innbygger)",
       x =" ",
       y = "kr per innbygger") +
  theme_bw()
## -----------------------------------------------------------------------------------------------------------------------------------------------

# combined_df %>% as_tibble() %>% print(n=197)