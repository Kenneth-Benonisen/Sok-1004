## ----setup, include=FALSE-----------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(comment=NA)


## -----------------------------------------------------------------------------------------------------------------------------------------------
library(WDI)
library(tidyverse)


## -----------------------------------------------------------------------------------------------------------------------------------------------
imports <- WDIsearch('imports') %>% as_tibble()
imports


## -----------------------------------------------------------------------------------------------------------------------------------------------
df_import <- WDI(indicator = "NE.IMP.GNFS.CD", country = "all")
head(df_import)


## -----------------------------------------------------------------------------------------------------------------------------------------------
df_import %>%
  filter(country=="Norway") %>% 
  rename(import=NE.IMP.GNFS.CD,
         år=year) %>%
  mutate(import=import/1e9) %>% 
  ggplot(aes(x=år, y=import)) +
  geom_line(col="dark blue") +
  labs(title="Norsk import av varer og tjenester \n (nominelle tall)",
       x =" ",
       y = "milliarder US$") +
  theme_bw()




## ----------------------------------------------------------------------------------------------------------------------------------------------- 
# Oppgave 1 - Beregn prosentvis endring i import fra ett år til det neste. Mellom hvilke år var det størst økning/nedgang?

endringimp <- df_import %>%
  filter(country == "Norway") %>%
  rename(import=NE.IMP.GNFS.CD,
         år=year) %>%
  mutate(prosIMP = 100*(import - lag(import))/lag(import)) %>%
  mutate(import=import/1e9) %>%
  arrange(desc(prosIMP))
tibble(endringimp[1:5, ])
tibble(endringimp[47:51, ])

# Den største økningen er i år 2008 med en prosentvis økning på 24.7% og den største nedgangen er i 1972 hvor det oppsto en nedgang på -30.3%. 

## -----------------------------------------------------------------------------------------------------------------------------------------------
exports <- WDIsearch('exports') %>% as_tibble()
exports


## -----------------------------------------------------------------------------------------------------------------------------------------------
df_export <- WDI(indicator = "NE.EXP.GNFS.CD", country = "all")
head(df_export)


## -----------------------------------------------------------------------------------------------------------------------------------------------
df_export %>%
  filter(country=="Norway") %>% 
  rename(eksport=NE.EXP.GNFS.CD,
         år=year) %>%
  mutate(eksport=eksport/1e9) %>% 
  ggplot(aes(x=år, y=eksport)) +
  geom_line(col="dark red") +
  labs(title="Norsk eksport av varer og tjenester \n (nominelle tall)",
       x =" ",
       y = "milliarder US$") +
  theme_bw()



## -----------------------------------------------------------------------------------------------------------------------------------------------
# Oppgave 2 - Beregn prosentvis endring i eksport fra ett år til det neste. Mellom hvilke år var det størst økning/nedgang? 
# Følger endringer i eksport og import det samme mønsteret?

endringeks <- df_export %>%
  filter(country == "Norway") %>%
  rename(eksport=NE.EXP.GNFS.CD,
         år=year) %>%
  mutate(prosEKS = 100*(eksport - lag(eksport))/lag(eksport)) %>%
  mutate(eksport=eksport/1e9) %>%
  arrange(desc(prosEKS))
tibble(endringeks[1:5, ])
tibble(endringeks[47:51, ])


# Det var størst eksport i 2008 hvor det lå på 40.1% og den største nedgangen fant vi i året 1972 som hadde -28.0%. Det kan se ut til at det er en
# viss sammenheng mellom eksport og import ettersom de årene har både størst økning / reduksjon i handelsbalansen. 


## -----------------------------------------------------------------------------------------------------------------------------------------------
dframe <- left_join(df_import, df_export, by = c("iso2c", "country", "year"))
head(dframe)


## -----------------------------------------------------------------------------------------------------------------------------------------------
dframe %>%
  filter(country=="Norway") %>% 
  rename(import=NE.IMP.GNFS.CD,
         eksport=NE.EXP.GNFS.CD,
         år=year) %>%
  mutate(import=import/1e9,
         eksport=eksport/1e9) %>% 
  select(år, import, eksport) %>% 
  pivot_longer(-år, names_to="aktivitet", values_to="verdi") %>% 
  ggplot(aes(x=år, y=verdi, col=aktivitet)) +
  geom_line() +
  scale_color_manual(values=c("dark red", "dark blue")) +
  labs(title="Norsk eksport og import av varer og tjenester \n (nominelle tall)",
       x =" ",
       y = "milliarder US$") +
  theme_bw()


## -----------------------------------------------------------------------------------------------------------------------------------------------
df_export %>%
  filter(country %in% c("Norway","Sweden")) %>% 
  rename(eksport=NE.EXP.GNFS.CD,
         land=country,
         år=year) %>%
  mutate(eksport=eksport/1e9) %>% 
  ggplot(aes(x=år, y=eksport, col=land)) +
  geom_line() +
  labs(title="Eksport av varer og tjenester \n (nominelle tall)",
       x =" ",
       y = "milliarder US$") +
  theme_bw()




## -----------------------------------------------------------------------------------------------------------------------------------------------
# Oppgave 3 - Benytt funksjonen dplyr::recode() til å endre til norske landnavn i figuren over.

df_export %>%
  filter(country %in% c("Norway","Sweden")) %>% 
  rename(eksport=NE.EXP.GNFS.CD,
         land=country,
         år=year) %>%
  mutate(eksport=eksport/1e9) %>%
  mutate(land=recode(land, "Norway"="Norge", "Sweden"="Sverige")) %>%  
  ggplot(aes(x=år, y=eksport, col=land)) +
  geom_line() +
  labs(title="Eksport av varer og tjenester \n (nominelle tall)",
       x =" ",
       y = "milliarder US$") +
  theme_bw()


## -----------------------------------------------------------------------------------------------------------------------------------------------
# Oppgave 4 - Lag en figur som viser den kumulative prosentvise endringen i eksport og import for Norge og Sverige.

df_nordic <- dframe 

df_nordic <- df_nordic %>% 
  filter(country %in% c("Norway","Sweden")) 
tibble(df_nordic)


## -----------------------------------------------------------------------------------------------------------------------------------------------
# Først ordner vi kumulative prosentivse endringer for eksport og import tilknyttet de svenske verdiene. 

endring_imp_SWE <- df_import %>%
  filter(country == "Sweden") %>%
  rename(import=NE.IMP.GNFS.CD,
         år=year) %>%
  mutate(prosIMP = 100*(import - lag(import))/lag(import)) %>%
  mutate(import=import/1e9) %>%
  arrange(desc(prosIMP))
tibble(endring_imp_SWE)


endring_eks_SWE <- df_export %>%
  filter(country == "Sweden") %>%
  rename(eksport=NE.EXP.GNFS.CD,
         år=year) %>%
  mutate(prosEKS = 100*(eksport - lag(eksport))/lag(eksport)) %>%
  mutate(eksport=eksport/1e9) %>%
  arrange(desc(prosEKS))
tibble(endring_eks_SWE)



## -----------------------------------------------------------------------------------------------------------------------------------------------
# Deretter kombinerer vi Norsk og Svensk import og eksport til en tabell. 


combined_df_NO_SWE_IMP <- bind_rows(endring_imp_SWE, endringimp) %>% 
  select(år, country, prosIMP)
tibble(combined_df_SWE)


combined_df_NO_SWE_EKS <- bind_rows(endring_eks_SWE, endringeks) %>% 
  select(år, country, prosEKS)
tibble(combined_df_SWE)


## -----------------------------------------------------------------------------------------------------------------------------------------------
# Først nå kan vi plotte import og eksport for Norsk og Svenske verdier. 


combined_df_NO_SWE_EKS %>%
  filter(country %in% c("Norway","Sweden")) %>% 
  rename(land=country) %>%
  mutate(eksport=eksport/1e9) %>%
  mutate(land=recode(land, "Norway"="Norge", "Sweden"="Sverige")) %>%  
  ggplot(aes(x=år, y=prosEKS, col=land)) +
  geom_line() +
  labs(title="Eksport av varer og tjenester \n (kumulative prosentvise endring)",
       x ="År",
       y = "Midlertidig tekst") +
  theme_bw()


## -----------------------------------------------------------------------------------------------------------------------------------------------



combined_df_NO_SWE_IMP %>%
  filter(country %in% c("Norway","Sweden")) %>% 
  rename(land=country) %>%
  mutate(land=recode(land, "Norway"="Norge", "Sweden"="Sverige")) %>%  
  ggplot(aes(x=år, y=prosIMP, col=land)) +
  geom_line() +
  labs(title="Import av varer og tjenester \n (kumulative prosentvise endring)",
       x ="År",
       y = "Midlertidig tekst") +
  theme_bw()
