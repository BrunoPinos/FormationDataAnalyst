library(tidyverse)
library(stringr)
library(DT)

###3333333333333333333333333333333333333333333333

dispali_kcal<-read.csv("dispali_kcal.csv", encoding = "UTF-8")
pop2013_corrigee<-read.csv("pop2013_corrigee.csv", encoding = "UTF-8")

dispali_kcal_totale<-
  full_join(dispali_kcal,
            select(pop2013_corrigee, Zone, Valeur),by = "Zone")%>%
  mutate(Code.Élément = "À déterminer",
         Élément = "Disponibilité alimentaire totale",
         Unité = "10^6 Kcal/jour",
         Valeur = round(Valeur.x*Valeur.y/1000,0))%>%
  drop_na()%>%
  select(c(1:11,16,13:14))


dispali_proteine<-read.csv("dispali_proteine.csv", encoding = "UTF-8")

dispali_proteine_totale<-full_join(dispali_proteine,
                                   select(pop2013_corrigee, Zone, Valeur),
                                   by = "Zone")%>%
  mutate(Code.Élément = "À déterminer",
         Élément = "Disponibilité totale de protéines en quantité",
         Unité = "tonne/jour",
         Valeur = round(Valeur.x*Valeur.y/1000,0))%>%
  drop_na()%>%
  select(c(1:11,16,13:14))

###444444444444444444444444444444444444444444444444444444

nourriture2013<-read.csv("nourriture2013.csv", encoding = "UTF-8")%>%
  filter(Valeur >= 10)


ratio_e_p<-full_join(filter(dispali_kcal_totale, Valeur >= 1),
                     select(nourriture2013, Zone, Produit, Valeur),
                     by = c("Zone","Produit"))%>%
  mutate(Code.Élément = "À déterminer",
         Élément = "ratio (énergie/poids)",
         Unité = "kcal/kg",
         Valeur = round(Valeur.x*365/Valeur.y,0))%>%
  drop_na()%>%
  select(c(1:11,16,13:14))

ratio_p_p<-
  full_join(dispali_proteine_totale,
            select(nourriture2013, Zone, Produit, Valeur),
            by = c("Zone","Produit"))%>%
  mutate(Code.Élément = "À déterminer",
         Élément = "(poids de protéines/poids total)*100",
         Unité = "pourcentage",
         Valeur = round(
           (Valeur.x*365/(Valeur.y*1000))*100,0))%>%
  drop_na()%>%
  select(c(1:11,16,13:14))

###5555555555555555555555555555555555555555555555

ratio_e_p<-
  filter(ratio_e_p, ratio_e_p$Millier_tonnes>=1)

somme_ratio_e_p<-ratio_e_p %>% 
  group_by(Produit) %>% 
  summarise(Kcal=sum(Kcal),Millier_tonnes=sum(Millier_tonnes))


somme_ratio_e_p$Kcal_kg<-
  somme_ratio_e_p$Kcal*365/(somme_ratio_e_p$Millier_tonnes*10^6)

