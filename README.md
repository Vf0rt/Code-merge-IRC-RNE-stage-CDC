# Code-merge-IRC-RNE-stage-CDC
Ce code jointure final base de donnée du ministère de l'intérieur : Répertoire national des élu(e)s (RNE) et de Ircantec. 
---
title: "Merge RNE - IRC"
output:
  html_document:
    code_folding: hide
    toc: yes
    toc_float: yes
    theme: spacelab
date: "Etude réalisée le `r format(Sys.time(), '%d %B %Y')`"
editor_options: 
  markdown: 
    wrap: 72
---
# 0 : Connexion
```{r setup, echo=TRUE, message=FALSE, warning=FALSE}
library(openxlsx)
library(ggplot2)
library(dplyr)
library(stringr) 
library(data.table)
library(tidyverse)
library(writexl)
library(tidyr)
library(readxl)
library(questionr)
library(stargazer)
library(GGally)
library(broom)
library(readr)
library(gt)
setwd("/working_directoty/users/")
```


# 1 IRC : Extraction

## 1.1 tab_elus_finale_a : Extraction 
```{r, include=FALSE, message=FALSE, warning=FALSE}
tab_elus_finale <- readRDS("working_directory")
```



### 1.1.1 Corr : date 

Problème dans l'importation :   

- Format date non respecté => AAAA - mm -- jj 23:00:00, sol : passer au format date ces variables.    
```{r, echo=TRUE, message=FALSE, warning=FALSE}
tab_elus_finale_a <- tab_elus_finale %>% 
  arrange(dt_deb_indiv, dt_fin_indiv)

tab_elus_finale_a$dt_deb_bcr_11  <- as.Date(tab_elus_finale_a$dt_deb_bcr_11)                      
tab_elus_finale_a$dt_fin_bcr_11  <- as.Date(tab_elus_finale_a$dt_fin_bcr_11)

tab_elus_finale_a$dt_deb_bcr_21  <- as.Date(tab_elus_finale_a$dt_deb_bcr_21)
tab_elus_finale_a$dt_fin_bcr_21  <- as.Date(tab_elus_finale_a$dt_fin_bcr_21)
tab_elus_finale_a$dt_deb_bcr_22  <- as.Date(tab_elus_finale_a$dt_deb_bcr_22)
tab_elus_finale_a$dt_fin_bcr_22  <- as.Date(tab_elus_finale_a$dt_fin_bcr_22)

tab_elus_finale_a$dt_deb_bcr_31  <- as.Date(tab_elus_finale_a$dt_deb_bcr_31)
tab_elus_finale_a$dt_fin_bcr_31  <- as.Date(tab_elus_finale_a$dt_fin_bcr_31)
tab_elus_finale_a$dt_deb_bcr_32  <- as.Date(tab_elus_finale_a$dt_deb_bcr_32)
tab_elus_finale_a$dt_fin_bcr_32  <- as.Date(tab_elus_finale_a$dt_fin_bcr_32)
tab_elus_finale_a$dt_deb_bcr_33  <- as.Date(tab_elus_finale_a$dt_deb_bcr_33)
tab_elus_finale_a$dt_fin_bcr_33  <- as.Date(tab_elus_finale_a$dt_fin_bcr_33)

tab_elus_finale_a$dt_deb_bcr_41  <- as.Date(tab_elus_finale_a$dt_deb_bcr_41)
tab_elus_finale_a$dt_fin_bcr_41  <- as.Date(tab_elus_finale_a$dt_fin_bcr_41)
tab_elus_finale_a$dt_deb_bcr_42  <- as.Date(tab_elus_finale_a$dt_deb_bcr_42)
tab_elus_finale_a$dt_fin_bcr_42  <- as.Date(tab_elus_finale_a$dt_fin_bcr_42)
tab_elus_finale_a$dt_deb_bcr_43  <- as.Date(tab_elus_finale_a$dt_deb_bcr_43)
tab_elus_finale_a$dt_fin_bcr_43  <- as.Date(tab_elus_finale_a$dt_fin_bcr_43)
tab_elus_finale_a$dt_deb_bcr_44  <- as.Date(tab_elus_finale_a$dt_deb_bcr_44)
tab_elus_finale_a$dt_fin_bcr_44  <- as.Date(tab_elus_finale_a$dt_fin_bcr_44)

tab_elus_finale_a$dt_deb_indiv  <- as.Date(tab_elus_finale_a$dt_deb_indiv)
tab_elus_finale_a$dt_fin_indiv  <- as.Date(tab_elus_finale_a$dt_fin_indiv)
tab_elus_finale_a$date_deces_corr  <- as.Date(tab_elus_finale_a$date_deces_corr)
tab_elus_finale_a$date_naiss  <- as.Date(tab_elus_finale_a$date_naiss)

```

### 1.1.2 Corr : départements
```{r, echo=TRUE, message=FALSE, warning=FALSE}

#exclusion des COM
excluded_deps <- c("975", "987", "988")
tab_elus_finale_a <- tab_elus_finale_a %>% 
  filter(!departement_11 %in% excluded_deps)#859,789
#nrow(tab_elus_finale_a)

tab_elus_finale_a <- tab_elus_finale_a %>% 
  filter(!departement_21 %in% excluded_deps)#859,775
#nrow(tab_elus_finale_a)

tab_elus_finale_a <- tab_elus_finale_a %>% 
  filter(!departement_22 %in% excluded_deps)#859,771

tab_elus_finale_a <- tab_elus_finale_a %>% 
  filter(!departement_31 %in% excluded_deps)

tab_elus_finale_a <- tab_elus_finale_a %>% 
  filter(!departement_32 %in% excluded_deps)

tab_elus_finale_a <- tab_elus_finale_a %>% 
  filter(!departement_33 %in% excluded_deps)

tab_elus_finale_a <- tab_elus_finale_a %>% 
  filter(!departement_41 %in% excluded_deps)

tab_elus_finale_a <- tab_elus_finale_a %>% 
  filter(!departement_42 %in% excluded_deps)

tab_elus_finale_a <- tab_elus_finale_a %>% 
  filter(!departement_43 %in% excluded_deps)

tab_elus_finale_a <- tab_elus_finale_a %>% 
  filter(!departement_44 %in% excluded_deps)

nrow(tab_elus_finale_a)#853,547 on ne perd plus de ligne au delà de 2 bcr_emp


#Des NA qui trainent

#test <- tab_elus_finale_a %>% 
#  filter(nb_bcr_emp == 1 & is.na(departement_11))#118 : ce sont des communes de Taïti => les supprimer 

tab_elus_finale_a <- tab_elus_finale_a %>% 
  filter((nb_bcr_emp == 1 & !is.na(departement_11)) | (nb_bcr_emp > 1 & is.na(departement_11))) #Les indiv qui ont 1 seul bcr_emp et pas de vide, et les indiv qui ont plusieurs bcr_emp et du vide dans departement 11.
                                                                                                    
nrow(tab_elus_finale_a)#853,429

#859771 - 859653 = 118 # Parfait

#test <- tab_elus_finale_a %>% 
#  filter(nb_bcr_emp == 2 & (is.na(departement_21) | is.na(departement_22)))#3 :que pour 22 et = COMMUNE ALGER => supprimer 

tab_elus_finale_a <- tab_elus_finale_a %>% 
  filter(!num_contrat_affil %in% c("1AD23GMP", "1AD26SWL", "1AE60NBN")) 

nrow(tab_elus_finale_a)#853,426 : OK

#test <- tab_elus_finale_a %>% 
#  filter(nb_bcr_emp == 3 & (is.na(departement_31) | is.na(departement_32) | is.na(departement_33)))#0 : Ok

#test <- tab_elus_finale_a %>% 
#  filter(nb_bcr_emp == 4 & (is.na(departement_41) | is.na(departement_42) | is.na(departement_43) | is.na(departement_44)))#0 : Ok




#Vérif corse 

#test <- tab_elus_finale_a %>% 
#  filter(departement_11 == "2A")#2,005
#test <- tab_elus_finale_a %>% 
#  filter(departement_11 == "2B")#3,139
#
#
#test <- tab_elus_finale_a %>% 
#  filter(departement_21 == "2A")#21
#test <- tab_elus_finale_a %>% 
#  filter(departement_21 == "2B")#31
#
#test <- tab_elus_finale_a %>% 
#  filter(departement_44 == "2A")#0 Pour les bcr_emp >3 aucun n'a changé en corse 2A (j'ai fais tous les test (31,32,33,41,42,43,44) et nrow = 0)
#
#test <- tab_elus_finale_a %>% 
#  filter(departement_32 == "2B")#2
#
#test <- tab_elus_finale_a %>% 
#  filter(departement_33 == "2B")#1
#
#test <- tab_elus_finale_a %>% 
#  filter(departement_44 == "2B")#0


#test <- tab_elus_finale_a %>% 
#  filter(departement_11 == 20)#361
#
#test <- tab_elus_finale_a %>% 
#  filter(departement_21 == 20)#3
#
#test <- tab_elus_finale_a %>% 
#  filter(departement_22 == 20)#2

#Pas besoin de modifier 

```

## 1.2 Création matricule
xx = année : 21 pour 2021 par exemple.

__assembl_ID__ : nom_elu_xx_regul, prenom_elu_xx_regul, sexe_xx, Annee_naiss_xx, Mois_naiss_xx, Jour_naiss_xx

__assembl_ID_avecDep__ : departement_xx, nom_elu_xx_regul, prenom_elu_xx_regul, sexe_xx, Annee_naiss_xx, Mois_naiss_xx, Jour_naiss_xx 

### 1.2.1 Verif variables
```{r, echo=TRUE, message=FALSE, warning=FALSE}
test <- tab_elus_finale_a %>% 
  filter(is.na(date_naiss) & is.na(nom_usuel) & is.na(nom_naiss))#0 : parfait

test <- tab_elus_finale_a %>% 
  filter(is.na(nom_usuel) & is.na(nom_naiss))#0 : parfait

test <- tab_elus_finale_a %>% 
  filter(is.na(date_naiss))#9,612 : Ces individus ne se rattacheront à rien => pas besoin de les virer étanr donné que l'on va faire plusieurs fusion de IRC vers RNE.
```

### 1.2.2 date naiss

Création variables pour dates de naissances
```{r, echo=TRUE, message=FALSE, warning=FALSE}

tab_elus_finale_a$jour_naiss  <- substr(tab_elus_finale_a$date_naiss, 9, 10)
tab_elus_finale_a$mois_naiss  <- substr(tab_elus_finale_a$date_naiss, 6, 7)
tab_elus_finale_a$annee_naiss <- substr(tab_elus_finale_a$date_naiss, 1, 4)

```

### 1.2.3 Corr : prénoms

Erreur dans certains prénoms (regarder les tests sous les # pour s'en persuader).

- Ceux qui ont que 1 ou 2 characters & un _prenom_usuel_ différent de _prenom_etat_civil_premier_ => prendre _prenom_etat_civil_premier_.

- Ceux qui ont que 3 characters & un _prenom_usuel_ différent de _prenom_etat_civil_premier_ & le début de leurs prénoms qui commencent de la même manière => prendre _prenom_etat_civil_premier_.  

- On observe que dès fois, prenom_usuel = prenom_etat_civil_premier + prenom_etat_civil_second MAIS !! Lorsque je regade comment ces individus apparaissent dans RNE (grace à test5 et test6) c'est bien leurs nom "composé qui marche... => ne rien changer pour test4

Il faut aussi faire une vraie gestion des prénoms composés: JEAN-CLAUDE vs JEAN CLAUDE. En fait, au sein de la même base (IRC ici mais aussi dans RNE) les prénoms et noms composés sont dès fois écrit avec un tiret et dès fois non => homogénéiser pour que les bases se rapproche sur _assembl_ID_avecDep_.   
```{r, echo=TRUE, message=FALSE, warning=FALSE}
#test <- tab_elus_finale_a %>%
#  filter(prenom_usuel != prenom_etat_civil_premier) #Problème 
#
#test1 <- tab_elus_finale_a %>% 
#  filter(nchar(prenom_usuel) == 1 & (prenom_usuel != prenom_etat_civil_premier)) # Prendre prenom_etat_civil_premier
#
#test2 <- tab_elus_finale_a %>% 
#  filter(nchar(prenom_usuel) == 2 & (prenom_usuel != prenom_etat_civil_premier)) # Prendre prenom_etat_civil_premier
#
#test3 <- tab_elus_finale_a %>% 
#  filter(nchar(prenom_usuel) == 3 & (prenom_usuel != prenom_etat_civil_premier) & substr(prenom_usuel, 1,3) == substr(prenom_etat_civil_premier, 1,3)) # Prendre prenom_etat_civil_premier
#
#test4 <- tab_elus_finale_a %>% 
#  filter(substr(prenom_usuel, 1,4) == substr(prenom_etat_civil_premier, 1,4) & (prenom_usuel != prenom_etat_civil_premier)) # On observe que prenom_usuel = prenom_etat_civil_premier + prenom_etat_civil_second => prendre prenom_etat_civil_premier aussi 

#test5 <- tab_elus_finale_a %>% 
#  filter(substr(prenom_usuel, 1,4) == substr(prenom_etat_civil_premier, 1,4) & (prenom_usuel != prenom_etat_civil_premier) & dt_fin_indiv #>= as.Date("2014-01-01"))

#test6 <- tab_RNE_finale %>% 
# filter(nom_elu_20_regul == "LAVAL" & prenom_elu_20_regul == "JEAN PIERRE")

#test7 <- tab_RNE_finale %>% 
# filter(nom_elu_20_regul == "BARET" & prenom_elu_20_regul == "JEAN PIERRE")

tab_elus_finale_a$prenom_usuel <- as.character(tab_elus_finale_a$prenom_usuel)
tab_elus_finale_a$prenom_etat_civil_premier <- as.character(tab_elus_finale_a$prenom_etat_civil_premier)


tab_elus_finale_a <- tab_elus_finale_a %>% 
  mutate(prenom_usuel_corr = case_when(
    nchar(prenom_usuel) == 1 & (prenom_usuel != prenom_etat_civil_premier) ~ prenom_etat_civil_premier,
    
    nchar(prenom_usuel) == 2 & (prenom_usuel != prenom_etat_civil_premier) ~ prenom_etat_civil_premier,
    
    nchar(prenom_usuel) == 3 & (prenom_usuel != prenom_etat_civil_premier) & substr(prenom_usuel, 1,3) == substr(prenom_etat_civil_premier, 1,3) ~ prenom_etat_civil_premier,
   
    TRUE ~ prenom_usuel))

#test_bis  <- tab_elus_finale_a %>% 
#  filter(prenom_usuel_corr != prenom_etat_civil_premier) #Souvent on observe que le 2ème nom est utilisé 

#test_bis  <- tab_elus_finale_a %>% 
#  filter(prenom_usuel_corr != prenom_etat_civil_premier & dt_fin_indiv >= as.Date("2014-01-01")

#test <- tab_RNE_finale %>% 
#  filter(prenom_elu_25_regul == "JEAN-LOUIS" & nom_elu_25_regul == "BULAND")

#Correction manuel de quelques prénoms : j'ai vérifié dans RNE comment ils ont été enregistré 
#Able => Abel 
tab_elus_finale_a <- tab_elus_finale_a %>% 
  mutate(prenom_usuel_corr = ifelse(
    prenom_usuel_corr == "ABLE", 
    gsub("ABLE", "ABEL", prenom_usuel_corr),
    prenom_usuel_corr
  ))

#J Louis en Jean Louis
tab_elus_finale_a <- tab_elus_finale_a %>% 
  mutate(prenom_usuel_corr = ifelse(
    prenom_usuel_corr == "J LOUIS", 
    gsub("J LOUIS", "JEAN-LOUIS", prenom_usuel_corr),
    prenom_usuel_corr
  ))

tab_elus_finale_a <- tab_elus_finale_a %>% 
  select(-prenom_usuel) %>% 
  relocate(prenom_usuel_corr, .after = nom_marital)


#Suppression des tirets dans les prénoms et noms, homogénéisons ça avec RNE. Dès fois, JEAU-CLAUDE et dès fois, JEAN CLAUDE => Sois partout avec soit partout sans : partout sans. 

tab_elus_finale_a <- tab_elus_finale_a %>% 
  mutate(prenom_usuel_corr = gsub("-", " ", prenom_usuel_corr),
         nom_usuel = gsub("-", " ", nom_usuel), 
         nom_naiss = gsub("-", " ", nom_naiss),
         nom_marital = gsub("-", " ", nom_marital),
         prenom_etat_civil_premier = gsub("-", " ", prenom_etat_civil_premier),
         prenom_etat_civil_second = gsub("-", " ", prenom_etat_civil_second))

#freq(grepl("-", tab_elus_finale_a$prenom_usuel_corr))
#freq(grepl("-", tab_elus_finale_a$nom_usuel))
#freq(grepl("-", tab_elus_finale_a$nom_naiss))
#freq(grepl("-", tab_elus_finale_a$nom_marital))

#test <- tab_elus_finale_a %>% 
#  filter(prenom_usuel_corr != prenom_etat_civil_premier) 

#Nom marital 
#test <- tab_elus_finale_a %>% 
#  filter(!is.na(nom_marital) & nom_marital != nom_usuel & nom_marital != nom_naiss & dt_fin_indiv >= as.Date("2014-01-01"))

#NOM et PRENOM 
#pour un passage propre (même package que pour RNE)
library(stringi)
tab_elus_finale_a$prenom_usuel_corr <- stri_trans_general(tab_elus_finale_a$prenom_usuel_corr, "Latin-ASCII")
tab_elus_finale_a$prenom_usuel_corr <- toupper(tab_elus_finale_a$prenom_usuel_corr)

tab_elus_finale_a$nom_usuel <- stri_trans_general(tab_elus_finale_a$nom_usuel, "Latin-ASCII")
tab_elus_finale_a$nom_usuel <- toupper(tab_elus_finale_a$nom_usuel)

tab_elus_finale_a$nom_naiss <- stri_trans_general(tab_elus_finale_a$nom_naiss, "Latin-ASCII")
tab_elus_finale_a$nom_naiss <- toupper(tab_elus_finale_a$nom_naiss)

tab_elus_finale_a$nom_marital <- stri_trans_general(tab_elus_finale_a$nom_marital, "Latin-ASCII")
tab_elus_finale_a$nom_marital <- toupper(tab_elus_finale_a$nom_marital)

tab_elus_finale_a$prenom_etat_civil_premier <- stri_trans_general(tab_elus_finale_a$prenom_etat_civil_premier, "Latin-ASCII")
tab_elus_finale_a$prenom_etat_civil_premier <- toupper(tab_elus_finale_a$prenom_etat_civil_premier)

tab_elus_finale_a$prenom_etat_civil_second <- stri_trans_general(tab_elus_finale_a$prenom_etat_civil_second, "Latin-ASCII")
tab_elus_finale_a$prenom_etat_civil_second <- toupper(tab_elus_finale_a$prenom_etat_civil_second)

tab_elus_finale_a$prenom_usuel_corr = str_to_upper(tab_elus_finale_a$prenom_usuel_corr)
tab_elus_finale_a$nom_usuel = str_to_upper(tab_elus_finale_a$nom_usuel)
tab_elus_finale_a$nom_naiss = str_to_upper(tab_elus_finale_a$nom_naiss)
tab_elus_finale_a$nom_marital = str_to_upper(tab_elus_finale_a$nom_marital)
tab_elus_finale_a$prenom_etat_civil_premier = str_to_upper(tab_elus_finale_a$prenom_etat_civil_premier)
tab_elus_finale_a$prenom_etat_civil_second = str_to_upper(tab_elus_finale_a$prenom_etat_civil_second)


#Problème de guillement
tab_elus_finale_a <- tab_elus_finale_a %>%
  mutate(prenom_usuel_corr = gsub("\"", "", prenom_usuel_corr))
tab_elus_finale_a <- tab_elus_finale_a %>%
  mutate(nom_usuel = gsub("\"", "", nom_usuel))
tab_elus_finale_a <- tab_elus_finale_a %>%
  mutate(nom_naiss = gsub("\"", "", nom_naiss))
tab_elus_finale_a <- tab_elus_finale_a %>%
  mutate(nom_marital = gsub("\"", "", nom_marital))
tab_elus_finale_a <- tab_elus_finale_a %>%
  mutate(prenom_etat_civil_premier = gsub("\"", "", prenom_etat_civil_premier))
tab_elus_finale_a <- tab_elus_finale_a %>%
  mutate(prenom_etat_civil_second = gsub("\"", "", prenom_etat_civil_second))

#Gestion des points
#freq(grepl("\\.", tab_elus_finale_a$prenom_usuel_corr))#OK
#freq(grepl("\\.", tab_elus_finale_a$nom_usuel))#OK
#freq(grepl("\\.", tab_elus_finale_a$nom_naiss))#OK
#freq(grepl("\\.", tab_elus_finale_a$nom_marital))#OK
#freq(grepl("\\.", tab_elus_finale_a$prenom_etat_civil_premier))#OK
#freq(grepl("\\.", tab_elus_finale_a$prenom_etat_civil_second))#OK
```

### 1.2.3 ID sans dép
```{r, echo=TRUE, message=FALSE, warning=FALSE}
#Matricule sans code commune avec noms et prénom usuel
tab_elus_finale_a$assembl_ID_usuel = paste0(tab_elus_finale_a$nom_usuel,
                                   " - ", tab_elus_finale_a$prenom_usuel_corr,
                                   " - ", tab_elus_finale_a$code_sexe,
                                   " - ", tab_elus_finale_a$annee_naiss,
                                   " - ",tab_elus_finale_a$mois_naiss,
                                   " - ",tab_elus_finale_a$jour_naiss)

tab_elus_finale_a$assembl_ID_naiss = paste0(tab_elus_finale_a$nom_naiss,
                                   " - ", tab_elus_finale_a$prenom_usuel_corr,
                                   " - ", tab_elus_finale_a$code_sexe,
                                   " - ", tab_elus_finale_a$annee_naiss,
                                   " - ",tab_elus_finale_a$mois_naiss,
                                   " - ",tab_elus_finale_a$jour_naiss)

tab_elus_finale_a$assembl_ID_marital = paste0(tab_elus_finale_a$nom_marital,
                                   " - ", tab_elus_finale_a$prenom_usuel_corr,
                                   " - ", tab_elus_finale_a$code_sexe,
                                   " - ", tab_elus_finale_a$annee_naiss,
                                   " - ",tab_elus_finale_a$mois_naiss,
                                   " - ",tab_elus_finale_a$jour_naiss)

```

### 1.2.4 Corr dép / code postal
```{r, echo=TRUE, message=FALSE, warning=FALSE}
tab_elus_finale_a <- tab_elus_finale_a %>% 
  rename(designation_employeur_11 = condesignation_employeur_11)


#Certains codes postaux n'ont que 4 index => manque un 0 devant 
#
#test <- tab_elus_finale_a %>% 
#  filter(departement_11 != substr(code_postal_11, 1, 2))
#
#test <- tab_elus_finale_a %>% 
#  filter(nchar(code_postal_21) == 4)
#
#test <- tab_elus_finale_a %>% 
#  filter(nchar(code_insee_comm_21) == 4)
#
#test <- tab_elus_finale_a %>% 
#  filter(nchar(departement_21) == 1)

tab_elus_finale_a <- tab_elus_finale_a %>% 
  mutate(code_postal_11 = ifelse(nchar(code_postal_11) == 4, paste0("0", code_postal_11), code_postal_11),
         code_postal_21 = ifelse(nchar(code_postal_21) == 4, paste0("0", code_postal_21), code_postal_21),
         code_postal_22 = ifelse(nchar(code_postal_22) == 4, paste0("0", code_postal_22), code_postal_22),
         
         code_postal_31 = ifelse(nchar(code_postal_31) == 4, paste0("0", code_postal_31), code_postal_31),
         code_postal_32 = ifelse(nchar(code_postal_32) == 4, paste0("0", code_postal_32), code_postal_32),
         code_postal_33 = ifelse(nchar(code_postal_33) == 4, paste0("0", code_postal_33), code_postal_33),
         
         code_postal_41 = ifelse(nchar(code_postal_41) == 4, paste0("0", code_postal_41), code_postal_41),
         code_postal_42 = ifelse(nchar(code_postal_42) == 4, paste0("0", code_postal_42), code_postal_42),
         code_postal_43 = ifelse(nchar(code_postal_43) == 4, paste0("0", code_postal_43), code_postal_43),
         code_postal_44 = ifelse(nchar(code_postal_44) == 4, paste0("0", code_postal_44), code_postal_44))


tab_elus_finale_a <- tab_elus_finale_a %>% 
  mutate(code_insee_comm_11 = ifelse(nchar(code_insee_comm_11) == 4, paste0("0", code_insee_comm_11), code_insee_comm_11),
         code_insee_comm_21 = ifelse(nchar(code_insee_comm_21) == 4, paste0("0", code_insee_comm_21), code_insee_comm_21),
         code_insee_comm_22 = ifelse(nchar(code_insee_comm_22) == 4, paste0("0", code_insee_comm_22), code_insee_comm_22),
         
         code_insee_comm_31 = ifelse(nchar(code_insee_comm_31) == 4, paste0("0", code_insee_comm_31), code_insee_comm_31),
         code_insee_comm_32 = ifelse(nchar(code_insee_comm_32) == 4, paste0("0", code_insee_comm_32), code_insee_comm_32),
         code_insee_comm_33 = ifelse(nchar(code_insee_comm_33) == 4, paste0("0", code_insee_comm_33), code_insee_comm_33),
         
         code_insee_comm_41 = ifelse(nchar(code_insee_comm_41) == 4, paste0("0", code_insee_comm_41), code_insee_comm_41),
         code_insee_comm_42 = ifelse(nchar(code_insee_comm_42) == 4, paste0("0", code_insee_comm_42), code_insee_comm_42),
         code_insee_comm_43 = ifelse(nchar(code_insee_comm_43) == 4, paste0("0", code_insee_comm_43), code_insee_comm_43),
         code_insee_comm_44 = ifelse(nchar(code_insee_comm_44) == 4, paste0("0", code_insee_comm_44), code_insee_comm_44))

tab_elus_finale_a <- tab_elus_finale_a %>% 
  mutate(departement_11 = ifelse(nchar(departement_11) == 1, paste0("0", departement_11), departement_11),
         departement_21 = ifelse(nchar(departement_21) == 1, paste0("0", departement_21), departement_21),
         departement_22 = ifelse(nchar(departement_22) == 1, paste0("0", departement_22), departement_22),
         
         departement_31 = ifelse(nchar(departement_31) == 1, paste0("0", departement_31), departement_31),
         departement_32 = ifelse(nchar(departement_32) == 1, paste0("0", departement_32), departement_32),
         departement_33 = ifelse(nchar(departement_33) == 1, paste0("0", departement_33), departement_33),
         
         departement_41 = ifelse(nchar(departement_41) == 1, paste0("0", departement_41), departement_41),
         departement_42 = ifelse(nchar(departement_42) == 1, paste0("0", departement_42), departement_42),
         departement_43 = ifelse(nchar(departement_43) == 1, paste0("0", departement_43), departement_43),
         departement_44 = ifelse(nchar(departement_44) == 1, paste0("0", departement_44), departement_44))
```

(Inutile de run ce chunk)
Il permet uniquement de repérer les communes qui ont un département différent des 2 premiers indices de son code postal.
```{r, echo=TRUE, message=FALSE, warning=FALSE}
#test <- tab_elus_finale_a %>% 
#  filter(departement_11 != substr(code_postal_11, 1, 2) & departement_11 != "2A" & departement_11 != "2B" & departement_11 != 971 & #departement_11 != 972 & departement_11 != 973 & departement_11 != 974 & departement_11 != 975 & departement_11 != 976 & departement_11 #!= 987 & departement_11 != 988)
#
#essaie_11 <- unique(test$designation_employeur_11)
#
#test <- tab_elus_finale_a %>% 
#  filter(departement_21 != substr(code_postal_21, 1, 2) & departement_21 != "2A" & departement_21 != "2B" & departement_21 != 971 & #departement_21 != 972 & departement_21 != 973 & departement_21 != 974 & departement_21 != 975 & departement_21 != 976 & departement_21 #!= 987 & departement_21 != 988)
#
#
#essaie_21 <- unique(test$designation_employeur_21)
#
#test <- tab_elus_finale_a %>% 
#  filter(departement_22 != substr(code_postal_22, 1, 2) & departement_22 != "2A" & departement_22 != "2B" & departement_22 != 971 & #departement_22 != 972 & departement_22 != 973 & departement_22 != 974 & departement_22 != 975 & departement_22 != 976 & departement_22 #!= 987 & departement_22 != 988)
#
#
#essaie_22 <- unique(test$designation_employeur_22)
#
#test <- tab_elus_finale_a %>% 
#  filter(departement_31 != substr(code_postal_31, 1, 2) & departement_31 != "2A" & departement_31 != "2B" & departement_31 != 971 & #departement_31 != 972 & departement_31 != 973 & departement_31 != 974 & departement_31 != 975 & departement_31 != 976 & departement_31 #!= 987 & departement_31 != 988)
#
#
#essaie_31 <- unique(test$designation_employeur_31)
#
#test <- tab_elus_finale_a %>% 
#  filter(departement_32 != substr(code_postal_32, 1, 2) & departement_32 != "2A" & departement_32 != "2B" & departement_32 != 971 & #departement_32 != 972 & departement_32 != 973 & departement_32 != 974 & departement_32 != 975 & departement_32 != 976 & departement_32 #!= 987 & departement_32 != 988)
#
#
#essaie_32 <- unique(test$designation_employeur_32)
#
#test <- tab_elus_finale_a %>% 
#  filter(departement_33 != substr(code_postal_33, 1, 2) & departement_33 != "2A" & departement_33 != "2B" & departement_33 != 971 & #departement_33 != 972 & departement_33 != 973 & departement_33 != 974 & departement_33 != 975 & departement_33 != 976 & departement_33 #!= 987 & departement_33 != 988)
#
#
#essaie_33 <- unique(test$designation_employeur_33)
#
#test <- tab_elus_finale_a %>% 
#  filter(departement_41 != substr(code_postal_41, 1, 2) & departement_41 != "2A" & departement_41 != "2B" & departement_41 != 971 & #departement_41 != 972 & departement_41 != 973 & departement_41 != 974 & departement_41 != 975 & departement_41 != 976 & departement_41 #!= 987 & departement_41 != 988)
#
#
#essaie_41 <- unique(test$designation_employeur_41)
#
#test <- tab_elus_finale_a %>% 
#  filter(departement_42 != substr(code_postal_42, 1, 2) & departement_42 != "2A" & departement_42 != "2B" & departement_42 != 971 & #departement_42 != 972 & departement_42 != 973 & departement_42 != 974 & departement_42 != 975 & departement_42 != 976 & departement_42 #!= 987 & departement_42 != 988)
#
#
#essaie_42 <- unique(test$designation_employeur_42)
#
#test <- tab_elus_finale_a %>% 
#  filter(departement_43 != substr(code_postal_43, 1, 2) & departement_43 != "2A" & departement_43 != "2B" & departement_43 != 971 & #departement_43 != 972 & departement_43 != 973 & departement_43 != 974 & departement_43 != 975 & departement_43 != 976 & departement_43 #!= 987 & departement_43 != 988)
#
#
#essaie_43 <- unique(test$designation_employeur_43)
#
#test <- tab_elus_finale_a %>% 
#  filter(departement_44 != substr(code_postal_44, 1, 2) & departement_44 != "2A" & departement_44 != "2B" & departement_44 != 971 & #departement_44 != 972 & departement_44 != 973 & departement_44 != 974 & departement_44 != 975 & departement_44 != 976 & departement_44 #!= 987 & departement_44 != 988)
#
#
#essaie_44 <- unique(test$designation_employeur_44)
#
#essaie_11 <- as.data.frame(essaie_11)
#essaie_21 <- as.data.frame(essaie_21)
#essaie_22 <- as.data.frame(essaie_22)
#essaie_31 <- as.data.frame(essaie_31)
#essaie_32 <- as.data.frame(essaie_32)
#essaie_33 <- as.data.frame(essaie_33)
#essaie_41 <- as.data.frame(essaie_41)
#essaie_42 <- as.data.frame(essaie_42)
#essaie_43 <- as.data.frame(essaie_43)
#essaie_44 <- as.data.frame(essaie_44)
#
#commune_pb <- bind_rows(essaie_11, essaie_21, essaie_22, essaie_31, essaie_32, essaie_33, essaie_41, essaie_42, essaie_43, essaie_44)
#
#commune_pb <- commune_pb %>%
#  mutate(COMMUNE = coalesce(essaie_11, essaie_21, essaie_22, essaie_31, essaie_32, essaie_33, essaie_41, essaie_42, essaie_43, #essaie_44))
#
#commune_pb <- commune_pb %>%
#  select(COMMUNE) %>% 
#  distinct()
#
#commune_pb2 <- commune_pb %>% 
#  left_join(tab_elus_finale_a, by = c("COMMUNE" = "designation_employeur_11")) %>% 
#  select(COMMUNE, departement_11, code_insee_comm_11, code_postal_11) %>% 
#  distinct()
#
#commune_pb3 <- commune_pb %>% 
#  left_join(tab_elus_finale_a, by = c("COMMUNE" = "designation_employeur_21")) %>% 
#  select(COMMUNE, departement_21, code_insee_comm_21, code_postal_21) %>% 
#  distinct()
#
#commune_pb4 <- commune_pb %>% 
#  left_join(tab_elus_finale_a, by = c("COMMUNE" = "designation_employeur_22")) %>% 
#  select(COMMUNE, departement_22, code_insee_comm_22, code_postal_22) %>% 
#  distinct()
#
#commune_pb5 <- commune_pb %>% 
#  left_join(tab_elus_finale_a, by = c("COMMUNE" = "designation_employeur_31")) %>% 
#  select(COMMUNE, departement_31, code_insee_comm_31, code_postal_31) %>% 
#  distinct()
#
#commune_pb6 <- commune_pb %>% 
#  left_join(tab_elus_finale_a, by = c("COMMUNE" = "designation_employeur_33")) %>% 
#  select(COMMUNE, departement_33, code_insee_comm_33, code_postal_33) %>% 
#  distinct()
#
##32, 41, 42, 43, 44 sont vides => pas besoin de les left_join.
#
#comune_pb_finale <- commune_pb2 %>% 
#  left_join(commune_pb3)
#
#comune_pb_finale <- comune_pb_finale %>% 
#  left_join(commune_pb4)
#
#comune_pb_finale <- comune_pb_finale %>% 
#  left_join(commune_pb5)
#
#comune_pb_finale <- comune_pb_finale %>% 
#  left_join(commune_pb6)
#
##Vérification code insee dans RNE
#tab_RNE_finale$code_comm_insee_14 <- as.numeric(tab_RNE_finale$code_comm_insee_14)
#tab_RNE_finale$code_comm_insee_20 <- as.numeric(tab_RNE_finale$code_comm_insee_20)
#
#test3 <- tab_RNE_finale %>% 
#  filter(code_comm_insee_14 == 72317) 
#
#test4 <- tab_RNE_finale %>% 
#  filter(code_comm_insee_20 == 76034)

#-------------------------------------------------------------------------------------------------------------------------------
#write_csv2(working_directory")
```


On observe que les communes suivantes contiennent des erreures dans, département, code insee ou code postaux.
Ces communes sont à retraités :  
__COMMUNE VIENNE__ : corriger département et code insee : 38, 38544.  
__COMMUNE DE BOURSIES__ : code postal faux.  
__COMMUNE DE MOEUVRES__ : code postal faux.    
__COMMUNE DE DOIGNIES__ : code postal faux : wiki 59400.  
__COMMUNE LES QUATRE ROUTES DU LOT__ : Code commune faux : wiki :46232, Dans RNE = bien 46232, correspond à Lemmes. A changé de nom en 2019 : Le vignon en Quercy.  
__COMMUNE DE STE LIVIERE__ : code insee wiki 52182, dep wiki 52 (dans RNE = 52). A changé de nom en 1975 : Éclaron-Braucourt-Sainte-Livière.  
__COMMUNE DE LE CONTROIS EN SOLOGNE__ : Tout faux, dep 41 wiki, code insee 41059. Est la réunion de 5 commune en 2014 est Contres et 2020 controis en sologne.  
__COMMUNE DE MARCILLE__ :  Erreur code insee 79251 wiki, dep 79 wiki. A changé de nom en 2019, est la fusion entre Pouffonds et Saint-Génard.  
__COMMUNE DE PLACE 1ER BUREAU 1ERE DIVISION__ : à supprimer.  
__COMMUNE DE CASTINE EN PLAINE__ : Nom de la comunes est faux, les informations sont bonnes.  
__COMMUNE VAUJOURS__ : Erreur code insee 93074 wiki, dep 93 wiki.  
__COMMUNE DE MONTRELAIS__ : Erreur code insee 44104 wiki, dep 44 wiki. 80566 : fieffes Montrelet donc vérifie ce code dans IRC.  
__COMMUNE DE VAL DE SCIE__ : Erreur code insee 76034 wiki, dep 76 wiki. A changé de nom en 2019. avant 'Auffay, Cressy et Sévis, auj Val-de-Scie.  

```{r, echo=TRUE, message=FALSE, warning=FALSE}
#CORR COMMUNE VIENNE 
#dep : 38 
#code insee : 38544
#Selon le site de l'insee : https://www.insee.fr/fr/metadonnees/geographie/commune/38544-vienne

verif <- tab_elus_finale_a %>% 
  filter(designation_employeur_11 == "COMMUNE VIENNE")  

tab_elus_finale_a <- tab_elus_finale_a %>% 
  mutate(departement_11 = ifelse(designation_employeur_11 == "COMMUNE VIENNE", 38, departement_11),
          
          departement_21 = ifelse(designation_employeur_21 == "COMMUNE VIENNE", 38, departement_21),
          departement_22 = ifelse(designation_employeur_22 == "COMMUNE VIENNE", 38, departement_22),
          
          departement_31 = ifelse(designation_employeur_31 == "COMMUNE VIENNE", 38, departement_31),
          departement_32 = ifelse(designation_employeur_32 == "COMMUNE VIENNE", 38, departement_32),
          departement_33 = ifelse(designation_employeur_33 == "COMMUNE VIENNE", 38, departement_33),
          
          departement_41 = ifelse(designation_employeur_41 == "COMMUNE VIENNE", 38, departement_41),
          departement_42 = ifelse(designation_employeur_42 == "COMMUNE VIENNE", 38, departement_42),
          departement_43 = ifelse(designation_employeur_43 == "COMMUNE VIENNE", 38, departement_43),
          departement_44 = ifelse(designation_employeur_44 == "COMMUNE VIENNE", 38, departement_44),
          )

tab_elus_finale_a <- tab_elus_finale_a %>% 
  mutate(code_insee_comm_11 = ifelse(designation_employeur_11 == "COMMUNE VIENNE", 38544, code_insee_comm_11),
          
          code_insee_comm_21 =ifelse(designation_employeur_21 == "COMMUNE VIENNE", 38544, code_insee_comm_21),
          code_insee_comm_22 =ifelse(designation_employeur_22 == "COMMUNE VIENNE", 38544, code_insee_comm_22),
          
          code_insee_comm_31 =ifelse(designation_employeur_31 == "COMMUNE VIENNE", 38544, code_insee_comm_31),
          code_insee_comm_32 =ifelse(designation_employeur_32 == "COMMUNE VIENNE", 38544, code_insee_comm_32),
          code_insee_comm_33 =ifelse(designation_employeur_33 == "COMMUNE VIENNE", 38544, code_insee_comm_33),
          
          code_insee_comm_41 =ifelse(designation_employeur_41 == "COMMUNE VIENNE", 38544, code_insee_comm_41),
          code_insee_comm_42 =ifelse(designation_employeur_42 == "COMMUNE VIENNE", 38544, code_insee_comm_42),
          code_insee_comm_43 =ifelse(designation_employeur_43 == "COMMUNE VIENNE", 38544, code_insee_comm_43),
          code_insee_comm_44 =ifelse(designation_employeur_44 == "COMMUNE VIENNE", 38544, code_insee_comm_44),
          )


#CORR COMMUNE DE BOURSIES
#code postal : 59400 selon https://fr.wikipedia.org/wiki/Boursies

verif <- tab_elus_finale_a %>% 
  filter(designation_employeur_11 == "COMMUNE DE BOURSIES")  


tab_elus_finale_a <- tab_elus_finale_a %>% 
  mutate(code_postal_11 = ifelse(designation_employeur_11 == "COMMUNE DE BOURSIES", 59400, code_postal_11),
          
          code_postal_21 =ifelse(designation_employeur_21 == "COMMUNE DE BOURSIES", 59400, code_postal_21),
          code_postal_22 =ifelse(designation_employeur_22 == "COMMUNE DE BOURSIES", 59400, code_postal_22),
          
          code_postal_31 =ifelse(designation_employeur_31 == "COMMUNE DE BOURSIES", 59400, code_postal_31),
          code_postal_32 =ifelse(designation_employeur_32 == "COMMUNE DE BOURSIES", 59400, code_postal_32),
          code_postal_33 =ifelse(designation_employeur_33 == "COMMUNE DE BOURSIES", 59400, code_postal_33),
          
          code_postal_41 =ifelse(designation_employeur_41 == "COMMUNE DE BOURSIES", 59400, code_postal_41),
          code_postal_42 =ifelse(designation_employeur_42 == "COMMUNE DE BOURSIES", 59400, code_postal_42),
          code_postal_43 =ifelse(designation_employeur_43 == "COMMUNE DE BOURSIES", 59400, code_postal_43),
          code_postal_44 =ifelse(designation_employeur_44 == "COMMUNE DE BOURSIES", 59400, code_postal_44),
          )




#CORR COMMUNE DE MOEUVRES
#code postal : 59400 selon https://fr.wikipedia.org/wiki/M%C5%93uvres
verif <- tab_elus_finale_a %>% 
  filter(designation_employeur_11 == "COMMUNE DE MOEUVRES")

tab_elus_finale_a <- tab_elus_finale_a %>% 
  mutate(code_postal_11 = ifelse(designation_employeur_11 == "COMMUNE DE MOEUVRES", 59400, code_postal_11),
          
          code_postal_21 =ifelse(designation_employeur_21 == "COMMUNE DE MOEUVRES", 59400, code_postal_21),
          code_postal_22 =ifelse(designation_employeur_22 == "COMMUNE DE MOEUVRES", 59400, code_postal_22),
          
          code_postal_31 =ifelse(designation_employeur_31 == "COMMUNE DE MOEUVRES", 59400, code_postal_31),
          code_postal_32 =ifelse(designation_employeur_32 == "COMMUNE DE MOEUVRES", 59400, code_postal_32),
          code_postal_33 =ifelse(designation_employeur_33 == "COMMUNE DE MOEUVRES", 59400, code_postal_33),
          
          code_postal_41 =ifelse(designation_employeur_41 == "COMMUNE DE MOEUVRES", 59400, code_postal_41),
          code_postal_42 =ifelse(designation_employeur_42 == "COMMUNE DE MOEUVRES", 59400, code_postal_42),
          code_postal_43 =ifelse(designation_employeur_43 == "COMMUNE DE MOEUVRES", 59400, code_postal_43),
          code_postal_44 =ifelse(designation_employeur_44 == "COMMUNE DE MOEUVRES", 59400, code_postal_44),
          )

#CORR COMMUNE DE DOIGNIES
# code postal : 59400
verif <- tab_elus_finale_a %>% 
  filter(designation_employeur_11 == "COMMUNE DE DOIGNIES")

tab_elus_finale_a <- tab_elus_finale_a %>% 
  mutate(code_postal_11 = ifelse(designation_employeur_11 == "COMMUNE DE DOIGNIES", 59400, code_postal_11),
          
          code_postal_21 =ifelse(designation_employeur_21 == "COMMUNE DE DOIGNIES", 59400, code_postal_21),
          code_postal_22 =ifelse(designation_employeur_22 == "COMMUNE DE DOIGNIES", 59400, code_postal_22),
          
          code_postal_31 =ifelse(designation_employeur_31 == "COMMUNE DE DOIGNIES", 59400, code_postal_31),
          code_postal_32 =ifelse(designation_employeur_32 == "COMMUNE DE DOIGNIES", 59400, code_postal_32),
          code_postal_33 =ifelse(designation_employeur_33 == "COMMUNE DE DOIGNIES", 59400, code_postal_33),
          
          code_postal_41 =ifelse(designation_employeur_41 == "COMMUNE DE DOIGNIES", 59400, code_postal_41),
          code_postal_42 =ifelse(designation_employeur_42 == "COMMUNE DE DOIGNIES", 59400, code_postal_42),
          code_postal_43 =ifelse(designation_employeur_43 == "COMMUNE DE DOIGNIES", 59400, code_postal_43),
          code_postal_44 =ifelse(designation_employeur_44 == "COMMUNE DE DOIGNIES", 59400, code_postal_44),
          )



#CORR COMMUNE LES QUATRE ROUTES DU LOT
#Code insee : 46232 
#dep 46
verif <- tab_elus_finale_a %>% 
  filter(designation_employeur_11 == "COMMUNE LES QUATRE ROUTES DU LOT")

tab_elus_finale_a <- tab_elus_finale_a %>% 
  mutate(code_insee_comm_11 = ifelse(designation_employeur_11 == "COMMUNE LES QUATRE ROUTES DU LOT", 46232, code_insee_comm_11),
          
          code_insee_comm_21 =ifelse(designation_employeur_21 == "COMMUNE LES QUATRE ROUTES DU LOT", 46232, code_insee_comm_21),
          code_insee_comm_22 =ifelse(designation_employeur_22 == "COMMUNE LES QUATRE ROUTES DU LOT", 46232, code_insee_comm_22),
          
          code_insee_comm_31 =ifelse(designation_employeur_31 == "COMMUNE LES QUATRE ROUTES DU LOT", 46232, code_insee_comm_31),
          code_insee_comm_32 =ifelse(designation_employeur_32 == "COMMUNE LES QUATRE ROUTES DU LOT", 46232, code_insee_comm_32),
          code_insee_comm_33 =ifelse(designation_employeur_33 == "COMMUNE LES QUATRE ROUTES DU LOT", 46232, code_insee_comm_33),
          
          code_insee_comm_41 =ifelse(designation_employeur_41 == "COMMUNE LES QUATRE ROUTES DU LOT", 46232, code_insee_comm_41),
          code_insee_comm_42 =ifelse(designation_employeur_42 == "COMMUNE LES QUATRE ROUTES DU LOT", 46232, code_insee_comm_42),
          code_insee_comm_43 =ifelse(designation_employeur_43 == "COMMUNE LES QUATRE ROUTES DU LOT", 46232, code_insee_comm_43),
          code_insee_comm_44 =ifelse(designation_employeur_44 == "COMMUNE LES QUATRE ROUTES DU LOT", 46232, code_insee_comm_44),
          )


tab_elus_finale_a <- tab_elus_finale_a %>% 
  mutate(departement_11 = ifelse(designation_employeur_11 == "COMMUNE LES QUATRE ROUTES DU LOT", 46, departement_11),
          
          departement_21 = ifelse(designation_employeur_21 == "COMMUNE LES QUATRE ROUTES DU LOT", 46, departement_21),
          departement_22 = ifelse(designation_employeur_22 == "COMMUNE LES QUATRE ROUTES DU LOT", 46, departement_22),
          
          departement_31 = ifelse(designation_employeur_31 == "COMMUNE LES QUATRE ROUTES DU LOT", 46, departement_31),
          departement_32 = ifelse(designation_employeur_32 == "COMMUNE LES QUATRE ROUTES DU LOT", 46, departement_32),
          departement_33 = ifelse(designation_employeur_33 == "COMMUNE LES QUATRE ROUTES DU LOT", 46, departement_33),
          
          departement_41 = ifelse(designation_employeur_41 == "COMMUNE LES QUATRE ROUTES DU LOT", 46, departement_41),
          departement_42 = ifelse(designation_employeur_42 == "COMMUNE LES QUATRE ROUTES DU LOT", 46, departement_42),
          departement_43 = ifelse(designation_employeur_43 == "COMMUNE LES QUATRE ROUTES DU LOT", 46, departement_43),
          departement_44 = ifelse(designation_employeur_44 == "COMMUNE LES QUATRE ROUTES DU LOT", 46, departement_44),
          )

#CORR COMMUNE DE STE LIVIERE
#Code insee : 52182
#Dép 52

verif <- tab_elus_finale_a %>% 
  filter(designation_employeur_11 == "COMMUNE DE STE LIVIERE")

tab_elus_finale_a <- tab_elus_finale_a %>% 
  mutate(code_insee_comm_11 = ifelse(designation_employeur_11 == "COMMUNE DE STE LIVIERE", 52182, code_insee_comm_11),
          
          code_insee_comm_21 =ifelse(designation_employeur_21 == "COMMUNE DE STE LIVIERE", 52182, code_insee_comm_21),
          code_insee_comm_22 =ifelse(designation_employeur_22 == "COMMUNE DE STE LIVIERE", 52182, code_insee_comm_22),
          
          code_insee_comm_31 =ifelse(designation_employeur_31 == "COMMUNE DE STE LIVIERE", 52182, code_insee_comm_31),
          code_insee_comm_32 =ifelse(designation_employeur_32 == "COMMUNE DE STE LIVIERE", 52182, code_insee_comm_32),
          code_insee_comm_33 =ifelse(designation_employeur_33 == "COMMUNE DE STE LIVIERE", 52182, code_insee_comm_33),
          
          code_insee_comm_41 =ifelse(designation_employeur_41 == "COMMUNE DE STE LIVIERE", 52182, code_insee_comm_41),
          code_insee_comm_42 =ifelse(designation_employeur_42 == "COMMUNE DE STE LIVIERE", 52182, code_insee_comm_42),
          code_insee_comm_43 =ifelse(designation_employeur_43 == "COMMUNE DE STE LIVIERE", 52182, code_insee_comm_43),
          code_insee_comm_44 =ifelse(designation_employeur_44 == "COMMUNE DE STE LIVIERE", 52182, code_insee_comm_44),
          )


tab_elus_finale_a <- tab_elus_finale_a %>% 
  mutate(departement_11 = ifelse(designation_employeur_11 == "COMMUNE DE STE LIVIERE", 52, departement_11),
          
          departement_21 = ifelse(designation_employeur_21 == "COMMUNE DE STE LIVIERE", 52, departement_21),
          departement_22 = ifelse(designation_employeur_22 == "COMMUNE DE STE LIVIERE", 52, departement_22),
          
          departement_31 = ifelse(designation_employeur_31 == "COMMUNE DE STE LIVIERE", 52, departement_31),
          departement_32 = ifelse(designation_employeur_32 == "COMMUNE DE STE LIVIERE", 52, departement_32),
          departement_33 = ifelse(designation_employeur_33 == "COMMUNE DE STE LIVIERE", 52, departement_33),
          
          departement_41 = ifelse(designation_employeur_41 == "COMMUNE DE STE LIVIERE", 52, departement_41),
          departement_42 = ifelse(designation_employeur_42 == "COMMUNE DE STE LIVIERE", 52, departement_42),
          departement_43 = ifelse(designation_employeur_43 == "COMMUNE DE STE LIVIERE", 52, departement_43),
          departement_44 = ifelse(designation_employeur_44 == "COMMUNE DE STE LIVIERE", 52, departement_44),
          )

#CORR COMMUNE DE LE CONTROIS EN SOLOGNE
#Code insee : 41059
#dep : 41

verif <- tab_elus_finale_a %>% 
  filter(designation_employeur_11 == "COMMUNE DE LE CONTROIS EN SOLOGNE")  
 
tab_elus_finale_a <- tab_elus_finale_a %>% 
  mutate(departement_11 = ifelse(designation_employeur_11 == "COMMUNE DE LE CONTROIS EN SOLOGNE", 41, departement_11),
          
          departement_21 = ifelse(designation_employeur_21 == "COMMUNE DE LE CONTROIS EN SOLOGNE", 41, departement_21),
          departement_22 = ifelse(designation_employeur_22 == "COMMUNE DE LE CONTROIS EN SOLOGNE", 41, departement_22),
          
          departement_31 = ifelse(designation_employeur_31 == "COMMUNE DE LE CONTROIS EN SOLOGNE", 41, departement_31),
          departement_32 = ifelse(designation_employeur_32 == "COMMUNE DE LE CONTROIS EN SOLOGNE", 41, departement_32),
          departement_33 = ifelse(designation_employeur_33 == "COMMUNE DE LE CONTROIS EN SOLOGNE", 41, departement_33),
          
          departement_41 = ifelse(designation_employeur_41 == "COMMUNE DE LE CONTROIS EN SOLOGNE", 41, departement_41),
          departement_42 = ifelse(designation_employeur_42 == "COMMUNE DE LE CONTROIS EN SOLOGNE", 41, departement_42),
          departement_43 = ifelse(designation_employeur_43 == "COMMUNE DE LE CONTROIS EN SOLOGNE", 41, departement_43),
          departement_44 = ifelse(designation_employeur_44 == "COMMUNE DE LE CONTROIS EN SOLOGNE", 41, departement_44),
          )

tab_elus_finale_a <- tab_elus_finale_a %>% 
  mutate(code_insee_comm_11 = ifelse(designation_employeur_11 == "COMMUNE DE LE CONTROIS EN SOLOGNE", 41059, code_insee_comm_11),
          
          code_insee_comm_21 =ifelse(designation_employeur_21 == "COMMUNE DE LE CONTROIS EN SOLOGNE", 41059, code_insee_comm_21),
          code_insee_comm_22 =ifelse(designation_employeur_22 == "COMMUNE DE LE CONTROIS EN SOLOGNE", 41059, code_insee_comm_22),
          
          code_insee_comm_31 =ifelse(designation_employeur_31 == "COMMUNE DE LE CONTROIS EN SOLOGNE", 41059, code_insee_comm_31),
          code_insee_comm_32 =ifelse(designation_employeur_32 == "COMMUNE DE LE CONTROIS EN SOLOGNE", 41059, code_insee_comm_32),
          code_insee_comm_33 =ifelse(designation_employeur_33 == "COMMUNE DE LE CONTROIS EN SOLOGNE", 41059, code_insee_comm_33),
          
          code_insee_comm_41 =ifelse(designation_employeur_41 == "COMMUNE DE LE CONTROIS EN SOLOGNE", 41059, code_insee_comm_41),
          code_insee_comm_42 =ifelse(designation_employeur_42 == "COMMUNE DE LE CONTROIS EN SOLOGNE", 41059, code_insee_comm_42),
          code_insee_comm_43 =ifelse(designation_employeur_43 == "COMMUNE DE LE CONTROIS EN SOLOGNE", 41059, code_insee_comm_43),
          code_insee_comm_44 =ifelse(designation_employeur_44 == "COMMUNE DE LE CONTROIS EN SOLOGNE", 41059, code_insee_comm_44),
          )


#CORR COMMUNE VAUJOURS
#Code isnee : 93074
#Dep : 93
verif <- tab_elus_finale_a %>% 
  filter(designation_employeur_22 == "COMMUNE VAUJOURS")

tab_elus_finale_a <- tab_elus_finale_a %>% 
  mutate(departement_11 = ifelse(designation_employeur_11 == "COMMUNE VAUJOURS", 93, departement_11),
          
          departement_21 = ifelse(designation_employeur_21 == "COMMUNE VAUJOURS", 93, departement_21),
          departement_22 = ifelse(designation_employeur_22 == "COMMUNE VAUJOURS", 93, departement_22),
          
          departement_31 = ifelse(designation_employeur_31 == "COMMUNE VAUJOURS", 93, departement_31),
          departement_32 = ifelse(designation_employeur_32 == "COMMUNE VAUJOURS", 93, departement_32),
          departement_33 = ifelse(designation_employeur_33 == "COMMUNE VAUJOURS", 93, departement_33),
          
          departement_41 = ifelse(designation_employeur_41 == "COMMUNE VAUJOURS", 93, departement_41),
          departement_42 = ifelse(designation_employeur_42 == "COMMUNE VAUJOURS", 93, departement_42),
          departement_43 = ifelse(designation_employeur_43 == "COMMUNE VAUJOURS", 93, departement_43),
          departement_44 = ifelse(designation_employeur_44 == "COMMUNE VAUJOURS", 93, departement_44),
          )

tab_elus_finale_a <- tab_elus_finale_a %>% 
  mutate(code_insee_comm_11 = ifelse(designation_employeur_11 == "COMMUNE VAUJOURS", 93074, code_insee_comm_11),
          
          code_insee_comm_21 =ifelse(designation_employeur_21 == "COMMUNE VAUJOURS", 93074, code_insee_comm_21),
          code_insee_comm_22 =ifelse(designation_employeur_22 == "COMMUNE VAUJOURS", 93074, code_insee_comm_22),
          
          code_insee_comm_31 =ifelse(designation_employeur_31 == "COMMUNE VAUJOURS", 93074, code_insee_comm_31),
          code_insee_comm_32 =ifelse(designation_employeur_32 == "COMMUNE VAUJOURS", 93074, code_insee_comm_32),
          code_insee_comm_33 =ifelse(designation_employeur_33 == "COMMUNE VAUJOURS", 93074, code_insee_comm_33),
          
          code_insee_comm_41 =ifelse(designation_employeur_41 == "COMMUNE VAUJOURS", 93074, code_insee_comm_41),
          code_insee_comm_42 =ifelse(designation_employeur_42 == "COMMUNE VAUJOURS", 93074, code_insee_comm_42),
          code_insee_comm_43 =ifelse(designation_employeur_43 == "COMMUNE VAUJOURS", 93074, code_insee_comm_43),
          code_insee_comm_44 =ifelse(designation_employeur_44 == "COMMUNE VAUJOURS", 93074, code_insee_comm_44),
          )

#CORR COMMUNE DE MONTRELAIS
#Code insee : 44104
#Dep : 44

verif <- tab_elus_finale_a %>% 
  filter(designation_employeur_22 == "COMMUNE DE MONTRELAIS")

tab_elus_finale_a <- tab_elus_finale_a %>% 
  mutate(departement_11 = ifelse(designation_employeur_11 == "COMMUNE VAUJOURS", 44, departement_11),
          
          departement_21 = ifelse(designation_employeur_21 == "COMMUNE VAUJOURS", 44, departement_21),
          departement_22 = ifelse(designation_employeur_22 == "COMMUNE VAUJOURS", 44, departement_22),
          
          departement_31 = ifelse(designation_employeur_31 == "COMMUNE VAUJOURS", 44, departement_31),
          departement_32 = ifelse(designation_employeur_32 == "COMMUNE VAUJOURS", 44, departement_32),
          departement_33 = ifelse(designation_employeur_33 == "COMMUNE VAUJOURS", 44, departement_33),
          
          departement_41 = ifelse(designation_employeur_41 == "COMMUNE VAUJOURS", 44, departement_41),
          departement_42 = ifelse(designation_employeur_42 == "COMMUNE VAUJOURS", 44, departement_42),
          departement_43 = ifelse(designation_employeur_43 == "COMMUNE VAUJOURS", 44, departement_43),
          departement_44 = ifelse(designation_employeur_44 == "COMMUNE VAUJOURS", 44, departement_44),
          )

tab_elus_finale_a <- tab_elus_finale_a %>% 
  mutate(code_insee_comm_11 = ifelse(designation_employeur_11 == "COMMUNE VAUJOURS", 44104, code_insee_comm_11),
          
          code_insee_comm_21 =ifelse(designation_employeur_21 == "COMMUNE VAUJOURS", 44104, code_insee_comm_21),
          code_insee_comm_22 =ifelse(designation_employeur_22 == "COMMUNE VAUJOURS", 44104, code_insee_comm_22),
          
          code_insee_comm_31 =ifelse(designation_employeur_31 == "COMMUNE VAUJOURS", 44104, code_insee_comm_31),
          code_insee_comm_32 =ifelse(designation_employeur_32 == "COMMUNE VAUJOURS", 44104, code_insee_comm_32),
          code_insee_comm_33 =ifelse(designation_employeur_33 == "COMMUNE VAUJOURS", 44104, code_insee_comm_33),
          
          code_insee_comm_41 =ifelse(designation_employeur_41 == "COMMUNE VAUJOURS", 44104, code_insee_comm_41),
          code_insee_comm_42 =ifelse(designation_employeur_42 == "COMMUNE VAUJOURS", 44104, code_insee_comm_42),
          code_insee_comm_43 =ifelse(designation_employeur_43 == "COMMUNE VAUJOURS", 44104, code_insee_comm_43),
          code_insee_comm_44 =ifelse(designation_employeur_44 == "COMMUNE VAUJOURS", 44104, code_insee_comm_44),
          )

#CORR COMMUNE DE VAL DE SCIE
#Code insee : 76034
#Dep : 76

verif <- tab_elus_finale_a %>% 
  filter(designation_employeur_22 == "COMMUNE DE VAL DE SCIE")

tab_elus_finale_a <- tab_elus_finale_a %>% 
  mutate(departement_11 = ifelse(designation_employeur_11 == "COMMUNE DE VAL DE SCIE", 76, departement_11),
          
          departement_21 = ifelse(designation_employeur_21 == "COMMUNE DE VAL DE SCIE", 76, departement_21),
          departement_22 = ifelse(designation_employeur_22 == "COMMUNE DE VAL DE SCIE", 76, departement_22),
          
          departement_31 = ifelse(designation_employeur_31 == "COMMUNE DE VAL DE SCIE", 76, departement_31),
          departement_32 = ifelse(designation_employeur_32 == "COMMUNE DE VAL DE SCIE", 76, departement_32),
          departement_33 = ifelse(designation_employeur_33 == "COMMUNE DE VAL DE SCIE", 76, departement_33),
          
          departement_41 = ifelse(designation_employeur_41 == "COMMUNE DE VAL DE SCIE", 76, departement_41),
          departement_42 = ifelse(designation_employeur_42 == "COMMUNE DE VAL DE SCIE", 76, departement_42),
          departement_43 = ifelse(designation_employeur_43 == "COMMUNE DE VAL DE SCIE", 76, departement_43),
          departement_44 = ifelse(designation_employeur_44 == "COMMUNE DE VAL DE SCIE", 76, departement_44),
          )

tab_elus_finale_a <- tab_elus_finale_a %>% 
  mutate(code_insee_comm_11 = ifelse(designation_employeur_11 == "COMMUNE DE VAL DE SCIE", 76034, code_insee_comm_11),
          
          code_insee_comm_21 =ifelse(designation_employeur_21 == "COMMUNE DE VAL DE SCIE", 76034, code_insee_comm_21),
          code_insee_comm_22 =ifelse(designation_employeur_22 == "COMMUNE DE VAL DE SCIE", 76034, code_insee_comm_22),
          
          code_insee_comm_31 =ifelse(designation_employeur_31 == "COMMUNE DE VAL DE SCIE", 76034, code_insee_comm_31),
          code_insee_comm_32 =ifelse(designation_employeur_32 == "COMMUNE DE VAL DE SCIE", 76034, code_insee_comm_32),
          code_insee_comm_33 =ifelse(designation_employeur_33 == "COMMUNE DE VAL DE SCIE", 76034, code_insee_comm_33),
          
          code_insee_comm_41 =ifelse(designation_employeur_41 == "COMMUNE DE VAL DE SCIE", 76034, code_insee_comm_41),
          code_insee_comm_42 =ifelse(designation_employeur_42 == "COMMUNE DE VAL DE SCIE", 76034, code_insee_comm_42),
          code_insee_comm_43 =ifelse(designation_employeur_43 == "COMMUNE DE VAL DE SCIE", 76034, code_insee_comm_43),
          code_insee_comm_44 =ifelse(designation_employeur_44 == "COMMUNE DE VAL DE SCIE", 76034, code_insee_comm_44),
          )

#test <- tab_elus_finale_a %>% 
#  filter(departement_11 != substr(code_insee_comm_11, 1, 2) & departement_11 != "2A" & departement_11 != "2B" & departement_11 != #971 & departement_11 != 972 & departement_11 != 973 & departement_11 != 974 & departement_11 != 975 & departement_11 != 976 & #departement_11 != 987 & departement_11 != 988)#0 OK
```


Regardons la corse et DROM :OK pour les 2
```{r, echo=TRUE, message=FALSE, warning=FALSE}
#test <- tab_elus_finale_a %>% 
#  filter(departement_11 != substr(code_insee_comm_11, 1, 2) & departement_11 != 971 & departement_11 != 972 & departement_11 != 973 #& departement_11 != 974 & departement_11 != 975 & departement_11 != 976 & departement_11 != 987 & departement_11 != 988)
#
#test <- tab_elus_finale_a %>% 
#  filter(departement_11 != substr(code_insee_comm_11, 1, 2))#10,050 : que les DROM
#
#test1 <- tab_elus_finale_a %>% 
#  filter(departement_11 == substr(code_insee_comm_11, 1, 3))#10,050 aussi => OK pour les DROM
```


### 1.2.5 ID avec Dép
On va créer une ligne par département différent pour un même individu, i.e 2620 personnes seront doublonnés.

- 2 fois, si 2 départements.    
- 3 fois, si 3 départements.  
- 4 fois, si 4 départements. 

Cela ne pose aucun problème étant donné que nous allons effectué une succéssion de merge sur différentes clés, cela augmente même la probabilité pour que les bases se touchent ;) puis ces doublons seront dans tous les cas corrigés avec le bcr personne lors de la dernière étape du merge.
```{r, echo=TRUE, message=FALSE, warning=FALSE}
#Création de la variable "dep_unique" qui met 1 pour les individu avec 1 département (même si plusieurs bcr_emp) et 0 pour les autres. 
tab_elus_finale_a <- tab_elus_finale_a %>% 
  arrange(dt_deb_indiv, dt_fin_indiv) %>% 
  mutate(dep_unique = case_when( 
    nb_bcr_emp == 1 | (departement_21 == departement_22) | (departement_31 == departement_32 & departement_31 == departement_33 & departement_32 == departement_33) | (departement_41 == departement_42 & departement_41 == departement_43 & departement_41 == departement_44 & departement_42 == departement_43 & departement_42 == departement_44 & departement_43 == departement_44) ~ 1, #Tous les indiv, qui même si ont changé de communes, sont restés dans le même département. 
    TRUE ~ 0 # Ceux qui ont changé de département.
    ))

#Tous les individus avec 1 seul département
table1 <- tab_elus_finale_a %>% 
  filter(dep_unique == 1)#857,038 indiv qui n'ont pas changé de département

#test <- table1 %>% 
#  filter(is.na(departement_11) & !is.na(code_insee_comm_11)) #OK

table1$departement_11  <- as.character(table1$departement_11)
table1$departement_21  <- as.character(table1$departement_21)
table1$departement_31  <- as.character(table1$departement_31)
table1$departement_41  <- as.character(table1$departement_41)

table1 <- table1 %>% 
  mutate(dep_finale =case_when(
    nb_bcr_emp == 1 & !is.na(departement_11) ~ departement_11,
    nb_bcr_emp == 2 & !is.na(departement_21) ~ departement_21, # departement_21 == departement_22 
    nb_bcr_emp == 3 & !is.na(departement_31) ~ departement_31, # departement_31 == departement_32 == departement_33
    nb_bcr_emp == 4 & !is.na(departement_41) ~ departement_41)) # departement_41 == departement_42 == departement_43 == departement_44

#La création de la variable code_insee_final n'est pas précise !!
#Peut rester dans même département mais changer de commune
table1 <- table1 %>% 
  mutate(code_insee_final =case_when(
    nb_bcr_emp == 1 & !is.na(code_insee_comm_11) ~ code_insee_comm_11,
    nb_bcr_emp == 2 & !is.na(code_insee_comm_21) ~ code_insee_comm_21, # code_insee_comm_21 == code_insee_comm_22 
    nb_bcr_emp == 3 & !is.na(code_insee_comm_31) ~ code_insee_comm_31, 
    nb_bcr_emp == 4 & !is.na(code_insee_comm_41) ~ code_insee_comm_41)) 
  

table1$assembl_ID_usuel_avecDep = paste0(table1$dep_finale,
                                   " - ", table1$nom_usuel,
                                   " - ", table1$prenom_usuel_corr,
                                   " - ", table1$code_sexe,
                                   " - ",table1$annee_naiss,
                                   " - ",table1$mois_naiss,
                                   " - ",table1$jour_naiss)

table1$assembl_ID_naiss_avecDep = paste0(table1$dep_finale,
                                   " - ", table1$nom_naiss,
                                   " - ", table1$prenom_usuel_corr,
                                   " - ", table1$code_sexe,
                                   " - ",table1$annee_naiss,
                                   " - ",table1$mois_naiss,
                                   " - ",table1$jour_naiss)

table1$assembl_ID_marital_avecDep = paste0(table1$dep_finale,
                                   " - ", table1$nom_marital,
                                   " - ", table1$prenom_usuel_corr,
                                   " - ", table1$code_sexe,
                                   " - ",table1$annee_naiss,
                                   " - ",table1$mois_naiss,
                                   " - ",table1$jour_naiss)




#Tous les individus avec plusieurs départements
table2 <- tab_elus_finale_a %>% 
  filter(dep_unique == 0) #2,612
#On veux doublonner les individus en fonction du nombre de bcr_emp. Exemple, un individu à 3 bcr_emp => probablement 3 département différents => on aura une ligne par département pour un même individu. Il faudra néanmois filtrer les doublons sur "assembl_ID_avecDep" car un individu avec 3 bcr_emp peut n'avoir que 2 départements différents.

table2_b <- table2 %>% 
  uncount(nb_bcr_emp, .remove = FALSE) #Doublonnage des lignes : ATTENTION aux doublons des individus qui ont moins de départements différents que de bcr_emp différents.

table2_b <- table2_b %>% 
  group_by(num_contrat_affil) %>% 
  mutate(n_ligne = row_number())

#test <- table2_b %>% 
#  filter(nb_bcr_emp == 3) # OK

table2_b$departement_21  <- as.character(table2_b$departement_21)
table2_b$departement_22  <- as.character(table2_b$departement_22)
table2_b$departement_31  <- as.character(table2_b$departement_31)
table2_b$departement_32  <- as.character(table2_b$departement_32)
table2_b$departement_33  <- as.character(table2_b$departement_33)
table2_b$departement_41  <- as.character(table2_b$departement_41)
table2_b$departement_42  <- as.character(table2_b$departement_42)
table2_b$departement_43  <- as.character(table2_b$departement_43)
table2_b$departement_44  <- as.character(table2_b$departement_44)

#Même chose que mutate :
table2_b <- table2_b %>% 
  mutate(dep_finale = ifelse(nb_bcr_emp == 2 & n_ligne == 1, departement_21,
                             ifelse(nb_bcr_emp == 2 & n_ligne == 2, departement_22, 
                                    ifelse(nb_bcr_emp == 3 & n_ligne == 1, departement_31,
                                           ifelse(nb_bcr_emp == 3 & n_ligne == 2, departement_32,
                                                  ifelse(nb_bcr_emp == 3 & n_ligne == 3, departement_33,
                                                         ifelse(nb_bcr_emp == 4 & n_ligne == 1, departement_41,
                                                                ifelse(nb_bcr_emp == 4 & n_ligne == 2, departement_42,
                                                                       ifelse(nb_bcr_emp == 4 & n_ligne == 3, departement_43,
                                                                              ifelse( nb_bcr_emp == 4 & n_ligne == 4, departement_44, "toto"))))))))))
    

#test <- table2_b %>% 
#  filter(dep_finale == "toto")#OK

#La création de la variable code_insee_final n'est pas précise !!
#Peut rester dans même département mais changer de commune
table2_b <- table2_b %>% 
  mutate(code_insee_final = ifelse(nb_bcr_emp == 2 & n_ligne == 1, code_insee_comm_21,
                             ifelse(nb_bcr_emp == 2 & n_ligne == 2, code_insee_comm_22, 
                                    ifelse(nb_bcr_emp == 3 & n_ligne == 1, code_insee_comm_31,
                                           ifelse(nb_bcr_emp == 3 & n_ligne == 2, code_insee_comm_32,
                                                  ifelse(nb_bcr_emp == 3 & n_ligne == 3, code_insee_comm_33,
                                                         ifelse(nb_bcr_emp == 4 & n_ligne == 1, code_insee_comm_41,
                                                                ifelse(nb_bcr_emp == 4 & n_ligne == 2, code_insee_comm_42,
                                                                       ifelse(nb_bcr_emp == 4 & n_ligne == 3, code_insee_comm_43,
                                                                              ifelse( nb_bcr_emp == 4 & n_ligne == 4, code_insee_comm_44, "toto"))))))))))

    
#test <- table2_b %>% 
#  filter(code_insee_finale == "toto")#OK
    
table2_b$assembl_ID_usuel_avecDep = paste0(table2_b$dep_finale,
                                   " - ", table2_b$nom_usuel,
                                   " - ", table2_b$prenom_usuel_corr,
                                   " - ", table2_b$code_sexe,
                                   " - ",table2_b$annee_naiss,
                                   " - ",table2_b$mois_naiss,
                                   " - ",table2_b$jour_naiss)

table2_b$assembl_ID_naiss_avecDep = paste0(table2_b$dep_finale,
                                   " - ", table2_b$nom_naiss,
                                   " - ", table2_b$prenom_usuel_corr,
                                   " - ", table2_b$code_sexe,
                                   " - ",table2_b$annee_naiss,
                                   " - ",table2_b$mois_naiss,
                                   " - ",table2_b$jour_naiss)

table2_b$assembl_ID_marital_avecDep = paste0(table2_b$dep_finale,
                                   " - ", table2_b$nom_marital,
                                   " - ", table2_b$prenom_usuel_corr,
                                   " - ", table2_b$code_sexe,
                                   " - ",table2_b$annee_naiss,
                                   " - ",table2_b$mois_naiss,
                                   " - ",table2_b$jour_naiss)

#Contrôler les doublons sur les matricules

#doublons_a <- table2_b %>% 
#  group_by(assembl_ID_usuel_avecDep) %>% 
#  summarise(n = n())
#
#test <- doublons_a %>% 
#  filter(n > 1) #115
#
#doublons_b <- table2_b %>% 
#  group_by(assembl_ID_naiss_avecDep) %>% 
#  summarise(n1 = n())
#
#test1 <- doublons_b %>% 
#  filter(n1 > 1)#116

table2_b <- table2_b %>% 
  distinct(assembl_ID_naiss_avecDep, .keep_all = TRUE)#5,245

table2_b <- table2_b %>% 
  distinct(assembl_ID_usuel_avecDep, .keep_all = TRUE)#5,245

table2_b <- table2_b %>% 
  distinct(assembl_ID_marital_avecDep, .keep_all = TRUE)#5,245

#Même type pour bind row :
table1$departement_21  <- as.character(table1$departement_21)
table1$departement_22  <- as.character(table1$departement_22)
table1$departement_31  <- as.character(table1$departement_31)
table1$departement_32  <- as.character(table1$departement_32)
table1$departement_33  <- as.character(table1$departement_33)
table1$departement_41  <- as.character(table1$departement_41)
table1$departement_42  <- as.character(table1$departement_42)
table1$departement_43  <- as.character(table1$departement_43)
table1$departement_44  <- as.character(table1$departement_44)

table2_b$departement_11  <- as.character(table2_b$departement_11)
table2_b$departement_21  <- as.character(table2_b$departement_21)
table2_b$departement_31  <- as.character(table2_b$departement_31)
table2_b$departement_41  <- as.character(table2_b$departement_41)

tab_elus_finale_a <- bind_rows(table1, table2_b) # 
nrow(tab_elus_finale_a)# 855,974 : on rajoute du bruit 
#(table1) + (table2_b) = 850,781 + 5,193 = 855,974 : OK 
tab_elus_finale_b <- tab_elus_finale_a %>% 
  select(-n_ligne)

#test <- tab_elus_finale_b %>% 
#  filter(dep_finale != substr(code_insee_finale, 1, 2) & nchar(dep_finale)!=3)#OK

#-------------------------------------------------------------------------------------------------------------------------------
saveRDS(tab_elus_finale_b, file = working_directory")
```
## 1.3 Corr doublons sur ID avecDep

Pour rapprocher IRC vers RNE, on va commencer par s'appuyer les ID avecDep car ayant doublonné les indiv avec plusieurs dép on devrait avoir pour un même indiv 1 seul ID avecDep. Or ce n'est pas vraiment le cas, on observe que certaines lignes on le même ID avecDep mais pas les même _bcr_personne_, ou _num_contrat_affil_, ce qui est une erreurs, ce sont des doublons, il faut donc les corriger.  

Corriger = Prendre le min de sa date de début et le max de sa date de fin pour avoir sa période totale correcte, prendre la somme de ses ass, pts et jours avant 2014, 14-20 et 20-26.  

On va effectuer cette correction des doublons sur _assembl_ID_naiss_avecDep_ et _assembl_ID_usuel_avecDep_ mais pas sur _assembl_ID_marital_avecDep_ car trop de NA. 
```{r, echo=TRUE, message=FALSE, warning=FALSE}
#doublons_ID_IRC <- tab_elus_finale_b %>%  
#  group_by(assembl_ID_marital_avecDep) %>% 
#  summarise(dbl = n())
#
#ch_periodes_3 <- doublons_ID_IRC %>% 
#  arrange(assembl_ID_marital_avecDep) %>%
#  filter(dbl > 1)
```

### 1.3.1 sur assembl_ID_naiss_avecDep. 
On fait le gros du travail sur _assembl_ID_naiss_avecDep_ 635 indiv sont traité ici.
```{r, echo=TRUE, message=FALSE, warning=FALSE}
tab_elus_finale_b <- readRDS("/working_directory")
#-------------------------------------------------------------------------------------------------------------------------------

doublons_ID_IRC_a <- tab_elus_finale_b %>%  
  group_by(assembl_ID_naiss_avecDep) %>% 
  summarise(dbl_ID_naiss_avecDep = n())

tab_elus_finale_b <-tab_elus_finale_b %>% 
  left_join(doublons_ID_IRC_a, by = "assembl_ID_naiss_avecDep")

df_ch <- tab_elus_finale_b %>% 
  filter(dbl_ID_naiss_avecDep > 1) %>% 
  arrange(assembl_ID_naiss_avecDep, dt_deb_indiv)
nrow(df_ch)#1,337

tab_elus_finale_b_1 <- tab_elus_finale_b %>% 
  filter(dbl_ID_naiss_avecDep == 1)


#Lag sur les périodes
df_ch <- df_ch %>% 
  arrange(assembl_ID_naiss_avecDep, dt_deb_indiv, dt_fin_indiv) %>% 
  group_by(assembl_ID_naiss_avecDep) %>%
  mutate(lag_dt_deb_indiv = lag(dt_deb_indiv)) %>% ungroup()

df_ch <- df_ch %>% 
  arrange(assembl_ID_naiss_avecDep, dt_deb_indiv, dt_fin_indiv) %>% 
  group_by(assembl_ID_naiss_avecDep) %>%
  mutate(lag_dt_fin_indiv = lag(dt_fin_indiv))%>% ungroup()


#Lag sur les nb_points_tot
df_ch <- df_ch %>% 
  arrange(assembl_ID_naiss_avecDep, dt_deb_indiv, dt_fin_indiv) %>% 
  group_by(assembl_ID_naiss_avecDep) %>% 
  mutate(lag_pts_tot_avant_2014 = lag(pts_tot_avant_2014))%>% ungroup()

df_ch <- df_ch %>% 
  arrange(assembl_ID_naiss_avecDep, dt_deb_indiv, dt_fin_indiv) %>% 
  group_by(assembl_ID_naiss_avecDep) %>% 
  mutate(lag_pts_tot_2014_2020 = lag(pts_tot_2014_2020))%>% ungroup()

df_ch <- df_ch %>% 
  arrange(assembl_ID_naiss_avecDep, dt_deb_indiv, dt_fin_indiv) %>% 
  group_by(assembl_ID_naiss_avecDep) %>% 
  mutate(lag_pts_tot_2020_2026 = lag(pts_tot_2020_2026))%>% ungroup()

#Lag sur les assiettes
df_ch <- df_ch %>% 
  arrange(assembl_ID_naiss_avecDep, dt_deb_indiv, dt_fin_indiv) %>% 
  group_by(assembl_ID_naiss_avecDep) %>% 
  mutate(lag_ass_tot_avant_2014 = lag(ass_tot_avant_2014))%>% ungroup()

df_ch <- df_ch %>% 
  arrange(assembl_ID_naiss_avecDep, dt_deb_indiv, dt_fin_indiv) %>% 
  group_by(assembl_ID_naiss_avecDep) %>% 
  mutate(lag_ass_tot_2014_2020 = lag(ass_tot_2014_2020))%>% ungroup()

df_ch <- df_ch %>% 
  arrange(assembl_ID_naiss_avecDep, dt_deb_indiv, dt_fin_indiv) %>% 
  group_by(assembl_ID_naiss_avecDep) %>% 
  mutate(lag_ass_tot_2020_2026 = lag(ass_tot_2020_2026))%>% ungroup()

#Lag jours 
df_ch <- df_ch %>% 
  arrange(assembl_ID_naiss_avecDep, dt_deb_indiv, dt_fin_indiv) %>% 
  group_by(assembl_ID_naiss_avecDep) %>% 
  mutate(lag_duree_aff_jour_tot_avant_2014 = lag(duree_aff_jour_tot_avant_2014))%>% ungroup()

df_ch <- df_ch %>% 
  arrange(assembl_ID_naiss_avecDep, dt_deb_indiv, dt_fin_indiv) %>% 
  group_by(assembl_ID_naiss_avecDep) %>% 
  mutate(lag_duree_aff_jour_tot_2014_2020 = lag(duree_aff_jour_tot_2014_2020))%>% ungroup()

df_ch <- df_ch %>% 
  arrange(assembl_ID_naiss_avecDep, dt_deb_indiv, dt_fin_indiv) %>% 
  group_by(assembl_ID_naiss_avecDep) %>% 
  mutate(lag_duree_aff_jour_tot_2020_2026 = lag(duree_aff_jour_tot_2020_2026)) %>% ungroup()

#Lag sur ID
df_ch <- df_ch %>% 
  arrange(assembl_ID_naiss_avecDep, dt_deb_indiv, dt_fin_indiv) %>% 
  group_by(assembl_ID_naiss_avecDep) %>% 
  mutate(lag_assembl_ID_naiss_avecDep = lag(assembl_ID_naiss_avecDep)) %>% ungroup()
#view(df_ch)


df_ch <- df_ch %>%  
  mutate(chevauchement_1 = case_when (
    !is.na(lag_assembl_ID_naiss_avecDep)  ~ 1,
    
    TRUE ~ 0
  )) 

df_ch_filtre <- df_ch %>% 
  filter(chevauchement_1 == 1)

nrow(df_ch_filtre)#702
```

```{r, echo=TRUE, message=FALSE, warning=FALSE}
#dates 
date_corr_max <- df_ch_filtre %>% 
  arrange(assembl_ID_naiss_avecDep, dt_deb_indiv, dt_fin_indiv) %>% 
  group_by(assembl_ID_naiss_avecDep) %>% 
    summarise(date_corr_max = max(dt_fin_indiv, lag_dt_fin_indiv)) %>% 
    ungroup()

date_corr_min <- df_ch_filtre %>% 
  arrange(assembl_ID_naiss_avecDep, dt_deb_indiv, dt_fin_indiv) %>% 
  group_by(assembl_ID_naiss_avecDep) %>% 
    summarise(date_corr_min = min(dt_deb_indiv, lag_dt_deb_indiv)) %>% 
    ungroup()

#Assiettes 
ass_tr_a_av_2014_corr <- df_ch_filtre %>% 
  arrange(assembl_ID_naiss_avecDep, dt_deb_indiv, dt_fin_indiv) %>% 
  group_by(assembl_ID_naiss_avecDep) %>% 
    summarise(ass_tr_a_av_2014_corr = sum(ass_tot_avant_2014 + lag_ass_tot_avant_2014)) %>% 
    ungroup()

ass_tr_a_2014_2020_corr <- df_ch_filtre %>% 
  arrange(assembl_ID_naiss_avecDep, dt_deb_indiv, dt_fin_indiv) %>% 
  group_by(assembl_ID_naiss_avecDep) %>% 
    summarise(ass_tr_a_2014_2020_corr = sum(ass_tot_2014_2020 + lag_ass_tot_2014_2020)) %>% 
    ungroup()

ass_tr_a_2020_2026_corr <- df_ch_filtre %>% 
  arrange(assembl_ID_naiss_avecDep, dt_deb_indiv, dt_fin_indiv) %>% 
  group_by(assembl_ID_naiss_avecDep) %>% 
    summarise(ass_tr_a_2020_2026_corr = sum(ass_tot_2020_2026 + lag_ass_tot_2020_2026)) %>% 
    ungroup()

#Points
nb_points_tot_av_2014_corr <- df_ch_filtre %>% 
  arrange(assembl_ID_naiss_avecDep, dt_deb_indiv, dt_fin_indiv) %>% 
  group_by(assembl_ID_naiss_avecDep) %>% 
    summarise(nb_points_tot_av_2014_corr = sum(pts_tot_avant_2014 + lag_pts_tot_avant_2014)) %>% 
    ungroup()

nb_points_tot_2014_2020_corr <- df_ch_filtre %>% 
  arrange(assembl_ID_naiss_avecDep, dt_deb_indiv, dt_fin_indiv) %>% 
  group_by(assembl_ID_naiss_avecDep) %>% 
    summarise(nb_points_tot_2014_2020_corr = sum(pts_tot_2014_2020 + lag_pts_tot_2014_2020)) %>% 
    ungroup()

nb_points_tot_2020_2026_corr <- df_ch_filtre %>% 
  arrange(assembl_ID_naiss_avecDep, dt_deb_indiv, dt_fin_indiv) %>% 
  group_by(assembl_ID_naiss_avecDep) %>% 
    summarise(nb_points_tot_2020_2026_corr = sum(pts_tot_2020_2026 + lag_pts_tot_2020_2026)) %>% 
    ungroup()

#jours
nb_jours_tot_av_2014_corr <- df_ch_filtre %>% 
  arrange(assembl_ID_naiss_avecDep, dt_deb_indiv, dt_fin_indiv) %>% 
  group_by(assembl_ID_naiss_avecDep) %>% 
    summarise(nb_jours_tot_av_2014_corr = sum(duree_aff_jour_tot_avant_2014 + lag_duree_aff_jour_tot_avant_2014)) %>% 
    ungroup()

nb_jours_tot_2014_2020_corr <- df_ch_filtre %>% 
  arrange(assembl_ID_naiss_avecDep, dt_deb_indiv, dt_fin_indiv) %>% 
  group_by(assembl_ID_naiss_avecDep) %>% 
    summarise(nb_jours_tot_2014_2020_corr = sum(duree_aff_jour_tot_2014_2020 + lag_duree_aff_jour_tot_2014_2020)) %>% 
    ungroup()

nb_jours_tot_2020_2026_corr <- df_ch_filtre %>% 
  arrange(assembl_ID_naiss_avecDep, dt_deb_indiv, dt_fin_indiv) %>% 
  group_by(assembl_ID_naiss_avecDep) %>% 
    summarise(nb_jours_tot_2020_2026_corr = sum(duree_aff_jour_tot_2020_2026 + lag_duree_aff_jour_tot_2020_2026)) %>% 
    ungroup()




#view(nb_points_tot_corr2)

tab_corr <- df_ch_filtre %>% 
  arrange(num_contrat_affil, dt_deb_indiv, dt_fin_indiv) %>%
  select(-dt_fin_indiv,-lag_dt_fin_indiv, -dt_deb_indiv, -lag_dt_deb_indiv, -ass_tot_avant_2014, -lag_ass_tot_avant_2014, -ass_tot_2014_2020, -lag_ass_tot_2014_2020, -ass_tot_2020_2026, -lag_ass_tot_2020_2026, -pts_tot_avant_2014, -lag_pts_tot_avant_2014, -pts_tot_2014_2020, -lag_pts_tot_2014_2020, -pts_tot_2020_2026, -lag_pts_tot_2020_2026, -duree_aff_jour_tot_avant_2014, -lag_duree_aff_jour_tot_avant_2014, -duree_aff_jour_tot_2014_2020, -lag_duree_aff_jour_tot_2014_2020, -duree_aff_jour_tot_2020_2026, -lag_duree_aff_jour_tot_2020_2026, -lag_assembl_ID_naiss_avecDep) %>% 
  distinct()

#nrow(tab_corr)#702

df_ch_filtre_b <- tab_corr %>% 
  left_join(date_corr_min, by = c("assembl_ID_naiss_avecDep"))

df_ch_filtre_b <- df_ch_filtre_b %>% 
  left_join(date_corr_max, by = c("assembl_ID_naiss_avecDep"))

df_ch_filtre_b <- df_ch_filtre_b %>% 
  left_join(ass_tr_a_av_2014_corr, by = c("assembl_ID_naiss_avecDep"))

df_ch_filtre_b <- df_ch_filtre_b %>% 
  left_join(ass_tr_a_2014_2020_corr, by = c("assembl_ID_naiss_avecDep"))

df_ch_filtre_b <- df_ch_filtre_b %>% 
  left_join(ass_tr_a_2020_2026_corr, by = c("assembl_ID_naiss_avecDep"))

df_ch_filtre_b <- df_ch_filtre_b %>% 
  left_join(nb_points_tot_av_2014_corr, by = c("assembl_ID_naiss_avecDep"))

df_ch_filtre_b <- df_ch_filtre_b %>% 
  left_join(nb_points_tot_2014_2020_corr, by = c("assembl_ID_naiss_avecDep"))

df_ch_filtre_b <- df_ch_filtre_b %>% 
  left_join(nb_points_tot_2020_2026_corr, by = c("assembl_ID_naiss_avecDep"))

df_ch_filtre_b <- df_ch_filtre_b %>% 
  left_join(nb_jours_tot_av_2014_corr, by = c("assembl_ID_naiss_avecDep"))

df_ch_filtre_b <- df_ch_filtre_b %>% 
  left_join(nb_jours_tot_2014_2020_corr, by = c("assembl_ID_naiss_avecDep"))

df_ch_filtre_b <- df_ch_filtre_b %>% 
  left_join(nb_jours_tot_2020_2026_corr, by = c("assembl_ID_naiss_avecDep"))

df_ch_filtre_b <- df_ch_filtre_b %>%
  rename(dt_deb_indiv = date_corr_min,
         dt_fin_indiv = date_corr_max,
         
         ass_tot_avant_2014 = ass_tr_a_av_2014_corr,
         ass_tot_2014_2020 = ass_tr_a_2014_2020_corr, 
         ass_tot_2020_2026 = ass_tr_a_2020_2026_corr, 
         
         pts_tot_avant_2014 = nb_points_tot_av_2014_corr, 
         pts_tot_2014_2020 = nb_points_tot_2014_2020_corr, 
         pts_tot_2020_2026 = nb_points_tot_2020_2026_corr, 
         
         duree_aff_jour_tot_avant_2014 = nb_jours_tot_av_2014_corr, 
         duree_aff_jour_tot_2014_2020 = nb_jours_tot_2014_2020_corr, 
         duree_aff_jour_tot_2020_2026 = nb_jours_tot_2020_2026_corr)

df_ch_filtre_b <- df_ch_filtre_b %>% 
  select(-chevauchement_1, -dbl_ID_naiss_avecDep)

tab_elus_finale_c <- bind_rows(tab_elus_finale_b_1, df_ch_filtre_b)

nrow(tab_elus_finale_c)#855,339

tab_elus_finale_c <- tab_elus_finale_c %>%
  arrange(desc(annee_fin_corr), desc(mois_fin_corr))%>% #Permets de trier (grâce à l'ancienne fausse date) 
  distinct(assembl_ID_naiss_avecDep, .keep_all = TRUE) #Et permets de garder les  bcr_personne, num contrat ... les plus récents puisque trié dans l'ordre décroissant à partir de la plus vielle date et comme la fonction select prend la 1ère lignes par ID, on prendre la ligne la + récente. 

nrow(tab_elus_finale_c)#855,272
```


### 1.3.2 sur assembl_ID_usuel_avecDep. 
On fait le même travail sur _assembl_ID_usuel_avecDep_ (pour 26 personnes), comme presque rien à changé peut le faire facilement.
```{r, echo=TRUE, message=FALSE, warning=FALSE}
doublons_ID_IRC_b <- tab_elus_finale_c %>%  
  group_by(assembl_ID_usuel_avecDep) %>% 
  summarise(dbl_ID_usuel_avecDep = n())

tab_elus_finale_c <-tab_elus_finale_c %>% 
  left_join(doublons_ID_IRC_b, by = "assembl_ID_usuel_avecDep")

df_ch <- tab_elus_finale_c %>% 
  filter(dbl_ID_usuel_avecDep > 1) %>% 
  arrange(assembl_ID_usuel_avecDep, dt_deb_indiv)
nrow(df_ch)#1,337

tab_elus_finale_c_1 <- tab_elus_finale_c %>% 
  filter(dbl_ID_usuel_avecDep == 1)


#Lag sur les périodes
df_ch <- df_ch %>% 
  arrange(assembl_ID_usuel_avecDep, dt_deb_indiv, dt_fin_indiv) %>% 
  group_by(assembl_ID_usuel_avecDep) %>%
  mutate(lag_dt_deb_indiv = lag(dt_deb_indiv)) %>% ungroup()

df_ch <- df_ch %>% 
  arrange(assembl_ID_usuel_avecDep, dt_deb_indiv, dt_fin_indiv) %>% 
  group_by(assembl_ID_usuel_avecDep) %>%
  mutate(lag_dt_fin_indiv = lag(dt_fin_indiv))%>% ungroup()


#Lag sur les nb_points_tot
df_ch <- df_ch %>% 
  arrange(assembl_ID_usuel_avecDep, dt_deb_indiv, dt_fin_indiv) %>% 
  group_by(assembl_ID_usuel_avecDep) %>% 
  mutate(lag_pts_tot_avant_2014 = lag(pts_tot_avant_2014))%>% ungroup()

df_ch <- df_ch %>% 
  arrange(assembl_ID_usuel_avecDep, dt_deb_indiv, dt_fin_indiv) %>% 
  group_by(assembl_ID_usuel_avecDep) %>% 
  mutate(lag_pts_tot_2014_2020 = lag(pts_tot_2014_2020))%>% ungroup()

df_ch <- df_ch %>% 
  arrange(assembl_ID_usuel_avecDep, dt_deb_indiv, dt_fin_indiv) %>% 
  group_by(assembl_ID_usuel_avecDep) %>% 
  mutate(lag_pts_tot_2020_2026 = lag(pts_tot_2020_2026))%>% ungroup()

#Lag sur les assiettes
df_ch <- df_ch %>% 
  arrange(assembl_ID_usuel_avecDep, dt_deb_indiv, dt_fin_indiv) %>% 
  group_by(assembl_ID_usuel_avecDep) %>% 
  mutate(lag_ass_tot_avant_2014 = lag(ass_tot_avant_2014))%>% ungroup()

df_ch <- df_ch %>% 
  arrange(assembl_ID_usuel_avecDep, dt_deb_indiv, dt_fin_indiv) %>% 
  group_by(assembl_ID_usuel_avecDep) %>% 
  mutate(lag_ass_tot_2014_2020 = lag(ass_tot_2014_2020))%>% ungroup()

df_ch <- df_ch %>% 
  arrange(assembl_ID_usuel_avecDep, dt_deb_indiv, dt_fin_indiv) %>% 
  group_by(assembl_ID_usuel_avecDep) %>% 
  mutate(lag_ass_tot_2020_2026 = lag(ass_tot_2020_2026))%>% ungroup()

#Lag jours 
df_ch <- df_ch %>% 
  arrange(assembl_ID_usuel_avecDep, dt_deb_indiv, dt_fin_indiv) %>% 
  group_by(assembl_ID_usuel_avecDep) %>% 
  mutate(lag_duree_aff_jour_tot_avant_2014 = lag(duree_aff_jour_tot_avant_2014))%>% ungroup()

df_ch <- df_ch %>% 
  arrange(assembl_ID_usuel_avecDep, dt_deb_indiv, dt_fin_indiv) %>% 
  group_by(assembl_ID_usuel_avecDep) %>% 
  mutate(lag_duree_aff_jour_tot_2014_2020 = lag(duree_aff_jour_tot_2014_2020))%>% ungroup()

df_ch <- df_ch %>% 
  arrange(assembl_ID_usuel_avecDep, dt_deb_indiv, dt_fin_indiv) %>% 
  group_by(assembl_ID_usuel_avecDep) %>% 
  mutate(lag_duree_aff_jour_tot_2020_2026 = lag(duree_aff_jour_tot_2020_2026)) %>% ungroup()

#Lag sur ID
df_ch <- df_ch %>% 
  arrange(assembl_ID_usuel_avecDep, dt_deb_indiv, dt_fin_indiv) %>% 
  group_by(assembl_ID_usuel_avecDep) %>% 
  mutate(lag_assembl_ID_usuel_avecDep = lag(assembl_ID_usuel_avecDep)) %>% ungroup()
#view(df_ch)


df_ch <- df_ch %>%  
  mutate(chevauchement_1 = case_when (
    !is.na(lag_assembl_ID_usuel_avecDep)  ~ 1, 
    
    TRUE ~ 0
  )) 

df_ch_filtre <- df_ch %>% 
  filter(chevauchement_1 == 1)

#nrow(df_ch_filtre)#52
```

```{r, echo=TRUE, message=FALSE, warning=FALSE}
#dates 
date_corr_max <- df_ch_filtre %>% 
  arrange(assembl_ID_usuel_avecDep, dt_deb_indiv, dt_fin_indiv) %>% 
  group_by(assembl_ID_usuel_avecDep) %>% 
    summarise(date_corr_max = max(dt_fin_indiv, lag_dt_fin_indiv)) %>% 
    ungroup()

date_corr_min <- df_ch_filtre %>% 
  arrange(assembl_ID_usuel_avecDep, dt_deb_indiv, dt_fin_indiv) %>% 
  group_by(assembl_ID_usuel_avecDep) %>% 
    summarise(date_corr_min = min(dt_deb_indiv, lag_dt_deb_indiv)) %>% 
    ungroup()

#Assiettes 
ass_tr_a_av_2014_corr <- df_ch_filtre %>% 
  arrange(assembl_ID_usuel_avecDep, dt_deb_indiv, dt_fin_indiv) %>% 
  group_by(assembl_ID_usuel_avecDep) %>% 
    summarise(ass_tr_a_av_2014_corr = sum(ass_tot_avant_2014 + lag_ass_tot_avant_2014)) %>% 
    ungroup()

ass_tr_a_2014_2020_corr <- df_ch_filtre %>% 
  arrange(assembl_ID_usuel_avecDep, dt_deb_indiv, dt_fin_indiv) %>% 
  group_by(assembl_ID_usuel_avecDep) %>% 
    summarise(ass_tr_a_2014_2020_corr = sum(ass_tot_2014_2020 + lag_ass_tot_2014_2020)) %>% 
    ungroup()

ass_tr_a_2020_2026_corr <- df_ch_filtre %>% 
  arrange(assembl_ID_usuel_avecDep, dt_deb_indiv, dt_fin_indiv) %>% 
  group_by(assembl_ID_usuel_avecDep) %>% 
    summarise(ass_tr_a_2020_2026_corr = sum(ass_tot_2020_2026 + lag_ass_tot_2020_2026)) %>% 
    ungroup()

#Points
nb_points_tot_av_2014_corr <- df_ch_filtre %>% 
  arrange(assembl_ID_usuel_avecDep, dt_deb_indiv, dt_fin_indiv) %>% 
  group_by(assembl_ID_usuel_avecDep) %>% 
    summarise(nb_points_tot_av_2014_corr = sum(pts_tot_avant_2014 + lag_pts_tot_avant_2014)) %>% 
    ungroup()

nb_points_tot_2014_2020_corr <- df_ch_filtre %>% 
  arrange(assembl_ID_usuel_avecDep, dt_deb_indiv, dt_fin_indiv) %>% 
  group_by(assembl_ID_usuel_avecDep) %>% 
    summarise(nb_points_tot_2014_2020_corr = sum(pts_tot_2014_2020 + lag_pts_tot_2014_2020)) %>% 
    ungroup()

nb_points_tot_2020_2026_corr <- df_ch_filtre %>% 
  arrange(assembl_ID_usuel_avecDep, dt_deb_indiv, dt_fin_indiv) %>% 
  group_by(assembl_ID_usuel_avecDep) %>% 
    summarise(nb_points_tot_2020_2026_corr = sum(pts_tot_2020_2026 + lag_pts_tot_2020_2026)) %>% 
    ungroup()

#jours
nb_jours_tot_av_2014_corr <- df_ch_filtre %>% 
  arrange(assembl_ID_usuel_avecDep, dt_deb_indiv, dt_fin_indiv) %>% 
  group_by(assembl_ID_usuel_avecDep) %>% 
    summarise(nb_jours_tot_av_2014_corr = sum(duree_aff_jour_tot_avant_2014 + lag_duree_aff_jour_tot_avant_2014)) %>% 
    ungroup()

nb_jours_tot_2014_2020_corr <- df_ch_filtre %>% 
  arrange(assembl_ID_usuel_avecDep, dt_deb_indiv, dt_fin_indiv) %>% 
  group_by(assembl_ID_usuel_avecDep) %>% 
    summarise(nb_jours_tot_2014_2020_corr = sum(duree_aff_jour_tot_2014_2020 + lag_duree_aff_jour_tot_2014_2020)) %>% 
    ungroup()

nb_jours_tot_2020_2026_corr <- df_ch_filtre %>% 
  arrange(assembl_ID_usuel_avecDep, dt_deb_indiv, dt_fin_indiv) %>% 
  group_by(assembl_ID_usuel_avecDep) %>% 
    summarise(nb_jours_tot_2020_2026_corr = sum(duree_aff_jour_tot_2020_2026 + lag_duree_aff_jour_tot_2020_2026)) %>% 
    ungroup()




#view(nb_points_tot_corr2)

tab_corr <- df_ch_filtre %>% 
  arrange(num_contrat_affil, dt_deb_indiv, dt_fin_indiv) %>%
  select(-dt_fin_indiv,-lag_dt_fin_indiv, -dt_deb_indiv, -lag_dt_deb_indiv, -ass_tot_avant_2014, -lag_ass_tot_avant_2014, -ass_tot_2014_2020, -lag_ass_tot_2014_2020, -ass_tot_2020_2026, -lag_ass_tot_2020_2026, -pts_tot_avant_2014, -lag_pts_tot_avant_2014, -pts_tot_2014_2020, -lag_pts_tot_2014_2020, -pts_tot_2020_2026, -lag_pts_tot_2020_2026, -duree_aff_jour_tot_avant_2014, -lag_duree_aff_jour_tot_avant_2014, -duree_aff_jour_tot_2014_2020, -lag_duree_aff_jour_tot_2014_2020, -duree_aff_jour_tot_2020_2026, -lag_duree_aff_jour_tot_2020_2026, -lag_assembl_ID_usuel_avecDep) %>% 
  distinct()

nrow(tab_corr)#26

df_ch_filtre_b <- tab_corr %>% 
  left_join(date_corr_min, by = c("assembl_ID_usuel_avecDep"))

df_ch_filtre_b <- df_ch_filtre_b %>% 
  left_join(date_corr_max, by = c("assembl_ID_usuel_avecDep"))

df_ch_filtre_b <- df_ch_filtre_b %>% 
  left_join(ass_tr_a_av_2014_corr, by = c("assembl_ID_usuel_avecDep"))

df_ch_filtre_b <- df_ch_filtre_b %>% 
  left_join(ass_tr_a_2014_2020_corr, by = c("assembl_ID_usuel_avecDep"))

df_ch_filtre_b <- df_ch_filtre_b %>% 
  left_join(ass_tr_a_2020_2026_corr, by = c("assembl_ID_usuel_avecDep"))

df_ch_filtre_b <- df_ch_filtre_b %>% 
  left_join(nb_points_tot_av_2014_corr, by = c("assembl_ID_usuel_avecDep"))

df_ch_filtre_b <- df_ch_filtre_b %>% 
  left_join(nb_points_tot_2014_2020_corr, by = c("assembl_ID_usuel_avecDep"))

df_ch_filtre_b <- df_ch_filtre_b %>% 
  left_join(nb_points_tot_2020_2026_corr, by = c("assembl_ID_usuel_avecDep"))

df_ch_filtre_b <- df_ch_filtre_b %>% 
  left_join(nb_jours_tot_av_2014_corr, by = c("assembl_ID_usuel_avecDep"))

df_ch_filtre_b <- df_ch_filtre_b %>% 
  left_join(nb_jours_tot_2014_2020_corr, by = c("assembl_ID_usuel_avecDep"))

df_ch_filtre_b <- df_ch_filtre_b %>% 
  left_join(nb_jours_tot_2020_2026_corr, by = c("assembl_ID_usuel_avecDep"))

df_ch_filtre_b <- df_ch_filtre_b %>%
  rename(dt_deb_indiv = date_corr_min,
         dt_fin_indiv = date_corr_max,
         
         ass_tot_avant_2014 = ass_tr_a_av_2014_corr,
         ass_tot_2014_2020 = ass_tr_a_2014_2020_corr, 
         ass_tot_2020_2026 = ass_tr_a_2020_2026_corr, 
         
         pts_tot_avant_2014 = nb_points_tot_av_2014_corr, 
         pts_tot_2014_2020 = nb_points_tot_2014_2020_corr, 
         pts_tot_2020_2026 = nb_points_tot_2020_2026_corr, 
         
         duree_aff_jour_tot_avant_2014 = nb_jours_tot_av_2014_corr, 
         duree_aff_jour_tot_2014_2020 = nb_jours_tot_2014_2020_corr, 
         duree_aff_jour_tot_2020_2026 = nb_jours_tot_2020_2026_corr)

df_ch_filtre_b <- df_ch_filtre_b %>% 
  select(-chevauchement_1, -dbl_ID_usuel_avecDep)

tab_elus_finale_c <- bind_rows(tab_elus_finale_c_1, df_ch_filtre_b)

nrow(tab_elus_finale_c)#855,246

tab_elus_finale_c <- tab_elus_finale_c %>% 
  arrange(desc(annee_fin_corr), desc(mois_fin_corr))%>% #Permets de trier (grâce à l'ancienne fausse date) 
  distinct(assembl_ID_usuel_avecDep, .keep_all = TRUE) #Et permets de garder les  bcr_personne, num contrat ... les plus récents puisque trié dans l'ordre décroissant à partir de la plus vielle date et comme la fonction select prend la 1ère lignes par ID, on prendre la ligne la + récente. 

nrow(tab_elus_finale_c)#855,246

tab_elus_finale_c <- tab_elus_finale_c %>% 
  select(-dbl_ID_naiss_avecDep, -dbl_ID_usuel_avecDep)
```

```{r, echo=TRUE, message=FALSE, warning=FALSE}
tab_elus_finale_c <- tab_elus_finale_c %>% 
  mutate(annee_debut_corr = as.numeric(substr(dt_deb_indiv, 1, 4)),
         annee_fin_corr = as.numeric(substr(dt_fin_indiv, 1, 4)),
         mois_debut_corr = month(as.Date(dt_deb_indiv)),
         mois_fin_corr = month(as.Date(dt_fin_indiv)))
```


## 1.4 Création matricule supplémentaire pour les individus qui ne se rattachent pas sur RNE.
On essaye de rapprocher les individus de IRC qui ont cotisé entre 2014 et 2026 mais qui ne se sont pas rattachés à RNE.  
Exemple Anne Hidalgo = Anne dans RNE mais Anna Maria dans IRC => ne peut pas se rapprocher.   

OU individus qui dans RNE on comme nom : nom naiss + nom usuel ou inversement => quand on merge ne se touchent pas. Exemple : RNE = TORRES FORET DODELIN  et IRC : nom naiss = TORRES et nom usuel = FORET DODELIN.   

__Sol__ : _ID_usuel_naiss_avecDep_ et _ID_naiss_usuel_avecDep_ et rapprocher sur (tab_RNE_IRC_b_corr).  

OU prénom RNE différent de prenom IRC.  

__Sol__ : _ID_codeComm_dt_naiss_usuel_ et _ID_codeComm_dt_naiss_naiss_ et rapprocher sur (tab_RNE_IRC_b_corr).
```{r, echo=TRUE, message=FALSE, warning=FALSE}
#Pour homogénéiser avec RNE

tab_elus_finale_c <- tab_elus_finale_c %>% 
  mutate(code_insee_final = case_when(
    dep_finale == 75 ~ "75056", 
    
    code_insee_final == 13202 ~ "13055",
    
    code_insee_final == 69381 ~ "69123", 
    
    TRUE ~ code_insee_final
    ))

#ATTNETION la variable code_insee_final est biaisé pour les individus qui ont plusieurs bcr employeur. Si 2 bcr emp => bcr_emp 21 et bcr_emp_22, code_insee_final va prendre 21, si 3 bcr_emp, va prendre bcr_emp_31 et si 4, va prendre bcr_emp_41.
tab_elus_finale_c$ID_codeInsee_dt_naiss = paste0(tab_elus_finale_c$code_insee_final,
                                   " - ", tab_elus_finale_c$annee_naiss,
                                   " - ",tab_elus_finale_c$mois_naiss,
                                   " - ",tab_elus_finale_c$jour_naiss)
nrow(tab_elus_finale_c)#855,265
```


```{r, echo=TRUE, message=FALSE, warning=FALSE}
tab_elus_finale_c <- tab_elus_finale_c %>% 
  select(-prez_2013, -prez_2015, -prez_2019, -prez_2021, -prez_2025, -prez_2027)

tab_elus_finale_c <- tab_elus_finale_c %>% 
  mutate(cotiz_14_26 = ifelse((!is.na(duree_aff_jour_tot_2014_2020) | !is.na(duree_aff_jour_tot_2020_2026)) , 1, 0),
         
         cotiz_14_20 = ifelse(!is.na(duree_aff_jour_tot_2014_2020) , 1, 0),
         
         cotiz_20_26 = ifelse(!is.na(duree_aff_jour_tot_2020_2026) , 1, 0),
         
         cotiz_14 = ifelse(annee_debut_corr <= 2014 & annee_fin_corr >= 2014 & cotiz_14_20 == 1, 1, 0),
         
         cotiz_20 = ifelse(annee_debut_corr <= 2019 & annee_fin_corr >= 2019 & cotiz_14_20 == 1, 1, 0),
         
         cotiz_21 = ifelse(annee_debut_corr <= 2021 & annee_fin_corr >= 2021 & cotiz_20_26 == 1, 1, 0),
         
         cotiz_25 = ifelse(annee_debut_corr <= 2025 & annee_fin_corr >= 2025 & cotiz_20_26 == 1, 1, 0)
         )

tab_cotiz_14_26 <- tab_elus_finale_c %>% 
  filter(cotiz_14_26 == 1)

test <- tab_elus_finale_c %>% 
  filter(cotiz_14 == 1)
nrow(test)

test <- tab_elus_finale_c %>% 
  filter(cotiz_20 == 1)
nrow(test)

test <- tab_elus_finale_c %>% 
  filter(cotiz_21 == 1)
nrow(test)

test <- tab_elus_finale_c %>% 
  filter(cotiz_25 == 1)
nrow(test)


saveRDS(tab_cotiz_14_26, file = working_directory")

#0) Affichage Markdown :
tab_elus_finale_c %>% 
  head(10) %>% 
  collect() %>% 
  dplyr::select(assembl_ID_usuel,
                assembl_ID_naiss,
                assembl_ID_marital,
                assembl_ID_naiss_avecDep,
                assembl_ID_usuel_avecDep,
                assembl_ID_marital_avecDep,
                ID_codeInsee_dt_naiss)%>% 
  gt(caption = "tab_elus_finale_c") %>% 
  tab_style(cell_text(size = px(12)), 
            locations = cells_body())


#-------------------------------------------------------------------------------------------------------------------------------
saveRDS(tab_elus_finale_c, file = working_directory")
```



# 2 RNE : Extraction

ATTENTION : pour rappel nous avons construit la base Ircantec en base large. Ainsi, pour les individus possédant plus d'un bcr_employeur (max 4), ces derniers vont renseigner certaines informations comme le département sur la même ligne. Exemple : élu xxxx possède 3 bcr_employeurs différents alors caractéristique_3x signifie que cet individu possède 3 bcr_employeur : departement_31 = 75, parmi les 3 département qu'il possède le 1 est Paris. Donc departement_21 est ici un département d'un individu qui possède 2 bcr_employeur différent et le premier département dans lequel cet individu aurait travaillé. departement_2021 = le département de RNE en 2021.

NB : si un individu a plusieurs bcr_employeur mais dans le même département alors departement_21 = departement_22.

```{r, echo=TRUE, message=FALSE, warning=FALSE}
tab_RNE_finale <- readRDS("working_directory")
#-------------------------------------------------------------------------------------------------------------------------------

#intersect(names(tab_RNE_finale), names(tab_elus_finale_c)) #departement_21

tab_RNE_finale <- tab_RNE_finale %>% 
  rename(departement_2021 = departement_21)
```

## 2.1 Gestion manuel ID 
```{r, echo=TRUE, message=FALSE, warning=FALSE}
tab_RNE_finale <- tab_RNE_finale %>% 
  mutate(assembl_ID_14 = ifelse(
    assembl_ID_14 == "(GUYARD) AUDRIE - SOPHIE - F - 1965 - 08 - 02", 
    gsub("[(]GUYARD[)] AUDRIE - SOPHIE - F - 1965 - 08 - 02", "AUDRIE - SOPHIE - F - 1965 - 08 - 02", assembl_ID_14),
    assembl_ID_14
  )) #Gestion des parenthèses avec gsub nécessite [] (Attention à l'espace aussi)

tab_RNE_finale <- tab_RNE_finale %>% 
  mutate(assembl_ID_14 = ifelse(
    assembl_ID_14 == "58 - (GUYARD) AUDRIE - SOPHIE - F - 1965 - 08 - 02", 
    gsub("58 - [(]GUYARD[])] AUDRIE - SOPHIE - F - 1965 - 08 - 02", "58 - AUDRIE - SOPHIE - F - 1965 - 08 - 02", assembl_ID_14),
    assembl_ID_14
  ))

tab_RNE_finale <- tab_RNE_finale %>% 
  mutate(assembl_ID_20 = ifelse(
    assembl_ID_20 == "58 - (GUYARD) AUDRIE - SOPHIE - F - 1965 - 08 - 02", 
    gsub("58 - [(]GUYARD[])] AUDRIE - SOPHIE - F - 1965 - 08 - 02", "58 - AUDRIE - SOPHIE - F - 1965 - 08 - 02", assembl_ID_20),
    assembl_ID_20
  ))

tab_RNE_finale <- tab_RNE_finale %>% 
  mutate(assembl_ID_21 = ifelse(
    assembl_ID_21 == "58 - (GUYARD) AUDRIE - SOPHIE - F - 1965 - 08 - 02", 
    gsub("58 - [(]GUYARD[])] AUDRIE - SOPHIE - F - 1965 - 08 - 02", "58 - AUDRIE - SOPHIE - F - 1965 - 08 - 02", assembl_ID_21),
    assembl_ID_21
  ))

tab_RNE_finale <- tab_RNE_finale %>% 
  mutate(assembl_ID_25 = ifelse(
    assembl_ID_25 == "58 - (GUYARD) AUDRIE - SOPHIE - F - 1965 - 08 - 02", 
    gsub("58 - [(]GUYARD[])] AUDRIE - SOPHIE - F - 1965 - 08 - 02", "58 - AUDRIE - SOPHIE - F - 1965 - 08 - 02", assembl_ID_25),
    assembl_ID_25
  ))

#Modifier les parenthèses en RNE 2014 et 2020 pour ne pas à avoir à faire ce genre de retraitement 
```
Création variable nombre de département différent par indiv.  

Aucun indiv n'aurait changé de département dans RNE entre 2014, 2020, 2021 et 2025.
```{r, echo=TRUE, message=FALSE, warning=FALSE}
#test <- tab_RNE_finale %>% 
#  filter(departement_14 != departement_20 | departement_14 != departement_2021| departement_14 != departement_25 | departement_20 != #departement_2021| departement_20 != departement_25 | departement_2021 != departement_25) #0

#Si change de dép ,solution : 
#tab_RNE_finale$nb_dep <- apply(tab_RNE_finale[, c("departement_14", "departement_20", "departement_2021", "departement_25")], 1, #function(x) {length(unique(na.omit(x)))}) 
```

## 2.2 Gestion doublons sur ID_commun_avecDep

```{r, echo=TRUE, message=FALSE, warning=FALSE}
#test <- tab_RNE_finale %>% 
#    filter(is.na(ID_commun_avecDep))#0 : OK

#doublons_ID_RNE <- tab_RNE_finale %>% 
#  group_by(ID_commun_avecDep) %>% 
#  summarise(dbl = n())
#
#test <- doublons_ID_RNE %>% 
#  filter(dbl > 1)#0 :OK
```


## 2.3 Création matricule supp
```{r, echo=TRUE, message=FALSE, warning=FALSE}
#2014
tab_RNE_finale$ID_codeInsee_dt_naiss_14 = paste0(tab_RNE_finale$code_comm_insee_14,
                                   " - ", tab_RNE_finale$Annee_naiss_14,
                                   " - ",tab_RNE_finale$Mois_naiss_14,
                                   " - ",tab_RNE_finale$Jour_naiss_14)

tab_RNE_finale <- tab_RNE_finale %>% 
  relocate(ID_codeInsee_dt_naiss_14, .after = assembl_ID_14)

#2020
tab_RNE_finale$ID_codeInsee_dt_naiss_20 = paste0(tab_RNE_finale$code_comm_insee_20,
                                   " - ", tab_RNE_finale$Annee_naiss_20,
                                   " - ",tab_RNE_finale$Mois_naiss_20,
                                   " - ",tab_RNE_finale$Jour_naiss_20)

tab_RNE_finale <- tab_RNE_finale %>% 
  relocate(ID_codeInsee_dt_naiss_20, .after = assembl_ID_20)

#2021
tab_RNE_finale$ID_codeInsee_dt_naiss_21 = paste0(tab_RNE_finale$code_comm_insee_21,
                                   " - ", tab_RNE_finale$Annee_naiss_21,
                                   " - ",tab_RNE_finale$Mois_naiss_21,
                                   " - ",tab_RNE_finale$Jour_naiss_21)

tab_RNE_finale <- tab_RNE_finale %>% 
  relocate(ID_codeInsee_dt_naiss_21, .after = assembl_ID_21)

#2025
tab_RNE_finale$ID_codeInsee_dt_naiss_25 = paste0(tab_RNE_finale$code_comm_insee_25,
                                   " - ", tab_RNE_finale$Annee_naiss_25,
                                   " - ",tab_RNE_finale$Mois_naiss_25,
                                   " - ",tab_RNE_finale$Jour_naiss_25)

tab_RNE_finale <- tab_RNE_finale %>% 
  relocate(ID_codeInsee_dt_naiss_25, .after = assembl_ID_25)

#propreté 
tab_RNE_finale_b <- tab_RNE_finale %>% 
  mutate(ID_codeInsee_dt_naiss_14 = case_when(
    ID_codeInsee_dt_naiss_14 == "NA - NA - NA - NA" ~ NA, 
    
    TRUE ~ ID_codeInsee_dt_naiss_14),
    
    ID_codeInsee_dt_naiss_20 = case_when(
    ID_codeInsee_dt_naiss_20 == "NA - NA - NA - NA" ~ NA, 
    
    TRUE ~ ID_codeInsee_dt_naiss_20),
    
    ID_codeInsee_dt_naiss_21 = case_when(
    ID_codeInsee_dt_naiss_21 == "NA - NA - NA - NA" ~ NA, 
    
    TRUE ~ ID_codeInsee_dt_naiss_21),
    
    ID_codeInsee_dt_naiss_25 = case_when(
    ID_codeInsee_dt_naiss_25 == "NA - NA - NA - NA" ~ NA, 
    
    TRUE ~ ID_codeInsee_dt_naiss_25))

#0) Affichage markdown : 
tab_RNE_finale_b %>% 
  head(10) %>% 
  collect() %>% 
  dplyr::select(ID_commun_avecDep,
                assembl_ID_14,
                ID_codeInsee_dt_naiss_14,
                assembl_ID_20,
                ID_codeInsee_dt_naiss_20,
                assembl_ID_21,
                ID_codeInsee_dt_naiss_21,
                assembl_ID_25,
                ID_codeInsee_dt_naiss_25)%>% 
  gt(caption = "tab_RNE_finale_b") %>% 
  tab_style(cell_text(size = px(12)), 
            locations = cells_body())

#-------------------------------------------------------------------------------------------------------------------------------
saveRDS(tab_RNE_finale_b, file = working_directory")
```

# 3 Merge 

__Méthode__ : La méthode du merge est expliqué dans un word nommé : "Fiche QPS élus Valentin", rangé dans : ...

Des informations sont également présentes dans ce markdown. EN cas de doute, privilégier le word.


## 3.1 Merge 1 

__Méthode__ : La première étape se décompose en 6 sous étapes et 5 sous merges :  

1) Left_join sur ID avec nom naiss : _ID_commun_avecDep_ = _assembl_ID_naiss_avecDep_ : (cf 3.1.1).    

2) Left_join sur ID avec nom usuel : _ID_commun_avecDep_ = _assembl_ID_usuel_avecDep_ : (cf 3.1.2).  

3) Left_join sur ID avec nom marital : _ID_commun_avecDep_ = _assembl_ID_marital_avecDep_ : (cf 3.1.3).  

6) Assemblage : (cf 3.1.4).

__NB__ : Grâce à 1.3.1 et 1.3.2 on sait que _assembl_ID_naiss_avecDep_ et _assembl_ID_usuel_avecDep_ sont uniques.   

0) Ajustements correspond à des manip pour homogénéiser des noms de variables, ranger la table ... Juste des manip de propreté.  


### 3.1.1 (tab_RNE_IRC_1) : naiss avecDep

__Méthode__ :  
 - 1) Left_join sur ID avec nom naiss : _ID_commun_avecDep_ = _assembl_ID_naiss_avecDep_.  
 - 2) Création de la varibale _indicatrice_ qui met 1 lorsque l'individu s'est rattaché à RNE et à sinon selon si le _bcr_personne_ est vide.
 - 3) Création des tables __(tab_RNE_IRC_1_bis)__ et __(tab_RNE_IRC_1_ter)__ qui représentent respectivement les individus ayant _indicatrice_ égale à 1, donc les individus qui se rattachent (élus indemnisés) et _indicatrice_ égale à 0, donc les individus qui ne se rattachent pas.  


```{r, echo=TRUE, message=FALSE, warning=FALSE}
tab_elus_finale_c <- readRDS("working_directory")
tab_RNE_finale_b <- readRDS("working_directory")
#-------------------------------------------------------------------------------------------------------------------------------

#0) Ajustements
tab_elus_finale_c <- tab_elus_finale_c %>%
  mutate(assembl_ID_naiss_avecDep_copie = assembl_ID_naiss_avecDep) #Pour concerver la variable assembl_ID_naiss_avecDep

#1) 
tab_RNE_IRC_1 <- tab_RNE_finale_b %>% 
  left_join(tab_elus_finale_c, by = c("ID_commun_avecDep" = "assembl_ID_naiss_avecDep"))

#2)
tab_RNE_IRC_1 <- tab_RNE_IRC_1 %>% 
  mutate(indicatrice = ifelse(!is.na(bcr_personne), 1, 0)) #bcr_personne mais j'aurais pu prende une autre variable, l'objectif est de voir si les données ircantec sont présentes. 

#3)
tab_RNE_IRC_1_bis <- tab_RNE_IRC_1 %>% 
  filter(indicatrice == 1)

tab_RNE_IRC_1_ter <- tab_RNE_IRC_1 %>% 
  filter(indicatrice == 0)
  
#0) Ajustements
tab_RNE_IRC_1_bis <- tab_RNE_IRC_1_bis %>% 
  rename(assembl_ID_naiss_avecDep = assembl_ID_naiss_avecDep_copie) #Pour concerver la variable assembl_ID_naiss_avecDep

tab_RNE_IRC_1_ter <- tab_RNE_IRC_1_ter %>% 
  rename(assembl_ID_naiss_avecDep = assembl_ID_naiss_avecDep_copie) #Pour concerver la variable assembl_ID_naiss_avecDep

nrow(tab_RNE_IRC_1_bis)#243,953
nrow(tab_RNE_IRC_1_ter)#633,819

tab_elus_finale_c <- tab_elus_finale_c %>%
  select(-assembl_ID_naiss_avecDep_copie)
```

### 3.1.2 (tab_RNE_IRC_2) : usuel avecDep

Raisonnement analogue.  
```{r, echo=TRUE, message=FALSE, warning=FALSE}
#0) Ajustements
tab_elus_finale_c <- tab_elus_finale_c %>%
  mutate(assembl_ID_usuel_avecDep_copie = assembl_ID_usuel_avecDep) #Pour concerver la variable assembl_ID_usuel_avecDep

#1) 
tab_RNE_IRC_2 <- tab_RNE_finale_b %>% 
  left_join(tab_elus_finale_c, by = c("ID_commun_avecDep" = "assembl_ID_usuel_avecDep"))

#2)
tab_RNE_IRC_2 <- tab_RNE_IRC_2 %>% 
  mutate(indicatrice = ifelse(!is.na(bcr_personne), 1, 0)) #bcr_personne mais j'aurais pu prende une autre variable, l'objectif est de voir si les données ircantec sont présentes.

#3)
tab_RNE_IRC_2_bis <- tab_RNE_IRC_2 %>% 
  filter(indicatrice == 1)

tab_RNE_IRC_2_ter <- tab_RNE_IRC_2 %>% 
  filter(indicatrice == 0)

#0) Ajustements
tab_RNE_IRC_2_bis <- tab_RNE_IRC_2_bis %>% 
  rename(assembl_ID_usuel_avecDep = assembl_ID_usuel_avecDep_copie) #Pour concerver la variable assembl_ID_usuel_avecDep

tab_RNE_IRC_2_ter <- tab_RNE_IRC_2_ter %>% 
  rename(assembl_ID_usuel_avecDep = assembl_ID_usuel_avecDep_copie) #Pour concerver la variable assembl_ID_usuel_avecDep

nrow(tab_RNE_IRC_2_bis)#306,693
nrow(tab_RNE_IRC_2_ter)#571,079

tab_elus_finale_c <- tab_elus_finale_c %>%
  select(-assembl_ID_usuel_avecDep_copie)

```

### 3.1.3 (tab_RNE_IRC_3) : marital avecDep

Raisonnement analogue.
```{r, echo=TRUE, message=FALSE, warning=FALSE}
#0) Ajustements 
tab_elus_finale_c <- tab_elus_finale_c %>%
  mutate(assembl_ID_marital_avecDep_copie = assembl_ID_marital_avecDep) #Pour concerver la variable assembl_ID_marital_avecDep

#1) 
tab_RNE_IRC_3 <- tab_RNE_finale_b %>% 
  left_join(tab_elus_finale_c, by = c( "ID_commun_avecDep" = "assembl_ID_marital_avecDep"))

#2)
tab_RNE_IRC_3 <- tab_RNE_IRC_3 %>% 
  mutate(indicatrice = ifelse(!is.na(bcr_personne), 1, 0)) #bcr_personne mais j'aurais pu prende une autre variable, l'objectif est de voir si les données ircantec sont présentes. 

#3)
tab_RNE_IRC_3_bis <- tab_RNE_IRC_3 %>% 
  filter(indicatrice == 1)

tab_RNE_IRC_3_ter <- tab_RNE_IRC_3 %>% 
  filter(indicatrice == 0)

#0) Ajustements
tab_RNE_IRC_3_bis <- tab_RNE_IRC_3_bis %>% 
  rename(assembl_ID_marital_avecDep = assembl_ID_marital_avecDep_copie) #Pour concerver la variable assembl_ID_marital_avecDep

tab_RNE_IRC_3_ter <- tab_RNE_IRC_3_ter %>% 
  rename(assembl_ID_marital_avecDep = assembl_ID_marital_avecDep_copie) #Pour concerver la variable assembl_ID_marital_avecDep

nrow(tab_RNE_IRC_3_bis)#52,253
nrow(tab_RNE_IRC_3_ter)#825,519

tab_elus_finale_c <- tab_elus_finale_c %>%
  select(-assembl_ID_marital_avecDep_copie)
```

#### 3.1.4.1 (tab_RNE_IRC_a) : élus cotisants 

Des individus peuvent se merges sur au moins un ID => pour ceux qui se sont merge surplus de un ID : distinct() s'occupent de les sortir. 
```{r, echo=TRUE, message=FALSE, warning=FALSE}

tab_RNE_IRC_a <- bind_rows(tab_RNE_IRC_1_bis, tab_RNE_IRC_2_bis, tab_RNE_IRC_3_bis) %>% 
  distinct()

nrow(tab_RNE_IRC_a)#315,584


#0) Affichage markdown : 
tab_RNE_IRC_a %>% 
  head(10) %>% 
  collect() %>% 
  dplyr::select(ID_commun_avecDep,
                tab_RNE_2014_b_indic,
                tab_RNE_2020_b_indic,
                tab_RNE_2021_b_indic,
                tab_RNE_2025_b_indic,
                bcr_personne,
                assembl_ID_naiss_avecDep,
                assembl_ID_usuel_avecDep,
                assembl_ID_marital_avecDep,
                ID_codeInsee_dt_naiss)%>% 
  gt(caption = "tab_RNE_IRC_a") %>% 
  tab_style(cell_text(size = px(12)), 
            locations = cells_body())
#-------------------------------------------------------------------------------------------------------------------------------
saveRDS(tab_RNE_IRC_a, file = "working_directory")
```

NON COTISANTS IRC : 

#### 3.1.4.2 (tab_RNE_IRC_b_corr) : élus non cotisants 

__Problème__ : Certains indiv se sont merge sur noms de naiss d'autres sur nom usuel ou marital => les indiv qui ont se rapprochent sur 1 seul des 3 noms vont être comptabilisés 2 fois. E.g je me rapproche de RNE sur _nom_naiss_ mais j'ai un NA dans _nom_usuel_ => _indicatrice_ == 1 lorsque je merge sur ID avec nom de naiss et _indicatrice_ == 0 lorsque je merge sur ID avec nom de usuel => distinct() ne suffit pas.   

__Solution__ : Distinguer les individus qui sont dans (tab_RNE_IRC_b) mais qui ne sont pas dans (tab_RNE_IRC_a) sur _ID_commun_avecDep_.  

_anti_join (tab1, tab2, by = "var1")_ : je veux garder les lignes de tab1 qui ne sont pas dans tab2 en fonction de var1.


```{r, echo=TRUE, message=FALSE, warning=FALSE}
tab_RNE_IRC_a <- readRDS("working_directory") 
#-------------------------------------------------------------------------------------------------------------------------------

tab_RNE_IRC_b <- bind_rows(tab_RNE_IRC_1_ter, tab_RNE_IRC_2_ter, tab_RNE_IRC_3_ter)%>% 
  distinct()

nrow(tab_RNE_IRC_b)#869,037

tab_RNE_IRC_b_corr <- anti_join(tab_RNE_IRC_b, tab_RNE_IRC_a, by = "ID_commun_avecDep")

nrow(tab_RNE_IRC_b_corr)#562,133



#0) Affichage markdown : 
tab_RNE_IRC_b_corr %>% 
  head(10) %>% 
  collect() %>% 
  dplyr::select(ID_commun_avecDep,
                tab_RNE_2014_b_indic,
                tab_RNE_2020_b_indic,
                tab_RNE_2021_b_indic,
                tab_RNE_2025_b_indic,
                bcr_personne,
                assembl_ID_naiss_avecDep,
                assembl_ID_usuel_avecDep,
                assembl_ID_marital_avecDep,
                ID_codeInsee_dt_naiss)%>% 
  gt(caption = "tab_RNE_IRC_b_corr") %>% 
  tab_style(cell_text(size = px(12)), 
            locations = cells_body())
#-------------------------------------------------------------------------------------------------------------------------------
saveRDS(tab_RNE_IRC_b_corr, file = "working_directory")
```


### 3.1.4 Vérif 1
On va vérifier pourquoi les individus de IRC qui devraient se rattacher à RNE ne se sont pas rapprochés.   
  
```{r, echo=TRUE, message=FALSE, warning=FALSE}

verif1 <- anti_join(tab_cotiz_14_26, tab_RNE_IRC_a, by = "bcr_personne")

##----------------------------------
#verif1 <- tab_RNE_IRC_a %>% 
#  filter(bcr_personne == "03TLV940")
#
#verif1 <- tab_RNE_IRC_b_corr %>% 
#  filter(bcr_personne == "03TLV940")
#
#verif1 <- tab_elus_finale_c %>% 
#  filter(bcr_personne == "03TLV940")
#
#verif2 <- tab_RNE_IRC_a %>% 
#  filter(assembl_ID_naiss_avecDep == "74 - BERTRAND - AGNES - F - 1954 - 07 - 10")
#
#verif1 <- tab_cotiz_14_26 %>% 
#  filter(assembl_ID_naiss_avecDep == "01 - GABON - AURELIE CELESTINE - F - 1982 - 09 - 28")
#
#verif2 <- tab_RNE_IRC_b_corr %>% 
#  filter(ID_codeInsee_dt_naiss == "59648 - 1948 - 03 - 01")
#
#verif3 <- tab_RNE_finale_b %>% 
#  filter(nom_elu_25_regul == "MONTAGNE")
#
#toto <- tab_RNE_IRC_b_corr %>% 
#  filter(nom_usuel == "MONTAGNE")
#
#toto1 <- tab_elus_finale_c %>% 
#  filter(nom_usuel == "MONTAGNE")
```

## 3.2 Merge 2

__Merge 2__ est le merge qui permet de passer outre les problèmes de prénom enregistrés différement entre RNE et en IRC. Exemple ANNE Hidalgo dans RNE : _ANNE_ et dans IRC : _ANA MARIA_.   
__Méthode__ :   

1) Partir de __(tab_RNE_IRC_b_corr)__ pour tenter de rattacher uniquement les individus qui ne se sont pas déjà rattachés.  

2) Couper  __(tab_RNE_IRC_b_corr)__ en 2, ne garder que la partie gauche / la partie RNE (la partie droite / IRC) étant vide car non rattaché.  

3) Respectivement en 2014, 2020, 2021, 2505, merge sur la double condition, annéee par année sur, _ID_codeInsee_dt_naiss_ et _nom_usuel_, _nom_naiss_ ainsi que _nom_marital_. Obtenir grâce à _indicatrice_2_ une table bis (qui a 1 pour les individus qui se rattachent) et une table ter (qui a 0 pour les individus qui ne se rattachent toujours pas) : (c.f de 3.2.1 à 3.2.4) 

4) Assembler : Créer __(tab_RNE_IRC_a_corr)__ qui rapproche les individus initialement dans __(tab_RNE_IRC_b_corr)__ vers __(tab_RNE_IRC_a)__. Créer également  __(tab_RNE_IRC_b_corr2)__ qui, elle, retire les individus initialement dans __(tab_RNE_IRC_b_corr)__ :(c.f 3.2.6).

 
```{r, echo=TRUE, message=FALSE, warning=FALSE}
tab_RNE_IRC_b_corr <- readRDS("working_directory")
tab_elus_finale_c <- readRDS("/working_directory")
#-------------------------------------------------------------------------------------------------------------------------------

#Parti droite (IRC) de table = vide => si on re rapproche IRC => ne garder que la partie gauche (RNE) de la table

tab_RNE_IRC_b_corr_1 <- tab_RNE_IRC_b_corr %>% 
  select(ID_commun_avecDep, tab_RNE_2014_b_indic, tab_RNE_2020_b_indic, tab_RNE_2021_b_indic, tab_RNE_2025_b_indic, departement_14, lib_departement_14, lib_commune_14, nbre_hab_comm_14, nom_elu_14_regul, prenom_elu_14_regul, sexe_14, date_naissance_14, PCS, lib_profession_14, code_comm_insee_14, Jour_naiss_14, Mois_naiss_14, Annee_naiss_14, lib_fonction_14, fonction_tri_14, assembl_ID_14, ID_codeInsee_dt_naiss_14, nationalite_elu_14, departement_20, lib_departement_20, code_comm_insee_20, lib_commune_20, nom_elu_20, prenom_elu_20, sexe_20, date_naissance_20, code_profession_20, lib_profession_20, date_debut_mandat_20, lib_fonction_20, date_debut_fonction_20, nationalite_elu_20, nom_elu_20_regul, prenom_elu_20_regul, fonction_tri_20, Jour_naiss_20, Mois_naiss_20, Annee_naiss_20, assembl_ID_20, ID_codeInsee_dt_naiss_20, departement_2021, lib_departement_21, code_comm_insee_21, lib_commune_21, nom_elu_21, prenom_elu_21, sexe_21, date_naissance_21, code_profession_21, lib_profession_21, date_debut_mandat_21, lib_fonction_21, date_debut_fonction_21, nationalite_elu_21, nom_elu_21_regul, prenom_elu_21_regul, fonction_tri_21, Jour_naiss_21, Mois_naiss_21, Annee_naiss_21, assembl_ID_21, ID_codeInsee_dt_naiss_21, departement_25, lib_departement_25, code_comm_insee_25, lib_commune_25, nom_elu_25, prenom_elu_25, sexe_25, date_naissance_25, code_profession_25, lib_profession_25, date_debut_mandat_25, lib_fonction_25, date_debut_fonction_25, nationalite_elu_25, fonction_tri_25, Jour_naiss_25, Mois_naiss_25, Annee_naiss_25, nom_elu_25_regul, prenom_elu_25_regul, assembl_ID_25, ID_codeInsee_dt_naiss_25, indicatrice)

tab_elus_finale_c <- tab_elus_finale_c %>%
  mutate(ID_codeInsee_dt_naiss_copie = ID_codeInsee_dt_naiss,
         nom_usuel_copie = nom_usuel, 
         nom_naiss_copie = nom_naiss,
         nom_marital_copie = nom_marital,
         code_sexe_copie = code_sexe)
```

### 3.2.1 merge 2014 
```{r, echo=TRUE, message=FALSE, warning=FALSE}
df1_usuel_14 <- tab_RNE_IRC_b_corr_1 %>% 
  left_join(tab_elus_finale_c, by = c("ID_codeInsee_dt_naiss_14" = "ID_codeInsee_dt_naiss", "nom_elu_14_regul" = "nom_usuel", "sexe_14" = "code_sexe"))

df1_usuel_14 <- df1_usuel_14 %>% 
  mutate(indicatrice_2 = ifelse(!is.na(bcr_personne), 1, 0))

df1_naiss_14 <- tab_RNE_IRC_b_corr_1 %>% 
  left_join(tab_elus_finale_c, by = c("ID_codeInsee_dt_naiss_14" = "ID_codeInsee_dt_naiss", "nom_elu_14_regul" = "nom_naiss", "sexe_14" = "code_sexe"))

df1_naiss_14 <- df1_naiss_14 %>% 
  mutate(indicatrice_2 = ifelse(!is.na(bcr_personne), 1, 0))

df1_marital_14 <- tab_RNE_IRC_b_corr_1 %>% 
  left_join(tab_elus_finale_c, by = c("ID_codeInsee_dt_naiss_14" = "ID_codeInsee_dt_naiss", "nom_elu_14_regul" = "nom_marital", "sexe_14" = "code_sexe"))

df1_marital_14 <- df1_marital_14 %>% 
  mutate(indicatrice_2 = ifelse(!is.na(bcr_personne), 1, 0))


df1_14 <- bind_rows(df1_usuel_14, df1_naiss_14, df1_marital_14) %>%
  arrange(ID_commun_avecDep, desc(dt_fin_indiv)) 

#0) Ajustements
df1_14 <- df1_14 %>% 
  select(-nom_usuel, -nom_naiss, -nom_marital) %>% 
  rename(nom_usuel = nom_usuel_copie,
         nom_naiss = nom_naiss_copie,
         nom_marital = nom_marital_copie) #Pour garder les information sur usuel et naiss d'origine + homogénéiser

df1_14 <- df1_14 %>% 
  relocate(nom_usuel, .before = prenom_usuel_corr)

df1_14 <- df1_14 %>% 
  relocate(nom_naiss, .before = prenom_usuel_corr)

df1_14 <- df1_14 %>% 
  relocate(nom_marital, .before = prenom_usuel_corr)

#comptage <- df1_14 %>% 
#  group_by(ID_commun_avecDep) %>% 
#  summarise(n = n())
#
#test <- comptage %>% 
#  filter(n > 1)
#
#test6 <- df1_14 %>% 
#  filter(ID_commun_avecDep == "01 - ALTHERR - PATRICIA - F - 1964 - 10 - 14")

df1_14_bis <- df1_14 %>% 
  filter(indicatrice_2 == 1) %>% 
  arrange(ID_commun_avecDep, desc(dt_fin_indiv)) %>% 
  distinct(ID_commun_avecDep, .keep_all = TRUE)#11 doublons persistent sinon (idéalement il faudrait les corriger ...)
#nrow(df1_14_bis)#26,916

df1_14_ter <- df1_14 %>% 
  filter(indicatrice_2 == 0) %>% 
  distinct()


df1_14_ter_corr <- anti_join(df1_14_ter, df1_14_bis, by = "ID_commun_avecDep")
#nrow(df1_14_ter_corr)#535,217
```

### 3.2.2 merge 2020
```{r, echo=TRUE, message=FALSE, warning=FALSE}
df1_usuel_20 <- tab_RNE_IRC_b_corr_1 %>% 
  left_join(tab_elus_finale_c, by = c("ID_codeInsee_dt_naiss_20" = "ID_codeInsee_dt_naiss", "nom_elu_20_regul" = "nom_usuel", "sexe_20" = "code_sexe"))

df1_usuel_20 <- df1_usuel_20 %>% 
  mutate(indicatrice_2 = ifelse(!is.na(bcr_personne), 1, 0))

df1_naiss_20 <- tab_RNE_IRC_b_corr_1 %>% 
  left_join(tab_elus_finale_c, by = c("ID_codeInsee_dt_naiss_20" = "ID_codeInsee_dt_naiss", "nom_elu_20_regul" = "nom_naiss", "sexe_20" = "code_sexe"))

df1_naiss_20 <- df1_naiss_20 %>% 
  mutate(indicatrice_2 = ifelse(!is.na(bcr_personne), 1, 0))

df1_marital_20 <- tab_RNE_IRC_b_corr_1 %>% 
  left_join(tab_elus_finale_c, by = c("ID_codeInsee_dt_naiss_20" = "ID_codeInsee_dt_naiss", "nom_elu_20_regul" = "nom_marital", "sexe_20" = "code_sexe"))

df1_marital_20 <- df1_marital_20 %>% 
  mutate(indicatrice_2 = ifelse(!is.na(bcr_personne), 1, 0))


df1_20 <- bind_rows(df1_usuel_20, df1_naiss_20, df1_marital_20) %>%
  arrange(ID_commun_avecDep, desc(dt_fin_indiv)) 

df1_20 <- df1_20 %>% 
  select(-nom_usuel, -nom_naiss, -nom_marital) %>% 
  rename(nom_usuel = nom_usuel_copie,
         nom_naiss = nom_naiss_copie,
         nom_marital = nom_marital_copie) #Pour garder les information sur usuel et naiss d'origine + homogénéiser

df1_20 <- df1_20 %>% 
  relocate(nom_usuel, .before = prenom_usuel_corr)

df1_20 <- df1_20 %>% 
  relocate(nom_naiss, .before = prenom_usuel_corr)

df1_20 <- df1_20 %>% 
  relocate(nom_marital, .before = prenom_usuel_corr)

df1_20 <- df1_20 %>%
  distinct()

#comptage <- df1_20 %>% 
#  group_by(ID_commun_avecDep) %>% 
#  summarise(n = n())
#
#test <- comptage %>% 
#  filter(n > 1)
#
#test6 <- df1_20 %>% 
#  filter(ID_commun_avecDep == "01 - ALTHERR - PATRICIA - F - 1964 - 10 - 14")

df1_20_bis <- df1_20 %>% 
  filter(indicatrice_2 == 1) %>% 
  arrange(ID_commun_avecDep, desc(dt_fin_indiv)) %>% 
  distinct(ID_commun_avecDep, .keep_all = TRUE) #11 doublons persistent sinon (idéalement il faudrait les corriger ...)

df1_20_ter <- df1_20 %>% 
  filter(indicatrice_2 == 0) %>% 
  distinct()

df1_20_ter_corr <- anti_join(df1_20_ter, df1_20_bis, by = "ID_commun_avecDep")
```

### 3.2.3 merge 2021
```{r, echo=TRUE, message=FALSE, warning=FALSE}
df1_usuel_21 <- tab_RNE_IRC_b_corr_1 %>% 
  left_join(tab_elus_finale_c, by = c("ID_codeInsee_dt_naiss_21" = "ID_codeInsee_dt_naiss", "nom_elu_21_regul" = "nom_usuel", "sexe_21" = "code_sexe"))

df1_usuel_21 <- df1_usuel_21 %>% 
  mutate(indicatrice_2 = ifelse(!is.na(bcr_personne), 1, 0))

df1_naiss_21 <- tab_RNE_IRC_b_corr_1 %>% 
  left_join(tab_elus_finale_c, by = c("ID_codeInsee_dt_naiss_21" = "ID_codeInsee_dt_naiss", "nom_elu_21_regul" = "nom_naiss", "sexe_21" = "code_sexe"))

df1_naiss_21 <- df1_naiss_21 %>% 
  mutate(indicatrice_2 = ifelse(!is.na(bcr_personne), 1, 0))

df1_marital_21 <- tab_RNE_IRC_b_corr_1 %>% 
  left_join(tab_elus_finale_c, by = c("ID_codeInsee_dt_naiss_21" = "ID_codeInsee_dt_naiss", "nom_elu_21_regul" = "nom_marital", "sexe_21" = "code_sexe"))

df1_marital_21 <- df1_marital_21 %>% 
  mutate(indicatrice_2 = ifelse(!is.na(bcr_personne), 1, 0))


df1_21 <- bind_rows(df1_usuel_21, df1_naiss_21, df1_marital_21) %>%
  arrange(ID_commun_avecDep, desc(dt_fin_indiv)) 

df1_21 <- df1_21 %>% 
  select(-nom_usuel, -nom_naiss, -nom_marital) %>% 
  rename(nom_usuel = nom_usuel_copie,
         nom_naiss = nom_naiss_copie,
         nom_marital = nom_marital_copie) #Pour garder les information sur usuel et naiss d'origine + homogénéiser

df1_21 <- df1_21 %>% 
  relocate(nom_usuel, .before = prenom_usuel_corr)

df1_21 <- df1_21 %>% 
  relocate(nom_naiss, .before = prenom_usuel_corr)

df1_21 <- df1_21 %>% 
  relocate(nom_marital, .before = prenom_usuel_corr)

df1_21 <- df1_21 %>%
  distinct()

#comptage <- df1_21 %>% 
#  group_by(ID_commun_avecDep) %>% 
#  summarise(n = n())
#
#test <- comptage %>% 
#  filter(n > 1)
#
#test6 <- df1_21 %>% 
#  filter(ID_commun_avecDep == "01 - ALTHERR - PATRICIA - F - 1964 - 10 - 14")

df1_21_bis <- df1_21 %>% 
  filter(indicatrice_2 == 1) %>% 
  arrange(ID_commun_avecDep, desc(dt_fin_indiv)) %>% 
  distinct(ID_commun_avecDep, .keep_all = TRUE) #11 doublons persistent sinon (idéalement il faudrait les corriger ...)

df1_21_ter <- df1_21 %>% 
  filter(indicatrice_2 == 0) %>% 
  distinct()


df1_21_ter_corr <- anti_join(df1_21_ter, df1_21_bis, by = "ID_commun_avecDep")
```

### 3.2.4 merge 2025
```{r, echo=TRUE, message=FALSE, warning=FALSE}
df1_usuel_25 <- tab_RNE_IRC_b_corr_1 %>% 
  left_join(tab_elus_finale_c, by = c("ID_codeInsee_dt_naiss_25" = "ID_codeInsee_dt_naiss", "nom_elu_25_regul" = "nom_usuel", "sexe_25" = "code_sexe"))

df1_usuel_25 <- df1_usuel_25 %>% 
  mutate(indicatrice_2 = ifelse(!is.na(bcr_personne), 1, 0))

df1_naiss_25 <- tab_RNE_IRC_b_corr_1 %>% 
  left_join(tab_elus_finale_c, by = c("ID_codeInsee_dt_naiss_25" = "ID_codeInsee_dt_naiss", "nom_elu_25_regul" = "nom_naiss", "sexe_25" = "code_sexe"))

df1_naiss_25 <- df1_naiss_25 %>% 
  mutate(indicatrice_2 = ifelse(!is.na(bcr_personne), 1, 0))

df1_marital_25 <- tab_RNE_IRC_b_corr_1 %>% 
  left_join(tab_elus_finale_c, by = c("ID_codeInsee_dt_naiss_25" = "ID_codeInsee_dt_naiss", "nom_elu_25_regul" = "nom_marital", "sexe_25" = "code_sexe"))

df1_marital_25 <- df1_marital_25 %>% 
  mutate(indicatrice_2 = ifelse(!is.na(bcr_personne), 1, 0))


df1_25 <- bind_rows(df1_usuel_25, df1_naiss_25, df1_marital_25) %>%
  arrange(ID_commun_avecDep, desc(dt_fin_indiv)) 

df1_25 <- df1_25 %>% 
  select(-nom_usuel, -nom_naiss, -nom_marital) %>% 
  rename(nom_usuel = nom_usuel_copie,
         nom_naiss = nom_naiss_copie,
         nom_marital = nom_marital_copie) #Pour garder les information sur usuel et naiss d'origine + homogénéiser

df1_25 <- df1_25 %>% 
  relocate(nom_usuel, .before = prenom_usuel_corr)

df1_25 <- df1_25 %>% 
  relocate(nom_naiss, .before = prenom_usuel_corr)

df1_25 <- df1_25 %>% 
  relocate(nom_marital, .before = prenom_usuel_corr)

df1_25 <- df1_25 %>%
  distinct()

#comptage <- df1_25 %>% 
#  group_by(ID_commun_avecDep) %>% 
#  summarise(n = n())
#
#test <- comptage %>% 
#  filter(n > 1)
#
#test6 <- df1_25 %>% 
#  filter(ID_commun_avecDep == "01 - ALTHERR - PATRICIA - F - 1964 - 10 - 14")

df1_25_bis <- df1_25 %>% 
  filter(indicatrice_2 == 1) %>% 
  arrange(ID_commun_avecDep, desc(dt_fin_indiv)) %>% 
  distinct(ID_commun_avecDep, .keep_all = TRUE) #11 doublons persistent sinon (idéalement il faudrait les corriger ...)

df1_25_ter <- df1_25 %>% 
  filter(indicatrice_2 == 0) %>% 
  distinct()

df1_25_ter_corr <- anti_join(df1_25_ter, df1_25_bis, by = "ID_commun_avecDep")
```

### 3.2.5 Assemblage intermédiaire
```{r, echo=TRUE, message=FALSE, warning=FALSE}
tab_RNE_IRC_a <- readRDS("working_directory")
#-------------------------------------------------------------------------------------------------------------------------------

tab_bis <- bind_rows(df1_14_bis, df1_20_bis, df1_21_bis, df1_25_bis) %>% 
  distinct()
nrow(tab_bis)#38,321


tab_ter <- bind_rows(df1_14_ter_corr, df1_20_ter_corr, df1_21_ter_corr, df1_25_ter_corr) %>% 
  distinct()

tab_ter_corr <- anti_join(tab_ter, tab_bis, by = "ID_commun_avecDep")
  
nrow(tab_ter_corr)#523,812

#523812 + 38321 = 562133 (=tab_RNE_IRC_b_corr) : OK 

#--------------------------------------------------

tab_RNE_IRC_a_corr <- bind_rows(tab_RNE_IRC_a, tab_bis) %>% 
  arrange(ID_commun_avecDep, desc(diff_jour_pers)) %>% 
  distinct(ID_commun_avecDep, .keep_all = TRUE)#Sinon on retrouve 4 doublons 
nrow(tab_RNE_IRC_a_corr)#353,960

tab_RNE_IRC_b_corr2 <- anti_join(tab_ter_corr, tab_RNE_IRC_a_corr, by = "ID_commun_avecDep") #dejà fait ci-dessus en plusieurs fois
nrow(tab_RNE_IRC_b_corr2)#523,812

#353960 + 523812  = 877,772 : OK

verif <- tab_RNE_IRC_a_corr %>% 
  filter(nom_naiss == "HIDALGO")

#0) Affichage markdown : 
tab_RNE_IRC_a_corr %>% 
  head(10) %>% 
  collect() %>% 
  dplyr::select(ID_commun_avecDep,
                tab_RNE_2014_b_indic,
                tab_RNE_2020_b_indic,
                tab_RNE_2021_b_indic,
                tab_RNE_2025_b_indic,
                bcr_personne,
                assembl_ID_naiss_avecDep,
                assembl_ID_usuel_avecDep,
                assembl_ID_marital_avecDep,
                ID_codeInsee_dt_naiss)%>% 
  gt(caption = "tab_RNE_IRC_a_corr") %>% 
  tab_style(cell_text(size = px(12)), 
            locations = cells_body())

tab_RNE_IRC_b_corr2 %>% 
  head(10) %>% 
  collect() %>% 
  dplyr::select(ID_commun_avecDep,
                tab_RNE_2014_b_indic,
                tab_RNE_2020_b_indic,
                tab_RNE_2021_b_indic,
                tab_RNE_2025_b_indic,
                bcr_personne,
                assembl_ID_naiss_avecDep,
                assembl_ID_usuel_avecDep,
                assembl_ID_marital_avecDep)%>% 
  gt(caption = "tab_RNE_IRC_b_corr2") %>% 
  tab_style(cell_text(size = px(12)), 
            locations = cells_body())


#-------------------------------------------------------------------------------------------------------------------------------
saveRDS(tab_RNE_IRC_a_corr, file = working_directory")
saveRDS(tab_RNE_IRC_b_corr2, file = working_directory")
```
### 3.2.6 Vérif 2
```{r, echo=TRUE, message=FALSE, warning=FALSE}
tab_cotiz_14_26 <- readRDS("working_directory")
tab_RNE_IRC_a_corr <- readRDS("working_directory")
#-------------------------------------------------------------------------------------------------------------------------------

verif2 <- anti_join(tab_cotiz_14_26, tab_RNE_IRC_a_corr, by = "bcr_personne")

```

## 3.3 Merge 3

__Merge 3__ est le merge qui permet de passer outre les problèmes de noms enregistrés différements entre RNE et en IRC. Exemple _MICHEL_ _MARTIN__DUPONT_, dans RNE : _MICHEL__MARTIN_ et dans IRC : _MICHEL__DUPONT_.  Permet également de rapprocher les individus qui ont une erreur dans le sexe => ne pas ajouter le sexe comme condition pour le merge ici. 

Raisonnement analogue au Merge 2

```{r, echo=TRUE, message=FALSE, warning=FALSE}
tab_RNE_IRC_b_corr2 <- readRDS("working_directory")
tab_elus_finale_c <- readRDS("working_directory")
#-------------------------------------------------------------------------------------------------------------------------------

#Parti droite (IRC) de table = vide => si on re rapproche IRC => supprimer IRC / ne garder que la partie RNE de la table

tab_RNE_IRC_b_corr2_1 <- tab_RNE_IRC_b_corr2 %>% 
  select(ID_commun_avecDep, tab_RNE_2014_b_indic, tab_RNE_2020_b_indic, tab_RNE_2021_b_indic, tab_RNE_2025_b_indic, departement_14, lib_departement_14, lib_commune_14, nbre_hab_comm_14, nom_elu_14_regul, prenom_elu_14_regul, sexe_14, date_naissance_14, PCS, lib_profession_14, code_comm_insee_14, Jour_naiss_14, Mois_naiss_14, Annee_naiss_14, lib_fonction_14, fonction_tri_14, assembl_ID_14, ID_codeInsee_dt_naiss_14, nationalite_elu_14, departement_20, lib_departement_20, code_comm_insee_20, lib_commune_20, nom_elu_20, prenom_elu_20, sexe_20, date_naissance_20, code_profession_20, lib_profession_20, date_debut_mandat_20, lib_fonction_20, date_debut_fonction_20, nationalite_elu_20, nom_elu_20_regul, prenom_elu_20_regul, fonction_tri_20, Jour_naiss_20, Mois_naiss_20, Annee_naiss_20, assembl_ID_20, ID_codeInsee_dt_naiss_20, departement_2021, lib_departement_21, code_comm_insee_21, lib_commune_21, nom_elu_21, prenom_elu_21, sexe_21, date_naissance_21, code_profession_21, lib_profession_21, date_debut_mandat_21, lib_fonction_21, date_debut_fonction_21, nationalite_elu_21, nom_elu_21_regul, prenom_elu_21_regul, fonction_tri_21, Jour_naiss_21, Mois_naiss_21, Annee_naiss_21, assembl_ID_21, ID_codeInsee_dt_naiss_21, departement_25, lib_departement_25, code_comm_insee_25, lib_commune_25, nom_elu_25, prenom_elu_25, sexe_25, date_naissance_25, code_profession_25, lib_profession_25, date_debut_mandat_25, lib_fonction_25, date_debut_fonction_25, nationalite_elu_25, fonction_tri_25, Jour_naiss_25, Mois_naiss_25, Annee_naiss_25, nom_elu_25_regul, prenom_elu_25_regul, assembl_ID_25, ID_codeInsee_dt_naiss_25, indicatrice)

tab_elus_finale_c <- tab_elus_finale_c %>%
  mutate(ID_codeInsee_dt_naiss_copie = ID_codeInsee_dt_naiss, 
         prenom_usuel_corr_copie = prenom_usuel_corr)
```

### 3.3.1 merge 2014
```{r, echo=TRUE, message=FALSE, warning=FALSE}
df1_14 <- tab_RNE_IRC_b_corr2_1 %>% 
  left_join(tab_elus_finale_c, by = c("ID_codeInsee_dt_naiss_14" = "ID_codeInsee_dt_naiss", "prenom_elu_14_regul" = "prenom_usuel_corr"))

df1_14 <- df1_14 %>% 
  mutate(indicatrice_3 = ifelse(!is.na(bcr_personne), 1, 0)) %>% 
  distinct()



df1_14_bis <- df1_14 %>% 
  filter(indicatrice_3 == 1) %>% 
  arrange(ID_commun_avecDep, desc(dt_fin_indiv)) %>% 
  distinct(ID_commun_avecDep, .keep_all = TRUE) #11 doublons persistent sinon (idéalement il faudrait les corriger ...)

df1_14_ter <- df1_14 %>% 
  filter(indicatrice_3 == 0)%>% 
  arrange(ID_commun_avecDep, desc(dt_fin_indiv))
```

### 3.3.2 merge 2020
```{r, echo=TRUE, message=FALSE, warning=FALSE}
df1_20 <- tab_RNE_IRC_b_corr2_1 %>% 
  left_join(tab_elus_finale_c, by = c("ID_codeInsee_dt_naiss_20" = "ID_codeInsee_dt_naiss", "prenom_elu_20_regul" = "prenom_usuel_corr"))

df1_20 <- df1_20 %>% 
  mutate(indicatrice_3 = ifelse(!is.na(bcr_personne), 1, 0)) %>% 
  distinct()



df1_20_bis <- df1_20 %>% 
  filter(indicatrice_3 == 1) %>% 
  arrange(ID_commun_avecDep, desc(dt_fin_indiv)) %>% 
  distinct(ID_commun_avecDep, .keep_all = TRUE) #11 doublons persistent sinon (idéalement il faudrait les corriger ...)

df1_20_ter <- df1_20 %>% 
  filter(indicatrice_3 == 0)%>% 
  arrange(ID_commun_avecDep, desc(dt_fin_indiv))
```

### 3.3.3 merge 2021
```{r, echo=TRUE, message=FALSE, warning=FALSE}
df1_21 <- tab_RNE_IRC_b_corr2_1 %>% 
  left_join(tab_elus_finale_c, by = c("ID_codeInsee_dt_naiss_21" = "ID_codeInsee_dt_naiss", "prenom_elu_21_regul" = "prenom_usuel_corr"))

df1_21 <- df1_21 %>% 
  mutate(indicatrice_3 = ifelse(!is.na(bcr_personne), 1, 0)) %>% 
  distinct()


df1_21_bis <- df1_21 %>% 
  filter(indicatrice_3 == 1) %>% 
  arrange(ID_commun_avecDep, desc(dt_fin_indiv)) %>% 
  distinct(ID_commun_avecDep, .keep_all = TRUE) #11 doublons persistent sinon (idéalement il faudrait les corriger ...)

df1_21_ter <- df1_21 %>% 
  filter(indicatrice_3 == 0)%>% 
  arrange(ID_commun_avecDep, desc(dt_fin_indiv))
```

### 3.3.4 merge 2025
```{r, echo=TRUE, message=FALSE, warning=FALSE}
df1_25 <- tab_RNE_IRC_b_corr2_1 %>% 
  left_join(tab_elus_finale_c, by = c("ID_codeInsee_dt_naiss_25" = "ID_codeInsee_dt_naiss", "prenom_elu_25_regul" = "prenom_usuel_corr"))

df1_25 <- df1_25 %>% 
  mutate(indicatrice_3 = ifelse(!is.na(bcr_personne), 1, 0)) %>% 
  distinct()


df1_25_bis <- df1_25 %>% 
  filter(indicatrice_3 == 1) %>% 
  arrange(ID_commun_avecDep, desc(dt_fin_indiv)) %>% 
  distinct(ID_commun_avecDep, .keep_all = TRUE) #11 doublons persistent sinon (idéalement il faudrait les corriger ...)

df1_25_ter <- df1_25 %>% 
  filter(indicatrice_3 == 0)%>% 
  arrange(ID_commun_avecDep, desc(dt_fin_indiv))
```

### 3.3.5 Assemblage intermédiaire
```{r, echo=TRUE, message=FALSE, warning=FALSE}
tab_RNE_IRC_a_corr <- readRDS("working_directory")
#-------------------------------------------------------------------------------------------------------------------------------

tab_bis2 <- bind_rows(df1_14_bis, df1_20_bis, df1_21_bis, df1_25_bis) %>% 
  distinct()
nrow(tab_bis2)#26,143

tab_RNE_IRC_a_corr2 <- bind_rows(tab_RNE_IRC_a_corr, tab_bis2) %>% 
  distinct(ID_commun_avecDep, .keep_all = TRUE)
nrow(tab_RNE_IRC_a_corr2)#380,103


tab_ter <- bind_rows(df1_14_ter, df1_20_ter, df1_21_ter, df1_25_ter) %>% 
  distinct()

tab_RNE_IRC_b_corr3 <- anti_join(tab_ter, tab_RNE_IRC_a_corr2, by = "ID_commun_avecDep")

nrow(tab_RNE_IRC_b_corr3)#497,669

#380103 + 497669 = 877772 : OK


#0) Affichage markdown : 
tab_RNE_IRC_a_corr2 %>% 
  head(10) %>% 
  collect() %>% 
  dplyr::select(ID_commun_avecDep,
                tab_RNE_2014_b_indic,
                tab_RNE_2020_b_indic,
                tab_RNE_2021_b_indic,
                tab_RNE_2025_b_indic,
                bcr_personne,
                assembl_ID_naiss_avecDep,
                assembl_ID_usuel_avecDep,
                assembl_ID_marital_avecDep,
                ID_codeInsee_dt_naiss)%>% 
  gt(caption = "tab_RNE_IRC_a_corr2") %>% 
  tab_style(cell_text(size = px(12)), 
            locations = cells_body())

tab_RNE_IRC_b_corr3 %>% 
  head(10) %>% 
  collect() %>% 
  dplyr::select(ID_commun_avecDep,
                tab_RNE_2014_b_indic,
                tab_RNE_2020_b_indic,
                tab_RNE_2021_b_indic,
                tab_RNE_2025_b_indic,
                bcr_personne,
                assembl_ID_naiss_avecDep,
                assembl_ID_usuel_avecDep,
                assembl_ID_marital_avecDep)%>% 
  gt(caption = "tab_RNE_IRC_b_corr3") %>% 
  tab_style(cell_text(size = px(12)), 
            locations = cells_body())
#-------------------------------------------------------------------------------------------------------------------------------
saveRDS(tab_RNE_IRC_a_corr2, file ="working_directory")
saveRDS(tab_RNE_IRC_b_corr3, file ="working_directory")
```


### 3.3.6 Vérif 3
```{r, echo=TRUE, message=FALSE, warning=FALSE}
tab_cotiz_14_26 <- readRDS("working_directory")
tab_RNE_IRC_a_corr2 <- readRDS("working_directory")
#-------------------------------------------------------------------------------------------------------------------------------

verif3 <- anti_join(tab_cotiz_14_26, tab_RNE_IRC_a_corr2, by = "bcr_personne")

```




## 3.4 Merge 4


```{r, echo=TRUE, message=FALSE, warning=FALSE}
tab_RNE_IRC_b_corr3 <- readRDS("working_directory")
tab_elus_finale_c <- readRDS("working_directory")
#-------------------------------------------------------------------------------------------------------------------------------

#Parti droite (IRC) de table = vide => si on re rapproche IRC => supprimer IRC / ne garder que la partie RNE de la table

tab_RNE_IRC_b_corr3_1 <- tab_RNE_IRC_b_corr3 %>% 
  select(ID_commun_avecDep, tab_RNE_2014_b_indic, tab_RNE_2020_b_indic, tab_RNE_2021_b_indic, tab_RNE_2025_b_indic, departement_14, lib_departement_14, lib_commune_14, nbre_hab_comm_14, nom_elu_14_regul, prenom_elu_14_regul, sexe_14, date_naissance_14, PCS, lib_profession_14, code_comm_insee_14, Jour_naiss_14, Mois_naiss_14, Annee_naiss_14, lib_fonction_14, fonction_tri_14, assembl_ID_14, ID_codeInsee_dt_naiss_14, nationalite_elu_14, departement_20, lib_departement_20, code_comm_insee_20, lib_commune_20, nom_elu_20, prenom_elu_20, sexe_20, date_naissance_20, code_profession_20, lib_profession_20, date_debut_mandat_20, lib_fonction_20, date_debut_fonction_20, nationalite_elu_20, nom_elu_20_regul, prenom_elu_20_regul, fonction_tri_20, Jour_naiss_20, Mois_naiss_20, Annee_naiss_20, assembl_ID_20, ID_codeInsee_dt_naiss_20, departement_2021, lib_departement_21, code_comm_insee_21, lib_commune_21, nom_elu_21, prenom_elu_21, sexe_21, date_naissance_21, code_profession_21, lib_profession_21, date_debut_mandat_21, lib_fonction_21, date_debut_fonction_21, nationalite_elu_21, nom_elu_21_regul, prenom_elu_21_regul, fonction_tri_21, Jour_naiss_21, Mois_naiss_21, Annee_naiss_21, assembl_ID_21, ID_codeInsee_dt_naiss_21, departement_25, lib_departement_25, code_comm_insee_25, lib_commune_25, nom_elu_25, prenom_elu_25, sexe_25, date_naissance_25, code_profession_25, lib_profession_25, date_debut_mandat_25, lib_fonction_25, date_debut_fonction_25, nationalite_elu_25, fonction_tri_25, Jour_naiss_25, Mois_naiss_25, Annee_naiss_25, nom_elu_25_regul, prenom_elu_25_regul, assembl_ID_25, ID_codeInsee_dt_naiss_25, indicatrice)

tab_elus_finale_c <- tab_elus_finale_c %>%
  mutate(code_insee_final_copie = code_insee_final,
         nom_usuel_copie = nom_usuel,
         nom_naiss_copie = nom_naiss,
         nom_marital_copie = nom_marital,
         prenom_usuel_corr_copie = prenom_usuel_corr,
         code_sexe_copie = code_sexe)
```

### 3.4.1 merge 2014
Pour corriger les dates de naissances 
```{r, echo=TRUE, message=FALSE, warning=FALSE}
df1_usuel_14 <- tab_RNE_IRC_b_corr3_1 %>% 
  left_join(tab_elus_finale_c, by = c("code_comm_insee_14" = "code_insee_final", "nom_elu_14_regul" = "nom_usuel", "prenom_elu_14_regul" = "prenom_usuel_corr", "sexe_14" = "code_sexe"))

df1_usuel_14 <- df1_usuel_14 %>% 
  mutate(indicatrice_4 = ifelse(!is.na(bcr_personne), 1, 0)) %>% 
  distinct()

df1_naiss_14 <- tab_RNE_IRC_b_corr3_1 %>% 
  left_join(tab_elus_finale_c, by = c("code_comm_insee_14" = "code_insee_final", "nom_elu_14_regul" = "nom_naiss", "prenom_elu_14_regul" = "prenom_usuel_corr", "sexe_14" = "code_sexe"))

df1_naiss_14 <- df1_naiss_14 %>% 
  mutate(indicatrice_4 = ifelse(!is.na(bcr_personne), 1, 0)) %>% 
  distinct()

df1_marital_14 <- tab_RNE_IRC_b_corr3_1 %>% 
  left_join(tab_elus_finale_c, by = c("code_comm_insee_14" = "code_insee_final", "nom_elu_14_regul" = "nom_marital", "prenom_elu_14_regul" = "prenom_usuel_corr", "sexe_14" = "code_sexe"))

df1_marital_14 <- df1_marital_14 %>% 
  mutate(indicatrice_4 = ifelse(!is.na(bcr_personne), 1, 0)) %>% 
  distinct()


df1_14 <- bind_rows(df1_usuel_14, df1_naiss_14, df1_marital_14)


#0) Ajustements
df1_14 <- df1_14 %>% 
  select(-nom_usuel, -nom_naiss, -nom_marital) %>% 
  rename(nom_usuel = nom_usuel_copie,
         nom_naiss = nom_naiss_copie,
         nom_marital = nom_marital_copie) #Pour garder les information sur usuel et naiss d'origine + homogénéiser

df1_14 <- df1_14 %>% 
  relocate(nom_usuel, .before = prenom_etat_civil_premier)

df1_14 <- df1_14 %>% 
  relocate(nom_naiss, .before = prenom_etat_civil_premier)

df1_14 <- df1_14 %>% 
  relocate(nom_marital, .before = prenom_etat_civil_premier)


df1_14_bis <- df1_14 %>% 
  filter(indicatrice_4 == 1) %>% 
  arrange(ID_commun_avecDep, desc(dt_fin_indiv)) %>% 
  distinct(ID_commun_avecDep, .keep_all = TRUE) #11 doublons persistent sinon (idéalement il faudrait les corriger ...)

df1_14_ter <- df1_14 %>% 
  filter(indicatrice_4 == 0)%>% 
  arrange(ID_commun_avecDep, desc(dt_fin_indiv))

df1_14_ter_corr <- anti_join(df1_14_ter, df1_14_bis, by = "ID_commun_avecDep")



test <- df1_14_bis %>% 
  filter(date_naissance_14 == date_naiss)#0 : Parfait : on voit bien les erreurs dans les dates de naissances 
```

### 3.4.2 merge 2020
```{r, echo=TRUE, message=FALSE, warning=FALSE}
df1_usuel_20 <- tab_RNE_IRC_b_corr3_1 %>% 
  left_join(tab_elus_finale_c, by = c("code_comm_insee_20" = "code_insee_final", "nom_elu_20_regul" = "nom_usuel", "prenom_elu_20_regul" = "prenom_usuel_corr", "sexe_20" = "code_sexe"))

df1_usuel_20 <- df1_usuel_20 %>% 
  mutate(indicatrice_4 = ifelse(!is.na(bcr_personne), 1, 0)) %>% 
  distinct()

df1_naiss_20 <- tab_RNE_IRC_b_corr3_1 %>% 
  left_join(tab_elus_finale_c, by = c("code_comm_insee_20" = "code_insee_final", "nom_elu_20_regul" = "nom_naiss", "prenom_elu_20_regul" = "prenom_usuel_corr", "sexe_20" = "code_sexe"))

df1_naiss_20 <- df1_naiss_20 %>% 
  mutate(indicatrice_4 = ifelse(!is.na(bcr_personne), 1, 0)) %>% 
  distinct()

df1_marital_20 <- tab_RNE_IRC_b_corr3_1 %>% 
  left_join(tab_elus_finale_c, by = c("code_comm_insee_20" = "code_insee_final", "nom_elu_20_regul" = "nom_marital", "prenom_elu_20_regul" = "prenom_usuel_corr", "sexe_20" = "code_sexe"))

df1_marital_20 <- df1_marital_20 %>% 
  mutate(indicatrice_4 = ifelse(!is.na(bcr_personne), 1, 0)) %>% 
  distinct()


df1_20 <- bind_rows(df1_usuel_20, df1_naiss_20, df1_marital_20)


#0) Ajustements
df1_20 <- df1_20 %>% 
  select(-nom_usuel, -nom_naiss, -nom_marital) %>% 
  rename(nom_usuel = nom_usuel_copie,
         nom_naiss = nom_naiss_copie,
         nom_marital = nom_marital_copie) #Pour garder les information sur usuel et naiss d'origine + homogénéiser

df1_20 <- df1_20 %>% 
  relocate(nom_usuel, .before = prenom_etat_civil_premier)

df1_20 <- df1_20 %>% 
  relocate(nom_naiss, .before = prenom_etat_civil_premier)

df1_20 <- df1_20 %>% 
  relocate(nom_marital, .before = prenom_etat_civil_premier)


df1_20_bis <- df1_20 %>% 
  filter(indicatrice_4 == 1) %>% 
  arrange(ID_commun_avecDep, desc(dt_fin_indiv)) %>% 
  distinct(ID_commun_avecDep, .keep_all = TRUE) #11 doublons persistent sinon (idéalement il faudrait les corriger ...)

df1_20_ter <- df1_20 %>% 
  filter(indicatrice_4 == 0)%>% 
  arrange(ID_commun_avecDep, desc(dt_fin_indiv))

df1_20_ter_corr <- anti_join(df1_20_ter, df1_20_bis, by = "ID_commun_avecDep")



test <- df1_20_bis %>% 
  filter(date_naissance_20 == date_naiss)#0 : Parfait : on voit bien les erreurs dans les dates de naissances
```

### 3.4.3 merge 2021
```{r, echo=TRUE, message=FALSE, warning=FALSE}
df1_usuel_21 <- tab_RNE_IRC_b_corr3_1 %>% 
  left_join(tab_elus_finale_c, by = c("code_comm_insee_21" = "code_insee_final", "nom_elu_21_regul" = "nom_usuel", "prenom_elu_21_regul" = "prenom_usuel_corr", "sexe_21" = "code_sexe"))

df1_usuel_21 <- df1_usuel_21 %>% 
  mutate(indicatrice_4 = ifelse(!is.na(bcr_personne), 1, 0)) %>% 
  distinct()

df1_naiss_21 <- tab_RNE_IRC_b_corr3_1 %>% 
  left_join(tab_elus_finale_c, by = c("code_comm_insee_21" = "code_insee_final", "nom_elu_21_regul" = "nom_naiss", "prenom_elu_21_regul" = "prenom_usuel_corr", "sexe_21" = "code_sexe"))

df1_naiss_21 <- df1_naiss_21 %>% 
  mutate(indicatrice_4 = ifelse(!is.na(bcr_personne), 1, 0)) %>% 
  distinct()

df1_marital_21 <- tab_RNE_IRC_b_corr3_1 %>% 
  left_join(tab_elus_finale_c, by = c("code_comm_insee_21" = "code_insee_final", "nom_elu_21_regul" = "nom_marital", "prenom_elu_21_regul" = "prenom_usuel_corr", "sexe_21" = "code_sexe"))

df1_marital_21 <- df1_marital_21 %>% 
  mutate(indicatrice_4 = ifelse(!is.na(bcr_personne), 1, 0)) %>% 
  distinct()


df1_21 <- bind_rows(df1_usuel_21, df1_naiss_21, df1_marital_21)


#0) Ajustements
df1_21 <- df1_21 %>% 
  select(-nom_usuel, -nom_naiss, -nom_marital) %>% 
  rename(nom_usuel = nom_usuel_copie,
         nom_naiss = nom_naiss_copie,
         nom_marital = nom_marital_copie) #Pour garder les information sur usuel et naiss d'origine + homogénéiser

df1_21 <- df1_21 %>% 
  relocate(nom_usuel, .before = prenom_etat_civil_premier)

df1_21 <- df1_21 %>% 
  relocate(nom_naiss, .before = prenom_etat_civil_premier)

df1_21 <- df1_21 %>% 
  relocate(nom_marital, .before = prenom_etat_civil_premier)


df1_21_bis <- df1_21 %>% 
  filter(indicatrice_4 == 1) %>% 
  arrange(ID_commun_avecDep, desc(dt_fin_indiv)) %>% 
  distinct(ID_commun_avecDep, .keep_all = TRUE) #11 doublons persistent sinon (idéalement il faudrait les corriger ...)

df1_21_ter <- df1_21 %>% 
  filter(indicatrice_4 == 0)%>% 
  arrange(ID_commun_avecDep, desc(dt_fin_indiv))

df1_21_ter_corr <- anti_join(df1_21_ter, df1_21_bis, by = "ID_commun_avecDep")



test <- df1_21_bis %>% 
  filter(date_naissance_21 == date_naiss)#0 : Parfait : on voit bien les erreurs dans les dates de naissances
```

### 3.4.4 merge 2025
```{r, echo=TRUE, message=FALSE, warning=FALSE}
df1_usuel_25 <- tab_RNE_IRC_b_corr3_1 %>% 
  left_join(tab_elus_finale_c, by = c("code_comm_insee_25" = "code_insee_final", "nom_elu_25_regul" = "nom_usuel", "prenom_elu_25_regul" = "prenom_usuel_corr", "sexe_25" = "code_sexe"))

df1_usuel_25 <- df1_usuel_25 %>% 
  mutate(indicatrice_4 = ifelse(!is.na(bcr_personne), 1, 0)) %>% 
  distinct()

df1_naiss_25 <- tab_RNE_IRC_b_corr3_1 %>% 
  left_join(tab_elus_finale_c, by = c("code_comm_insee_25" = "code_insee_final", "nom_elu_25_regul" = "nom_naiss", "prenom_elu_25_regul" = "prenom_usuel_corr", "sexe_25" = "code_sexe"))

df1_naiss_25 <- df1_naiss_25 %>% 
  mutate(indicatrice_4 = ifelse(!is.na(bcr_personne), 1, 0)) %>% 
  distinct()

df1_marital_25 <- tab_RNE_IRC_b_corr3_1 %>% 
  left_join(tab_elus_finale_c, by = c("code_comm_insee_25" = "code_insee_final", "nom_elu_25_regul" = "nom_marital", "prenom_elu_25_regul" = "prenom_usuel_corr", "sexe_25" = "code_sexe"))

df1_marital_25 <- df1_marital_25 %>% 
  mutate(indicatrice_4 = ifelse(!is.na(bcr_personne), 1, 0)) %>% 
  distinct()


df1_25 <- bind_rows(df1_usuel_25, df1_naiss_25, df1_marital_25)


#0) Ajustements
df1_25 <- df1_25 %>% 
  select(-nom_usuel, -nom_naiss, -nom_marital) %>% 
  rename(nom_usuel = nom_usuel_copie,
         nom_naiss = nom_naiss_copie,
         nom_marital = nom_marital_copie) #Pour garder les information sur usuel et naiss d'origine + homogénéiser

df1_25 <- df1_25 %>% 
  relocate(nom_usuel, .before = prenom_etat_civil_premier)

df1_25 <- df1_25 %>% 
  relocate(nom_naiss, .before = prenom_etat_civil_premier)

df1_25 <- df1_25 %>% 
  relocate(nom_marital, .before = prenom_etat_civil_premier)


df1_25_bis <- df1_25 %>% 
  filter(indicatrice_4 == 1) %>% 
  arrange(ID_commun_avecDep, desc(dt_fin_indiv)) %>% 
  distinct(ID_commun_avecDep, .keep_all = TRUE) #11 doublons persistent sinon (idéalement il faudrait les corriger ...)

df1_25_ter <- df1_25 %>% 
  filter(indicatrice_4 == 0)%>% 
  arrange(ID_commun_avecDep, desc(dt_fin_indiv))

df1_25_ter_corr <- anti_join(df1_25_ter, df1_25_bis, by = "ID_commun_avecDep")



test <- df1_25_bis %>% 
  filter(date_naissance_25 == date_naiss)#0 : Parfait : on voit bien les erreurs dans les dates de naissances
```

### 3.4.5 Assemblage intermédiaire
```{r, echo=TRUE, message=FALSE, warning=FALSE}
tab_RNE_IRC_a_corr2 <- readRDS("working_directory")
#-------------------------------------------------------------------------------------------------------------------------------

tab_bis <- bind_rows(df1_14_bis, df1_20_bis, df1_21_bis, df1_25_bis) %>% 
  distinct()
nrow(tab_bis)#4,072

tab_RNE_IRC_a_corr3 <- bind_rows(tab_RNE_IRC_a_corr2, tab_bis) %>% 
  distinct(ID_commun_avecDep, .keep_all = TRUE)
nrow(tab_RNE_IRC_a_corr3)#384,883


tab_ter <- bind_rows(df1_14_ter, df1_20_ter, df1_21_ter, df1_25_ter) %>% 
  distinct()

tab_RNE_IRC_b_corr4 <- anti_join(tab_ter, tab_RNE_IRC_a_corr3, by = "ID_commun_avecDep")

nrow(tab_RNE_IRC_b_corr4)#492,889

#384883 + 492889 = 877772 : OK


#0) Affichage markdown : 
tab_RNE_IRC_a_corr3 %>% 
  head(10) %>% 
  collect() %>% 
  dplyr::select(ID_commun_avecDep,
                tab_RNE_2014_b_indic,
                tab_RNE_2020_b_indic,
                tab_RNE_2021_b_indic,
                tab_RNE_2025_b_indic,
                bcr_personne,
                assembl_ID_naiss_avecDep,
                assembl_ID_usuel_avecDep,
                assembl_ID_marital_avecDep,
                ID_codeInsee_dt_naiss)%>% 
  gt(caption = "tab_RNE_IRC_a_corr3") %>% 
  tab_style(cell_text(size = px(12)), 
            locations = cells_body())

tab_RNE_IRC_b_corr4 %>% 
  head(10) %>% 
  collect() %>% 
  dplyr::select(ID_commun_avecDep,
                tab_RNE_2014_b_indic,
                tab_RNE_2020_b_indic,
                tab_RNE_2021_b_indic,
                tab_RNE_2025_b_indic,
                bcr_personne,
                assembl_ID_naiss_avecDep,
                assembl_ID_usuel_avecDep,
                assembl_ID_marital_avecDep)%>% 
  gt(caption = "tab_RNE_IRC_b_corr4") %>% 
  tab_style(cell_text(size = px(12)), 
            locations = cells_body())
#-------------------------------------------------------------------------------------------------------------------------------
saveRDS(tab_RNE_IRC_a_corr3, file = "working_directory")
saveRDS(tab_RNE_IRC_b_corr4, file = "working_directory")
```


### 4.4.6 Vérif 4
```{r, echo=TRUE, message=FALSE, warning=FALSE}
tab_cotiz_14_26 <- readRDS("working_directory")
tab_RNE_IRC_a_corr3 <- readRDS("working_directory")
#-------------------------------------------------------------------------------------------------------------------------------

verif4 <- anti_join(tab_cotiz_14_26, tab_RNE_IRC_a_corr3, by = "bcr_personne")

```


## 3.5 Merge 5 

```{r, echo=TRUE, message=FALSE, warning=FALSE}
tab_RNE_IRC_b_corr4 <- readRDS("working_directory")
tab_elus_finale_c <- readRDS("working_directory")
#-------------------------------------------------------------------------------------------------------------------------------

#Parti droite (IRC) de table = vide => si on re rapproche IRC => supprimer IRC / ne garder que la partie RNE de la table

tab_RNE_IRC_b_corr4_1 <- tab_RNE_IRC_b_corr4 %>% 
  select(ID_commun_avecDep, tab_RNE_2014_b_indic, tab_RNE_2020_b_indic, tab_RNE_2021_b_indic, tab_RNE_2025_b_indic, departement_14, lib_departement_14, lib_commune_14, nbre_hab_comm_14, nom_elu_14_regul, prenom_elu_14_regul, sexe_14, date_naissance_14, PCS, lib_profession_14, code_comm_insee_14, Jour_naiss_14, Mois_naiss_14, Annee_naiss_14, lib_fonction_14, fonction_tri_14, assembl_ID_14, ID_codeInsee_dt_naiss_14, nationalite_elu_14, departement_20, lib_departement_20, code_comm_insee_20, lib_commune_20, nom_elu_20, prenom_elu_20, sexe_20, date_naissance_20, code_profession_20, lib_profession_20, date_debut_mandat_20, lib_fonction_20, date_debut_fonction_20, nationalite_elu_20, nom_elu_20_regul, prenom_elu_20_regul, fonction_tri_20, Jour_naiss_20, Mois_naiss_20, Annee_naiss_20, assembl_ID_20, ID_codeInsee_dt_naiss_20, departement_2021, lib_departement_21, code_comm_insee_21, lib_commune_21, nom_elu_21, prenom_elu_21, sexe_21, date_naissance_21, code_profession_21, lib_profession_21, date_debut_mandat_21, lib_fonction_21, date_debut_fonction_21, nationalite_elu_21, nom_elu_21_regul, prenom_elu_21_regul, fonction_tri_21, Jour_naiss_21, Mois_naiss_21, Annee_naiss_21, assembl_ID_21, ID_codeInsee_dt_naiss_21, departement_25, lib_departement_25, code_comm_insee_25, lib_commune_25, nom_elu_25, prenom_elu_25, sexe_25, date_naissance_25, code_profession_25, lib_profession_25, date_debut_mandat_25, lib_fonction_25, date_debut_fonction_25, nationalite_elu_25, fonction_tri_25, Jour_naiss_25, Mois_naiss_25, Annee_naiss_25, nom_elu_25_regul, prenom_elu_25_regul, assembl_ID_25, ID_codeInsee_dt_naiss_25, indicatrice)

tab_elus_finale_c <- tab_elus_finale_c %>%
  mutate(ID_codeInsee_dt_naiss_copie = ID_codeInsee_dt_naiss,
         code_sexe_copie = code_sexe)
```

### 3.5.1 merge 2014
```{r, echo=TRUE, message=FALSE, warning=FALSE}
df1_14 <- tab_RNE_IRC_b_corr4_1 %>% 
  left_join(tab_elus_finale_c, by = c("ID_codeInsee_dt_naiss_14" = "ID_codeInsee_dt_naiss", "sexe_14" = "code_sexe"))

df1_14 <- df1_14 %>% 
  mutate(indicatrice_5 = ifelse(!is.na(bcr_personne), 1, 0)) %>% 
  distinct()


df1_14_bis <- df1_14 %>% 
  filter(indicatrice_5 == 1) %>% 
  arrange(ID_commun_avecDep, desc(dt_fin_indiv)) %>% 
  distinct(ID_commun_avecDep, .keep_all = TRUE) #11 doublons persistent sinon (idéalement il faudrait les corriger ...)

df1_14_ter <- df1_14 %>% 
  filter(indicatrice_5 == 0)%>% 
  arrange(ID_commun_avecDep, desc(dt_fin_indiv))

df1_14_ter_corr <- anti_join(df1_14_ter, df1_14_bis, by = "ID_commun_avecDep")
```

### 3.5.2 merge 2020
```{r, echo=TRUE, message=FALSE, warning=FALSE}
df1_20 <- tab_RNE_IRC_b_corr4_1 %>% 
  left_join(tab_elus_finale_c, by = c("ID_codeInsee_dt_naiss_20" = "ID_codeInsee_dt_naiss", "sexe_20" = "code_sexe"))

df1_20 <- df1_20 %>% 
  mutate(indicatrice_5 = ifelse(!is.na(bcr_personne), 1, 0)) %>% 
  distinct()



df1_20_bis <- df1_20 %>% 
  filter(indicatrice_5 == 1) %>% 
  arrange(ID_commun_avecDep, desc(dt_fin_indiv)) %>% 
  distinct(ID_commun_avecDep, .keep_all = TRUE) #11 doublons persistent sinon (idéalement il faudrait les corriger ...)

df1_20_ter <- df1_20 %>% 
  filter(indicatrice_5 == 0)%>% 
  arrange(ID_commun_avecDep, desc(dt_fin_indiv))

df1_20_ter_corr <- anti_join(df1_20_ter, df1_20_bis, by = "ID_commun_avecDep")
```

### 3.5.3 merge 2021
```{r, echo=TRUE, message=FALSE, warning=FALSE}
df1_21 <- tab_RNE_IRC_b_corr4_1 %>% 
  left_join(tab_elus_finale_c, by = c("ID_codeInsee_dt_naiss_21" = "ID_codeInsee_dt_naiss", "sexe_21" = "code_sexe"))

df1_21 <- df1_21 %>% 
  mutate(indicatrice_5 = ifelse(!is.na(bcr_personne), 1, 0)) %>% 
  distinct()



df1_21_bis <- df1_21 %>% 
  filter(indicatrice_5 == 1) %>% 
  arrange(ID_commun_avecDep, desc(dt_fin_indiv)) %>% 
  distinct(ID_commun_avecDep, .keep_all = TRUE) #11 doublons persistent sinon (idéalement il faudrait les corriger ...)

df1_21_ter <- df1_21 %>% 
  filter(indicatrice_5 == 0)%>% 
  arrange(ID_commun_avecDep, desc(dt_fin_indiv))

df1_21_ter_corr <- anti_join(df1_21_ter, df1_21_bis, by = "ID_commun_avecDep")
```

### 3.5.4 merge 2025
```{r, echo=TRUE, message=FALSE, warning=FALSE}
df1_25 <- tab_RNE_IRC_b_corr4_1 %>% 
  left_join(tab_elus_finale_c, by = c("ID_codeInsee_dt_naiss_25" = "ID_codeInsee_dt_naiss", "sexe_25" = "code_sexe"))

df1_25 <- df1_25 %>% 
  mutate(indicatrice_5 = ifelse(!is.na(bcr_personne), 1, 0)) %>% 
  distinct()



df1_25_bis <- df1_25 %>% 
  filter(indicatrice_5 == 1) %>% 
  arrange(ID_commun_avecDep, desc(dt_fin_indiv)) %>% 
  distinct(ID_commun_avecDep, .keep_all = TRUE) #11 doublons persistent sinon (idéalement il faudrait les corriger ...)

df1_25_ter <- df1_25 %>% 
  filter(indicatrice_5 == 0)%>% 
  arrange(ID_commun_avecDep, desc(dt_fin_indiv))

df1_25_ter_corr <- anti_join(df1_25_ter, df1_25_bis, by = "ID_commun_avecDep")
```



### 3.5.5 Assemblage intermédiaire
```{r, echo=TRUE, message=FALSE, warning=FALSE}
tab_RNE_IRC_a_corr3 <- readRDS("working_directory")
#-------------------------------------------------------------------------------------------------------------------------------

tab_bis <- bind_rows(df1_14_bis, df1_20_bis, df1_21_bis, df1_25_bis) %>% 
  distinct()
nrow(tab_bis)#3,501

tab_RNE_IRC_a_corr4 <- bind_rows(tab_RNE_IRC_a_corr3, tab_bis) %>% 
  distinct(ID_commun_avecDep, .keep_all = TRUE)
nrow(tab_RNE_IRC_a_corr4)#388,384


tab_ter <- bind_rows(df1_14_ter_corr, df1_20_ter_corr, df1_21_ter_corr, df1_25_ter_corr) %>% 
  distinct()

tab_RNE_IRC_b_corr5 <- anti_join(tab_ter, tab_RNE_IRC_a_corr4, by = "ID_commun_avecDep")

nrow(tab_RNE_IRC_b_corr5)#489,388

#388384 + 489388 = 877772 : OK


#0) Affichage markdown : 
tab_RNE_IRC_a_corr4 %>% 
  head(10) %>% 
  collect() %>% 
  dplyr::select(ID_commun_avecDep,
                tab_RNE_2014_b_indic,
                tab_RNE_2020_b_indic,
                tab_RNE_2021_b_indic,
                tab_RNE_2025_b_indic,
                bcr_personne,
                assembl_ID_naiss_avecDep,
                assembl_ID_usuel_avecDep,
                assembl_ID_marital_avecDep,
                ID_codeInsee_dt_naiss)%>% 
  gt(caption = "tab_RNE_IRC_a_corr4") %>% 
  tab_style(cell_text(size = px(12)), 
            locations = cells_body())

tab_RNE_IRC_b_corr5 %>% 
  head(10) %>% 
  collect() %>% 
  dplyr::select(ID_commun_avecDep,
                tab_RNE_2014_b_indic,
                tab_RNE_2020_b_indic,
                tab_RNE_2021_b_indic,
                tab_RNE_2025_b_indic,
                bcr_personne,
                assembl_ID_naiss_avecDep,
                assembl_ID_usuel_avecDep,
                assembl_ID_marital_avecDep)%>% 
  gt(caption = "tab_RNE_IRC_b_corr5") %>% 
  tab_style(cell_text(size = px(12)), 
            locations = cells_body())
#-------------------------------------------------------------------------------------------------------------------------------
saveRDS(tab_RNE_IRC_a_corr4, file ="working_directory")
saveRDS(tab_RNE_IRC_b_corr5, file = "working_directory")
```



### 4.5.6 Vérif 5
```{r, echo=TRUE, message=FALSE, warning=FALSE}
tab_cotiz_14_26 <- readRDS("working_directory")
tab_RNE_IRC_a_corr4 <- readRDS("working_directory")
#-------------------------------------------------------------------------------------------------------------------------------

verif5 <- anti_join(tab_cotiz_14_26, tab_RNE_IRC_a_corr4, by = "bcr_personne")


test <- verif5 %>% 
  filter(nb_bcr_emp > 1)#733 du fait de 1.2.5 (biaise un peu la variable code_insee_final) 
```











## 3.6 Corr doublons

Vérifions si des doublons persistent : OUI.  

Avant d'assembler, une étape supplémentaire est nécéssaire pour la table a_corr2 (table finale des élus indemnisés qui se sont rattachés à RNE). Il faut corriger environs 10 000 doublons qui resistent : je suis une femme je change de nom en cours de mandat => j'ai un doublon dans _ID_commun_avecDep_ ou bien, mon nom ou prénom à subit une faute de frappe => doublon dans _ID_commun_avecDep_.   

__Méthode__ : 

1) Couper __(tab_RNE_IRC_a_corr2)__ en 2 selon _bcr_personne_: df1 et df2, respectivement les doublons et les individus uniques.  

2) Couper __(df1)__ en 2 : __(df1_a)__ et __(df1_b)__, respectivement les variables de RNE (parfaitements complémentaires) et les variables IRC (substituables).  

3) Créer __(df1_a_corr)__ qui corrige __(df1_a)__ : met sur une seule ligne les informations complémentaires.  

4) Créer __(df1_b_corr)__ qui corrige __(df1_a)__ : met sur une seule lignes les informations substiruables. (trié du + récent au moins récent pour concerver l'ID avec le nom ou prénom le plus actuel).  

5) Assemblage de __(df1_a_corr)__ et __(df1_b_corr)__ en __(df1_corr)__. Puis Assemblage de __(df1_corr)__ et __(df2)__ en __(tab_RNE_IRC_a_corr5)__.  

```{r, echo=TRUE, message=FALSE, warning=FALSE}
tab_RNE_IRC_a_corr4 <- readRDS("working_directory")
#-------------------------------------------------------------------------------------------------------------------------------

#0) Ajustements de quelques variables
tab_RNE_IRC_a_corr4 <- tab_RNE_IRC_a_corr4 %>% 
  mutate(ID_codeInsee_dt_naiss = case_when(
    
    is.na(ID_codeInsee_dt_naiss) ~ ID_codeInsee_dt_naiss_copie,
    
    TRUE ~ ID_codeInsee_dt_naiss),
    
    
    
    code_insee_final = case_when(
      
      is.na(code_insee_final) ~ code_insee_final_copie,
    
    TRUE ~ code_insee_final),
    
    
    
    prenom_usuel_corr = case_when(
      
    is.na(prenom_usuel_corr) ~ prenom_usuel_corr_copie,
    
    TRUE ~ prenom_usuel_corr)
    )

#0) Ajustements de quelques variables

tab_RNE_IRC_a_corr4 <- tab_RNE_IRC_a_corr4 %>% 
  mutate(indicatrice_final = case_when(
    
    indicatrice == 1 | indicatrice_2 == 1| indicatrice_3 == 1 | indicatrice_4 == 1 | indicatrice_5 == 1 ~ 1, 
    
    TRUE ~ 0))


tab_RNE_IRC_a_corr4 <- tab_RNE_IRC_a_corr4 %>% 
  select(-indicatrice, -indicatrice_2, -indicatrice_3, -indicatrice_4, -indicatrice_5, -ID_codeInsee_dt_naiss_copie, -code_insee_final_copie, -prenom_usuel_corr_copie)


#1) 
#Comptage doublons sur bcr_personne
comptage <- tab_RNE_IRC_a_corr4 %>% 
  group_by(bcr_personne) %>% 
  summarise(n_bcr = n())

tab_RNE_IRC_a_corr4 <- tab_RNE_IRC_a_corr4 %>% 
  left_join(comptage, by = "bcr_personne") 

df1 <- tab_RNE_IRC_a_corr4 %>% 
  filter(n_bcr > 1) %>% 
  arrange(bcr_personne)
#36,665 : J'ai l'impression que se sont que des femmes qui changent de nom au cours des mandats => dès fois se rattachent sous nom de naiss ou usuel mais aussi sous nom marital. OU des individus qui sont enregistrés sous différents prénoms ou noms. Les informatins RNE sont complémentaires et IRC substituables. => correction facile.  

#Exemple :

test1 <- tab_RNE_IRC_a_corr4 %>% 
  filter(bcr_personne == "02CGT220")

test2 <- tab_RNE_IRC_a_corr4 %>% 
  filter(bcr_personne == "01NJH618")

test3 <- tab_RNE_IRC_a_corr4 %>% 
  filter(bcr_personne == "01AEX029")

#2)
df1_a <- df1 %>% 
  select(tab_RNE_2014_b_indic, tab_RNE_2020_b_indic, tab_RNE_2021_b_indic, tab_RNE_2025_b_indic, departement_14, lib_departement_14, lib_commune_14, nbre_hab_comm_14, nom_elu_14_regul, prenom_elu_14_regul, sexe_14, date_naissance_14, PCS, lib_profession_14, code_comm_insee_14, Jour_naiss_14, Mois_naiss_14, Annee_naiss_14, lib_fonction_14, fonction_tri_14, nationalite_elu_14, assembl_ID_14, ID_codeInsee_dt_naiss_14, departement_20, lib_departement_20, code_comm_insee_20, lib_commune_20, nom_elu_20, prenom_elu_20, sexe_20, date_naissance_20, code_profession_20, lib_profession_20, date_debut_mandat_20, lib_fonction_20, date_debut_fonction_20, nationalite_elu_20, nom_elu_20_regul, prenom_elu_20_regul, fonction_tri_20, Jour_naiss_20, Mois_naiss_20, Annee_naiss_20, assembl_ID_20, ID_codeInsee_dt_naiss_20, departement_2021, lib_departement_21, code_comm_insee_21, lib_commune_21, nom_elu_21, prenom_elu_21, sexe_21, date_naissance_21, code_profession_21, lib_profession_21, date_debut_mandat_21, lib_fonction_21, date_debut_fonction_21, nationalite_elu_21, nom_elu_21_regul, prenom_elu_21_regul, fonction_tri_21, Jour_naiss_21, Mois_naiss_21, Annee_naiss_21, assembl_ID_21, ID_codeInsee_dt_naiss_21, departement_25, lib_departement_25, code_comm_insee_25, lib_commune_25, nom_elu_25, prenom_elu_25, sexe_25, date_naissance_25, code_profession_25, lib_profession_25, date_debut_mandat_25, lib_fonction_25, date_debut_fonction_25, nationalite_elu_25, fonction_tri_25, Jour_naiss_25, Mois_naiss_25, Annee_naiss_25, nom_elu_25_regul, prenom_elu_25_regul, assembl_ID_25, ID_codeInsee_dt_naiss_25, bcr_personne)%>% 
  arrange(bcr_personne)

#3)
#Met des NA pour utiliser summarise(across(everything(), ~ .x[which(!is.na(.x))[1]]), .groups = "drop")
df1_a <- df1_a %>% 
  mutate(tab_RNE_2014_b_indic = case_when(tab_RNE_2014_b_indic == 0 ~ NA, TRUE ~ 1),
         tab_RNE_2020_b_indic = case_when(tab_RNE_2020_b_indic == 0 ~ NA, TRUE ~ 1),
         tab_RNE_2021_b_indic = case_when(tab_RNE_2021_b_indic == 0 ~ NA, TRUE ~ 1),
         tab_RNE_2025_b_indic = case_when(tab_RNE_2025_b_indic == 0 ~ NA, TRUE ~ 1),
         )  


df1_a_corr <- df1_a %>% 
  arrange(bcr_personne) %>% 
  group_by(bcr_personne) %>% 
  summarise(across(everything(), ~ .x[which(!is.na(.x))[1]]), .groups = "drop")

#On remet les 0
df1_a_corr <- df1_a_corr %>% 
  mutate(tab_RNE_2014_b_indic = case_when(is.na(tab_RNE_2014_b_indic) == TRUE ~ 0, TRUE ~ 1),
         tab_RNE_2020_b_indic = case_when(is.na(tab_RNE_2020_b_indic) == TRUE ~ 0, TRUE ~ 1),
         tab_RNE_2021_b_indic = case_when(is.na(tab_RNE_2021_b_indic) == TRUE ~ 0, TRUE ~ 1),
         tab_RNE_2025_b_indic = case_when(is.na(tab_RNE_2025_b_indic) == TRUE ~ 0, TRUE ~ 1),
         )

#4)
#-----------------------------------partie droite : IRC 
df1_b <- df1 %>% 
  select(-departement_14, -lib_departement_14, -lib_commune_14, -nbre_hab_comm_14, -nom_elu_14_regul, -prenom_elu_14_regul, -sexe_14, -date_naissance_14, -PCS, -lib_profession_14, -code_comm_insee_14, -Jour_naiss_14, -Mois_naiss_14, -Annee_naiss_14, -lib_fonction_14, -fonction_tri_14, -nationalite_elu_14, -assembl_ID_14, -ID_codeInsee_dt_naiss_14, -departement_20, -lib_departement_20, -code_comm_insee_20, -lib_commune_20, -nom_elu_20, -prenom_elu_20, -sexe_20, -date_naissance_20, -code_profession_20, -lib_profession_20, -date_debut_mandat_20, -lib_fonction_20, -date_debut_fonction_20, -nationalite_elu_20, -nom_elu_20_regul, -prenom_elu_20_regul, -fonction_tri_20, -Jour_naiss_20, -Mois_naiss_20, -Annee_naiss_20, -assembl_ID_20, -ID_codeInsee_dt_naiss_20, -departement_2021, -lib_departement_21, -code_comm_insee_21, -lib_commune_21, -nom_elu_21, -prenom_elu_21, -sexe_21, -date_naissance_21, -code_profession_21, -lib_profession_21, -date_debut_mandat_21, -lib_fonction_21, -date_debut_fonction_21, -nationalite_elu_21, -nom_elu_21_regul, -prenom_elu_21_regul, -fonction_tri_21, -Jour_naiss_21, -Mois_naiss_21, -Annee_naiss_21, -assembl_ID_21, -ID_codeInsee_dt_naiss_21, -departement_25, -lib_departement_25, -code_comm_insee_25, -lib_commune_25, -nom_elu_25, -prenom_elu_25, -sexe_25, -date_naissance_25, -code_profession_25, -lib_profession_25, -date_debut_mandat_25, -lib_fonction_25, -date_debut_fonction_25, -nationalite_elu_25, -fonction_tri_25, -Jour_naiss_25, -Mois_naiss_25, -Annee_naiss_25, -nom_elu_25_regul, -prenom_elu_25_regul, -assembl_ID_25, -ID_codeInsee_dt_naiss_25) %>% 
  arrange(bcr_personne, desc(tab_RNE_2025_b_indic), desc(tab_RNE_2021_b_indic), desc(tab_RNE_2020_b_indic), desc(tab_RNE_2014_b_indic))

#Il y a  des triplons aussi => ne garder que la 1ère ligne
verif <- tab_RNE_IRC_a_corr4 %>%
  filter(n_bcr>2) %>% 
  arrange(ID_commun_avecDep)#3427

df1_b <- df1_b %>% 
  select(-tab_RNE_2014_b_indic, -tab_RNE_2020_b_indic, -tab_RNE_2021_b_indic, -tab_RNE_2025_b_indic)

df1_b_corr <- df1_b %>% 
  group_by(bcr_personne) %>% 
  slice(1) %>% 
  ungroup()
  

#5)
df1_corr <- df1_a_corr %>%
  left_join(df1_b_corr, by = "bcr_personne")

df2 <- tab_RNE_IRC_a_corr4 %>% 
  filter(n_bcr == 1)

tab_RNE_IRC_a_corr5 <- bind_rows(df1_corr, df2) 

#0)
tab_RNE_IRC_a_corr5 <- tab_RNE_IRC_a_corr5 %>% 
  select(-n_bcr)

tab_RNE_IRC_a_corr5 <- tab_RNE_IRC_a_corr5 %>% 
  relocate(assembl_ID_naiss_avecDep, .before = assembl_ID_marital_avecDep)

tab_RNE_IRC_a_corr5 <- tab_RNE_IRC_a_corr5 %>% 
  relocate(ID_commun_avecDep, .before = bcr_personne)

tab_RNE_IRC_a_corr5 <- tab_RNE_IRC_a_corr5 %>% 
  relocate(bcr_personne, .before = dt_deb_indiv)


test4 <- tab_RNE_IRC_a_corr5 %>% 
  filter(bcr_personne == "02CGT220")

test5 <- tab_RNE_IRC_a_corr5 %>% 
  filter(bcr_personne == "01NJH618")

test6 <- tab_RNE_IRC_a_corr5 %>% 
  filter(bcr_personne == "01AEX029")

toto <- tab_RNE_IRC_a_corr5 %>% 
  filter(bcr_personne == "02KGK124")

toto2 <- tab_RNE_IRC_a_corr5 %>% 
  filter(bcr_personne == "03MGT505")

#0) Affichage Markdown :
tab_RNE_IRC_a_corr5 %>% 
  head(10) %>% 
  collect() %>% 
  dplyr::select(ID_commun_avecDep,
                tab_RNE_2014_b_indic,
                tab_RNE_2020_b_indic,
                tab_RNE_2021_b_indic,
                tab_RNE_2025_b_indic,
                bcr_personne,
                assembl_ID_naiss_avecDep,
                assembl_ID_usuel_avecDep,
                assembl_ID_marital_avecDep,
                ID_codeInsee_dt_naiss)%>% 
  gt(caption = "tab_RNE_IRC_a_corr5") %>% 
  tab_style(cell_text(size = px(12)), 
            locations = cells_body())
#-------------------------------------------------------------------------------------------------------------------------------
saveRDS(tab_RNE_IRC_a_corr5, file = "working_directory")
```

### 3.5.1 Vérif X
```{r, echo=TRUE, message=FALSE, warning=FALSE}
tab_cotiz_14_26 <- readRDS("/working_directory")
tab_RNE_IRC_a_corr5 <- readRDS("working_directory")
#-------------------------------------------------------------------------------------------------------------------------------

verif4 <- anti_join(tab_cotiz_14_26, tab_RNE_IRC_a_corr5, by = "bcr_personne")
#Normal qu'entre verif3 et verif4 j'ai le même nombre d'individus (car les doublons ont été corrigé sur les bcr_personne)


test <- verif4 %>% 
  filter(nb_bcr_emp > 1)


#test <- tab_RNE_finale_b %>% 
#  filter(nom_elu_25_regul == "VIDAL" & prenom_elu_25_regul == "ALAIN")#Il a un pb dans sa date de naissance son mois est 06 dans RNE et #07 dans IRC.
#
#test <- tab_RNE_finale_b %>% 
#  filter(nom_elu_25_regul == "BERTRAND" & prenom_elu_25_regul == "CHRISTOPHE" & departement_25 ==31)#0 n'est pas dans RNE
#
#test <- tab_RNE_finale_b %>% 
#  filter(nom_elu_25_regul == "DEGOUVE" & prenom_elu_25_regul == "JOANNE")#Problème date de naissance
#
#test <- tab_RNE_finale_b %>% 
#  filter(code_comm_insee_25 == 88304)
#
#
#test1 <- tab_RNE_IRC_finale %>% 
#  filter(ID_commun_avecDep == "88 - VIDAL - FRANCOISE - F - 1950 - 04 - 24")
#
#toto <- tab_RNE_IRC_finale %>% 
#  filter(ID_codeInsee_dt_naiss == "88304 - 1950 - 04 - 24")
#
#toto <- tab_RNE_IRC_finale %>% 
#  filter(code_insee_comm_11 != code_insee_final)#OK que PLM


```



## 3.7 Assemblage Final

```{r, echo=TRUE, message=FALSE, warning=FALSE}
tab_RNE_IRC_a_corr5 <- readRDS("working_directory")
tab_RNE_IRC_b_corr5 <- readRDS("/working_directory")
#-------------------------------------------------------------------------------------------------------------------------------

tab_RNE_IRC_finale <- bind_rows(tab_RNE_IRC_a_corr5, tab_RNE_IRC_b_corr5)

nrow(tab_RNE_IRC_finale)#858,700

#test <- tab_RNE_IRC_finale %>% 
#  filter(is.na(code_sexe) & indicatrice_final == 1)

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(code_sexe = case_when(
    
    is.na(code_sexe) ~ code_sexe_copie,
    
    TRUE ~ code_sexe))

#0) Ajustements de quelques variables
tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  select(-ID_codeInsee_dt_naiss_copie, -indicatrice, -indicatrice_5, -code_sexe_copie)


tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(indicatrice_final = case_when(
    is.na(indicatrice_final) ~ 0,
    
    TRUE ~ indicatrice_final))


tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>%
  select(-nom_elu_20, -prenom_elu_20, -nom_elu_21, -prenom_elu_21, -nom_elu_25, -prenom_elu_25, -code_produit_y)


tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>%
  relocate(dt_fin_indiv, .after = indicatrice_final)

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>%
  relocate(dt_deb_indiv, .after = indicatrice_final)


#0) Affichage Markdown :
tab_RNE_IRC_finale %>% 
  head(10) %>% 
  collect() %>% 
  dplyr::select(ID_commun_avecDep,
                tab_RNE_2014_b_indic,
                tab_RNE_2020_b_indic,
                tab_RNE_2021_b_indic,
                tab_RNE_2025_b_indic,
                bcr_personne,
                assembl_ID_naiss_avecDep,
                assembl_ID_usuel_avecDep,
                assembl_ID_marital_avecDep,
                ID_codeInsee_dt_naiss,
                indicatrice_final)%>% 
  gt(caption = "tab_RNE_IRC_finale") %>% 
  tab_style(cell_text(size = px(12)), 
            locations = cells_body())
#-------------------------------------------------------------------------------------------------------------------------------
saveRDS(tab_RNE_IRC_finale, file = "working_directory")
```


Les 9612 indiv qui n'avaient pas de date de naissance dans IRC ne se sont rattachés à rien => peut-être les filter et essayer de les rappatrier en créant un ID sans date de naissance mais avec département.

Vérif :
```{r, echo=TRUE, message=FALSE, warning=FALSE}
tab_cotiz_14_26 <- readRDS("working_directory")
#-------------------------------------------------------------------------------------------------------------------------------
test <- tab_RNE_IRC_finale %>% 
  filter(nom_naiss == "CLAEYS")

test <- tab_RNE_IRC_finale %>% 
  filter(nom_naiss == "HIDALGO")
#-------------------------------------------------------------------------------------------------------------------------------
cotiz_14_26 <- tab_RNE_IRC_finale %>% 
  filter(cotiz_14_26 == 1)
nrow(cotiz_14_26)#344,838

test <- anti_join(tab_cotiz_14_26, tab_RNE_IRC_finale, by = "bcr_personne")
```

# 4 Traitement données pour Stat et Econométrie

## 4.1 Fonctions RNE
```{r, echo=TRUE, message=FALSE, warning=FALSE}
tab_RNE_IRC_finale <- readRDS("working_directory")
##-------------------------------------------------------------------------------------------------------------------------------
#freq(tab_RNE_IRC_finale$cotiz_14_26)

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(cotiz_14_26 = case_when(
    is.na(cotiz_14_26) ~ 0, 
    
    TRUE ~ cotiz_14_26)
    )


tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(cotiz_14_20 = case_when(
    is.na(cotiz_14_20) ~ 0, 
    
    TRUE ~ cotiz_14_20)
    )

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(cotiz_20_26 = case_when(
    is.na(cotiz_20_26) ~ 0, 
    
    TRUE ~ cotiz_20_26)
    )

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(cotiz_14 = case_when(
    is.na(cotiz_14) ~ 0, 
    
    TRUE ~ cotiz_14)
    )

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(cotiz_20 = case_when(
    is.na(cotiz_20) ~ 0, 
    
    TRUE ~ cotiz_20)
    )

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(cotiz_21 = case_when(
    is.na(cotiz_21) ~ 0, 
    
    TRUE ~ cotiz_21)
    )

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(cotiz_25 = case_when(
    is.na(cotiz_25) ~ 0, 
    
    TRUE ~ cotiz_25)
    )
##-------------------------------------------------------------------------------------------------------------------------------
#Création de l'indicatrice présent en 2014 et 2020 / 2021 et 2025 => élus restant tout le mandat. 
#Création de l'indicatrice présent en 2014 ou 2020 / 2021 ou 2025 => Tous les élus présent sur chacun ds mandats.
tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(prez_14_et_20 = case_when(
    tab_RNE_2014_b_indic == 1 & tab_RNE_2020_b_indic == 1 ~ 1, #Indiv qui sont restés durant l'entièreté du mandat (prez juin 2014 et janvier 2020)
    
    TRUE ~ 0), 
    
    prez_20_et_26 = case_when(
    tab_RNE_2021_b_indic == 1 & tab_RNE_2025_b_indic == 1 ~ 1, #Sont les indiv qui sont resté durant l'entièreté du mandat (prez avril 2021 et décembre 2026)
    
    TRUE ~ 0), 
    
    prez_14_ou_20 = case_when(
    tab_RNE_2014_b_indic == 1 | tab_RNE_2020_b_indic == 1 ~ 1, #Indiv qui sont présents au début ou à la fin du mandat (prez juin 2014 et janvier 2020)
    
    TRUE ~ 0),
    
    prez_20_ou_26 = case_when(
    tab_RNE_2021_b_indic == 1 | tab_RNE_2025_b_indic == 1 ~ 1, #Indiv qui sont présent au début ou à la fin du mandat (prez avril 2021 et décembre 2026)
    
    TRUE ~ 0)
  )

##-------------------------------------------------------------------------------------------------------------------------------
#Création variable conseillé municipaux 
tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(cm_20 = case_when(
    tab_RNE_2020_b_indic == 1 & fonction_tri_20 == 7 ~ 1, 
    
    TRUE ~ 0),
    
    cm_21 = case_when(
    tab_RNE_2021_b_indic == 1 & fonction_tri_21 == 7 ~ 1, 
    
    TRUE ~ 0),
    
    cm_25 = case_when(
    tab_RNE_2025_b_indic == 1 & fonction_tri_25 == 7 ~ 1, 
    
    TRUE ~ 0)
  )

##-------------------------------------------------------------------------------------------------------------------------------

#Création variable adjoint
tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(adjoint_20 = case_when(
    
    fonction_tri_20 == 2 | fonction_tri_20 == 3 | fonction_tri_20 == 4 | fonction_tri_20 == 5 | fonction_tri_20 == 6 ~ 1, 
    
    TRUE ~ 0),
    
    adjoint_21 = case_when(
    
    fonction_tri_21 == 2 | fonction_tri_21 == 3 | fonction_tri_21 == 4 | fonction_tri_21 == 5 | fonction_tri_21 == 6 ~ 1, 
    
    TRUE ~ 0),
    
    adjoint_25 = case_when(
    
    fonction_tri_25 == 2 | fonction_tri_25 == 3 | fonction_tri_25 == 4 | fonction_tri_25 == 5 | fonction_tri_25 == 6 ~ 1, 
    
    TRUE ~ 0)
         )

#0) Ajustements
tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  relocate(fonction_tri_20, .after = lib_fonction_20)

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  relocate(fonction_tri_21, .after = lib_fonction_21)

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  relocate(fonction_tri_25, .after = lib_fonction_25)


tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  relocate(cm_20, .after = fonction_tri_20)

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  relocate(cm_21, .after = fonction_tri_21)

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  relocate(cm_25, .after = fonction_tri_25)


tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  relocate(adjoint_20, .after = fonction_tri_20)

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  relocate(adjoint_21, .after = fonction_tri_21)

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  relocate(adjoint_25, .after = fonction_tri_25)

##-------------------------------------------------------------------------------------------------------------------------------

#Création variable élus indemn 14, 20, 21, 25 qui met 1 lorsque l'individu est un élus indemnisé sur l'année AAAA.
tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(elu_indem_14 = case_when(
    tab_RNE_2014_b_indic == 1 & cotiz_14 == 1 ~ 1, 
    
    TRUE ~ 0), 
    
    elu_indem_20 = case_when(
    tab_RNE_2020_b_indic == 1 & cotiz_20 == 1 ~ 1, 
    
    TRUE ~ 0),
    
    elu_indem_21 = case_when(
    tab_RNE_2021_b_indic == 1 & cotiz_21 == 1 ~ 1, 
    
    TRUE ~ 0),
    
    elu_indem_25 = case_when(
    tab_RNE_2025_b_indic == 1 & cotiz_25 == 1 ~ 1, 
    
    TRUE ~ 0)
    )

#Même chose pour les individus non cotisants :

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(elu_non_indem_14 = case_when(
    cotiz_14 == 0 & tab_RNE_2014_b_indic == 1 ~ 1, 
    
    TRUE ~ 0),
    
    
    elu_non_indem_20 = case_when(
    cotiz_20 == 0 & tab_RNE_2020_b_indic == 1 ~ 1, 
    
    TRUE ~ 0),
    
    
    elu_non_indem_21 = case_when(
    cotiz_21 == 0 & tab_RNE_2021_b_indic == 1 ~ 1, 
    
    TRUE ~ 0),
    
    
    elu_non_indem_25 = case_when(
    cotiz_25 == 0 & tab_RNE_2025_b_indic == 1 ~ 1, 
    
    TRUE ~ 0)
  ) 

#0) Ajustements
tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  relocate(elu_indem_14, .after = lib_fonction_14)

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  relocate(elu_indem_20, .after = lib_fonction_20)

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  relocate(elu_indem_21, .after = lib_fonction_21)

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  relocate(elu_indem_25, .after = fonction_tri_25)


tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  relocate(elu_non_indem_14, .after = elu_indem_14)

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  relocate(elu_non_indem_20, .after = elu_indem_20)

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  relocate(elu_non_indem_21, .after = elu_indem_21)

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  relocate(elu_non_indem_25, .after = elu_indem_25)
```

## 4.2 CM indem / non indemn
```{r, echo=TRUE, message=FALSE, warning=FALSE}
#Création variables : conseillé indemnisé et conseillé non indemnisé
tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(cm_indem_20 = case_when(
    fonction_tri_20 == 7 & cotiz_20 == 1 ~ 1, 
    
    TRUE ~ 0),
  
  cm_non_indem_20 = case_when(
    fonction_tri_20 == 7 & cotiz_20 == 0 ~ 1, 
    
    TRUE ~ 0),
  
  
  cm_indem_21 = case_when(
    fonction_tri_21 == 7 & cotiz_21 == 1 ~ 1, 
    
    TRUE ~ 0),
  
  cm_non_indem_21 = case_when(
    fonction_tri_21 == 7 & cotiz_21 == 0 ~ 1, 
    
    TRUE ~ 0),
  
  
  cm_indem_25 = case_when(
    fonction_tri_25 == 7 & cotiz_25 == 1 ~ 1, 
    
    TRUE ~ 0),
  
  cm_non_indem_25 = case_when(
    fonction_tri_25 == 7 & cotiz_25 == 0 ~ 1, 
    
    TRUE ~ 0),
  )

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  relocate(cm_indem_20, .after = adjoint_20)

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  relocate(cm_indem_21, .after = adjoint_21)

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  relocate(cm_indem_21, .after = adjoint_21)

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  relocate(cm_non_indem_20, .after = cm_indem_20)

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  relocate(cm_non_indem_21, .after = cm_indem_21)

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  relocate(cm_non_indem_25, .after = cm_indem_25)



#test <- verif %>% 
#  filter(fonction_tri_20 == 7)
#
#test <- tab_RNE_IRC_finale %>% 
#  filter(cm_20 == 1)#341,841
#nrow(test)
#
#test <- tab_RNE_IRC_finale %>% 
#  filter(fonction_tri_20 == 7)#341,841 : OK
#nrow(test)
#
#test <- tab_RNE_IRC_finale %>% 
#  filter(cm_indem_20 == 1)#40482
#nrow(test)
#
#test <- tab_RNE_IRC_finale %>% 
#  filter(cm_non_indem_20 == 1)#301359
#nrow(test)
#
##40482+301359 = 341,841 #: OK
#
#
#test <- tab_RNE_IRC_finale %>% 
#  filter(cm_21 == 1)#355,057
#nrow(test)
#test <- tab_RNE_IRC_finale %>% 
#  filter(fonction_tri_21 == 7)#355,057 : OK
#nrow(test)
#test <- tab_RNE_IRC_finale %>% 
#  filter(cm_indem_21 == 1)#57318
#nrow(test)
#test <- tab_RNE_IRC_finale %>% 
#  filter(cm_non_indem_21 == 1)#297739
#nrow(test)
##57318+297739 = 355,057 : OK
#
#test <- tab_RNE_IRC_finale %>% 
#  filter(cm_25 == 1)#334,214
#nrow(test)
#test <- tab_RNE_IRC_finale %>% 
#  filter(fonction_tri_25 == 7)#334,214 : OK
#nrow(test)
#test <- tab_RNE_IRC_finale %>% 
#  filter(cm_indem_25 == 1)#51225
#nrow(test)
#test <- tab_RNE_IRC_finale %>% 
#  filter(cm_non_indem_25 == 1)#282989
#nrow(test)
##51225+282989 = 334,214 : OK
#
#test <- tab_RNE_IRC_finale %>% 
#  filter(tab_RNE_2014_b_indic == 1)
#nrow(test)#523209
#
#test <- tab_RNE_IRC_finale %>% 
#  filter(elu_indem_14 == 1)
#nrow(test)#196433
#
#test <- tab_RNE_IRC_finale %>% 
#  filter(elu_non_indem_14 == 1)
#nrow(test)#326776
#196433+326776 = 523209 : OK
```

## 4.3 Ages

Objectif : sortir la stat : l'age moyen des entrants est ... (donc il faut leur age au moment de leur entrée dans la vie politique : le second tour de la nouvelle élection).  
L'âge moyen des sortants est ... (donc il faut l'âge au moment de leur sortie dans la vie politique : le second tour de la prochaine élection).
```{r, echo=TRUE, message=FALSE, warning=FALSE} 
library(lubridate)
tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(age_deb_14_20 = floor(time_length(interval(date_naissance_14, ymd("2014-03-31")), "years")),#âge au moment du 2ème tour 2014
         
         age_fin_14_20 = floor(time_length(interval(date_naissance_20, ymd("2020-06-28")), "years")),#âge au moment du 2ème tour 2020 
         
         age_deb_20_26 = floor(time_length(interval(date_naissance_21, ymd("2020-06-28")), "years")),#âge au moment du 2ème tour 2020 
         
         age_fin_20_26 = floor(time_length(interval(date_naissance_25, ymd("2025-03-22")), "years")),#âge au moment du 2ème tour 2026
         )

# 1 : [25-]
# 2 : [26-30]
# 3 : [31-35]
# 4 : [36-40]
# 5 : [41-45]
# 6 : [46-50]
# 7 : [51-55]
# 8 : [56-60]
# 9 : [61-65]
# 10 : [66-70]
# 11 : [71-75]
# 12 : [76+]

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(tr_age_deb_14_20 = case_when(
    age_deb_14_20 <= 25 ~ 1, 
    age_deb_14_20 > 25 & age_deb_14_20 <= 30 ~ 2,
    age_deb_14_20 > 30 & age_deb_14_20 <= 35 ~ 3,
    age_deb_14_20 > 35 & age_deb_14_20 <= 40 ~ 4,
    age_deb_14_20 > 40 & age_deb_14_20 <= 45 ~ 5,
    age_deb_14_20 > 45 & age_deb_14_20 <= 50 ~ 6,
    age_deb_14_20 > 50 & age_deb_14_20 <= 55 ~ 7,
    age_deb_14_20 > 55 & age_deb_14_20 <= 60 ~ 8,
    age_deb_14_20 > 60 & age_deb_14_20 <= 65 ~ 9,
    age_deb_14_20 > 65 & age_deb_14_20 <= 70 ~ 10,
    age_deb_14_20 > 70 & age_deb_14_20 <= 75 ~ 11,
    age_deb_14_20 > 75 ~ 12, 
    
    TRUE ~ 0), 
    
    tr_age_fin_14_20 = case_when(
    age_fin_14_20 <= 25 ~ 1, 
    age_fin_14_20 > 25 & age_fin_14_20 <= 30 ~ 2,
    age_fin_14_20 > 30 & age_fin_14_20 <= 35 ~ 3,
    age_fin_14_20 > 35 & age_fin_14_20 <= 40 ~ 4,
    age_fin_14_20 > 40 & age_fin_14_20 <= 45 ~ 5,
    age_fin_14_20 > 45 & age_fin_14_20 <= 50 ~ 6,
    age_fin_14_20 > 50 & age_fin_14_20 <= 55 ~ 7,
    age_fin_14_20 > 55 & age_fin_14_20 <= 60 ~ 8,
    age_fin_14_20 > 60 & age_fin_14_20 <= 65 ~ 9,
    age_fin_14_20 > 65 & age_fin_14_20 <= 70 ~ 10,
    age_fin_14_20 > 70 & age_fin_14_20 <= 75 ~ 11,
    age_fin_14_20 > 75 ~ 12, 
    
    TRUE ~ 0),
    
    tr_age_deb_20_26 = case_when(
    age_deb_20_26 <= 25 ~ 1, 
    age_deb_20_26 > 25 & age_deb_20_26 <= 30 ~ 2,
    age_deb_20_26 > 30 & age_deb_20_26 <= 35 ~ 3,
    age_deb_20_26 > 35 & age_deb_20_26 <= 40 ~ 4,
    age_deb_20_26 > 40 & age_deb_20_26 <= 45 ~ 5,
    age_deb_20_26 > 45 & age_deb_20_26 <= 50 ~ 6,
    age_deb_20_26 > 50 & age_deb_20_26 <= 55 ~ 7,
    age_deb_20_26 > 55 & age_deb_20_26 <= 60 ~ 8,
    age_deb_20_26 > 60 & age_deb_20_26 <= 65 ~ 9,
    age_deb_20_26 > 65 & age_deb_20_26 <= 70 ~ 10,
    age_deb_20_26 > 70 & age_deb_20_26 <= 75 ~ 11,
    age_deb_20_26 > 75 ~ 12, 
    
    TRUE ~ 0),
    
    tr_age_fin_20_26 = case_when(
    age_fin_20_26 <= 25 ~ 1, 
    age_fin_20_26 > 25 & age_fin_20_26 <= 30 ~ 2,
    age_fin_20_26 > 30 & age_fin_20_26 <= 35 ~ 3,
    age_fin_20_26 > 35 & age_fin_20_26 <= 40 ~ 4,
    age_fin_20_26 > 40 & age_fin_20_26 <= 45 ~ 5,
    age_fin_20_26 > 45 & age_fin_20_26 <= 50 ~ 6,
    age_fin_20_26 > 50 & age_fin_20_26 <= 55 ~ 7,
    age_fin_20_26 > 55 & age_fin_20_26 <= 60 ~ 8,
    age_fin_20_26 > 60 & age_fin_20_26 <= 65 ~ 9,
    age_fin_20_26 > 65 & age_fin_20_26 <= 70 ~ 10,
    age_fin_20_26 > 70 & age_fin_20_26 <= 75 ~ 11,
    age_fin_20_26 > 75 ~ 12, 
    
    TRUE ~ 0)
    )

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(lib_tr_age_deb_14_20 = case_when(
    tr_age_deb_14_20 == 1 ~ "Moins 25 ans",
    tr_age_deb_14_20 == 2 ~ "26 - 30 ans",
    tr_age_deb_14_20 == 3 ~ "31 - 35 ans",
    tr_age_deb_14_20 == 4 ~ "36 - 40 ans",
    tr_age_deb_14_20 == 5 ~ "41 - 45 ans",
    tr_age_deb_14_20 == 6 ~ "46 - 50 ans",
    tr_age_deb_14_20 == 7 ~ "51 - 55 ans",
    tr_age_deb_14_20 == 8 ~ "56 - 60 ans",
    tr_age_deb_14_20 == 9 ~ "61 - 65 ans",
    tr_age_deb_14_20 == 10 ~ "66 - 70 ans",
    tr_age_deb_14_20 == 11 ~ "71 - 75 ans",
    tr_age_deb_14_20 == 12 ~ "Plus de 76"),
    
    lib_tr_age_fin_14_20 = case_when(
    tr_age_fin_14_20 == 1 ~ "Moins 25 ans",
    tr_age_fin_14_20 == 2 ~ "26 - 30 ans",
    tr_age_fin_14_20 == 3 ~ "31 - 35 ans",
    tr_age_fin_14_20 == 4 ~ "36 - 40 ans",
    tr_age_fin_14_20 == 5 ~ "41 - 45 ans",
    tr_age_fin_14_20 == 6 ~ "46 - 50 ans",
    tr_age_fin_14_20 == 7 ~ "51 - 55 ans",
    tr_age_fin_14_20 == 8 ~ "56 - 60 ans",
    tr_age_fin_14_20 == 9 ~ "61 - 65 ans",
    tr_age_fin_14_20 == 10 ~ "66 - 70 ans",
    tr_age_fin_14_20 == 11 ~ "71 - 75 ans",
    tr_age_fin_14_20 == 12 ~ "Plus de 76"),
    
    lib_tr_age_deb_20_26 = case_when(
    tr_age_deb_20_26 == 1 ~ "Moins 25 ans",
    tr_age_deb_20_26 == 2 ~ "26 - 30 ans",
    tr_age_deb_20_26 == 3 ~ "31 - 35 ans",
    tr_age_deb_20_26 == 4 ~ "36 - 40 ans",
    tr_age_deb_20_26 == 5 ~ "41 - 45 ans",
    tr_age_deb_20_26 == 6 ~ "46 - 50 ans",
    tr_age_deb_20_26 == 7 ~ "51 - 55 ans",
    tr_age_deb_20_26 == 8 ~ "56 - 60 ans",
    tr_age_deb_20_26 == 9 ~ "61 - 65 ans",
    tr_age_deb_20_26 == 10 ~ "66 - 70 ans",
    tr_age_deb_20_26 == 11 ~ "71 - 75 ans",
    tr_age_deb_20_26 == 12 ~ "Plus de 76"),
    
    lib_tr_age_fin_20_26 = case_when(
    tr_age_fin_20_26 == 1 ~ "Moins 25 ans",
    tr_age_fin_20_26 == 2 ~ "26 - 30 ans",
    tr_age_fin_20_26 == 3 ~ "31 - 35 ans",
    tr_age_fin_20_26 == 4 ~ "36 - 40 ans",
    tr_age_fin_20_26 == 5 ~ "41 - 45 ans",
    tr_age_fin_20_26 == 6 ~ "46 - 50 ans",
    tr_age_fin_20_26 == 7 ~ "51 - 55 ans",
    tr_age_fin_20_26 == 8 ~ "56 - 60 ans",
    tr_age_fin_20_26 == 9 ~ "61 - 65 ans",
    tr_age_fin_20_26 == 10 ~ "66 - 70 ans",
    tr_age_fin_20_26 == 11 ~ "71 - 75 ans",
    tr_age_fin_20_26 == 12 ~ "Plus de 76")
  )
           
tab_RNE_IRC_finale$lib_tr_age_deb_14_20 <- as.factor(tab_RNE_IRC_finale$lib_tr_age_deb_14_20)
tab_RNE_IRC_finale$lib_tr_age_fin_14_20 <- as.factor(tab_RNE_IRC_finale$lib_tr_age_fin_14_20)
tab_RNE_IRC_finale$lib_tr_age_deb_20_26 <- as.factor(tab_RNE_IRC_finale$lib_tr_age_deb_20_26)
tab_RNE_IRC_finale$lib_tr_age_fin_20_26 <- as.factor(tab_RNE_IRC_finale$lib_tr_age_fin_20_26)

tab_RNE_IRC_finale$lib_tr_age_deb_14_20= factor(tab_RNE_IRC_finale$lib_tr_age_deb_14_20, levels=c("Moins 25 ans", "26 - 30 ans", "31 - 35 ans", "36 - 40 ans", "41 - 45 ans", "46 - 50 ans", "51 - 55 ans", "56 - 60 ans", "61 - 65 ans", "66 - 70 ans", "71 - 75 ans", "Plus de 76"))

tab_RNE_IRC_finale$lib_tr_age_fin_14_20= factor(tab_RNE_IRC_finale$lib_tr_age_fin_14_20, levels=c("Moins 25 ans", "26 - 30 ans", "31 - 35 ans", "36 - 40 ans", "41 - 45 ans", "46 - 50 ans", "51 - 55 ans", "56 - 60 ans", "61 - 65 ans", "66 - 70 ans", "71 - 75 ans", "Plus de 76"))

tab_RNE_IRC_finale$lib_tr_age_deb_20_26= factor(tab_RNE_IRC_finale$lib_tr_age_deb_20_26, levels=c("Moins 25 ans", "26 - 30 ans", "31 - 35 ans", "36 - 40 ans", "41 - 45 ans", "46 - 50 ans", "51 - 55 ans", "56 - 60 ans", "61 - 65 ans", "66 - 70 ans", "71 - 75 ans", "Plus de 76"))

tab_RNE_IRC_finale$lib_tr_age_fin_20_26= factor(tab_RNE_IRC_finale$lib_tr_age_fin_20_26, levels=c("Moins 25 ans", "26 - 30 ans", "31 - 35 ans", "36 - 40 ans", "41 - 45 ans", "46 - 50 ans", "51 - 55 ans", "56 - 60 ans", "61 - 65 ans", "66 - 70 ans", "71 - 75 ans", "Plus de 76"))


levels(tab_RNE_IRC_finale$lib_tr_age_deb_14_20)  
levels(tab_RNE_IRC_finale$lib_tr_age_fin_14_20)  
levels(tab_RNE_IRC_finale$lib_tr_age_deb_20_26)  
levels(tab_RNE_IRC_finale$lib_tr_age_fin_20_26)  
           
           
##-------------------------------------------------------------------------------------------------------------------------------
#On observe erreurs dans les dates de naissances : 
#2B - ALBERTINI - FRANCOIS - M - 1949 - 08 - 25 pas même date de naissance 
#test <- tab_RNE_IRC_finale %>% 
#  filter(age_deb_14_20 > age_deb_20_26)
#
#
#test <- tab_RNE_IRC_finale %>% 
#  filter(date_naissance_14 != date_naissance_20)
##-------------------------------------------------------------------------------------------------------------------------------
```


## 4.4 taille commune
```{r, echo=TRUE, message=FALSE, warning=FALSE} 
tab_taille_comm_18 <- read_excel("Working_directoryfichier_diffusion_2021.xlsx",
                                     skip = 4)


tab_taille_comm_22 <- read_excel("Working_directoryfichier_diffusion_2025.xlsx",
                                     skip = 4)
##-------------------------------------------------------------------------------------------------------------------------------
tab_taille_comm_18 <- tab_taille_comm_18 %>%
  select(CODGEO, PMUN18) %>% 
  rename(CODGEO_2018 = CODGEO, 
         PMUN_2018 = PMUN18)

tab_taille_comm_22 <- tab_taille_comm_22 %>% 
  select(CODGEO, PMUN22) %>% 
  rename(CODGEO_2022 = CODGEO, 
         PMUN_2022 = PMUN22)

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>%
  left_join(tab_taille_comm_18, by = c("code_comm_insee_14" = "CODGEO_2018")) 

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>%
  left_join(tab_taille_comm_18, by = c("code_comm_insee_20" = "CODGEO_2018")) 

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>%
  left_join(tab_taille_comm_18, by = c("code_comm_insee_21" = "CODGEO_2018")) 

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>%
  left_join(tab_taille_comm_18, by = c("code_comm_insee_25" = "CODGEO_2018")) 

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(PMUN_2018 = coalesce(PMUN_2018.x, PMUN_2018.y, PMUN_2018.x.x, PMUN_2018.y.y))
  

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>%
  left_join(tab_taille_comm_22, by = c("code_comm_insee_14" = "CODGEO_2022"))

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>%
  left_join(tab_taille_comm_22, by = c("code_comm_insee_20" = "CODGEO_2022"))

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>%
  left_join(tab_taille_comm_22, by = c("code_comm_insee_21" = "CODGEO_2022"))

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>%
  left_join(tab_taille_comm_22, by = c("code_comm_insee_25" = "CODGEO_2022"))

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(PMUN_2022 = coalesce(PMUN_2022.x, PMUN_2022.y, PMUN_2022.x.x, PMUN_2022.y.y))

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>%
  select(-PMUN_2018.x, -PMUN_2018.y, -PMUN_2018.x.x, -PMUN_2018.y.y, -PMUN_2022.x, -PMUN_2022.y, -PMUN_2022.x.x, -PMUN_2022.y.y)

tab_taille_comm_14 <- tab_RNE_IRC_finale %>%
  filter(!is.na(code_comm_insee_14) & !is.na(nbre_hab_comm_14)) %>% 
  select(code_comm_insee_14, nbre_hab_comm_14)

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  select(-nbre_hab_comm_14)

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>%
  left_join(tab_taille_comm_14, by = "code_comm_insee_14")

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>%
  left_join(tab_taille_comm_14, by = c("code_comm_insee_20" = "code_comm_insee_14"))

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>%
  left_join(tab_taille_comm_14, by = c("code_comm_insee_21" = "code_comm_insee_14"))

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>%
  left_join(tab_taille_comm_14, by = c("code_comm_insee_25" = "code_comm_insee_14"))

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(PMUN_2014 = coalesce(nbre_hab_comm_14.x, nbre_hab_comm_14.y, nbre_hab_comm_14.x.x, nbre_hab_comm_14.y.y))

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>%
  select(-nbre_hab_comm_14.x, -nbre_hab_comm_14.y, -nbre_hab_comm_14.x.x, -nbre_hab_comm_14.y.y)



tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>%
  relocate(PMUN_2014, .before = PMUN_2018)

#test <- tab_RNE_IRC_finale %>% 
#  filter(is.na(PMUN_2022))#Logique dynamique de fusion de commune entre 2014 et 2022 => de - en - => de + en + de vide => = bien des individus plutôt présent en 2014
  
##-------------------------------------------------------------------------------------------------------------------------------

# 1 : [- de 500]
# 2 : [500-1000]
# 3 : [1000-3500]
# 4 : [3500-10000]
# 5 : [10000-20000]
# 6 : [20000-50000]
# 7 : [50000-100000]
# 8 : [100000-200000]
# 9 : [+ de 200000]

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(tr_taille_comm_14 = ifelse(is.na(PMUN_2014)==FALSE & PMUN_2014<500, 1,
                                      ifelse(is.na(PMUN_2014)==FALSE & PMUN_2014>=500 & PMUN_2014<1000, 2,
                                      ifelse(is.na(PMUN_2014)==FALSE & PMUN_2014>=1000 & PMUN_2014<3500, 3,
                                      ifelse(is.na(PMUN_2014)==FALSE & PMUN_2014>=3500 & PMUN_2014<10000, 4,
                                      ifelse(is.na(PMUN_2014)==FALSE & PMUN_2014>=10000 & PMUN_2014<20000, 5,
                                      ifelse(is.na(PMUN_2014)==FALSE & PMUN_2014>=20000 & PMUN_2014<50000, 6,
                                      ifelse(is.na(PMUN_2014)==FALSE & PMUN_2014>=50000 & PMUN_2014<100000, 7,
                                      ifelse(is.na(PMUN_2014)==FALSE & PMUN_2014>=100000 & PMUN_2014<200000, 8,
                                      ifelse(is.na(PMUN_2014)==FALSE & PMUN_2014>=200000, 9, NA))))))))))

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(tr_taille_comm_18 = ifelse(is.na(PMUN_2018)==FALSE & PMUN_2018<500, 1,
                                      ifelse(is.na(PMUN_2018)==FALSE & PMUN_2018>=500 & PMUN_2018<1000, 2,
                                      ifelse(is.na(PMUN_2018)==FALSE & PMUN_2018>=1000 & PMUN_2018<3500, 3,
                                      ifelse(is.na(PMUN_2018)==FALSE & PMUN_2018>=3500 & PMUN_2018<10000, 4,
                                      ifelse(is.na(PMUN_2018)==FALSE & PMUN_2018>=10000 & PMUN_2018<20000, 5,
                                      ifelse(is.na(PMUN_2018)==FALSE & PMUN_2018>=20000 & PMUN_2018<50000, 6,
                                      ifelse(is.na(PMUN_2018)==FALSE & PMUN_2018>=50000 & PMUN_2018<100000, 7,
                                      ifelse(is.na(PMUN_2018)==FALSE & PMUN_2018>=100000 & PMUN_2018<200000, 8,
                                      ifelse(is.na(PMUN_2018)==FALSE & PMUN_2018>=200000, 9, NA))))))))))

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(tr_taille_comm_22 = ifelse(is.na(PMUN_2022)==FALSE & PMUN_2022<500, 1,
                                      ifelse(is.na(PMUN_2022)==FALSE & PMUN_2022>=500 & PMUN_2022<1000, 2,
                                      ifelse(is.na(PMUN_2022)==FALSE & PMUN_2022>=1000 & PMUN_2022<3500, 3,
                                      ifelse(is.na(PMUN_2022)==FALSE & PMUN_2022>=3500 & PMUN_2022<10000, 4,
                                      ifelse(is.na(PMUN_2022)==FALSE & PMUN_2022>=10000 & PMUN_2022<20000, 5,
                                      ifelse(is.na(PMUN_2022)==FALSE & PMUN_2022>=20000 & PMUN_2022<50000, 6,
                                      ifelse(is.na(PMUN_2022)==FALSE & PMUN_2022>=50000 & PMUN_2022<100000, 7,
                                      ifelse(is.na(PMUN_2022)==FALSE & PMUN_2022>=100000 & PMUN_2022<200000, 8,
                                      ifelse(is.na(PMUN_2022)==FALSE & PMUN_2022>=200000, 9, NA))))))))))

#NA dans PMUN_2014 => NA dans les tranches et lib_tranches => on va dire que si tranche en 2018 = tranche en 2022 alors en 2014 sera la même tranche.
tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(tr_taille_comm_14 = case_when(
    is.na(tr_taille_comm_14) & (tr_taille_comm_18 == tr_taille_comm_22) ~ tr_taille_comm_18, 
    
    is.na(tr_taille_comm_14) & (tr_taille_comm_18 != tr_taille_comm_22) ~ tr_taille_comm_18,
    
    TRUE ~ tr_taille_comm_14)
    )

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(lib_tr_taille_comm_14 = case_when(
    tr_taille_comm_14 == 1 ~ "Moins 500 habs",
    tr_taille_comm_14 == 2 ~ "500 - 1000 habs",
    tr_taille_comm_14 == 3 ~ "1000 - 3500 habs",
    tr_taille_comm_14 == 4 ~ "3500 - 10 000 habs",
    tr_taille_comm_14 == 5 ~ "10 000 - 20 000 habs",
    tr_taille_comm_14 == 6 ~ "20 000 - 50 000 habs",
    tr_taille_comm_14 == 7 ~ "50 000 - 100 000 habs",
    tr_taille_comm_14 == 8 ~ "100 000 - 200 000 habs",
    tr_taille_comm_14 == 9 ~ "Plus de 200 000"),
    
    lib_tr_taille_comm_18 = case_when(
    tr_taille_comm_18 == 1 ~ "Moins 500 habs",
    tr_taille_comm_18 == 2 ~ "500 - 1000 habs",
    tr_taille_comm_18 == 3 ~ "1000 - 3500 habs",
    tr_taille_comm_18 == 4 ~ "3500 - 10 000 habs",
    tr_taille_comm_18 == 5 ~ "10 000 - 20 000 habs",
    tr_taille_comm_18 == 6 ~ "20 000 - 50 000 habs",
    tr_taille_comm_18 == 7 ~ "50 000 - 100 000 habs",
    tr_taille_comm_18 == 8 ~ "100 000 - 200 000 habs",
    tr_taille_comm_18 == 9 ~ "Plus de 200 000"),
    
    lib_tr_taille_comm_22 = case_when(
    tr_taille_comm_22 == 1 ~ "Moins 500 habs",
    tr_taille_comm_22 == 2 ~ "500 - 1000 habs",
    tr_taille_comm_22 == 3 ~ "1000 - 3500 habs",
    tr_taille_comm_22 == 4 ~ "3500 - 10 000 habs",
    tr_taille_comm_22 == 5 ~ "10 000 - 20 000 habs",
    tr_taille_comm_22 == 6 ~ "20 000 - 50 000 habs",
    tr_taille_comm_22 == 7 ~ "50 000 - 100 000 habs",
    tr_taille_comm_22 == 8 ~ "100 000 - 200 000 habs",
    tr_taille_comm_22 == 9 ~ "Plus de 200 000")
  )


tab_RNE_IRC_finale$lib_tr_taille_comm_14 <- as.factor(tab_RNE_IRC_finale$lib_tr_taille_comm_14)           
tab_RNE_IRC_finale$lib_tr_taille_comm_18 <- as.factor(tab_RNE_IRC_finale$lib_tr_taille_comm_18)
tab_RNE_IRC_finale$lib_tr_taille_comm_22 <- as.factor(tab_RNE_IRC_finale$lib_tr_taille_comm_22)

tab_RNE_IRC_finale$lib_tr_taille_comm_14 = factor(tab_RNE_IRC_finale$lib_tr_taille_comm_14, levels=c("Moins 500 habs", "500 - 1000 habs", "1000 - 3500 habs", "3500 - 10 000 habs", "10 000 - 20 000 habs", "20 000 - 50 000 habs", "50 000 - 100 000 habs", "100 000 - 200 000 habs", "Plus de 200 000"))

tab_RNE_IRC_finale$lib_tr_taille_comm_18 = factor(tab_RNE_IRC_finale$lib_tr_taille_comm_18, levels=c("Moins 500 habs", "500 - 1000 habs", "1000 - 3500 habs", "3500 - 10 000 habs", "10 000 - 20 000 habs", "20 000 - 50 000 habs", "50 000 - 100 000 habs", "100 000 - 200 000 habs", "Plus de 200 000"))

tab_RNE_IRC_finale$lib_tr_taille_comm_22 = factor(tab_RNE_IRC_finale$lib_tr_taille_comm_22, levels=c("Moins 500 habs", "500 - 1000 habs", "1000 - 3500 habs", "3500 - 10 000 habs", "10 000 - 20 000 habs", "20 000 - 50 000 habs", "50 000 - 100 000 habs", "100 000 - 200 000 habs", "Plus de 200 000"))

#levels(tab_RNE_IRC_finale$lib_tr_taille_comm_14)
#levels(tab_RNE_IRC_finale$lib_tr_taille_comm_18)  
#levels(tab_RNE_IRC_finale$lib_tr_taille_comm_22)  
```


```{r, echo=TRUE, message=FALSE, warning=FALSE} 
#Dummy tranche de commune 2022
tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(indic_tr_moins_500_habs = case_when(
    tr_taille_comm_22 == 1 ~ 1, 
    
    TRUE ~ 0),
    
    indic_tr_500_1000_habs = case_when(
    tr_taille_comm_22 == 1 ~ 1, 
    
    TRUE ~ 0),
    
    indic_tr_1000_3500_habs = case_when(
    tr_taille_comm_22 == 1 ~ 1, 
    
    TRUE ~ 0),
    indic_tr_3500_10000_habs = case_when(
    tr_taille_comm_22 == 1 ~ 1, 
    
    TRUE ~ 0),
    indic_tr_10000_20000_habs = case_when(
    tr_taille_comm_22 == 1 ~ 1, 
    
    TRUE ~ 0),
    indic_tr_20000_50000_habs = case_when(
    tr_taille_comm_22 == 1 ~ 1, 
    
    TRUE ~ 0),
    indic_tr_50000_100000_habs = case_when(
    tr_taille_comm_22 == 1 ~ 1, 
    
    TRUE ~ 0),
    indic_tr_100000_200000_habs = case_when(
    tr_taille_comm_22 == 1 ~ 1, 
    
    TRUE ~ 0),
    
    indic_tr_plus_200000_habs = case_when(
    tr_taille_comm_22 == 1 ~ 1, 
    
    TRUE ~ 0)
    )
```
## 4.5 PCS
```{r, echo=TRUE, message=FALSE, warning=FALSE} 
verif <- tab_RNE_IRC_finale %>% 
  group_by(lib_profession_14) %>%
  summarise(frequence = n()) %>% 
  mutate(pourcentage = (frequence / sum(frequence)) * 100) 

verif <- tab_RNE_IRC_finale %>%
  group_by(lib_profession_20) %>%
  summarise(frequence = n()) %>%
  mutate(pourcentage = (frequence / sum(frequence)) * 100) 

verif <- tab_RNE_IRC_finale %>%
  group_by(lib_profession_21) %>%
  summarise(frequence = n()) %>%
  mutate(pourcentage = (frequence / sum(frequence)) * 100)

verif <- tab_RNE_IRC_finale %>%
  group_by(lib_profession_25) %>%
  summarise(frequence = n()) %>%
  mutate(pourcentage = (frequence / sum(frequence)) * 100)


#Selection uniquement les Retraité et les anciens ...
#Code profession qui commence par 7 => retraité => prendre les Anciens ...

# 1 : retraité 
# 0 : non retraité 
tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(retraite_14_20 = case_when(
    lib_profession_14 == "Autre retraité" |
      lib_profession_14 == "Autres retraités" |
      lib_profession_14 == "Retr.artis.commerc.chef d entrep."|
      lib_profession_14 == "Retr.artis.commerc.chefs d'entrep."|
      lib_profession_14 == "Retraité agricole"|
      lib_profession_14 == "Retraité de l'enseignement"|
      lib_profession_14 == "Retraité des entreprises publiques"|
      lib_profession_14 == "Retraité des professions libérales"|
      lib_profession_14 == "Retraité fonct.publique (sf enseig.)"|
      lib_profession_14 == "Retraité salarié privé"|
      lib_profession_14 == "Retraités agricoles"|
      lib_profession_14 == "Retraités de l'enseignement"| 
      lib_profession_14 == "Retraités des entreprises publiques"|
      lib_profession_14 == "Retraités des professions libérales"|
      lib_profession_14 == "Retraités fonct.publique (sf enseig.)"|
      
      lib_profession_20 == "Anciennes professions intermédiaires"|
      lib_profession_20 == "Anciens agriculteurs exploitants"|
      lib_profession_20 == "Anciens artisans, commerçants, chefs d'entreprise"|
      lib_profession_20 == "Anciens cadres"|
      lib_profession_20 == "Anciens employés"|
      lib_profession_20 == "Anciens ouvriers"|
      lib_profession_20 == "Retr.artis.commerc.chefs d'entrep. (obsolète)"|
      lib_profession_20 == "Retraités agricoles (obsolète)"|
      lib_profession_20 == "Retraités de l'enseignement (obsolète)"|
      lib_profession_20 == "Retraités des entreprises publiques (obsolète)"|
      lib_profession_20 == "Retraités des professions libérales (obsolète)"|
      lib_profession_20 == "Retraités fonct.publique (sf enseig.) (obsolète)"|
      lib_profession_20 == "Retraités salariés privés (obsolète)"~ 1, 
    
    TRUE ~ 0),
    
    retraite_20_26 = case_when(
      lib_profession_21 == "Ancien agriculteur exploitant"|
      lib_profession_21 == "Ancien artisan, commerçant, chef d'entreprise" |
      lib_profession_21 == "Ancien cadre"|
      lib_profession_21 == "Ancien employé"|
      lib_profession_21 == "Ancien ouvrier"|
      lib_profession_21 == "Ancienne profession intermédiaire"|
      lib_profession_21 == "Retr.artis.commerc.chef d'entrep. (obsolète)"|
      lib_profession_21 == "Retraité agricole (obsolète)"|
      lib_profession_21 == "Retraité de l'enseignement (obsolète)"|
      lib_profession_21 == "Retraité des entreprises publiques (obsolète)"|
      lib_profession_21 == "Retraité des professions libérales (obsolète)"|
      lib_profession_21 == "Retraité fonct.publique (sf enseig.) (obsolète)"|
      lib_profession_21 == "Retraité salarié privé (obsolète)"|
        
      lib_profession_25 == "Ancien agriculteur exploitant"|
      lib_profession_25 == "Ancien artisan, commerçant, chef d'entreprise"|
      lib_profession_25 == "Ancien cadre"|
      lib_profession_25 == "Ancien employé"|
      lib_profession_25 == "Ancien ouvrier"|
      lib_profession_25 == "Ancienne profession intermédiaire"|
      lib_profession_25 == "Retr.artis.commerc.chef d'entrep. (obsolète)"|
      lib_profession_25 == "Retraité des entreprises publiques (obsolète)"|
      lib_profession_25 == "Retraité des professions libérales (obsolète)"|
      lib_profession_25 == "Retraité fonct.publique (sf enseig.) (obsolète)"|
      lib_profession_25 == "Retraité salarié privé (obsolète)"~ 1, 
    
    TRUE ~ 0))

#freq(tab_RNE_IRC_finale$retraite)
```

```{r, echo=TRUE, message=FALSE, warning=FALSE}
tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(retraite_14 = case_when(
    lib_profession_14 == "Autre retraité" |
      lib_profession_14 == "Autres retraités" |
      lib_profession_14 == "Retr.artis.commerc.chef d entrep."|
      lib_profession_14 == "Retr.artis.commerc.chefs d'entrep."|
      lib_profession_14 == "Retraité agricole"|
      lib_profession_14 == "Retraité de l'enseignement"|
      lib_profession_14 == "Retraité des entreprises publiques"|
      lib_profession_14 == "Retraité des professions libérales"|
      lib_profession_14 == "Retraité fonct.publique (sf enseig.)"|
      lib_profession_14 == "Retraité salarié privé"|
      lib_profession_14 == "Retraités agricoles"|
      lib_profession_14 == "Retraités de l'enseignement"| 
      lib_profession_14 == "Retraités des entreprises publiques"|
      lib_profession_14 == "Retraités des professions libérales"|
      lib_profession_14 == "Retraités fonct.publique (sf enseig.)" ~ 1, 
    
    TRUE ~ 0))

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(retraite_20 = case_when(
      lib_profession_20 == "Anciennes professions intermédiaires"|
      lib_profession_20 == "Anciens agriculteurs exploitants"|
      lib_profession_20 == "Anciens artisans, commerçants, chefs d'entreprise"|
      lib_profession_20 == "Anciens cadres"|
      lib_profession_20 == "Anciens employés"|
      lib_profession_20 == "Anciens ouvriers"|
      lib_profession_20 == "Retr.artis.commerc.chefs d'entrep. (obsolète)"|
      lib_profession_20 == "Retraités agricoles (obsolète)"|
      lib_profession_20 == "Retraités de l'enseignement (obsolète)"|
      lib_profession_20 == "Retraités des entreprises publiques (obsolète)"|
      lib_profession_20 == "Retraités des professions libérales (obsolète)"|
      lib_profession_20 == "Retraités fonct.publique (sf enseig.) (obsolète)"|
      lib_profession_20 == "Retraités salariés privés (obsolète)"~ 1, 
    
    TRUE ~ 0))


    
tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(retraite_21 = case_when(
      lib_profession_21 == "Ancien agriculteur exploitant"|
      lib_profession_21 == "Ancien artisan, commerçant, chef d'entreprise" |
      lib_profession_21 == "Ancien cadre"|
      lib_profession_21 == "Ancien employé"|
      lib_profession_21 == "Ancien ouvrier"|
      lib_profession_21 == "Ancienne profession intermédiaire"|
      lib_profession_21 == "Retr.artis.commerc.chef d'entrep. (obsolète)"|
      lib_profession_21 == "Retraité agricole (obsolète)"|
      lib_profession_21 == "Retraité de l'enseignement (obsolète)"|
      lib_profession_21 == "Retraité des entreprises publiques (obsolète)"|
      lib_profession_21 == "Retraité des professions libérales (obsolète)"|
      lib_profession_21 == "Retraité fonct.publique (sf enseig.) (obsolète)"|
      lib_profession_21 == "Retraité salarié privé (obsolète)" ~ 1, 
      
      TRUE ~ 0))


tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(retraite_25 = case_when(
      lib_profession_25 == "Ancien agriculteur exploitant"|
      lib_profession_25 == "Ancien artisan, commerçant, chef d'entreprise"|
      lib_profession_25 == "Ancien cadre"|
      lib_profession_25 == "Ancien employé"|
      lib_profession_25 == "Ancien ouvrier"|
      lib_profession_25 == "Ancienne profession intermédiaire"|
      lib_profession_25 == "Retr.artis.commerc.chef d'entrep. (obsolète)"|
      lib_profession_25 == "Retraité des entreprises publiques (obsolète)"|
      lib_profession_25 == "Retraité des professions libérales (obsolète)"|
      lib_profession_25 == "Retraité fonct.publique (sf enseig.) (obsolète)"|
      lib_profession_25 == "Retraité salarié privé (obsolète)"~ 1, 
    
    TRUE ~ 0))

```

```{r, echo=TRUE, message=FALSE, warning=FALSE}
#PCS Correction

#PCS_14 <- tab_RNE_IRC_finale %>%
#  group_by(lib_profession_14, PCS) %>% 
#  summarise(n = n()) %>% 
#  arrange(PCS)
#
#PCS_20 <- tab_RNE_IRC_finale %>%
#  group_by(lib_profession_20, code_profession_20) %>% 
#  summarise(n = n()) %>% 
#  arrange(code_profession_20)
#
#PCS_21 <- tab_RNE_IRC_finale %>%
#  group_by(lib_profession_21, code_profession_21) %>% 
#  summarise(n = n()) %>% 
#  arrange(code_profession_21)
#
#PCS_25 <- tab_RNE_IRC_finale %>%
#  group_by(lib_profession_25, code_profession_25) %>% 
#  summarise(n = n()) %>% 
#  arrange(code_profession_25)
#
#write_xlsx(PCS_14, "/xdl301/fichiers/retraites/Z_BAC_A_SABLE/ValentinF/data/PCS_14.xlsx")
#write_xlsx(PCS_20, "/xdl301/fichiers/retraites/Z_BAC_A_SABLE/ValentinF/data/PCS_20.xlsx")
#write_xlsx(PCS_21, "/xdl301/fichiers/retraites/Z_BAC_A_SABLE/ValentinF/data/PCS_21.xlsx")
#write_xlsx(PCS_25, "/xdl301/fichiers/retraites/Z_BAC_A_SABLE/ValentinF/data/PCS_25.xlsx")

##-------------------------------------------------------------------------------------------------------------------------------
PCS_14 <- read_excel("/xdl301/fichiers/retraites/Z_BAC_A_SABLE/ValentinF/data/PCS_14.xlsx")
PCS_20 <- read_excel("/xdl301/fichiers/retraites/Z_BAC_A_SABLE/ValentinF/data/PCS_20.xlsx")
PCS_21 <- read_excel("/xdl301/fichiers/retraites/Z_BAC_A_SABLE/ValentinF/data/PCS_21.xlsx")
PCS_25 <- read_excel("/xdl301/fichiers/retraites/Z_BAC_A_SABLE/ValentinF/data/PCS_25.xlsx")
##-------------------------------------------------------------------------------------------------------------------------------

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  left_join(PCS_14, by = "lib_profession_14")

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  left_join(PCS_20, by = "lib_profession_20")

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  left_join(PCS_21, by = "lib_profession_21")

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  left_join(PCS_25, by = "lib_profession_25")

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
select(-PCS.x, -PCS.y, -n.x, -n.y, -n.x.x, -n.y.y, -code_profession_20.x, -code_profession_20.y, -code_profession_21.x, -code_profession_21.y, -code_profession_25.x, -code_profession_25.y)

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>%
  relocate(lib_PCS_corr_14, .after = date_naissance_14)
tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>%
  relocate(lib_PCS_corr_20, .after = date_naissance_20)
tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>%
  relocate(lib_PCS_corr_21, .after = date_naissance_21)
tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>%
  relocate(lib_PCS_corr_25, .after = date_naissance_25)

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>%
  mutate(code_PCS_14 = case_when(
    lib_PCS_corr_14 == "Agriculteurs exploitants" ~ 1,
    
    lib_PCS_corr_14 == "Artisans, commerçants et chefs d’entreprise" ~ 2,
    
    lib_PCS_corr_14 == "Cadres et professions intellectuelles supérieures" ~ 3,
    
    lib_PCS_corr_14 == "Professions intermédiaires" ~ 4,
    
    lib_PCS_corr_14 == "Employés" ~ 5,
    
    lib_PCS_corr_14 == "Ouvriers" ~ 6,
    
    lib_PCS_corr_14 == "Sans profession et autres professions" ~ 99, 
    
    TRUE ~ 0),
    
    code_PCS_20 = case_when(
    lib_PCS_corr_20 == "Agriculteurs exploitants" ~ 1,
    
    lib_PCS_corr_20 == "Artisans, commerçants et chefs d’entreprise" ~ 2,
    
    lib_PCS_corr_20 == "Cadres et professions intellectuelles supérieures" ~ 3,
    
    lib_PCS_corr_20 == "Professions intermédiaires" ~ 4,
    
    lib_PCS_corr_20 == "Employés" ~ 5,
    
    lib_PCS_corr_20 == "Ouvriers" ~ 6,
    
    lib_PCS_corr_20 == "Sans profession et autres professions" ~ 99, 
    
    TRUE ~ 0),
    
    code_PCS_21 = case_when(
    lib_PCS_corr_21 == "Agriculteurs exploitants" ~ 1,
    
    lib_PCS_corr_21 == "Artisans, commerçants et chefs d’entreprise" ~ 2,
    
    lib_PCS_corr_21 == "Cadres et professions intellectuelles supérieures" ~ 3,
    
    lib_PCS_corr_21 == "Professions intermédiaires" ~ 4,
    
    lib_PCS_corr_21 == "Employés" ~ 5,
    
    lib_PCS_corr_21 == "Ouvriers" ~ 6,
    
    lib_PCS_corr_21 == "Sans profession et autres professions" ~ 99, 
    
    TRUE ~ 0),
    
    code_PCS_25 = case_when(
    lib_PCS_corr_25 == "Agriculteurs exploitants" ~ 1,
    
    lib_PCS_corr_25 == "Artisans, commerçants et chefs d’entreprise" ~ 2,
    
    lib_PCS_corr_25 == "Cadres et professions intellectuelles supérieures" ~ 3,
    
    lib_PCS_corr_25 == "Professions intermédiaires" ~ 4,
    
    lib_PCS_corr_25 == "Employés" ~ 5,
    
    lib_PCS_corr_25 == "Ouvriers" ~ 6,
    
    lib_PCS_corr_25 == "Sans profession et autres professions" ~ 99, 
    
    TRUE ~ 0)
    )

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>%
  relocate(code_PCS_14, .after = lib_PCS_corr_14)
tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>%
  relocate(code_PCS_20, .after = lib_PCS_corr_20)
tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>%
  relocate(code_PCS_21, .after = lib_PCS_corr_21)
tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>%
  relocate(code_PCS_25, .after = lib_PCS_corr_25)

tab_RNE_IRC_finale$lib_PCS_corr_14 <- as.factor(tab_RNE_IRC_finale$lib_PCS_corr_14)           
tab_RNE_IRC_finale$lib_PCS_corr_20 <- as.factor(tab_RNE_IRC_finale$lib_PCS_corr_20) 
tab_RNE_IRC_finale$lib_PCS_corr_21 <- as.factor(tab_RNE_IRC_finale$lib_PCS_corr_21) 
tab_RNE_IRC_finale$lib_PCS_corr_25 <- as.factor(tab_RNE_IRC_finale$lib_PCS_corr_25)

tab_RNE_IRC_finale$lib_PCS_corr_14 = factor(tab_RNE_IRC_finale$lib_PCS_corr_14, levels=c("Agriculteurs exploitants", "Artisans, commerçants et chefs d’entreprise", "Cadres et professions intellectuelles supérieures", "Professions intermédiaires", "Employés", "Ouvriers", "Sans profession et autres professions"))

tab_RNE_IRC_finale$lib_PCS_corr_20 = factor(tab_RNE_IRC_finale$lib_PCS_corr_20, levels=c("Agriculteurs exploitants", "Artisans, commerçants et chefs d’entreprise", "Cadres et professions intellectuelles supérieures", "Professions intermédiaires", "Employés", "Ouvriers", "Sans profession et autres professions"))

tab_RNE_IRC_finale$lib_PCS_corr_21 = factor(tab_RNE_IRC_finale$lib_PCS_corr_21, levels=c("Agriculteurs exploitants", "Artisans, commerçants et chefs d’entreprise", "Cadres et professions intellectuelles supérieures", "Professions intermédiaires", "Employés", "Ouvriers", "Sans profession et autres professions"))

tab_RNE_IRC_finale$lib_PCS_corr_25 = factor(tab_RNE_IRC_finale$lib_PCS_corr_25, levels=c("Agriculteurs exploitants", "Artisans, commerçants et chefs d’entreprise", "Cadres et professions intellectuelles supérieures", "Professions intermédiaires", "Employés", "Ouvriers", "Sans profession et autres professions"))

```


## 4.6 Sexe
Homogénéisation du sexe, 2 cas :    

1) J'ai un sexe différent entre les années de RNE, par chance ces 81 individus sont rattaché à IRC => nous prennons _code_sexe_. Ainsi, l'arbitrage effectué pour les individus ayant réellement (pas une erreur) changé de sexe est d'enregistré l'information au moment de sa cotisation.  

2) J'ai un sexe différent entre toutes les année de RNE et IRC => garder les informations de RNE. On estime que cela est une erreur dans IRC.  

3) Création de la variable _sexe_final_ qui enregistre dans une seule variable le sexe de tous les individus de la table.
```{r, echo=TRUE, message=FALSE, warning=FALSE} 
#1) 
erreur_sexe <- tab_RNE_IRC_finale %>% 
  filter(sexe_14 != sexe_20 | sexe_14 != sexe_21 | sexe_14 != sexe_25 | sexe_20 != sexe_21 | sexe_20 != sexe_25 | sexe_21 != sexe_25) %>% 
  select(sexe_14, sexe_20, sexe_21, sexe_25, code_sexe, ID_commun_avecDep, assembl_ID_14, assembl_ID_20, assembl_ID_21, assembl_ID_25, assembl_ID_usuel_avecDep, assembl_ID_naiss_avecDep, assembl_ID_marital_avecDep, bcr_personne, code_comm_insee_14, code_comm_insee_20, code_comm_insee_21, code_comm_insee_25, indicatrice_final)

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(sexe_14 = case_when(
    (sexe_14 != sexe_20 | sexe_14 != sexe_21 | sexe_14 != sexe_25 | sexe_20 != sexe_21 | sexe_20 != sexe_25 | sexe_21 != sexe_25) ~ code_sexe,
    #Au moins un sexe différent
    TRUE ~ sexe_14),
    
    sexe_20 = case_when(
    !(sexe_14 == sexe_20 & sexe_14 == sexe_21 & sexe_14 == sexe_25) ~ code_sexe,
    
    TRUE ~ sexe_20),
    
    sexe_21 = case_when(
    !(sexe_14 == sexe_20 & sexe_14 == sexe_21 & sexe_14 == sexe_25) ~ code_sexe,
    
    TRUE ~ sexe_21),
    
    sexe_25 = case_when(
    !(sexe_14 == sexe_20 & sexe_14 == sexe_21 & sexe_14 == sexe_25) ~ code_sexe,
    
    TRUE ~ sexe_25)
    )

#2)
erreur_sexe <- tab_RNE_IRC_finale %>% 
  filter(code_sexe != sexe_14 | code_sexe != sexe_20 | code_sexe != sexe_21 | code_sexe != sexe_25) %>% 
  select(sexe_14, sexe_20, sexe_21, sexe_25, code_sexe, ID_commun_avecDep, assembl_ID_14, assembl_ID_20, assembl_ID_21, assembl_ID_25, assembl_ID_usuel_avecDep, assembl_ID_naiss_avecDep, assembl_ID_marital_avecDep, bcr_personne, code_comm_insee_14, code_comm_insee_20, code_comm_insee_21, code_comm_insee_25, indicatrice_final)


tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>%
  mutate(code_sexe_corr = case_when(
    indicatrice_final==1 & !is.na(sexe_14) & (code_sexe != sexe_14 | code_sexe != sexe_20 | code_sexe != sexe_21 | code_sexe != sexe_25) ~ sexe_14, 
    
    indicatrice_final==1 & !is.na(sexe_20) & (code_sexe != sexe_14 | code_sexe != sexe_20 | code_sexe != sexe_21 | code_sexe != sexe_25) ~ sexe_20,
    
    indicatrice_final==1 & !is.na(sexe_21) & (code_sexe != sexe_14 | code_sexe != sexe_20 | code_sexe != sexe_21 | code_sexe != sexe_25) ~ sexe_21,
    
    indicatrice_final==1 & !is.na(sexe_25) & (code_sexe != sexe_14 | code_sexe != sexe_20 | code_sexe != sexe_21 | code_sexe != sexe_25) ~ sexe_25,
    
    TRUE ~ code_sexe)
    )

#3) 
tab_RNE_IRC_finale$sexe_14 <- as.character(tab_RNE_IRC_finale$sexe_14)
tab_RNE_IRC_finale$sexe_20 <- as.character(tab_RNE_IRC_finale$sexe_20)
tab_RNE_IRC_finale$sexe_21 <- as.character(tab_RNE_IRC_finale$sexe_21)
tab_RNE_IRC_finale$sexe_25 <- as.character(tab_RNE_IRC_finale$sexe_25)

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(sexe_final = case_when(
    !is.na(sexe_14) ~ sexe_14, 
    
    !is.na(sexe_20) ~ sexe_20,
    
    !is.na(sexe_21) ~ sexe_21,
    
    !is.na(sexe_25) ~ sexe_25,
    
    TRUE ~ NA)
    )

#verif <-  tab_RNE_IRC_finale %>% 
#  select(sexe_14, sexe_20, sexe_21, sexe_25, code_sexe, code_sexe_corr, sexe_final)

#test <- tab_RNE_IRC_finale %>% 
#  filter(sexe_final != sexe_14 | sexe_final != sexe_20 | sexe_final != sexe_21 | sexe_final != sexe_25 | sexe_final != code_sexe_corr)


tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>%
  relocate(code_sexe_corr, .before = code_sexe) %>% 
  select(-code_sexe)

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(lib_sexe_final = case_when(
    
    sexe_final == "F" ~ "FEMME",
    
    TRUE ~ "HOMME"))

tab_RNE_IRC_finale$lib_sexe_final <- as.factor(tab_RNE_IRC_finale$lib_sexe_final)

levels(tab_RNE_IRC_finale$lib_sexe_final)

#verif <-  tab_RNE_IRC_finale %>% 
#  select(sexe_14, sexe_20, sexe_21, sexe_25, code_sexe_corr, sexe_final, lib_sexe_final)

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(sexe_F_indic = case_when(
    sexe_final == "F" ~ 1, 
    
    TRUE ~ 0), 
    
    sexe_H_indic = case_when(
    sexe_final == "M" ~ 1, 
    
    TRUE ~ 0)
    )
```


## 4.7 DCD
Création variables dcd avant 2014, dcd pendant le mandat 2014-2020 et dcd pendant le mandat 2020-2026. 
```{r, echo=TRUE, message=FALSE, warning=FALSE}
tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(dcd_indic = case_when(
    !is.na(date_deces_corr) ~ 1, 
    
    TRUE ~ 0
  ))


tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(dcd_av_14 = case_when(
    dcd_indic == 1 & date_deces_corr <= as.Date("2014-04-01") ~ 1, 
    
    TRUE ~ 0), 
    
    dcd_14_20 = case_when(
    dcd_indic == 1 & date_deces_corr > as.Date("2014-04-01") & date_deces_corr <= as.Date("2020-07-01") ~ 1, 
    
    TRUE ~ 0),
    
    dcd_20_26 = case_when(
    dcd_indic == 1 & date_deces_corr > as.Date("2020-07-01") & date_deces_corr <= as.Date("2026-04-01") ~ 1, 
    
    TRUE ~ 0)
  )

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>%
  relocate(date_deces_corr, .before = dcd_indic)

#test <- tab_RNE_IRC_finale %>%
#  filter((dcd_av_14 == 1 & dcd_14_20 ==1) | (dcd_av_14 == 1 & dcd_20_26 ==1)| (dcd_14_20 == 1 & dcd_20_26 ==1))
```


## 4.8 Fusions
```{r, echo=TRUE, message=FALSE, warning=FALSE} 
Tab_fusion_2026 <- read_excel("/xdl301/fichiers/retraites/Z_BAC_A_SABLE/ValentinF/data/Tab_fusion_2026.xlsx", skip = 5)

tab_scission_communes_2026 <- read_excel("/xdl301/fichiers/retraites/Z_BAC_A_SABLE/ValentinF/data/tab_scission_communes_2026.xlsx", skip = 5)


tab_fusion_20_26 <- Tab_fusion_2026 %>% 
  filter(ANNEE_MODIF == 2020 | ANNEE_MODIF == 2021 | ANNEE_MODIF == 2022  | ANNEE_MODIF == 2023 | ANNEE_MODIF == 2024 | ANNEE_MODIF == 2025)

tab_fusion_14_20 <- Tab_fusion_2026 %>% 
  filter(ANNEE_MODIF == 2014 | ANNEE_MODIF == 2015 | ANNEE_MODIF == 2016  | ANNEE_MODIF == 2017 | ANNEE_MODIF == 2018 | ANNEE_MODIF == 2019)

tab_scission_20_26 <- tab_scission_communes_2026 %>% 
  filter(ANNEE_MODIF == 2020 | ANNEE_MODIF == 2021 | ANNEE_MODIF == 2022  | ANNEE_MODIF == 2023 | ANNEE_MODIF == 2024 | ANNEE_MODIF == 2025)

tab_scission_14_20 <- tab_scission_communes_2026 %>% 
  filter(ANNEE_MODIF == 2014 | ANNEE_MODIF == 2015 | ANNEE_MODIF == 2016  | ANNEE_MODIF == 2017 | ANNEE_MODIF == 2018 | ANNEE_MODIF == 2019)


tab_fusion_14_20 <- tab_fusion_14_20 %>%
  select(COM_INI, COM_FIN) %>% 
  rename(CODE_fusion_14 = COM_INI,
         CODE_fusion_19 = COM_FIN) 

tab_scission_14_20 <- tab_scission_14_20 %>%
  select(COM_INI, COM_FIN) %>% 
  rename(CODE_scission_14 = COM_INI,
         CODE_scission_20 = COM_FIN) 

tab_scission_14_20$CODE_scission_14 <- as.character(tab_scission_14_20$CODE_scission_14)

tab_fusion_20_26 <- tab_fusion_20_26 %>%
  select(COM_INI, COM_FIN) %>% 
  rename(CODE_fusion_20 = COM_INI,
         CODE_fusion_26 = COM_FIN) 

tab_scission_20_26 <- tab_scission_20_26 %>%
  select(COM_INI, COM_FIN) %>% 
  rename(CODE_scission_20 = COM_INI,
         CODE_scission_26 = COM_FIN) 

tab_scission_20_26$CODE_scission_20 <- as.character(tab_scission_20_26$CODE_scission_20)
tab_scission_20_26$CODE_scission_26 <- as.character(tab_scission_20_26$CODE_scission_26)

test <- tab_fusion_20_26 %>% 
  distinct(CODE_fusion_20)#183 communes en 2020

test <- tab_fusion_20_26 %>% 
  distinct(CODE_fusion_26)#80 communes en 2026 => 80 fusions sur le mandat 2020-2026

test <- anti_join(tab_fusion_20_26, tab_RNE_IRC_finale, by = c("CODE_fusion_20" = "code_comm_insee_21"))

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  left_join(tab_fusion_20_26, by = c("code_comm_insee_14" = "CODE_fusion_20"))

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  left_join(tab_scission_20_26, by = c("code_comm_insee_25" = "CODE_scission_26"))

#Les fusions qui ont eu lieux 
test <- tab_RNE_IRC_finale %>% 
  filter(!is.na(CODE_fusion_26) & (tab_RNE_2021_b_indic == 1 | tab_RNE_2025_b_indic == 1)  & code_comm_insee_25 == CODE_fusion_26) %>% 
  select(ID_commun_avecDep, code_comm_insee_14, code_comm_insee_21, code_comm_insee_25, CODE_fusion_26, lib_commune_21, lib_commune_25)

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(fusion_20_26 = case_when(
    
    !is.na(CODE_fusion_26) & (tab_RNE_2021_b_indic == 1 | tab_RNE_2025_b_indic == 1)  & code_comm_insee_25 == CODE_fusion_26 ~ 1, 
    
    TRUE ~ 0
    
    ))

#verif <- tab_RNE_IRC_finale %>% 
#  filter(code_comm_insee_21 != code_comm_insee_25)
#  
#toto <- anti_join(verif, test, by = "code_comm_insee_25")%>% 
#  select(ID_commun_avecDep, code_comm_insee_14, code_comm_insee_21, code_comm_insee_25, CODE_fusion_26, lib_commune_21, lib_commune_25)
#
#test1 <- tab_RNE_IRC_finale %>% 
#  filter(ID_commun_avecDep == "54 - PETIT - CHRISTIAN - M - 1954 - 01 - 22")

#Les scissions qui ont eu lieux 
test <- tab_RNE_IRC_finale %>% 
  filter(!is.na(CODE_scission_20) & (tab_RNE_2021_b_indic == 1 | tab_RNE_2025_b_indic == 1)  & code_comm_insee_21 == CODE_scission_20) %>% 
  select(ID_commun_avecDep, code_comm_insee_14, code_comm_insee_21, code_comm_insee_25, CODE_scission_20, lib_commune_21, lib_commune_25)

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(scission_20_26 = case_when(
    
    !is.na(CODE_scission_20) & (tab_RNE_2021_b_indic == 1 | tab_RNE_2025_b_indic == 1)  & code_comm_insee_21 == CODE_scission_20 ~ 1, 
    
    TRUE ~ 0
    
    ))
```


## 4.9 Créations variables explicatives pour econo
 
```{r, echo=TRUE, message=FALSE, warning=FALSE}
tab_RNE_IRC_finale$lib_PCS_corr_21 <- as.factor(tab_RNE_IRC_finale$lib_PCS_corr_21) 
tab_RNE_IRC_finale$lib_PCS_corr_25 <- as.factor(tab_RNE_IRC_finale$lib_PCS_corr_25)

tab_RNE_IRC_finale$lib_PCS_corr_21 = factor(tab_RNE_IRC_finale$lib_PCS_corr_21, levels=c("Agriculteurs exploitants", "Artisans, commerçants et chefs d’entreprise", "Cadres et professions intellectuelles supérieures", "Professions intermédiaires", "Employés", "Ouvriers", "Sans profession et autres professions"))

tab_RNE_IRC_finale$lib_PCS_corr_25 = factor(tab_RNE_IRC_finale$lib_PCS_corr_25, levels=c("Agriculteurs exploitants", "Artisans, commerçants et chefs d’entreprise", "Cadres et professions intellectuelles supérieures", "Professions intermédiaires", "Employés", "Ouvriers", "Sans profession et autres professions"))

#levels(tab_RNE_IRC_finale$lib_PCS_corr_21)  
#levels(tab_RNE_IRC_finale$lib_PCS_corr_25)
```


```{r, echo=TRUE, message=FALSE, warning=FALSE}
#Création de la dummy Maire 
tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(Maire_14 = ifelse(fonction_tri_14 == 1, 1, 0),
         Maire_20 = ifelse(fonction_tri_20 == 1, 1, 0),
         Maire_21 = ifelse(fonction_tri_21 == 1, 1, 0),
         Maire_25 = ifelse(fonction_tri_25 == 1, 1, 0)
         )

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>%
  mutate(Maire_14 = case_when(
    is.na(Maire_14) ~ 0, 
    TRUE ~ Maire_14),
    
    Maire_20 = case_when(
    is.na(Maire_20) ~ 0, 
    TRUE ~ Maire_20),
    
    Maire_21 = case_when(
    is.na(Maire_21) ~ 0, 
    TRUE ~ Maire_21),
    
    Maire_25 = case_when(
    is.na(Maire_25) ~ 0, 
    TRUE ~ Maire_25),
    )

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(tr_age_fin_moins_45_indic = case_when(
    tr_age_fin_20_26 == 1 | tr_age_fin_20_26 == 2 | tr_age_fin_20_26 == 3 | tr_age_fin_20_26 == 4 | tr_age_fin_20_26 == 5 ~ 1, 
    
    TRUE ~ 0))

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(tr_age_fin_46_55_indic = case_when(
    tr_age_fin_20_26 == 6 | tr_age_fin_20_26 == 7 ~ 1, 
    
    TRUE ~ 0))

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(tr_age_fin_56_65_indic = case_when(
    tr_age_fin_20_26 == 8 | tr_age_fin_20_26 == 9 ~ 1, 
    
    TRUE ~ 0))

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(tr_age_fin_66_75_indic = case_when(
    tr_age_fin_20_26 == 10 | tr_age_fin_20_26 == 11 ~ 1, 
    
    TRUE ~ 0))


tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(tr_age_fin_plus_76_indic = case_when(
    tr_age_fin_20_26 == 12 ~ 1, 
    
    TRUE ~ 0))

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(entrant_20_26 = case_when(
    tab_RNE_2021_b_indic == 0 & tab_RNE_2025_b_indic == 1 ~ 1, 
    
    TRUE ~ 0))

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(sortant_20_26 = case_when(
    tab_RNE_2021_b_indic == 1 & tab_RNE_2025_b_indic == 0 ~ 1, 
    
    TRUE ~ 0))

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(restant_20_26 = case_when(
    tab_RNE_2021_b_indic == 1 & tab_RNE_2025_b_indic == 1 ~ 1, 
    
    TRUE ~ 0))

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(PMUN_2022_div_1000 = PMUN_2022/1000)
```


```{r, echo=TRUE, message=FALSE, warning=FALSE}

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(tr_age_deb_moins_45_indic = case_when(
    tr_age_deb_20_26 == 1 | tr_age_deb_20_26 == 2 | tr_age_deb_20_26 == 3 | tr_age_deb_20_26 == 4 | tr_age_deb_20_26 == 5 ~ 1, 
    
    TRUE ~ 0))

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(tr_age_deb_46_55_indic = case_when(
    tr_age_deb_20_26 == 6 | tr_age_deb_20_26 == 7 ~ 1, 
    
    TRUE ~ 0))

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(tr_age_deb_56_65_indic = case_when(
    tr_age_deb_20_26 == 8 | tr_age_deb_20_26 == 9 ~ 1, 
    
    TRUE ~ 0))

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(tr_age_deb_66_75_indic = case_when(
    tr_age_deb_20_26 == 10 | tr_age_deb_20_26 == 11 ~ 1, 
    
    TRUE ~ 0))


tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(tr_age_deb_plus_76_indic = case_when(
    tr_age_deb_20_26 == 12 ~ 1, 
    
    TRUE ~ 0))

#Variation fonction facteur
tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(fact_fonction = case_when(
   Maire_21 == 1 ~ "Maire",
   adjoint_21 == 1 ~ "Adjoint",
   cm_indem_21 == 1 ~ "CM indem", 
   cm_non_indem_21 == 1 ~ "CM non indem",
   TRUE ~ NA))

tab_RNE_IRC_finale$fact_fonction <- as.factor(tab_RNE_IRC_finale$fact_fonction)

tab_RNE_IRC_finale$fact_fonction = factor(tab_RNE_IRC_finale$fact_fonction, levels=c("Maire", "Adjoint","CM indem", "CM non indem"))


tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(fact_fonction_25 = case_when(
   Maire_25 == 1 ~ "Maire",
   adjoint_25 == 1 ~ "Adjoint",
   cm_indem_25 == 1 ~ "CM indem", 
   cm_non_indem_25 == 1 ~ "CM non indem",
   TRUE ~ NA))

tab_RNE_IRC_finale$fact_fonction_25 <- as.factor(tab_RNE_IRC_finale$fact_fonction_25)

tab_RNE_IRC_finale$fact_fonction_25 = factor(tab_RNE_IRC_finale$fact_fonction_25, levels=c("Maire", "Adjoint","CM indem", "CM non indem"))
```


## 4.10 Var promotion 
```{r, echo=TRUE, message=FALSE, warning=FALSE}
tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>%
  mutate(evolution_20_26 = case_when(
    adjoint_21 == 1 & Maire_25 == 1 |
      cm_indem_21 == 1 & Maire_25 == 1 |
      cm_non_indem_21 & Maire_25 == 1 | 
      
      cm_indem_21 == 1 & adjoint_25 ==1 | 
      cm_non_indem_21 == 1 & adjoint_25 == 1 | 
      
      cm_non_indem_21 ==1 & cm_indem_25 == 1 ~ 1, 
    
    TRUE ~ 0
  ))
#21 192 indiv qui évoluent entre 2021 et 2025

```

## 4.1 Pari score
```{r, echo=TRUE, message=FALSE, warning=FALSE}
pariscore_muni_2020 <- read_csv("/dz-rstudio/applis/dl3/ext/data/retraites/Z_BAC_A_SABLE/ValentinF/data/pariscore-muni-2020.csv")

pariscore_muni_2020 <- pariscore_muni_2020 %>% 
  select(code_circo, population_totale, population_consideree, taille_liste, type_commune, mediane_niveau_vie, part_bac5, pariscore, raison)

#freq(pariscore_muni_2020$pariscore)

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  left_join(pariscore_muni_2020, by = c("code_comm_insee_20" = "code_circo"))

#Camambert
camembert <- table(tab_RNE_IRC_finale$type_commune)

#Afficher les pourcentages 
labels <- paste0(names(camembert), " (", round(100*camembert / sum(camembert), 1), "%)")

pie(camembert, labels = labels)


#Camambert
camembert <- table(tab_RNE_IRC_finale$pariscore)

#Afficher les pourcentages 
labels <- paste0(names(camembert), " (", round(100*camembert / sum(camembert), 1), "%)")

pie(camembert, labels = labels)

#Reclassement de la variable raison
tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(raison_corr = case_when(
    raison == "Les classes supérieures s'imposent structurellement dans la représentation locale, au détriment visible des classes populaires." |
    raison == "Domination simultanée des classes supérieures sur les classes moyennes et populaires. Déficit démocratique majeur." | 
    raison == "Les classes supérieures écrasent les classes populaires de façon nette. Déséquilibre structurel grave."| 
    raison == "Situation de domination structurelle extrême. Les classes supérieures monopolisent quasi totalement le pouvoir politique." ~ "supérieur",
    
    
    
    raison == "Situation dite de \"bastion populaire\" : les classes populaires sont clairement sur-représentées et les classes supérieures décrochent nettement en dessous." | 
    raison == "Situation dite de \"bastion populaire\" : les classes populaires devancent structurellement les deux autres classes." ~ "inférieur", 
    
    
    
    raison == "Configuration atypique : les classes moyennes sont structurellement absentes et une coalition entre classes supérieures et populaires se dessine, où les premières dominent." |
    raison == "Configuration atypique mais acceptable : les classes moyennes sont structurellement absentes mais les classes populaires restent premières, en coalition avec les classes supérieures." | 
    raison == "Situation \"acceptable\" : les classes supérieures ont un léger avantage sans domination franche. Les classes populaires restent dans une position acceptable. Cette situation semble ressembler aux recommandations du rapport de dlp." | 
    raison == "Situation d'équilibre entre classes populaires et moyennes, proche de la parité sociale au sens stricte. Dans ce cas, les classes supérieures sont en retrait." | 
    raison == "Situation de parité sociale au sens de la \"représentation-miroir\" : les trois classes sont à l'équilibre les unes par rapport aux autres. Aucun groupe ne domine structurellement." | 
    raison == "Situation intermédiaire : les classes supérieures commencent à capter le pouvoir par rapport aux autres classes sociales." | 
    raison == "Situation intermédiaire : les classes supérieures sont présentes mais leur domination reste insuffisante pour qualifier une capture confirmée, ou elles dominent surtout les classes moyennes." | 
    raison == "Situation intermédiaire : pas de capture du pouvoir politique par les classes supérieures, mais les classes moyennes verrouillent l'accès des classes populaires à la représentation. Situation ambiguë politiquement." | 
    raison == "Situation intermédiaire : une dynamique de capture par les classes supérieures existe, mais les classes populaires maintiennent une présence suffisante. La capture reste trop faible pour être qualifiée de structurelle." |
    raison == "Situation intermédiaire sans signal fort. Les rapports entre classes ne dessinent pas de domination structurelle." ~ "equilibré",
    
    TRUE ~ NA))

#freq(tab_RNE_IRC_finale$raison_corr)

#Camambert
camembert <- table(tab_RNE_IRC_finale$raison_corr)

#Afficher les pourcentages 
labels <- paste0(names(camembert), " (", round(100*camembert / sum(camembert), 1), "%)")

pie(camembert, labels = labels)

tab_RNE_IRC_finale$raison_corr <- as.factor(tab_RNE_IRC_finale$raison_corr)

tab_RNE_IRC_finale$raison_corr = factor(tab_RNE_IRC_finale$raison_corr, levels=c("supérieur", "intermediaire", "inférieur"))

#levels(tab_RNE_IRC_finale$raison_corr)

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  arrange(code_comm_insee_20)

tab_Pcomm <- tab_RNE_IRC_finale %>% 
  select(code_comm_insee_20, mediane_niveau_vie) %>% 
  group_by(code_comm_insee_20) %>% 
  distinct()
  

#Niv de vie 
ggplot(tab_Pcomm, 
       aes(x = mediane_niveau_vie)) + 
  geom_histogram(bins = 1000) +
  labs(
    title = "Distribution du niveau de vie médian par commune française en 2020",
    x = "niveau de vie médian en euro", 
    y = "Nombre de communes"
  ) +
  theme_minimal()

#Création tranche de niveau de vie médian
test <- tab_RNE_IRC_finale %>% 
  filter(mediane_niveau_vie <= 10000)#Aucune commune à moins de 10 000 de revenu médian

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(tr_niv_vie_median = case_when(
      
      mediane_niveau_vie <= 15000  ~ "moins_15000",
      
      mediane_niveau_vie > 15000 & mediane_niveau_vie <= 20000  ~ "15000_20000", 
      
      mediane_niveau_vie > 20000 & mediane_niveau_vie <= 25000  ~ "20000_25000", 
      
      mediane_niveau_vie > 25000 & mediane_niveau_vie <= 30000  ~ "25000_30000",
      
      mediane_niveau_vie > 30000   ~ "plus_30000",
      
      TRUE ~ NA
    )
  )

tab_RNE_IRC_finale$tr_niv_vie_median <- as.factor(tab_RNE_IRC_finale$tr_niv_vie_median)

tab_RNE_IRC_finale$tr_niv_vie_median = factor(tab_RNE_IRC_finale$tr_niv_vie_median, levels=c("plus_30000", "25000_30000", "20000_25000", "15000_20000","moins_15000"))

#levels(tab_RNE_IRC_finale$tr_niv_vie_median)

#Camambert
camembert <- table(tab_RNE_IRC_finale$tr_niv_vie_median)

#Afficher les pourcentages 
labels <- paste0(names(camembert), " (", round(100*camembert / sum(camembert), 1), "%)")

pie(camembert, labels = labels)


#Mise au format facteur letype de commune 
#freq(pariscore_muni_2020$type_commune)

tab_RNE_IRC_finale$type_commune <- as.factor(tab_RNE_IRC_finale$type_commune)

#Part des bac + 5

tab_bac5 <- tab_RNE_IRC_finale %>% 
  select(code_comm_insee_20, part_bac5) %>% 
  group_by(code_comm_insee_20) %>% 
  distinct()
  

#Niv de vie 
ggplot(tab_bac5, 
       aes(x = part_bac5)) + 
  geom_histogram(bins = 1000) +
  labs(
    title = "Distribution du niveau de vie médian par commune française en 2020",
    x = "niveau de vie médian en euro", 
    y = "Nombre de communes"
  ) +
  theme_minimal()

#Création tranche de niveau de vie médian
test <- tab_RNE_IRC_finale %>% 
  filter(part_bac5 < 2.5)#Aucune commune à moins de 10 000 de revenu médian

tab_RNE_IRC_finale <- tab_RNE_IRC_finale %>% 
  mutate(tr_bac5 = case_when(
      
      part_bac5 <= 5  ~ "moins_5",
      
      part_bac5 > 5 & part_bac5 <= 10  ~ "5_10", 
      
      part_bac5 > 10 & part_bac5 <= 15  ~ "10_15", 
      
      part_bac5 > 15 & part_bac5 <= 20  ~ "15_20",
      
      part_bac5 > 20  ~ "plus_20",
      
      TRUE ~ NA
    )
  )


tab_RNE_IRC_finale$tr_bac5 <- as.factor(tab_RNE_IRC_finale$tr_bac5)

tab_RNE_IRC_finale$tr_bac5 = factor(tab_RNE_IRC_finale$tr_bac5, levels=c("plus_20", "15_20", "10_15", "5_10","moins_5"))

#levels(tab_RNE_IRC_finale$tr_bac5)

#Camambert
camembert <- table(tab_RNE_IRC_finale$tr_bac5)

#Afficher les pourcentages 
labels <- paste0(names(camembert), " (", round(100*camembert / sum(camembert), 1), "%)")

pie(camembert, labels = labels)

#tr_bac5, type_commune, tr_niv_vie_median, raison_corr
```



```{r, echo=TRUE, message=FALSE, warning=FALSE}
##-------------------------------------------------------------------------------------------------------------------------------
tab_RNE_IRC_finale_2 <- tab_RNE_IRC_finale %>% 
  filter(ID_commun_avecDep != "NA - NA - NA - NA - NA - NA - NA")

saveRDS(tab_RNE_IRC_finale_2, file = "Working_directorytab_RNE_IRC_finale_2.rds")
```




