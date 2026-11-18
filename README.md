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


# 3 Merge 

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
Confidentiel
```

## 3.2 Merge 2

__Merge 2__ est le merge qui permet de passer outre les problèmes de prénom enregistrés différement entre RNE et en IRC. 
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
#test <- tab_RNE_IRC_finale %>% 
#  filter(age_deb_14_20 > age_deb_20_26)
#
#
#test <- tab_RNE_IRC_finale %>% 
#  filter(date_naissance_14 != date_naissance_20)
##-------------------------------------------------------------------------------------------------------------------------------
```




