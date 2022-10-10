# author: "Jonathan Richir"
# date: "01 October 2022"


#Rscript

###############################
##  ##
###############################

#####Packages : dplyr
#               tidyr
#               readr
#               writexl
#               stringr
#               readxl
#               tibble
#               lubridate
#               cowplot
#               magrittr
#               rmarkdown
library(magrittr)
#####Load arguments

args <- commandArgs(trailingOnly = TRUE)

#####Import data

if (length(args) < 1) {
    stop("This tool needs at least 1 argument")
}else {
    ecology_input <- args[1]

}


## load qecbNew data ; df saved from CB_qecb script

qecb <- readRDS(ecology_input)

`%notin%` <- Negate(`%in%`)

## reorder and/or create new variables

# variable site_year_month_day moved for clarity purpose, not needed necessarily
qecb <- tibble::add_column(qecb, qecb$site_year_month_day, .after = "Site_bis")
qecb <- qecb[, -which(names(qecb) %in% c("site_year_month_day"))]
qecb <- dplyr::rename(qecb, site_year_month_day = `qecb$site_year_month_day`)

# new variable period (nothing to see with the existing périod variable)
period <- rep(NA, nrow(qecb))
qecb <- tibble::add_column(qecb, period, .after = "Day")
qecb$period <- ifelse(as_numeric(qecb$Month) < 7, "p1", "p2")
qecb$period <- as_factor(qecb$period)
rm(period)

qecb <- dplyr::arrange(qecb, region, site_year_month_day, Type_Bloc, Numéro_Bloc_échantillon, Face)

# NB: les infos surface d'accolement sont dupliquées de la face inf vers la face sup de blocs mobiles (même si peu de sens d'avoir cette info pour les face sup ...)
# NB: les data "Abondance ressources ciblées par pêcheurs à pied" présentes uniquement pour les blocs mobiles sont dupliquées entre face inf et face sup_


## SCRIPT I - NAs <- 0

# already performed for part in the CB_qecb script ; but here I also consider mobile organisms, logical observation (or not) according to boulders, faces etc ... so more complete_ Could be some kind of script fusion to only keep Na to 0 correction in one script, i_e_ moving this script to the CB_qecb script ...

#unique(qecb$region)
#unique(qecb$Type_Bloc)
bretagne_bm <- dplyr::filter(qecb, region == "Bretagne" & Type_Bloc == "Bloc mobile")
bretagne_bf <- dplyr::filter(qecb, region == "Bretagne" & Type_Bloc %in% c("Bloc fixé", "Roche en place"))
egmp_basq_bm <- dplyr::filter(qecb, region == "EGMp_BASQ" & Type_Bloc == "Bloc mobile")
egmp_basq_bf <- dplyr::filter(qecb, region == "EGMp_BASQ" & Type_Bloc %in% c("Bloc fixé", "Roche en place"))

# replace NAs by "0" for variables used in qecb determination

{

  # bretagne_bm
  bretagne_bm[, c(
    "X..algues_brunes",
    "Strate_algues_brunes",
    "X..algues_rouges",
    "Strate_algues_rouges",
    "X..algues_vertes",
    "Strate_algues_vertes",
    "X..Cladophora",
    "X..Lithophyllum",
    "X..Recouvrement.Sediment", # is NA, then replace by 0 as well because no sense to have a NA value for "% recouvrement sédiment" as well_
    #"Type_Sediment",
    "X..Roche_Nue", # is NA, then replace by 0 as well because no sense to have a NA value for "% roche nue" as well_
    "Nb_Littorina_obtusata",
    "Nb_Gibbula_cineraria",
    "Nb_Gibbula_pennanti",
    "Nb_Gibbula_umbilicalis",
    "Nb_Phallusia_mamillata",
    "Nb_Tethya_aurantium",
    #"Nb_Spirobranchus_lamarckii_1B",
    #"Nb_Spirobranchus_lamarckii_2B",
    #"Nb_Spirobranchus_lamarckii_3B",
    #"Nb_Spirobranchus_lamarckii_4B",
    #"Nb_Spirobranchus_lamarckii_5B",
    "Nb_Spirobranchus_lamarckii_total",
    #"Nb_spirorbis_1A",
    #"Nb_spirorbis_2A",
    #"Nb_spirorbis_3A",
    #"Nb_spirorbis_4A",
    #"Nb_spirorbis_5A",
    "Nb_spirorbis_total",
    #.."Nb_Crassostrea_gigas",
    #.."Nb_Ostrea_edulis",
    #.."X..Mytilus_sp_",
    #.."X..Hermelles",
    #.."X..Hydraires",
    "X..Eponges",
    "X..Ascidies_Coloniales",
    "X..Ascidies_Solitaires",
    "X..Bryozoaires_Dresses",
    "X..Balanes_Vivantes",
    #"Commentaires_Avant",
    "X..Surface_Accolement", # is NA, then replace by 0 as well because no sense to have a NA value for "% surface accolement" as well_
    #"Type_sustrat.observé",
    #"Commentaires",
    "Nb_Cancer_pagurus_.Tourteau.",
    "Nb_Necora_puber_.Etrille_",
    "Nb_Carcinus_maenas_.Crabe_vert.",
    "Nb_Nucella_lapilus_.Pourpre_",
    #.."Nb_Eriphia_verrucosa_.Crabe_verruqueux.",
    #.."Nb_Octopus_vulgaris_.Poulpe_",
    "Nb_Galathea_.Galathées_",
    #.."Nb_Paracentrotus_lividus_.Oursin_",
    "Nb_Lophozozymus_incisus_.ancien_Xantho_incisus_",
    "Nb_Palaemon_sp_.Crevette_bouquet.ou.crevette_rose_",
    "Nb_Haliotis_tuberculata_.Ormeau.",
    #"Nb_Stramonita_haemastoma_.Pourpre_bouche_de_sang_",
    "Nb_Littorina_littorea_.Bigorneau.",
    "Nb_Xantho_pilipes_.Xanthe_poilu.",
    "Nb_Mimachlamys_varia_.Pétoncle_noir_"
  )
  ] <- lapply(bretagne_bm[, c(
    "X..algues_brunes",
    "Strate_algues_brunes",
    "X..algues_rouges",
    "Strate_algues_rouges",
    "X..algues_vertes",
    "Strate_algues_vertes",
    "X..Cladophora",
    "X..Lithophyllum",
    "X..Recouvrement.Sediment", # is NA, then replace by 0 as well because no sense to have a NA value for "% recouvrement sédiment" as well_
    #"Type_Sediment",
    "X..Roche_Nue", # is NA, then replace by 0 as well because no sense to have a NA value for "% roche nue" as well_
    "Nb_Littorina_obtusata",
    "Nb_Gibbula_cineraria",
    "Nb_Gibbula_pennanti",
    "Nb_Gibbula_umbilicalis",
    "Nb_Phallusia_mamillata",
    "Nb_Tethya_aurantium",
    #"Nb_Spirobranchus_lamarckii_1B",
    #"Nb_Spirobranchus_lamarckii_2B",
    #"Nb_Spirobranchus_lamarckii_3B",
    #"Nb_Spirobranchus_lamarckii_4B",
    #"Nb_Spirobranchus_lamarckii_5B",
    "Nb_Spirobranchus_lamarckii_total",
    #"Nb_spirorbis_1A",
    #"Nb_spirorbis_2A",
    #"Nb_spirorbis_3A",
    #"Nb_spirorbis_4A",
    #"Nb_spirorbis_5A",
    "Nb_spirorbis_total",
    #.."Nb_Crassostrea_gigas",
    #.."Nb_Ostrea_edulis",
    #.."X..Mytilus_sp_",
    #.."X..Hermelles",
    #.."X..Hydraires",
    "X..Eponges",
    "X..Ascidies_Coloniales",
    "X..Ascidies_Solitaires",
    "X..Bryozoaires_Dresses",
    "X..Balanes_Vivantes",
    #"Commentaires_Avant",
    "X..Surface_Accolement", # is NA, then replace by 0 as well because no sense to have a NA value for "% surface accolement" as well_
    #"Type_sustrat.observé",
    #"Commentaires",
    "Nb_Cancer_pagurus_.Tourteau.",
    "Nb_Necora_puber_.Etrille_",
    "Nb_Carcinus_maenas_.Crabe_vert.",
    "Nb_Nucella_lapilus_.Pourpre_",
    #.."Nb_Eriphia_verrucosa_.Crabe_verruqueux.",
    #.."Nb_Octopus_vulgaris_.Poulpe_",
    "Nb_Galathea_.Galathées_",
    #.."Nb_Paracentrotus_lividus_.Oursin_",
    "Nb_Lophozozymus_incisus_.ancien_Xantho_incisus_",
    "Nb_Palaemon_sp_.Crevette_bouquet.ou.crevette_rose_",
    "Nb_Haliotis_tuberculata_.Ormeau.",
    #"Nb_Stramonita_haemastoma_.Pourpre_bouche_de_sang_",
    "Nb_Littorina_littorea_.Bigorneau.",
    "Nb_Xantho_pilipes_.Xanthe_poilu.",
    "Nb_Mimachlamys_varia_.Pétoncle_noir_"
  )
  ], function(x) replace(x, is_na(x), 0))
  
  
  # bretagne_bf
  bretagne_bf[,c(
    "X..algues_brunes"                       ,            
    "Strate_algues_brunes"                   ,          
    "X..algues_rouges"                       ,           
    "Strate_algues_rouges"                   ,           
    "X..algues_vertes"                       ,            
    "Strate_algues_vertes"                   ,            
    "X..Cladophora"                          ,           
    "X..Lithophyllum"                        ,           
    "X..Recouvrement.Sediment"               , # is NA, then replace by 0 as well because no sense to have a NA value for "% recouvrement sédiment" as well_          
    #"Type_Sediment"                          ,           
    "X..Roche_Nue"                           , # is NA, then replace by 0 as well because no sense to have a NA value for "% roche nue" as well_          
    "Nb_Littorina_obtusata"                  ,           
    "Nb_Gibbula_cineraria"                   ,           
    "Nb_Gibbula_pennanti"                    ,           
    "Nb_Gibbula_umbilicalis"                 ,           
    "Nb_Phallusia_mamillata"                 ,           
    "Nb_Tethya_aurantium"                    ,           
    #"Nb_Spirobranchus_lamarckii_1B"          ,           
    #"Nb_Spirobranchus_lamarckii_2B"          ,           
    #"Nb_Spirobranchus_lamarckii_3B"          ,           
    #"Nb_Spirobranchus_lamarckii_4B"          ,           
    #"Nb_Spirobranchus_lamarckii_5B"          ,           
    "Nb_Spirobranchus_lamarckii_total"       ,           
    #"Nb_spirorbis_1A"                        ,           
    #"Nb_spirorbis_2A"                        ,           
    #"Nb_spirorbis_3A"                        ,           
    #"Nb_spirorbis_4A"                        ,           
    #"Nb_spirorbis_5A"                        ,           
    "Nb_spirorbis_total"                     ,           
    #.."Nb_Crassostrea_gigas"                   ,           
    #.."Nb_Ostrea_edulis"                       ,           
    #.."X..Mytilus_sp_"                         ,           
    #.."X..Hermelles"                           ,           
    #.."X..Hydraires"                           ,           
    "X..Eponges"                             ,           
    "X..Ascidies_Coloniales"                 ,           
    "X..Ascidies_Solitaires"                 ,           
    "X..Bryozoaires_Dresses"                 ,           
    "X..Balanes_Vivantes"                    ,           
    #"Commentaires_Avant"                     ,            
    "X..Surface_Accolement"                  #, # is NA, then replace by 0 as well because no sense to have a NA value for "% surface accolement" as well_              
    #"Type_sustrat.observé"                   ,           
    #"Commentaires"                           ,            
    #."Nb_Cancer_pagurus_.Tourteau."           ,           
    #.."Nb_Necora_puber_.Etrille_"              ,           
    #."Nb_Carcinus_maenas_.Crabe_vert."        ,           
    #."Nb_Nucella_lapilus_.Pourpre_"           ,           
    #.."Nb_Eriphia_verrucosa_.Crabe_verruqueux.",           
    #.."Nb_Octopus_vulgaris_.Poulpe_"           ,           
    #."Nb_Galathea_.Galathées_"                ,          
    #.."Nb_Paracentrotus_lividus_.Oursin_"      ,            
    #."Nb_Lophozozymus_incisus_.ancien_Xantho_incisus_"         ,
    #."Nb_Palaemon_sp_.Crevette_bouquet.ou.crevette_rose_"      ,
    #."Nb_Haliotis_tuberculata_.Ormeau."                        ,
    #."Nb_Stramonita_haemastoma_.Pourpre_bouche_de_sang_"       ,
    #."Nb_Littorina_littorea_.Bigorneau."      ,           
    #."Nb_Xantho_pilipes_.Xanthe_poilu."       ,           
    #."Nb_Mimachlamys_varia_.Pétoncle_noir_"
  )
  ] <- lapply(bretagne_bf[, c(
    "X..algues_brunes",
    "Strate_algues_brunes",
    "X..algues_rouges",
    "Strate_algues_rouges",
    "X..algues_vertes",
    "Strate_algues_vertes",
    "X..Cladophora",
    "X..Lithophyllum",
    "X..Recouvrement.Sediment", # is NA, then replace by 0 as well because no sense to have a NA value for "% recouvrement sédiment" as well_
    #"Type_Sediment",
    "X..Roche_Nue", # is NA, then replace by 0 as well because no sense to have a NA value for "% roche nue" as well_
    "Nb_Littorina_obtusata",
    "Nb_Gibbula_cineraria",
    "Nb_Gibbula_pennanti",
    "Nb_Gibbula_umbilicalis",
    "Nb_Phallusia_mamillata",
    "Nb_Tethya_aurantium",
    #"Nb_Spirobranchus_lamarckii_1B",
    #"Nb_Spirobranchus_lamarckii_2B",
    #"Nb_Spirobranchus_lamarckii_3B",
    #"Nb_Spirobranchus_lamarckii_4B",
    #"Nb_Spirobranchus_lamarckii_5B",
    "Nb_Spirobranchus_lamarckii_total",
    #"Nb_spirorbis_1A",
    #"Nb_spirorbis_2A",
    #"Nb_spirorbis_3A",
    #"Nb_spirorbis_4A",
    #"Nb_spirorbis_5A",
    "Nb_spirorbis_total",
    #.."Nb_Crassostrea_gigas",
    #.."Nb_Ostrea_edulis",
    #.."X..Mytilus_sp_",
    #.."X..Hermelles",
    #.."X..Hydraires",
    "X..Eponges",
    "X..Ascidies_Coloniales",
    "X..Ascidies_Solitaires",
    "X..Bryozoaires_Dresses",
    "X..Balanes_Vivantes",
    #"Commentaires_Avant",
    "X..Surface_Accolement"#, # is NA, then replace by 0 as well because no sense to have a NA value for "% surface accolement" as well_
    #"Type_sustrat.observé",
    #"Commentaires",
    #."Nb_Cancer_pagurus_.Tourteau.",
    #.."Nb_Necora_puber_.Etrille_",
    #."Nb_Carcinus_maenas_.Crabe_vert.",
    #."Nb_Nucella_lapilus_.Pourpre_",
    #.."Nb_Eriphia_verrucosa_.Crabe_verruqueux.",
    #.."Nb_Octopus_vulgaris_.Poulpe_",
    #."Nb_Galathea_.Galathées_",
    #.."Nb_Paracentrotus_lividus_.Oursin_",
    #."Nb_Lophozozymus_incisus_.ancien_Xantho_incisus_",
    #."Nb_Palaemon_sp_.Crevette_bouquet.ou.crevette_rose_",
    #."Nb_Haliotis_tuberculata_.Ormeau.",
    #."Nb_Stramonita_haemastoma_.Pourpre_bouche_de_sang_",
    #."Nb_Littorina_littorea_.Bigorneau.",
    #."Nb_Xantho_pilipes_.Xanthe_poilu.",
    #."Nb_Mimachlamys_varia_.Pétoncle_noir_"
  )
  ], function(x) replace(x, is_na(x), 0))
  
  
  # egmp_basq_bm
  egmp_basq_bm[, c(
    "X..algues_brunes",
    "Strate_algues_brunes",
    "X..algues_rouges",
    "Strate_algues_rouges",
    "X..algues_vertes",
    "Strate_algues_vertes",
    "X..Cladophora",
    "X..Lithophyllum",
    "X..Recouvrement.Sediment", # is NA, then replace by 0 as well because no sense to have a NA value for "% recouvrement sédiment" as well_
    #"Type_Sediment",
    "X..Roche_Nue", # is NA, then replace by 0 as well because no sense to have a NA value for "% roche nue" as well_
    "Nb_Littorina_obtusata",
    "Nb_Gibbula_cineraria",
    "Nb_Gibbula_pennanti",
    "Nb_Gibbula_umbilicalis",
    "Nb_Phallusia_mamillata",
    "Nb_Tethya_aurantium",
    #"Nb_Spirobranchus_lamarckii_1B",
    #"Nb_Spirobranchus_lamarckii_2B",
    #"Nb_Spirobranchus_lamarckii_3B",
    #"Nb_Spirobranchus_lamarckii_4B",
    #"Nb_Spirobranchus_lamarckii_5B",
    "Nb_Spirobranchus_lamarckii_total",
    #"Nb_spirorbis_1A",
    #"Nb_spirorbis_2A",
    #"Nb_spirorbis_3A",
    #"Nb_spirorbis_4A",
    #"Nb_spirorbis_5A",
    "Nb_spirorbis_total",
    "Nb_Crassostrea_gigas",
    "Nb_Ostrea_edulis",
    "X..Mytilus_sp_",
    "X..Hermelles",
    "X..Hydraires",
    "X..Eponges",
    "X..Ascidies_Coloniales",
    "X..Ascidies_Solitaires",
    "X..Bryozoaires_Dresses",
    "X..Balanes_Vivantes",
    #"Commentaires_Avant",
    "X..Surface_Accolement", # is NA, then replace by 0 as well because no sense to have a NA value for "% surface accolement" as well_
    #"Type_sustrat.observé",
    #"Commentaires",
    "Nb_Cancer_pagurus_.Tourteau.",
    "Nb_Necora_puber_.Etrille_",
    "Nb_Carcinus_maenas_.Crabe_vert.",
    "Nb_Nucella_lapilus_.Pourpre_",
    "Nb_Eriphia_verrucosa_.Crabe_verruqueux.",
    "Nb_Octopus_vulgaris_.Poulpe_",
    "Nb_Galathea_.Galathées_",
    "Nb_Paracentrotus_lividus_.Oursin_",
    "Nb_Lophozozymus_incisus_.ancien_Xantho_incisus_",
    "Nb_Palaemon_sp_.Crevette_bouquet.ou.crevette_rose_",
    "Nb_Haliotis_tuberculata_.Ormeau.",
    "Nb_Stramonita_haemastoma_.Pourpre_bouche_de_sang_",
    "Nb_Littorina_littorea_.Bigorneau.",
    "Nb_Xantho_pilipes_.Xanthe_poilu.",
    "Nb_Mimachlamys_varia_.Pétoncle_noir_"
  )
  ] <- lapply(egmp_basq_bm[, c(
    "X..algues_brunes",
    "Strate_algues_brunes",
    "X..algues_rouges",
    "Strate_algues_rouges",
    "X..algues_vertes",
    "Strate_algues_vertes",
    "X..Cladophora",
    "X..Lithophyllum",
    "X..Recouvrement.Sediment", # is NA, then replace by 0 as well because no sense to have a NA value for "% recouvrement sédiment" as well_
    #"Type_Sediment",
    "X..Roche_Nue", # is NA, then replace by 0 as well because no sense to have a NA value for "% roche nue" as well_
    "Nb_Littorina_obtusata",
    "Nb_Gibbula_cineraria",
    "Nb_Gibbula_pennanti",
    "Nb_Gibbula_umbilicalis",
    "Nb_Phallusia_mamillata",
    "Nb_Tethya_aurantium",
    #"Nb_Spirobranchus_lamarckii_1B",
    #"Nb_Spirobranchus_lamarckii_2B",
    #"Nb_Spirobranchus_lamarckii_3B",
    #"Nb_Spirobranchus_lamarckii_4B",
    #"Nb_Spirobranchus_lamarckii_5B",
    "Nb_Spirobranchus_lamarckii_total",
    #"Nb_spirorbis_1A",
    #"Nb_spirorbis_2A",           
    #"Nb_spirorbis_3A",
    #"Nb_spirorbis_4A",
    #"Nb_spirorbis_5A",
    "Nb_spirorbis_total",
    "Nb_Crassostrea_gigas",
    "Nb_Ostrea_edulis",
    "X..Mytilus_sp_",
    "X..Hermelles",
    "X..Hydraires",
    "X..Eponges",
    "X..Ascidies_Coloniales",
    "X..Ascidies_Solitaires",
    "X..Bryozoaires_Dresses",
    "X..Balanes_Vivantes",
    #"Commentaires_Avant",
    "X..Surface_Accolement", # is NA, then replace by 0 as well because no sense to have a NA value for "% surface accolement" as well_
    #"Type_sustrat.observé",
    #"Commentaires",
    "Nb_Cancer_pagurus_.Tourteau.",
    "Nb_Necora_puber_.Etrille_",
    "Nb_Carcinus_maenas_.Crabe_vert.",
    "Nb_Nucella_lapilus_.Pourpre_",
    "Nb_Eriphia_verrucosa_.Crabe_verruqueux.",
    "Nb_Octopus_vulgaris_.Poulpe_",
    "Nb_Galathea_.Galathées_",
    "Nb_Paracentrotus_lividus_.Oursin_",
    "Nb_Lophozozymus_incisus_.ancien_Xantho_incisus_",
    "Nb_Palaemon_sp_.Crevette_bouquet.ou.crevette_rose_",
    "Nb_Haliotis_tuberculata_.Ormeau.",
    "Nb_Stramonita_haemastoma_.Pourpre_bouche_de_sang_",
    "Nb_Littorina_littorea_.Bigorneau.",
    "Nb_Xantho_pilipes_.Xanthe_poilu.",
    "Nb_Mimachlamys_varia_.Pétoncle_noir_"
  )
  ], function(x) replace(x, is_na(x), 0))
  
  
  # egmp_basq_bf
  egmp_basq_bf[, c(
    "X..algues_brunes",
    "Strate_algues_brunes",
    "X..algues_rouges",
    "Strate_algues_rouges",
    "X..algues_vertes",
    "Strate_algues_vertes",
    "X..Cladophora",
    "X..Lithophyllum",
    "X..Recouvrement.Sediment", # is NA, then replace by 0 as well because no sense to have a NA value for "% recouvrement sédiment" as well_
    #"Type_Sediment",
    "X..Roche_Nue", # is NA, then replace by 0 as well because no sense to have a NA value for "% roche nue" as well_
    "Nb_Littorina_obtusata",
    "Nb_Gibbula_cineraria",
    "Nb_Gibbula_pennanti",
    "Nb_Gibbula_umbilicalis",
    "Nb_Phallusia_mamillata",
    "Nb_Tethya_aurantium",
    #"Nb_Spirobranchus_lamarckii_1B",
    #"Nb_Spirobranchus_lamarckii_2B",
    #"Nb_Spirobranchus_lamarckii_3B",
    #"Nb_Spirobranchus_lamarckii_4B",
    #"Nb_Spirobranchus_lamarckii_5B",
    "Nb_Spirobranchus_lamarckii_total",
    #"Nb_spirorbis_1A",
    #"Nb_spirorbis_2A",
    #"Nb_spirorbis_3A",
    #"Nb_spirorbis_4A",
    #"Nb_spirorbis_5A",
    "Nb_spirorbis_total",
    "Nb_Crassostrea_gigas",
    "Nb_Ostrea_edulis",
    "X..Mytilus_sp_",
    "X..Hermelles",
    "X..Hydraires",
    "X..Eponges",
    "X..Ascidies_Coloniales",
    "X..Ascidies_Solitaires",
    "X..Bryozoaires_Dresses",
    "X..Balanes_Vivantes",
    #"Commentaires_Avant",
    "X..Surface_Accolement"#, # is NA, then replace by 0 as well because no sense to have a NA value for "% surface accolement" as well_
    #"Type_sustrat.observé",
    #"Commentaires",            
    #."Nb_Cancer_pagurus_.Tourteau.",
    #.."Nb_Necora_puber_.Etrille_",
    #."Nb_Carcinus_maenas_.Crabe_vert.",
    #."Nb_Nucella_lapilus_.Pourpre_",
    #.."Nb_Eriphia_verrucosa_.Crabe_verruqueux.",
    #.."Nb_Octopus_vulgaris_.Poulpe_",
    #."Nb_Galathea_.Galathées_",
    #.."Nb_Paracentrotus_lividus_.Oursin_",
    #."Nb_Lophozozymus_incisus_.ancien_Xantho_incisus_",
    #."Nb_Palaemon_sp_.Crevette_bouquet.ou.crevette_rose_",
    #."Nb_Haliotis_tuberculata_.Ormeau.",
    #."Nb_Stramonita_haemastoma_.Pourpre_bouche_de_sang_",
    #."Nb_Littorina_littorea_.Bigorneau.",
    #."Nb_Xantho_pilipes_.Xanthe_poilu.",
    #."Nb_Mimachlamys_varia_.Pétoncle_noir_"
  )
  ] <- lapply(egmp_basq_bf[, c(
    "X..algues_brunes",
    "Strate_algues_brunes",
    "X..algues_rouges",
    "Strate_algues_rouges",
    "X..algues_vertes",
    "Strate_algues_vertes",
    "X..Cladophora",
    "X..Lithophyllum",
    "X..Recouvrement.Sediment", # is NA, then replace by 0 as well because no sense to have a NA value for "% recouvrement sédiment" as well_
    #"Type_Sediment",
    "X..Roche_Nue", # is NA, then replace by 0 as well because no sense to have a NA value for "% roche nue" as well_
    "Nb_Littorina_obtusata",
    "Nb_Gibbula_cineraria",
    "Nb_Gibbula_pennanti",
    "Nb_Gibbula_umbilicalis",
    "Nb_Phallusia_mamillata",
    "Nb_Tethya_aurantium",
    #"Nb_Spirobranchus_lamarckii_1B",
    #"Nb_Spirobranchus_lamarckii_2B",
    #"Nb_Spirobranchus_lamarckii_3B",
    #"Nb_Spirobranchus_lamarckii_4B",
    #"Nb_Spirobranchus_lamarckii_5B",
    "Nb_Spirobranchus_lamarckii_total",
    #"Nb_spirorbis_1A",
    #"Nb_spirorbis_2A",
    #"Nb_spirorbis_3A",
    #"Nb_spirorbis_4A",
    #"Nb_spirorbis_5A",
    "Nb_spirorbis_total",
    "Nb_Crassostrea_gigas",
    "Nb_Ostrea_edulis",
    "X..Mytilus_sp_",
    "X..Hermelles",
    "X..Hydraires",
    "X..Eponges",
    "X..Ascidies_Coloniales",
    "X..Ascidies_Solitaires",
    "X..Bryozoaires_Dresses",
    "X..Balanes_Vivantes",
    #"Commentaires_Avant",
    "X..Surface_Accolement"#, # is NA, then replace by 0 as well because no sense to have a NA value for "% surface accolement" as well_
    #"Type_sustrat.observé",
    #"Commentaires",
    #."Nb_Cancer_pagurus_.Tourteau.",
    #.."Nb_Necora_puber_.Etrille_",
    #."Nb_Carcinus_maenas_.Crabe_vert.",           
    #."Nb_Nucella_lapilus_.Pourpre_",           
    #.."Nb_Eriphia_verrucosa_.Crabe_verruqueux.",
    #.."Nb_Octopus_vulgaris_.Poulpe_",           
    #."Nb_Galathea_.Galathées_",
    #.."Nb_Paracentrotus_lividus_.Oursin_",
    #."Nb_Lophozozymus_incisus_.ancien_Xantho_incisus_",
    #."Nb_Palaemon_sp_.Crevette_bouquet.ou.crevette_rose_",
    #."Nb_Haliotis_tuberculata_.Ormeau.",
    #."Nb_Stramonita_haemastoma_.Pourpre_bouche_de_sang_",
    #."Nb_Littorina_littorea_.Bigorneau.",
    #."Nb_Xantho_pilipes_.Xanthe_poilu.",
    #."Nb_Mimachlamys_varia_.Pétoncle_noir_"
  )
  ], function(x) replace(x, is_na(x), 0))
  
}

# merge dfs_
qecbnato0 <- dplyr::bind_rows(bretagne_bm, bretagne_bf)
qecbnato0 <- dplyr::bind_rows(qecbnato0, egmp_basq_bm)
qecbnato0 <- dplyr::bind_rows(qecbnato0, egmp_basq_bf)

qecbnato0 <- dplyr::arrange(qecbnato0, region, site_year_month_day, Type_Bloc, Numéro_Bloc_échantillon, Face)

rm(bretagne_bm, bretagne_bf, egmp_basq_bm, egmp_basq_bf)


## analyse matricielle

# NB some variables were dplyr::renamed or created, cfr I originally merged qecb and ivr data in below script to do some correlation analysis_ This is not the case anymore, so no more merging anymore_

qecbnato0 <- tibble::add_column(qecbnato0, region_site_year_month_day = paste0(qecbnato0$region, qecbnato0$site_year_month_day), .before = "region")


numero_quadrat <- stringr::str_sub(qecbnato0$quadrat_bis, start = -1)
qecbnato0 <- tibble::add_column(qecbnato0, numero_quadrat, .after = "quadrat_bis")
rm(numero_quadrat)
qecbnato0$numero_quadrat <- as_integer(qecbnato0$numero_quadrat)




qecbnato0$Year <- as_integer(qecbnato0$Year)
qecbnato0$Month <- as_integer(qecbnato0$Month)
qecbnato0$Day <- as_integer(qecbnato0$Day)

############################################################
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Anna still hasn't corrected for boulder nb in FINS_Quemenes_2020.10.16 data encoding ! removed from the df_
qecbnato0 <- qecbnato0 %>% dplyr::filter(site_year_month_day != "FINS_Quemenes_2020.10.16")
############################################################


# first, create vector (4) for qecb and fishing by region (same as above)

bret_egmp_basq_qecb <- c(
  "X..algues_brunes",
  "X..algues_rouges",
  "X..algues_vertes",
  "X..Cladophora",
  "X..Lithophyllum",
  "Nb_Littorina_obtusata",
  "Nb_Gibbula_cineraria",
  "Nb_Gibbula_pennanti",
  "Nb_Gibbula_umbilicalis",
  "Nb_Phallusia_mamillata",
  "Nb_Tethya_aurantium",
  "Nb_Spirobranchus_lamarckii_total",
  "Nb_spirorbis_total",
  "X..Eponges",
  "X..Ascidies_Coloniales",
  "X..Ascidies_Solitaires",
  "X..Bryozoaires_Dresses",
  "X..Balanes_Vivantes"
  #, "X..Recouvrement.Sediment"
  #, "X..Roche_Nue"
  #, "X..Surface_Accolement"
  )

egmp_basq_qecb <- c("Nb_Crassostrea_gigas", "Nb_Ostrea_edulis", "X..Mytilus_sp_", "X..Hermelles", "X..Hydraires")

bret_egmp_basq_fishing <- c("Nb_Cancer_pagurus_.Tourteau.",
                            "Nb_Necora_puber_.Etrille_",
                            "Nb_Carcinus_maenas_.Crabe_vert.",
                            "Nb_Nucella_lapilus_.Pourpre_",
                            "Nb_Galathea_.Galathées_",
                            "Nb_Lophozozymus_incisus_.ancien_Xantho_incisus_",
                            "Nb_Palaemon_sp_.Crevette_bouquet.ou.crevette_rose_",
                            "Nb_Haliotis_tuberculata_.Ormeau.",
                            "Nb_Littorina_littorea_.Bigorneau.",
                            "Nb_Xantho_pilipes_.Xanthe_poilu.",
                            "Nb_Mimachlamys_varia_.Pétoncle_noir_")

egmp_basq_fishing <- c("Nb_Eriphia_verrucosa_.Crabe_verruqueux.", "Nb_Octopus_vulgaris_.Poulpe_", "Nb_Paracentrotus_lividus_.Oursin_", "Nb_Stramonita_haemastoma_.Pourpre_bouche_de_sang_")

# what to do with spirorbes & Nb_Spirobranchus_lamarckii_total? log10 transformation

qecbnato0 <- tibble::add_column(qecbnato0, log10.Nb_spirorbis_total = log10(qecbnato0$Nb_spirorbis_total + 1), .after = "Nb_spirorbis_total")
qecbnato0 <- tibble::add_column(qecbnato0, log10.Nb_Spirobranchus_lamarckii_total = log10(qecbnato0$Nb_Spirobranchus_lamarckii_total + 1), .after = "Nb_Spirobranchus_lamarckii_total")

# here I can choose to either replace spirorbis and/or spirobranchus by their log10 transformation in bret_egmp_basq_qecb vector
bret_egmp_basq_qecb <- replace(bret_egmp_basq_qecb, bret_egmp_basq_qecb == "Nb_spirorbis_total", "log10.Nb_spirorbis_total")



## determination of coefficient of dissimilarity face supérieure bloc mobile vs roche en place

# loop in a fct

matri_fct_bmf <- function(data, conca) {

  matri_df <- data
  
  for (x in c(1:length(unique(matri_df$site_year_month_day)))) {

    qecbnato0_x <- matri_df %>% dplyr::filter(site_year_month_day == unique(matri_df$site_year_month_day)[[x]])

    rownames(qecbnato0_x) <- paste0(qecbnato0_x$Type_Bloc, "_", qecbnato0_x$Face,  "_", qecbnato0_x$Numéro_Bloc_échantillon, "_", qecbnato0_x$quadrat_bis)


  mtxdis <- vegan::vegdist(
    sqrt(qecbnato0_x[, conca]), #Transform your species abundance data_ Typically, raw abundances are transformed prior to analysis_ Usually you will use square root, fourth-root, log(X+1), or presence-absence (square root being least extreme, P/A being most). I would start with square root. (https://stats_stackexchange_com/questions/234495/double-zeroes-problem-with-euclidean-distance-and-abundance-data-is-the-proble)

    na_rm = TRUE,
    method = "bray" #Construct species abundance dissimilarity matrices with Bray-Curtis_ If your data contains samples that are all-zero you will run into the double zero problem_ This can be overcome by using a zero-adjusted Bray-Curtis coefficient, which is sometimes referred to as a 'dummy variable' which damps down the similarity fluctuations between samples that are both zero (undefined). => see below for zero-adjusted Bray-Curtis coefficient ; #another possibility, sqrt + 1 ??
  )


 #https://rdrr_io/github/phytomosaic/ecole/man/bray0.html
 # mtxdis <- ecole::bray0(
 #   sqrt
  #  (qecbnato0_x[,conca]), na_rm = TRUE)

  mtxdis
  expand_grid(mtxdis)

  mtxdisdf <- as_data_frame(as_matrix(mtxdis))

  a_ <- NA
  b_ <- NA
  c_ <- NA
  d_ <- NA
  e_ <- NA
  f_ <- NA
  g_ <- NA
  h_ <- NA
  i_ <- NA
  j_ <- NA
  k_ <- NA
  l_ <- NA
  m_ <- NA
  n_ <- NA

  for (z in c(1:nrow(mtxdisdf))) {

    a_[[z]] <- (paste0(rownames(mtxdisdf[z + 1, ]), " & ", ifelse(nrow(mtxdisdf) >= 1, colnames(mtxdisdf[1]), NA)))
    b_[[z]] <- (paste0(rownames(mtxdisdf[z + 2, ]), " & ", ifelse(nrow(mtxdisdf) >= 2, colnames(mtxdisdf[2]), NA)))
    c_[[z]] <- (paste0(rownames(mtxdisdf[z + 3, ]), " & ", ifelse(nrow(mtxdisdf) >= 3, colnames(mtxdisdf[3]), NA)))
    d_[[z]] <- (paste0(rownames(mtxdisdf[z + 4, ]), " & ", ifelse(nrow(mtxdisdf) >= 4, colnames(mtxdisdf[4]), NA)))
    e_[[z]] <- (paste0(rownames(mtxdisdf[z + 5, ]), " & ", ifelse(nrow(mtxdisdf) >= 5, colnames(mtxdisdf[5]), NA)))
    f_[[z]] <- (paste0(rownames(mtxdisdf[z + 6, ]), " & ", ifelse(nrow(mtxdisdf) >= 6, colnames(mtxdisdf[6]), NA)))
    g_[[z]] <- (paste0(rownames(mtxdisdf[z + 7, ]), " & ", ifelse(nrow(mtxdisdf) >= 7, colnames(mtxdisdf[7]), NA)))
    h_[[z]] <- (paste0(rownames(mtxdisdf[z + 8, ]), " & ", ifelse(nrow(mtxdisdf) >= 8, colnames(mtxdisdf[8]), NA)))
    i_[[z]] <- (paste0(rownames(mtxdisdf[z + 9, ]), " & ", ifelse(nrow(mtxdisdf) >= 9, colnames(mtxdisdf[9]), NA)))
    j_[[z]] <- (paste0(rownames(mtxdisdf[z + 10, ]), " & ",  ifelse(nrow(mtxdisdf) >= 10, colnames(mtxdisdf[10]), NA)))
    k_[[z]] <- (paste0(rownames(mtxdisdf[z + 11, ]), " & ",  ifelse(nrow(mtxdisdf) >= 11, colnames(mtxdisdf[11]), NA)))
    l_[[z]] <- (paste0(rownames(mtxdisdf[z + 12, ]), " & ",  ifelse(nrow(mtxdisdf) >= 12, colnames(mtxdisdf[12]), NA)))
    m_[[z]] <- (paste0(rownames(mtxdisdf[z + 13, ]), " & ", ifelse(nrow(mtxdisdf) >= 13, colnames(mtxdisdf[13]), NA)))
    n_[[z]] <- (paste0(rownames(mtxdisdf[z + 14, ]), " & ", ifelse(nrow(mtxdisdf) >= 14, colnames(mtxdisdf[14]), NA)))

  }

  rm(z)

  y <- length(a_) - (ifelse(nrow(mtxdisdf) >= 1, 1, nrow(mtxdisdf)))
  a_ <- a_[1:y]
  y <- length(b_) - (ifelse(nrow(mtxdisdf) >= 2, 2, nrow(mtxdisdf)))
  b_ <- b_[1:y]
  y <- length(c_) - (ifelse(nrow(mtxdisdf) >= 3, 3, nrow(mtxdisdf)))
  c_ <- c_[1:y]
  y <- length(d_) - (ifelse(nrow(mtxdisdf) >= 4, 4, nrow(mtxdisdf)))
  d_ <- d_[1:y]
  y <- length(e_) - (ifelse(nrow(mtxdisdf) >= 5, 5, nrow(mtxdisdf)))
  e_ <- e_[1:y]
  y <- length(f_) - (ifelse(nrow(mtxdisdf) >= 6, 6, nrow(mtxdisdf)))
  f_ <- f_[1:y]
  y <- length(g_) - (ifelse(nrow(mtxdisdf) >= 7, 7, nrow(mtxdisdf)))
  g_ <- g_[1:y]
  y <- length(h_) - (ifelse(nrow(mtxdisdf) >= 8, 8, nrow(mtxdisdf)))
  h_ <- h_[1:y]
  y <- length(i_) - (ifelse(nrow(mtxdisdf) >= 9, 9, nrow(mtxdisdf)))
  i_ <- i_[1:y]
  y <- length(j_) - (ifelse(nrow(mtxdisdf) >= 10, 10, nrow(mtxdisdf)))
  j_ <- j_[1:y]
  y <- length(k_) - (ifelse(nrow(mtxdisdf) >= 11, 11, nrow(mtxdisdf)))
  k_ <- k_[1:y]
  y <- length(l_) - (ifelse(nrow(mtxdisdf) >= 12, 12, nrow(mtxdisdf)))
  l_ <- l_[1:y]
  y <- length(m_) - (ifelse(nrow(mtxdisdf) >= 13, 13, nrow(mtxdisdf)))
  m_ <- m_[1:y]
  y <- length(n_) - (ifelse(nrow(mtxdisdf) >= 14, 14, nrow(mtxdisdf)))
  n_ <- n_[1:y]

  rm(y)

  name_ <- c(a_, b_, c_, d_, e_, f_, g_, h_, i_, j_, k_, l_, m_, n_)
  df_ <- data_frame(expand_grid(mtxdis), name_[1:nrow(expand_grid(mtxdis))])
  names(df_) <- c("dist.", "name_")

  rm(a_, b_, c_, d_, e_, f_, g_, h_, i_, j_, k_, l_, m_, n_)
  rm(name_)

  q_ <- strsplit(df_$name_, " & ")
  mat_  <- matrix(unlist(q_), ncol = 2, byrow = TRUE)
  q_df_   <- as_data_frame(matrix(unlist(q_), ncol = 2, byrow = TRUE))
  df_ <- dplyr::bind_cols(df_, q_df_)

  rm(q_, mat_, q_df_)

  split_ <- strsplit(df_$V1, "_")
  v1_split_ <- as_data_frame(matrix(unlist(split_), ncol = 4, byrow = TRUE))
  split_ <- strsplit(df_$V2, "_")
  v2_split_ <- as_data_frame(matrix(unlist(split_), ncol = 4, byrow = TRUE))

  df_ <- dplyr::bind_cols(df_, v1_split_)
  df_ <- dplyr::bind_cols(df_, v2_split_)
  df_red_ <- subset(df_, V4...8 == V4...12 & V1...5 != V1...9)
  site_year_month_day <- rep(unique(qecbnato0_x$site_year_month_day), nrow(df_red_))

  df_red_ <- tibble::add_column(df_red_, site_year_month_day, .before = "dist.")

  rm(split_, v1_split_, v2_split_)
  rm(mtxdis, mtxdisdf, df_, site_year_month_day)


  matri_list[[x]] <- df_red_ 
  matri_list <<- matri_list

  rm(df_red_, qecbnato0_x, x)

  }

  matri_df <- do_call("rbind", matri_list)

  names(matri_df) <- c("site_year_month_day", "dist.", "name_", "name_left", "name_right", "Type_Bloc_left", "Face_left", "Numéro_Bloc_échantillon_left", "Quadrat.left", "Type_Bloc_right", "Face_right", "Numéro_Bloc_échantillon_right", "Quadrat.right")

  matri_df <<- matri_df

  hist(matri_df$dist.)

}

data_ <- dplyr::filter(qecbnato0, Face == "face supérieure")
data_$Type_Bloc <- ifelse(as_character(data_$Type_Bloc) == "Roche en place", "Bloc fixé", as_character(data_$Type_Bloc))
matri_list <- vector("list", length(unique(data_$site_year_month_day)))

matri_fct_bmf(data = data_, conca = c(bret_egmp_basq_qecb, egmp_basq_qecb))
hist(matri_df$dist., main = c(paste("Histo_ of Bray (0-adjusted) dist. dissimilarity measures"), paste(" (sqrt transfo) - BMfs vs BF -")))

matri_full_bm_bf_fs <- matri_df

saveRDS(matri_full_bm_bf_fs, "matri_full_log_spi_Bm_BF_Fs_RDS")
rm(data_, matri_df, matri_list)


## determination of coefficient of dissimilarity between blocs mobiles face sup vs face inf_

# loop in a fct

matri_fct_bmm <- function(data, conca) {

  matri_df <- data

  for (x in c(1:length(unique(matri_df$site_year_month_day)))) {

    qecbnato0_x <- matri_df %>% dplyr::filter(site_year_month_day == unique(matri_df$site_year_month_day)[[x]])

    rownames(qecbnato0_x) <- paste0(qecbnato0_x$Type_Bloc, "_", qecbnato0_x$Face,  "_", qecbnato0_x$Numéro_Bloc_échantillon, "_", qecbnato0_x$quadrat_bis)


  #mtxdis <- vegan::vegdist(
  #  sqrt
  #  (qecbnato0_x[,c(bret_egmp_basq_qecb)]), #Transform your species abundance data_ Typically, raw abundances are transformed prior to analysis_ Usually you will use square root, fourth-root, log(X+1), or presence-absence (square root being least extreme, P/A being most). I would start with square root. (https://stats_stackexchange_com/questions/234495/double-zeroes-problem-with-euclidean-distance-and-abundance-data-is-the-proble)
  #  na_rm = T,
  #  method = "bray" #Construct species abundance dissimilarity matrices with Bray-Curtis_ If your data contains samples that are all-zero you will run into the double zero problem_ This can be overcome by using a zero-adjusted Bray-Curtis coefficient, which is sometimes referred to as a 'dummy variable' which damps down the similarity fluctuations between samples that are both zero (undefined). => see below for zero-adjusted Bray-Curtis coefficient ; #another possibility, sqrt + 1 ??
  #)
  

  mtxdis <- ecole::bray0(
    sqrt(qecbnato0_x[, conca]), na_rm = TRUE)


  mtxdis
  mtxdis
  expand_grid(mtxdis)

  mtxdisdf <- as_data_frame(as_matrix(mtxdis))

  a_ <- NA
  b_ <- NA
  c_ <- NA
  d_ <- NA
  e_ <- NA
  f_ <- NA
  g_ <- NA
  h_ <- NA
  i_ <- NA
  j_ <- NA
  k_ <- NA
  l_ <- NA
  m_ <- NA
  n_ <- NA
  o_ <- NA
  p_ <- NA
  q_ <- NA
  r_ <- NA
  s_ <- NA

  for (z in c(1:nrow(mtxdisdf))) {

    a_[[z]] <- (paste0(rownames(mtxdisdf[z + 1, ]), " & ", ifelse(nrow(mtxdisdf) >= 1, colnames(mtxdisdf[1]), NA))) 
    b_[[z]] <- (paste0(rownames(mtxdisdf[z + 2, ]), " & ", ifelse(nrow(mtxdisdf) >= 2, colnames(mtxdisdf[2]), NA)))
    c_[[z]] <- (paste0(rownames(mtxdisdf[z + 3, ]), " & ", ifelse(nrow(mtxdisdf) >= 3, colnames(mtxdisdf[3]), NA)))
    d_[[z]] <- (paste0(rownames(mtxdisdf[z + 4, ]), " & ", ifelse(nrow(mtxdisdf) >= 4, colnames(mtxdisdf[4]), NA)))
    e_[[z]] <- (paste0(rownames(mtxdisdf[z + 5, ]), " & ", ifelse(nrow(mtxdisdf) >= 5, colnames(mtxdisdf[5]), NA)))
    f_[[z]] <- (paste0(rownames(mtxdisdf[z + 6, ]), " & ", ifelse(nrow(mtxdisdf) >= 6, colnames(mtxdisdf[6]), NA)))
    g_[[z]] <- (paste0(rownames(mtxdisdf[z + 7, ]), " & ", ifelse(nrow(mtxdisdf) >= 7, colnames(mtxdisdf[7]), NA)))
    h_[[z]] <- (paste0(rownames(mtxdisdf[z + 8, ]), " & ", ifelse(nrow(mtxdisdf) >= 8, colnames(mtxdisdf[8]), NA)))
    i_[[z]] <- (paste0(rownames(mtxdisdf[z + 9, ]), " & ", ifelse(nrow(mtxdisdf) >= 9, colnames(mtxdisdf[9]), NA)))
    j_[[z]] <- (paste0(rownames(mtxdisdf[z + 10, ]), " & ",  ifelse(nrow(mtxdisdf) >= 10, colnames(mtxdisdf[10]), NA)))
    k_[[z]] <- (paste0(rownames(mtxdisdf[z + 11, ]), " & ",  ifelse(nrow(mtxdisdf) >= 11, colnames(mtxdisdf[11]), NA)))
    l_[[z]] <- (paste0(rownames(mtxdisdf[z + 12, ]), " & ",  ifelse(nrow(mtxdisdf) >= 12, colnames(mtxdisdf[12]), NA)))
    m_[[z]] <- (paste0(rownames(mtxdisdf[z + 13, ]), " & ", ifelse(nrow(mtxdisdf) >= 13, colnames(mtxdisdf[13]), NA)))
    n_[[z]] <- (paste0(rownames(mtxdisdf[z + 14, ]), " & ", ifelse(nrow(mtxdisdf) >= 14, colnames(mtxdisdf[14]), NA)))
    o_[[z]] <- (paste0(rownames(mtxdisdf[z + 15, ]), " & ", ifelse(nrow(mtxdisdf) >= 15, colnames(mtxdisdf[15]), NA)))
    p_[[z]] <- (paste0(rownames(mtxdisdf[z + 16, ]), " & ", ifelse(nrow(mtxdisdf) >= 16, colnames(mtxdisdf[16]), NA)))
    q_[[z]] <- (paste0(rownames(mtxdisdf[z + 17, ]), " & ", ifelse(nrow(mtxdisdf) >= 17, colnames(mtxdisdf[17]), NA)))
    r_[[z]] <- (paste0(rownames(mtxdisdf[z + 18, ]), " & ", ifelse(nrow(mtxdisdf) >= 18, colnames(mtxdisdf[18]), NA)))
    s_[[z]] <- (paste0(rownames(mtxdisdf[z + 19, ]), " & ", ifelse(nrow(mtxdisdf) >= 19, colnames(mtxdisdf[19]), NA)))

  }

  rm(z)

  y <- length(a_) - (ifelse(nrow(mtxdisdf) >= 1, 1, nrow(mtxdisdf)))
  a_ <- a_[1:y]
  y <- length(b_) - (ifelse(nrow(mtxdisdf) >= 2, 2, nrow(mtxdisdf)))
  b_ <- b_[1:y]
  y <- length(c_) - (ifelse(nrow(mtxdisdf) >= 3, 3, nrow(mtxdisdf)))
  c_ <- c_[1:y]
  y <- length(d_) - (ifelse(nrow(mtxdisdf) >= 4, 4, nrow(mtxdisdf)))
  d_ <- d_[1:y]
  y <- length(e_) - (ifelse(nrow(mtxdisdf) >= 5, 5, nrow(mtxdisdf)))
  e_ <- e_[1:y]
  y <- length(f_) - (ifelse(nrow(mtxdisdf) >= 6, 6, nrow(mtxdisdf)))
  f_ <- f_[1:y]
  y <- length(g_) - (ifelse(nrow(mtxdisdf) >= 7, 7, nrow(mtxdisdf)))
  g_ <- g_[1:y]
  y <- length(h_) - (ifelse(nrow(mtxdisdf) >= 8, 8, nrow(mtxdisdf)))
  h_ <- h_[1:y]
  y <- length(i_) - (ifelse(nrow(mtxdisdf) >= 9, 9, nrow(mtxdisdf)))
  i_ <- i_[1:y]
  y <- length(j_) - (ifelse(nrow(mtxdisdf) >= 10, 10, nrow(mtxdisdf)))
  j_ <- j_[1:y]
  y <- length(k_) - (ifelse(nrow(mtxdisdf) >= 11, 11, nrow(mtxdisdf)))
  k_ <- k_[1:y]
  y <- length(l_) - (ifelse(nrow(mtxdisdf) >= 12, 12, nrow(mtxdisdf)))
  l_ <- l_[1:y]
  y <- length(m_) - (ifelse(nrow(mtxdisdf) >= 13, 13, nrow(mtxdisdf)))
  m_ <- m_[1:y]
  y <- length(n_) - (ifelse(nrow(mtxdisdf) >= 14, 14, nrow(mtxdisdf)))
  n_ <- n_[1:y]
  y <- length(o_) - (ifelse(nrow(mtxdisdf) >= 15, 15, nrow(mtxdisdf)))
  o_ <- o_[1:y]
  y <- length(p_) - (ifelse(nrow(mtxdisdf) >= 16, 16, nrow(mtxdisdf)))
  p_ <- p_[1:y]
  y <- length(q_) - (ifelse(nrow(mtxdisdf) >= 17, 17, nrow(mtxdisdf)))
  q_ <- q_[1:y]
  y <- length(r_) - (ifelse(nrow(mtxdisdf) >= 18, 18, nrow(mtxdisdf)))
  r_ <- r_[1:y]
  y <- length(s_) - (ifelse(nrow(mtxdisdf) >= 19, 19, nrow(mtxdisdf)))
  s_ <- s_[1:y]

  rm(y)

  name_ <- c(a_, b_, c_, d_, e_, f_, g_, h_, i_, j_, k_, l_, m_, n_, o_, p_, q_, r_, s_)
  df_ <- data_frame(expand_grid(mtxdis), name_[1:nrow(expand_grid(mtxdis))])
  names(df_) <- c("dist.", "name_")

  rm(a_, b_, c_, d_, e_, f_, g_, h_, i_, j_, k_, l_, m_, n_, o_, p_, q_, r_, s_)
  rm(name_)

  q_ <- strsplit(df_$name_, " & ")
  mat_  <- matrix(unlist(q_), ncol = 2, byrow = TRUE)
  q_df_   <- as_data_frame(matrix(unlist(q_), ncol = 2, byrow = TRUE))
  df_ <- dplyr::bind_cols(df_, q_df_)

  rm(q_, mat_, q_df_)

  split_ <- strsplit(df_$V1, "_")
  v1_split_ <- as_data_frame(matrix(unlist(split_), ncol = 4, byrow = TRUE))
  split_ <- strsplit(df_$V2, "_")
  v2_split_ <- as_data_frame(matrix(unlist(split_), ncol = 4, byrow = TRUE))

  df_ <- dplyr::bind_cols(df_, v1_split_)
  df_ <- dplyr::bind_cols(df_, v2_split_)
  df_red_ <- subset(df_, V4...8 == V4...12 & V3...7 == V3...11)
  site_year_month_day <- rep(unique(qecbnato0_x$site_year_month_day), nrow(df_red_))

  df_red_ <- tibble::add_column(df_red_, site_year_month_day, .before = "dist.")

  rm(split_, v1_split_, v2_split_)
  rm(mtxdis, mtxdisdf, df_, site_year_month_day)

  
  matri_list[[x]] <- df_red_ 
  matri_list <<- matri_list

  rm(df_red_, qecbnato0_x, x)

  }

  matri_df <- do_call("rbind", matri_list)

  names(matri_df) <- c("site_year_month_day", "dist.", "name_", "name_left", "name_right", "Type_Bloc_left", "Face_left", "Numéro_Bloc_échantillon_left", "Quadrat.left", "Type_Bloc_right", "Face_right", "Numéro_Bloc_échantillon_right", "Quadrat.right")

  matri_df <<- matri_df

  hist(matri_df$dist.)

}

data_ <- dplyr::filter(qecbnato0, Type_Bloc == "Bloc mobile")
matri_list <- vector("list", length(unique(data_$site_year_month_day)))

matri_fct_bmm(data = data_, conca = c(bret_egmp_basq_qecb, egmp_basq_qecb))
hist(matri_df$dist., main = c(paste("Histo_ of Bray (0-adjusted) dist. dissimilarity measures"), paste(" (sqrt transfo) - BMfs vs BMfi -")))

matri_full_bm_fs_fi <- matri_df

saveRDS(matri_full_bm_fs_fi, "matri_full_log_spi_BM_Fs_Fi_RDS")
rm(data_, matri_df, matri_list)


## plot

# activate line






matri_full_bm_bf_fs <- tidyr::separate(matri_full_bm_bf_fs, "site_year_month_day", into = c("departement", "Site", "Year", "Month", "Day"), remove = FALSE)
matri_full_bm_bf_fs$Site <- paste0(matri_full_bm_bf_fs$departement, "_", matri_full_bm_bf_fs$Site)
matri_full_bm_bf_fs <- subset(matri_full_bm_bf_fs, select = - c(departement))
matri_full_bm_bf_fs <- tibble::add_column(matri_full_bm_bf_fs, Date = as_Date(paste0(matri_full_bm_bf_fs$Year, "-", matri_full_bm_bf_fs$Month, "-", matri_full_bm_bf_fs$Day), origin = "1970-01-01"), .after = "Site")
matri_full_bm_bf_fs$Site <- as_factor(matri_full_bm_bf_fs$Site)

matri_full_bm_fs_fi <- tidyr::separate(matri_full_bm_fs_fi, "site_year_month_day", into = c("departement", "Site", "Year", "Month", "Day"), remove = FALSE)
matri_full_bm_fs_fi$Site <- paste0(matri_full_bm_fs_fi$departement, "_", matri_full_bm_fs_fi$Site)
matri_full_bm_fs_fi <- subset(matri_full_bm_fs_fi, select = - c(departement))
matri_full_bm_fs_fi <- tibble::add_column(matri_full_bm_fs_fi, Date = as_Date(paste0(matri_full_bm_fs_fi$Year, "-", matri_full_bm_fs_fi$Month, "-", matri_full_bm_fs_fi$Day), origin = "1970-01-01"), .after = "Site")
matri_full_bm_fs_fi$Site <- as_factor(matri_full_bm_fs_fi$Site)

# if error message "Error in .Call_graphics(C_palette2, .Call(C_palette2, NULL)) : invalid graphics state"



bf_fs_plot <- ggplot2::ggplot(matri_full_bm_bf_fs, ggplot2::aes(x = Site, y = dist.)) +
  ggplot2::geom_boxplot() +
  #geom_jitter(shape = 16, position=position_jitter(0.2)) +
  ggplot2::xlab("") +
  ggplot2::ylab("distance diss_ Bm_BF_FS") +
  ggplot2::theme(axis_text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))

ggplot2::ggsave("distance_diss_BF_Fs_png", bf_fs_plot)

fs_fi_plot <- ggplot2::ggplot(matri_full_bm_fs_fi, ggplot2::aes(x = Site, y = dist.)) +
  ggplot2::geom_boxplot() +
  #geom_jitter(shape = 16, position=position_jitter(0.2)) +
  ggplot2::xlab("") +
  ggplot2::ylab("distance diss_ BM_Fs_FI") +
  ggplot2::theme(axis_text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))

ggplot2::ggsave("distance_diss_FS_Fi_png", fs_fi_plot)

# issue with type de bloc, numéro de bloc and quadrat for df_ Bm_BF_FS, cfr df_ left vs right variables doesn't give the right combination (variables with left vs right label in names come from the dissimilarity coefficient functions).
matri_full_bm_bf_fs$Quadrat <- NA
for (i in c(1:nrow(matri_full_bm_bf_fs))) {
  ifelse(matri_full_bm_bf_fs$Type_Bloc_left[i] == "Bloc mobile", matri_full_bm_bf_fs$Quadrat[i] <- matri_full_bm_bf_fs$Quadrat.left[i], matri_full_bm_bf_fs$Quadrat[i] <- matri_full_bm_bf_fs$Quadrat.right[i])}
matri_full_bm_bf_fs$Numéro_Bloc <- NA
for (i in c(1:nrow(matri_full_bm_bf_fs))) {
  ifelse(matri_full_bm_bf_fs$Type_Bloc_left[i] == "Bloc mobile", matri_full_bm_bf_fs$Numéro_Bloc[i] <- matri_full_bm_bf_fs$Numéro_Bloc_échantillon_left[i], matri_full_bm_bf_fs$Numéro_Bloc[i] <- matri_full_bm_bf_fs$Numéro_Bloc_échantillon_right[i])}

matri_full_bm_bf_fs <- tibble::add_column(matri_full_bm_bf_fs, site_year_month_day.q_BMnb = paste0(matri_full_bm_bf_fs$site_year_month_day, "_",  matri_full_bm_bf_fs$Quadrat, "_", matri_full_bm_bf_fs$Numéro_Bloc), .after = "site_year_month_day")
matri_full_bm_fs_fi <- tibble::add_column(matri_full_bm_fs_fi, site_year_month_day.q_BMnb = paste0(matri_full_bm_fs_fi$site_year_month_day, "_",  matri_full_bm_fs_fi$Quadrat.left, "_", matri_full_bm_fs_fi$Numéro_Bloc_échantillon_left), .after = "site_year_month_day")

colnames(matri_full_bm_bf_fs) <- paste("Bm_BF_FS", colnames(matri_full_bm_bf_fs), sep = "_")
matri_full_bm_bf_fs <- dplyr::rename(matri_full_bm_bf_fs, site_year_month_day.q_BMnb = Bm_BF_FS_site_year_month_day.q_BMnb)
colnames(matri_full_bm_fs_fi) <- paste("BM_Fs_FI", colnames(matri_full_bm_fs_fi), sep = "_")
matri_full_bm_fs_fi <- dplyr::rename(matri_full_bm_fs_fi, site_year_month_day.q_BMnb = BM_Fs_FI_site_year_month_day.q_BMnb)

matri_full <- dplyr::full_join(matri_full_bm_bf_fs[, c("site_year_month_day.q_BMnb", "Bm_BF_FS_dist.")], matri_full_bm_fs_fi[, c("site_year_month_day.q_BMnb", "BM_Fs_FI_dist.")])

matri_full <- tidyr::separate(matri_full, "site_year_month_day.q_BMnb", into = c("departement", "Site", "Year", "Month", "Day", "Quadrat", "Bloc Mobile Number"), remove = FALSE)
matri_full$Site <- paste0(matri_full$departement, "_", matri_full$Site)
matri_full <- subset(matri_full, select = - c(departement))
matri_full <- tibble::add_column(matri_full, Date = as_Date(paste0(matri_full$Year, "-", matri_full$Month, "-", matri_full$Day), origin = "1970-01-01"), .after = "Site")

# Name for report/plot

matri_full <- tibble::add_column(matri_full, Site_bis = NA, .after = "Site")

matri_full$Site_bis <- ifelse(matri_full$Site == "GDMO_Locmariaquer", "Locmariaquer", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "GDMO_BegLann", "Beg Lann", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "FOUR_PlateauFour", "Plateau du Four", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "EGMP_GroinCou", "Groin du Cou", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "EGMP_PasEmsembert", "Le Pas d'Emsembert", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "EGMP_BreeBains", "La Brée-les-Bains", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "EGMP_PerreAntiochat", "Le Perré d'Antiochat", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "EGMP_Chassiron", "Chassiron", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "BASQ_FlotsBleusZP", "Les Flots Bleus / zone pêcheurs", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "BASQ_FlotsBleusZF", "Les Flots Bleus / zone familles", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "GONB_IlotStMichel", "Îlot Saint-Michel", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "FINS_Quemenes", "Quéménès", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "FINS_SeinGoulenez", "Île de Sein - Goulenez", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "FINS_SeinKilaourou", "Île de Sein - Kilaourou", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "ARMO_Verdelet", "Îlot du Verdelet", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "ARMO_Piegu", "Piégu", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "ARMO_Bilfot", "Pointe de Bilfot", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "ARMO_IlePlate", "Île Plate", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "PDMO_Perharidy", "Perharidy", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "BRES_Keraliou", "Keraliou", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "FINS_Mousterlin", "Pointe de Mousterlin", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "FINS_StNicolasGlenan", "Saint-Nicolas des Glénan", matri_full$Site_bis)

unique(matri_full[, c("Site", "Site_bis")])


saveRDS(matri_full, "matri_full_log_spi_RDS")


## plot dissimilarity coefficient

matri_full$Year <- as_integer(matri_full$Year)
matri_full$Month <- as_integer(matri_full$Month)
matri_full$Day <- as_integer(matri_full$Day)


## BM_Fs_FI_dist => mobile boulder upper vs lower faces

# Stats

bm_fs_fi_dist_stat <- matri_full %>% dplyr::group_by(Site, Site_bis, Date, Year, Month, Day) %>% dplyr::summarize(BM_Fs_FI_dist.moy = mean(BM_Fs_FI_dist., na_rm = TRUE), BM_Fs_FI_dist.et = sd(BM_Fs_FI_dist., na_rm = TRUE), BM_Fs_FI_dist.med = median(BM_Fs_FI_dist., na_rm = TRUE), BM_Fs_FI_dist.min = min(BM_Fs_FI_dist., na_rm = TRUE), BM_Fs_FI_dist.max = max(BM_Fs_FI_dist., na_rm = TRUE), nb_ = dplyr::n(), nb_notNa = sum(!is_na(BM_Fs_FI_dist.)))

bm_fs_fi_dist_stat <- dplyr::ungroup(bm_fs_fi_dist_stat)

# Quality scale based on quartiles

one <- round(mean(unlist(dplyr::filter(matri_full, BM_Fs_FI_dist. <= quantile(matri_full$BM_Fs_FI_dist., 0.25, na_rm = TRUE))["BM_Fs_FI_dist."])), digits = 3)
two <- round(mean(unlist(dplyr::filter(matri_full, BM_Fs_FI_dist. > quantile(matri_full$BM_Fs_FI_dist., 0.25, na_rm = TRUE) & BM_Fs_FI_dist. <= quantile(matri_full$BM_Fs_FI_dist., 0.5, na_rm = TRUE))["BM_Fs_FI_dist."])), digits = 3)
three <- round(mean(unlist(dplyr::filter(matri_full, BM_Fs_FI_dist. > quantile(matri_full$BM_Fs_FI_dist., 0.5, na_rm = TRUE) & BM_Fs_FI_dist. <= quantile(matri_full$BM_Fs_FI_dist., 0.75, na_rm = TRUE))["BM_Fs_FI_dist."])), digits = 3)
four <- round(mean(unlist(dplyr::filter(matri_full, BM_Fs_FI_dist. > quantile(matri_full$BM_Fs_FI_dist., 0.75, na_rm = TRUE))["BM_Fs_FI_dist."])), digits = 3)

# Plot

for (i in c(1:length(unique(bm_fs_fi_dist_stat$Site)))) {

  df1 <- dplyr::filter(bm_fs_fi_dist_stat, Site == unique(bm_fs_fi_dist_stat$Site)[i])

  xmin_ <- as_Date(ifelse(min(df1$Year) >= 2014, "2014-01-01", paste0(min(matri_full$Year), "-01-01")), origin = "1970-01-01")
  xmax_ <- as_Date(ifelse(max(df1$Year) <= 2017, "2018-01-01", #paste0(max(matri_full$Year)+1,
                          "2022-01-01")
                   #)
                   , origin = "1970-01-01")

  ymin_ <- 0
  ymax_ <- 1

  png(paste0("diss_", unique(bm_fs_fi_dist_stat$Site), ".png"))
  plot(bm_fs_fi_dist_stat$Date, bm_fs_fi_dist_stat$BM_Fs_FI_dist.med, xlim = c(xmin_, xmax_), ylim = c(ymin_, ymax_), pch = 19, main = "", xlab = "", ylab = "", type = "n", axes = FALSE)

  rect(as_Date("2013-01-01", origin = "1970-01-01"), -0.1, as_Date("2023-01-01", origin = "1970-01-01"), one, col = "red", border = NA)
  rect(as_Date("2013-01-01", origin = "1970-01-01"), one, as_Date("2023-01-01", origin = "1970-01-01"), two, col = "orange", border = NA)
  rect(as_Date("2013-01-01", origin = "1970-01-01"), two, as_Date("2023-01-01", origin = "1970-01-01"), three, col = "yellow", border = NA)
  rect(as_Date("2013-01-01", origin = "1970-01-01"), three, as_Date("2023-01-01", origin = "1970-01-01"), four, col = "olivedrab", border = NA)
  rect(as_Date("2013-01-01", origin = "1970-01-01"), four, as_Date("2023-01-01", origin = "1970-01-01"), 1.1, col = "blue", border = NA)

  par(new = TRUE)
  plot(bm_fs_fi_dist_stat$Date, bm_fs_fi_dist_stat$BM_Fs_FI_dist.med, xlim = c(xmin_, xmax_), ylim = c(ymin_, ymax_), pch = 19, cex = 1, main = unique(df1$Site_bis), xlab = "Année",
       ylab = "Coef_ dissi_ BM_Fs_FI", col = "grey")
  points(df1$Date, df1$BM_Fs_FI_dist.med, pch = 19, cex = 1.5)
  arrows(df1$Date, df1$BM_Fs_FI_dist.med, df1$Date, df1$BM_Fs_FI_dist.max, code = 3, angle = 90, length = 0.00)
  arrows(df1$Date, df1$BM_Fs_FI_dist.med, df1$Date, df1$BM_Fs_FI_dist.min, code = 3, angle = 90, length = 0.00)

}

rm(df1, four, i, one, three, two, xmax_, xmin_, ymax_, ymin_)


## Bm_BF_FS_dist => mobile boulder vs fixed boulder upper faces

# Stats

bm_fs_fi_dist_stat_ <- matri_full %>% dplyr::group_by(Site, Site_bis, Date, Year, Month, Day) %>% dplyr::summarize(Bm_BF_FS_dist.moy = mean(Bm_BF_FS_dist., na_rm = TRUE), Bm_BF_FS_dist.et = sd(Bm_BF_FS_dist., na_rm = TRUE), Bm_BF_FS_dist.med = median(Bm_BF_FS_dist., na_rm = TRUE), Bm_BF_FS_dist.min = min(Bm_BF_FS_dist., na_rm = TRUE), Bm_BF_FS_dist.max = max(Bm_BF_FS_dist., na_rm = TRUE), nb_ = dplyr::n(), nb_notNa = sum(!is_na(Bm_BF_FS_dist.)))

bm_fs_fi_dist_stat_ <- dplyr::ungroup(bm_fs_fi_dist_stat_)

# Quality scale based on quartiles

one <- round(mean(unlist(dplyr::filter(matri_full, Bm_BF_FS_dist. <= quantile(matri_full$Bm_BF_FS_dist., 0.25, na_rm = TRUE))["Bm_BF_FS_dist."])), digits = 3)
two <- round(mean(unlist(dplyr::filter(matri_full, Bm_BF_FS_dist. > quantile(matri_full$Bm_BF_FS_dist., 0.25, na_rm = TRUE) & Bm_BF_FS_dist. <= quantile(matri_full$Bm_BF_FS_dist., 0.5, na_rm = TRUE))["Bm_BF_FS_dist."])), digits = 3)
three <- round(mean(unlist(dplyr::filter(matri_full, Bm_BF_FS_dist. > quantile(matri_full$Bm_BF_FS_dist., 0.5, na_rm = TRUE) & Bm_BF_FS_dist. <= quantile(matri_full$Bm_BF_FS_dist., 0.75, na_rm = TRUE))["Bm_BF_FS_dist."])), digits = 3)
four <- round(mean(unlist(dplyr::filter(matri_full, Bm_BF_FS_dist. > quantile(matri_full$Bm_BF_FS_dist., 0.75, na_rm = TRUE))["Bm_BF_FS_dist."])), digits = 3)

# Plot

for (i in c(1:length(unique(bm_fs_fi_dist_stat_$Site)))) {

  dplyr::filter(bm_fs_fi_dist_stat_, Site == unique(bm_fs_fi_dist_stat_$Site)[i]) -> df1

  xmin_ <- as_Date(ifelse(min(df1$Year) >= 2014, "2014-01-01", paste0(min(matri_full$Year), "-01-01")), origin = "1970-01-01")
  xmax_ <- as_Date(ifelse(max(df1$Year) <= 2017, "2018-01-01", #paste0(max(matri_full$Year)+1, 
                          "2022-01-01")
                   #)
                   , origin = "1970-01-01")

  ymin_ <- 0
  ymax_ <- 1

  png(paste0("diss_", unique(bm_fs_fi_dist_stat_$Site), ".png"))
  plot(bm_fs_fi_dist_stat_$Date, bm_fs_fi_dist_stat_$Bm_BF_FS_dist.med, xlim = c(xmin_, xmax_), ylim = c(ymin_, ymax_), pch = 19, main = "", xlab = "", ylab = "", type = "n", axes = FALSE)

  rect(as_Date("2013-01-01", origin = "1970-01-01"), -0.1, as_Date("2023-01-01", origin = "1970-01-01"), one, col = "blue", border = NA)
  rect(as_Date("2013-01-01", origin = "1970-01-01"), one, as_Date("2023-01-01", origin = "1970-01-01"), two, col = "olivedrab", border = NA)
  rect(as_Date("2013-01-01", origin = "1970-01-01"), two, as_Date("2023-01-01", origin = "1970-01-01"), three, col = "yellow", border = NA)
  rect(as_Date("2013-01-01", origin = "1970-01-01"), three, as_Date("2023-01-01", origin = "1970-01-01"), four, col = "orange", border = NA)
  rect(as_Date("2013-01-01", origin = "1970-01-01"), four, as_Date("2023-01-01", origin = "1970-01-01"), 1.1, col = "red", border = NA)

  par(new = TRUE)
  plot(bm_fs_fi_dist_stat_$Date, bm_fs_fi_dist_stat_$Bm_BF_FS_dist.med, xlim = c(xmin_, xmax_), ylim = c(ymin_, ymax_), pch = 19, cex = 1, main = unique(df1$Site_bis), xlab = "Année", 
       ylab = "Coef_ dissi_ Bm_BF_FS", col = "grey")
  points(df1$Date, df1$Bm_BF_FS_dist.med, pch = 19, cex = 1.5)
  arrows(df1$Date, df1$Bm_BF_FS_dist.med, df1$Date, df1$Bm_BF_FS_dist.max, code = 3, angle = 90, length = 0.00)
  arrows(df1$Date, df1$Bm_BF_FS_dist.med, df1$Date, df1$Bm_BF_FS_dist.min, code = 3, angle = 90, length = 0.00)

}

rm(df1, four, i, one, three, two, xmax_, xmin_, ymax_, ymin_)

write_table(bm_fs_fi_dist_stat_, "Valeurs_stat.tabular", row.names = FALSE, quote = FALSE, sep = "\t", dec = ".", fileEncoding = "UTF-8")

saveRDS(bm_fs_fi_dist_stat, "matri_full_log_spi_bm_fs_fi_dist_statRDS")


saveRDS(bm_fs_fi_dist_stat_, "matri_full_log_spi_bm_fs_fi_dist_stat_RDS")
