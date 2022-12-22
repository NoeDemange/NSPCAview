## code to prepare `demo_eaux_foret` dataset goes here

demo_eaux_foret <- read.csv("D:/documents/stage/stage g.sapriel/Dossier_NOE/stage 2/shiny/data/sortie_freq_eaux_forets.csv", row.names=1)
usethis::use_data(demo_eaux_foret, overwrite = TRUE)
