## code to prepare `demo_botanique` dataset goes here

demo_botanique <- read.csv("D:/documents/stage/stage g.sapriel/Dossier_NOE/stage 2/shiny/data/sortie_freq_botanique.csv", row.names=1)
usethis::use_data(demo_botanique, overwrite = TRUE)
