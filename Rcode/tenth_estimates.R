source("data/set_up_pheno_estimates.R")

## Generate spp specific pheno estimates. Note these estimates will take a bit to run
Danaus_plexippus <- obs_to_estimate("Danaus plexippus", tenthfun)
Phyciodes_tharos <- obs_to_estimate("Phyciodes tharos", tenthfun)
Papilio_glaucus <- obs_to_estimate("Papilio glaucus", tenthfun)
Pieris_rapae <- obs_to_estimate("Pieris rapae", tenthfun)
Epargyreus_clarus <- obs_to_estimate("Epargyreus clarus", tenthfun)
Vanessa_atalanta <- obs_to_estimate("Vanessa atalanta", tenthfun)
Junonia_coenia <- obs_to_estimate("Junonia coenia", tenthfun)
Colias_eurytheme <- obs_to_estimate("Colias eurytheme", tenthfun)
Papilio_polyxenes <- obs_to_estimate("Papilio polyxenes", tenthfun)
Polygonia_comma <- obs_to_estimate("Polygonia comma", tenthfun)
Speyeria_cybele <- obs_to_estimate("Speyeria cybele", tenthfun)
Limenitis_archippus <- obs_to_estimate("Limenitis archippus", tenthfun)
Poanes_zabulon <- obs_to_estimate("Poanes zabulon", tenthfun)
Ancyloxypha_numitor <- obs_to_estimate("Ancyloxypha numitor", tenthfun)
Megisto_cymela <- obs_to_estimate("Megisto cymela", tenthfun)
Polites_peckius <- obs_to_estimate("Polites peckius", tenthfun)
Polygonia_interrogationis <- obs_to_estimate("Polygonia interrogationis", tenthfun)
Erynnis_baptisiae <- obs_to_estimate("Erynnis baptisiae", tenthfun)
Limenitis_arthemis_astyanax <- obs_to_estimate("Limenitis arthemis astyanax", tenthfun)
Vanessa_virginiensis <- obs_to_estimate("Vanessa virginiensis", tenthfun)
Vanessa_cardui <- obs_to_estimate("Vanessa cardui", tenthfun)
Colias_philodice <- obs_to_estimate("Colias philodice", tenthfun)
Cercyonis_pegala <- obs_to_estimate("Cercyonis pegala", tenthfun)
Cupido_comyntas <- obs_to_estimate("Cupido comyntas", tenthfun)
Papilio_troilus <- obs_to_estimate("Papilio troilus", tenthfun)
Atalopedes_campestris <- obs_to_estimate("Atalopedes campestris", tenthfun)
Euptoieta_claudia <- obs_to_estimate("Euptoieta claudia", tenthfun)
Eurytides_marcellus <- obs_to_estimate("Eurytides marcellus", tenthfun)
Polites_themistocles <- obs_to_estimate("Polites themistocles", tenthfun)
Pyrgus_communis <- obs_to_estimate("Pyrgus communis", tenthfun)
Asterocampa_celtis <- obs_to_estimate("Asterocampa celtis", tenthfun)
Lerema_accius <- obs_to_estimate("Lerema accius", tenthfun)
Nastra_lherminier <- obs_to_estimate("Nastra lherminier", tenthfun)
Nymphalis_antiopa <- obs_to_estimate("Nymphalis antiopa", tenthfun)
Papilio_cresphontes <- obs_to_estimate("Papilio cresphontes", tenthfun)

empirical_tenth_estimates <- rbind(Danaus_plexippus
                                   ,Phyciodes_tharos
                                   ,Papilio_glaucus
                                   ,Pieris_rapae
                                   ,Epargyreus_clarus
                                   ,Vanessa_atalanta
                                   ,Junonia_coenia
                                   ,Colias_eurytheme
                                   ,Papilio_polyxenes
                                   ,Polygonia_comma
                                   ,Speyeria_cybele
                                   ,Limenitis_archippus
                                   ,Poanes_zabulon
                                   ,Ancyloxypha_numitor
                                   ,Megisto_cymela
                                   ,Polites_peckius
                                   ,Polygonia_interrogationis
                                   ,Erynnis_baptisiae
                                   ,Limenitis_arthemis_astyanax
                                   ,Vanessa_virginiensis
                                   ,Vanessa_cardui
                                   ,Colias_philodice
                                   ,Cercyonis_pegala
                                   ,Cupido_comyntas
                                   ,Papilio_troilus
                                   ,Atalopedes_campestris
                                   ,Euptoieta_claudia
                                   ,Eurytides_marcellus
                                   ,Polites_themistocles
                                   ,Pyrgus_communis
                                   ,Asterocampa_celtis
                                   ,Lerema_accius
                                   ,Nastra_lherminier
                                   ,Nymphalis_antiopa
                                   ,Papilio_cresphontes)



write.csv(empirical_tenth_estimates, 
          file = "Emperical_Results/naive_tenth_estimates_BCAsCIs.csv", 
          row.names = FALSE)
