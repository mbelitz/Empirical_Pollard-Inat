source("empirical_scripts/set_up_naive_estimates.R")

# Danaus_plexippus <- obs_to_estimate("Danaus plexippus", fiftiethfun)
# Phyciodes_tharos <- obs_to_estimate("Phyciodes tharos", fiftiethfun)
# Papilio_glaucus <- obs_to_estimate("Papilio glaucus", fiftiethfun)
# Pieris_rapae <- obs_to_estimate("Pieris rapae", fiftiethfun)
# Epargyreus_clarus <- obs_to_estimate("Epargyreus clarus", fiftiethfun)
# Vanessa_atalanta <- obs_to_estimate("Vanessa atalanta", fiftiethfun)
# Junonia_coenia <- obs_to_estimate("Junonia coenia", fiftiethfun)
# Colias_eurytheme <- obs_to_estimate("Colias eurytheme", fiftiethfun)
# Papilio_polyxenes <- obs_to_estimate("Papilio polyxenes", fiftiethfun)
# Polygonia_comma <- obs_to_estimate("Polygonia comma", fiftiethfun)
# Speyeria_cybele <- obs_to_estimate("Speyeria cybele", fiftiethfun)
# Limenitis_archippus <- obs_to_estimate("Limenitis archippus", fiftiethfun)
# Poanes_zabulon <- obs_to_estimate("Poanes zabulon", fiftiethfun)
# Ancyloxypha_numitor <- obs_to_estimate("Ancyloxypha numitor", fiftiethfun)
# Megisto_cymela <- obs_to_estimate("Megisto cymela", fiftiethfun)
# Polites_peckius <- obs_to_estimate("Polites peckius", fiftiethfun)
# Polygonia_interrogationis <- obs_to_estimate("Polygonia interrogationis", fiftiethfun)
# Erynnis_baptisiae <- obs_to_estimate("Erynnis baptisiae", fiftiethfun)
# Limenitis_arthemis_astyanax <- obs_to_estimate("Limenitis arthemis astyanax", fiftiethfun)
# Vanessa_virginiensis <- obs_to_estimate("Vanessa virginiensis", fiftiethfun)
# Vanessa_cardui <- obs_to_estimate("Vanessa cardui", fiftiethfun)
# Colias_philodice <- obs_to_estimate("Colias philodice", fiftiethfun)
# Cercyonis_pegala <- obs_to_estimate("Cercyonis pegala", fiftiethfun)
# Cupido_comyntas <- obs_to_estimate("Cupido comyntas", fiftiethfun)
# Papilio_troilus <- obs_to_estimate("Papilio troilus", fiftiethfun)
# Atalopedes_campestris <- obs_to_estimate("Atalopedes campestris", fiftiethfun)
# Euptoieta_claudia <- obs_to_estimate("Euptoieta claudia", fiftiethfun)
# Eurytides_marcellus <- obs_to_estimate("Eurytides marcellus", fiftiethfun)
# Polites_themistocles <- obs_to_estimate("Polites themistocles", fiftiethfun)
# Pyrgus_communis <- obs_to_estimate("Pyrgus communis", fiftiethfun)
Asterocampa_celtis <- obs_to_estimate("Asterocampa celtis", fiftiethfun)
Lerema_accius <- obs_to_estimate("Lerema accius", fiftiethfun)
Nastra_lherminier <- obs_to_estimate("Nastra lherminier", fiftiethfun)
Nymphalis_antiopa <- obs_to_estimate("Nymphalis antiopa", fiftiethfun)
Papilio_cresphontes <- obs_to_estimate("Papilio cresphontes", fiftiethfun)

empirical_fiftieth_estimates <- rbind(Danaus_plexippus
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

write.csv(empirical_fiftieth_estimates, file = "Emperical_Results/naive_fiftieth_estimates.csv", row.names = FALSE)
