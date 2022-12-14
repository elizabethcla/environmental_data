rm(list = ls())

pol_n_predation = 26
pol_n_no_predation = 184
pol_n_total = 210
pol_predation_rate = 0.124
  
psd_n_predation = 25
psd_n_no_predation = 706
psd_n_total = 731
psd_predation_rate = 0.034

print(
  paste0(
    "The seed predation rate for Polyscias fulva is: ",
    round(pol_predation_rate, digits = 3))) 

print(
  paste0(
    "The seed predation rate for Pseudospondias microcarpa is: ",
    round(psd_predation_rate, digits = 3)))
(pol_n_predation/pol_n_total)/(psd_n_predation/psd_n_total)

shit <- data.frame("species" = c('Polyscias fulva (pol)','Pseudospondias microcarpa (psd)'),
                   "Any Taken" = c(26, 25),
                   "None Taken" = c(184, 706),
                   "N" = c(210, 731),
                   "Predation Rate" = c(0.124, 0.34))
View(shit)
