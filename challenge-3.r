library(assertthat)
library(dplyr)
library(tidyverse)
library(readr)

### Challenge III
# * Load both the 2020 election results ('wahlergebnisse.rds') and stadtteil_profile ('stadtteil_profil.rds').
# * Calculate each parties' election result per district (valid ballots are basis for this calculation).
# * Hint: investigate the function `across()` for applying calculation on multiple columns at once.
# * Calculate the ratio of people with a migration background in the total population of each district.
# * Compare migration ratio to results of the AfD
# * Compare the voter turnout to both other variables.
# * Join the two data sets.
# * Arrange by the AFD's results in descending order. 
# * Prepare to discuss in the next session!
# * Hint: the final table must have the following columns: stadtteil, mig_ratio, turn_out, afd.


stadtteil_profil <- readRDS("C:/Users/julia/Desktop/Wahlpflicht- R/all_about_data/data/stadtteil_profil.rds")
wahlergebnisse <- readRDS("C:/Users/julia/Desktop/Wahlpflicht- R/all_about_data/data/wahlergebnisse.rds")


table<-wahlergebnisse %>% 
  mutate(turn_out=((wahlende/wahlberechtigte_insgesamt)*100)) %>% 
  select(bezeichnung, turn_out) 

table2<-wahlergebnisse %>% 
  group_by(bezeichnung) %>% 
  summarise(
    across(c(spd, cdu, die_linke, fdp, grune, af_d, freie_wahler, odp, piraten, volt_hamburg, di_b, menschliche_welt, sedat_ayhan, sldp), .fns = sum, na_rm=TRUE), 
    .groups="drop"
  ) %>% 
  select(bezeichnung, af_d) %>% 
  left_join(table)


combined <-stadtteil_profil %>% 
  select( stadtteil, bevolkerung, bevolkerung_mit_migrations_hintergrund, anteil_der_bevolkerung_mit_migrations_hintergrund_in_percent) %>% 
  mutate(mig_ratio=((bevolkerung/bevolkerung_mit_migrations_hintergrund)*anteil_der_bevolkerung_mit_migrations_hintergrund_in_percent)) %>% 
  select(stadtteil, mig_ratio) %>% 
  left_join(table, by=c("stadtteil" = "bezeichnung"))

colnames(combined)[colnames(combined)=="af_d"]<-"afd"


if (
  assert_that(
    has_name(combined, "stadtteil"), msg = "Spalte 'stadtteil' fehlt"
  ) &
  assert_that(
    has_name(combined, "mig_ratio"), msg = "Spalte 'mig_ratio' fehlt"
  ) &
  assert_that(
    has_name(combined, "afd"), msg = "Spalte 'afd' fehlt"
  ) &
  assert_that(
    has_name(combined, "turn_out"), msg = "Spalte 'turn_out' fehlt"
  ) &
  assert_that(
    openssl::md5(paste(combined$stadtteil, collapse = ", ")) == "072ab9abd1f677ded727744ce0fc9f42",
    msg = "Spalte 'stadtteil' enthält einen Fehler"
  ) &
  assert_that(
    openssl::md5(paste(combined$afd, collapse = ", ")) == "9e37002645e55b6bb397622eb8984e21",
    msg = "Spalte 'afd' enthält einen Fehler"
  ) &
  assert_that(
    openssl::md5(paste(combined$mig_ratio, collapse = ", ")) == "222086dd76fcbefb0cdce33ca561ae10",
    msg = "Spalte 'mig_ratio' enthält einen Fehler"
  ) &
  assert_that(
    openssl::md5(paste(combined$turn_out, collapse = ", ")) == "5f4281dded9968151702c6533fba4fec",
    msg = "Spalte 'turn_out' fehlt"
  )
) {
  writeLines("10/10 Points. Congrats!")
}
