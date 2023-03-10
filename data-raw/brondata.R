## code to prepare `brondata` dataset
suppressPackageStartupMessages(library(openxlsx))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(reshape2))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(akima))
suppressPackageStartupMessages(library(tictoc))

# Constanten
archetypes <- c("hV","pV","kV","hVz","kVz","V","Vz","aVz") # 1-8
bronnen <- c("grfr", "ov", "wn") # 1-3
type_percelen <- c("ref", "owd", "dd") # 1-3 Opmerking: definieer ook "scenarios"
winterpeilen <- c("gelijk aan zomerpeil", "10 cm beneden zomerpeil", "20 cm beneden zomerpeil") # 1-4
variabelen <- c("CO2-uitstoot.min", "CO2-uitstoot.mediaan", "CO2-uitstoot.max") # 1-3
kwel_scenarios <- c("lichte kwel", "lichte wegzijging") # 1-2 Opmerking: Gegevens mbt kwel zijn alleen beschikbaar in de Overijssel dataset ("bron"="ov")
scenarios <- c("medium_peil", "hoog_peil") # 1-2 Opmerking: alleen van toepassing bij drukdrainage ("type_perceel"="dd")
keys <- c("archetype", "bron", "type_perceel", "winterpeil", "variabele", "kwel_scenario", "scenario")
nr_archetype <- 1:length(archetypes); names(nr_archetype) <- archetypes
nr_bron <- 1:length(bronnen); names(nr_bron) <- bronnen
nr_type_perceel <- 1:length(type_percelen); names(nr_type_perceel) <- type_percelen
nr_winterpeil <- 1:length(winterpeilen); names(nr_winterpeil) <- winterpeilen
nr_variabele <- 1:length(variabelen); names(nr_variabele) <- variabelen
nr_kwel_scenario <- 1:length(kwel_scenarios); names(nr_kwel_scenario) <- kwel_scenarios
nr_scenario <- 1:length(scenarios); names(nr_scenario ) <- scenarios
nr_key <- 1:length(keys); names(nr_key ) <- keys

# Lees bron gegevens uit xlsx files en converteer naar tidy format.
data_raw_fldr <- "data-raw"
bron_grfr <-
  file.path(data_raw_fldr, "uitstootcijfers_friesland_groningen_definitief-1.xlsx") %>%
  openxlsx::read.xlsx(sheet = "Brondata")
bron_grfr$kwel_scenario <- NA
bron_grfr$bron <- "grfr"
bron_grfr$X10 <- NULL
bron_ov <-  file.path(data_raw_fldr, "uitstootcijfers_overijssel_definitief.xlsx") %>%
  openxlsx::read.xlsx(sheet = "Brondata")
bron_ov$bron <- "ov"
bron_wn <-  file.path(data_raw_fldr, "uitstootcijfers_west-nederland_definitief.xlsx") %>%
  openxlsx::read.xlsx(sheet = "Brondata")
bron_wn$kwel_scenario <- NA
bron_wn$bron <- "wn"
bron_grfr %<>% dplyr::select(sort(names(.)))
bron_ov %<>% dplyr::select(sort(names(.)))
bron_wn %<>% dplyr::select(sort(names(.)))
brdata <- rbind(bron_grfr, bron_ov, bron_wn)
brondata <- reshape2::melt(brdata, measure.vars=c("CO2-uitstoot.max", "CO2-uitstoot.mediaan", "CO2-uitstoot.min")) %>%
  dplyr::rename( "CO2uitstoot"= "value") %>% dplyr::rename( "variabele"= "variable") %>%  dplyr::mutate_at("variabele", as.character) %>%
  dplyr::mutate(winterpeil=if_else(winterpeil=="is gelijk aan zomerpeil", "gelijk aan zomerpeil", winterpeil)) %>%
  tidyr::replace_na(list(kwel_scenario="-", scenario="-")) %>% tidyr::drop_na(CO2uitstoot)

# Maak id's
df_archetypes <-
  data.frame(archetype_id = 1:length(archetypes),
             archetype = archetypes)
df_bronnen <-
  data.frame(bron_id = 1:length(bronnen), bron = bronnen)
df_type_percelen <-
  data.frame(type_perceel_id = 1:length(type_percelen),
             type_perceel = type_percelen)
df_winterpeilen <-
  data.frame(winterpeil_id = 1:length(winterpeilen),
             winterpeil = winterpeilen)
df_variabelen <-
  data.frame(variabele_id = 1:length(variabelen),
             variabele = variabelen)
df_kwel_scenarios <-
  data.frame(kwel_scenario_id = 1:length(kwel_scenarios),
             kwel_scenario = kwel_scenarios)
df_scenarios <-
  data.frame(scenario_id = 1:length(scenarios),
             scenario = scenarios)
x <-
  brondata %>% dplyr::select(-c(drooglegging, CO2uitstoot, slootafstand)) %>% dplyr::distinct()

x %<>% dplyr::full_join(df_archetypes)
x %<>% dplyr::full_join(df_bronnen)
x %<>% dplyr::full_join(df_type_percelen)
x %<>% dplyr::full_join(df_winterpeilen)
x %<>% dplyr::full_join(df_variabelen)
x %<>% dplyr::full_join(df_kwel_scenarios)
x %<>% dplyr::full_join(df_scenarios)
x %<>% tidyr::replace_na(list(kwel_scenario_id=0, scenario_id=0))
#x%<>% dplyr::rename("type_perceel"="type_percelen", "type_perceel_id"="type_percelen_id")
x %<>% dplyr::mutate_at(vars(ends_with("_id")), as.character) %>% dplyr::mutate(
  id = paste0(
    archetype_id,
    bron_id,
    type_perceel_id,
    winterpeil_id,
    variabele_id,
    kwel_scenario_id,
    scenario_id
  )
)

# Vul de brondata aan met deze id's.
brondata %<>% dplyr::full_join(x)

usethis::use_data(brondata, overwrite = TRUE, internal=TRUE)
