# read in XML-file from SpiGes
# 21.2.2024, Hedwig Prey
# R script under GPL-3 license, see LICENSE.md file
# Copyright Owner: Fachstelle für Statistik Kanton St.Gallen
# Experimental R script, use at your own risk.

library(conflicted)
library(tidyverse)
library(xml2)

conflicts_prefer(dplyr::filter)

# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# get file and read in xml ====
# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

input_file <- "https://dam-api.bfs.admin.ch/hub/api/dam/assets/32129227/master"
xsd_file <- "https://dam-api.bfs.admin.ch/hub/api/dam/assets/32129176/master"

xml0 <- read_xml(input_file)
schema_file <- read_xml(xsd_file)

# validate input file
xml_validate(xml0, schema_file)

# extract all namespaces from the xml-document
xml_ns(xml0)

# Rename default namespaces to informative names:
ns <- xml_ns_rename(xml_ns(xml0), d1 = "spiges", xsi = "xsi")

# Now we can pass ns to other xml function to use fully qualified names
level0 <- xml_children(xml0)
level1 <- xml_children(xml_children(xml0))
level2 <- xml_children(xml_children(xml_children(xml0)))

# show nodes by level if desired
# xml_name(xml0, ns)
# xml_name(level0, ns)
# xml_name(level1, ns)
# xml_name(level2, ns)

# separate all nodesets
spital <- xml_find_all(xml0, "//spiges:Unternehmen", ns)
standorte <- xml_find_all(xml0, "//spiges:Standort", ns)
faelle <- xml_find_all(xml0, "//spiges:Fall", ns)
admin <- xml_find_all(xml0, "//spiges:Administratives", ns)
neugeb <- xml_find_all(xml0, "//spiges:Neugeborene", ns)
psych <- xml_find_all(xml0, "//spiges:Psychiatrie", ns)
ktrfall <- xml_find_all(xml0, "//spiges:KostentraegerFall", ns)
diagnose <- xml_find_all(xml0, "//spiges:Diagnose", ns)
behandlung <- xml_find_all(xml0, "//spiges:Behandlung", ns)
operierende <- xml_find_all(xml0, "//spiges:Operierende", ns)
medikamente <- xml_find_all(xml0, "//spiges:Medikament", ns)
bewegung <- xml_find_all(xml0, "//spiges:Patientenbewegung", ns)
rechnung <- xml_find_all(xml0, "//spiges:Rechnung", ns)
ktrunt <- xml_find_all(xml0, "//spiges:KostentraegerUnternehmen", ns)
ktrstao <- xml_find_all(xml0, "//spiges:KostentraegerStandort", ns)
fallkantonsdaten <- xml_find_all(xml0, "//spiges:Kantonsdaten", ns)

# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# 1: Administratives ====
# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

# full list of variables
l.admin <- c('burnr_gesv','abc_fall','geschlecht','alter','alter_U1',
             'wohnort_medstat','wohnkanton','wohnland','nationalitaet',
             'eintrittsdatum','eintritt_aufenthalt','eintrittsart','einw_instanz',
             'liegeklasse','versicherungsklasse','admin_urlaub','chlz',
             'aufenthalt_ips','beatmung','schwere_score','art_score',
             'nems','aufenthalt_imc','aufwand_imc','hauptleistungsstelle',
             'grundversicherung','tarif','austrittsdatum','austrittsentscheid',
             'austritt_aufenthalt','austritt_behandlung')

# create data frame with first three columns of id-variables
df.admin_id <- data.frame(
  ent_id = xml_attr(xml_parent(xml_parent(xml_parent(admin))), "ent_id"),
  burnr = xml_attr(xml_parent(xml_parent(admin)), "burnr"),
  fall_id = xml_attr(xml_parent(admin), "fall_id"))

# add rest of variables with data filled by xml
# Variables not delivered in the xml-file are created as NA-columns
df.admin <- dplyr::bind_cols(df.admin_id, purrr::map(l.admin, ~ df.admin_id |>
                                                       mutate(!!(.x) := xml_attr(admin, .x), .keep = "none")))

# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# 2: Neugeborene ====
# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

# full list of variables
l.neugeb <- c('geburtszeit','vitalstatus','mehrling','geburtsrang',
              'geburtsgewicht','laenge','missbildungen','fall_id_mutter',
              'gestationsalter1','gestationsalter2','vorh_schwanger',
              'vorh_lebendgeburten','vorh_fehlgeburten','vorh_abbrueche',
              'aufnahmegewicht','kopfumfang')

# create data frame with first three columns of id-variables
df.neugeb_id <- data.frame(
  ent_id = xml_attr(xml_parent(xml_parent(xml_parent(neugeb))), "ent_id"),
  burnr = xml_attr(xml_parent(xml_parent(neugeb)), "burnr"),
  fall_id = xml_attr(xml_parent(neugeb), "fall_id") )

# add rest of variables with data filled by xml
# Variables not delivered in the xml-file are created as NA-columns
df.neugeb <- dplyr::bind_cols(df.neugeb_id, purrr::map(l.neugeb, ~ df.neugeb_id |>
                                                         mutate(!!(.x) := xml_attr(neugeb, .x), .keep = "none")))

# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# 3: Psychiatrie ====
# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

# full list of variables
l.psych <- c('psy_zivilstand','psy_eintritt_aufenthalt','psy_eintritt_teilzeit',
             'psy_eintritt_vollzeit','psy_eintritt_arbeitslos','psy_eintritt_hausarbeit',
             'psy_eintritt_ausbildung','psy_eintritt_reha','psy_eintritt_rente',
             'psy_eintritt_gesch_arbeit','psy_eintritt_unbekannt','psy_schulbildung',
             'psy_einweisende_instanz','psy_fu','psy_behandlung',
             'psy_pp_neuroleptika','psy_pp_depotneuroleptika','psy_pp_antidepressiva',
             'psy_pp_tranquilizer','psy_pp_hypnotika','psy_pp_antiepileptika',
             'psy_pp_lithium','psy_pp_substitution','psy_pp_suchtaversion',
             'psy_pp_antiparkinson','psy_pp_andere','psy_pp_koerper_medi',
             'psy_entsch_austritt','psy_austritt_aufenthalt',
             'psy_austritt_behandlung','psy_behandlungsbereich')

# create data frame with first three columns of id-variables
df.psych_id <- data.frame(
  ent_id = xml_attr(xml_parent(xml_parent(xml_parent(psych))), "ent_id"),
  burnr = xml_attr(xml_parent(xml_parent(psych)), "burnr"),
  fall_id = xml_attr(xml_parent(psych), "fall_id")
)

# add rest of variables with data from xml (vars not delivered = NA)
df.psych <- dplyr::bind_cols(df.psych_id,
                             purrr::map(l.psych, ~ df.psych_id |>
                                          mutate(!!(.x) := xml_attr(psych, .x), .keep = "none")))

# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# 4: Fallkosten ====
# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

# full list of variables "ktr" (applies to 'Fall', 'Standort', 'Unternehmen' )
l.ktr <- c('ktr_typ','ktr_beschr','ktr_60','ktr_61','ktr_62','ktr_65',
           'ktr_66','ktr_68','ktr_69','ktr_697','ktr_4001','ktr_4002',
           'ktr_4012','ktr_4011','ktr_40_rest','ktr_4051','ktr_4052',
           'ktr_3801','ktr_3802','ktr_3811','ktr_3812','ktr_480','ktr_485',
           'ktr_486','ktr_10','ktr_20','ktr_21','ktr_23','ktr_24','ktr_25',
           'ktr_26','ktr_27','ktr_28','ktr_29','ktr_30','ktr_31_1_5',
           'ktr_31_6a1','ktr_31_6a2','ktr_31_6b1','ktr_31_6b2','ktr_31_6b3',
           'ktr_31_6b4','ktr_31_6b5','ktr_32','ktr_33','ktr_34','ktr_35',
           'ktr_36','ktr_38','ktr_39','ktr_40','ktr_41','ktr_42','ktr_43',
           'ktr_44','ktr_45','ktr_77','ktr_nicht_pb','ktr_10_ank','ktr_20_ank',
           'ktr_21_ank','ktr_23_ank','ktr_24_ank','ktr_25_ank','ktr_26_ank',
           'ktr_27_ank','ktr_28_ank','ktr_29_ank','ktr_30_ank','ktr_31_1_5_ank',
           'ktr_31_6a1_ank','ktr_31_6a2_ank','ktr_31_6b1_ank','ktr_31_6b2_ank',
           'ktr_31_6b3_ank','ktr_31_6b4_ank','ktr_31_6b5_ank','ktr_32_ank',
           'ktr_33_ank','ktr_34_ank','ktr_35_ank','ktr_36_ank','ktr_38_ank',
           'ktr_39_ank','ktr_40_ank','ktr_41_ank','ktr_42_ank','ktr_43_ank',
           'ktr_44_ank','ktr_45_ank','ktr_77_ank','ktr_44_vkl', 'ktr_44_rekole', 'ktr_kosten_65',
           'ktr_le_ambulant','ktr_methodik')

# create data frame with first three columns of id-variables
df.ktrfall_id <- data.frame(
  ent_id = xml_attr(xml_parent(xml_parent(xml_parent(ktrfall))), "ent_id"),
  burnr = xml_attr(xml_parent(xml_parent(ktrfall)), "burnr"),
  fall_id = xml_attr(xml_parent(ktrfall), "fall_id")
)

# add rest of variables with data from xml (vars not delivered = NA)
df.ktrfall <- dplyr::bind_cols(df.ktrfall_id,
                               purrr::map(l.ktr, ~ df.ktrfall_id |>
                                            mutate(!!(.x) := xml_attr(ktrfall, .x), .keep = "none")))


# admin, neugeb, psych, and ktrFall data can be joined if desired
# df.fall <- df.admin |>
#   left_join(df.neugeb, by = c("ent_id", "burnr", "fall_id")) |>
#   left_join(df.psych, by = c("ent_id", "burnr", "fall_id")) |>
#   left_join(df.ktrfall, by = c("ent_id", "burnr", "fall_id"))


# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# 5: Diagnosen ====
# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

# full list of variables
l.diagnose <- c('diagnose_id','diagnose_kode','diagnose_seitigkeit',
                'diagnose_poa','diagnose_zusatz')

# create data frame with first three columns of id-variables. Because there can
# be multiple lines per case, 'xml_find_first' is necessary to identify IDs
df.diagnose_id <- data.frame(
  ent_id = xml_attr(xml_find_first(xml_find_first(xml_find_first(diagnose, "./parent::*"), "./parent::*"), "./parent::*"), "ent_id"),
  burnr = xml_attr(xml_find_first(xml_find_first(diagnose, "./parent::*"), "./parent::*"), "burnr"),
  fall_id = xml_attr(xml_find_first(diagnose, "./parent::*"), "fall_id"))

# add rest of variables with data from xml (vars not delivered = NA)
df.diagnose <- dplyr::bind_cols(df.diagnose_id,
                                purrr::map(l.diagnose, ~ df.diagnose_id |>
                                             mutate(!!(.x) := xml_attr(diagnose, .x), .keep = "none")))

# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# 6: Behandlungen ====
# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

# full list of variables
l.behandlung <- c('behandlung_id','behandlung_chop','behandlung_seitigkeit',
                  'behandlung_beginn','behandlung_auswaerts','behandlung_bur')

# create data frame with first three columns of id-variables. Because there can
# be multiple lines per case, 'xml_find_first' is necessary to identify IDs
df.behandlung_id <- data.frame(
  ent_id = xml_attr(xml_find_first(xml_find_first(xml_find_first(behandlung, "./parent::*"), "./parent::*"), "./parent::*"), "ent_id"),
  burnr = xml_attr(xml_find_first(xml_find_first(behandlung, "./parent::*"), "./parent::*"), "burnr"),
  fall_id = xml_attr(xml_find_first(behandlung, "./parent::*"), "fall_id"))

# add rest of variables with data from xml (vars not delivered = NA)
df.behandlung <- dplyr::bind_cols(df.behandlung_id,
                                  purrr::map(l.behandlung, ~ df.behandlung_id |>
                                               mutate(!!(.x) := xml_attr(behandlung, .x), .keep = "none")))

# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# 7: Operierende ====
# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

# full list of variables
l.operierende <- c('op_gln','op_liste','op_rolle')

# create data frame with first four columns of id-variables. Because there can
# be multiple lines per case, 'xml_find_first' is necessary to identify IDs
df.operierende_id <- data.frame(
  ent_id = xml_attr(xml_find_first(xml_find_first(xml_find_first(
    xml_find_first(operierende,"./parent::*"),"./parent::*"),"./parent::*"),"./parent::*"),"ent_id"),
  burnr = xml_attr(xml_find_first(xml_find_first(
    xml_find_first(operierende,"./parent::*"),"./parent::*"),"./parent::*"),"burnr"),
  fall_id = xml_attr(xml_find_first(
    xml_find_first(operierende,"./parent::*"),"./parent::*"),"fall_id"),
  behandlung_id = xml_attr(xml_find_first(operierende, "./parent::*"), "behandlung_id"))

# add rest of variables with data from xml (vars not delivered = NA)
df.operierende <- dplyr::bind_cols(df.operierende_id,
                                   purrr::map(l.operierende, ~ df.operierende_id |>
                                                mutate(!!(.x) := xml_attr(operierende, .x), .keep = "none")))

# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# 8: Medikamente ====
# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

# full list of variables
l.medikamente <- c('medi_id','medi_atc','medi_zusatz','medi_verabreichungsart',
                   'medi_dosis','medi_einheit')

# create data frame with first three columns of id-variables. Because there can
# be multiple lines per case, 'xml_find_first' is necessary to identify IDs
df.medikamente_id <- data.frame(
  ent_id = xml_attr(xml_find_first(xml_find_first(xml_find_first(medikamente, "./parent::*"), "./parent::*"), "./parent::*"), "ent_id"),
  burnr = xml_attr(xml_find_first(xml_find_first(medikamente, "./parent::*"), "./parent::*"), "burnr"),
  fall_id = xml_attr(xml_find_first(medikamente, "./parent::*"), "fall_id"))

# add rest of variables with data from xml (vars not delivered = NA)
df.medikamente <- dplyr::bind_cols(df.medikamente_id,
                                   purrr::map(l.medikamente, ~ df.medikamente_id |>
                                                mutate(!!(.x) := xml_attr(medikamente, .x), .keep = "none")))

# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# 9: Rechnung ====
# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

# full list of variables
l.rechnung <- c('rech_id','rech_kostentraeger','rech_versicherer',
                'rech_unfallnr','rech_betrag','rech_tariftyp','rech_tarifcode',
                'rech_ext_faktor','rech_basispreis','rech_einheit','rech_menge')

# create data frame with first three columns of id-variables. Because there can
# be multiple lines per case, 'xml_find_first' is necessary to identify IDs
df.rechnung_id <- data.frame(
  ent_id = xml_attr(xml_find_first(xml_find_first(xml_find_first(rechnung, "./parent::*"), "./parent::*"), "./parent::*"), "ent_id"),
  burnr = xml_attr(xml_find_first(xml_find_first(rechnung, "./parent::*"), "./parent::*"), "burnr"),
  fall_id = xml_attr(xml_find_first(rechnung, "./parent::*"), "fall_id"))

# add rest of variables with data from xml (vars not delivered = NA)
df.rechnung <- dplyr::bind_cols(df.rechnung_id,
                                purrr::map(l.rechnung, ~ df.rechnung_id |>
                                             mutate(!!(.x) := xml_attr(rechnung, .x), .keep = "none")))

# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# 10: Patientenbewegung ====
# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

# full list of variables
l.bewegung <- c('episode_id','episode_beginn','episode_ende','episode_art',
                'burnr_episode','wiedereintritt_aufenthalt','grund_wiedereintritt')

# create data frame with first three columns of id-variables. Because there can
# be multiple lines per case, 'xml_find_first' is necessary to identify IDs
df.bewegung_id <- data.frame(
  ent_id = xml_attr(xml_find_first(xml_find_first(xml_find_first(bewegung, "./parent::*"), "./parent::*"), "./parent::*"), "ent_id"),
  burnr = xml_attr(xml_find_first(xml_find_first(bewegung, "./parent::*"), "./parent::*"), "burnr"),
  fall_id = xml_attr(xml_find_first(bewegung, "./parent::*"), "fall_id"))

# add rest of variables with data from xml (vars not delivered = NA)
df.bewegung <- dplyr::bind_cols(df.bewegung_id,
                                purrr::map(l.bewegung, ~ df.bewegung_id |>
                                             mutate(!!(.x) := xml_attr(bewegung, .x), .keep = "none")))

# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# 11: Kostentraeger Standort ====
# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

# full list of variables: l.ktr

# create data frame with first three columns of id-variables. Because there can
# be multiple lines per case, 'xml_find_first' is necessary to identify IDs
df.ktrstao_id <- data.frame(
  ent_id = xml_attr(xml_find_first(xml_find_first(ktrstao, "./parent::*"), "./parent::*"), "ent_id"),
  burnr = xml_attr(xml_find_first(ktrstao, "./parent::*"), "burnr"))

# add rest of variables with data from xml (vars not delivered = NA)
df.ktrstao <- dplyr::bind_cols(df.ktrstao_id,
                               purrr::map(l.ktr, ~ df.ktrstao_id |>
                                            mutate(!!(.x) := xml_attr(ktrstao, .x), .keep = "none")))

# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# 12: Kostentraeger Unternehmen ====
# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

# full list of variables: l.ktr

# create data frame with first three columns of id-variables. Because there can
# be multiple lines per case, 'xml_find_first' is necessary to identify IDs
df.ktrunt_id <- data.frame(
  ent_id = xml_attr(xml_find_first(ktrunt, "./parent::*"), "ent_id"))

# add rest of variables with data from xml (vars not delivered = NA)
df.ktrunt <- dplyr::bind_cols(df.ktrunt_id,
                              purrr::map(l.ktr, ~ df.ktrunt_id |>
                                           mutate(!!(.x) := xml_attr(ktrunt, .x), .keep = "none")))


rm(df.admin_id, df.neugeb_id, df.psych_id, df.diagnose_id, df.behandlung_id,
   df.ktrfall_id, df.operierende_id, df.medikamente_id, df.rechnung_id,
   df.bewegung_id, df.ktrstao_id, df.ktrunt_id)
