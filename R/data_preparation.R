# ==============================================================================
# VERÐBÓLGUSKÝRSLA - DATA PREPARATION
# ==============================================================================
#
# Byggir tvö .rds gagnasöfn fyrir Power BI skýrsluna:
#   data/final_data.rds - allt nema spár (sjá lista í kafla 16.0.0)
#   data/spar.rds       - verðbólguspá VR og spár annarra (kafli 16.0.0 SPÁR)
#
# ------------------------------------------------------------------------------
# UM HAGSTOFU-VISTAÐAR FYRIRSPURNIR ("saved queries") - LESIST EF EITTHVAÐ BROTNAR
# ------------------------------------------------------------------------------
# Flest gögnin eru sótt með slóðum á forminu:
#
#   https://px.hagstofa.is:443/pxis/sq/<guid>
#
# Þetta eru "vistaðar fyrirspurnir" (saved queries) í PxWeb. Þær eru þægilegar
# (ein slóð = ein tilbúin CSV-tafla) EN ÞÆR ERU BROTHÆTTAR: Hagstofan endurnýjar
# vistaðar fyrirspurnir þegar töflum er breytt, þegar flokkunarkerfi er uppfært
# (t.d. COICOP-breytingin um áramót 2025/2026) eða þegar tafla er færð í
# "Eldra efni". Þá skilar slóðin engu, tómri töflu eða villu og kóðinn hér
# brotnar - oftast í set_names() eða janitor::clean_names() því fjöldi eða heiti
# stuðla passar ekki lengur.
#
# ÞESS VEGNA: hverri slóð hér fyrir neðan fylgir athugasemd með
#   * PX-töfluauðkenni (t.d. VIS01300) og heiti töflunnar,
#   * SLÓÐ Á VEFVIÐMÓTIÐ sem má opna í vafra til að skoða/velja gögnin,
#   * hvaða stuðla (columns) fyrirspurnin skilar - í réttri röð, með
#     upprunalegum íslenskum heitum úr CSV-skránni,
#   * hvaða breytur kóðinn gefur þeim eftir set_names()/clean_names().
#
# AÐ ENDURHEIMTA BROTNA SLÓÐ:
#   1. Opnaðu vefviðmótsslóðina úr athugasemdinni í vafra.
#   2. Veldu sömu stuðla/liði og listaðir eru í athugasemdinni (og "allt" í tíma).
#   3. Ýttu á "Áfram"/"Continue", veldu vistun sem CSV með semíkommu (CSV 1).
#      Undir "Vista fyrirspurn" / "Save query" fæst ný sq/<guid> slóð.
#   4. Skiptu gömlu slóðinni út fyrir þá nýju. Ef stuðlafjöldi/röð er sú sama
#      þarf engu öðru að breyta; annars uppfærðu set_names() í samræmi.
#
# ATH. um snið CSV-skránna frá Hagstofu:
#   * read_csv2() er notað því skrárnar eru semíkommu-skildar með kommu sem
#     tugabroti.
#   * Vantandi gildi eru "." (eða ".." í eldri töflum) - þess vegna na = "."
#     í mörgum köllum hér.
#   * Mánuðir koma sem "2026M01" -> fix_date() / make_date(str_sub(...)) breyta
#     þeim í dagsetningu (fyrsti dagur mánaðar).
#   * Fyrsta lína CSV-skránna getur innihaldið endurtekið stuðlaheiti sem gildi
#     (t.d. "Útgjaldaflokkur" eða "Liður" í flokkastuðlinum). Þær línur hafa
#     ekkert tölugildi og hverfa í drop_na().
#   * Sumar töflur skila vísitölu *tífaldaðri* (t.d. 1234 = 123,4) og vogum
#     *tíuþúsundfölduðum* (10000 = 100%). Þar sem svo er stendur value / 10
#     eða vog / 10000 í kóðanum - sjá athugasemd við hverja töflu.
#
# YFIRLIT YFIR PX-TÖFLUR SEM ERU NOTAÐAR HÉR:
#   VIS01000 Vísitala neysluverðs og breytingar, grunnur 1988=100
#   VIS01101 Vísitala neysluverðs eftir eðli og uppruna          (ný flokkun)
#   VIS01102 Vísitala neysluverðs eftir eðli og uppruna 1997-2025 (eldri, Eldra efni)
#   VIS01300 Undirvísitölur, breytingar og áhrif                 (ný flokkun)
#   VIS01304 Undirvísitölur neysluverðs, grunnur 2008=100, 2008-2025 (eldri)
#   VIS01305 Vogir fyrir undirvísitölur neysluverðs 1992-2024    (eldri vogir)
#   VIS01306 Vogir fyrir undirvísitölur neysluverðs              (nýjar vogir)
#
# Rót vefviðmótsins fyrir allar VNV-töflur (ef slóð hér að neðan brotnar líka,
# byrjaðu hér og flettu þig niður í "Vísitala neysluverðs"):
#   https://px.hagstofa.is/pxis/pxweb/is/Efnahagur/Efnahagur__visitolur__1_vnv
#
# Aðrar gagnaheimildir en Hagstofan:
#   * Eurostat HICP  - kafli 6.0.0 (REST API, sjá athugasemd þar)
#   * Seðlabankinn   - gengi (kafli 9.0.0, XML tímaraðir)
#   * Seðlabankinn   - vextir, útlán og inngrip (kaflar 7.3-10.0, sótt með
#     R/sedlabanki_data.R)
#   * Excel-skrár í data/ - verðbólguvæntingar og spár annarra (þessar eru
#     uppfærðar handvirkt og eru viðkvæmar fyrir því að raðir færist til, því
#     þær eru lesnar með slice() á raðnúmerum)
# ==============================================================================

# 1.0.0 SETUP ----

# Must come before any library() call: points .libPaths() at the machine-wide
# package library and loads credentials (DB plus FRED_API_KEY, used by fredr
# below) from a real local file rather than through HOME/~. See
# C:/R/rwd_setup.R.
# source("C:/R/rwd_setup.R")

library(tidyverse)
library(eurostat)
library(XML)
library(httr)
library(openxlsx)
library(zoo)
library(rvest)
library(readxl)
library(here)
library(fredr)
library(modeltime)
library(tidymodels)
library(httr2)
library(hicp)

# VARÚÐ: harðkóðuð slóð bundin við eina vél/notanda ("hd"). Allar skráaslóðir
# neðar (data/...) eru afstæðar við þetta. Á annarri vél þarf að breyta þessu
# (eða athugasemda það út og treysta á here::here() / RStudio-verkefnisrótina).
# setwd("c:/Users/hd/Rwd/verdbolguskyrsla")

# data/lond.csv: iso_3, land (+ fleiri stuðlar sem eru ekki notaðir hér)
lond_tbl <- read_csv("data/lond.csv") |>
  select(iso_3, land)

# 1.1.0 Functions ----

# Hagstofu-töflur skila mánuði sem "2026M01" í stuðli sem heitir "Mánuður" og
# verður "manudur" eftir janitor::clean_names(). Hér er honum breytt í
# dagsetningu (fyrsti dagur mánaðarins).
# ATH. krefst þess að clean_names() hafi verið kallað fyrst, þ.e. að stuðullinn
# heiti nákvæmlega "manudur". Ef Hagstofan endurnefnir tímastuðulinn brotnar
# þetta hér.
fix_date <- function(data) {
  data %>%
    mutate(date = make_date(str_sub(manudur, 1, 4), str_sub(manudur, 6, 7)))
}

# 1.2.0 Other ----

# Sýnitímabil skýrslunnar: 5 ár aftur í tímann frá yfirstandandi mánuði.
date_from <- floor_date(today() - years(5), "month")

# Excel-skrár sem eru uppfærðar handvirkt (sjá athugasemdir í köflum 7-11)
# ATH. úr verdbolguskyrsla_data.xlsx er nú aðeins blaðið "spar" uppfært
# handvirkt; blaðið "vextir" er lesið sem frosin saga. Vextir, útlán og inngrip
# eru sótt beint til Seðlabankans með föllum úr R/sedlabanki_data.R.
data_path <- "data/verdbolguskyrsla_data.xlsx"
exp_path <- "data/Verdbolguvaentingar-a-mismunandi-maelikvarda.xlsx"

source("R/sedlabanki_data.R")

# Hagstofan tók upp nýtt COICOP-flokkunarkerfi í janúar 2026. Eldri og nýrri
# tímaraðir eru því sóttar úr sitt hvorri töflu og skeytt saman við þessa
# dagsetningu (sjá kafla 2.3.3 og 5.3.0).
new_coicop_date_from <- "2026-01-01"

# 2.0.0 Verðbólga ----

# 2.1.0 Verðbólga með og án húsnæðis ----
print("Verðbólga með og án húsænðis")

# PX-tafla: VIS01000 "Vísitala neysluverðs og breytingar, grunnur 1988=100"
# Vefviðmót (opnaðu í vafra til að endurgera fyrirspurnina):
# https://px.hagstofa.is/pxis/pxweb/is/Efnahagur/Efnahagur__visitolur__1_vnv__1_vnv/VIS01000.px
#
# Stuðlar sem fyrirspurnin skilar (4 talsins, í þessari röð):
#   1 "Mánuður"              -> manudur              t.d. "2026M01"
#   2 "Vísitala"             -> visitala             tvö gildi:
#                                 "Vísitala neysluverðs"
#                                 "Vísitala neysluverðs án húsnæðis"
#   3 "Liður"                -> lidur                aðeins "Vísitala" (fast gildi,
#                                                    ekki notað - fellt út í select())
#   4 "Vísitala neysluverðs" -> visitala_neysluverds tölugildi vísitölunnar
#
# Ath. stuðull 2 og 4 heita svipað en eru ólíkir: 2 er FLOKKURINN (hvor
# vísitalan) og 4 er TALAN. Eftir set_names() verða þeir "flokkur" og "visitala".
# Valið í vefviðmótinu: liður = "Vísitala", vísitölur = báðar að ofan, allir mánuðir.
vnv_tbl <- read_csv2(
  "https://px.hagstofa.is:443/pxis/sq/6dd6d9a9-ff2f-4379-a78f-7fc800e09ea9"
) %>%
  janitor::clean_names()

# Hér verður vnv_tbl: date | flokkur | visitala | verdbolga (12m) | milli_manada (1m)
# flokkur tekur gildin "Verðbólga" og "Verðbólga án húsnæðis".
# Þessi tafla er notuð víða neðar (m.a. í kafla 3.4.0 og 5.x) sem uppspretta
# hinnar OPINBERU verðbólgu.
vnv_tbl <- vnv_tbl %>%
  fix_date() %>%
  # lidur (fast gildi "Vísitala") er felldur út hér
  select(date, visitala, visitala_neysluverds) %>%
  set_names("date", "flokkur", "visitala") %>%
  mutate(
    flokkur = ifelse(
      flokkur == "Vísitala neysluverðs",
      "Verðbólga",
      "Verðbólga án húsnæðis"
    )
  ) %>%
  group_by(flokkur) %>%
  mutate(verdbolga = visitala / lag(visitala, 12) - 1) %>%
  drop_na() %>%
  ungroup()


vnv_tbl <- vnv_tbl %>%
  arrange(date) %>%
  group_by(flokkur) %>%
  mutate(milli_manada = visitala / lag(visitala) - 1) %>%
  ungroup() %>%
  drop_na() |>
  filter(date >= date_from)

# Valuebox í Power BI: nýjasta 12m verðbólga, 1m breyting, "hraði" og breyting
# á hraða. Hraði = árstíðarjöfnuð 6 mánaða þróun á 12m hlaupandi meðaltali
# vísitölunnar, umreiknuð á árskvarða.
valuebox_verdbolga <- vnv_tbl %>%
  filter(flokkur == "Verðbólga") %>%
  arrange(date) %>%
  mutate(
    visital_ma = TTR::SMA(visitala, 12),
    hradi = ((visital_ma / lag(visital_ma, 6))^(1 / 6))^12 - 1,
    hradi_diff = hradi - lag(hradi)
  ) %>%
  filter(date == max(date)) %>%
  select(verdbolga, milli_manada, hradi, hradi_diff)

verdbolga_latest <- vnv_tbl |>
  filter(date == max(date), flokkur == "Verðbólga") |>
  pull(verdbolga)

# 2.2.0 Innlend og erlend verðbólga ----
# ÓNOTAÐ / AFTURKALLAÐ: byggði á VIS01102 (sama slóð og kafli 2.3.1 notar) og
# skipti útgjaldaflokkum í "Innflutt verðlag" / "Húsnæði" / "Innlent verðlag
# utan húsnæðis". Leyst af hólmi með kafla 2.3.0 (eðli og uppruna).
# Geymt til viðmiðunar - ekki uppfært fyrir nýja COICOP-flokkun.

# innlend_innflutt_old_tbl <- read_csv2(
#   "https://px.hagstofa.is:443/pxis/sq/efeec1b8-11a5-402c-b747-99fe8ab61bec",
#   na = "."
# ) %>%
#   set_names("date", "flokkur", "visitala", "weight") |>
#   drop_na()

# innlend_innflutt_tbl <- innlend_innflutt_tbl %>%
#   mutate(
#     # Laga gildi
#     visitala = visitala / 10,
#     weight = weight / 1000,

#     # Bý til dagsetningu
#     date = make_date(year = str_sub(date, 1, 4), month = str_sub(date, 6, 7)),

#     # Bý til nýja flokka
#     flokkur_2 = case_when(
#       str_detect(flokkur, "5|6|7|8|9") ~ "Innflutt verðlag",
#       str_detect(flokkur, "Húsnæði") ~ "Húsnæði",
#       str_detect(flokkur, "1|2|3|4|11|12") ~ "Innlent verðlag utan húsnæðis"
#     )
#   )

# innlend_innflutt_tbl <- innlend_innflutt_tbl %>%
#   drop_na() |>
#   group_by(date, flokkur_2) %>%
#   mutate(
#     new_weight = weight / sum(weight),
#     new_index = new_weight * visitala
#   ) %>%
#   summarise(index = sum(new_index)) %>%
#   group_by(flokkur_2) %>%
#   mutate(delta = index / lag(index, 12) - 1) %>%
#   drop_na(delta) %>%
#   ungroup()

# Frá nóvember 2022
# innlend_innflutt_nov_tbl <- innlend_innflutt_tbl %>%
#   filter(date >= date_from) %>%
#   group_by(flokkur_2) %>%
#   mutate(voxtur = index / index[1] - 1)

# 2.3.0 Verðbólga eftir eðli og uppruna ----

# 2.3.1 eldri ----
print("Verðbólga eftir eðli og uppruna - eldri")

# PX-tafla: VIS01102 "Vísitala neysluverðs eftir eðli og uppruna frá 1997-2025"
# (þetta er ELDRI flokkunin, komin í "Eldra efni" hjá Hagstofu - hún uppfærist
#  ekki lengur. Nýrri tímaröðin er sótt í kafla 2.3.2 úr VIS01101.)
# Vefviðmót:
# https://px.hagstofa.is/pxis/pxweb/is/Efnahagur/Efnahagur__visitolur__1_vnv__4_eldraefni/VIS01102.px
#
# Stuðlar sem fyrirspurnin skilar (4 talsins, í þessari röð):
#   1 "Mánuður"                          -> date    t.d. "1997M03"
#   2 "Útgjaldaflokkur"                  -> flokkur  sjá lista að neðan
#   3 "Vísitala, janúar 2008 = 100"      -> value    TÍFÖLDUÐ (þ.a.l. value / 10)
#   4 "Vægi, %"                          -> weight   ÞÚSUNDFALDAÐ (þ.a.l. / 1000)
#
# Gildi í stuðli 2 (útgjaldaflokkur) - eldri flokkun, númeruð 1-12 auk
# samtölulína sem byrja á "þar af:":
#   "Vísitala neysluverðs"
#   "1  Búvörur án grænmetis"
#   "2  Grænmeti (innlent og innflutt)"
#   "3  Aðrar innlendar mat- og drykkjarvörur"
#   "4  Aðrar innlendar vörur án grænmetis"
#   "5  Innfluttar mat- og drykkjarvörur"
#   "6  Nýr bíll og varahlutir"
#   "7  Bensín"
#   "8  Innfluttar vörur aðrar"
#   "9  Áfengi og tóbak"
#   "10  Húsnæði"
#   "11  Opinber þjónusta"
#   "12  Önnur þjónusta"
#   "þar af:  Búvörur og grænmeti (liðir 1 og 2)"
#   "þar af:  Dagvara"
#   "þar af:  Innfluttar vörur alls (liðir 5 til 9)"
#   "þar af:  Innfluttar vörur án áf. og tób. (liðir 5 til 8)"
#   "þar af:  Innlendar vörur og grænmeti (liðir 1 til 4)"
#   "þar af:  Innlendar vörur án búvöru og grænmetis (liðir 3 og 4)"
#   "þar af:  Kjarnavísitala 1 (VNV án búvöru, grænmetis, ávaxta og bensíns)"
#   "þar af:  Kjarnavísitala 2 (kjarnavísitala 1 án opinberrar þjónustu)"
#   "þar af:  Kjarnavísitala 3 (kjarnavísitala 2 án áhrifa vaxtabreytinga á húsnæðislið)"
#   "þar af:  Kjarnavísitala 4 (kjarnavísitala 2 án reiknaðrar húsaleigu)"
#   "þar af: Rekstur eigin bifreiðar"
# (auk línu þar sem gildið er sjálft "Útgjaldaflokkur" - hausleif, fellur í drop_na)
#
# ATH. drop_na() hér fellir út allar línur þar sem vísitala VANTAR (".") - m.a.
# "Vísitala neysluverðs" línuna sjálfa, sem er tóm í þessari töflu.
edli_og_uppruna_old_raw_tbl <- read_csv2(
  "https://px.hagstofa.is:443/pxis/sq/efeec1b8-11a5-402c-b747-99fe8ab61bec",
  na = "."
) %>%
  drop_na() |>
  set_names("date", "flokkur", "value", "weight") %>%
  mutate(
    value = value / 10,
    weight = weight / 1000
  ) %>%
  mutate(
    date = make_date(str_sub(date, 1, 4), str_sub(date, 6, 7))
  ) %>%
  drop_na(value)

# Endurflokkun: mörgu af ofangreindu er hent (verður "annad" og er síað út).
# Aðeins þessir fimm flokkar eru notaðir og heiti þeirra eru valin þannig að
# þau STEMMI VIÐ nýju flokkunina í kafla 2.3.2 svo hægt sé að skeyta röðunum
# saman í 2.3.3. Ef þú breytir heiti hér þarftu að breyta því þar líka.
edli_og_uppruna_old_tbl <- edli_og_uppruna_old_raw_tbl %>%
  mutate(
    flokkur = case_when(
      str_detect(flokkur, "Innlendar vörur og grænmeti") ~ "Innlendar vörur",
      str_detect(flokkur, "Innfluttar vörur alls") ~ "Innfluttar vörur",
      str_detect(flokkur, "Opinber þjónusta") ~ "Opinber þjónusta",
      str_detect(flokkur, "Önnur þjónusta") ~ "Önnur þjónusta",
      str_detect(flokkur, "10  Húsnæði") ~ "Húsnæði",
      TRUE ~ "annad"
    )
  ) %>%
  arrange(date, flokkur) %>%
  group_by(flokkur) %>%
  mutate(
    delta = value / lag(value, 12) - 1
    # weight = zoo::rollapply(
    #   weight,
    #   width = 12,
    #   FUN = mean,
    #   fill = NA,
    #   partial = FALSE,
    #   align = "right"
    # )
  ) %>%
  ungroup() %>%
  drop_na(delta) %>%
  # mutate(
  #   hlutdeild = delta * weight
  # ) |>
  filter(!flokkur == "annad") |>
  select(-weight)

# 2.3.2 ný útgáfa ----
print("Verðbólga eftir eðli og uppruna - ný útgáfa")

# PX-tafla: VIS01101 "Vísitala neysluverðs eftir eðli og uppruna" (NÝ flokkun,
# hefst 2025M01). Þetta er virka taflan - eldri röðin er í VIS01102 (kafli 2.3.1).
# Vefviðmót:
# https://px.hagstofa.is/pxis/pxweb/is/Efnahagur/Efnahagur__visitolur__1_vnv__3_greiningarvisitolur/VIS01101.px
#
# Stuðlar sem fyrirspurnin skilar (4 talsins, í þessari röð):
#   1 "Mánuður"          -> date     t.d. "2025M01"
#   2 "Útgjaldaflokkur"  -> flokkur  sjá lista að neðan
#   3 "Vísitala"         -> value    TÍFÖLDUÐ (þ.a.l. value / 10)
#   4 "Vægi, %"          -> weight   NOTAÐ ÓBREYTT hér (ekki deilt með 1000 eins
#                                    og í eldri töflunni - vogirnar eru aðeins
#                                    notaðar sem hlutfallslegar þyngdir í
#                                    vegnu meðaltali að neðan, svo kvarðinn
#                                    styttist út)
#
# Gildi í stuðli 2 (útgjaldaflokkur) - ný flokkun:
#   "Vísitala neysluverðs"
#   "Vörur" / "Þjónusta"                            (yfirflokkar)
#   "1a Varanlegar vörur"
#   "1b Hálfvaranlegar vörur"
#   "1c Óvaranlegar vörur"
#   "2a Innlendar vörur"
#   "2a-i  Innlendur matur og drykkir"
#   "2a-ii Aðrar innlendar vörur"
#   "2b Innfluttar vörur"
#   "2b-i  Innfluttur matur og drykkir"
#   "2b-ii Aðrar innfluttar vörur"
#   "2c Vörur í blönduðum flokkum"
#   "2c-i  Matur og drykkir í blönduðum flokkum"
#   "2c-ii Aðrar vörur í blönduðum flokkum"
#   "3 Búvörur" / "3 Dagvörur"
#   "4 Vörur án orku og matvöru"
#   "5a Ferðaþjónusta"
#   "5b Húsnæði"
#   "5c Opinber þjónusta"
#   "5d Önnur þjónusta"
#   "6 Þjónusta án húsnæðis"
#   "Kjarnavisitala_1 (VNV án búvöru, grænmetis, ávaxta og bensíns)"
#   "Kjarnavisitala_2  (Kjarnavísitala 1 án opinberrar þjónustu)"
#   "Kjarnavisitala_4 (kjarnavísitala 2 án reiknaðrar húsaleigu)"
#   "Kjarnavisitala_5 (VNV án orku og matvöru)"
# (auk hausleifar-línu með gildinu "Útgjaldaflokkur", fellur í drop_na)
edli_og_uppruna_new_raw_tbl <- read_csv2(
  "https://px.hagstofa.is:443/pxis/sq/53b28474-c147-41d6-b16e-a46605ac8626",
  na = "."
) |>
  drop_na() |>
  set_names("date", "flokkur", "value", "weight") %>%
  mutate(
    value = value / 10
    #weight = weight / 1000
  ) %>%
  mutate(
    date = make_date(str_sub(date, 1, 4), str_sub(date, 6, 7))
  ) %>%
  drop_na(value)

# Endurflokkun yfir í sömu heiti og eldri röðin (kafli 2.3.1) notar, svo hægt
# sé að skeyta þeim saman. Nokkrir nýir undirflokkar eru steyptir saman í einn
# (t.d. 5a+5b+5d -> "Ferðaþjónusta, húsnæði og önnur þjónusta"); þess vegna er
# tekið VEGIÐ MEÐALTAL vísitalna með vægi sem þyngd í summarise() að neðan.
edli_og_uppruna_new_tbl <- edli_og_uppruna_new_raw_tbl |>
  mutate(
    flokkur = case_when(
      str_detect(flokkur, "2a Innlendar vörur") ~ "Innlendar vörur",
      str_detect(flokkur, "2b Innfluttar vörur") ~ "Innfluttar vörur",
      str_detect(
        flokkur,
        "2c Vörur í blönduðum flokkum"
      ) ~ "Innlendar og innfluttar",
      str_detect(flokkur, "5c Opinber þjónusta") ~ "Opinber þjónusta",
      str_detect(
        flokkur,
        "5a Ferðaþjónusta|5b Húsnæði|5d Önnur þjónusta"
      ) ~ "Ferðaþjónusta, húsnæði og önnur þjónusta",
      str_detect(flokkur, "3 Búvörur") ~ "Búvörur",
      str_detect(
        flokkur,
        "4 Vörur án orku og matvöru"
      ) ~ "Vörur án orku og matvöru",
      TRUE ~ "annað"
    )
  ) |>
  arrange(date, flokkur) %>%
  group_by(flokkur, date) %>%
  summarise(value = sum(value * weight) / sum(weight))

edli_og_uppruna_new_tbl <- edli_og_uppruna_new_tbl |>
  group_by(flokkur) |>
  mutate(
    #value = mean(value),
    delta = value / lag(value, 12) - 1
    # weight = zoo::rollapply(
    #   weight,
    #   width = 12,
    #   FUN = mean,
    #   fill = NA,
    #   partial = TRUE,
    #   align = "right"
    # )
  ) %>%
  ungroup() %>%
  drop_na(delta) %>%
  filter(!flokkur == "annað")

# 2.3.3 sameina gögn ----
print("Verðbólga eftir eðli og uppruna - sameina")

# Skeytt saman við new_coicop_date_from (2026-01-01): eldri röðin fyrir þann
# tíma, nýja röðin eftir. Aðeins þeir flokkar sem eiga sér samsvörun í BÁÐUM
# röðum eru teknir með úr eldri gögnunum (flokkur %in% unique(...new...)) -
# annars kæmu göt í myndritin.
# Niðurstaða: date | flokkur | value (vísitala) | delta (12m breyting)
edli_og_uppruna_tbl <-
  bind_rows(
    edli_og_uppruna_old_tbl |>
      filter(
        flokkur %in% unique(edli_og_uppruna_new_tbl$flokkur),
        date < new_coicop_date_from
      ),
    edli_og_uppruna_new_tbl |> filter(date >= new_coicop_date_from)
  )

# Bæti við verðbólgunni
edli_og_uppruna_tbl <- edli_og_uppruna_tbl %>%
  left_join(
    vnv_tbl %>% filter(flokkur == "Verðbólga") %>% select(-c(visitala, flokkur))
  ) %>%
  drop_na() |>
  filter(date >= date_from)


# 2.4.0 Undirliggjandi verðbólga ----

# 2.4.1 eldri ----
print("Undirliggjandi verðbólga - eldri")

# PX-tafla: VIS01102 "Vísitala neysluverðs eftir eðli og uppruna frá 1997-2025"
# (sama tafla og í 2.3.1 en ÖNNUR fyrirspurn: hér eru kjarnavísitölurnar
#  valdar og lagðar út í BREIÐU formi - einn stuðull á hverja kjarnavísitölu.)
# Vefviðmót:
# https://px.hagstofa.is/pxis/pxweb/is/Efnahagur/Efnahagur__visitolur__1_vnv__4_eldraefni/VIS01102.px
#
# Stuðlar sem fyrirspurnin skilar (4 talsins, í þessari röð):
#   1 "Mánuður"                                                     -> date
#   2 "Vísitala, janúar 2008 = 100 þar af:  Kjarnavísitala 1
#      (VNV án búvöru, grænmetis, ávaxta og bensíns)"               -> "Kjarnavísitala 1"
#   3 "Vísitala, janúar 2008 = 100 þar af:  Kjarnavísitala 2
#      (kjarnavísitala 1 án opinberrar þjónustu)"                   -> "Kjarnavísitala 2"
#   4 "Vísitala, janúar 2008 = 100 þar af:  Kjarnavísitala 4
#      (kjarnavísitala 2 án reiknaðrar húsaleigu)"                  -> "Kjarnavísitala 4"
#
# ATH. hér er EKKI deilt með 10 - ólíkt fyrirspurninni í 2.3.1 skilar þessi
# vísitölunni á réttum kvarða. Kjarnavísitala 3 og 5 eru ekki með í eldri
# röðinni (5 kom fyrst með nýju flokkuninni, sjá 2.4.2).
# Til að endurgera: veldu liðina "þar af: Kjarnavísitala 1/2/4" undir
# "Vísitala, janúar 2008 = 100" og leggðu vísitölurnar út sem stuðla.
undirliggjandi_old_tbl <- read_csv2(
  "https://px.hagstofa.is:443/pxis/sq/8643c932-380e-4c83-889a-14be1ad99354",
  na = "."
) |>
  drop_na() |>
  set_names(
    "date",
    "Kjarnavísitala 1",
    "Kjarnavísitala 2",
    "Kjarnavísitala 4"
  ) |>
  mutate(date = make_date(str_sub(date, 1, 4), str_sub(date, 6, 7))) %>%
  pivot_longer(cols = -date) %>%
  arrange(name, date) %>%
  group_by(name) %>%
  mutate(value = value / lag(value, 12) - 1) %>%
  drop_na() |>
  filter(date >= date_from) |>
  ungroup()


# 2.4.2 ný útgáfa ----
print("Undirliggjandi verðbólga - ný útgáfa")

# PX-tafla: VIS01101 "Vísitala neysluverðs eftir eðli og uppruna" (NÝ flokkun,
# hefst 2025M04 í þessari fyrirspurn). Sama tafla og 2.3.2 en breitt form.
# Vefviðmót:
# https://px.hagstofa.is/pxis/pxweb/is/Efnahagur/Efnahagur__visitolur__1_vnv__3_greiningarvisitolur/VIS01101.px
#
# Stuðlar sem fyrirspurnin skilar (5 talsins, í þessari röð):
#   1 "Mánuður"                                                        -> date
#   2 "Vísitala Kjarnavisitala_1 (VNV án búvöru, grænmetis,
#      ávaxta og bensíns)"                                             -> "Kjarnavísitala 1"
#   3 "Vísitala Kjarnavisitala_2  (Kjarnavísitala 1 án
#      opinberrar þjónustu)"                                           -> "Kjarnavísitala 2"
#   4 "Vísitala Kjarnavisitala_4 (kjarnavísitala 2 án
#      reiknaðrar húsaleigu)"                                          -> "Kjarnavísitala 4"
#   5 "Vísitala Kjarnavisitala_5 (VNV án orku og matvöru)"             -> "Kjarnavísitala 5"
#
# ATH. FIMM stuðlar hér en FJÓRIR í eldri röðinni (kjarnavísitala 5 er ný).
# Vísitalan er á réttum kvarða (ekki tífölduð) og engin "." gildi - þess vegna
# ekkert na = "." hér.
undirliggjandi_new_tbl <- read_csv2(
  "https://px.hagstofa.is:443/pxis/sq/09238158-eb02-47c2-a560-60c52341bcb4"
) |>
  drop_na() |>
  set_names(
    "date",
    "Kjarnavísitala 1",
    "Kjarnavísitala 2",
    "Kjarnavísitala 4",
    "Kjarnavísitala 5"
  ) |>
  mutate(date = make_date(str_sub(date, 1, 4), str_sub(date, 6, 7))) %>%
  pivot_longer(cols = -date) %>%
  arrange(name, date) %>%
  group_by(name) %>%
  mutate(value = value / lag(value, 12) - 1) %>%
  drop_na() |>
  filter(date >= date_from) |>
  ungroup()


# 2.4.3 sameina ----
print("Undirliggjandi verðbólga - sameina")

# Skeytt saman. Ath. hér er skorið á max(nýja) - EKKI á min(nýja) eða
# new_coicop_date_from - svo eldri röðin fyllir upp í allt fram að nýjasta
# mánuði nýju raðarinnar. Þar sem raðirnar skarast verða því TVÖ gildi á sama
# mánuði fyrir kjarnavísitölur 1, 2 og 4 (eitt úr hvorri flokkun).
undirliggjandi_tbl <-
  bind_rows(
    undirliggjandi_old_tbl |> filter(date < max(undirliggjandi_new_tbl$date)),
    undirliggjandi_new_tbl
  )


# 3.0.0 WATERFALL - ----

# Based on this: https://www.r-bloggers.com/2019/05/basic-waterfall-graphs-in-r/
# https://rpubs.com/techanswers88/waterfall-chart-ggplot

# 3.1.0 Vísitölugildi ----

print("waterfall - vísitölugildi")


# PX-tafla: VIS01300 "Undirvísitölur, breytingar og áhrif" (NÝ flokkun, hefst
# 2025M05). Þetta er lykiltaflan fyrir waterfall-myndina og froop (kafli 5.2).
# Vefviðmót:
# https://px.hagstofa.is/pxis/pxweb/is/Efnahagur/Efnahagur__visitolur__1_vnv__2_undirvisitolur/VIS01300.px
#
# Stuðlar sem fyrirspurnin skilar (3 talsins, í þessari röð):
#   1 "Mánuður"        -> manudur       t.d. "2025M05"  (felldur út eftir fix_date)
#   2 "Undirvísitala"  -> undirvisitala COICOP-flokkur MEÐ númeri fremst
#   3 "Vísitala"       -> visitala      tölugildi (réttur kvarði, ekki tífaldað)
#
# Stuðull 2 inniheldur ALLT COICOP-tréð, frá tveggja stafa yfirflokkum niður í
# sex stafa smáflokka, t.d.:
#   "Vísitala neysluverðs"                (samtala - er TÓM "." nýjustu mánuði!)
#   "01 Matur og óáfengir drykkir"        (2 stafir - yfirflokkur)
#   "011 Matur"                           (3 stafir)
#   "0111 Korn og kornvörur"              (4 stafir)
#   "01111 Korn"                          (5 stafir)
#   "011131 Brauð"                        (6 stafir)
# Númerið er notað neðar til að velja rétt greiningarstig (sjá undirflokkar_tbl)
# og til að para við FROOPP-kóða úr hicp-pakkanum (kafli 5.0.0).
#
# Til að endurgera: veldu "Vísitala" sem lið og ALLAR undirvísitölur, alla mánuði.
undirflokkar_raw_new_tbl <- read_csv2(
  "https://px.hagstofa.is:443/pxis/sq/4635986b-ad3b-4cff-98ff-957ee8d2c4c8",
  na = "."
) |>
  janitor::clean_names() %>%
  fix_date() %>%
  select(-c(manudur)) %>%
  # ATH. röðin hér er undirflokkur | visitala | date (date bætist AFTAST við í
  # fix_date), ekki date fyrst eins og annars staðar í skránni.
  set_names("undirflokkur", "visitala", "date")


# 3.1.3 sameina raw undirflokka ----

# Find the overlap date (first month in new data that exists in old data)
# overlap_date <- min(undirflokkar_raw_new_tbl$date)

# # Calculate rebasing factors for each undirflokkur
# # Factor = old_visitala / new_visitala at overlap date
# rebase_factors <- undirflokkar_raw_old_tbl %>%
#   filter(date == overlap_date) %>%
#   select(undirflokkur, visitala_old = visitala) %>%
#   inner_join(
#     undirflokkar_raw_new_tbl %>%
#       filter(date == overlap_date) %>%
#       select(undirflokkur, visitala_new = visitala),
#     by = "undirflokkur"
#   ) %>%
#   mutate(factor = visitala_old / visitala_new) %>%
#   select(undirflokkur, factor)

# # Rebase new data and combine
# undirflokkar_raw_tbl <- bind_rows(
#   # Old data up to (but not including) the overlap date
#   undirflokkar_raw_old_tbl %>% filter(date < overlap_date),
#   # New data from overlap date onwards, rebased to old index
#   undirflokkar_raw_new_tbl %>%
#     left_join(rebase_factors, by = "undirflokkur") %>%
#     mutate(
#       factor = replace_na(factor, 1),
#       visitala = visitala * factor
#     ) %>%
#     select(-factor)
# )

# Bý til undirflokka án númers og vel þá undirflokka sem ég vil

print("waterfall - undirflokkar án númers")


# Velur EFSTA greiningarstig COICOP (tveggja stafa flokkana 01-12) og hendir
# öllum dýpri undirflokkum, því waterfall-myndin sýnir aðeins yfirflokkana.
#   numer_flokks  = talan fremst í heitinu (NA fyrir "Vísitala neysluverðs")
#   nchar_undirfl = fjöldi stafa í númerinu; 2 = yfirflokkur
#   is.na(...)    = "Vísitala neysluverðs" línan (engin tala) - höfð með
# Undantekningar: "Póstur og sími" og "Menntun" eru teknir með sérstaklega því
# númer þeirra les parse_number() ekki sem tveggja stafa tölu.
# Eftir þetta er númerið fjarlægt úr heitinu (str_remove_all á tölustöfum).
undirflokkar_tbl <- undirflokkar_raw_new_tbl %>%
  mutate(
    visitala = as.numeric(visitala),
    numer_flokks = parse_number(undirflokkur),
    undirflokkur = str_trim(str_remove_all(undirflokkur, "[:digit:]")),
    nchar_undirfl = nchar(numer_flokks),
    to_select = case_when(
      nchar_undirfl == 2 | is.na(nchar_undirfl) ~ "select",
      str_detect(undirflokkur, "Póstur og sími") ~ "select",
      str_detect(undirflokkur, "Menntun") ~ "select",
      TRUE ~ "remove"
    )
  ) %>%
  filter(to_select == "select") %>%
  select(-c(to_select, numer_flokks, nchar_undirfl))


# 3.2.0 Vogir ----

print("waterfall - vogir")

# Vogir eru aðeins gefnar út í mars (eldri ár) eða desember (nýrri ár), og
# tímastuðullinn kemur sem texti ("Desember 2025"), ekki sem "2025M12". Þessi
# tafla þýðir mánaðarheitið yfir í númer.
# ATH. manudir_tbl er ENDURSKILGREINT í kafla 13.0.0 með öllum 12 mánuðunum -
# passaðu að nota ekki þessa útgáfu eftir þann punkt.
manudir_tbl <- tibble(
  manudur = c("Mars", "Desember"),
  man_no = c(3, 12)
)

# Nýjar vogir
# desember 2025 á við 2026

# PX-tafla: VIS01306 "Vogir fyrir undirvísitölur neysluverðs" (NÝJAR vogir)
# Vefviðmót:
# https://px.hagstofa.is/pxis/pxweb/is/Efnahagur/Efnahagur__visitolur__1_vnv__2_undirvisitolur/VIS01306.px
#
# Stuðlar sem fyrirspurnin skilar (3 talsins, í þessari röð):
#   1 "Undirvísitala"        -> undirvisitala        COICOP-flokkur með númeri
#   2 "Tími"                 -> timi                 TEXTI, t.d. "Desember 2025"
#                                                    (EKKI "2025M12" - þess vegna
#                                                     separate() + manudir_tbl)
#   3 "Vísitala neysluverðs" -> visitala_neysluverds VOGIN, í tíuþúsundustu
#                                                    hlutum (10000 = 100%),
#                                                    þ.a.l. vog / 10000
#
# ATH. stuðull 1 og 2 eru í ÖFUGRI röð miðað við flestar aðrar töflur hér
# (undirvísitala kemur á undan tíma).
undirflokkar_vogir_new_raw_tbl <- read_csv2(
  "https://px.hagstofa.is:443/pxis/sq/95692056-2ee8-43c6-82fa-9479ac9d6534"
) |>
  janitor::clean_names() |>
  separate(timi, c("manudur", "ar")) |>
  left_join(manudir_tbl) |>
  mutate(date = make_date(year = ar, month = man_no)) %>%
  select(date, undirvisitala, visitala_neysluverds) %>%
  set_names("date", "undirflokkur", "vog") %>%
  mutate(vog = as.numeric(vog), vog = vog / 10000) |>
  arrange(date, undirflokkur)

# Vogir sem eru gefnar út í desember 2025 GILDA FYRIR árið 2026. Hér er
# dagsetningunni því ýtt fram um einn mánuð (des 2025 -> jan 2026) og síðan
# fyllt niður á alla mánuði ársins með crossing() + fill(.direction = "down"),
# svo hver mánuður eigi sér vog.
undirflokkar_vogir_new_raw_tbl <- undirflokkar_vogir_new_raw_tbl |>
  mutate(date = date %m+% months(1)) |>
  right_join(
    crossing(
      date = seq.Date(
        from = as.Date("2026-01-01"),
        to = max(undirflokkar_raw_new_tbl$date),
        by = "month"
      ),
      undirflokkur = unique(undirflokkar_vogir_new_raw_tbl$undirflokkur)
    ),
    by = c("date", "undirflokkur")
  ) |>
  arrange(undirflokkur, date) |>
  group_by(undirflokkur) |>
  fill(vog, .direction = "down") |>
  ungroup() |>
  drop_na(vog)

# 3.2.3 sameina vogir ----
# undirflokkar_vogir_tbl <- bind_rows(
#   undirflokkar_vogir_old_raw_tbl |>
#     filter(date < min(undirflokkar_vogir_new_raw_tbl$date)),
#   undirflokkar_vogir_new_raw_tbl
# )

print("waterfall - sameina vogir")

undirflokkar_vogir_tbl <- undirflokkar_vogir_new_raw_tbl %>%
  mutate(
    numer_flokks = parse_number(undirflokkur),
    undirflokkur = str_trim(str_remove_all(undirflokkur, "[:digit:]")),
    nchar_undirfl = nchar(numer_flokks),
    to_select = case_when(
      nchar_undirfl == 2 | is.na(nchar_undirfl) ~ "select",
      str_detect(undirflokkur, "Póstur og sími") ~ "select",
      str_detect(undirflokkur, "Menntun") ~ "select",
      TRUE ~ "remove"
    )
  ) %>%
  filter(to_select == "select") %>%
  select(-c(numer_flokks:to_select))

# 3.3.0 Sameina gögn ----

print("waterfall - sameina öll gögn")

undirflokkar_og_vogir_tbl <- undirflokkar_tbl %>%
  drop_na() |>
  arrange(date, undirflokkur) |>
  group_by(undirflokkur) |>
  mutate(
    verdbolga = visitala / lag(visitala, 12) - 1,
    verdbolga_1m = visitala / lag(visitala, 1) - 1
  ) |>
  drop_na() |>
  left_join(undirflokkar_vogir_tbl) %>%
  group_by(undirflokkur) %>%
  fill(vog, .direction = "down") %>%
  drop_na(vog) %>%
  ungroup() |>
  drop_na(visitala) |>
  mutate(ahrif = verdbolga * vog, ahrif_1m = verdbolga_1m * vog)

# 3.4.0 Úreikningar ----

print("waterfall - final útreikningar")

# Opinber verðbólga (12m og 1m) fyrir hvern mánuð - úr vnv_tbl (vísitala neysluverðs).
# Notað til að þvinga framlög undirliða til að leggja saman í rétta verðbólgu.
# Ath. undirflokka-gagnasettið sjálft er með NA á "Vísitala neysluverðs" línunni
# fyrir nýjustu mánuði og er því ekki nothæft sem uppspretta hér.
verdbolga_opinber_tbl <- vnv_tbl %>%
  filter(flokkur == "Verðbólga") %>%
  select(
    date,
    verdbolga_alls = verdbolga,
    verdbolga_1m_alls = milli_manada
  )

# Verð að splitta þeim upp og sameina aftur til að fá final verðbólgu neðst
undirflokkar_latest_tbl <- undirflokkar_og_vogir_tbl %>%
  filter(date %in% tail(sort(unique(date)), 3)) %>%
  # verdbolga/verdbolga_1m eru verðbreytingar undirliðarins sjálfs (ekki framlag) og
  # eru notaðar sem viðbótarmerking á framlagsmyndunum.
  select(date, undirflokkur, ahrif, ahrif_1m, verdbolga, verdbolga_1m) %>%
  filter(!undirflokkur == "Vísitala neysluverðs") %>%
  # Vogir eru endurmetnar í janúar sem veldur því að summa framlaga hittir ekki
  # nákvæmlega á opinberu verðbólguna. Skölum framlögin hlutfallslega svo þau standist.
  left_join(verdbolga_opinber_tbl, by = "date") %>%
  group_by(date) %>%
  mutate(
    ahrif = if (is.na(first(verdbolga_alls)) || sum(ahrif, na.rm = TRUE) == 0) {
      ahrif
    } else {
      ahrif * first(verdbolga_alls) / sum(ahrif, na.rm = TRUE)
    },
    ahrif_1m = if (
      is.na(first(verdbolga_1m_alls)) || sum(ahrif_1m, na.rm = TRUE) == 0
    ) {
      ahrif_1m
    } else {
      ahrif_1m * first(verdbolga_1m_alls) / sum(ahrif_1m, na.rm = TRUE)
    }
  ) %>%
  ungroup() %>%
  select(date, undirflokkur, ahrif, ahrif_1m, verdbolga, verdbolga_1m) %>%
  arrange(date, desc(ahrif))


# 3.5.0 Milli mánaða - tilbúin gögn frá Hagstofu ----
# PX-tafla: VIS01300 "Undirvísitölur, breytingar og áhrif" - sama tafla og í
# 3.1.0 en hér er valinn liðurinn "Áhrif á vísitölu, %" í stað "Vísitala".
# Þ.e. Hagstofan reiknar framlag hvers undirliðar til mánaðarbreytingarinnar
# fyrir okkur - við þurfum ekki að vega það sjálf eins og gert er í 3.3/3.4.
# Vefviðmót:
# https://px.hagstofa.is/pxis/pxweb/is/Efnahagur/Efnahagur__visitolur__1_vnv__2_undirvisitolur/VIS01300.px
#
# Stuðlar sem fyrirspurnin skilar (4 talsins, í þessari röð):
#   1 "Mánuður"              -> date   t.d. "2025M07"
#   2 "Liður"                -> FELLDUR ÚT með select(-2); fast gildi
#                               "Áhrif á vísitölu, %"
#   3 "Undirvísitala"        -> name   COICOP-flokkur með númeri
#   4 "Vísitala neysluverðs" -> value  ÁHRIFIN í prósentum (t.d. 0.01 = 0,01%)
#
# ATH. select(-2) er STÖÐUBUNDIÐ. Ef Hagstofan bætir stuðli við eða breytir röð
# fellur rangur stuðull út - athugaðu þetta fyrst ef gildin líta undarlega út.
#
# Síað neðar á "^\\d{3}\\s" = aðeins ÞRIGGJA STAFA COICOP-flokkar (t.d.
# "011 Matur"), þ.e. millistig - hvorki yfirflokkar né smæstu liðir.
ahrif_a_visitolu_1m_tbl <-
  read_csv2(
    "https://px.hagstofa.is:443/pxis/sq/825f985f-8623-42f2-be9d-0d60e5ec1bbd"
  ) |>
  select(-2) |>
  set_names("date", "name", "value") |>
  mutate(
    value = as.numeric(value),
    date = make_date(str_sub(date, 1, 4), str_sub(date, 6, 7))
  ) |>
  filter(date %in% tail(sort(unique(date)), 4)) |>
  filter(str_detect(name, "^\\d{3}\\s")) |>
  mutate(name = str_remove(name, "^\\d+\\s+"))


# 4.0.0 STÖPLARIT UNDIRFLOKKA ----
# Byggir á undirflokkar_tbl (kafli 3.1.0, VIS01300) - engin ný gagnasókn hér.
# Sérkennileg uppbygging: pivot_wider + pivot_longer eru notuð til að draga
# "Vísitala neysluverðs" línuna út í SÉRSTAKAN stuðul sem endurtekst á öllum
# línum (heildarverðbólgan til viðmiðunar), meðan hinir flokkarnir verða
# name/value pör. Þess vegna heitir fyrsti stuðullinn "verdbolga" eftir
# set_names() - hann geymir heildina, ekki flokkinn.

print("stöplarit undirflokka")


undirflokkar_12m_tbl <- undirflokkar_tbl %>%
  arrange(undirflokkur, date) %>%
  group_by(undirflokkur) %>%
  mutate(
    verdbolga = visitala / lag(visitala, 12) - 1
  ) %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  select(undirflokkur, verdbolga) %>%
  pivot_wider(names_from = undirflokkur, values_from = verdbolga) %>%
  pivot_longer(cols = -"Vísitala neysluverðs") |>
  set_names("verdbolga", "name", "value")

if (all(is.na(undirflokkar_12m_tbl$verdbolga))) {
  undirflokkar_12m_tbl$verdbolga <- verdbolga_latest
}

undirflokkar_1m_tbl <- undirflokkar_tbl %>%
  arrange(undirflokkur, date) %>%
  group_by(undirflokkur) %>%
  mutate(
    verdbolga = visitala / lag(visitala, 1) - 1
  ) %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  select(undirflokkur, verdbolga) %>%
  pivot_wider(names_from = undirflokkur, values_from = verdbolga) %>%
  pivot_longer(cols = -"Vísitala neysluverðs") |>
  set_names("verdbolga", "name", "value")


# 5.0.0 FROOP ----
print("Froop")

# FROOPP = "Frequent Out-Of-Pocket Purchases", þ.e. vörur og þjónusta sem
# heimili kaupa oft og greiða beint úr eigin vasa (matvara, eldsneyti o.þ.h.).
# Þetta er evrópskur samræmdur mælikvarði - hugsunin er að þetta sé sú
# verðbólga sem fólk FINNUR mest fyrir í daglegu lífi.
#
# Listinn yfir COICOP-kóða sem teljast til FROOPP kemur úr `hicp` pakkanum
# (spec.agg), ekki frá Hagstofu. Sóttir eru tveir listar því flokkunarkerfið
# breyttist:
#   froopp_old = ECOICOP 1 -> paraður við eldri Hagstofu-töfluna (VIS01304)
#   froopp_new = ECOICOP 2 -> paraður við nýju töfluna (VIS01300)
# "CP" forskeytið er fjarlægt svo kóðarnir séu berir tölustafir og passi við
# númerin sem parse_number() les úr heitum Hagstofu-flokkanna.
options(hicp.coicop.prefix = "CP")

options(hicp.coicop.version = "ecoicop1.hicp")
froopp_old <- str_remove(spec.agg("FROOPP")$FROOPP, "CP")

options(hicp.coicop.version = "ecoicop2.hicp")
froopp_new <- str_remove(spec.agg("FROOPP")$FROOPP, "CP")

# 5.1.0 Eldri flokkun ----

print("Froop - eldri")

# visitala
# PX-tafla: VIS01304 "Undirvísitölur neysluverðs, grunnur 2008=100 frá
# 2008-2025" (ELDRI flokkun, komin í "Eldra efni" - uppfærist ekki lengur).
# Nýja samsvörunin er VIS01300, sótt í kafla 3.1.0.
# Vefviðmót:
# https://px.hagstofa.is/pxis/pxweb/is/Efnahagur/Efnahagur__visitolur__1_vnv__4_eldraefni/VIS01304.px
#
# Stuðlar sem fyrirspurnin skilar (3 talsins, í þessari röð):
#   1 "Mánuður"              -> manudur              t.d. "2008M01" (felldur út)
#   2 "Undirvísitala"        -> undirvisitala        COICOP-flokkur með númeri,
#                                                    t.d. "01 Matur og drykkjarvörur"
#                                                    (ath. eldri heiti - "Matur og
#                                                     DRYKKJARVÖRUR" en ekki
#                                                     "óáfengir drykkir" eins og
#                                                     í nýju töflunni)
#   3 "Vísitala neysluverðs" -> visitala_neysluverds tölugildi, 2008=100
#
# ATH. vantandi gildi eru ".." í ÞESSARI töflu (tveir punktar), ekki "." eins
# og í flestum hinum - þess vegna na = ".." hér.
undirflokkar_raw_old_tbl <- read_csv2(
  "https://px.hagstofa.is:443/pxis/sq/ea481531-344b-45ba-b5bf-90f45bb08022",
  na = ".."
) %>%
  janitor::clean_names() %>%
  fix_date() %>%
  select(-manudur) %>%
  set_names("undirflokkur", "visitala", "date")

# vogir
# PX-tafla: VIS01305 "Vogir fyrir undirvísitölur neysluverðs frá 1992-2024"
# (ELDRI vogir, í "Eldra efni"). Nýja samsvörunin er VIS01306, kafli 3.2.0.
# Vefviðmót:
# https://px.hagstofa.is/pxis/pxweb/is/Efnahagur/Efnahagur__visitolur__1_vnv__4_eldraefni/VIS01305.px
#
# Stuðlar sem fyrirspurnin skilar (3 talsins, í þessari röð):
#   1 "Undirvísitala"        -> undirvisitala        COICOP-flokkur með númeri
#   2 "Tími"                 -> timi                 TEXTI, t.d. "Nóvember 1992",
#                                                    "Mars 1997", "Mars 1998" ...
#   3 "Vísitala neysluverðs" -> visitala_neysluverds VOGIN í tíuþúsundustu
#                                                    hlutum (10000 = 100%)
#
# ATH. eldri vogir voru gefnar út í MARS (og stöku sinnum nóvember). manudir_tbl
# þekkir aðeins "Mars" og "Desember", svo drop_na(man_no) hendir hér út
# nóvember-línunum frá 1992 - það er viljandi, tímaröðin byrjar hvort eð er
# ekki fyrr en 2008 í undirflokkar_raw_old_tbl.
undirflokkar_vogir_old_raw_tbl <- read_csv2(
  "https://px.hagstofa.is:443/pxis/sq/dac051cc-1094-449d-b638-b3a95ddf2b86"
) %>%
  janitor::clean_names() %>%
  separate(timi, c("manudur", "ar")) |>
  left_join(manudir_tbl) |>
  drop_na(man_no) |>
  mutate(date = make_date(year = ar, month = man_no)) %>%
  drop_na(date) %>%
  select(date, undirvisitala, visitala_neysluverds) %>%
  set_names("date", "undirflokkur", "vog") %>%
  mutate(vog = as.numeric(vog), vog = vog / 10000)


vnv_vogir_old_tbl <- undirflokkar_raw_old_tbl %>%
  drop_na(visitala) %>%
  left_join(undirflokkar_vogir_old_raw_tbl) %>%
  group_by(undirflokkur) %>%
  fill(vog, .direction = "down") %>%
  drop_na(vog) %>%
  ungroup()


# Bý til númer flokks
# Númerið er núllfyllt upp í FJÓRA stafi ("7" -> "0007", "011" -> "0011") því
# svo eru FROOPP-kóðarnir úr hicp-pakkanum sniðnir. "Vísitala neysluverðs"
# hefur ekkert númer og fær "0000" (og dettur út í filter, enda ekki í listanum).
# Síðan: vogir endurskalaðar svo þær summi í 1 innan FROOPP-mengisins, og
# vísitalan reiknuð sem vegin summa undirvísitalna.
froop_old_tbl <- vnv_vogir_old_tbl %>%
  # Númer flokka
  mutate(
    numer_flokks = parse_number(undirflokkur),
    numer_flokks = case_when(
      is.na(numer_flokks) ~ "0000",
      nchar(numer_flokks) == 3 ~ paste0("0", numer_flokks),
      nchar(numer_flokks) == 2 ~ paste0("00", numer_flokks),
      nchar(numer_flokks) == 1 ~ paste0("000", numer_flokks),
      TRUE ~ as.character(numer_flokks)
    )
  ) %>%
  filter(numer_flokks %in% froopp_old) %>%
  # Vogir
  group_by(date) %>%
  mutate(vog = vog / sum(vog)) %>%
  ungroup() %>%

  # Froop vísitala
  mutate(
    visitala = as.numeric(visitala),
    impact = visitala * vog
  ) %>%
  group_by(date) %>%
  summarise(froop = sum(impact)) %>%
  mutate(infl = froop / lag(froop, 12) - 1) %>%
  drop_na() %>%
  select(date, infl, froop) %>%
  set_names("date", "froop", "froop_index")


# Sama útreikningur en fyrir MÓTMENGIÐ (!numer_flokks %in% froopp_old), þ.e.
# allt sem telst EKKI til tíðra útgjalda úr eigin vasa.
non_froop_old_tbl <- vnv_vogir_old_tbl %>%
  # Númer flokka
  mutate(
    numer_flokks = parse_number(undirflokkur),
    numer_flokks = case_when(
      is.na(numer_flokks) ~ "0000",
      nchar(numer_flokks) == 3 ~ paste0("0", numer_flokks),
      nchar(numer_flokks) == 2 ~ paste0("00", numer_flokks),
      nchar(numer_flokks) == 1 ~ paste0("000", numer_flokks),
      TRUE ~ as.character(numer_flokks)
    )
  ) %>%
  filter(!numer_flokks %in% froopp_old) %>%
  # Vogir
  group_by(date) %>%
  mutate(vog = vog / sum(vog)) %>%
  ungroup() %>%

  # Froop vísitala
  mutate(
    visitala = as.numeric(visitala),
    impact = visitala * vog
  ) %>%
  group_by(date) %>%
  summarise(index = sum(impact)) %>%
  mutate(infl = index / lag(index, 12) - 1) %>%
  drop_na() %>%
  select(date, infl, index) %>%
  set_names("date", "non_froop", "non_froop_index")

# skeiti við vnv
froop_old_tbl <- froop_old_tbl %>%
  left_join(non_froop_old_tbl) %>%
  left_join(
    vnv_tbl %>%
      filter(flokkur == "Verðbólga") %>%
      select(date, verdbolga, visitala)
  ) |>
  filter(date >= date_from)

# 5.2.0 ný flokkun ----

print("froop - ný útgáfa")

vnv_vogir_tbl <- undirflokkar_raw_new_tbl %>%
  drop_na(visitala) %>%
  left_join(undirflokkar_vogir_new_raw_tbl) %>%
  group_by(undirflokkur) %>%
  fill(vog, .direction = "down") %>%
  drop_na(vog) %>%
  ungroup()


# Bý til númer flokks
froop_tbl <- vnv_vogir_tbl %>%
  # Númer flokka
  mutate(
    numer_flokks = parse_number(undirflokkur),
    numer_flokks = case_when(
      is.na(numer_flokks) ~ "0000",
      nchar(numer_flokks) == 3 ~ paste0("0", numer_flokks),
      nchar(numer_flokks) == 2 ~ paste0("00", numer_flokks),
      nchar(numer_flokks) == 1 ~ paste0("000", numer_flokks),
      TRUE ~ as.character(numer_flokks)
    )
  ) %>%
  filter(numer_flokks %in% froopp_new) %>%
  # Vogir
  group_by(date) %>%
  mutate(vog = vog / sum(vog)) %>%
  ungroup() %>%

  # Froop vísitala
  mutate(
    visitala = as.numeric(visitala),
    impact = visitala * vog
  ) %>%
  group_by(date) %>%
  summarise(froop = sum(impact)) %>%
  mutate(infl = froop / lag(froop, 12) - 1) %>%
  drop_na() %>%
  select(date, infl, froop) %>%
  set_names("date", "froop", "froop_index")


non_froop_tbl <- vnv_vogir_tbl %>%
  # Númer flokka
  mutate(
    numer_flokks = parse_number(undirflokkur),
    numer_flokks = case_when(
      is.na(numer_flokks) ~ "0000",
      nchar(numer_flokks) == 3 ~ paste0("0", numer_flokks),
      nchar(numer_flokks) == 2 ~ paste0("00", numer_flokks),
      nchar(numer_flokks) == 1 ~ paste0("000", numer_flokks),
      TRUE ~ as.character(numer_flokks)
    )
  ) %>%
  filter(!numer_flokks %in% froopp_new) %>%
  # Vogir
  group_by(date) %>%
  mutate(vog = vog / sum(vog)) %>%
  ungroup() %>%

  # Froop vísitala
  mutate(
    visitala = as.numeric(visitala),
    impact = visitala * vog
  ) %>%
  group_by(date) %>%
  summarise(index = sum(impact)) %>%
  mutate(infl = index / lag(index, 12) - 1) %>%
  drop_na() %>%
  select(date, infl, index) %>%
  set_names("date", "non_froop", "non_froop_index")


# Skeiti við vnv ----
print("Froop - bæti vnv við")
froop_tbl <- froop_tbl %>%
  left_join(non_froop_tbl) %>%
  left_join(
    vnv_tbl %>%
      filter(flokkur == "Verðbólga") %>%
      select(date, verdbolga, visitala)
  ) |>
  filter(date >= date_from)


# 5.3.0 Sameina eldri og nýrri ----
# Niðurstaða: date | froop | froop_index | non_froop | non_froop_index |
#             verdbolga | visitala
# ATH. hér er einfaldlega bind_rows() án þess að skera á dagsetningu (ólíkt
# 2.3.3), því raðirnar tvær skarast ekki - eldri taflan (VIS01304) endar þar
# sem sú nýja (VIS01300) tekur við.
froop_final_tbl <- froop_old_tbl |>
  bind_rows(froop_tbl)


# 6.0.0 EUROSTAT - HICP ----

print("EUROSTAT - HICP")


# EKKI Hagstofa - þessi gögn koma frá Eurostat REST API (dataset prc_hicp_minr,
# samræmd vísitala neysluverðs). Notað til alþjóðlegs samanburðar.
# Vafraslóð á gagnasettið (til að skoða víddir og kóða):
# https://ec.europa.eu/eurostat/databrowser/view/prc_hicp_minr/default/table
#
# COICOP-flokkarnir sem eru sóttir (Eurostat-kóðar, ekki Hagstofu-númer):
#   TOTAL     - heildarvísitala
#   TOT_X_NRG - heild án orku
#   SERV      - þjónusta
#   GD        - vörur (goods)
#   FOOD      - matvara
coicops <- c(
  "TOTAL",
  "TOT_X_NRG",
  "SERV",
  "GD",
  "FOOD"
)

# Ísland, Norðurlöndin og ESB27 (skilgreining frá 2020, þ.e. án Bretlands)
geos <- c("IS", "DK", "NO", "FI", "SE", "EU27_2020")

# Sækir eitt land í einu og flettir JSON-stat svarinu út í tíbblu.
# Svarið er "sparse": resp$value er hlutur með strengjalykla sem eru LÍNULEG
# vísitala inn í fullt kross-margfeldi víddanna (röð-fyrst / row-major). Þess
# vegna er byggt fullt grid með expand_grid og gildunum raðað inn eftir stöðu.
# Stuðlar sem falla út úr þessu: freq | unit | geo | coicop18 | time | value
# (víddirnar ráðast af resp$id, svo þær breytast ef beiðninni er breytt).
# unit=I25 þýðir vísitala með 2025=100; freq=M = mánaðarlegt.
fetch_hicp_minr <- function(geo) {
  url <- paste0(
    "https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/prc_hicp_minr",
    "?format=JSON",
    "&geo=",
    geo,
    "&unit=I25",
    "&freq=M",
    "&",
    paste0("coicop18=", coicops, collapse = "&")
  )

  resp <- request(url) %>%
    req_perform() %>%
    resp_body_json()

  dim_ids <- resp$id
  sizes <- unlist(resp$size)

  # Build ordered labels for each dimension
  dim_labels <- map(
    dim_ids,
    ~ {
      cats <- resp$dimension[[.x]]$category
      idx <- order(unlist(cats$index))
      names(cats$label)[idx]
    }
  ) %>%
    set_names(dim_ids)

  # Create full grid (row-major order matches Eurostat indexing)
  grid <- expand_grid(!!!map(dim_labels, ~.x))

  # Compute linear index for each row
  grid$idx <- seq_len(nrow(grid)) - 1L

  # Map sparse values by position index
  vals <- rep(NA_real_, nrow(grid))
  for (key in names(resp$value)) {
    pos <- as.integer(key) + 1L
    vals[pos] <- resp$value[[key]]
  }
  grid$value <- vals
  grid$idx <- NULL

  filter(grid, !is.na(value))
}

hicp_raw_tbl <- map(geos, fetch_hicp_minr) %>%
  bind_rows() %>%
  mutate(time = as.Date(paste0(time, "-01"))) |>
  rename("coicop" = "coicop18")


# Varnagli: time-stuðullinn getur komið annaðhvort þegar sem Date (ef
# as.Date() að ofan tókst) eða enn sem texti "2026-01" (ef sniðið frá Eurostat
# breytist). Báðar leiðir enda í sömu stuðlum: date | flokkur | svaedi | value
if (class(unique(hicp_raw_tbl$time)) == "Date") {
  hicp_tbl <- hicp_raw_tbl %>%
    select(-c(unit, freq)) %>%
    rename(
      "date" = "time",
      "flokkur" = "coicop",
      "svaedi" = "geo"
    )
} else {
  hicp_tbl <- hicp_raw_tbl %>%
    mutate(date = ym(time)) %>%
    select(-c(time, unit, freq)) %>%
    rename(
      "flokkur" = "coicop",
      "svaedi" = "geo"
    )
}

hicp_infl_tbl <- hicp_tbl %>%
  arrange(date, flokkur, svaedi) %>%
  group_by(flokkur, svaedi) %>%
  mutate(
    infl = value / lag(value, 12) - 1,
    infl_3m = value / lag(value, 3) - 1
  ) %>%
  drop_na() %>%
  ungroup()


# Finn minnsta samnefnara í date
# Löndin birta HICP á mismunandi tímum, svo nýjasti mánuður er ekki sá sami
# alls staðar. Hér er fundinn sá nýjasti mánuður sem FLEST lönd eiga gögn fyrir
# og allt eftir hann skorið burt, svo samanburðurinn sé á jafnræðisgrundvelli.
max_date_hicp <- hicp_infl_tbl %>%
  filter(flokkur == "TOTAL") %>%
  group_by(svaedi) %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  count(date) %>%
  filter(n == max(n)) %>%
  pull(date)

hicp_infl_tbl <- hicp_infl_tbl %>%
  filter(date <= max_date_hicp)


# Preparation for power bi
land_tbl <- tibble(
  svaedi = c("DK", "FI", "IS", "NO", "SE", "EU27_2020"),
  country = c("Danmörk", "Finnland", "Ísland", "Noregur", "Svíþjóð", "EU")
)

hicp_pbi_tbl <- hicp_infl_tbl %>%
  filter(flokkur == "TOTAL") %>%
  left_join(land_tbl) |>
  filter(date >= date_from)


# 6.1.0 By Coicip ----

# HICP undirflokkar - power bi
hicp_undirflokkar_tbl <- hicp_infl_tbl %>%
  filter(
    flokkur %in% c("GD", "SERV", "TOT_X_NRG", "FOOD"),
    date == max(date)
  ) %>%
  mutate(
    flokkur = case_when(
      flokkur == "GD" ~ "Vörur",
      flokkur == "SERV" ~ "Þjónusta",
      flokkur == "FOOD" ~ "Matur",
      TRUE ~ "Verðbólga án orkugjafa"
    ),
    flokkur = enc2utf8(flokkur)
  ) %>%
  left_join(land_tbl)


# HIPC valuebox
hicp_valuebox_tbl <- hicp_pbi_tbl %>%
  filter(date == max(date)) %>%
  select(country, infl) %>%
  pivot_wider(names_from = country, values_from = infl)


# 7.0.0 HÚSNÆÐISVERÐ ----

# 7.1.0 eldri ----
print("Húsnæðisverð - eldri gögn")

# PX-tafla: VIS01304 "Undirvísitölur neysluverðs, grunnur 2008=100 frá
# 2008-2025" (ELDRI flokkun, "Eldra efni"). Sama tafla og í 5.1.0 en hér eru
# aðeins húsnæðisliðirnir valdir.
# Vefviðmót:
# https://px.hagstofa.is/pxis/pxweb/is/Efnahagur/Efnahagur__visitolur__1_vnv__4_eldraefni/VIS01304.px
#
# Stuðlar sem fyrirspurnin skilar (3 talsins, í þessari röð):
#   1 "Mánuður"              -> date      t.d. "2008M01"
#   2 "Undirvísitala"        -> lidur     TVEIR liðir:
#                                           "041 Greidd húsaleiga"
#                                           "042 Reiknuð húsaleiga"
#                                         (númerið er strípað af með str_remove
#                                          hér að neðan)
#   3 "Vísitala neysluverðs" -> visitala  tölugildi, 2008=100
#
# ATH. hér er EKKI kallað á clean_names(); stuðullinn er ávarpaður með
# upprunalega heitinu `Undirvísitala` (með broddi) í mutate() að neðan.
hus_eldri_tbl <-
  read_csv2(
    "https://px.hagstofa.is:443/pxis/sq/34cafc5e-5977-4ea3-a5ce-1ea804fe506c"
  )

hus_unnid_eldri_tbl <- hus_eldri_tbl %>%
  mutate(Undirvísitala = str_remove(Undirvísitala, "^\\d+\\s*")) %>%
  set_names("date", "lidur", "visitala") %>%
  mutate(
    date = make_date(str_sub(date, 1, 4), str_sub(date, 6, 7))
  ) %>%
  arrange(date, lidur) %>%
  group_by(lidur) %>%
  mutate(infl = visitala / lag(visitala, 12) - 1) %>%
  drop_na() %>%
  ungroup() |>
  filter(date >= date_from)

# 7.2.0 nýrri ----
print("Húsnæðisverð - nýrri gögn")

# PX-tafla: VIS01300 "Undirvísitölur, breytingar og áhrif" (NÝ flokkun, hefst
# 2025M02). Sama tafla og 3.1.0 / 3.5.0 en aðeins húsnæðisliðirnir.
# Vefviðmót:
# https://px.hagstofa.is/pxis/pxweb/is/Efnahagur/Efnahagur__visitolur__1_vnv__2_undirvisitolur/VIS01300.px
#
# Stuðlar sem fyrirspurnin skilar (3 talsins, í þessari röð):
#   1 "Mánuður"       -> date      t.d. "2025M02"
#   2 "Undirvísitala" -> lidur     "041 Greidd húsaleiga" / "042 Reiknuð húsaleiga"
#   3 "Vísitala"      -> visitala  tölugildi
#
# ATH. þriðji stuðullinn heitir "Vísitala" hér en "Vísitala neysluverðs" í
# eldri töflunni að ofan - það skiptir ekki máli því set_names() yfirskrifar
# heitin eftir STÖÐU, en er gott að vita ef þú berð töflurnar saman í vafra.
hus_tbl <- read_csv2(
  "https://px.hagstofa.is:443/pxis/sq/629d82e1-97f9-48ee-8432-1c4644462186"
)

hus_unnid_tbl <- hus_tbl %>%
  mutate(Undirvísitala = str_remove(Undirvísitala, "^\\d+\\s*")) %>%
  set_names("date", "lidur", "visitala") %>%
  mutate(
    date = make_date(str_sub(date, 1, 4), str_sub(date, 6, 7))
  ) %>%
  arrange(date, lidur) %>%
  group_by(lidur) %>%
  mutate(infl = visitala / lag(visitala, 12) - 1) %>%
  drop_na() %>%
  ungroup() |>
  filter(date >= date_from)

# sameina
hus_unnid_tbl <- bind_rows(
  hus_unnid_eldri_tbl |> filter(lidur %in% unique(hus_unnid_tbl$lidur)),
  hus_unnid_tbl
)

# 7.3.0 Vextir ----
print("Vextir bankanna")

# EKKI Hagstofa - almennir vextir óverðtryggðra / verðtryggðra útlána úr
# mánaðarlegum vaxtatilkynningum SÍ (sjá si_almennir_vextir() í
# R/sedlabanki_data.R). Saga til og með síðustu útfylltu línu blaðsins "vextir"
# í data/verdbolguskyrsla_data.xlsx, nýrri mánuðir sóttir sjálfvirkt.
# Stuðlar: date | Óverðtryggðir | Verðtryggðir (vextir í PRÓSENTUM, þess
# vegna / 100 hér).
vextir_tbl <- si_almennir_vextir(data_path) |>
  mutate(
    Óverðtryggðir = Óverðtryggðir / 100,
    Verðtryggðir = Verðtryggðir / 100
  ) |>
  filter(date >= date_from)


# 7.3.1 Meginvextir ----
print("Hleð inn meginvexti")

# Meginvextir SÍ (XML tímaröð 17923), mánaðarmeðaltal daglegra gilda sem brot.
# Stuðlar: date | Meginvextir.
meginvextir_tbl <- si_meginvextir()


vextir_tbl <- vextir_tbl %>%
  left_join(meginvextir_tbl) %>%
  drop_na() |>
  select(date, everything())


# 8.0.0 ÚTLÁN ----

print("Útlán")

# Útlánagögnin eru sótt beint til Seðlabankans (R/sedlabanki_data.R). Raðir
# eru fundnar eftir heiti og dagsetningar lesnar úr skjölunum, svo ef
# Seðlabankinn breytir uppsetningu stöðvast keyrslan með villu frekar en að
# skila röngum tölum. Allar fjárhæðir í M.kr.

# 8.1.0 Útlánastabbi ----
# "Stabbi" = staða útlána (útistandandi fjárhæð), ekki ný útlán.

# Innlánsstofnanir
print("Útlán innlánsstofnana")
# Verðtryggð / óverðtryggð íbúðalán innlánsstofnana til heimila.
# Stuðlar: date | banki_vtr | banki_ovtr. Frá desember 1997.
bankakerfi_tbl <- si_utlan_bankar()


# Lífeyrissjóðir
print("Útlán lífeyrissjóða")

# Verðtryggð / óverðtryggð íbúðalán lífeyrissjóða til heimila.
# Stuðlar: date | lif_vtr | lif_ovtr. Frá janúar 1997.
lifeyrissjodir_tbl <- si_utlan_lif()


# Lánasjóðir ríkisins
print("Útlán lánasjóða ríkisins")

# Verðtryggð / óverðtryggð íbúðalán lánasjóða ríkisins (m.a. ÍL-sjóður).
# Stuðlar: date | lanasjodir_vtr | lanasjodir_ovtr. Frá mars 1992.
lsj_tbl <- si_utlan_lanasjodir()


# sameina stabba
# Leggur saman öll þrjú kerfin í "Verðtryggð" / "Óverðtryggð" og reiknar
# hlutdeild hvors um sig af heildinni.
# ATH. select(-contains("_")) treystir á að ALLIR upprunalegu stuðlarnir
# innihaldi undirstrik (banki_vtr, lif_ovtr, ...) og að hvorki "Verðtryggð",
# "Óverðtryggð" né "date" geri það. Ef stuðull er endurnefndur með undirstriki
# hverfur hann þegjandi hér.
utlan_stada_tbl <- bankakerfi_tbl %>%
  left_join(lifeyrissjodir_tbl, by = "date") %>%
  left_join(lsj_tbl, by = "date") %>%
  drop_na() %>%
  mutate(
    "Verðtryggð" = (banki_vtr + lif_vtr + lanasjodir_vtr),
    "Óverðtryggð" = c(banki_ovtr + lif_ovtr + lanasjodir_ovtr)
  ) %>%
  select(-contains("_")) %>%
  pivot_longer(cols = -date) %>%
  group_by(date) %>%
  mutate(share = value / sum(value)) %>%
  filter(date >= date_from) %>%
  ungroup()


# 8.2.0 Ný útlán ----

# Lífeyrissjóðir
print("Ný útlán lífeyrissjóða")

# Verðtryggð / óverðtryggð NÝ útlán lífeyrissjóða til heimila (flæði, ekki
# staða). Stuðlar: date | vtr | ovtr. Frá janúar 2009.
lif_ny_utlan <- si_ny_utlan_lif()


# Innlánsstofnanir
print("Ný útlán innlánsstofnana")
# Ný íbúðalán innlánsstofnana til heimila. Stuðlar:
#   br_ovt  = breytilegir vextir, óverðtryggt
#   fst_ovt = fastir vextir,      óverðtryggt
#   br_vt   = breytilegir vextir, verðtryggt
#   fst_vt  = fastir vextir,      verðtryggt
# Frá janúar 2013.
banki_ny_utlan_tbl <- si_ny_utlan_bankar()


# sameina ný útlán
ny_utlan_tbl <- lif_ny_utlan %>%
  left_join(banki_ny_utlan_tbl, by = "date") %>%
  drop_na()

ny_utlan_tbl <- ny_utlan_tbl %>%
  mutate(
    "Verðtryggð lán" = (vtr + br_vt + fst_vt),
    "Óverðtryggð lán" = (ovtr + br_ovt + fst_ovt)
  ) %>%
  select(date, contains("lán")) |>
  pivot_longer(cols = -date) |>
  filter(date >= date_from)


# 9.0.0 GENGI ----
print("Sæki upplýsingar um gengi af heimasíðu Seðlabankans")

# EKKI Hagstofa - XML tímaraðaþjónusta Seðlabanka Íslands.
# Vefviðmót til að fletta upp TimeSeriesID (ef númerin hér að neðan hætta að
# virka): https://www.sedlabanki.is/hagtolur/opinber-gengisskraning/
#
# Svarið er XML með <Entry> hnútum sem hver inniheldur tvo undirhnúti:
#   <Date>  dagsetning á sniðinu m/d/á kl. (þess vegna mdy_hms)
#   <Value> gengið
# xmlToDataFrame skilar þeim sem tveimur stuðlum sem eru nefndir date | gengi.
#
# TimeSeriesID sem eru notuð hér:
#   4064 = EUR (miðgengi)
#   4055 = USD (miðgengi)
#   4117 = gengisvísitala
get_si_gengi <- function(gjaldmidill) {
  currency_id <- switch(
    gjaldmidill,
    "eur" = 4064,
    "usd" = 4055,
    "visitala" = 4117
  )

  path_to_data <- paste0(
    "http://www.sedlabanki.is/xmltimeseries/Default.aspx?DagsFra=2005-01-01&DagsTil=",
    today(),
    "T00:00:00&TimeSeriesID=",
    currency_id,
    "&Type=xml"
  )

  doc <- GET(path_to_data)

  xml_1 <- xml2::read_xml(content(doc, "text"))
  xml_2 <- xmlParse(xml_1)

  gengi_tbl <- xmlToDataFrame(nodes = getNodeSet(xml_2, "//Entry")) %>%
    as_tibble() %>%
    set_names("date", "gengi")

  gengi_tbl %>%
    mutate(
      date = as.Date(mdy_hms(date)),
      gengi = as.numeric(gengi)
    ) %>%
    set_names("date", gjaldmidill)
}

# EUR: gjaldmidill = eur
# USD: gjaldmidill = usd
# gengisvíaitala: gjaldmidill = visitala

# Dagleg gengisgögn eru sótt fyrir hvern gjaldmiðil og sameinuð á dagsetningu,
# síðan tekið mánaðarmeðaltal. Niðurstaða: date | name | value þar sem name
# tekur gildin "EUR", "USD", "Gengisvísitala".
gengi_tbl <- get_si_gengi("eur") %>%
  left_join(get_si_gengi("usd")) %>%
  left_join(get_si_gengi("visitala")) %>%
  set_names("date", "EUR", "USD", "Gengisvísitala") %>%
  pivot_longer(cols = -date) %>%
  mutate(date = floor_date(date, "month")) %>%
  group_by(date, name) %>%
  summarise(value = mean(value)) |>
  filter(date >= date_from)


# 10.0.0 INNGRIP SEÐLABANKANS Á GJALDEYRISMARKAÐ ----

print("Inngrip seðlabankans")

# EKKI Hagstofa - XML tímaraðir Seðlabankans (si_inngrip() í
# R/sedlabanki_data.R), dagleg gildi í m.kr.:
#   velta  heildarvelta á millibankamarkaði með gjaldeyri
#   kaup   gjaldeyriskaup Seðlabankans
#   sala   gjaldeyrissala Seðlabankans
# Dagleg gögn eru lögð saman upp í mánuði.
inngrip_si_tbl <- si_inngrip() |>
  mutate(date = floor_date(date, "month")) |>
  group_by(date) |>
  summarise(
    velta = sum(velta),
    kaup = sum(kaup),
    sala = sum(sala)
  ) |>
  mutate(kaup_hlutfall_velta = kaup / velta) |>
  filter(date < floor_date(today(), "month"))

inngrip_si_tbl <- inngrip_si_tbl |>
  left_join(
    gengi_tbl |>
      ungroup() |>
      filter(name == "EUR") |>
      select(-name) |>
      rename("EURISK" = "value")
  ) |>
  filter(date >= date_from)

# 11.0.0 VERÐBÓLGUVÆNTINGAR ----

print("Verðbólguvæntingar")

# EKKI Hagstofa - Excel-skrár frá Seðlabankanum:
#   exp_path = data/Verdbolguvaentingar-a-mismunandi-maelikvarda.xlsx
#   data/Vaentingar_markadsadila.xlsx
# Sækja má nýjar útgáfur hér:
#   https://www.sedlabanki.is/hagtolur/verdbolguvaentingar/
#
# VARÚÐ - STÖÐUBUNDIÐ eins og útlánakaflinn: gögnin eru í breiðu formi
# (ársfjórðungar sem dálkar) og rétt röð er valin með slice(rows). Dagsetningar
# eru BÚNAR TIL með seq.Date() frá föstum upphafsfjórðungi, ekki lesnar úr
# skjalinu. Ef Seðlabankinn bætir röð við eða breytir upphafi færist allt til
# án villuboða.
#
# Fallið les eina röð, snýr henni í langt form og gefur:
#   value (væntingar í PRÓSENTUM, þess vegna / 100) | date (ársfjórðungslega)
get_infl_exp <- function(exp_path, rows, sheet_name, date_from) {
  read_excel(exp_path, sheet = sheet_name) |>
    slice(rows) |>
    pivot_longer(cols = everything()) |>
    select(2) |>
    slice(-1) |>
    mutate(
      date = seq.Date(
        from = as.Date(date_from),
        length.out = nrow(cur_data()),
        by = "quarter"
      ),
      value = as.numeric(value) / 100
    ) |>
    drop_na()
}

# Heimili - blað "Heimili_Households", röð 9 = væntingar til 12 mánaða.
# Ársfjórðungsleg röð frá 1. ársfj. 2003.
infl_exp_heimili_tbl <- get_infl_exp(
  exp_path = exp_path,
  rows = 9,
  sheet_name = "Heimili_Households",
  date_from = "2003-01-01"
) |>
  mutate(key = "Heimili")

# Fyrirtæki - blað "Fyrirtæki_Businesses", röð 9 = væntingar til 12 mánaða.
infl_exp_fyrirtaeki_tbl <- get_infl_exp(
  exp_path = exp_path,
  rows = 9,
  sheet_name = "Fyrirtæki_Businesses",
  date_from = "2003-01-01"
) |>
  mutate(key = "Fyrirtæki")

# Markaðsaðilar - ÖNNUR skrá (data/Vaentingar_markadsadila.xlsx), blað "III-a",
# röð 9 = væntingar til 12 mánaða. Ársfjórðungsleg röð frá 1. ársfj. 2012.
# Fyrsti dálkur er skýringartexti og er felldur út.
infl_exp_markadsadilar_tbl <- read_excel(
  "data/Vaentingar_markadsadila.xlsx",
  sheet = "III-a"
) |>
  slice(9) |>
  select(-1) |>
  pivot_longer(cols = everything()) |>
  select(-name) |>
  mutate(
    date = seq.Date(
      from = as.Date("2012-01-01"),
      length.out = nrow(cur_data()),
      by = "quarter"
    ),
    value = as.numeric(value) / 100
  ) |>
  mutate(key = "Markaðsaðilar")


# sameina
infl_exp_tbl <- bind_rows(
  infl_exp_heimili_tbl,
  infl_exp_fyrirtaeki_tbl,
  infl_exp_markadsadilar_tbl
) |>
  filter(date >= date_from)

# Skuldabréfamarkaður - verðbólguálag (breakeven), þ.e. vaxtamunur óverðtryggðra
# og verðtryggðra ríkisbréfa.
# Blað "Verðbólguálag_Breakeven rates", raðir 4-7, í þessari röð:
#   rað 4 -> "Verðbólguálag til 1 árs"
#   rað 5 -> "Verðbólguálag til 2 ára"
#   rað 6 -> "Verðbólguálag til 5 ára"
#   rað 7 -> "Verðbólguálag til 10 ára"
# Röðin á heitunum í set_names() verður að fylgja röðinni í slice().
# Ársfjórðungsleg röð frá 1. ársfj. 2003, gildi í prósentum (/ 100).
infl_exp_breakeven_tbl <- read_excel(
  exp_path,
  sheet = "Verðbólguálag_Breakeven rates"
) |>
  slice(4:7) |>
  pivot_longer(cols = -1) |>
  select(-name) |>
  set_names("bref", "value") |>
  mutate(value = as.numeric(value) / 100) |>
  pivot_wider(names_from = bref, values_from = value) |>
  unnest(everything()) |>
  set_names(
    "Verðbólguálag til 1 árs",
    "Verðbólguálag til 2 ára",
    "Verðbólguálag til 5 ára",
    "Verðbólguálag til 10 ára"
  ) |>
  mutate(
    date = seq.Date(
      from = as.Date("2003-01-01"),
      length.out = nrow(cur_data()),
      by = "quarter"
    ),
  ) |>
  pivot_longer(cols = -date) |>
  rename("key" = "name") |>
  filter(date >= date_from)


# # 11.1.0 Skuldabréfamarkaðurinn ----
# print("Skuldabréfamarkaðurinn")

# combine_interpolate_bonds <- function(
#   overdtryggd_tbl,
#   verdtryggd_tbl,
#   method = "linear"
# ) {
#   # Create sequence of monthly maturities
#   date_seq <- seq(
#     min(overdtryggd_tbl$maturity),
#     max(overdtryggd_tbl$maturity),
#     by = "month"
#   )

#   # Define interpolation function based on chosen method
#   interpolate_yield <- function(maturity, yield, xout, method) {
#     if (method == "linear") {
#       return(
#         approx(maturity, yield, xout = xout, method = "linear", rule = 2)$y
#       )
#     } else if (method == "spline") {
#       return(spline(maturity, yield, xout = xout, method = "natural")$y)
#     } else {
#       stop("Invalid method. Choose either 'linear' or 'spline'.")
#     }
#   }

#   # Apply interpolation
#   indexed_yield <- interpolate_yield(
#     verdtryggd_tbl$maturity,
#     verdtryggd_tbl$yield,
#     date_seq,
#     method
#   )
#   non_indexed_yield <- interpolate_yield(
#     overdtryggd_tbl$maturity,
#     overdtryggd_tbl$yield,
#     date_seq,
#     method
#   )

#   # Create interpolated yield tables
#   indexed_tbl <- tibble(maturity = date_seq, yield = indexed_yield)
#   non_indexed_tbl <- tibble(maturity = date_seq, yield = non_indexed_yield)

#   # Join datasets and calculate break-even inflation
#   combined_tbl <- inner_join(
#     indexed_tbl,
#     non_indexed_tbl,
#     by = "maturity",
#     suffix = c("_indexed", "_non_indexed")
#   ) %>%
#     mutate(
#       break_even_inflation = (1 + yield_non_indexed) / (1 + yield_indexed) - 1
#     ) %>%
#     select(maturity, break_even_inflation)

#   return(combined_tbl)
# }

# combine_interpolate_bonds_multiple_days <- function(
#   overdtryggd,
#   verdtryggd,
#   method = "linear"
# ) {
#   # Ensure we have valid dates
#   unique_dates <- unique(c(overdtryggd$date, verdtryggd$date))

#   # Nest data by date
#   nested_data <- tibble(date = unique_dates) %>%
#     mutate(
#       overd_data = map(date, ~ filter(overdtryggd, date == .x) %>% drop_na()),
#       verd_data = map(date, ~ filter(verdtryggd, date == .x) %>% drop_na())
#     )

#   # Apply function to each nested date
#   results <- nested_data %>%
#     mutate(
#       interpolated = map2(
#         overd_data,
#         verd_data,
#         ~ {
#           if (nrow(.x) == 0 || nrow(.y) == 0) {
#             return(NULL)
#           } # Handle missing cases
#           combine_interpolate_bonds(.x, .y, method = method)
#         }
#       )
#     ) %>%
#     unnest(interpolated) %>%
#     select(date, maturity, break_even_inflation)

#   return(results)
# }

# # 11.1.1 Söguleg gögn ----

# # * RIKB ----
# rikb_files <- list.files(paste0(base_path, "/00_data/skuldabref/"), pattern = "rikb_")

# rikb_ls <- rikb_files %>%
#   paste0(base_path, "/00_data/skuldabref/", .) %>%
#   map(., .f = function(x) read_csv2(x))

# names(rikb_ls) <- rikb_files

# rikb_tbl <- rikb_ls %>%
#   bind_rows(.id = "bref") %>%
#   mutate(
#     maturity = str_remove(bref, "rikb_|"),
#     maturity = str_remove(maturity, ".csv"),
#     maturity = ymd(maturity),
#     date = dmy(DateTime)
#   ) %>%
#   select(-DateTime) %>%
#   set_names("bref", "yield", "maturity", "date") %>%
#   mutate(
#     yield = yield / 100,
#     key   = "ovt"
#   )

# # Bý til smooth feril fyrir yield. Of mikið noise ef valinn er einn dagur
# rikb_tbl <- rikb_tbl %>%
#   arrange(bref, date) %>%
#   mutate(yield = rollapplyr(yield, width = 10, FUN = mean, partial = TRUE))

# # * RIKS ----
# riks_files <- list.files(paste0(base_path, "/00_data/skuldabref/"), pattern = "riks_")

# riks_ls <- riks_files %>%
#   paste0(base_path, "/00_data/skuldabref/", .) %>%
#   map(., .f = function(x) read_csv2(x))

# names(riks_ls) <- riks_files

# riks_tbl <- riks_ls %>%
#   bind_rows(.id = "bref") %>%
#   mutate(
#     maturity = str_remove(bref, "riks_|"),
#     maturity = str_remove(maturity, ".csv"),
#     maturity = ymd(maturity),
#     date = dmy(DateTime)
#   ) %>%
#   select(-DateTime) %>%
#   set_names("bref", "yield", "maturity", "date") %>%
#   mutate(
#     yield = yield / 100,
#     key   = "vt"
#   )

# # Bý til smooth feril fyrir yield. Of mikið noise ef valinn er einn dagur
# riks_tbl <- riks_tbl %>%
#   arrange(bref, date) %>%
#   mutate(yield = rollapplyr(yield, width = 10, FUN = mean, partial = TRUE))

# # * Verðbólguálag ----

# combined_bonds_over_time <- combine_interpolate_bonds_multiple_days(
#   rikb_tbl %>% filter(date >= "2023-10-01"),
#   riks_tbl %>% filter(date >= "2023-10-01")
# )

# # Define the current month
# current_month <- floor_date(Sys.Date(), "month")

# # Filter for the last 3 months
# last_3_months_dates <- seq(from = current_month - months(3),
#                            to = current_month - months(1),
#                            by = "month")

# # Find the last date available in each of the past 3 months
# dates_vector <- tibble(date = last_3_months_dates) %>%
#   mutate(last_available_date = map(date, ~ {
#     last_date <- max(
#       filter(combined_bonds_over_time, floor_date(date, "month") == .x)$date,
#       na.rm = TRUE
#     )
#     return(last_date)
#   })) %>%
#   unnest(last_available_date) %>%
#   pull(last_available_date)

# combined_bonds_over_time <- combined_bonds_over_time %>%
#   filter(date %in% c(today(), dates_vector))

# combined_bonds_over_time <- combined_bonds_over_time %>%
#   mutate(index = as.numeric(as_factor(as.character(date))))

# 12.0.0 Top 10 listinn ----
# Fínasta skipting hvers flokks. Finna svo top og botn 10 flokkana
# varðandi 12 mánaða og 1 mánaða breytingu

print("Top 10 listinn")

# PX-tafla: VIS01300 "Undirvísitölur, breytingar og áhrif" (NÝ flokkun, hefst
# 2025M02). Sama tafla og 3.1.0 - en hér er hún notuð ÓSÍUÐ, þ.e. allt
# COICOP-tréð niður í fínasta stig, til að finna hæstu og lægstu liði.
# Vefviðmót:
# https://px.hagstofa.is/pxis/pxweb/is/Efnahagur/Efnahagur__visitolur__1_vnv__2_undirvisitolur/VIS01300.px
#
# Stuðlar sem fyrirspurnin skilar (3 talsins, í þessari röð):
#   1 "Mánuður"       -> date      t.d. "2025M02"
#   2 "Undirvísitala" -> flokkur   COICOP-flokkur MEÐ númeri, öll stig
#                                  (2, 3, 4, 5 og 6 stafa)
#   3 "Vísitala"      -> visitala  tölugildi
vnv_allir_flokkar_tbl <- read_csv2(
  "https://px.hagstofa.is:443/pxis/sq/e4d89d5c-d295-41bd-8d17-0f2b22944d02",
  na = "."
) |>
  set_names("date", "flokkur", "visitala") |>
  drop_na()


# 1) Finna "leaf" kóða (þ.e. kóða sem EKKI eru forskeyti fyrir neinum öðrum kóða)
# COICOP-tréð er stigskipt og kóðarnir eru forskeyti hver annars: "01" -> "011"
# -> "0111" -> "01111". Ef við tækjum alla flokka með værum við að telja sömu
# vöruna oft. Hér eru því valdir aðeins BOTNFLOKKARNIR - þeir sem enginn annar
# kóði byrjar á. Þannig fæst fínasta mögulega skiptingin án tvítalningar.
# (Sama aðferð er endurtekin fyrir eldri gögnin í kafla 13.1.0.)
cpi_leaf <- vnv_allir_flokkar_tbl %>%
  mutate(
    code = stringr::str_extract(flokkur, "^\\d+") # ná í tölukóðann fremst (ef til er)
  )

codes_tbl <- cpi_leaf %>%
  filter(!is.na(code)) %>%
  distinct(code)

leaf_codes <- codes_tbl %>%
  mutate(
    has_child = purrr::map_lgl(
      code,
      ~ any(codes_tbl$code != .x & stringr::str_starts(codes_tbl$code, .x))
    )
  ) %>%
  filter(!has_child) %>%
  pull(code)

# 2) Sía á leaf-kóða og fjarlægja tölukóðann úr heitinu
cpi_detailed <- cpi_leaf %>%
  filter(!is.na(code), code %in% leaf_codes) %>%
  mutate(
    flokkur = stringr::str_remove(flokkur, "^\\d+\\s+"),
    # Tveir ólíkir COICOP-liðir heita báðir einfaldlega "Vatn" eftir að númerið
    # er strípað af. Þeir eru aðgreindir hér svo þeir renni ekki saman í einn
    # flokk í group_by/join síðar.
    flokkur = case_when(
      code == "01221" ~ "Vatn (drykkjarvörur)",
      code == "0443" ~ "Vatn (innan heimilis)",
      TRUE ~ flokkur
    )
  ) %>%
  select(date, flokkur, visitala) |>
  mutate(visitala = as.numeric(visitala))

# 12m og 1m breyting á hverjum botnflokki, aðeins NÝJASTI mánuðurinn.
# Notað í top/botn 10 listana hér að neðan.
infl_allir_flokkar_tbl <- cpi_detailed |>
  mutate(date = make_date(str_sub(date, 1, 4), str_sub(date, 6, 7))) |>
  arrange(date, flokkur) |>
  group_by(flokkur) |>
  mutate(
    infl_1m = visitala / lag(visitala) - 1,
    infl_12m = visitala / lag(visitala, 12) - 1
  ) |>
  ungroup() |>
  drop_na() |>
  filter(date == max(date))

# bottom 10
botn_1m_tbl <- infl_allir_flokkar_tbl |>
  arrange(infl_1m) |>
  slice_head(n = 10) |>
  select(flokkur, infl_1m)

botn_12m_tbl <- infl_allir_flokkar_tbl |>
  arrange(infl_12m) |>
  slice_head(n = 10) |>
  select(flokkur, infl_12m)

# top 10
top_1m_tbl <- infl_allir_flokkar_tbl |>
  arrange(desc(infl_1m)) |>
  slice_head(n = 10) |>
  select(flokkur, infl_1m)

top_12m_tbl <- infl_allir_flokkar_tbl |>
  arrange(desc(infl_12m)) |>
  slice_head(n = 10) |>
  select(flokkur, infl_12m)


# 13.0.0 UMFANG VERÐHÆKKANA ----

print("Umfang verðhækkana - heatmap")

# Hitakort sem sýnir hve STÓR HLUTI neyslukörfunnar (mælt í vogum) er að hækka
# um hversu mikið. Hver mánuður er brotinn niður í bil: verðlækkun, 0-2,5%,
# 2,5-5%, 5-7,5%, 7,5-10% og meira en 10%.

# ATH. manudir_tbl er hér ENDURSKILGREINT með öllum 12 mánuðunum (það var
# skilgreint í kafla 3.2.0 með aðeins "Mars" og "Desember"). Það er í lagi því
# vogatöflurnar sem notuðu fyrri útgáfuna hafa þegar verið lesnar - en ef þú
# færir kóða til, passaðu röðina.
manudir_tbl <-
  tibble(
    manudur = c(
      "Janúar",
      "Febrúar",
      "Mars",
      "Apríl",
      "Maí",
      "Júní",
      "Júlí",
      "Ágúst",
      "September",
      "Október",
      "Nóvember",
      "Desember"
    ),
    man_no = 1:12
  )

# 13.1.0 eldri ----

# PX-tafla: VIS01304 "Undirvísitölur neysluverðs, grunnur 2008=100 frá
# 2008-2025" (ELDRI flokkun, "Eldra efni"). Sama tafla og 5.1.0 / 7.1.0 en hér
# ósíuð - allt COICOP-tréð.
# Vefviðmót:
# https://px.hagstofa.is/pxis/pxweb/is/Efnahagur/Efnahagur__visitolur__1_vnv__4_eldraefni/VIS01304.px
#
# Stuðlar sem fyrirspurnin skilar (3 talsins, í þessari röð):
#   1 "Mánuður"              -> date      t.d. "2008M01"
#   2 "Undirvísitala"        -> flokkur   COICOP-flokkur með númeri, öll stig
#   3 "Vísitala neysluverðs" -> visitala  tölugildi, 2008=100
vnv_allir_flokkar_old_tbl <-
  read_csv2(
    "https://px.hagstofa.is:443/pxis/sq/73fd14fd-ab13-44ff-8f69-b888199b5d8f"
  ) |>
  set_names("date", "flokkur", "visitala") |>
  drop_na()


# 1) Finna "leaf" kóða (þ.e. kóða sem EKKI eru forskeyti fyrir neinum öðrum kóða)
cpi_leaf_old <- vnv_allir_flokkar_old_tbl %>%
  mutate(
    code = stringr::str_extract(flokkur, "^\\d+") # ná í tölukóðann fremst (ef til er)
  )

codes_old_tbl <- cpi_leaf_old %>%
  filter(!is.na(code)) %>%
  distinct(code)

leaf_codes_old <- codes_old_tbl %>%
  mutate(
    has_child = purrr::map_lgl(
      code,
      ~ any(
        codes_old_tbl$code != .x & stringr::str_starts(codes_old_tbl$code, .x)
      )
    )
  ) %>%
  filter(!has_child) %>%
  pull(code)

# 2) Sía á leaf-kóða og fjarlægja tölukóðann úr heitinu
cpi_detailed_old <- cpi_leaf_old %>%
  filter(!is.na(code), code %in% leaf_codes_old) %>%
  mutate(
    flokkur = stringr::str_remove(flokkur, "^\\d+\\s+"),
    flokkur = case_when(
      code == "01221" ~ "Vatn (drykkjarvörur)",
      code == "0443" ~ "Vatn (innan heimilis)",
      TRUE ~ flokkur
    )
  ) %>%
  select(date, flokkur, visitala) |>
  mutate(visitala = as.numeric(visitala))


# 13.1.1 vogir old ----

# PX-tafla: VIS01305 "Vogir fyrir undirvísitölur neysluverðs frá 1992-2024"
# (ELDRI vogir, "Eldra efni"). Sama tafla og 5.1.0 en hér eru allir flokkar
# teknir með, ekki bara þeir sem froop þarf.
# Vefviðmót:
# https://px.hagstofa.is/pxis/pxweb/is/Efnahagur/Efnahagur__visitolur__1_vnv__4_eldraefni/VIS01305.px
#
# Stuðlar sem fyrirspurnin skilar (3 talsins, í þessari röð):
#   1 "Undirvísitala"        -> undirvisitala        COICOP-flokkur með númeri
#   2 "Tími"                 -> timi                 TEXTI "<Mánuður> <ár>",
#                                                    t.d. "Nóvember 1992",
#                                                    "Mars 1997"
#   3 "Vísitala neysluverðs" -> visitala_neysluverds VOGIN í tíuþúsundustu
#                                                    hlutum (10000 = 100%)
#
# ATH. tíminn er EKKI á "2026M01" sniði heldur texti - þess vegna
# separate_wider_delim() á bili og uppfletting í manudir_tbl (sem hér inniheldur
# alla 12 mánuðina, sjá athugasemd að ofan).
# ATH. vogin er EKKI deilt með 10000 hér (ólíkt köflum 3.2.0 og 5.1.0) - hún er
# endursköluð hlutfallslega neðar (vog / sum(vog)) svo kvarðinn skiptir ekki máli.
vnv_vog_old_tbl <- read_csv2(
  "https://px.hagstofa.is:443/pxis/sq/ab5cf065-2286-4011-bdbe-ec5185dd6364"
) |>
  janitor::clean_names()

vnv_vog_old_tbl <- vnv_vog_old_tbl |>
  separate_wider_delim(timi, delim = " ", names = c("manudur", "ar")) |>
  left_join(manudir_tbl) |>
  mutate(date = make_date(ar, man_no)) |>
  select(date, undirvisitala, visitala_neysluverds) |>
  set_names("date", "flokkur", "vog") |>
  mutate(flokkur = str_remove(flokkur, "^\\d+\\s*")) |>
  drop_na()


# 13.1.2 umfang ----
infl_umfang_old_tbl <- cpi_detailed_old |>
  mutate(date = make_date(str_sub(date, 1, 4), str_sub(date, 6, 7))) |>
  arrange(date, flokkur) |>
  group_by(flokkur) |>
  mutate(
    infl = visitala / lag(visitala, 12) - 1
  ) |>
  ungroup() |>
  drop_na()


# Pörun við vogir er á FLOKKSHEITI (join á date + flokkur), ekki á númeri, því
# númerið hefur þegar verið strípað af báðum megin. Vogir eru aðeins til fyrir
# einn mánuð á ári, svo fill(.direction = "down") teygir þær yfir árið.
# Síðan er hver flokkur settur í hækkunarbil og vogir hvers bils lagðar saman -
# niðurstaðan segir hve stór hluti körfunnar hækkaði um hve mikið.
infl_umfang_old_tbl <- infl_umfang_old_tbl |>
  left_join(
    vnv_vog_old_tbl
  ) |>
  group_by(flokkur) |>
  fill(vog, .direction = "down") |>
  filter(date >= "2020-01-01") |>
  mutate(
    vog = as.numeric(vog),
    haekkanir = case_when(
      infl < 0 ~ "Verðlækkun",
      infl < 0.025 ~ "0-2,5%",
      infl < 0.05 ~ "2,5-5%",
      infl < 0.075 ~ "5-7,5%",
      infl < 0.1 ~ "7,5%-10%",
      TRUE ~ "Meira en 10%"
    )
  ) |>
  group_by(date) |>
  mutate(vog = vog / sum(vog, na.rm = TRUE)) |>
  group_by(date, haekkanir) |>
  summarise(vaegi = sum(vog, na.rm = TRUE)) |>
  ungroup()


infl_umfang_old_tbl <- infl_umfang_old_tbl |>
  mutate(
    haekkanir = factor(
      haekkanir,
      levels = c(
        "Meira en 10%",
        "7,5%-10%",
        "5-7,5%",
        "2,5-5%",
        "0-2,5%",
        "Verðlækkun"
      )
    )
  )


# 13.2.0 nýrri ----

# Nýrri vogirnar eru þegar lesnar inn í kafla 3.2.0 (VIS01306) - hér er sú
# tafla bara endurnefnd og númerið strípað af flokksheitinu svo hún passi við
# cpi_detailed (þar sem númerið hefur líka verið fjarlægt).
# Stuðlar: date | flokkur | vog
vnv_vog_tbl <- undirflokkar_vogir_new_raw_tbl |>
  set_names("date", "flokkur", "vog") |>
  mutate(
    flokkur = str_trim(str_remove_all(flokkur, "[:digit:]"))
  )


infl_umfang_tbl <- cpi_detailed |>
  mutate(date = make_date(str_sub(date, 1, 4), str_sub(date, 6, 7))) |>
  arrange(date, flokkur) |>
  group_by(flokkur) |>
  mutate(
    infl = visitala / lag(visitala, 12) - 1
  ) |>
  ungroup() |>
  drop_na()


infl_umfang_tbl <- infl_umfang_tbl |>
  left_join(
    vnv_vog_tbl
  ) |>
  group_by(flokkur) |>
  fill(vog, .direction = "down") |>
  filter(date >= "2020-01-01") |>
  mutate(
    vog = as.numeric(vog),
    haekkanir = case_when(
      infl < 0 ~ "Verðlækkun",
      infl < 0.025 ~ "0-2,5%",
      infl < 0.05 ~ "2,5-5%",
      infl < 0.075 ~ "5-7,5%",
      infl < 0.1 ~ "7,5%-10%",
      TRUE ~ "Meira en 10%"
    )
  ) |>
  group_by(date) |>
  mutate(vog = vog / sum(vog, na.rm = TRUE)) |>
  group_by(date, haekkanir) |>
  summarise(vaegi = sum(vog, na.rm = TRUE)) |>
  ungroup()

infl_umfang_tbl <- infl_umfang_tbl |>
  mutate(
    haekkanir = factor(
      haekkanir,
      levels = c(
        "Meira en 10%",
        "7,5%-10%",
        "5-7,5%",
        "2,5-5%",
        "0-2,5%",
        "Verðlækkun"
      )
    )
  )


# 13.3.0 Sameina umfang ----
# ATH. bind_rows() án dagsetningarskurðar - eldri röðin (frá 2020) og sú nýrri
# SKARAST því báðar ná yfir 2025. Þar sem bæði eldri og nýrri gögn eru til
# fyrir sama mánuð koma tvær færslur á hvert (date, haekkanir) par.
infl_umfang_tbl <- infl_umfang_tbl |>
  bind_rows(infl_umfang_old_tbl)

# 14.0.0 ALÞJÓÐLEGUR SAMANBURÐUR ----
# EKKI Hagstofa - staðbundin CSV-skrá sem er búin til annars staðar
# (sjá R/althjodleg_data.R og R/scrape_trading_econoimcs.R).
altjodlegar_upplysingar_tbl <- read_csv("data/altjodlegar.csv")


# 15.0.0 MATARVERÐ ----

# PX-tafla: VIS01300 "Undirvísitölur, breytingar og áhrif" (NÝ flokkun, hefst
# 2025M02). Sama tafla og 3.1.0 en hér eru valdir ÞRIGGJA/FJÖGURRA STAFA
# undirflokkar matvöru (COICOP 011x).
# Vefviðmót:
# https://px.hagstofa.is/pxis/pxweb/is/Efnahagur/Efnahagur__visitolur__1_vnv__2_undirvisitolur/VIS01300.px
#
# Stuðlar sem fyrirspurnin skilar (3 talsins, í þessari röð):
#   1 "Mánuður"       -> date     t.d. "2025M02"
#   2 "Undirvísitala" -> flokkur  matvöruflokkar með númeri, t.d.
#                                   "0111 Korn og kornvörur"
#                                   "0112 Kjöt, lifandi dýr og aðrir hlutar
#                                          af slátruðum landdýrum"
#                                   "0113 Fiskur og annað sjávarfang"
#                                   ... o.s.frv. undir 011 Matur
#   3 "Vísitala"      -> value    tölugildi (breytist í 12m breytingu að neðan)
matur_3ja_stafa_tbl <-
  read_csv2(
    "https://px.hagstofa.is:443/pxis/sq/68f399fb-89b2-4322-a7a7-7578e786e30c"
  ) |>
  set_names("date", "flokkur", "value") |>
  mutate(
    date = make_date(str_sub(date, 1, 4), str_sub(date, 6, 7)),
    flokkur = str_remove(flokkur, "^\\d+\\s+")
  ) |>
  group_by(flokkur) |>
  mutate(value = value / lag(value, 12) - 1) |>
  drop_na() |>
  ungroup()

# PX-tafla: VIS01101 "Vísitala neysluverðs eftir eðli og uppruna" (NÝ flokkun,
# hefst 2025M01). Sama tafla og 2.3.2 en hér eru valdir matarliðirnir eftir
# UPPRUNA (innlent / innflutt / blandað).
# Vefviðmót:
# https://px.hagstofa.is/pxis/pxweb/is/Efnahagur/Efnahagur__visitolur__1_vnv__3_greiningarvisitolur/VIS01101.px
#
# Stuðlar sem fyrirspurnin skilar (3 talsins, í þessari röð):
#   1 "Mánuður"         -> date     t.d. "2025M01"
#   2 "Útgjaldaflokkur" -> flokkur  þrír liðir:
#                                     "2a-i  Innlendur matur og drykkir"
#                                     "2b-i  Innfluttur matur og drykkir"
#                                     "2c-i  Matur og drykkir í blönduðum flokkum"
#   3 "Vísitala"        -> value    tölugildi (réttur kvarði, EKKI tífaldað
#                                   ólíkt fyrirspurninni í 2.3.2)
#
# ATH. hér er kóðinn ("2a-i") strípaður af með "^[^ ]+\\s+" (allt fram að
# fyrsta bili) en ekki "^\\d+\\s+" eins og annars staðar, því þessir kóðar
# innihalda bókstafi og bandstrik.
matur_uppruni_tbl <-
  read_csv2(
    "https://px.hagstofa.is:443/pxis/sq/e0e6a6af-0f1f-487e-9a7e-69c8c5a52ce6",
    na = "."
  ) |>
  set_names("date", "flokkur", "value") |>
  mutate(
    date = make_date(str_sub(date, 1, 4), str_sub(date, 6, 7)),
    flokkur = str_remove(flokkur, "^[^ ]+\\s+")
  )

matur_uppruni_tbl <- matur_uppruni_tbl |>
  group_by(flokkur) |>
  mutate(value = value / lag(value, 12) - 1) |>
  ungroup() |>
  drop_na()


# 16.0.0 Vista ----

# Allar töflur eru skrifaðar í eina .rds-skrá sem nafngreindur listi. Power BI
# skýrslan les hana og vísar í töflurnar eftir NAFNI lykilsins - ef þú
# endurnefnir lykil hér þarf að uppfæra skýrsluna líka.
# (Ath. tveir lyklar eru ritvillu-nefndir "phi_" í stað "pbi_" - það er látið
#  standa því skýrslan vísar í þau nöfn.)
list(
  # Verðbólga
  pbi_inflation_verdbolga = vnv_tbl,
  pbi_inflation_verdbolga_valuebox = valuebox_verdbolga,

  # Undirliggjandi
  pbi_inflation_undirliggjandi = undirliggjandi_tbl,
  pbi_inflation_uppruni = edli_og_uppruna_tbl,
  pbi_inflation_froop = froop_final_tbl,

  # Framlag undirliða
  pbi_inflation_waterfall = undirflokkar_latest_tbl,
  phi_inflation_undirflokkar = undirflokkar_12m_tbl,
  phi_inflation_undirflokkar_1m = ahrif_a_visitolu_1m_tbl,
  pbi_inflation_stoplarit_1m = undirflokkar_1m_tbl,

  # HICP
  pbi_inflation_hicp = hicp_pbi_tbl,
  pbi_inflation_hicp_undirflokkar = hicp_undirflokkar_tbl,
  pbi_inflation_hicp_valuebox = hicp_valuebox_tbl,

  # Húsnæðisverð
  pbi_inflation_husnaedisverd = hus_unnid_tbl,
  pbi_inflation_husnaaedisvextir = vextir_tbl,

  # Útlán
  pbi_inflation_utlanastabbi = utlan_stada_tbl,
  pbi_inflation_utlan_ny = ny_utlan_tbl,

  # Gengi og væntingar
  #pbi_inflation_skuldabref = combined_bonds_over_time,
  pbi_inflation_vaentingar = infl_exp_tbl,
  pbi_inflation_vaentingar_skuldabrefamarkadur = infl_exp_breakeven_tbl,
  pbi_inflation_gengi = gengi_tbl,
  pbi_inflation_inngrip = inngrip_si_tbl,

  # Top listinn
  botn_1m = botn_1m_tbl,
  botn_12m = botn_12m_tbl,
  top_1m = top_1m_tbl,
  top_12m = top_12m_tbl,

  # hitakort
  hitakort = infl_umfang_tbl,

  # Alþjóðlegur samanburður
  althjodlegur = altjodlegar_upplysingar_tbl,

  # Matur
  matur_thriggja = matur_3ja_stafa_tbl,
  matur_uppruni = matur_uppruni_tbl
) |>

  write_rds("data/final_data.rds")


# * -----
# 16.0.0 SPÁR ----

date_from <- floor_date(today() - years(5), "month")

# 1.1.0 Data ----
# PX-tafla: VIS01000 "Vísitala neysluverðs og breytingar, grunnur 1988=100"
# (sama tafla og kafli 2.1.0 en hér í BREIÐU formi og með lengri sögu, frá
#  1990M09 - notuð sem inntak í spálíkanið).
# Vefviðmót:
# https://px.hagstofa.is/pxis/pxweb/is/Efnahagur/Efnahagur__visitolur__1_vnv__1_vnv/VIS01000.px
#
# Stuðlar sem fyrirspurnin skilar (3 talsins, í þessari röð):
#   1 "Mánuður"                                  -> date  t.d. "1990M09"
#   2 "Vísitala neysluverðs Vísitala"            -> vnv   VNV
#   3 "Vísitala neysluverðs án húsnæðis Vísitala"-> vnvh  VNV án húsnæðis
#
# ATH. þessi fyrirspurn skilar vísitölunni TÍFALDAÐRI (þess vegna / 10) - ólíkt
# fyrirspurninni í kafla 2.1.0 sem skilar henni á réttum kvarða úr SÖMU TÖFLU.
# Munurinn liggur í því hvernig liðurinn er valinn í vefviðmótinu.
data_raw_tbl <- read_csv2(
  "https://px.hagstofa.is:443/pxis/sq/cc5a0596-bf1a-4a88-872a-485896d53ed6"
) %>%
  set_names("date", "vnv", "vnvh") %>%
  mutate(
    date = make_date(str_sub(date, 1, 4), str_sub(date, 6, 7)),
    vnv = vnv / 10,
    vnvh = vnvh / 10
  ) %>%
  drop_na() %>%
  mutate(id = "id") %>%
  filter(date >= "2003-01-01") %>%
  arrange(date)


level_roc1_max <- data_raw_tbl %>%
  filter(date == max(date)) %>%
  pull(vnv)

# EKKI Hagstofa - blað "spar" í data/verdbolguskyrsla_data.xlsx: spár annarra
# greiningaraðila, uppfært handvirkt.
# Stuðlar: date | Seðlabankinn | <einn stuðull á hvern banka/greiningaraðila>.
# Seðlabankinn er tekinn út sérstaklega því hann er sýndur sem sérstök lína í
# skýrslunni. Aðrir stuðlar (t.d. gamla creation_date) eru hunsaðir.
data_path <- "data/verdbolguskyrsla_data.xlsx"
spar_annarra_tbl <- readxl::read_excel(data_path, "spar") |>
  mutate(date = date(date)) |>
  select(-any_of("creation_date"))

spar_bankar_tbl <- spar_annarra_tbl |>
  select(-Seðlabankinn) |>
  pivot_longer(cols = -c(date)) |>
  drop_na()

spar_si_tbl <- spar_annarra_tbl |>
  select(date, Seðlabankinn)


# 2.0.0 Models ----

# 2.1.0 Medium term forecast ----
# Spálíkan VR: temporal hierarchy (THieF) - ARIMA er lagað að gögnunum á
# mörgum tíðnistigum (mánuðir, ársfjórðungar, ár o.s.frv.) og spárnar sættar
# með "struc" samsetningu. Spáð er 12 mánaða VERÐBÓLGU (ekki vísitölunni), og
# vísitalan síðan endurreiknuð úr spánni (fc_vnv = lag(vnv, 12) * (1 + .value)).
medium_data_tbl <- data_raw_tbl %>%
  mutate(infl = vnv / lag(vnv, 12) - 1) %>%
  drop_na()

medium_mtbl <- temporal_hierarchy(
  use_model = "arima",
  combination_method = "struc"
) %>%
  set_engine("thief") %>%
  fit(infl ~ date, data = medium_data_tbl) %>%
  modeltime_table()

fc_medium_tbl <- medium_mtbl %>%
  modeltime_forecast(
    h = 6,
    actual_data = medium_data_tbl,
    keep_data = TRUE
  )

fc_medium_tbl <- fc_medium_tbl %>%
  select(date, .key, .value) %>%
  left_join(data_raw_tbl %>% select(date, vnv)) %>%
  mutate(
    fc_vnv = lag(vnv, 12) * (1 + .value),
    fc_1m = fc_vnv / lag(fc_vnv) - 1
  )


# 2.2.0 Búa til nokkrar spár ----

# Eins mánaða spá
short_1m <- fc_medium_tbl |>
  filter(.key != "actual") %>%
  slice_head(n = 1) %>%
  pull(fc_1m)

# 6 mánaða spá í ágúst
# VARÚÐ: dagsetningarnar hér ("2026-02-01", "2026-08-01") eru HARÐKÓÐAÐAR og
# miðast við tiltekna útgáfu skýrslunnar. Þær þarf að uppfæra handvirkt þegar
# spátímabilið færist - annars skilar filter() engum línum og pull() tómum
# vektor, sem birtist sem tóm valuebox í skýrslunni fremur en sem villa.
fc_6m_aug <- fc_medium_tbl |>
  mutate(vnv_calc = if_else(is.na(vnv), lag(vnv, 12) * (1 + .value), vnv)) |>
  filter(date %in% c("2026-02-01", "2026-08-01")) |>
  mutate(
    fc_6m = ((vnv_calc / lag(vnv_calc))^(1 / 6))^12 - 1
  ) |>
  drop_na(fc_6m) |>
  pull(fc_6m)

# 12 mánaða spá í ágúst
fc_12m_august <- fc_medium_tbl |>
  filter(date == "2026-08-01") |>
  pull(.value)


# Bæti skammtímaspá við
fc_tbl <- fc_medium_tbl %>%
  mutate(name = if_else(.key == "actual", "Söguleg", "VR")) %>%
  select(date, .value, name, fc_vnv) %>%
  rename("value" = ".value") %>%
  # bind_rows(short_forecast_12m_tbl) %>%
  # as_tibble() |>
  filter(date >= date_from)

# Bæti við spám annarra greiningaraðila
fc_tbl <- fc_tbl |>
  bind_rows(spar_bankar_tbl) |>
  left_join(spar_si_tbl)

# 2.2.1 TÍMABUNDIÐ MODIFICATION Á SPÁ VEGNA 24% -> 11% VSK Á BENSÍN ----
#
# Handvirk leiðrétting: þegar VSK á bensíni var lækkaður úr 24% í 11% var það
# einskiptisáhrif sem líkanið gat ekki séð fyrir, svo það var dregið frá
# spánni handvirkt.
#
# VARÚÐ - ÞESSI KÓÐI ER ÓVIRKUR OG MYNDI BROTNA EF HANN KEYRÐI:
# `bein_ahrif` er AÐEINS skilgreint í athugasemdinni hér að neðan, ekki í
# virkum kóða. Skilyrðið month(today()) == 4 er því það eina sem kemur í veg
# fyrir "object 'bein_ahrif' not found" villu - í apríl myndi þetta stöðva
# keyrsluna. Ef á að nota þetta aftur þarf að afkomma línuna að neðan (og
# uppfæra bæði gildið og date_range).
#bein_ahrif <- 0.003742742

date_range <- seq.Date(
  from = as.Date("2026-05-01"),
  to = as.Date("2026-08-01"),
  by = "month"
)

if (month(today()) == 4) {
  fc_tbl <- fc_tbl |>
    mutate(
      value = if_else(
        name == "VR" & date %in% date_range,
        value - bein_ahrif,
        value
      )
    )

  short_1m <- short_1m - bein_ahrif

  # sex mánaða verðbólgan
  fc_6m_aug <- fc_medium_tbl |>
    mutate(vnv_calc = if_else(is.na(vnv), lag(vnv, 12) * (1 + .value), vnv)) |>
    filter(date %in% c("2026-02-01", "2026-08-01")) |>
    mutate(
      vnv_calc = if_else(
        date == max(date),
        vnv_calc * (1 - bein_ahrif),
        vnv_calc
      ),
      fc_6m = ((vnv_calc / lag(vnv_calc))^(1 / 6))^12 - 1
    ) |>
    drop_na(fc_6m) |>
    pull(fc_6m)

  fc_12m_august <- fc_12m_august - bein_ahrif
} else {
  "Spá VR mun aðlaga sig út frá 0.3% lækkun í "
}


# 3.5.0 Valuebox ----
fc_valuebox_tbl <- tibble(
  #spa_12m = percent(short_forecast_12m_tbl$value[1], accuracy = 0.01),
  spa_1m = percent(short_1m, accuracy = 0.01),
  fc_6m = percent(fc_6m_aug, accuracy = 0.01),
  fc_12m = percent(fc_12m_august, accuracy = 0.01)
)


# 4.0.0 SAVE ----

list(
  "verdbolguspa" = fc_tbl,
  "verdbolguspa_valuebox" = fc_valuebox_tbl
) |>
  write_rds("data/spar.rds")


# 17.0.0 GIT PUSH ----
# Skriftan er keyrð sjálfvirkt (sjá R/schedule.R) og committar uppfærðu
# .rds-skrárnar sjálf. Ef push mistekst (einhver annar hefur ýtt á undan) er
# reynt að rebase-a og ýta aftur.
# ATH. "git add ." tekur ALLT sem hefur breyst í vinnutrénu, ekki bara
# data/*.rds - passaðu að skilja ekki eftir hálfkláraðar breytingar þegar
# skriftan er keyrð handvirkt.

system("git add .")
system('git commit -m "Auto update data"')

if (system("git push origin main") != 0) {
  system("git pull --rebase origin main")
  system("git push origin main")
}


print(paste0("Uppfærslu lokið - ", today()))
