# Samantekt á gögnum fyrir Lykiltölur á ytri vef VR.


# 1.0.0 - SETUP - ---------------------------------------------------------

library(tidyverse)
library(timetk)
library(vr)
library(rvest)
library(httr)
library(readxl)
library(BIS)
library(janitor)
library(withr)

con <- vr_gagnagrunnur()

lond_tbl <- vr::heiti_landa %>% select(country, land)


# 2.0.0 - DATA - ----------------------------------------------------------

date_back <- floor_date(today() - months(60), "year")

fix_date <- function(x) {
  make_date(str_sub(x, 1, 4), str_sub(x, 6, 7))
}


max_date <- floor_date(today() - 75, "month")

months_tbl <- tibble(
  manudur = c("Janúar", "Febrúar", "Mars", "Apríl", "Maí", "Júní", "Júlí", "Ágúst", "September", "Október", "Nóvember", "Desember"),
  man_no = 1:12
)

# 2.1.0 Verðbólga ---------------------------------------------------------

vnv_tbl <- read_csv2(
  "https://px.hagstofa.is:443/pxis/sq/38b328bb-aebf-4265-a86a-e3643f5b6f5c"
) %>% 
  set_names("date", "vnv", "vnvh") %>% 
  mutate(date = fix_date(date))

vnv_tbl <- vnv_tbl %>% 
  mutate(
    vnv = vnv / lag(vnv, 12) - 1,
    vnvh =  vnvh / lag(vnvh, 12) - 1
    ) %>% 
  filter(date >= date_back) %>% 
  set_names("date", "Verðbólga", "Verðbólga án húsnæðis") %>% 
  pivot_longer(cols = -date)



# 2.2.0 Launaþróun --------------------------------------------------------

# 2.2.1 Hagstofa ----------------------------------------------------------

laun_hagstofa_tbl <- read_csv2("https://px.hagstofa.is:443/pxis/sq/c5f433b1-1c4c-4af0-8814-2d15969d0e83") %>% 
  set_names("date", "Almennur vinnumarkaður", "Opinberir starfsmenn") %>% 
  mutate(date = fix_date(date))

laun_hagstofa_tbl <- laun_hagstofa_tbl %>% 
  pivot_longer(cols = -date) %>% 
  arrange(date, name) %>% 
  group_by(name) %>% 
  filter(date >= date_back) %>% 
  mutate(value = value / value[1] * 100) %>% 
  ungroup() %>% 
  drop_na()


# 2.2.2 VR ----------------------------------------------------------------

laun_vr_tbl <- tbl(con, "launavisitala") %>% 
  select(date, midgildi) %>% 
  as_tibble() %>% 
  mutate(date = date(date)) %>% 
  rename("VR" = "midgildi")

# Árstíðarleiðrétting á tölum VR
laun_vr_tbl <- laun_vr_tbl %>% 
  tk_anomaly_diagnostics(date, VR) %>% 
  select(date, seasadj) %>% 
  rename("VR" = "seasadj") %>% 
  filter(date >= date_back) %>% 
  mutate(VR = VR / VR[1] * 100) %>% 
  pivot_longer(cols = -date)


laun_tbl <- bind_rows(
  laun_hagstofa_tbl,
  laun_vr_tbl
)



# 2.3.0 Kaupmáttur --------------------------------------------------------
# Þarf að hugsa betur.
# Hagstofa birtir ekki lengur neitt sem kallast Kaupmáttarvísitala. Þeir hafa,
# réttilega, skýrt þetta laun á föstu verðlagi. Viljum við sýna það?

kaupmattur_vr_tbl <- tbl(con, "kaupmattarvisitala") %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  select(date, leitni) %>% 
  filter(date >= date_back) %>% 
  arrange(date) %>% 
  mutate(
    date = date(date),
    leitni = leitni / leitni[1] * 100
    ) %>% 
  rename("Kaupmáttarvísitala VR" = "leitni") %>% 
  pivot_longer(cols = -date)



# 2.4.0 Ná endum saman ----------------------------------------------------



# 2.5.0 Slaki á vinnumarkaði ----------------------------------------------
# Til að meta óuppfyllta þörf fyrir atvinnu eru eftirfarandi hópar lagðir saman
# og hlutfall þeirra af mannfjölda metið: 1) Atvinnulausir; 2) Fólk í hlutastörfum
# sem vill og getur unnið meira; 3) Tilbúnir að vinna en eru ekki að leita að vinnu;
# 4) Ekki tilbúnir að hefja störf innan tveggja vikna en eru þó að leita sé að vinnu.
# Síðastnefndu tveir hóparnir falla undir yfirhóp þeirra sem eru utan vinnumarkaðar
# og teljast mögulegt vinnuafl.
# Deilitalan til þess að reikna slaka (hlutfallið) er þá heildarfjöldi þeirra sem eru starfandi, atvinnulausir og í mögulegu vinnuafli.

slaki_tbl <- read_csv2("https://px.hagstofa.is:443/pxis/sq/55b928be-b958-4963-bb76-0cb8624eb7d6") %>% 
  set_names("date", "Slaki á vinnumarkaði") %>% 
  mutate(date = fix_date(date)) %>% 
  pivot_longer(cols = -date) %>% 
  filter(date >= date_back)


# 2.6.0 Vinnustundir ------------------------------------------------------

vinnustundir_tbl <- read_csv2("https://px.hagstofa.is:443/pxis/sq/bbbb2a35-e815-4c16-b022-c10004a529a5") %>% 
  set_names("date", "kyn", "eining", "Vinnustundir") %>% 
  select(-eining)

vinnustundir_tbl <- vinnustundir_tbl %>% 
  mutate(
    date = fix_date(date),
    vinnustundir = Vinnustundir / 10
    ) %>% 
  select(date, vinnustundir, kyn) %>% 
  filter(date >= date_back, kyn == "Alls") %>% 
  select(-Kyn)


# 2.7.0 Hlutafll af vinnumarkaði ------------------------------------------

starfandi_tbl <- read_csv2("https://px.hagstofa.is:443/pxis/sq/1fe9c9fd-2cb7-4e64-bd23-d738794ac4b3") %>% 
  janitor::clean_names() %>% 
  select(-c(aldur, bakgrunnur, logheimili)) %>% 
  set_names("date", "kyn", "alls", "opinberi")

starfandi_tbl <- starfandi_tbl %>% 
  mutate(
    almenni = alls - opinberi,
    date = fix_date(date)
    ) %>% 
  select(date, kyn, almenni)


# VR
skilagreinar_tbl <- tbl(con, "skilagreinar") %>%
  as_tibble()

adjust_fjoldi <- function(x) {
  
  x %>%
    mutate(dagar_fra = as.numeric(today() - date)) %>%
    left_join(skilagreinar_tbl) %>%
    mutate(
      margfoldunarstudull = if_else(is.na(margfoldunarstudull), 1, margfoldunarstudull),
      fjoldi = fjoldi * margfoldunarstudull
    )
}


vr_starfandi_tbl <- tbl(con, "FG_V") %>%
  filter(INNH %in% c("A", ".")) %>%
  select(ID, ARMA) %>%
  left_join(tbl(con, "FE_V") %>% select(ID, KYN)) %>% 
  group_by(ARMA, KYN) %>%
  summarise(fjoldi = n_distinct(ID)) %>%
  as_tibble() %>%
  janitor::clean_names() %>%
  mutate(date = make_date(str_sub(arma, 1, 4), str_sub(arma, 5, 6))) %>%
  arrange(date) %>%
  adjust_fjoldi %>%
  filter(date >= date_back, date <= max_date) %>%
  select(date, fjoldi, kyn) %>% 
  mutate(kyn = if_else(kyn == "Karl", "Karlar", "Konur"))


hlutfall_tbl <- vr_starfandi_tbl %>% 
  left_join(starfandi_tbl) %>% 
  mutate(hlutfall = fjoldi / almenni)



# 2.8.0 Erlent ríkisfang --------------------------------------------------

erlent_rikisfang_tbl <- tbl(con, "FG_V") %>%
  filter(INNH %in% c("A", ".")) %>%
  select(ID, ARMA) %>%
  left_join(tbl(con, "FE_V") %>% select(ID, HEITI)) %>%
  mutate(rikisfang = if_else(HEITI == "Ísland", "islenskt", "erlent")) %>% 
  group_by(ARMA, rikisfang) %>%
  summarise(fjoldi = n_distinct(ID), .groups = "drop") %>%
  as_tibble() %>% 
  janitor::clean_names() %>%
  mutate(date = make_date(str_sub(arma, 1, 4), str_sub(arma, 5, 6))) %>%
  drop_na() %>% 
  pivot_wider(names_from = rikisfang, values_from = fjoldi) %>%
  mutate(hlutfall = erlent / (erlent + islenskt)) %>%
  filter(date >= date_back, date <= max_date) %>% 
  arrange(date)
  


# 2.9.0 Aldurssamsetning --------------------------------------------------

aldursskipting_og_kyn_tbl <- tbl(con, "FG_V") %>% 
  filter(INNH %in% c("A", ".")) %>%
  select(ID, ARMA, ALDUR) %>%
  left_join(tbl(con, "FE_V") %>% select(ID, KYN)) %>% 
  janitor::clean_names() %>% 
  mutate(
    aldurshopur = 
      case_when(
        aldur < 25 ~ "Undir 25 ára",
        aldur < 35 ~ "25-34 ára",
        aldur < 45 ~ "35-44 ára",
        aldur < 55 ~ "45-54 ára",
        TRUE ~ "55 ára og eldri"
        )
    ) %>% 
  group_by(arma, aldurshopur) %>% 
  summarise(fjoldi = n_distinct(id)) %>% 
  as_tibble() %>% 
  mutate(date = make_date(str_sub(arma, 1, 4), str_sub(arma, 5, 6))) %>% 
  select(-arma)

aldursskipting_og_kyn_tbl <- aldursskipting_og_kyn_tbl %>% 
  filter(date >= date_back, date <= max_date) %>% 
  arrange(date)

# 2.10.0 Ferðaþjónustan ---------------------------------------------------

fjoldi_ferdamanna_tbl <- read_csv2("https://px.hagstofa.is:443/pxis/sq/567d0d46-4486-4c6e-b1c3-21ff5e6d6ee4") %>% 
  set_names("date", "Íslendingar", "Útlendingar") %>% 
  pivot_longer(-date) %>% 
  mutate(date = fix_date(date)) %>% 
  filter(date >= date_back)

gistinaetur_tbl <- read_csv2("https://px.hagstofa.is:443/pxis/sq/09599522-b580-439e-8ad9-2e8c6ecf3388") %>% 
  janitor::clean_names() %>% 
  left_join(months_tbl) %>% 
  mutate(date = make_date(ar, man_no))
  
gistinaetur_tbl <- gistinaetur_tbl %>% 
  select(-c(ar, manudur, man_no)) %>% 
  set_names("Gistinætur", "date") %>% 
  pivot_longer(cols = -date) %>% 
  filter(date >= date_back) %>% 
  mutate(value = as.numeric(value)) %>% 
  drop_na()


# 2.11.0 Atvinnulausir ----------------------------------------------------

# 2.11.1 Hagstofa ---------------------------------------------------------

unemp_hagstofa_tbl <- read_csv2("https://px.hagstofa.is:443/pxis/sq/de06aa6a-7666-49dc-a910-f370e6eba608") %>% 
  set_names("date", "mannfjoldi", "atvinnulausir") %>% 
  mutate(
    date = fix_date(date),
    Hagstofa = atvinnulausir / mannfjoldi
  )

# 2.11.2 Vinnumálastofnun -------------------------------------------------

# URL to scrape
url <- "https://island.is/s/vinnumalastofnun/maelabord-og-toelulegar-upplysingar"

# Read and parse the HTML content
page <- read_html(url)

# Find all links ending in .xlsx
xlsx_links <- page %>%
  html_elements("a") %>%
  html_attr("href") %>%
  str_subset("\\.xlsx$")

# If there are relative paths, prefix with domain
xlsx_links <- ifelse(str_starts(xlsx_links, "http"), xlsx_links, paste0("https://island.is", xlsx_links))


# Example: Download and read the first .xlsx file
if (length(xlsx_links) > 0) {
  temp_file <- tempfile(fileext = ".xlsx")
  download.file(xlsx_links[1], temp_file, mode = "wb")
  vmst_unemp_tbl <- read_excel(temp_file, sheet = 5, skip = 5, range = "D6:XO8")
}


vmst_unemp_tbl <- vmst_unemp_tbl %>% 
  pivot_longer(cols = everything()) %>% 
  drop_na()

vmst_date <- seq.Date(as.Date("2000-02-01"), length.out = nrow(vmst_unemp_tbl), by = "month")


vmst_unemp_tbl <- vmst_unemp_tbl %>% 
  mutate(date = vmst_date) %>% 
  select(date, value) %>% 
  rename("Vinnumálastofnun" = "value")



# 2.11.3 Sameina ----------------------------------------------------------

unemp_tbl <- unemp_hagstofa_tbl %>% 
  left_join(vmst_unemp_tbl) %>% 
  mutate(Vinnumálastofnun = Vinnumálastofnun / mannfjoldi) %>% 
  select(date, Hagstofa, Vinnumálastofnun) %>% 
  pivot_longer(cols = -date) %>% 
  filter(date >= date_back)



# 2.12.0 Stýrivextir ------------------------------------------------------

ds <- get_datasets()

ds_policy <- ds %>%
  filter(str_detect(name, regex("policy rates", ignore_case = TRUE)))

ds_policy %>% select(id, name)


policy_url <- ds$url[ds$id == "WS_CBPOL_csv_flat"]      # monthly
# policy_url <- ds$url[ds$id == "WS_CBPOL_D_csv_flat"]  # daily

policy_raw <- get_bis(policy_url, quiet = TRUE)

styrivextir_tbl <- policy_raw %>% 
  select(ref_area, time_period, obs_value) %>% 
  mutate(
    date    = ym(time_period),
    country = str_remove(ref_area, "^[A-Z]{2}:\\s*"),
    policy_rate = obs_value / 100
  ) %>% 
  select(date, country, policy_rate) %>% 
  drop_na() %>% 
  filter(date >= date_back) %>% 
  janitor::clean_names() %>% 
  left_join(lond_tbl) %>% 
  drop_na()


styrivextir_tbl <- styrivextir_tbl %>% 
  filter(land %in% c("Ísland", "Noregur", "Danmörk", "Svíþjóð", "Finnland", "Bandaríkin"))


# 3.0.0 Save data ---------------------------------------------------------

data_ls <- list(
  "verdbolga" = vnv_tbl,
  "laun" = laun_tbl,
  "kaupmattur" = kaupmattur_vr_tbl,
  "slaki_vinnum" = slaki_tbl,
  "vinnustundir" = vinnustundir_tbl,
  "vr_hlutfall" = hlutfall_tbl,
  "erlent_rikisfang" = erlent_rikisfang_tbl,
  "aldursskipting_vr" = aldursskipting_og_kyn_tbl,
  "fjoldi_ferdamanna" = fjoldi_ferdamanna_tbl,
  "fjodi_gistinatta" = gistinaetur_tbl,
  "atvinnuleysi" = unemp_tbl,
  "styrivextir" = styrivextir_tbl
)


data_ls %>% 
  write_rds("01_clean-data/data.rds")

