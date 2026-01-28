library(tidyverse)

#        ▄ ▄
#  ▄▄▄▄  ▄▄▄  ▄▄▄▄    ████▄  ▄▄ ▄▄  ▄▄  ▄▄▄    ▄▄ ▄▄  ▄▄▄  ▄▄
# ██ ▄▄ ██▀██ ██▄█▄   ██  ██ ██ ███▄██ ██▀██   ██▄██ ██▀██ ██
# ▀███▀ ▀███▀ ██ ██   ████▀  ██ ██ ▀██ ██▀██    ▀█▀  ██▀██ ██▄▄▄
#

# Ange TAB-id -------------------------------------------------------------
levid <- "" # <---- Till exempel "L00561A"

# Ange leverans-id --------------------------------------------------------
tabid <- "" # <---- Till exempel "TAB561"

# Ange år -----------------------------------------------------------------
year_to_use <- "" # <---- Till exempel "2024"


# Kör sedan scriptet i sin helhet







# Hämtar Kejsar Zings formelsamling ---------------------------------------
source("https://raw.githubusercontent.com/RKAKolada/RKA_SCB_API_2.0_Interaktion/refs/heads/main/SCB_API_funktionssamling.R")


# Identifierar metadatafilens plats ---------------------------------------
metapath <- paste0(tabid,"_metadata.xlsx")

# Hämtar data -------------------------------------------------------------
scb_data <- fetch_scb_data(tabid, year_to_use, metapath)

# Plattar till dataframen då den består av listor
scb_data <- scb_data %>%
  unnest(cols = everything())



# Läser in tabellens metadatafil ------------------------------------------
# Tar bort kod-kolumnerna vid inläsning
metadata <- read_excel(metapath, trim_ws = FALSE) %>%
  select(ends_with("_label"), kompID)
# Tar bort "_label"-delen från kolumnamnen:
metadata <- metadata %>%
  rename_with( ~ str_remove(.x, "_label$"))
# Filtrerar bort oönskade kombinationer
metadata <- metadata %>% filter(str_starts(kompID, "K"))
# Identifierar vilka koluner som finns i metadatan förutom kompID
variable_cols <- setdiff(names(metadata), "kompID")
# Joinar sedan på hämtad data
scb_data <- scb_data %>%
  left_join(metadata, by = variable_cols)
# Filtrerar bort alla som inte får en komponentID-match
scb_data <- scb_data %>% filter(str_starts(kompID, "K"))



# Transformerar till inläsningsfil ----------------------------------------
inlasfil <- scb_data %>%
  transmute(
    regID = case_when(
      str_length(Region_kod) == 2 ~ paste0("00", Region_kod),
      TRUE ~ Region_kod
    ),
    kompID = kompID,
    year = Tid,
    value = as.character(value),
  )

# Filtrerar bort regioner som inte ingår i 312-listan
inlasfil <- inlasfil %>%
  filter(regID %in% allregcodes)

# Aggregerar rader med samma kompID
inlasfil <- inlasfil %>%
  mutate(value = suppressWarnings(as.numeric(value))) %>%
  group_by(regID, kompID, year) %>%
  summarize(value = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(value = as.character(value))

# Lägger till bortfallskod, bfk
inlasfil <- inlasfil %>%
  mutate(bfk = case_when(is.na(value) ~ "1", TRUE ~ "0")
  
# Blankar vid bfk1
inlasfil$value <- ifelse(inlasfil$bfk == "1", "", inlasfil$value)


# Skriver ut till textfil -------------------------------------------------

# Om inte årsmappen finns skapas den
if(dir.exists(paste0(year_to_use,"/"))) {
  write.table(
    inlasfil,
    file = paste0(
      paste0(year_to_use, "/"),
      tabid,
      "-",
      as.Date(as.POSIXlt(Sys.time())),
      "-",
      Sys.getenv("USERNAME"),
      ".txt"
    ),
    quote = FALSE,
    sep = ";",
    col.names = FALSE,
    row.names = FALSE,
    na = ""
  )
} else {
  dir.create(paste0(year_to_use, "/"))
  
  write.table(
    inlasfil,
    file = paste0(
      paste0(year_to_use, "/"),
      tabid,
      "-",
      as.Date(as.POSIXlt(Sys.time())),
      "-",
      Sys.getenv("USERNAME"),
      ".txt"
    ),
    quote = FALSE,
    sep = ";",
    col.names = FALSE,
    row.names = FALSE,
    na = ""
  )
}

# Clas 2026-01-27


