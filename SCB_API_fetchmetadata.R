library(tidyverse)
library(openxlsx)


# Ange TAB-ID -------------------------------------------------------------
TABID <- ""






# Hämtar Kejsar Zings formelsamling ---------------------------------------
source(
  "https://raw.githubusercontent.com/RKAKolada/RKA_SCB_API_2.0_Interaktion/refs/heads/main/SCB_API_funktionssamling.R"
)


# Hämtar metadata ---------------------------------------------------------
metadata <- fetch_scb_metadata(TABID)



# Skapar excelfil ---------------------------------------------------------

wb <- createWorkbook()
addWorksheet(wb, "metadata")

writeDataTable(
  wb,
  sheet = "metadata",
  x = metadata,
  tableStyle = "TableStyleMedium9"
)

setColWidths(
  wb,
  sheet = "metadata",
  cols = 1:ncol(metadata),
  widths = "auto"
)

saveWorkbook(
  wb,
  paste0(TABID, "_metadata.xlsx"),
  overwrite = TRUE
)

