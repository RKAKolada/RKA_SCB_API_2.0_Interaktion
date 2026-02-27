

# Extrahera metadata ------------------------------------------------------
# Mata in TAB-ID så returnerar den en dataframe med alla möjliga
# variabelkombinationer exklusive år och region. Både med kod och label.

fetch_scb_metadata <- function(table_id) {
  url <- paste0("https://statistikdatabasen.scb.se/api/v2/tables/", 
                table_id, "/metadata")
  response <- httr::GET(url)
  
  if (httr::status_code(response) != 200) {
    stop("Misslyckades att hämta metadata. Kod: ", httr::status_code(response))
  }
  
  metadata <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
  parameters <- metadata$id
  params_to_combine <- setdiff(parameters, c("Tid", "Region"))
  
  param_lists <- list()
  param_labels <- list()
  
  for (param in params_to_combine) {
    if (!is.null(metadata$dimension[[param]]$category$label)) {
      codes <- names(metadata$dimension[[param]]$category$label)
      labels <- unlist(metadata$dimension[[param]]$category$label)
      param_lists[[param]] <- codes
      param_labels[[param]] <- labels
    }
  }
  
  # Skapar alla kombinationer
  combinations_df <- expand.grid(param_lists, stringsAsFactors = FALSE)
  
  # Lägger in läsbara etiketter
  for (param in params_to_combine) {
    label_col_name <- paste0(param, "_label")
    combinations_df[[label_col_name]] <- param_labels[[param]][
      match(combinations_df[[param]], names(param_labels[[param]]))
    ]
  }
  combinations_df <- combinations_df |> dplyr::mutate(kompID = "")
  return(combinations_df)
}



# Hämta data --------------------------------------------------------------
# Mata in tab-ID och år samt sökväg till metadatafilen
# Funktionen innehåller en hårdkodad limit på 125 000 celler
# SCB:s API tillåter egentligen 150 000 men denna tar lite höjd för om
# antalet regioner skulle vara varierande

fetch_scb_data <- function(tab_id, year, metadata_path) {
  if (!require("httr")) install.packages("httr")
  if (!require("jsonlite")) install.packages("jsonlite")
  if (!require("readxl")) install.packages("readxl")
  
  if (!file.exists(metadata_path)) {
    stop(paste("Metadata file not found:", metadata_path))
  }
  
  metadata <- readxl::read_excel(metadata_path)
  metadata_filtered <- metadata[grepl("K", metadata$kompID), ]
  
  if (nrow(metadata_filtered) == 0) {
    stop("No rows found with 'K' in kompID column")
  }
  
  # Intern cell-limit - satt lägre än SCB:s 150 000 som säkerhetsmarginal
  # Region med "*" antas max vara 350 enheter men är i praktiken oftast 312
  CELL_LIMIT <- 125000L
  
  all_cols <- names(metadata_filtered)
  variable_cols <- all_cols[!grepl("_label|kompID", all_cols, ignore.case = TRUE)]
  
  # ---- Bygg selection ----
  selection <- list()
  selection[[length(selection) + 1]] <- list(variableCode = "Region", valueCodes = I(c("*")))
  
  for (col in variable_cols) {
    unique_values <- unique(na.omit(as.character(metadata_filtered[[col]])))
    if (length(unique_values) == 0) next
    
    selection_item <- list(variableCode = col)
    
    codelist_col <- paste0(col, "_codeList")
    if (codelist_col %in% names(metadata_filtered)) {
      codelist_values <- unique(na.omit(as.character(metadata_filtered[[codelist_col]])))
      if (length(codelist_values) > 0) {
        selection_item$codeList <- codelist_values[1]
      }
    }
    
    selection_item$valueCodes <- I(unique_values)
    selection[[length(selection) + 1]] <- selection_item
  }
  
  selection[[length(selection) + 1]] <- list(variableCode = "Tid", valueCodes = I(c(as.character(year))))
  
  api_url <- paste0(
    "https://statistikdatabasen.scb.se/api/v2/tables/",
    tab_id,
    "/data?lang=sv&outputFormat=json-stat2"
  )
  
  # ---- Räkna celler för en selection ----
  # Region med "*" uppskattas konservativt till 350
  count_cells <- function(sel) {
    counts <- sapply(sel, function(x) {
      if (x$variableCode == "Region" && length(x$valueCodes) == 1 && x$valueCodes == "*") {
        return(350L)
      }
      length(x$valueCodes)
    })
    prod(counts, na.rm = TRUE)
  }
  
  # ---- Parsa API-svar till grid ----
  parse_response <- function(response) {
    response_content <- httr::content(response, "text", encoding = "UTF-8")
    json_data <- jsonlite::fromJSON(response_content)
    
    dimensions <- json_data$dimension
    dim_names <- names(dimensions)
    
    dim_labels_list <- lapply(dim_names, function(dim) {
      dimensions[[dim]]$category$label
    })
    names(dim_labels_list) <- dim_names
    
    grid <- expand.grid(rev(dim_labels_list), stringsAsFactors = FALSE)
    grid <- grid[, rev(names(grid)), drop = FALSE]
    
    if ("Region" %in% dim_names) {
      region_codes <- names(dimensions$Region$category$label)
      n_regions <- length(region_codes)
      n_other_dims <- nrow(grid) / n_regions
      region_kod_column <- rep(region_codes, each = n_other_dims)
      region_col_index <- which(names(grid) == "Region")
      
      if (region_col_index < ncol(grid)) {
        grid <- cbind(
          grid[, 1:region_col_index, drop = FALSE],
          Region_kod = region_kod_column,
          grid[, (region_col_index + 1):ncol(grid), drop = FALSE]
        )
      } else {
        grid$Region_kod <- region_kod_column
      }
    }
    
    grid$value <- as.vector(json_data$value)
    grid
  }
  
  # ---- Gör ett enskilt API-anrop ----
  do_request <- function(sel) {
    request_body <- list(selection = sel)
    cat("API URL:", api_url, "\n")
    cat("Request body:\n")
    cat(jsonlite::toJSON(request_body, auto_unbox = TRUE, pretty = TRUE), "\n\n")
    
    response <- httr::POST(
      url = api_url,
      body = jsonlite::toJSON(request_body, auto_unbox = TRUE),
      httr::content_type_json(),
      encode = "json"
    )
    
    if (httr::status_code(response) != 200) {
      stop(paste(
        "API request failed with status code:", httr::status_code(response),
        "\nResponse:", httr::content(response, "text", encoding = "UTF-8"),
        "\nRequest body:", jsonlite::toJSON(request_body, auto_unbox = TRUE, pretty = TRUE)
      ))
    }
    
    parse_response(response)
  }
  
  # ---- Dela upp selection om för många celler ----
  fetch_recursive <- function(sel) {
    n_cells <- count_cells(sel)
    
    if (n_cells <= CELL_LIMIT) {
      message(sprintf("Hämtar anrop med ~%s celler", format(n_cells, big.mark = " ")))
      return(do_request(sel))
    }
    
    # Hitta variabel att chunka på: flest värden, ej Region eller Tid
    skip_vars <- c("Region", "Tid")
    candidate_sizes <- sapply(sel, function(x) {
      if (x$variableCode %in% skip_vars) return(0L)
      length(x$valueCodes)
    })
    
    split_index <- which.max(candidate_sizes)
    split_var   <- sel[[split_index]]$variableCode
    split_vals  <- sel[[split_index]]$valueCodes
    
    other_cells <- n_cells / length(split_vals)
    chunk_size  <- max(1L, floor(CELL_LIMIT / other_cells))
    chunks      <- split(split_vals, ceiling(seq_along(split_vals) / chunk_size))
    
    message(sprintf(
      "Anropet har ~%s celler (limit %s) - delar upp '%s' (%d värden) i %d chunks om max %d värden",
      format(n_cells, big.mark = " "), format(CELL_LIMIT, big.mark = " "),
      split_var, length(split_vals), length(chunks), chunk_size
    ))
    
    all_results <- lapply(seq_along(chunks), function(i) {
      message(sprintf("  Chunk %d/%d för '%s'", i, length(chunks), split_var))
      sel_temp <- sel
      sel_temp[[split_index]]$valueCodes <- I(chunks[[i]])
      fetch_recursive(sel_temp)
    })
    
    do.call(rbind, all_results)
  }
  
  # ---- Hantera ContentsCode med flera värden ----
  contents_index <- which(sapply(selection, function(x) x$variableCode) == "ContentsCode")
  
  if (length(contents_index) == 1 && length(selection[[contents_index]]$valueCodes) > 1) {
    message("Flera ContentsCodes - delar upp per ContentsCode")
    contents_values <- selection[[contents_index]]$valueCodes
    
    all_results <- lapply(contents_values, function(code) {
      message("Hämtar ContentsCode: ", code)
      sel_temp <- selection
      sel_temp[[contents_index]]$valueCodes <- I(code)
      fetch_recursive(sel_temp)
    })
    
    return(do.call(rbind, all_results))
  }
  
  fetch_recursive(selection)
}



# Hårdkodade kommun/region/riket-listan -----------------------------------
# Används för filtrering
allregcodes <- c(
  "00","01","03","04","05","06","07","08","09","10","12","13","14","17",
  "18","19","20","21","22","23","24","25",
  "0114","0115","0117","0120","0123","0125","0126","0127","0128","0136",
  "0138","0139","0140","0160","0162","0163","0180","0181","0182","0183",
  "0184","0186","0187","0188","0191","0192","0305","0319","0330","0331",
  "0360","0380","0381","0382","0428","0461","0480","0481","0482","0483",
  "0484","0486","0488","0509","0512","0513","0560","0561","0562","0563",
  "0580","0581","0582","0583","0584","0586","0604","0617","0642","0643",
  "0662","0665","0680","0682","0683","0684","0685","0686","0687","0760",
  "0761","0763","0764","0765","0767","0780","0781","0821","0834","0840",
  "0860","0861","0862","0880","0881","0882","0883","0884","0885","0980",
  "1060","1080","1081","1082","1083","1214","1230","1231","1233","1256",
  "1257","1260","1261","1262","1263","1264","1265","1266","1267","1270",
  "1272","1273","1275","1276","1277","1278","1280","1281","1282","1283",
  "1284","1285","1286","1287","1290","1291","1292","1293","1315","1380",
  "1381","1382","1383","1384","1401","1402","1407","1415","1419","1421",
  "1427","1430","1435","1438","1439","1440","1441","1442","1443","1444",
  "1445","1446","1447","1452","1460","1461","1462","1463","1465","1466",
  "1470","1471","1472","1473","1480","1481","1482","1484","1485","1486",
  "1487","1488","1489","1490","1491","1492","1493","1494","1495","1496",
  "1497","1498","1499","1715","1730","1737","1760","1761","1762","1763",
  "1764","1765","1766","1780","1781","1782","1783","1784","1785","1814",
  "1860","1861","1862","1863","1864","1880","1881","1882","1883","1884",
  "1885","1904","1907","1960","1961","1962","1980","1981","1982","1983",
  "1984","2021","2023","2026","2029","2031","2034","2039","2061","2062",
  "2080","2081","2082","2083","2084","2085","2101","2104","2121","2132",
  "2161","2180","2181","2182","2183","2184","2260","2262","2280","2281",
  "2282","2283","2284","2303","2305","2309","2313","2321","2326","2361",
  "2380","2401","2403","2404","2409","2417","2418","2421","2422","2425",
  "2460","2462","2463","2480","2481","2482","2505","2506","2510","2513",
  "2514","2518","2521","2523","2560","2580","2581","2582","2583","2584")