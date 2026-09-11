## =============================================================================
## GEODEP - SHINY APP
## =============================================================================

library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(shiny)
library(leaflet)
library(htmltools)
library(DT)
library(sf)
library(readr)

here::i_am('geodep_shiny.Rproj')

## -----------------------------------------------------------------------
## 0. Load precomputed inputs
## -----------------------------------------------------------------------
geodep_inputs <- readRDS("1_Data/geodep_shiny_inputs.rds")

DEP_YEAR                  <- geodep_inputs$DEP_YEAR
eu_countries               <- geodep_inputs$eu_countries
sector_names                <- geodep_inputs$sector_names
sector_choices                <- geodep_inputs$sector_choices
world_polygons                  <- geodep_inputs$world_polygons
iso_name_lookup                   <- geodep_inputs$iso_name_lookup
country_choices                     <- geodep_inputs$country_choices
country_choices_ui                    <- geodep_inputs$country_choices_ui
dep_import_base                         <- geodep_inputs$dep_import_base
dep_export_base                           <- geodep_inputs$dep_export_base
traded_by_country                           <- geodep_inputs$traded_by_country
traded_by_country_export                      <- geodep_inputs$traded_by_country_export
imports_sector_long_all                         <- geodep_inputs$imports_sector_long_all

rm(geodep_inputs); gc()

sector_choices_ui <- list(
  "All Sectors" = "all",
  "Strategic Sector" = c(
    "All Strategic Sectors"        = "sect_strategic",
    "\u2003Critical Raw Materials" = "sect_crm",
    "\u2003Dual Use"               = "sect_dual_use",
    "\u2003Health"                 = "sect_health",
    "\u2003Agrifood"               = "sect_agrifood",
    "\u2003Energy"                 = "sect_energy"
  ),
  "Other" = c("Other" = "sect_other")
)

## -----------------------------------------------------------------------
## 1. Small helpers 
## -----------------------------------------------------------------------
to_eun <- function(iso3) {
  if_else(iso3 %in% eu_countries, "EUN", iso3)
}

iso_display_name <- function(iso3) {
  case_match(
    iso3,
    "EUN" ~ "European Union",
    .default = iso3
  )
}

iso_name <- function(iso3) {
  matched <- iso_name_lookup$name[match(iso3, iso_name_lookup$iso_a3)]
  ifelse(is.na(matched), iso3, matched)
}

## -----------------------------------------------------------------------
## 2. UI
## -----------------------------------------------------------------------

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: #eef3f1;
        font-family: 'Helvetica Neue', Arial, sans-serif;
        color: #2b2b2b;
        padding-bottom: 40px;
      }

      .container-fluid {
        max-width: 1400px;
        padding-top: 10px;
        padding-left: 30px;
        padding-right: 30px;
      }

      .title-banner {
        background-color: #1f6f5c;
        color: #ffffff;
        padding: 28px 34px;
        margin: -10px -30px 30px -30px;
        border-bottom: 4px solid #8b3a3a;
      }
      .title-banner h1 {
        margin: 0;
        font-size: 30px;
        font-weight: 700;
        letter-spacing: 0.5px;
      }

      .intro-text {
        background-color: #dce8e4;
        border-left: 4px solid #1f6f5c;
        padding: 18px 22px;
        border-radius: 4px;
        margin-bottom: 30px;
        color: #2b2b2b;
        line-height: 1.6;
      }

      h3, h4 {
        color: #1f6f5c;
      }
      h3 {
        border-bottom: 2px solid #dce8e4;
        padding-bottom: 10px;
        margin-top: 40px;
        margin-bottom: 20px;
      }

      label {
        color: #1f6f5c;
        font-weight: 600;
        margin-bottom: 8px;
      }

      .section-block {
        margin-bottom: 36px;
      }

      .btn {
        background-color: #1f6f5c;
        color: #ffffff;
        border: none;
        border-radius: 3px;
        font-weight: 600;
      }
      .btn:hover, .btn:focus {
        background-color: #164f42;
        color: #ffffff;
      }
      #reset_button {
        background-color: #6c7a76;
      }
      #reset_button:hover {
        background-color: #56635f;
      }
      #info_button {
        background-color: #8b3a3a;
      }
      #info_button:hover {
        background-color: #6e2c2c;
      }
      #download_table {
        background-color: #8b3a3a;
        width: 100%;
        display: block;
        text-align: center;
      }
      #download_table:hover {
        background-color: #6e2c2c;
      }
      #swap_button {
        padding-left: 14px;
        padding-right: 14px;
      }

      .form-group {
        margin-bottom: 20px;
      }
      .form-control, .selectize-input {
        border: 1px solid #b7cdc6;
        border-radius: 3px;
        padding: 8px 10px;
      }
      .form-control:focus, .selectize-input.focus {
        border-color: #1f6f5c;
        box-shadow: 0 0 0 2px rgba(31, 111, 92, 0.2);
      }
      
      select#sector_filter optgroup {
        font-weight: 700;
        font-style: normal;
      color: #1f6f5c;
      }
      
      select#sector_filter option {
        font-weight: normal;
        color: #2b2b2b;
      }
      .radio-inline, .radio label {
        color: #2b2b2b;
        font-weight: normal;
      }
      .radio {
        margin-bottom: 6px;
      }

      .well {
        background-color: #f4f8f6;
        border: 1px solid #cfe0da;
        border-left: 5px solid #8b3a3a;
        border-radius: 4px;
        padding: 20px 24px;
        margin-bottom: 20px;
      }
      
      #dependency_map {
        border: 1px solid #b7cdc6;
        border-radius: 4px;
        margin-bottom: 30px;
        box-sizing: border-box;
      }

      .leaflet-control.info.legend {
        font-size: 11px;
        padding: 6px 10px;
        line-height: 1.3;
        max-width: 170px;
      }
      .leaflet-control.info.legend strong {
        font-size: 11px;
      }

      hr {
        border-top: 1px solid #cfe0da;
        margin: 34px 0;
      }

      table.dataTable thead th {
        background-color: #1f6f5c;
        color: #ffffff;
        padding: 10px 12px;
      }
      table.dataTable tbody td {
        padding: 8px 12px;
      }
      .dataTables_wrapper {
        margin-top: 10px;
      }

      .app-layout {
        display: flex;
        gap: 24px;
        align-items: flex-start;
      }
      .sidebar-panel {
        flex: 0 0 300px;
        background-color: #ffffff;
        border: 1px solid #cfe0da;
        border-radius: 6px;
        padding: 22px;
        box-sizing: border-box;
        min-height: 650px;
        display: flex;
        flex-direction: column;
      }
      .main-panel {
        flex: 1;
        min-width: 0;
      }
      .sidebar-section {
        margin-bottom: 22px;
      }
      .sidebar-section:last-child {
        margin-bottom: 0;
      }
      .swap-wrap {
        display: flex;
        justify-content: center;
        margin: 2px 0 18px 0;
      }
      .sidebar-actions {
        display: flex;
        gap: 8px;
      }
      .sidebar-actions .btn {
        flex: 1;
      }
      
      @media (max-width: 768px) {
        .app-layout {
          flex-direction: column;
        }
        .sidebar-panel {
          flex: none;
          width: 100%;
          min-height: 0;
          margin-bottom: 20px;
        }
        .title-banner h1 {
          font-size: 24px;
        }
      }
    "))
  ),
  
  div(class = "title-banner",
      h1("GeoDep \u2014 Trade Dependencies")
  ),
  
  div(class = "intro-text",
      p("Click a country on the map to select it as the Importer (Destination); click a second country to select it as the Exporter (Origin). You can also search by name below. Click a third time on the map, or use Reset, to start over. EU-27 member states are treated as a single entity (EUN).")
  ),
  
  div(class = "app-layout",
      div(class = "sidebar-panel",
          div(class = "sidebar-section",
              radioButtons("dep_direction", "Show dependencies for:",
                           choices = c("Imports" = "import", "Exports" = "export"),
                           selected = "import")
          ),
          div(class = "sidebar-section",
              selectInput("sector_filter", "Sector:",
                          choices = sector_choices_ui, selected = "all", width = "100%")
          ),
          div(class = "sidebar-section",
              radioButtons("map_metric", "Map shows:",
                           choices = c("Share of products" = "count",
                                       "Share of trade value" = "value"),
                           selected = "count")
          ),
          div(class = "sidebar-section",
              selectizeInput("importer_select", "Importer (Destination):",
                             choices = country_choices_ui, selected = "",
                             options = list(placeholder = "Type a country name..."),
                             width = "100%"),
              div(class = "swap-wrap",
                  actionButton("swap_button", "\u21c5 Swap")
              ),
              selectizeInput("exporter_select", "Exporter (Origin):",
                             choices = country_choices_ui, selected = "",
                             options = list(placeholder = "Type a country name..."),
                             width = "100%")
          ),
          div(class = "sidebar-section sidebar-actions",
              actionButton("reset_button", "Reset selection"),
              actionButton("info_button", "\u2139 Methodology")
          ),
          div(class = "sidebar-section",
              style = "margin-top: auto;",
              conditionalPanel(
                condition = "input.importer_select !== ''",
                downloadButton("download_table", "Download Table (CSV)")
              )
          )
      ),
      
      div(class = "main-panel",
          leafletOutput("dependency_map", height = "650px"),
          
          div(class = "section-block",
              uiOutput("partners_panel"),
              hr(),
              DTOutput("dependency_table")
          )
      )
  )
)

## -----------------------------------------------------------------------
## 3. Server
## -----------------------------------------------------------------------

server <- function(input, output, session) {
  selected_countries <- reactiveVal(character())
  
  observeEvent(input$info_button, {
    showModal(modalDialog(
      title = "Methodology",
      size = "l",
      p("This app is built on the GeoDep database (CEPII), using the methodology described in ",
        tags$a(href = "https://www.cepii.fr/CEPII/fr/publications/pb/abstract.asp?NoDoc=14223",
               target = "_blank",
               "Lefebvre & Wibaux (2024), \u201cImport Dependencies: Where Does the EU Stand?\u201d, CEPII Policy Brief n\u00b02024-47"),
        "."),
      p("A product (HS 6-digit) is classified as ", tags$strong("import-dependent"),
        " for a country only if it meets all four of the following criteria at once:"),
      tags$ol(
        tags$li(tags$strong("Import concentration : "),
                " a Herfindahl-Hirschman Index (HHI) computed on the country's import shares by origin exceeds 0.4, meaning its supply of the product is concentrated among few trading partners."),
        tags$li(tags$strong("World export concentration : "),
                " an HHI computed on world export shares (by exporting country) for that product also exceeds 0.4, meaning few countries in the world are even capable of supplying it \u2014 so switching to an alternative supplier is hard, not just currently avoided."),
        tags$li(tags$strong("Non-substitutability by domestic supply : "),
                " the ratio of the country's imports to its own exports of the product is above 1. This assumes a country's exports of a good broadly proxy the domestic production that could, in principle, be redirected to satisfy domestic demand instead; if imports exceed exports, domestic capacity cannot realistically cover the shortfall."),
        tags$li(tags$strong("Persistence : "),
                " all three criteria above must hold in at least two of the last three years, so a one-off or temporary spike in concentration does not count as a structural dependency.")
      ),
      p("The same four criteria, applied symmetrically, define ", tags$strong("export-dependent"),
        " products: import concentration and world import concentration replace the export-side equivalents, and the roles of imports and exports are reversed in the non-substitutability ratio. Use the \u201cShow dependencies for\u201d toggle above the map to switch between the two views."),
      p("On the map, exposure is shown either as the share of traded HS6 products for which the country is dependent (\u201cShare of products\u201d), or as the share of its total trade value concentrated in those dependent products (\u201cShare of trade value\u201d)."),
      p("When an Importer and an Exporter are both selected, the partner panels list their top-3 ",
        tags$em("bilateral"), " dependency partners \u2014 an additional, stricter criterion applied on top of the four above: for the Importer, the exporters supplying more than 50% of a given dependent product's import value; for the Exporter, the destinations absorbing more than 50% of a given dependent product's export value."),
      p("Sector groupings in this app (Critical Raw Materials, Dual Use, Health, Agrifood, Energy, Other) come from dedicated reference lists (UNCTAD, EU dual-use regulation, CEPII health nomenclature, FAO, World Bank); \u201cStrategic Sector\u201d groups all products in any of these five sectors together, and is not identical to the broader set of \u201cstrategic sectors\u201d (based on the EU's strategic ecosystems) used in the CEPII policy brief."),
      p("Figures reflect 2024 (the only year for which the dependency indicators are available in this dataset) and EU-27 member states are aggregated into a single entity (EUN)."),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$reset_button, {
    selected_countries(character())
  })
  
  observeEvent(input$sector_filter, {
    selected_countries(character())
  })
  
  observeEvent(input$dep_direction, {
    selected_countries(character())
    
    if (input$dep_direction == "export") {
      updateSelectInput(session, "sector_filter",
                        choices = c("All Sectors" = "all"),
                        selected = "all")
    } else {
      updateSelectInput(session, "sector_filter",
                        choices = sector_choices_ui,
                        selected = "all")
    }
  }, ignoreInit = FALSE)
  
  observeEvent(selected_countries(), {
    sel <- selected_countries()
    imp <- if (length(sel) >= 1) sel[1] else ""
    exp <- if (length(sel) >= 2) sel[2] else ""
    updateSelectizeInput(session, "importer_select", selected = imp)
    updateSelectizeInput(session, "exporter_select", selected = exp)
  }, ignoreInit = TRUE)
  
  observeEvent(input$importer_select, {
    cur <- selected_countries()
    new_imp <- input$importer_select
    new_sel <- c(new_imp, if (length(cur) >= 2) cur[2] else NA)
    new_sel <- new_sel[!is.na(new_sel) & new_sel != ""]
    if (!identical(new_sel, cur)) selected_countries(new_sel)
  }, ignoreInit = TRUE)
  
  observeEvent(input$exporter_select, {
    cur <- selected_countries()
    imp <- if (length(cur) >= 1) cur[1] else NA
    new_sel <- c(imp, input$exporter_select)
    new_sel <- new_sel[!is.na(new_sel) & new_sel != ""]
    if (!identical(new_sel, cur)) selected_countries(new_sel)
  }, ignoreInit = TRUE)
  
  sector_map_data <- reactive({
    direction <- input$dep_direction
    
    base <- if (direction == "import") {
      traded_by_country |>
        rename(iso_plot = iso_d, dep1 = dependant_M_MC_t, dep2 = c4_M_MC, val = import_dpt)
    } else {
      traded_by_country_export |>
        rename(iso_plot = iso_o, dep1 = dependant_X_MC_t, dep2 = c4_X_MC, val = export_opt)
    }
    
    if (input$sector_filter != "all") {
      base <- base |> filter(.data[[input$sector_filter]] == 1)
    }
    
    total_traded <- base |>
      distinct(iso_plot, hs6, val) |>
      group_by(iso_plot) |>
      summarise(n_total = n(), total_value = sum(val, na.rm = TRUE), .groups = "drop")
    
    dependent <- base |>
      filter(dep1 == 1, dep2 == 1) |>
      distinct(iso_plot, hs6, val) |>
      group_by(iso_plot) |>
      summarise(n_dep = n(), dep_value = sum(val, na.rm = TRUE), .groups = "drop")
    
    total_traded |>
      left_join(dependent, by = "iso_plot") |>
      mutate(
        n_dep       = replace_na(n_dep, 0),
        dep_value   = replace_na(dep_value, 0),
        count_share = 100 * n_dep / n_total,
        value_share = 100 * dep_value / total_value
      ) |>
      select(iso_plot, count_share, value_share, n_dep, n_total, dep_value, total_value)
  }) |> bindCache(input$sector_filter, input$dep_direction)
  
  hover_data <- reactive({
    import_base <- dep_import_base
    export_base <- dep_export_base
    
    if (input$sector_filter != "all") {
      import_base <- import_base |> filter(.data[[input$sector_filter]] == 1)
      export_base <- export_base |> filter(.data[[input$sector_filter]] == 1)
    }
    
    import_top3 <- import_base |>
      group_by(iso_d, hs6) |>
      mutate(origin_share = imports / import_dpt) |>
      ungroup() |>
      filter(origin_share > 0.5) |>
      group_by(iso_d, iso_o) |>
      summarise(dep_count = n(), .groups = "drop") |>
      arrange(iso_d, desc(dep_count)) |>
      group_by(iso_d) |>
      slice_head(n = 3) |>
      mutate(part = paste0(iso_name(iso_o), " (", dep_count, ")")) |>
      summarise(import_top3 = paste(part, collapse = "; "), .groups = "drop") |>
      rename(iso_plot = iso_d)
    
    export_top3 <- export_base |>
      group_by(iso_o, hs6) |>
      mutate(dest_share = imports / export_opt) |>
      ungroup() |>
      filter(dest_share > 0.5) |>
      group_by(iso_o, iso_d) |>
      summarise(dep_count = n(), .groups = "drop") |>
      arrange(iso_o, desc(dep_count)) |>
      group_by(iso_o) |>
      slice_head(n = 3) |>
      mutate(part = paste0(iso_name(iso_d), " (", dep_count, ")")) |>
      summarise(export_top3 = paste(part, collapse = "; "), .groups = "drop") |>
      rename(iso_plot = iso_o)
    
    full_join(import_top3, export_top3, by = "iso_plot")
  }) |> bindCache(input$sector_filter)
  
  partner_map_data <- reactive({
    selection <- selected_countries()
    req(length(selection) >= 1)
    iso1 <- selection[1]
    
    if (input$dep_direction == "import") {
      base       <- dep_import_base |> filter(iso_d == iso1)
      partner_col <- "iso_o"
      total_col   <- "import_dpt"
      value_col   <- "imports"
    } else {
      base       <- dep_export_base |> filter(iso_o == iso1)
      partner_col <- "iso_d"
      total_col   <- "export_opt"
      value_col   <- "imports"
    }
    
    if (input$sector_filter != "all") {
      base <- base |> filter(.data[[input$sector_filter]] == 1)
    }
    
    if (nrow(base) == 0) return(NULL)
    
    base <- base |> rename(partner_iso = all_of(partner_col))
    
    totals <- base |>
      distinct(hs6, .data[[total_col]]) |>
      summarise(total_value = sum(.data[[total_col]], na.rm = TRUE), n_products = n())
    
    base |>
      group_by(hs6) |>
      mutate(partner_share = .data[[value_col]] / .data[[total_col]]) |>
      ungroup() |>
      group_by(partner_iso) |>
      summarise(
        supply_value = sum(.data[[value_col]], na.rm = TRUE),
        n_dominant   = n_distinct(hs6[partner_share > 0.5]),
        .groups      = "drop"
      ) |>
      mutate(
        iso_plot    = partner_iso,
        n_dep       = n_dominant,
        n_total     = totals$n_products,
        dep_value   = supply_value,
        total_value = totals$total_value,
        count_share = 100 * n_dominant / totals$n_products,
        value_share = 100 * supply_value / totals$total_value
      ) |>
      select(iso_plot, count_share, value_share, n_dep, n_total, dep_value, total_value)
  })
  
  active_map_data <- reactive({
    sel <- selected_countries()
    if (length(sel) == 0) {
      sector_map_data()
    } else {
      data <- partner_map_data()
      if (is.null(data)) {
        data <- tibble(
          iso_plot = character(), count_share = double(), value_share = double(),
          n_dep = double(), n_total = double(), dep_value = double(), total_value = double()
        )
      }
      data
    }
  })
  
  sector_map_sf <- reactive({
    counts <- active_map_data()
    
    world_polygons |>
      mutate(iso_plot = to_eun(iso_a3)) |>
      mutate(feature_id = paste0(iso_a3, "___", row_number())) |>
      left_join(counts, by = "iso_plot")
  })
  
  output$dependency_map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 2, maxZoom = 6,
                                     maxBoundsViscosity = 1.0)) |>
      addProviderTiles(providers$Esri.WorldGrayCanvas,
                       options = providerTileOptions(noWrap = TRUE)) |>
      setMaxBounds(-180, -85, 180, 85) |>
      htmlwidgets::onRender(
        "function(el, x) {
           L.control.zoom({ position: 'bottomright' }).addTo(this);
           var map = this;
           setTimeout(function() {
             map.invalidateSize();
             map.fitBounds([[-58, -170], [83, 190]]);
           }, 200);
         }" )
  })
  
  observe({
    map_sf <- sector_map_sf()
    metric <- input$map_metric
    sel    <- selected_countries()
    direction_label <- if (input$dep_direction == "import") "import" else "export"
    partner_role    <- if (input$dep_direction == "import") "supplier" else "buyer"
    
    fill_values <- if (metric == "count") map_sf$count_share else map_sf$value_share
    
    if (length(sel) >= 1) {
      iso1_label <- iso_display_name(sel[1])
      metric_label <- if (metric == "count") {
        paste0("Share of ", iso1_label, "'s dependent ", direction_label,
               "s led by each ", partner_role)
      } else {
        paste0("Share of ", iso1_label, "'s dependent ", direction_label,
               " trade value by ", partner_role)
      }
    } else {
      metric_label <- if (metric == "count") {
        paste0("Share of products dependent (", direction_label, "s)")
      } else {
        paste0("Share of trade value dependent (", direction_label, "s)")
      }
    }
    
    pal <- colorNumeric(
      palette  = "plasma",
      domain   = fill_values,
      reverse  = TRUE,
      na.color = "lightgrey"
    )
    
    label_text <- with(map_sf, {
      header <- if_else(iso_plot == "EUN", "European Union (EU-27)", name)
      detail <- if (metric == "count") {
        paste0(
          ifelse(is.na(count_share), "0%", paste0(round(count_share, 1), "%")),
          " (", ifelse(is.na(n_dep), 0, n_dep), " of ", ifelse(is.na(n_total), 0, n_total), " products)"
        )
      } else {
        paste0(ifelse(is.na(value_share), "0%", paste0(round(value_share, 1), "%")), " of trade value")
      }
      paste0("<strong>", header, "</strong><br/>", metric_label, ": ", detail)
    })
    label_arg <- lapply(label_text, HTML)
    
    proxy <- leafletProxy("dependency_map", data = map_sf) |>
      clearShapes() |>
      clearControls() |>
      addPolygons(
        fillColor   = ~pal(fill_values),
        weight      = 1,
        color       = "white",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight      = 2,
          color       = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        layerId      = ~feature_id,
        label        = label_arg,
        labelOptions = labelOptions(
          style     = list("font-size" = "12px", "max-width" = "260px", "white-space" = "normal"),
          textsize  = "12px",
          direction = "auto"
        )
      ) |>
      addLegend(
        position  = "bottomleft",
        pal       = pal,
        values    = fill_values,
        title     = HTML(paste0(
          metric_label, "<br/>(%)",
          "<br/><span style='font-weight:normal; font-size:10px; color:#666;'>Source: GeoDep IFE-CEPII (2026)</span>"
        )),
        labFormat = labelFormat(suffix = "%"),
        na.label  = "0%"
      ) 
    if (length(sel) >= 1) {
      imp_sf <- map_sf |> filter(iso_plot == sel[1])
      if (nrow(imp_sf) > 0) {
        proxy <- proxy |> addPolygons(
          data = imp_sf, fill = FALSE, color = "#1f78b4", weight = 4,
          opacity = 1, layerId = paste0("highlight_importer_", imp_sf$feature_id)
        )
      }
    }
    if (length(sel) >= 2) {
      exp_sf <- map_sf |> filter(iso_plot == sel[2])
      if (nrow(exp_sf) > 0) {
        proxy <- proxy |> addPolygons(
          data = exp_sf, fill = FALSE, color = "#e31a1c", weight = 4,
          opacity = 1, layerId = paste0("highlight_exporter_", exp_sf$feature_id)
        )
      }
    }
  })
  
  observeEvent(input$dependency_map_shape_click, {
    click <- input$dependency_map_shape_click
    raw_iso <- sub("___.*$", "", click$id)
    clicked_iso <- to_eun(raw_iso)
    current_selection <- selected_countries()
    
    if (length(current_selection) >= 2) {
      selected_countries(clicked_iso)
    } else if (clicked_iso %in% current_selection) {
      return(NULL)
    } else {
      selected_countries(c(current_selection, clicked_iso))
    }
  })
  
  observeEvent(input$swap_button, {
    current_selection <- selected_countries()
    if (length(current_selection) == 2) {
      selected_countries(rev(current_selection))
    }
  })
  
  
  sector_chart_data <- reactive({
    selection <- selected_countries()
    req(length(selection) >= 1)
    
    iso1 <- selection[1]
    iso2 <- if (length(selection) >= 2) selection[2] else NA_character_
    
    # leaf sector codes only (drop "all" and the "sect_strategic" umbrella)
    sector_cols <- setdiff(unlist(sector_choices_ui, use.names = FALSE),
                           c("all", "sect_strategic"))
    
    if (input$dep_direction == "import") {
      base <- dep_import_base |>
        filter(iso_d == iso1) |>
        group_by(hs6) |>
        mutate(partner_share = imports / import_dpt) |>
        ungroup() |>
        mutate(is_dominant = !is.na(iso2) & iso_o == iso2 & partner_share > 0.5)
    } else {
      base <- dep_export_base |>
        filter(iso_o == iso1) |>
        group_by(hs6) |>
        mutate(partner_share = imports / export_opt) |>
        ungroup() |>
        mutate(is_dominant = !is.na(iso2) & iso_d == iso2 & partner_share > 0.5)
    }
    
    if (nrow(base) == 0) return(NULL)
    
    dominant_by_hs6 <- base |>
      group_by(hs6) |>
      summarise(is_dominant = any(is_dominant), .groups = "drop")
    
    sectors_by_hs6 <- base |>
      distinct(hs6, across(all_of(sector_cols)))
    
    dominant_by_hs6 |>
      left_join(sectors_by_hs6, by = "hs6") |>
      pivot_longer(cols = all_of(sector_cols), names_to = "sector_code", values_to = "flag") |>
      filter(flag == 1) |>
      mutate(Sector_Name = unlist(sector_names[sector_code])) |>
      group_by(Sector_Name) |>
      summarise(
        n_dep      = n(),
        n_dominant = sum(is_dominant),
        .groups    = "drop"
      ) |>
      mutate(share_dominant = if_else(n_dep > 0, 100 * n_dominant / n_dep, 0))
  })
  
  make_sector_chart <- function() {
    df <- sector_chart_data()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    selection <- selected_countries()
    iso1 <- selection[1]
    iso2 <- if (length(selection) >= 2) selection[2] else NA_character_
    direction_label <- if (input$dep_direction == "import") "exporter" else "importer"
    flow_label       <- if (input$dep_direction == "import") "Import" else "Export"
    
    dominant_label <- if (!is.na(iso2)) paste0("Dominant: ", iso_display_name(iso2)) else "Other"
    
    df_long <- df |>
      mutate(n_other = n_dep - n_dominant) |>
      select(Sector_Name, n_dep, n_dominant, n_other) |>
      pivot_longer(cols = c(n_dominant, n_other), names_to = "category", values_to = "n") |>
      mutate(category = if_else(category == "n_dominant", dominant_label, "Other"))
    
    sector_order <- df |> arrange(n_dep) |> pull(Sector_Name)
    df_long <- df_long |> mutate(Sector_Name = factor(Sector_Name, levels = sector_order))
    
    p <- ggplot(df_long, aes(x = Sector_Name, y = n, fill = category)) +
      geom_col() +
      coord_flip() +
      labs(
        x = NULL, y = "Dependent products",
        title = paste0(flow_label, " dependencies by sector - ", iso_display_name(iso1), " (2024)"),
        subtitle = if (!is.na(iso2)) {
          paste0("Share of dependent products where ", iso_display_name(iso2),
                 " is the dominant ", direction_label, " (>50% of trade value)")
        } else {
          "Select a second country to see its share as dominant partner"
        },
        fill = NULL,
        caption = "Source : GeoDep IFE-CEPII (2026)"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        legend.position = "bottom",
        plot.caption    = element_text(hjust = 1, size = 8, color = "#666666", face = "italic")
      )
    
    if (!is.na(iso2)) {
      p <- p + scale_fill_manual(values = setNames(c("#e31a1c", "#4a86c9"),
                                                   c(dominant_label, "Other")))
    } else {
      p <- p + scale_fill_manual(values = c("Other" = "#4a86c9"), guide = "none")
    }
    
    p
  }
  
  output$sector_chart <- renderPlot({
    make_sector_chart()
  })
  output$download_plot <- downloadHandler(
    filename = function() {
      selection <- selected_countries()
      req(length(selection) >= 1)
      iso1 <- selection[1]
      iso2 <- if (length(selection) >= 2) paste0("_", selection[2]) else ""
      direction <- input$dep_direction
      paste0("sector_chart_", direction, "_", iso1, iso2, "_2024.png")
    },
    content = function(file) {

      p <- make_sector_chart()
      req(p)

      ggsave(filename = file, plot = p, device = "png", 
             width = 10, height = 6, dpi = 300, bg = "white")
    }
  )
  output$partners_panel <- renderUI({
    selection <- selected_countries()
    if (length(selection) == 0) return(NULL)
    
    hover <- hover_data()
    
    make_card_ui <- function(idx) {
      iso <- selection[idx]
      row <- hover |> filter(iso_plot == iso)
      import_txt <- if (nrow(row) == 0 || is.na(row$import_top3[1])) "None identified" else row$import_top3[1]
      export_txt <- if (nrow(row) == 0 || is.na(row$export_top3[1])) "None identified" else row$export_top3[1]
      
      wellPanel(
        h4(iso_display_name(iso)),
        tags$p(tags$em("Depends on for imports (top 3 exporters to it):")),
        tags$p(import_txt),
        tags$p(tags$em("Depends on for exports (top 3 destinations):")),
        tags$p(export_txt)
      )
    }
    
    cards <- if (length(selection) == 1) {
      fluidRow(column(6, make_card_ui(1)))
    } else {
      fluidRow(
        column(6, make_card_ui(1)),
        column(6, make_card_ui(2))
      )
    }
    
    tagList(
      cards,
      plotOutput("sector_chart", height = "320px"),
      div(style = "text-align: right; margin-top: 10px;",
          downloadButton("download_plot", "Download Graph (PNG)", 
                         style = "background-color: #6c7a76; color: white; border: none;")
      )
    )
  })
  
  filtered_dependency_data <- reactive({
    selection <- selected_countries()
    req(length(selection) >= 1)
    
    if (length(selection) == 2) {
      destination <- selection[1]
      origin <- selection[2]
      
      result <- dep_import_base |>
        filter(iso_d == destination, iso_o == origin) |>
        group_by(hs6) |>
        mutate(origin_share = imports / import_dpt) |>
        ungroup() |>
        filter(origin_share > 0.5)
      
      if (input$sector_filter != "all") {
        result <- result |>
          filter(.data[[input$sector_filter]] == 1)
      }
      
      result <- result |>
        mutate(share_odpt = origin_share * 100) |>
        select(
          `HS6 Product` = hs6,
          `Description` = Description,
          `Total Imports (World, k$)` = import_dpt,
          `Imports from Origin (k$)` = imports,
          `Share from Origin (%)` = share_odpt,
          starts_with("sect_")
        ) |>
        arrange(desc(`Imports from Origin (k$)`))
      
      return(result)
    }

    iso1 <- selection[1]
    
    if (input$dep_direction == "import") {
      base        <- dep_import_base |> filter(iso_d == iso1)
      total_col   <- "import_dpt"
      total_label <- "Total Imports (World, k$)"
    } else {
      base        <- dep_export_base |> filter(iso_o == iso1)
      total_col   <- "export_opt"
      total_label <- "Total Exports (World, k$)"
    }
    
    if (input$sector_filter != "all") {
      base <- base |> filter(.data[[input$sector_filter]] == 1)
    }
    
    result <- base |>
      distinct(hs6, Description, .data[[total_col]], across(starts_with("sect_"))) |>
      arrange(desc(.data[[total_col]]))
    
    colnames(result)[colnames(result) == "hs6"]     <- "HS6 Product"
    colnames(result)[colnames(result) == total_col] <- total_label
    
    result
  })
  
  output$dependency_table <- renderDT({
    data <- filtered_dependency_data()
    
    value_cols <- intersect(
      c("Total Imports (World, k$)", "Total Exports (World, k$)", "Imports from Origin (k$)"),
      colnames(data)
    )
    share_col <- intersect("Share from Origin (%)", colnames(data))
    
    order_col <- if ("Imports from Origin (k$)" %in% colnames(data)) {
      "Imports from Origin (k$)"
    } else {
      value_cols[1]
    }
    
    empty_msg <- if (length(selected_countries()) == 2) {
      "No product where the importer is dependent and this exporter is the dominant supplier (>50%)."
    } else {
      "No dependent products found for this selection."
    }
    
    dt <- datatable(
      data,
      rownames  = FALSE,
      selection = "none",
      options = list(
        pageLength = 10,
        language = list(
          search      = "Search:",
          lengthMenu  = "Show _MENU_ entries",
          info        = "Showing _START_ to _END_ of _TOTAL_ entries",
          paginate    = list(previous = "Previous", `next` = "Next"),
          emptyTable  = empty_msg
        ),
        order = list(list(which(colnames(data) == order_col) - 1, "desc"))
      )
    ) |>
      formatCurrency(
        columns  = value_cols,
        currency = "",
        interval = 3,
        mark     = ".",
        digits   = 0
      )
    
    if (length(share_col) == 1) {
      dt <- dt |> formatRound(columns = share_col, digits = 1)
    }
    
    dt
  })
  
  output$download_table <- downloadHandler(
    filename = function() {
      selection <- selected_countries()
      if (length(selection) == 2) {
        paste0("dependencies_", selection[1], "_from_", selection[2], "_",
               Sys.Date(), ".csv")
      } else if (length(selection) == 1) {
        paste0("dependencies_", selection[1], "_", input$dep_direction, "_",
               Sys.Date(), ".csv")
      } else {
        paste0("dependencies_", Sys.Date(), ".csv")
      }
    },
    content = function(file) {
      write_excel_csv2(filtered_dependency_data(), file)
    }
  )
}

shinyApp(ui = ui, server = server)