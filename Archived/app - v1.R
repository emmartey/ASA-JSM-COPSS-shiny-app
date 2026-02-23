#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# app.R
# COPSS Awards + JSM Events Dashboard (ASA-themed) with Awardee Spotlight
# ---------------------------------------------------------------------
# What you get:
#  - Data tab: select Google Sheet tabs + preview + load diagnostics
#  - COPSS Winners tab: filters + plots + clickable DT table
#  - JSM Events tab: filters + plots + table
#  - Integrated tab: COPSS x JSM inner-join on Year
#  - Awardee Spotlight tab: click an awardee row -> video embed + citation +
#    Google Scholar link + OpenAlex collaborators + top cited + most recent + profile draft
#
# Requirements:
#  - Sheets must have these schemas (column headers; case/space tolerant):
#    COPSS: AwardName, Year, Awardee, AwardeeAffiliation, Citation, AwardeeCeremony,
#           AwardPresentationStart, AwardPresentationEnd, AwardeeGoogleScholar,
#           AwardeeHighestDegreeInstitution
#    JSM:   Year, HostCity, HostState, HostCountry, Theme, EventWebsite,
#           PlenarySessionsWebcast, EventLogo
#
# Run:
#  1) install.packages(c("shiny","bslib","dplyr","stringr","lubridate","ggplot2","DT",
#                       "googlesheets4","tidyr","httr2","purrr"))
#  2) shiny::runApp()

library(shiny)
library(bslib)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(DT)
library(googlesheets4)
library(tidyr)
library(httr2)
library(purrr)

# -----------------------------
# CONFIG: Google Sheet URLs
# -----------------------------
SHEET_COPSS <- "https://docs.google.com/spreadsheets/d/1-NNKbCd-ibEYPkzwjT4ZGK19qGap4fU8nnGeuvG_SHc/edit?usp=drive_link"
SHEET_JSM   <- "https://docs.google.com/spreadsheets/d/1-HGUmfa8QGTMfrrn-GJQaHq9Eo2Fp_8m48lr8-HjNzQ/edit?usp=drivesdk"

# Default: public/link access
gs4_deauth()

# -----------------------------
# ASA THEME (from your screenshot palette)
# -----------------------------
asa_theme <- bs_theme(
  version = 5,
  bg = "#F8F9FA",         # off-white
  fg = "#2E3136",         # dark gray text
  primary = "#26364F",    # ASA navy
  secondary = "#31425A",  # deep navy
  info = "#60B0E0",       # ASA light blue accent
  base_font = font_google("Source Sans 3"),
  heading_font = font_google("Source Sans 3")
)

asa_css <- "
/* --- ASA look & feel --- */
.navbar, .navbar-dark, .navbar-light { background-color: #26364F !important; }
.navbar .navbar-brand, .navbar .nav-link { color: #F8F9FA !important; font-weight: 600; }
.navbar .nav-link.active { border-bottom: 3px solid #60B0E0; }

.btn-primary { background-color: #26364F !important; border-color: #26364F !important; }
.btn-primary:hover { background-color: #31425A !important; border-color: #31425A !important; }

a, a:hover, a:focus { color: #3070A4; }

.card { border-radius: 14px; border: 1px solid rgba(49,66,90,0.15); }
.card-header { background-color: #F8F9FA; border-bottom: 1px solid rgba(49,66,90,0.15);
               font-weight: 700; color: #2E3136; }

table.dataTable thead th { background-color: #26364F !important; color: #F8F9FA !important; }
table.dataTable tr.selected td { background-color: rgba(96,176,224,0.22) !important; }

.bslib-sidebar-layout .sidebar {
  background-color: #F8F9FA;
  border-right: 1px solid rgba(49,66,90,0.15);
}
"

asa_gg <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", color = "#2E3136"),
      axis.title = element_text(face = "bold", color = "#2E3136"),
      axis.text = element_text(color = "#2E3136"),
      panel.grid.minor = element_blank()
    )
}

# -----------------------------
# Utility helpers
# -----------------------------
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x

safe_sheet_names <- function(url) {
  tryCatch(sheet_names(url), error = function(e) character(0))
}

safe_read_sheet <- function(url, sheet) {
  tryCatch(read_sheet(url, sheet = sheet),
           error = function(e) tibble(.error = conditionMessage(e)))
}

canon_names <- function(x) {
  x |>
    str_trim() |>
    str_replace_all("\\s+", "") |>
    str_to_lower()
}

# -----------------------------
# Standardizers (schema enforcement)
# -----------------------------
standardize_copss <- function(df) {
  if (".error" %in% names(df)) return(df)
  
  nm_can <- canon_names(names(df))
  
  map <- c(
    awardname                        = "AwardName",
    year                             = "Year",
    awardee                          = "Awardee",
    awardeeaffiliation               = "AwardeeAffiliation",
    citation                         = "Citation",
    awardeeceremony                  = "AwardeeCeremony",
    awardpresentationstart           = "AwardPresentationStart",
    awardpresentationend             = "AwardPresentationEnd",
    awardeegooglescholar             = "AwardeeGoogleScholar",
    awardeehighestdegreeinstitution  = "AwardeeHighestDegreeInstitution"
  )
  
  for (k in names(map)) {
    hit <- which(nm_can == k)
    if (length(hit) == 1) names(df)[hit] <- map[[k]]
  }
  
  required <- unname(map)
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    return(tibble(.error = paste("COPSS sheet missing columns:", paste(missing, collapse = ", "))))
  }
  
  df %>%
    transmute(
      AwardName = as.character(AwardName),
      Year = suppressWarnings(as.integer(Year)),
      Awardee = as.character(Awardee),
      AwardeeAffiliation = as.character(AwardeeAffiliation),
      Citation = as.character(Citation),
      AwardeeCeremony = as.character(AwardeeCeremony),
      AwardPresentationStart = as.character(AwardPresentationStart),
      AwardPresentationEnd = as.character(AwardPresentationEnd),
      AwardeeGoogleScholar = as.character(AwardeeGoogleScholar),
      AwardeeHighestDegreeInstitution = as.character(AwardeeHighestDegreeInstitution)
    ) %>%
    mutate(across(everything(), ~ ifelse(is.na(.x), .x, str_squish(.x))))
}

standardize_jsm <- function(df) {
  if (".error" %in% names(df)) return(df)
  
  nm_can <- canon_names(names(df))
  
  map <- c(
    year = "Year",
    hostcity = "HostCity",
    hoststate = "HostState",
    hostcountry = "HostCountry",
    theme = "Theme",
    eventwebsite = "EventWebsite",
    plenarysessionswebcast = "PlenarySessionsWebcast",
    eventlogo = "EventLogo"
  )
  
  for (k in names(map)) {
    hit <- which(nm_can == k)
    if (length(hit) == 1) names(df)[hit] <- map[[k]]
  }
  
  required <- unname(map)
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    return(tibble(.error = paste("JSM sheet missing columns:", paste(missing, collapse = ", "))))
  }
  
  df %>%
    transmute(
      Year = suppressWarnings(as.integer(Year)),
      HostCity = as.character(HostCity),
      HostState = as.character(HostState),
      HostCountry = as.character(HostCountry),
      Theme = as.character(Theme),
      EventWebsite = as.character(EventWebsite),
      PlenarySessionsWebcast = as.character(PlenarySessionsWebcast),
      EventLogo = as.character(EventLogo)
    ) %>%
    mutate(across(everything(), ~ ifelse(is.na(.x), .x, str_squish(.x))))
}

# -----------------------------
# YouTube embed helpers
# -----------------------------
extract_youtube_id <- function(url) {
  if (is.na(url) || !nzchar(url)) return(NA_character_)
  id <- str_match(url, "youtu\\.be/([^?&]+)")[, 2]
  if (is.na(id)) id <- str_match(url, "v=([^?&]+)")[, 2]
  id
}

extract_time_param <- function(url, param = c("t", "start", "end")) {
  param <- match.arg(param)
  if (is.na(url) || !nzchar(url)) return(NA_integer_)
  m <- str_match(url, paste0("[?&]", param, "=([0-9]+)"))[, 2]
  suppressWarnings(as.integer(m))
}

build_youtube_embed <- function(start_url, end_url = NA_character_) {
  id <- extract_youtube_id(start_url)
  if (is.na(id)) return(NULL)
  
  start <- extract_time_param(start_url, "t")
  if (is.na(start)) start <- extract_time_param(start_url, "start")
  end <- extract_time_param(end_url, "end")
  
  q <- c()
  if (!is.na(start)) q <- c(q, paste0("start=", start))
  if (!is.na(end))   q <- c(q, paste0("end=", end))
  qs <- if (length(q)) paste0("?", paste(q, collapse = "&")) else ""
  
  paste0("https://www.youtube.com/embed/", id, qs)
}

# -----------------------------
# OpenAlex helpers
# -----------------------------
oa_find_author_id <- function(name) {
  req <- request("https://api.openalex.org/authors") %>%
    req_url_query(search = name, per_page = 1)
  resp <- req_perform(req)
  dat <- resp_body_json(resp)
  if (length(dat$results) == 0) return(NA_character_)
  dat$results[[1]]$id
}

oa_get_works <- function(author_id, years_back = 10) {
  if (is.na(author_id) || !nzchar(author_id)) return(tibble())
  year_min <- as.integer(format(Sys.Date(), "%Y")) - years_back
  
  req <- request("https://api.openalex.org/works") %>%
    req_url_query(
      filter = paste0("authorships.author.id:", author_id,
                      ",from_publication_date:", year_min, "-01-01"),
      per_page = 200
    )
  resp <- req_perform(req)
  dat <- resp_body_json(resp)
  if (length(dat$results) == 0) return(tibble())
  
  map_dfr(dat$results, function(w) {
    authors <- w$authorships %||% list()
    author_names <- map_chr(authors, ~ .x$author$display_name %||% NA_character_)
    
    tibble(
      title = w$display_name %||% NA_character_,
      year = suppressWarnings(as.integer(substr(w$publication_date %||% NA_character_, 1, 4))),
      citations = w$cited_by_count %||% 0L,
      doi = w$doi %||% NA_character_,
      openalex_work_url = w$id %||% NA_character_,
      authors = paste(na.omit(author_names), collapse = ", ")
    )
  })
}

oa_top_collaborators <- function(works_df, focal_name, top_n = 10) {
  if (nrow(works_df) == 0) return(tibble())
  works_df %>%
    mutate(auth_list = str_split(authors, ",\\s*")) %>%
    unnest(auth_list) %>%
    mutate(auth_list = str_squish(auth_list)) %>%
    filter(
      !is.na(auth_list),
      auth_list != "",
      str_to_lower(auth_list) != str_to_lower(focal_name)
    ) %>%
    count(auth_list, sort = TRUE) %>%
    slice_head(n = top_n) %>%
    rename(collaborator = auth_list, n_joint = n)
}

# -----------------------------
# UI
# -----------------------------
ui <- page_navbar(
  title = "COPSS Awards + JSM Events",
  theme = asa_theme,
  header = tags$head(tags$style(HTML(asa_css))),
  
  nav_panel(
    "Data",
    layout_sidebar(
      sidebar = sidebar(
        radioButtons(
          "auth_mode", "Sheets access mode",
          choices = c(
            "Public / link-access (no login)" = "public",
            "Private (I will login with Google)" = "private"
          ),
          selected = "public"
        ),
        hr(),
        strong("COPSS sheet tab"),
        uiOutput("copss_tab_ui"),
        br(),
        strong("JSM sheet tab"),
        uiOutput("jsm_tab_ui"),
        hr(),
        actionButton("reload", "Reload data", class = "btn-primary"),
        br(), br(),
        verbatimTextOutput("load_status"),
        width = 360
      ),
      layout_columns(
        card(card_header("COPSS preview"), DTOutput("copss_preview")),
        card(card_header("JSM preview"), DTOutput("jsm_preview")),
        col_widths = c(6, 6)
      )
    )
  ),
  
  nav_panel(
    "COPSS Winners",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("award_filter", "Award", choices = c("All")),
        sliderInput("copss_year_rng", "Year range", min = 1900, max = 2100, value = c(2000, 2026), sep = ""),
        textInput("copss_q", "Search (awardee/institution/citation)", ""),
        width = 360
      ),
      layout_columns(
        card(card_header("Winners per year"), plotOutput("copss_year_plot")),
        card(card_header("Top institutions"), plotOutput("copss_inst_plot")),
        col_widths = c(6, 6)
      ),
      card(
        card_header("COPSS winners table (click a row for Spotlight)"),
        DTOutput("copss_tbl")
      )
    )
  ),
  
  nav_panel(
    "JSM Events",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("country_filter", "Host country", choices = c("All")),
        sliderInput("jsm_year_rng", "Year range", min = 1900, max = 2100, value = c(2000, 2026), sep = ""),
        textInput("jsm_q", "Search (city/state/theme)", ""),
        width = 360
      ),
      layout_columns(
        card(card_header("Events per year"), plotOutput("jsm_year_plot")),
        card(card_header("Host cities (top 15)"), plotOutput("jsm_city_plot")),
        col_widths = c(6, 6)
      ),
      card(card_header("JSM events table"), DTOutput("jsm_tbl"))
    )
  ),
  
  nav_panel(
    "Integrated",
    layout_sidebar(
      sidebar = sidebar(
        sliderInput("join_year_rng", "Year range", min = 1900, max = 2100, value = c(2000, 2026), sep = ""),
        width = 360
      ),
      card(
        card_header("COPSS awardees by JSM host city (joined on Year)"),
        DTOutput("joined_tbl")
      )
    )
  ),
  
  nav_panel(
    "Awardee Spotlight",
    layout_sidebar(
      sidebar = sidebar(
        uiOutput("spotlight_header"),
        hr(),
        uiOutput("spotlight_links"),
        hr(),
        sliderInput("oa_years_back", "OpenAlex lookback (years)", min = 3, max = 25, value = 10),
        actionButton("refresh_openalex", "Refresh publications", class = "btn-primary"),
        width = 360
      ),
      layout_columns(
        card(card_header("Award video"), uiOutput("spotlight_video")),
        card(card_header("Award citation"), uiOutput("spotlight_citation")),
        col_widths = c(6, 6)
      ),
      layout_columns(
        card(card_header("Top collaborators (OpenAlex)"), plotOutput("spotlight_collab_plot")),
        card(card_header("Top 3 most-cited + most recent (OpenAlex)"), DTOutput("spotlight_pubs_tbl")),
        col_widths = c(6, 6)
      ),
      card(card_header("Profile draft"), uiOutput("spotlight_profile"))
    )
  )
)

# -----------------------------
# Server
# -----------------------------
server <- function(input, output, session) {
  
  observeEvent(input$auth_mode, {
    if (input$auth_mode == "public") gs4_deauth() else gs4_auth()
  }, ignoreInit = TRUE)
  
  output$copss_tab_ui <- renderUI({
    tabs <- safe_sheet_names(SHEET_COPSS)
    if (length(tabs) == 0) return(helpText("Could not list COPSS tabs (check access)."))
    selectInput("copss_tab", NULL, choices = tabs, selected = tabs[[1]])
  })
  
  output$jsm_tab_ui <- renderUI({
    tabs <- safe_sheet_names(SHEET_JSM)
    if (length(tabs) == 0) return(helpText("Could not list JSM tabs (check access)."))
    selectInput("jsm_tab", NULL, choices = tabs, selected = tabs[[1]])
  })
  
  raw_data <- eventReactive(input$reload, {
    req(input$copss_tab, input$jsm_tab)
    list(
      copss = safe_read_sheet(SHEET_COPSS, input$copss_tab),
      jsm   = safe_read_sheet(SHEET_JSM, input$jsm_tab)
    )
  }, ignoreInit = FALSE)
  
  copss <- reactive(standardize_copss(raw_data()$copss))
  jsm   <- reactive(standardize_jsm(raw_data()$jsm))
  
  output$load_status <- renderPrint({
    a <- copss(); e <- jsm()
    if (".error" %in% names(a)) { cat("COPSS load error:\n"); print(a); return() }
    if (".error" %in% names(e)) { cat("JSM load error:\n"); print(e); return() }
    cat("COPSS rows:", nrow(a), "\n")
    cat("JSM rows:", nrow(e), "\n")
  })
  
  output$copss_preview <- renderDT({
    datatable(copss(), options = list(pageLength = 6, scrollX = TRUE), rownames = FALSE)
  })
  
  output$jsm_preview <- renderDT({
    datatable(jsm(), options = list(pageLength = 6, scrollX = TRUE), rownames = FALSE)
  })
  
  observe({
    a <- copss()
    req(!".error" %in% names(a))
    updateSelectInput(session, "award_filter",
                      choices = c("All", sort(unique(na.omit(a$AwardName)))))
    yrs <- a$Year[is.finite(a$Year)]
    if (length(yrs)) {
      updateSliderInput(session, "copss_year_rng",
                        min = min(yrs), max = max(yrs), value = c(min(yrs), max(yrs)))
    }
  })
  
  observe({
    e <- jsm()
    req(!".error" %in% names(e))
    updateSelectInput(session, "country_filter",
                      choices = c("All", sort(unique(na.omit(e$HostCountry)))))
    yrs <- e$Year[is.finite(e$Year)]
    if (length(yrs)) {
      updateSliderInput(session, "jsm_year_rng",
                        min = min(yrs), max = max(yrs), value = c(min(yrs), max(yrs)))
    }
    
    yrs_all <- c(yrs, copss()$Year)
    yrs_all <- yrs_all[is.finite(yrs_all)]
    if (length(yrs_all)) {
      updateSliderInput(session, "join_year_rng",
                        min = min(yrs_all), max = max(yrs_all), value = c(min(yrs_all), max(yrs_all)))
    }
  })
  
  copss_f <- reactive({
    a <- copss()
    req(!".error" %in% names(a))
    
    out <- a %>%
      filter(is.na(Year) | (Year >= input$copss_year_rng[1] & Year <= input$copss_year_rng[2]))
    
    if (!is.null(input$award_filter) && input$award_filter != "All")
      out <- out %>% filter(AwardName == input$award_filter)
    
    if (nzchar(input$copss_q)) {
      pat <- str_to_lower(str_squish(input$copss_q))
      out <- out %>%
        filter(
          str_detect(str_to_lower(coalesce(Awardee, "")), fixed(pat)) |
            str_detect(str_to_lower(coalesce(AwardeeAffiliation, "")), fixed(pat)) |
            str_detect(str_to_lower(coalesce(Citation, "")), fixed(pat))
        )
    }
    out
  })
  
  jsm_f <- reactive({
    e <- jsm()
    req(!".error" %in% names(e))
    
    out <- e %>%
      filter(is.na(Year) | (Year >= input$jsm_year_rng[1] & Year <= input$jsm_year_rng[2]))
    
    if (!is.null(input$country_filter) && input$country_filter != "All")
      out <- out %>% filter(HostCountry == input$country_filter)
    
    if (nzchar(input$jsm_q)) {
      pat <- str_to_lower(str_squish(input$jsm_q))
      out <- out %>%
        filter(
          str_detect(str_to_lower(coalesce(HostCity, "")), fixed(pat)) |
            str_detect(str_to_lower(coalesce(HostState, "")), fixed(pat)) |
            str_detect(str_to_lower(coalesce(Theme, "")), fixed(pat))
        )
    }
    out
  })
  
  output$copss_tbl <- renderDT({
    datatable(
      copss_f(),
      options = list(pageLength = 20, scrollX = TRUE),
      rownames = FALSE,
      selection = "single"
    )
  })
  
  output$copss_year_plot <- renderPlot({
    df <- copss_f() %>% filter(!is.na(Year))
    validate(need(nrow(df) > 0, "No COPSS rows after filtering."))
    
    df %>%
      count(Year, AwardName) %>%
      ggplot(aes(x = Year, y = n, group = AwardName)) +
      geom_line() +
      labs(x = "Year", y = "Number of awardees") +
      asa_gg()
  })
  
  output$copss_inst_plot <- renderPlot({
    df <- copss_f() %>% filter(!is.na(AwardeeAffiliation) & AwardeeAffiliation != "")
    validate(need(nrow(df) > 0, "No institution data after filtering."))
    
    top <- df %>%
      count(AwardeeAffiliation, sort = TRUE) %>%
      slice_head(n = 15)
    
    ggplot(top, aes(x = reorder(AwardeeAffiliation, n), y = n)) +
      geom_col() +
      coord_flip() +
      labs(x = "Institution (top 15)", y = "Count") +
      asa_gg()
  })
  
  output$jsm_tbl <- renderDT({
    datatable(jsm_f(), options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)
  })
  
  output$jsm_year_plot <- renderPlot({
    df <- jsm_f() %>% filter(!is.na(Year))
    validate(need(nrow(df) > 0, "No JSM rows after filtering."))
    
    df %>%
      count(Year) %>%
      ggplot(aes(x = Year, y = n)) +
      geom_line() +
      labs(x = "Year", y = "Number of events") +
      asa_gg()
  })
  
  output$jsm_city_plot <- renderPlot({
    df <- jsm_f() %>% filter(!is.na(HostCity) & HostCity != "")
    validate(need(nrow(df) > 0, "No city data after filtering."))
    
    top <- df %>%
      count(HostCity, sort = TRUE) %>%
      slice_head(n = 15)
    
    ggplot(top, aes(x = reorder(HostCity, n), y = n)) +
      geom_col() +
      coord_flip() +
      labs(x = "Host city (top 15)", y = "Count") +
      asa_gg()
  })
  
  output$joined_tbl <- renderDT({
    a <- copss(); e <- jsm()
    req(!".error" %in% names(a), !".error" %in% names(e))
    
    joined <- a %>%
      inner_join(e, by = "Year") %>%
      filter(Year >= input$join_year_rng[1], Year <= input$join_year_rng[2]) %>%
      select(
        Year, AwardName, Awardee, AwardeeAffiliation,
        HostCity, HostState, HostCountry, Theme,
        EventWebsite, PlenarySessionsWebcast,
        AwardPresentationStart, AwardPresentationEnd,
        AwardeeGoogleScholar, EventLogo
      )
    
    datatable(joined, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)
  })
  
  # -----------------------------
  # Awardee Spotlight
  # -----------------------------
  selected_row <- reactiveVal(NULL)
  
  observeEvent(input$copss_tbl_rows_selected, {
    idx <- input$copss_tbl_rows_selected
    if (length(idx) == 1) selected_row(copss_f()[idx, , drop = FALSE])
  })
  
  spotlight <- reactive({
    sr <- selected_row()
    validate(need(!is.null(sr), "Click a row in the COPSS winners table to populate the Awardee Spotlight."))
    sr
  })
  
  output$spotlight_header <- renderUI({
    sr <- spotlight()
    tags$div(
      tags$h4(sr$Awardee),
      tags$div(tags$strong("Award: "), sr$AwardName),
      tags$div(tags$strong("Year: "), sr$Year),
      tags$div(tags$strong("Institution: "), sr$AwardeeAffiliation)
    )
  })
  
  output$spotlight_links <- renderUI({
    sr <- spotlight()
    tags$div(
      tags$p(tags$a(href = sr$AwardeeGoogleScholar, target = "_blank", "Open Google Scholar profile")),
      tags$p(tags$a(href = sr$AwardPresentationStart, target = "_blank", "Open award video link"))
    )
  })
  
  output$spotlight_video <- renderUI({
    sr <- spotlight()
    embed <- build_youtube_embed(sr$AwardPresentationStart, sr$AwardPresentationEnd)
    validate(need(!is.null(embed), "No embeddable YouTube link found for this awardee."))
    
    tags$iframe(
      src = embed,
      width = "100%",
      height = "360",
      frameborder = "0",
      allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share",
      allowfullscreen = NA
    )
  })
  
  output$spotlight_citation <- renderUI({
    sr <- spotlight()
    tags$div(style = "white-space: pre-wrap; line-height: 1.4;", sr$Citation %||% "")
  })
  
  oa_data <- eventReactive(input$refresh_openalex, {
    sr <- spotlight()
    author_id <- tryCatch(oa_find_author_id(sr$Awardee), error = function(e) NA_character_)
    works <- tryCatch(oa_get_works(author_id, years_back = input$oa_years_back), error = function(e) tibble())
    collab <- oa_top_collaborators(works, focal_name = sr$Awardee, top_n = 10)
    list(author_id = author_id, works = works, collab = collab)
  }, ignoreInit = FALSE)
  
  output$spotlight_collab_plot <- renderPlot({
    d <- oa_data()$collab
    validate(need(nrow(d) > 0, "No collaborator data returned (OpenAlex author match may be ambiguous)."))
    
    ggplot(d, aes(x = reorder(collaborator, n_joint), y = n_joint)) +
      geom_col() +
      coord_flip() +
      labs(x = "Collaborator", y = "Joint works (OpenAlex)") +
      asa_gg()
  })
  
  output$spotlight_pubs_tbl <- renderDT({
    works <- oa_data()$works
    validate(need(nrow(works) > 0, "No works returned (OpenAlex author match may be ambiguous)."))
    
    top3 <- works %>% arrange(desc(citations)) %>% slice_head(n = 3) %>% mutate(tag = "Top cited")
    recent <- works %>% arrange(desc(year)) %>% slice_head(n = 1) %>% mutate(tag = "Most recent")
    
    out <- bind_rows(top3, recent) %>%
      distinct(title, .keep_all = TRUE) %>%
      mutate(link = ifelse(!is.na(doi), doi, openalex_work_url)) %>%
      select(tag, year, citations, title, link)
    
    datatable(out, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
  
  output$spotlight_profile <- renderUI({
    sr <- spotlight()
    works <- oa_data()$works
    
    most_recent <- works %>% arrange(desc(year)) %>% slice_head(n = 1)
    top_cited <- works %>% arrange(desc(citations)) %>% slice_head(n = 3)
    
    tags$div(
      tags$h5("Profile draft"),
      tags$p(tags$strong(sr$Awardee), " is the ", sr$Year, " recipient of ", sr$AwardName, "."),
      tags$p("Affiliation: ", sr$AwardeeAffiliation),
      tags$p("Google Scholar: ", tags$a(href = sr$AwardeeGoogleScholar, target = "_blank", "link")),
      tags$h6("Selected works (top cited)"),
      tags$ul(lapply(top_cited$title, function(t) tags$li(t))),
      tags$h6("Most recent work"),
      tags$p(most_recent$title %||% "Not available")
    )
  })
}

shinyApp(ui, server)

