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


SHEET_COPSS <- "https://docs.google.com/spreadsheets/d/1-NNKbCd-ibEYPkzwjT4ZGK19qGap4fU8nnGeuvG_SHc/edit?usp=drive_link"
SHEET_JSM   <- "https://docs.google.com/spreadsheets/d/1-HGUmfa8QGTMfrrn-GJQaHq9Eo2Fp_8m48lr8-HjNzQ/edit?usp=drivesdk"

gs4_deauth()

sheet_names(SHEET_COPSS)

copss_awards1 <- read_sheet(SHEET_COPSS, sheet = "Sheet1")
copss_awards2 <- read_sheet(SHEET_COPSS, sheet = "Sheet2")
copss_awards <- bind_rows(copss_awards1, copss_awards2)

copss_awards <- copss_awards %>% 
  separate_rows(Awardee, AwardeeAffiliation, AwardeeGoogleScholar, AwardeeHighestDegreeInstitution,
                sep="; ")


jsm_events <- read_sheet(SHEET_JSM)


asa_gg <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", color = "#2E3136"),
      axis.title = element_text(face = "bold", color = "#2E3136"),
      axis.text = element_text(color = "#2E3136"),
      panel.grid.minor = element_blank()
    )
}


top <- copss_awards %>%
  count(AwardeeAffiliation, sort = TRUE) %>%
  filter(!is.na(AwardeeAffiliation)) %>% 
  slice_head(n = 15)

ggplot(top, aes(x = reorder(AwardeeAffiliation, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Affiliation (top 15)", y = "Count") +
  asa_gg()


# YouTube video
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
  end <- extract_time_param(end_url, "t")
  
  q <- c()
  if (!is.na(start)) q <- c(q, paste0("start=", start))
  if (!is.na(end))   q <- c(q, paste0("end=", end))
  qs <- if (length(q)) paste0("?", paste(q, collapse = "&")) else ""
  
  paste0("https://www.youtube.com/embed/", id, qs)
}

lester <- copss_awards %>% 
  filter(Awardee=="Lester Mackey" & Year==2025)

lester_yt_embed <- build_youtube_embed(lester$AwardPresentationStart,
                                       lester$AwardPresentationEnd)


# Open Alex
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


spotlight <- copss_awards %>% 
  filter(Awardee=="Weijie Su" & Year==2026)

sr <-  spotlight
author_id <- tryCatch(oa_find_author_id(sr$Awardee), error = function(e) NA_character_)
works <- tryCatch(oa_get_works(author_id, years_back = 10), error = function(e) tibble())
collab <- oa_top_collaborators(works, focal_name = sr$Awardee, top_n = 10)
list(author_id = author_id, works = works, collab = collab)


d <- oa_data()$collab
validate(need(nrow(d) > 0, "No collaborator data returned (OpenAlex author match may be ambiguous)."))

ggplot(d, aes(x = reorder(collaborator, n_joint), y = n_joint)) +
  geom_col() +
  coord_flip() +
  labs(x = "Collaborator", y = "Joint works (OpenAlex)") +
  asa_gg()


works <- oa_data()$works
validate(need(nrow(works) > 0, "No works returned (OpenAlex author match may be ambiguous)."))

top3 <- works %>% arrange(desc(citations)) %>% slice_head(n = 3) %>% mutate(tag = "Top cited")
recent <- works %>% arrange(desc(year)) %>% slice_head(n = 1) %>% mutate(tag = "Most recent")

out <- bind_rows(top3, recent) %>%
  distinct(title, .keep_all = TRUE) %>%
  mutate(link = ifelse(!is.na(doi), doi, openalex_work_url)) %>%
  select(tag, year, citations, title, link)


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





# Google Scholar 
if(!requireNamespace('remotes')) install.packages("remotes")
remotes::install_github('YuLab-SMU/scholar')


library(kableExtra)
library(formattable)
library(scholar)
library(tidyverse)
library(ggpubr)



#'%ni%' <- Negate('%in%')


author_id <- scholar::get_scholar_id(last_name = "Hanlon", first_name = "Alexandra")
pubs <- scholar::get_publications(author_id) 
glimpse(pubs)

author_id <- str_split(lester$AwardeeGoogleScholar, "=",simplify = T)[,2]
pubs <- scholar::get_publications(author_id) 
glimpse(pubs)

# Sankey plot
library(networkD3)

top_n <- 15

top_affil <- copss_awards %>%
  count(AwardeeAffiliation, sort = TRUE) %>%
  slice_head(n = top_n) %>%
  pull(AwardeeAffiliation)

top_deg <- copss_awards %>%
  count(AwardeeHighestDegreeInstitution, sort = TRUE) %>%
  slice_head(n = top_n) %>%
  pull(AwardeeHighestDegreeInstitution)

copss_sankey <- copss_awards %>%
  mutate(
    affil_grp = ifelse(AwardeeAffiliation %in% top_affil, AwardeeAffiliation, "Other affiliations"),
    deg_grp   = ifelse(AwardeeHighestDegreeInstitution %in% top_deg, AwardeeHighestDegreeInstitution, "Other degree institutions")
  )

links <- copss_sankey %>%
  filter(!is.na(affil_grp), !is.na(deg_grp)) %>%
  count(affil_grp, deg_grp, name = "value") %>%
  rename(source = deg_grp, target = affil_grp)

nodes <- data.frame(name = unique(c(links$source, links$target)))

links <- links %>%
  mutate(
    source = match(source, nodes$name) - 1,
    target = match(target, nodes$name) - 1
  )

sankeyNetwork(links, nodes, "source", "target", "value", "name",
              fontSize = 12, nodeWidth = 30)


# Adding a map
# library(dplyr)
# library(stringr)
# library(tidyr)
library(readr)
library(leaflet)
library(sf)
library(tigris)   
library(rnaturalearth)  
library(rnaturalearthdata)
library(tidygeocoder)  

geo_cache_path <- "jsm_city_geocodes.csv"

make_place_key <- function(city, state, country) {
  paste(
    str_squish(city),
    str_squish(state),
    str_squish(country),
    sep = ", "
  )
}

get_city_geocodes <- function(jsm_events) {
  
  places <- jsm_events %>%
    filter(!is.na(HostCity), HostCity != "",
           !is.na(HostCountry), HostCountry != "") %>%
    mutate(
      HostState = ifelse(is.na(HostState), "", HostState),
      place = make_place_key(HostCity, HostState, HostCountry)
    ) %>%
    distinct(place, HostCity, HostState, HostCountry)
  
  if (file.exists(geo_cache_path)) {
    geo <- read_csv(geo_cache_path, show_col_types = FALSE)
    missing <- anti_join(places, geo, by = "place")
  } else {
    geo <- tibble(place = character(), lat = double(), lon = double())
    missing <- places
  }
  
  if (nrow(missing) > 0) {
    # Geocode missing places (Nominatim by default; be gentle with rate limits)
    new_geo <- missing %>%
      mutate(query = place) %>%
      tidygeocoder::geocode(address = query, method = "osm",
                            lat = lat, long = lon, timeout = 10)
    
    geo <- bind_rows(geo, new_geo %>% select(place, lat, lon)) %>%
      distinct(place, .keep_all = TRUE)
    
    write_csv(geo, geo_cache_path)
  }
  
  left_join(places, geo, by = "place") %>%
    filter(!is.na(lat), !is.na(lon))
}


prep_city_tooltips <- function(jsm_events, city_geo) {
  
  years_by_city <- jsm_events %>%
    filter(!is.na(HostCity), HostCity != "",
           !is.na(HostCountry), HostCountry != "") %>%
    mutate(
      HostState = ifelse(is.na(HostState), "", HostState),
      place = make_place_key(HostCity, HostState, HostCountry)
    ) %>%
    group_by(place, HostCity, HostState, HostCountry) %>%
    summarize(
      years = paste(sort(unique(Year)), collapse = ", "),
      n_events = n(),
      .groups = "drop"
    )
  
  left_join(years_by_city, city_geo, by = c("place", "HostCity", "HostState", "HostCountry"))
}

####
df <- jsm_events %>%
  filter(HostCountry %in% c("USA", "Canada"),
         !is.na(HostCity), HostCity != "") %>%
  mutate(
    HostState = ifelse(is.na(HostState), "", HostState),
    place = paste(str_squish(HostCity), str_squish(HostState), HostCountry, sep = ", ")
  ) 

city_summary <- df %>%
  group_by(place, HostCity, HostState, HostCountry) %>%
  summarise(
    n_events = n(),                                  # frequency
    years = paste(sort(unique(Year)), collapse = ", "),
    .groups = "drop"
  )

city_geo <- get_city_geocodes(df)

# city_pts <- prep_city_tooltips(df, city_geo) %>%
#   filter(!is.na(lat), !is.na(lon))

city_pts <- city_summary %>%
  left_join(city_geo, by = c("place", "HostCity", "HostState", "HostCountry")) %>%
  filter(!is.na(lat), !is.na(lon))

# scale radius by frequency (bounded)
max_n <- max(city_pts$n_events, na.rm = TRUE)

city_pts <- city_pts %>%
  mutate(
    radius = 4 + 10 * sqrt(n_events / max_n)  # 4 to ~14
  )

us_states <- tigris::states(cb = TRUE, year = 2022) %>%
  st_transform(4326)

canada <- rnaturalearth::ne_countries(scale = "medium",
                                      country = "Canada",
                                      returnclass = "sf") %>%
  st_transform(4326)

# leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   addPolygons(data = us_states, weight = 1, fillOpacity = 0.05, color = "#555555") %>%
#   addPolygons(data = canada, weight = 1, fillOpacity = 0.05, color = "#555555") %>%
#   fitBounds(lng1 = -140, lat1 = 25, lng2 = -50, lat2 = 60) %>%
#   addCircleMarkers(
#     data = city_pts,
#     lng = ~lon, lat = ~lat,
#     radius = ~pmin(10, 3 + log1p(n_events)),  # slightly larger for repeat hosts
#     stroke = TRUE, weight = 1,
#     fillOpacity = 0.8,
#     popup = ~paste0(
#       "<b>", HostCity,
#       ifelse(HostState != "", paste0(", ", HostState), ""),
#       "</b><br/>",
#       HostCountry, "<br/>",
#       "<b>Years:</b> ", years
#     ),
#     label = ~paste0(
#       HostCity,
#       ifelse(HostState != "", paste0(", ", HostState), ""),
#       " (", HostCountry, ")"
#     ),
#     labelOptions = labelOptions(direction = "auto", opacity = 0.9)
#   )

leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  fitBounds(lng1 = -140, lat1 = 25, lng2 = -50, lat2 = 60) %>%
  addCircleMarkers(
    data = city_pts,
    lng = ~lon, lat = ~lat,
    radius = ~radius,              # <-- size by city frequency
    stroke = TRUE, weight = 1,
    fillOpacity = 0.85,
    label = ~paste0(
      "<b>", HostCity,
      ifelse(HostState != "", paste0(", ", HostState), ""),
      "</b><br/>",
      "<b>Years:</b> ", years
    ),
    labelOptions = labelOptions(
      direction = "auto",
      opacity = 0.95,
      textsize = "12px"
    ),
    popup = ~paste0(               # optional: click popup
      "<b>", HostCity,
      ifelse(HostState != "", paste0(", ", HostState), ""),
      "</b><br/>",
      HostCountry, "<br/>",
      "<b>Hosted:</b> ", n_events, " time(s)<br/>",
      "<b>Years:</b> ", years
    )
  )


####
output$jsm_map <- renderLeaflet({
  df <- jsm_f() %>%
    filter(HostCountry %in% c("USA", "Canada"),
           !is.na(HostCity), HostCity != "")
  
  validate(need(nrow(df) > 0, "No JSM events available after filtering."))
  
  # Geocodes (cached)
  city_geo <- get_city_geocodes(df)
  
  # Tooltip table
  city_pts <- prep_city_tooltips(df, city_geo) %>%
    filter(!is.na(lat), !is.na(lon))
  
  validate(need(nrow(city_pts) > 0, "Could not geocode any host cities."))
  
  # Base polygons: US states + Canada
  us_states <- tigris::states(cb = TRUE, year = 2022) %>%
    st_transform(4326)
  
  canada <- rnaturalearth::ne_countries(scale = "medium",
                                        country = "Canada",
                                        returnclass = "sf") %>%
    st_transform(4326)
  
  leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(data = us_states, weight = 1, fillOpacity = 0.05, color = "#555555") %>%
    addPolygons(data = canada, weight = 1, fillOpacity = 0.05, color = "#555555") %>%
    fitBounds(lng1 = -140, lat1 = 25, lng2 = -50, lat2 = 60) %>%
    addCircleMarkers(
      data = city_pts,
      lng = ~lon, lat = ~lat,
      radius = ~pmin(10, 3 + log1p(n_events)),  # slightly larger for repeat hosts
      stroke = TRUE, weight = 1,
      fillOpacity = 0.8,
      popup = ~paste0(
        "<b>", HostCity,
        ifelse(HostState != "", paste0(", ", HostState), ""),
        "</b><br/>",
        HostCountry, "<br/>",
        "<b>Years:</b> ", years
      ),
      label = ~paste0(
        HostCity,
        ifelse(HostState != "", paste0(", ", HostState), ""),
        " (", HostCountry, ")"
      ),
      labelOptions = labelOptions(direction = "auto", opacity = 0.9)
    )
})








