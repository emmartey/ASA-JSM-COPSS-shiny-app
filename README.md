# ASA-JSM-COPSS Shiny Dashboard

An interactive Shiny dashboard for exploring **COPSS award winners** and **JSM event history** in one place.

## Live dashboard

- [Open the dashboard](https://019c8957-13df-51ec-e288-2636dc78243d.share.connect.posit.cloud/)

## App overview

This app provides the following sections:

- **Overview**: onboarding guidance and quick navigation links.
- **Data**: load diagnostics and previews for COPSS/JSM source sheets.
- **COPSS Winners**: filterable winner table, institutional summaries, and Sankey flow.
- **Awardee Spotlight**: selected winner details, video/citation, and OpenAlex publication summaries.
- **Relive JSM Events**: host city summaries, event table, and map.
- **About**: award descriptions from COPSS Sheet3 plus author/repository links.

## Data sources used by the app

The app reads public Google Sheets:

- **COPSS sheet**
  - `Sheet1` + `Sheet2`: winner-level records
  - `Sheet3`: award descriptions (`AwardName`, `AboutAward`, `LearnMoreLink`)
- **JSM sheet**
  - `Sheet1`: event-level records

## Run locally

1. Install required packages in R:

```r
install.packages(c(
  "shiny","bslib","dplyr","stringr","lubridate","ggplot2","DT",
  "googlesheets4","tidyr","httr2","purrr","readr","leaflet","sf",
  "tigris","rnaturalearth","rnaturalearthdata","tidygeocoder","networkD3"
))
```

2. Launch the app:

```r
shiny::runApp()
```

## Notes

- The app uses link/public access to Google Sheets (`gs4_deauth()`).
- Geocode lookups are cached in `jsm_city_geocodes.csv`.
