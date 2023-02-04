library(shiny)
library(leaflet)
library(reactable)
library(sf)

ui <- fluidPage(
  tags$head(
    tags$style(
      htmltools::HTML(
        "
        .hidden-column-headers .rt-thead {
        position: absolute;
  width: 1px;
  height: 1px;
  padding: 0;
  margin: -1px;
  overflow: hidden;
  clip: rect(0, 0, 0, 0);
  border: 0;
}
        "
      )
    )
  ),
  fluidRow(
    column(4, reactable::reactableOutput("table")),
    column(8, leaflet::leafletOutput("map"))
  )
  
)

server <- function(input, output, session) {
  
  
  ctry_rgn_laua <- readr::read_csv("ctry_rgn_laua.csv") |>
    dplyr::select(ctrynm, rgnnm, lauanm, n, lng, lat) |>
    dplyr::filter(ctrynm %in% c("England", "Scotland", "Wales", "Northern Ireland")) 
  
  ctry <- ctry_rgn_laua  |>
    dplyr::group_by(ctrynm) |>
    dplyr::summarise(n = sum(n), lng = mean(lng), lat = mean(lat)) |>
    dplyr::arrange(-n)
  
  ctry_sf <- ctry|>
    sf::st_as_sf(coords = c("lng", "lat"), crs = st_crs(4326))
  
  ctry_rgn <- ctry_rgn_laua |>
    dplyr::group_by(ctrynm, rgnnm) |>
    dplyr::summarise(n = sum(n), lng = mean(lng), lat = mean(lat))
  
  onclick_js <- JS(
    "function(rowInfo, colInfo){
      if (window.Shiny) {
      Shiny.setInputValue('show_details', { index: rowInfo.index + 1, rnd: Math.random() });
      console.log(rowInfo.index);
    }
    }"
  )
  
  output$table <- renderReactable({
    reactable(
      ctry |> dplyr::select(-lng, -lat),
      details = function(index){
        rgn <- ctry_rgn |> 
          dplyr::ungroup() |>
          dplyr::filter(ctrynm == ctry$ctrynm[index]) |> 
          dplyr::select(-ctrynm, -lat, -lng)
        tbl <- reactable(rgn, fullWidth = TRUE,
                         class = "hidden-column-headers")
        htmltools::div(style = list(`margin-left` = "45px"), tbl)
      },
      onClick = onclick_js,
      rowStyle = list(cursor = "pointer", fontWeight = "bold")
    )
  })
  
  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(-2,52, 6) |>
      addCircleMarkers(data = ctry_sf, radius = ~scales::rescale(n, c(8, 20)), stroke = FALSE)
  })
}

shinyApp(ui, server)

