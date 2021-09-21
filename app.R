library(shiny)
library(dplyr)
library(plotly)
library(rlang)
library(markdown)

Sys.setenv(MAPBOX_TOKEN = "pk.eyJ1IjoiY3BzaWV2ZXJ0IiwiYSI6ImNpdGRiOWwwMjAwMHQyem84NG5tcXg5eXIifQ.UJ2VwrJmPW8DB8H2d-wN3g")

zones <- feather::read_feather("data/zones.feather")
zones$urban = factor(zones$urban, c("Rural area", "Provincial town", "Provincial city", "Capital suburb", "Capital"))

measures <- c(
  "Low income" = "pi_lowincome",
  "Short education" = "pi_basiceducation",
  "Not employed" = "pi_notemployed",
  "Manual work" = "pi_manuallabour",
  "Overcrowded household" = "ph_overcrowded",
  "No car owned" = "ph_nocarowned",
  "Lone adult household" = "ph_oneadult",
  "Rents home" = "ph_rentedaccommodation",
  "Residential transience" = "pi_residentialtransience",
  "Any criminality" = "pi_anycrime",
  "Born abroad" = "pi_foreignborn",
  "Physical illness" = "pi_anycharlson",
  "Population density" = "population_density",
  "Age distribution (0-14 years)" = "pi_age_grp1",
  "Age distribution (15-29 years)" = "pi_age_grp2",
  "Age distribution (30-49 years)" = "pi_age_grp3",
  "Age distribution (50-64 years)" = "pi_age_grp4",
  "Age distribution (65+ years)" = "pi_age_grp5",
  "Urbanicity" = "urban"
)

color <- paste0("#", c("00AC37", "1FFA5E", "FEFFA2", "FFAE61", "F44E3E"))
colscale <- tibble::tibble(
  probs = seq(0, 1, length.out = 5),
  color = scales::col_numeric(color, 1:5)(1:5)
)
format_numbers <- function(x, n = 1) {
  format(round(x, n), nsmall = n, big.mark = ",")
}


ABOUT <- div(
  id = "about",
  class = "bg-light text-center",
  style = "height: 550px; padding-top: 100px;",
  div(
    class = "container",
    h1("Learn more"),
    p(
      class = "lead my-5",
      style = "width: 100%; max-width: 800px; margin-left:auto; margin-right:auto",
      "This website aims to provide more information on our scientific publication in ",
      "TO BE DETERMINED.",
      "See the links below for information about this study, ",
      "our contact information, and more about our work at ",
      a(href = "https://econ.au.dk", "econ.au.dk", .noWS = "after"),
      ". To cite the paper use TO BE DETERMINED."
    ),
    div(
      class="d-flex justify-content-center",
      # TODO: paper link
      a(class = "btn btn-primary btn-xl text-uppercase", href = "#", "Paper"),
      a(class = "btn btn-primary btn-xl text-uppercase", "data-bs-toggle"="modal", href="#dataDownload", "Data"),
      a(class="btn btn-primary btn-xl text-uppercase", href="mailto:cbp@econ.au.dk", "Contact"),
      a(class="btn btn-primary btn-xl text-uppercase", href="https://econ.au.dk", "econ.au.dk")
    )
  )
)

shiny::addResourcePath("data", "data-raw")

ABOUT_MODAL <- modal(
  id = "dataDownload", title = "Data download",
  body = "Download data using the buttons below: ",
  buttons = list(
    "data/nli_zone0221_transpose.csv" = "Zonal indicators",
    "data/zone2016_kom_v01_clean.shp" = "Zonal regions shape file"
  )
)


ui <- fluidPage(
  theme = bslib::bs_theme(version = 5) %>%
    bslib::bs_add_rules(".accordion h2 {margin-top: 0}") %>%
    bslib::bs_add_rules(lapply(dir("scss", full.names = TRUE), sass::sass_file)),
  div(
    class = "pt-4 d-flex justify-content-center",
    style = "margin-bottom: -1rem",
    tags$span("Overview of ", class = "lead px-4"),
    selectInput("measure", NULL, measures, selectize = FALSE, width = "auto"),
    tags$span("between ", class = "lead px-4"),
    uiOutput("year")
  ),
  div(class = "d-flex justify-content-center", plotlyOutput("map", height = "650px", width = "80%")),
  accordion(
    accordion_item("Background on zonal regions", includeMarkdown("background.md")),
    accordion_item("Measurement details", uiOutput("description"), show = TRUE),
    accordion_item("Aggregation details", includeMarkdown("aggregation.md")),
  ),
  ABOUT, ABOUT_MODAL
)

server <- function(input, output, session) {

  output$year <- renderUI({
    req(input$measure)
    if (input$measure %in% names(zones)) return(NULL)
    vars <- grep(paste0("^", input$measure), names(zones), value = TRUE)
    choices <- vapply(strsplit(vars, "_"), function(x) {
      n <- length(x)
      paste(x[c(n - 1, n)], collapse = "_")
    }, character(1))
    shinyWidgets::sliderTextInput(
      "year", NULL, choices, hide_min_max = TRUE,
      selected = choices[length(choices)]
    )
  })

  output$description <- renderUI({
    req(input$measure)
    f <- file.path("descriptions", paste0(input$measure, ".md"))
    if (file.exists(f)) includeMarkdown(f)
  })

  var_name <- reactive({
    if (input$measure %in% names(zones)) {
      input$measure
    } else {
      paste0(input$measure, "_", input$year)
    }
  })

  vals <- reactive({
    zones[[var_name()]]
  })

  lvls <- reactive({
    v <- vals()
    if (is.numeric(v)) {
      quantile(v, seq(0, 1, length.out = 6), na.rm = TRUE)
    } else {
      levels(v) %||% unique(v)
    }
  })

  z <- reactive({
    v <- vals()
    if (is.numeric(v)) {
      cut(v, lvls(), labels = FALSE, include.lowest = TRUE)
    } else {
      match(v, lvls())
    }
  })

  ids <- reactive(zones$ID2)

  output$map <- renderPlotly({
    req(input$measure, input$year)

    title <- names(measures)[match(input$measure, measures)]

    lvls <- lvls()
    lvl_names <- lvls
    if (is.numeric(lvls())) {
      lvls <- lvls[-1]
      lvl_names <- paste0("≤ ", format_numbers(lvls))
    }

    plot_mapbox() %>%
      add_trace(
        type = "choroplethmapbox",
        geojson = "https://raw.githubusercontent.com/LDAvis2/denmark_zones/main/data/zones.json",
        locations = ids(),
        z = z(),
        colorscale = colscale,
        stroke = I("lightgray"),
        text = paste0(
          "ID: ", ids(), "<br>",
          title, ": ", if (is.numeric(vals())) format_numbers(vals()) else vals()
        ),
        hoverinfo = "text",
        span = I(0.5)
      ) %>%
      add_trace(
        type = "scattermapbox",
        split = seq_along(lvls),
        x = 0, y = 0,
        name = lvl_names,
        color = I(colscale[[2]]),
        marker = list(symbol = "square", size = 70)
      ) %>%
      hide_colorbar() %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c('sendDataToCloud','select2d','lasso2d','hoverClosestCartesian','hoverCompareCartesian')) %>%
      layout(
        transition = list(duration = 1000, easing = "linear"),
        legend = list(
          title = list(text = title),
          orientation = "h",
          y = -0.01, x = 0.5,
          yanchor = "top",
          xanchor = "center"
        ),
        mapbox = list(
          style = "light", zoom = 6,
          center = list(
            # Taken from sf::st_bbox() on shapefile
            lon = mean(c(8.07251, 15.15738)),
            lat = mean(c(54.55908, 57.75257))
          )
        )
      )
  })
}

shinyApp(ui, server)
