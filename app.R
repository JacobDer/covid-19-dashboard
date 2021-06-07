library(shiny)
library(shinyWidgets)
library(shinythemes)
library(sqldf)
library(leaflet)
library(dplyr)
library(tidyverse)

# USA polygon data
counties <- readRDS("data/gadm36_USA_2_sf.rds")

# Grab data from NY Times GitHub
path <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
sql <- "SELECT
          *
        FROM
          file
        WHERE
          date > DATE('2020-03-31')"
covid_df <- read.csv.sql(path, sql = sql)

# Merge data
map_data <- left_join(
              counties,
              covid_df,
              by = c("NAME_1" = "state", "NAME_2" = "county")
            )

# Most recent date
current_date <- as.Date(max(covid_df$date), "%Y-%m-%d")

# Create a base map and legend
base_map <- leaflet() %>%
              addProviderTiles("OpenStreetMap.Mapnik") %>%
              setView(lat = 38.8283, lng = -105.5795, zoom = 4)

# UI
ui <- bootstrapPage(

  navbarPage(
    theme = shinytheme("flatly"),
    HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">COVID-19 in the US</a>'),
    windowTitle = "Covid-19",

    tabPanel(
      title = "Covid-19 Map",
      div(
        class = "outer",
        tags$head(includeCSS("styles.css")),

        leafletOutput("map", width = "100%", height = "100%"),

        absolutePanel(
          id = "date_slider",
          class = "panel panel-default",
          top = 75, left = 55, width = 250, fixed = TRUE, draggable = TRUE,
          h5("Case and death counts are cumulative.", align = "center"),
          h3(textOutput("case_count"), align = "center"),
          h3(textOutput("death_count"), align = "center"),
          sliderTextInput(
            "date",
            label = "Date Selector",
            selected = current_date,
            choices = unique(covid_df$date),
            grid = FALSE
          )
        )
      )
    ),

    tabPanel(
      title = "About",
      div(
        h3("About this Dashboard"),
        "The Covid-19 data for this dashboard comes from the NY Times, and can
        be found",
        tags$a(href = "https://github.com/nytimes/covid-19-data/blob/master/us-counties.csv", "here."),
        "It should be kept in mind that counts of cases and deaths are
        cumulative, meaning enormous numbers do not necessarily correspond
        to active cases.",

        tags$br(),
        tags$br(),

        "The county-level map data was provided by GADM, and can be found ",
        tags$a(href = "https://gadm.org/download_country_v3.html", "here."),

        tags$br(),
        tags$br(),

        "This dashboard was made with R—the code used to produce it can be found",
        tags$a(href = "https://github.com/JacobDer/covid-19-dashboard", "here."),

        h3("Authors"),
        "Jacob Der",
      )
    )
  )

)

# Server
server <- function(input, output) {

  reactive_date <- reactive({
    input$date
  })

  reactive_data <- reactive({
    map_data %>% filter(date == reactive_date())
  })

  reactive_cases <- reactive({
    paste(sum(reactive_data()$cases), "cases")
  })
  output$case_count <- renderText(reactive_cases())

  reactive_deaths <- reactive({
    paste(sum(reactive_data()$deaths), "deaths")
  })
  output$death_count <- renderText(reactive_deaths())

  reactive_pal <- reactive({
    colorQuantile("Oranges", domain = reactive_data()$cases, n = 7)
  })
  reactive_text <- reactive({
    paste(
      "State: ", reactive_data()$NAME_1, "<br>",
      "County: ", reactive_data()$NAME_2, "<br>",
      "Cases: ", reactive_data()$cases, "<br>",
      "Deaths: ", reactive_data()$deaths, "<br>",
      "Date: ", reactive_data()$date, "<br>",
      sep = ""
    ) %>%
    lapply(htmltools::HTML)
  })

  output$map <- renderLeaflet(base_map)

  observeEvent(input$date, {
    leafletProxy("map") %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        data = reactive_data(),
        stroke = TRUE,
        color = "white",
        weight = 0.3,
        smoothFactor = 0,
        fillOpacity = 0.50,
        fillColor = ~reactive_pal()(cases),
        label = reactive_text()
      ) %>%
      addLegend(
        title = "Case Range Quantiles",
        position = "bottomright",
        pal = reactive_pal(),
        values = reactive_data()$cases,
        labFormat = function(type, cuts, p) {
                      n <- length(cuts)
                      paste0(floor(cuts[-n]), " &ndash; ", floor(cuts[-1]))
                    }
      )
    }
  )

}

shinyApp(ui, server)
