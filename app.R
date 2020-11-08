##### general setup and data #####



# libraries ----

# shiny
library(shiny)
library(shinyWidgets)
library(shinythemes)

# data and ggplot2
library(tidyverse)
library(haven)
library(labelled)

# geospatial data
library(sf)
library(sfheaders)
library(geojsonsf)

# map visualization
library(leaflet)
library(viridis)

# use tidyverse inside shiny
library(rlang)

# html
library(htmltools)

# ggplot2 theme setting
theme_set(theme_bw())


# data ----

### bti data
bti <- read_dta("data/BTI_2006_2020.dta")

# decode labels
bti <- bti %>%
  mutate_if(is.labelled, to_factor) %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.numeric, ~round(., 2)) %>%
  mutate_at(c("year", "rank_dem_stat", "rank_stat_ind"), as.integer) %>%
  mutate(country = iconv(country, from = "latin1", to = "UTF-8", sub = ""))


  


### geospatial data

# geospatial data of countries
countries <- geojson_sf("data/countries.geojson")

# recode country codes for joining with BTI data
countries <- countries %>%
  mutate(ISO_A3 = recode(ISO_A3, "-99" = "KSV", "BGD" = "BHD",
                         "COD" = "ZAR", "ROU" = "ROM"))

### combine bti with geospatial data
bti_geo <- countries %>%
  right_join(bti, by = c("ISO_A3" = "country_code")) %>%
  select(country, year, region,
         dem_stat, rank_dem_stat,
         stat_ind, rank_stat_ind, 
         gov_ind, rank_gov_ind,
         geometry) %>%
  # encode country names for succesful use with html and leaflet
  mutate(country = iconv(country, from = "latin1", to = "UTF-8", sub = "")) %>%
  # there's no spatial data for Bangladesh available sadly
  filter(country != "Bangladesh")

# original countries data no longer needed
rm(countries)



# further objects needed ----

### vector of indices
index_vec <- c("Democracy Index" = "dem_stat",
               "Status Index" = "stat_ind",
               "Governance Index" = "gov_ind")

### color palette for map plot
mypalette <- colorBin(palette = "magma", domain = c(1, 10),
                      na.color = "transparent", bins = seq(1, 10, 1))

### text labels for map's elements/countries
mytext_dem <- paste0("Country: ", (bti_geo %>% filter(year == max(year)))$country, "<br/>",
                 "Democracy Index: ", (bti_geo %>% filter(year == max(year)))$dem_stat, "<br/>",
                 "Rank: ", (bti_geo %>% filter(year == max(year)))$rank_dem_stat) %>%
  lapply(htmltools::HTML)

### leaflet map object
bti_map <- bti_geo %>%
    filter(year == max(year)) %>%
    leaflet() %>%
    addProviderTiles(providers$Stamen.TonerLite) %>%
    setView(lat=30, lng=10 , zoom=1.5) %>%
    addPolygons(fillColor = ~mypalette(dem_stat), stroke = FALSE,
                fillOpacity = 0.6, smoothFactor = 0.4, label = mytext_dem) %>%
    addLegend(position = "topleft", pal = mypalette, values = ~dem_stat,
              opacity = 0.6, title = "Democracy Index")




##### Shiny App #####


# User Interface ----

ui <- fluidPage(theme = shinytheme("flatly"),
                navbarPage(
                  "",
                  
                  ### landing page
                  tabPanel(
                    "Start",
                    # sidebar panel with some credits to the authors of the study
                    sidebarPanel(
                      p("This Shiny application is a private project and is not
                        related to the authors from the Bertelsmann Foundation."),
                      br(),
                      p("The data from the Bertelsmann Foundation can be downloaded ",
                        a("here", href = "https://bti-project.org/de/meta/downloads.html"),
                        ".")
                    ),
                    # main panel with some text as landing page
                    mainPanel(
                      h1("BTI Data Explorer"),
                      p('This Shiny App shall serve as a tool to explore the data from the
                        "Bertelsmann Transformation Index" (BTI).
                        The BTI is a project of the Bertelsmann Foundation and seeks to
                        evaluate the current state of democracy and the economy in
                        developing countries.'),
                      p("For a more comprehensive investigation of this project you can
                        visit its",
                        a("website", href = "https://bti-project.org/de/home.html?&cb=00000"))
                    )
                  ),
                  
                  ### tab 1: raw data
                  tabPanel(
                    "Data",
                    # select and filter the data
                    sidebarPanel(
                      helpText("Specify which data you are interested in"),
                      # select countries
                      pickerInput("select_country",
                                  h5("Select your countries of interest"),
                                  choices = unique(bti$country),
                                  options = list("actions-box" = T,
                                                 "live-search" = T),
                                  multiple = T),
                      # select years
                      sliderInput("select_year",
                                  h5("Select the years of interest"),
                                  min = min(bti$year), max = max(bti$year),
                                  value = c(2020, 2020),
                                  sep = "",
                                  step = 2),
                      # select variables of interest
                      pickerInput("select_vars",
                                  h5("Select your variables of interest"),
                                  choices = setdiff(names(bti), c("country","country_code",
                                                                  "year", "region")),
                                  options = list("actions-bo" = T,
                                                 "live-search" = T),
                                  multiple = T,
                                  selected = c("dem_stat", "rank_dem_stat",
                                               "stat_ind", "rank_stat_ind"))
                    ),
                    # data output
                    mainPanel(
                      h2("Raw BTI Data"),
                      br(),
                      p("Explore the raw BTI data."),
                      br(),
                      dataTableOutput("bti_table")
                    )
                  ),
                  
                  ### tab 2: country profiles
                  tabPanel(
                    "Country Profile",
                    # specify the observation of interest (country and year)
                    sidebarPanel(
                      helpText("Choose an observation"),
                      # select country
                      pickerInput("plot_country",
                                  h5("Choose a country"),
                                  choices = unique(bti$country),
                                  selected = "Afghanistan"),
                      # select year
                      pickerInput("plot_year",
                                  h5("Choose a year"),
                                  choices = unique(bti$year),
                                  selected = 2020)
                    ),
                    
                    # plot output
                    mainPanel(
                      h2("BTI Country Profile"),
                      br(),
                      plotOutput("country_profile")
                    )
                  ),
                  
                  ### tab 3: time series
                  tabPanel(
                    "Country Time Series",
                    sidebarPanel(
                      helpText("Which Time Serieses are you interested in?"),
                      # select countries of interest
                      pickerInput("ts_country",
                                  h5("Select your countries of interest"),
                                  choices = unique(bti$country),
                                  options = list("actions-box" = T,
                                                 "live-search" = T),
                                  multiple = T,
                                  selected = "Afghanistan"),
                      # select index
                      pickerInput("ts_index",
                                  h5("Which index would you like to inspect?"),
                                  choices = index_vec,
                                  selected = "dem_stat")
                    ),
                    
                    # plot output
                    mainPanel(
                      h2("BTI Time Series"),
                      br(),
                      plotOutput("ts_plot")
                    )
                    
                  ),
                  
                  
                  ### tab 4: map
                  tabPanel(
                    "Democracy Index Map",
                    fillPage(
                      tags$style(type = "text/css", "html, body {width:100%; height:100%}"),
                      leafletOutput("bti_map", width = "100%", height = 550),
                      absolutePanel(bottom = 10, left = 50, width = 400,
                                    h3("Democracy Index 2020"))
                      )
                    
                    )
                  
                )
)




# Server ----

server <- function(input, output, session) {
  
  ### tab1: raw data
  
  df <- reactive(
    # select countries of interest
    bti %>% filter(country %in% input$select_country) %>%
      # select years / range of interest
      filter(between(year, input$select_year[1], input$select_year[2])) %>%
      # select variables of interest
      select(c(c("country", "country_code", "year", "region"),
               input$select_vars))
  )
  
  # data output
  output$bti_table <- renderDataTable(df())
  
  
  
  ### tab 2: country profiles
  
  profile_plot <- reactive(
    ### preparation of the data for plot
    bti %>%
      # columns of interest and need
      select(c("country", "year", "region", # identifiers for observations
               "stateness", "pol_part", "ruleoflaw", "stab_dem", "integ", # democracy index
               "level_development", "market", "stab_econ", "priv_prop",
               "welfare", "perf_econ", "sustain", # status index
               "steering", "efficiency", "consens", "int_coop")) %>% # governance index
      # select country
      filter(country == input$plot_country) %>%
      # select year
      filter(year == input$plot_year) %>%
      # long format for ggplot
      pivot_longer(cols = !c("country", "year", "region"),
                   names_to = "index_name", values_to = "index_val") %>%
      # map each index to a category (from the authors of the study)
      mutate(index_cat = case_when(index_name %in% c("stateness", "pol_part", "ruleoflaw", "stab_dem", "integ") ~ "democracy",
                                   index_name %in% c("level_development", "market", "stab_econ", "priv_prop", "welfare", "perf_econ", "sustain") ~ "status",
                                   index_name %in% c("steering", "efficiency", "consens", "int_coop") ~ "governance")) %>%
      # specify order of factor variable levels
      mutate(index_name = factor(index_name, levels = rev(unique(index_name))),
             index_cat = factor(index_cat, levels = rev(unique(index_cat)))) %>%
      ### plot
      ggplot(aes(x=index_name, y=index_val, color=index_cat)) +
      # lines with dots on plot
      geom_segment(aes(x=index_name, xend=index_name, y=0, yend=index_val), size=1.1) +
      geom_point(size=3) +
      # color and legend
      scale_color_manual(breaks = c("democracy", "status", "governance"),
                         labels = c("Democracy", "Status", "Governance"),
                         values = c("#56B4E9", "#E69F00", "#009E73"),
                         name = "Index Category",
                         guide = guide_legend(override.aes = list(linetype = rep("blank", 3),
                                                                  size = 5))) +
      # full words for the axis ticks
      scale_x_discrete(labels = c("International Cooperation", "Consensus Building", "Resource Efficiency", "Steering Capability",
                                  "Sustainability", "Economic Performance", "Welfare Regime", "Private Property", "Monetary and Fiscal Stabiliry",
                                  "Market Organization", "Socioeconomic Level",
                                  "Political and Social Integration", "Stability of Democratic Institutions", "Rule of Law",
                                  "Political Participation", "Stateness")) +
      # adjust y-axis ticks
      scale_y_continuous(limits = c(0, 10),
                         breaks = seq(0, 10, 2)) +
      # no axis labels needed
      labs(x="", y="") +
      # final adjustments on legend
      theme(legend.position = "bottom",
            legend.background = element_blank(),
            legend.key = element_blank()) +
      # title
      ggtitle(paste0(input$plot_country, " ", "(", input$plot_year, ")")) +
      # flip plot to horizontal
      coord_flip()
  )
  
  # output plot
  output$country_profile <- renderPlot(profile_plot())
  
  
  
  ### tab 3: time series plot

  ts_plot_done <- reactive(
    # build plot
    bti %>%
    # select countries
    filter(country %in% input$ts_country) %>%
    ### plot
    # select variable for time series
    ggplot(aes_string(x="year", y=input$ts_index)) +
    theme_minimal() +
    # time series lines
    geom_line(aes(color = country),
              size = 1.2, alpha = 0.8) +
    # add points to time series
    geom_point(aes(color = country),
               size = 2, alpha = 0.8) +
    # adjustment of scales
    scale_x_continuous(limits = c(2006, 2020),
                       breaks = seq(2006, 2020, 2)) +
    scale_y_continuous(limits = c(0, 10),
                       breaks = seq(0, 10, 2)) +
    # color for countries and legend adjustment
    scale_color_viridis(discrete = TRUE, option = "D",
                        guide = guide_legend(override.aes = list(shape = NA))) +
    theme(legend.justification = "top") +
    # labels and title
    labs(x="Year", y=names(index_vec)[index_vec == input$ts_index], color = "Country") +
    ggtitle(paste0("Development of ", names(index_vec)[index_vec == input$ts_index]))
  )
  
  # output plot
  output$ts_plot <- renderPlot({
    ts_plot_done()
  })
  
  
  
  ### tab 4: map
  
  
  output$bti_map <- renderLeaflet(bti_map)
  
  

  
}



# Run the app ----
shinyApp(ui = ui, server = server)

