### Sara Grever

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(readxl)
library(forcats)
library(lubridate)
library(dplyr)
library(stringr)
library(glue)
library(janitor)
library(ggplot2)
library(sf)
library(leaflet)
library(tibble)
library(plotly)
library(reactable)
library(bslib)


# data ----

county_sf <- st_read(dsn = "data", layer = "county_borders")
dat <- read_excel("health_facility_locations.xlsx") %>% 
  clean_names()


dat$quarter_year <- quarter(dat$license_expiration_date, with_year = T)
dat <- dat %>% 
  filter(county_name != "CURRY")

# elements ----


blue <- "#4f75a3"
orange <- '#e89505'
red <- '#dd4b39'
light_gray <- "#efefef"
med_gray <- "#d0cfd4"
dark_gray <- '#333333'
valuebox_red <- "#dd4b39"
msg <- paste("<b>County:</b>", county_sf$name)
dat_url <- "https://data.chhs.ca.gov/dataset/healthcare-facility-locations/resource/098bbc36-044d-441f-9442-1f4db4d8aaa0"


# tab 1 top row ----


p1_row1 <- fluidRow(
  # map
  column(
    6,
    h5("Facilities by County"),
    align = "center",
    leafletOutput("fac_map",
                  width = "95%",
                  height = "550")
  ),
  # county select + preview table
  column(
    width = 6,
    align = "center",
    selectInput("county_select",
                "Choose county:",
                choices = sort(unique(dat$county_name)),
                selected = "SACRAMENTO"),
    reactableOutput("county_tab", width = "80%")
  )
)


# tab 1 bottom row ----


p1_row2 <- fluidRow(
  column(
    width = 8,
    align = "center",
    plotlyOutput("county_facilities",
                 width = "100%",
                 height = "300px")
  ),
  column(
    width = 4,
    align = "center",
    br(),
    valueBoxOutput("county_count", width = 6),
    valueBoxOutput("expired", width = 6),
    valueBoxOutput("mean_capacity", width = 6)
  )
)


# ui/server -----


ui <- fluidPage(
    
    # try Morph next
    theme = bslib::bs_theme(bootswatch = "united"),
    
    useShinydashboard(),
    
    # apply this to centered plots
    tags$head(tags$style(
      HTML("
            .center-div {
                display: flex;
                justify-content: center;
            }
        ")
    )),

    titlePanel(
      title = span(img(src = "hhs_blue_logo.jpg", height = 35), 
                   "Licensed Health Facilities in California")
      ),
    
    div("Data maintained by the California Department of Public Health (CDPH), 
        Center for Health Care Quality. Last updated 6/15/23.", 
        tags$a(href = dat_url, "Details")),
    br(),
  
    tabsetPanel(
      tabPanel(
        "County Measures",
        br(),
        p1_row1,
        hr(),
        p1_row2
        ),
      tabPanel(
        "State Measures",
        br(),
        div(class = "center-div",
            plotlyOutput("quarters", width = "80%")),
        hr(),
        br(),
        div(class = "center-div", 
            reactableOutput("full_table")),
        div(class = "center-div",
            tags$button(
              "Download as csv",
              onclick = "Reactable.downloadDataCSV('full_table')"
              )),
        br()
        )
      )

)


server <- function(input, output) {
  
    chosen_county <- reactive(
      dat %>% dplyr::filter(county_name == input$county_select)
    )
    county_border <- reactive(
      county_sf %>%
        dplyr::filter(name == str_to_title(input$county_select))
    )

    output$fac_map <- renderLeaflet({
      
      
      leaflet() %>% 
        addProviderTiles("Esri.WorldGrayCanvas") %>% 
        addCircleMarkers(data = dat,
                         lng = ~longitude,
                         lat = ~latitude,
                         radius = 2,
                         fillColor = orange,
                         stroke = F,
                         fillOpacity = 0.3) %>%
        addPolygons(data = county_sf,
                    color = blue,
                    fillColor = "white",
                    fillOpacity = 0.15,
                    weight = 1,
                    popup = msg,
                    popupOptions = popupOptions(closeButton = F)) 
      
    })
    
    observe({
      leafletProxy("fac_map",
                   data = county_border()) %>%
        removeShape(layerId = "added_shape") %>% 
        addPolygons(fillColor = "tomato",
                    fillOpacity = 0.25,
                    color = "red",
                    weight = 1,
                    layerId = "added_shape")
    })
    
    output$county_tab <- renderReactable({
      
      chosen_county() %>% 
        mutate(City = str_to_title(city),
               Facility = str_to_title(facname),
               Capacity = as.numeric(capacity)) %>% 
        select(City, Facility, Capacity) %>% 
        reactable(defaultPageSize = 5,
                  highlight = T,
                  outlined = T,
                  columns = list(
                    City = colDef(headerStyle = list(color = red)),
                    Facility = colDef(headerStyle = list(color = red)),
                    Capacity = colDef(headerStyle = list(color = red))
                  ))
      
    })
    
    output$county_facilities <- renderPlotly({
      county_plot <- chosen_county() %>% 
        # take top 5 most common types + lump others into "Other"
        mutate(fdr_fac = forcats::fct_infreq(
          str_to_title(fac_fdr)
        ) %>% 
          fct_lump_n(5)) %>% 
        ggplot() +
        # arrange in descending order with "Other" on bottom
        geom_bar(aes(x = reorder(fdr_fac, desc(fdr_fac))),
                 fill = orange,
                 col = orange) +
        coord_flip() +
        labs(title = glue("Facilities Types in {str_to_title(input$county_select)} County")) +
        theme(axis.title.y = element_blank(),
              axis.title.x = element_blank(),
              panel.background = element_blank(),
              panel.grid = element_line(color = light_gray),
              panel.grid.major.y = element_blank())
      
      plotly::ggplotly(county_plot,
                       width = 700,
                       height = 500,
                       tooltip = c("count")) 
    })
    
    
    output$county_count <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        nrow(chosen_county()),
        subtitle = "county facilities",
        color = "red"
      )
    })
    
    output$expired <- shinydashboard::renderValueBox({
      n_expired <- chosen_county() %>% 
        filter(license_expiration_date < date("2023-06-15")) %>% 
        nrow()
      
      shinydashboard::valueBox(
        paste0(n_expired),
        subtitle = "expired facilities",
        color = "red"
      )
    })
    
    output$mean_capacity <- shinydashboard::renderValueBox({
      # not sure if $ syntax works with reactive objects
      messy_mean <- chosen_county() %>% pull(capacity) %>% mean()
      
      shinydashboard::valueBox(
        round(messy_mean, 1),
        subtitle = "average capacity",
        color = "red"
      )
    })
    
    output$quarters <- renderPlotly({
      quarter_plot <- dat %>% 
        filter(quarter_year >= 2022.1) %>% 
        mutate(quarter_string = paste0(
          str_extract(quarter_year, "^\\d{4}"), 
          " Q", 
          str_extract(quarter_year, "(?<=\\.)\\d")
        )) %>% 
        ggplot() +
        geom_bar(aes(x = quarter_string,
                     # for tooltip
                     text = quarter_string),
                 fill = orange,
                 col = orange) +
        theme_minimal() +
        labs(y = "Number of Facilities",
             title = "License Expiration Dates") +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
              axis.title.x = element_blank(),
              panel.grid = element_line(color = light_gray),
              panel.grid.major.x = element_blank(),
              plot.title = element_text(hjust = 0.5)) 
      
      plotly::ggplotly(quarter_plot, tooltip = c("count"))

    })
    
    output$full_table <- renderReactable(
      dat %>% 
        select(facname,
               city, 
               county_name, 
               fac_fdr,
               entity_type_description,
               npi,
               ccn) %>% 
      reactable(filterable = T,
                highlight = T,
                outlined = T,
                bordered = T,
                columns = list(
                  facname = colDef(name = "Name",
                                   minWidth = 125,
                                   headerStyle = list(color = red)),
                  city = colDef(name = "City",
                                minWidth = 100,
                                headerStyle = list(color = red)),
                  county_name = colDef(name = "County",
                                       minWidth = 100,
                                       headerStyle = list(color = red)),
                  fac_fdr = colDef(name = "Facility Type",
                                   minWidth = 100,
                                   headerStyle = list(color = red)),
                  entity_type_description = colDef(
                    name = "Entity Type",
                    minWidth = 100,
                    headerStyle = list(color = red)
                    ),
                  npi = colDef(name = "NPI",
                               minWidth = 100,
                               headerStyle = list(color = red)),
                  ccn = colDef(name = "CCN",
                               minWidth = 100,
                               headerStyle = list(color = red))
                ))
    )
    
}




shinyApp(ui = ui, server = server)











