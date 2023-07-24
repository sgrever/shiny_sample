### Test app 
### Sara Grever

# does not work :)
# source("facilities-cleaning.R")
library(shiny)
library(shinydashboard)
library(readxl)
library(forcats)
library(dplyr)
library(stringr)
library(glue)
library(janitor)
library(ggplot2)
library(sf)
library(leaflet)
library(tibble)
library(DT)
library(plotly)
library(reactable)
library(bslib)


# data ----


dat <- read_excel("health_facility_locations.xlsx") %>% 
  clean_names()
county_sf <- st_read(dsn = "data", layer = "county_borders")


# elements ----


blue <- "#4f75a3"
orange <- '#e89505'
red <- "tomato"
light_gray <- "#efefef"
med_gray <- "#d0cfd4"
dark_gray <- '#333333'
msg <- paste("<b>County:</b>", county_sf$name)


# tab 1 top row ----


p1_row1 <- fluidRow(
  # map
  column(
    6,
    h5("Facilities by County"),
    # br(), # gap is a bit too big
    leafletOutput("fac_map",
                  width = "100%",
                  height = "550")
  ),
  # county selector + table
  column(
    width = 6,
    align = "left",
    # width = 5,
    # align = "center",
    selectInput("county_select",
                "Choose county:",
                choices = sort(unique(dat$county_name)),
                selected = "FRESNO"),
    # p(glue("Sample of 10 facilities in county:")),
    DT::DTOutput("county_tab")
  )
)


# tab 1 bottom row ----


p1_row2 <- fluidRow(
  column(
    width = 4,
    align = "center",
    h3("Value boxes (fix later)"),
    h4("Value box 1"),
    valueBox(nrow(dat), "State facilities"),
    hr(),
    h4("Value box 2"),
    p("# in county"),
    hr(),
    h4("Value box 3"),
    valueBoxOutput("filler")
  ),
  column(
    width = 8,
    # causes error
    # h5(glue("Facility Types in str_to_title({input$county_select}) County")),
    align = "left",
    # offset = 2,
    plotlyOutput("county_facilities",
                 width = "100%",
                 height = "300px")
  )
)


# ui/server -----


ui <- fluidPage(

    theme = bslib::bs_theme(bootswatch = "united"),

    titlePanel("Licensed Health Facilities in California"),

    tabsetPanel(
      tabPanel(
        "Tab1",
        br(),
        p1_row1,
        hr(),
        p1_row2
        ),
      tabPanel("Tab2"),
      tabPanel("Tab3")
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
                         # radius = ~sqrt(sqrt(capacity + 1)),
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
        addPolygons(fillColor = red,
                    fillOpacity = 0.25,
                    color = "red",
                    weight = 1,
                    layerId = "added_shape")
    })
   
    
    
    output$county_tab <- renderDataTable({
      
      # county_tab_display <-
      #   head(chosen_county(), 10) %>%
      #   mutate(City = str_to_title(city),
      #          Facility = str_to_title(facname),
      #          # capacity is already numeric but DT won't let me rename columns? :)
      #          Capacity = as.numeric(capacity)) %>%
      #   select(City, Facility, Capacity)
      
      county_tab_display <-
        chosen_county() %>%
        arrange(desc(capacity)) %>% 
        head(10) %>% 
        mutate(City = str_to_title(city),
               Facility = str_to_title(facname),
               # capacity is already numeric but DT won't let me rename columns? :)
               Capacity = as.numeric(capacity)) %>%
        select(City, Facility, Capacity)
      
      
      DT::datatable(
        county_tab_display,
        rownames = F,
        options = list(searching = F, paging = F)
      ) 

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
                       tooltop = ("x"))
    })
    
    output$filler <- renderValueBox({
      # not working :)
      valueBox(
        paste0(input$county_select),
        subtitle = "county name",
        icon = icon("list"),
        # error when trying to use "tomato"; no color is showing up anyways tho
        color = "purple"
      )
    })
    
}




shinyApp(ui = ui, server = server)











