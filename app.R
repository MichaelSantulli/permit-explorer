#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(dplyr)
library(sf)
library(mapgl)
library(stringr)
library(ggplot2)
library(plotly)
library(socratadata)

#---LOAD DATA---
get_live_data <- function(){
  soc_read("https://data.austintexas.gov/Building-and-Development/Issued-Building-Permits/quv8-5ckq/about_data",
           query = soc_query(
             where = "STATUS = 'Active'")
  )|>
    filter(certificate_of_occupancy == "Yes",
           sub_type %in% c("R- 101 Single Family Houses",
                           "R- 102 Secondary Apartment",
                           "R- 103 Two Family Bldgs",
                           "C- 101 Single Family Houses",
                           "C- 103 Two Family Bldgs",
                           "C- 104 Three & Four Family Bldgs",
                           "C- 105 Five or More Family Bldgs"
           )
    )
  
}

active_housing_permits <- get_live_data()

active_housing_permits <- active_housing_permits|>
  mutate(number_of_units = as.numeric(number_of_units),
         unit_type = str_sub(sub_type, start = 7L))

# Generate summary table by unit type
summary_init <- active_housing_permits|>
  st_drop_geometry()|>
  group_by(unit_type)|>
  summarise(total_units = sum(number_of_units, na.rm = TRUE))|>
  mutate(category = "Active Permits")|>
  arrange(total_units)

summary <- summary_init


#---VISUALIZATION FUNCTIONS---

#About text

#Function to generate initial map using MapLibre GL JS

hover_content <- glue::glue(
  "Units: {scales::label_comma()(active_housing_permits$number_of_units)}"
)

popup_content <- glue::glue(
  "<strong>Permit Number: </strong>{active_housing_permits$permit_number}<br>",
  "<strong>Permit subtype: </strong>{active_housing_permits$sub_type}<br>",
  "<strong>Year Issued: </strong>{active_housing_permits$calendar_year_issued}<br>",
  "<strong>Number of Units: </strong>{scales::label_comma()(active_housing_permits$number_of_units)}"
)

active_housing_permits$hover <- hover_content
active_housing_permits$popup <- popup_content

draw_map <- function(data_source){
  maplibre(
    style = carto_style("positron"),
    center = c(-97.7431,30.2988), #Austin
    zoom = 10)|>
    add_fullscreen_control()|>
    add_navigation_control()|>
    add_reset_control()|>
    add_scale_control(unit = "imperial")|>
    add_draw_control(
      rectangle = TRUE,
      radius = TRUE,
      show_measurements = TRUE,
      measurement_units = "imperial",
      controls = c(point = FALSE, line_string = FALSE, combine_features = FALSE, uncombine_features = FALSE)
    )|>
    #add_screenshot_control()|>
    add_circle_layer(
      id = "permits", 
      source = data_source,
      tooltip = "hover",
      popup = "popup",
      circle_color = match_expr(
        "unit_type",
        values = c(" Single Family Houses", " Secondary Apartment", " Two Family Bldgs", " Three & Four Family Bldgs", " Five or More Family Bldgs"),
        stops = c("#414487FF","#2A788EFF","#22A884FF","#7AD151FF", "#FDE725FF")
      ),
      circle_opacity = .9,
      #cluster_options = cluster_options(max_zoom = 12)
    )|>
    add_categorical_legend(
      legend_title = "Housing Type",
      values = c(" Single Family Houses", " Secondary Apartment", " Two Family Bldgs", " Three & Four Family Bldgs", " Five or More Family Bldgs"),
      colors = c("#414487FF","#2A788EFF","#22A884FF","#7AD151FF", "#FDE725FF"),
      patch_shape = "circle",
      position = "bottom-right"
    )
}

# Function to generate interactive bar chart with Plotly.js
draw_chart_bar <- function(data_source){
  ggplotly(ggplot(data_source, aes(x = category, y = total_units, fill = unit_type))+
             geom_col()+
             scale_fill_manual(values = c(" Single Family Houses" = "#414487FF", 
                                          " Secondary Apartment" = "#2A788EFF",
                                          " Two Family Bldgs" = "#22A884FF",
                                          " Three & Four Family Bldgs" = "#7AD151FF",
                                          " Five or More Family Bldgs" = "#FDE725FF"))+
             theme_void())|>
    #Default to show all
    layout(hovermode = 'x')
}


#---USER INTERFACE---

#Create app page with collapsible sidebar
ui <- page_sidebar(
  title = "Austin Permit Explorer",
  
  #Define sidebar contents
  sidebar = sidebar(
    card(
      card_header("About"),
      helpText(
        "Disclaimer:",
        br(),
        "This is a personal project and not affiliated with the City of Austin. No guarantees are made about the accuracy of the data displayed in the app.",
        br(),
        " ",
        br(),
        "Instructions:",
        br(),
        "This app shows a subset of currently active residential building permits in Austin, Texas. Use the draw controls (free draw, rectangle, or circle) on the map to select a custom area and click on the Analyze Selected Area button to filter the summary statistics.",
        br(),
        " ",
        br(),
        "Hover over each permit point to see the number of housing units. Click on the point for additional info"
      )
    ),
    
    card(
      card_header("Map Options"),
      radioButtons("basemap", 
                   label = "Basemap",
                   choices = list("Light" = "positron", "Dark" = "dark-matter", "Standard" = "voyager"),
                   selected = "positron"),
      #input_switch("hover", label = "Show Info on Hover") #Feature to add later for tooltips
    ),
    
    
    width = 350,
    #open = "always"
    
  ),
  
  #Define contents of main page area
  layout_column_wrap(
    style = css(grid_template_columns = "2fr 1fr"),
    
    #First column for map
    layout_column_wrap(
      width = 1/1,
      card(maplibreOutput("map")),
      heights_equal = "row"
    ),
    
    #Second column for stat card, chart, and button
    layout_column_wrap(
      width = 1/1,
      value_box(
        showcase = icon("house"),
        value = textOutput("feature_output"),
        title = "Housing Units Planned or Under Construction", 
        #icon("house")
        ),
      card(
        card_header("Units by Housing Type"),
        plotlyOutput("selection_chart")),
      actionButton("get_features", "Analyze Selected Area"),
      actionButton("reset", "Reset"),
      heights_equal = "row"
    )
    
  ), 

)  

  

#---APP LOGIC---
server <- function(input, output) {
  
  #Define reactive app elements
  output$map <- renderMaplibre({
    
    draw_map(active_housing_permits)
    
  })
  
  sum_units <- sum(active_housing_permits$number_of_units, na.rm = TRUE)
  
  output$feature_output <- renderText({scales::label_comma()(sum_units)})
  
  output$selection_chart <- renderPlotly({
    draw_chart_bar(summary)
    })
  
  #Filter data based on user-selected geography
  observeEvent(input$get_features, {
    
    drawn_features <- get_drawn_features(maplibre_proxy("map"))
    
    filtered_permits <- st_filter(active_housing_permits, drawn_features, .predicate = st_within)
    
    sum_units <- (sum(filtered_permits$number_of_units, na.rm = TRUE))
    
    output$feature_output <- renderText({scales::label_comma()(sum_units)})
    
    summary <- filtered_permits|>
      st_drop_geometry()|>
      group_by(unit_type)|>
      summarise(total_units = sum(number_of_units, na.rm = TRUE))|>
      mutate(category = "Active Permits")|>
      arrange(total_units)
    
    output$selection_chart <- renderPlotly({
      draw_chart_bar(summary)
    })
  })
  
  #Button to reset the dashboard
  observeEvent(input$reset, {
    
    #Reset map
    maplibre_proxy("map") |>
      clear_drawn_features()
    
    #Reset total units for card
    sum_units <- sum(active_housing_permits$number_of_units, na.rm = TRUE)
    output$feature_output <- renderText({scales::label_comma()(sum_units)})
    
    #Reset summary table for bar chart
    summary <- summary_init
    
    #Reset bar chart
    output$selection_chart <- renderPlotly({
      draw_chart_bar(summary)
    })
    
    
  })
  
  #Change basemap based on user input
  observeEvent(input$basemap,{
    maplibre_proxy("map")|>
      set_style(carto_style(input$basemap))
  })


}

#---RUN APPLICATION---
shinyApp(ui = ui, server = server)
