library(tidyverse)
library(dplyr)
library(ggplot2)
library(shiny)
library(countrycode)
library(plotly)
library(rsconnect)
library(shinyWidgets)


df <- read_csv("https://github.com/jakallio/stat436/blob/main/worldleaders.csv?raw=true") %>%
  #eliminating unused columns
  select(4:6, 8, 10, 15, 29:39, 43, 47, 51:63, 65) %>%
  
  #eliminating rows which haven't yet been coded
  drop_na(Coder_1) %>%
  
  #extracting years from start and end dates, then using country codes to create columns containing country names and regions
  mutate(startyear = as.integer(substr(startdate, 1, 4)), endyear = as.integer(substr(enddate, 1, 4)), country = countrycode(.[[1]], origin = 'cowc', destination = 'country.name'), region = countrycode(country, origin = 'country.name', destination = 'region23')) %>%
  
  #combining columns to create one which lists all countries lived in by that leader
  mutate(countries_lived_in = paste(foreign_country_1, foreign_country_2, foreign_country_3, foreign_country_n, sep = ", ")) %>%
  mutate(countries_lived_in = gsub("NA, ","", countries_lived_in), countries_lived_in = gsub("NA","", countries_lived_in)) %>%
  
  #combining columns to create one which lists all countries traveled to by that leader
  mutate(countries_traveled_to = paste(travel_country_1, travel_country_2, travel_country_3, sep = ", ")) %>%
  mutate(countries_traveled_to = gsub("NA, ","", countries_traveled_to), countries_traveled_to = gsub("NA","", countries_traveled_to)) %>%
  
  #getting rid of columns that are now irrelevant
  select(-c(foreign_country_1, foreign_country_2, foreign_country_3, foreign_country_n,travel_country_1, travel_country_2, travel_country_3))

#additional mutations for map visualizations
data2 <- df %>%
  mutate(male = ifelse(gender=='M', 1, 0)) %>%
  mutate(regular = ifelse(entry=="Regular", 1, 0)) %>%
  mutate(startyear = as.integer(substr(startdate, 1, 4)), endyear = as.integer(substr(enddate, 1, 4)))


#country by country grouping
cbcdata <- data2 %>%
  group_by(country) %>%
  summarise_at(.vars=vars(Living_Abroad_Civilian, Military_Service_Abroad, 
                          Diplomatic, First_Gen_Immigrant, Study_Abroad_Civilian, 
                          Foreign_Language, male, regular, Study_Abroad_Military),
               .funs = c(proportion="mean"), na.rm=TRUE)

#renaming to map specifications
cbcdata$country[cbcdata$country == "United States"] <- "USA"
cbcdata$country[cbcdata$country == "United Kingdom"] <- "UK"
cbcdata$country[cbcdata$country == "Bosnia & Herzegovina"] <- "Bosnia and Herzegovina"
cbcdata$country[cbcdata$country == "Congo - Brazzaville"] <- "Republic of Congo"
cbcdata$country[cbcdata$country == "Congo - Kinshasa"] <- "Democratic Republic of the Congo"
cbcdata$country[cbcdata$country == "Czechia"] <- "Czech Republic"
cbcdata$country[cbcdata$country == "Côte d’Ivoire"] <- "Ivory Coast"
cbcdata$country[cbcdata$country == "Myanmar (Burma)"] <- "Myanmar"

world_map <- map_data("world")

joineddata <- cbcdata %>%
  right_join(world_map, by=c("country"="region"), multiple="all")

find_samples <- function(df, selected_place, years) {
  filtered <- df %>%
    filter(grepl(selected_place, country, ignore.case = TRUE) | grepl(selected_place, region, ignore.case = TRUE)) %>%
    filter(startyear >= years[1] & endyear <= years[2])
  return(filtered)
}


years_abroad_pieplot <- function(df) {
  grouped <- df %>%
    filter(.[[19]] == 1) %>%
    mutate(mult_yrs = case_when(.[[20]] == 1 ~ "Less than one year", .[[21]] == 1 ~ "Between 1 and 5 years", .[[22]] == 1 ~ "Between 5 and 10 years", .[[23]] == 1 ~ "More than 10 years")) %>%
    group_by(mult_yrs) %>%
    summarize(count = n()) %>%
    drop_na()
  ggplot(grouped, aes(x="", y=count, fill=mult_yrs)) +
    geom_bar(stat="identity", width=1) +
    coord_polar("y", start=0) +
    guides(fill=guide_legend(title="Cumulative Time Spent Abroad"))
}

#creating a function which eliminates any columns that the output table shouldn't display
refine_table <- function(df) {
  refined <- df %>%
    select(leader, country, region, startyear, endyear, gender, Notes, countries_lived_in, Which_language)
}

#defining the user interface with a title, side subtitle describing the purpose of my app, a text input box for the name of countries/regions, and a year range input
ui <- fluidPage(
  titlePanel("World Leaders"),
  setBackgroundColor("mintcream"),
  tabsetPanel(
    tabPanel("Map", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(selectInput("leaderfactor", "Select leader factor", names(cbcdata)[-1], multiple=FALSE)),
               mainPanel(
                 plotlyOutput("makemap")
               )
             )
    ),
    tabPanel("Table", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(sliderInput("yrs", "Please select the years you wish to include", c(1925, 2021), min=1925, max=2021, sep=""), textInput("place", "Please enter the name of a country or region", value = "", width = NULL, placeholder = NULL)),  
               mainPanel(
                 plotOutput("piechart", width = 800, height = 400), 
                 dataTableOutput("list")
               )
             )
    )
  )
)

map_plot <- function(columns) {
  (ggplot(data=joineddata, aes(x=long, y=lat, 
                              group=group, 
                              fill=!!sym(columns))) +
    geom_polygon(color="black") +
    theme_void() +
    scale_fill_viridis_c(direction=-1,option="C")) %>%
  ggplotly()
}

#defining the server of my app, which calls my externally defined functions "find_samples" and "refine_table" to get a list output
server <- function(input, output) {
  samples <- reactive({
    find_samples(df, input$place, input$yrs)
  })
  columnselected <- reactive({input$leaderfactor})
  output$makemap <- renderPlotly(map_plot(columnselected()))
  output$piechart <- renderPlot(years_abroad_pieplot(samples()))
  output$list <- renderDataTable(refine_table(samples()))
}

#running my app
shinyApp(ui, server)