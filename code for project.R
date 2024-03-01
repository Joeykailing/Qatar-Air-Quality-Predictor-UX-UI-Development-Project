library(shiny)
library(knitr)
library(dplyr)
library(maps)
library(leaflet)
library(stringr)
library(shinycssloaders)
data(world.cities)

### Function to convert scientific notation string to integer
convert_scientific_notation_to_integer <- function(scientific_notation_string) {
  mantissa_and_exponent <- strsplit(scientific_notation_string, "E")[[1]]
  mantissa <- as.numeric(mantissa_and_exponent[1])
  exponent <- as.integer(mantissa_and_exponent[2])
  integer_value <- mantissa * 10^exponent
  return(integer_value)
}

# Define UI ----
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "gas", "Choose a gas:",
                  c("NOX",
                    "SO2")
                    ),
      selectInput(inputId = "year", "Year",
                  c("2018", "2017"), 
                  selected = "2018"),
      
      textInput(inputId = "dayStart", "Start Day",
                 value = "", placeholder = "Enter 1 - 365"),
                 selected = "1", # maybe not selected
      
      textInput(inputId = "dayEnd", "End Day",
                value = "", placeholder = "Enter 1 - 365"),
                selected = "1",
      
      # action for graph
      actionButton(inputId = "submitbutton", 
                    label = "Graph Conc"),
      h4("Locations"),
      checkboxInput(inputId = "Doha", # this will select which coordinate should be used
                    label = "Doha", value = FALSE),
      checkboxInput(inputId = "Al-Ghuwayriyah",
                    label = "Al-Ghuwayriyah", value = FALSE),
      checkboxInput(inputId = "Mukaynis",
                    label = "Mukaynis", value = FALSE),
      checkboxInput(inputId = "Dukhan",
                    label = "Dukhan", value = FALSE)
    ),
    mainPanel(
      # map
      leafletOutput(outputId = "map"),
      # graph
      shinycssloaders::withSpinner(
      plotOutput(outputId = "graph")
      ),
      # plot
      plotOutput("plot", click = "plot_click"),
      verbatimTextOutput("info")
    )
  )
)

# Define server logic ----
server <- function(input, output){
  # map
  output$map <- renderLeaflet({
    leaflet(world.cities %>%
              dplyr::filter(
                country.etc == "Qatar",
                pop > 10000
              )
    ) %>%
      addTiles() %>%
      addMarkers(lat = ~lat, lng = ~long)
  })
  
  output$graph <- renderPlot ({
    # read lines of data
    if(input$gas == "NOX"){
      df <- readLines("/Users/ace/Desktop/Research/Data-file-NOX.txt") # to get path - right click then press alt (options) and copy
    }
    if(input$gas == "SO2"){
      df <- readLines("/Users/ace/Desktop/Research/Data-file-SO2.txt") # to get path - right click then press alt (options) and copy
    }
    # empty lists
    conc <- list()
    time <- list()
    count <- 0

    # getting info from ui
    year <- strtoi(input$year)
    dayStart <- strtoi(input$dayStart)
    dayEnd <- strtoi(input$dayEnd)
    num_days <- dayEnd - dayStart
    dayList <- list(dayStart)
    for (i in 1:num_days){
      dayList <- c(dayList, dayStart + i)
    }
    
    location_column <- 4
    
    if(input$submitbutton){
      for (i in 15:758) { # 758 is the number of rows of data in the file
        conc_list <- unlist(strsplit(df[i], split = "  ")) # splitting data into parts
        conc_list_year <- trimws(conc_list[1])
        conc_list_day <- unlist(strsplit(trimws(conc_list[2]), split = " "))
        if (conc_list_year == year && conc_list_day[1] %in% dayList) # probably need to add location (need dictionary to convert Doha to location point)      
        {
          conc <- c(conc, convert_scientific_notation_to_integer(conc_list[location_column]))
          count = count + 1
          time <- c(time, count)
        }
      }
    }
      Sys.sleep(1.5)
      plot(runif(5))
      # plotting conc and time
      plot(time, conc, main = "Graph",
           xlab = "Time (hr)", ylab = "Concentration (ug/m**3)", type = "l", col = "red"
      )
  })

  # Doha
  output$plot <- renderPlot ({
    if(input$Doha){
      ####################code for graph####################
      # changing string scientific notation into integers and adding it to x and y list
      x_values <- trimws(df[10])
      y_values <- trimws(df[11])
      x_list <- unlist(strsplit(x_values, split = "  ")) # list of each number in scientific string notation 
      y_list <- unlist(strsplit(y_values, split = "  "))
      x <- list()
      y <- list()
      for (i in 2:7372) { 
        x <- c(x, convert_scientific_notation_to_integer(x_list[i]))
        y <- c(y, convert_scientific_notation_to_integer(y_list[i]))
      }
      # plotting x and y
      plot(x, y, main = "Locations",
           xlab = "X coordinate (km)", ylab = "Y coordinate (km)"
      )
    }
      })
}
# Run the app ----
shinyApp(ui = ui, server = server)











