###SHINY APP VISUALIZATION###

# filip-baumgartner.shinyapp.io/autobazar


library(dplyr)
library(tidyverse)
library(stringr)
library(forcats)
library(readr)
library(rsconnect)
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(visdat)
library(markdown)
library(knitr)
library(caret)
library(e1071)
library(ranger)

autobazar_shiny <- read.csv("data/autobazar_shiny_final.csv", encoding="UTF-8", header=T)
top10_brands <- read.csv("data/top10_brands.csv", encoding="UTF-8", header=T)
forest <- readRDS("data/model.tree2_250.rds")
model.lm <- readRDS("data/model.lm.rds")

autobazar_shiny$price_data <- as.numeric(autobazar_shiny$price_data)
autobazar_shiny$km_data <- as.numeric(autobazar_shiny$km_data)
autobazar_shiny$kw_data <- as.numeric(autobazar_shiny$kw_data)
autobazar_shiny$year_data <- as.numeric(autobazar_shiny$year_data)


ui <- fluidPage(
  
theme = shinythemes::shinytheme("cerulean"),  
tags$h3("AutoBazar.eu Data Visualization and Price Predictor"),
tags$h5("Dataset updated: April 2021"), 
"Be patient while Loading, please. Sometimes it may take a while.",
navbarPage("Menu",
           tabPanel("Visualization",
titlePanel("Select Inputs:"),
fluidRow(
  column(3,
         radioGroupButtons(
        inputId = "oput",
        label = "Plot:",
        choices = sort(c("Prices (Boxplot)"="Prices", "Counts (Bar Plot)"= "Counts","Distributions (Histogram)" = "Distributions", "Relationships (Scatter Plot)" = "Scatter")),
        selected = "Counts",
        direction = "vertical",
        individual = TRUE,
        checkIcon = list(
          yes = tags$i(class = "fa fa-circle", 
                       style = "color: steelblue"),
          no = tags$i(class = "fa fa-circle-o", 
                      style = "color: steelblue"))
    ),
    
    conditionalPanel(condition = "input.oput == 'Prices' || input.oput == 'Counts'", 
        awesomeRadio(
        inputId = "by",
        label = "Grouped by:",
        choices = c("Brands", "Models"),
        selected = "Models"
    )),
    
    conditionalPanel(condition = "(input.by == 'Models' && input.comparison == 'No')",
              pickerInput(inputId = "bar",
                label = "Select Make (Brand):",
                choices = sort(c("Skoda" = "Skoda", "Audi"="Audi","BMW" = "Bmw","Alfa Romeo" = "Alfa Romeo","Bentley" = "Bentley","Chevrolet" = "Chevrolet","Chrysler" = "Chrysler","Citroen" = "Citroen","Dacia" = "Dacia","Ferrari" = "Ferrari","Fiat" = "Fiat","Ford" = "Ford","Honda" = "Honda","Hyundai" = "Hyundai", 
                                 "Jaguar" = "Jaguar","Jeep" = "Jeep","VW" = "Volkswagen","Kia"= "Kia","Lamborghini" = "Lamborghini","Land Rover" = "Land Rover","Mazda" = "Mazda","Mercedes" = "Mercedes", 
                                 "Mini"= "Mini","Mitsubishi" = "Mitsubishi","Nissan" = "Nissan","Opel" = "Opel","Peugeot" = "Peugeot","Porsche"= "Porsche","Renault" = "Renault","Seat" = "Seat","Smart" = "Smart","Subaru" = "Subaru","Suzuki"= "Suzuki","Tesla" = "Tesla","Toyota" = "Toyota","Volvo" = "Volvo", "Lexus" = "Lexus", "Maserati"="Maserati")),
                selected = "Skoda"
    )),
    
    conditionalPanel(condition = "(input.by == 'Models' && input.comparison == 'No')",
                     numericInput(
                       inputId = "n",
                       label = "Min Count of Offers:",
                       value = 0,
                       min = 0, max = 20
                     ),
                     p("!Update only in the case of too many (overlapping) labels!")),
    
    conditionalPanel(condition = "(input.by == 'Brands' && input.comparison == 'No' && input.oput == 'Counts') || (input.by == 'Brands' && input.comparison == 'No' && input.oput == 'Prices')",
                     multiInput(inputId = "brands",
                                 label = "Select Make (Brands):",
                                 choices = sort(c("Skoda" = "Skoda", "Audi"="Audi","BMW" = "Bmw","Alfa Romeo" = "Alfa Romeo","Bentley" = "Bentley","Chevrolet" = "Chevrolet","Chrysler" = "Chrysler","Citroen" = "Citroen","Dacia" = "Dacia","Ferrari" = "Ferrari","Fiat" = "Fiat","Ford" = "Ford","Honda" = "Honda","Hyundai" = "Hyundai", 
                                                  "Jaguar" = "Jaguar","Jeep" = "Jeep","VW" = "Volkswagen","Kia"= "Kia","Lamborghini" = "Lamborghini","Land Rover" = "Land Rover","Mazda" = "Mazda","Mercedes" = "Mercedes", 
                                                  "Mini"= "Mini","Mitsubishi" = "Mitsubishi","Nissan" = "Nissan","Opel" = "Opel","Peugeot" = "Peugeot","Porsche"= "Porsche","Renault" = "Renault","Seat" = "Seat","Smart" = "Smart","Subaru" = "Subaru","Suzuki"= "Suzuki","Tesla" = "Tesla","Toyota" = "Toyota","Volvo" = "Volvo", "Lexus" = "Lexus", "Maserati"="Maserati")),
                                 selected = c("Skoda", "Audi", "Bmw", "Volkswagen", "Mercedes", "Seat", "Toyota", "Ford", "Renault")
                     )),
    
    conditionalPanel(condition = "input.oput == 'Distributions' && input.comparison == 'No'",
                     selectInput(inputId = "dist",
                                 label = "Distribution by:",
                                 choices=c("Power (kW)" = "kw_data", "Mileage (km)" = "km_data", "Price" = "price_data"))),

    conditionalPanel(condition = "input.oput == 'Distributions' && input.comparison == 'No'",
                     materialSwitch(inputId = "dist_gen",
                                 label = "General Distribution of All Observations:",
                            
                                 status = "primary",
                                 value = TRUE)),   

    conditionalPanel(condition = "input.by == 'Models' && (input.oput == 'Prices' || input.oput == 'Counts')",
    pickerInput(
      inputId = "comparison",
      label = "Model's Comparisons of more Brands",
      choices = c("No" = "No", "Yes" = "Yes"),
      selected = "No"
    ))
    ),
  
  column(3,  
    conditionalPanel(condition = "input.comparison == 'Yes' && input.by == 'Models' && (input.oput == 'Prices' || input.oput == 'Counts')",
                     radioButtons(
                       inputId = "comp_by",
                       label = "Compare by:",
                       choices = sort(c("Regions" = "region_data", "Vehicle Type" = "type_data", "Fuel Type" = "diesel_data", "Transmission" = "transmission_data", "4x4" = "quattro", "Years of Manufacturing:" ="year_data")),
                       selected = "diesel_data"
                     )),
    
    conditionalPanel(condition = "input.comparison == 'Yes' && input.by == 'Models' && (input.oput == 'Prices' || input.oput == 'Counts')",
                     numericInput(
                       inputId = "top_n",
                       label = "Count of most offered models to compare:",
                       value = 5,
                       min = 1, max = 10
                     )),
    

conditionalPanel(condition = "input.oput == 'Scatter' && input.comparison == 'No'",
                 p("All brands are included, no comparation between them is possible"),
                 pickerInput(
                   inputId = "scatter_y",
                   label = "Select variables for X axis:",
                   choices = c("Price" = "price_data", "Mileage (km)" = "km_data", "Power (kW)" = "kw_data"),
                   selected = "price_data"
                 )),
conditionalPanel(condition = "input.oput == 'Scatter' && input.comparison == 'No'",
                 awesomeCheckbox(
                   label = "Log?",
                   inputId = "log_y",
                   value = FALSE
                 )
),

conditionalPanel(condition = "input.oput == 'Scatter' && input.comparison == 'No'",
                 pickerInput(
                   inputId = "scatter_x",
                   label = "Select variables for X axis:",
                   choices = c("Price" = "price_data", "Mileage (km)" = "km_data", "Power (kW)" = "kw_data"),
                   selected = "km_data"
                 )),
conditionalPanel(condition = "input.oput == 'Scatter' && input.comparison == 'No'",
                 awesomeCheckbox(
                   label = "Log?",
                   inputId = "log_x",
                   value = FALSE
                 )
),
    
    conditionalPanel(condition = "((input.oput == 'Distributions' && input.comparison == 'No') || (input.oput == 'Counts' && input.comparison == 'Yes' && input.by == 'Brands') || (input.oput == 'Prices' && input.comparison == 'Yes' && input.by == 'Brands')) && input.oput != 'Scatter'",
      multiInput(
        inputId = "comparison_brand",
        label="Select exact brands to compare:",
        choices = sort(c("Skoda" = "Skoda", "Audi"="Audi","BMW" = "Bmw","Alfa Romeo" = "Alfa Romeo","Bentley" = "Bentley","Chevrolet" = "Chevrolet","Chrysler" = "Chrysler","Citroen" = "Citroen","Dacia" = "Dacia","Ferrari" = "Ferrari","Fiat" = "Fiat","Ford" = "Ford","Honda" = "Honda","Hyundai" = "Hyundai", 
                         "Jaguar" = "Jaguar","Jeep" = "Jeep","VW" = "Volkswagen","Kia"= "Kia","Lamborghini" = "Lamborghini","Land Rover" = "Land Rover","Mazda" = "Mazda","Mercedes" = "Mercedes", 
                         "Mini"= "Mini","Mitsubishi" = "Mitsubishi","Nissan" = "Nissan","Opel" = "Opel","Peugeot" = "Peugeot","Porsche"= "Porsche","Renault" = "Renault","Seat" = "Seat","Smart" = "Smart","Subaru" = "Subaru","Suzuki"= "Suzuki","Tesla" = "Tesla","Toyota" = "Toyota","Volvo" = "Volvo", "Lexus" = "Lexus", "Maserati"="Maserati")),
        selected = c("Skoda", "Audi")
      ))  
    ),
  column(3,    
   
        pickerInput(
        inputId = "region",
        label = "Select Location (regions):",
        selected = "BA kraj",
        multiple = TRUE,
        choices = sort(c("Zilina region"="ZA kraj", "Bratislava region"="BA kraj", "Kosice region"="KE kraj", "Banska Bystrica region"="BB kraj", "Trnava region"="TT kraj", "Nitra region"="NR kraj", "Presov region"="PO kraj"))
    ),
    
    materialSwitch(
        inputId = "svk",
        label = "Entire Slovakia",
        right = TRUE,
        status = "primary",
        value = TRUE
    ),
    
    checkboxGroupInput(
        inputId = "gear",
        label="Select Transmission:",
        choices= c("Automatic transmission" = "Automat", "Manual gearbox" = "Manuál"),
        selected = c("Automat", "Manuál")
    ),
    
    checkboxGroupInput(
        inputId = "engine",
        label="Select Fuel Type:",
        choices=sort(c("Diesel"="Diesel", "Gasoline"="Benzín", "Hybrid"="Hybrid", "Electric"="Elektro", "Gasoline + LPG"="Benzín+Plyn")),
        selected = c("Diesel", "Benzín", "Hybrid", "Elektro", "Benzín+Plyn")
    )
    ),
    column(3,
    sliderInput(inputId = "num",
                label = "Choose range for Year(s) of Manufacturing :",
                min=1962, max=2021, value=c(2000,2021), step=1, sep="", pre="year "
                
                
    ),
    sliderInput(inputId = "km",
                label = "Choose range for Mileage (km):",
                min=1, max=999000, value=c(1,500000), step=1000, sep=" ", post="km"),
    
    sliderInput(inputId = "kw",
                label = "Choose range for Power (kW):",
                min=1, max=700, value=c(1,654), step=10, sep=" ", post="kW"),
    
    sliderInput(inputId = "price",
                label = "Choose range for Price (EUR):",
                min=1, max=350000, value=c(1,200000), step=10, sep=" ", post="EUR"),
    
    pickerInput(
        inputId = "type",
        label = "Select Car's Body:",
        multiple = TRUE,
        selected = "Hatchback",
        #AUTOMAT deleted
        choices = c("Hatchback"="Hatchback", "Cabrio"="Cabrio", "Mpv"="Mpv", "Combi"="Combi", "Sedan"="Sedan", "Dodávka"="Dodávka",
                "Van"="Van", "Pick up"="Pick up","Bus"="Bus", "Limuzína"="Limuzína", "Liftback"="Liftback", "Roadster"="Roadster", "Coupé"="Coupé")
    ),
    
    materialSwitch(
      inputId = "all_types",
      label = "All Types",
      right = TRUE,
      status = "primary",
      value = TRUE
    )
    
    )),

    fluidRow(plotOutput("bar"), 
             
)),

tabPanel("Price Predictor",
         fluidRow(column(4, 
         pickerInput(inputId = "bar2",
                     label = "Select Make (Brand):",
                     choices = sort(c("Skoda" = "Skoda", "Audi"="Audi","BMW" = "Bmw","Ford" = "Ford", 
                                      "VW" = "Volkswagen","Kia"= "Kia","Mercedes" = "Mercedes", 
                                      "Opel" = "Opel","Peugeot" = "Peugeot","Renault" = "Renault")),
                     selected = "Skoda"
         ),
         
         numericInput(
           inputId = "year_tree",
           label = "Year:",
           value = 2010,
           min = 2000, max = 2020
         ),
         pickerInput(inputId = "type_factor",
                     label = "Select Body:",
                     choices = sort(c("Sedan" = "Sedan", "MPV" = "Mpv", "Limo" = "Limuzína", "Liftback" = "Liftback", 
                                      "Hatchback" = "Hatchback", "Coupé" = "Coupé", "Combi" = "Combi", "Other" = "Iné")),
                     selected = "Sedan"
         )), 
         column(4, pickerInput(inputId = "fuel_factor",
                     label = "Select Fuel:",
                     choices = sort(c("Gasoline" = "Benzín", "Diesel" = "Diesel", "Electro" = "Elektro")),
                     selected = "Diesel"
         ), 
         
         pickerInput(inputId = "transmission_factor",
                     label = "Select Transmission:",
                     choices = sort(c("Manual Gearbox" = "Manuál", "Automatic transmission" = "Automat")),
                     selected = "Manuál"
         ),  
         
         pickerInput(inputId = "quattro_factor",
                     label = "4x4?:",
                     choices = sort(c("Yes" = 1, "No" = 0)),
                     selected = 0
         ), 
         numericInput(
           inputId = "km_tree",
           label = "Mileage (km):",
           value = 250000,
           min = 0, max = 700000, step=5000
         ),   
         numericInput(
           inputId = "kw_tree",
           label = "Power (kW):",
           value = 80,
           min = 0, max = 600
         ))
         
         ),
         fluidRow(
           actionButton("go", "Predict"),
           p("Predicted price:"),
       tableOutput("tree"),
       p("The real offers containing given parameters:"),
       tableOutput("true")
         )),
tabPanel("About Price Predictor",fluidRow(uiOutput("modelmarkdown"))),
tabPanel("About Dataset",fluidRow(uiOutput("markdown")))
))

server <- function(input, output, session) {
  
  observeEvent(input$oput == "Distributions" | input$by == "Brands"| input$by == "Brands", {
    updatePickerInput(session = session, inputId = "comparison", selected = "No")
  })
  
  
  observeEvent(input$oput == "Scatter" | input$oput == "Distributions", ignoreInit = T, {
    updateAwesomeRadio(session = session, inputId = "by", selected = "Brands")
  })
  

  
    data <- reactive({
        if (input$all_types == TRUE & input$svk == FALSE & input$comparison == "No"){
            autobazar_shiny%>%filter(price_data >= as.numeric(input$price[1]) & price_data <= as.numeric(input$price[2]) & brand == input$bar & year_data >= input$num[1] & year_data <= input$num[2] & region_data %in% input$region & transmission_data %in% input$gear & diesel_data%in%input$engine & km_data >= as.numeric(input$km[1]) & km_data <= as.numeric(input$km[2]) & kw_data >= as.numeric(input$kw[1]) & kw_data <= as.numeric(input$kw[2]))
        }else if(input$all_types == FALSE & input$svk == TRUE & input$comparison == "No"){        
            autobazar_shiny%>%filter(price_data >= as.numeric(input$price[1]) & price_data <= as.numeric(input$price[2]) & brand == input$bar & year_data >= input$num[1] & year_data <= input$num[2] & diesel_data%in%input$engine & transmission_data%in%input$gear & km_data >= as.numeric(input$km[1]) & km_data <= as.numeric(input$km[2]) & kw_data >= as.numeric(input$kw[1]) & kw_data <= as.numeric(input$kw[2]) & type_data%in%input$type)
        }else if(input$all_types == TRUE & input$svk == TRUE & input$comparison == "No"){        
            autobazar_shiny%>%filter(price_data >= as.numeric(input$price[1]) & price_data <= as.numeric(input$price[2]) & brand == input$bar & year_data >= input$num[1] & year_data <= input$num[2] & diesel_data%in%input$engine & transmission_data%in%input$gear & km_data >= as.numeric(input$km[1]) & km_data <= as.numeric(input$km[2]) & kw_data >= as.numeric(input$kw[1]) & kw_data <= as.numeric(input$kw[2]))
            }else{        
            autobazar_shiny%>%filter(price_data >= as.numeric(input$price[1]) & price_data <= as.numeric(input$price[2]) & brand == input$bar & year_data >= input$num[1] & year_data <= input$num[2] & region_data %in% input$region & transmission_data%in%input$gear & diesel_data%in%input$engine & km_data >= as.numeric(input$km[1]) & km_data <= as.numeric(input$km[2]) & kw_data >= as.numeric(input$kw[1]) & kw_data <= as.numeric(input$kw[2]) & type_data%in%input$type )
}
    })

   data2 <- reactive({
        if (input$all_types == TRUE & input$svk == FALSE){
            autobazar_shiny%>%filter(price_data >= as.numeric(input$price[1]) & price_data <= as.numeric(input$price[2]) & brand %in% input$brands & year_data >= input$num[1] & year_data <= input$num[2] & region_data %in% input$region & transmission_data%in%input$gear & diesel_data%in%input$engine & km_data >= as.numeric(input$km[1]) & km_data <= as.numeric(input$km[2]) & kw_data >= as.numeric(input$kw[1]) & kw_data <= as.numeric(input$kw[2]) )
        }else if(input$all_types == FALSE & input$svk == TRUE){        
            autobazar_shiny%>%filter(price_data >= as.numeric(input$price[1]) & price_data <= as.numeric(input$price[2]) & brand %in% input$brands & year_data >= input$num[1] & year_data <= input$num[2] & diesel_data%in%input$engine & transmission_data%in%input$gear & km_data >= as.numeric(input$km[1]) & km_data <= as.numeric(input$km[2]) & kw_data >= as.numeric(input$kw[1]) & kw_data <= as.numeric(input$kw[2]) & type_data%in%input$type )
        }else if(input$all_types == TRUE & input$svk == TRUE){        
            autobazar_shiny%>%filter(price_data >= as.numeric(input$price[1]) & price_data <= as.numeric(input$price[2]) & brand %in% input$brands & year_data >= input$num[1] & year_data <= input$num[2] & diesel_data%in%input$engine & transmission_data%in%input$gear & km_data >= as.numeric(input$km[1]) & km_data <= as.numeric(input$km[2]) & kw_data >= as.numeric(input$kw[1]) & kw_data <= as.numeric(input$kw[2]) )
        }else{        
            autobazar_shiny%>%filter(price_data >= as.numeric(input$price[1]) & price_data <= as.numeric(input$price[2]) & brand %in% input$brands & year_data >= input$num[1] & year_data <= input$num[2] & region_data%in%input$region & transmission_data%in%input$gear & diesel_data%in%input$engine & km_data >= as.numeric(input$km[1]) & km_data <= as.numeric(input$km[2]) & kw_data >= as.numeric(input$kw[1]) & kw_data <= as.numeric(input$kw[2]) & type_data%in%input$type )
        }
    })
   
   data3 <- reactive({
     if (input$all_types == TRUE & input$svk == FALSE){
       autobazar_shiny%>%filter(price_data >= as.numeric(input$price[1]) & price_data <= as.numeric(input$price[2]) & brand %in% input$comparison_brand & year_data >= input$num[1] & year_data <= input$num[2] & region_data %in% input$region & transmission_data %in% input$gear & diesel_data%in%input$engine & km_data >= as.numeric(input$km[1]) & km_data <= as.numeric(input$km[2]) & kw_data >= as.numeric(input$kw[1]) & kw_data <= as.numeric(input$kw[2]))
     }else if(input$all_types == FALSE & input$svk == TRUE){        
       autobazar_shiny%>%filter(price_data >= as.numeric(input$price[1]) & price_data <= as.numeric(input$price[2]) & brand %in% input$comparison_brand & year_data >= input$num[1] & year_data <= input$num[2] & diesel_data%in%input$engine & transmission_data%in%input$gear & km_data >= as.numeric(input$km[1]) & km_data <= as.numeric(input$km[2]) & kw_data >= as.numeric(input$kw[1]) & kw_data <= as.numeric(input$kw[2]) & type_data%in%input$type)
     }else if(input$all_types == TRUE & input$svk == TRUE){        
       autobazar_shiny%>%filter(price_data >= as.numeric(input$price[1]) & price_data <= as.numeric(input$price[2]) & brand %in% input$comparison_brand & year_data >= input$num[1] & year_data <= input$num[2] & diesel_data%in%input$engine & transmission_data%in%input$gear & km_data >= as.numeric(input$km[1]) & km_data <= as.numeric(input$km[2]) & kw_data >= as.numeric(input$kw[1]) & kw_data <= as.numeric(input$kw[2]))
     }else{        
       autobazar_shiny%>%filter(price_data >= as.numeric(input$price[1]) & price_data <= as.numeric(input$price[2]) & brand %in% input$comparison_brand & year_data >= input$num[1] & year_data <= input$num[2] & region_data %in% input$region & transmission_data%in%input$gear & diesel_data%in%input$engine & km_data >= as.numeric(input$km[1]) & km_data <= as.numeric(input$km[2]) & kw_data >= as.numeric(input$kw[1]) & kw_data <= as.numeric(input$kw[2]) & type_data%in%input$type )
     }
   })  

   data4 <-  reactive({ 
     data <- data3();
     data %>% group_by(brand) %>% count(model, sort=T) %>% slice(1:as.numeric(input$top_n)) }) 
     
   data5 <- reactive({
     if (input$all_types == TRUE & input$svk == FALSE){
       autobazar_shiny%>%filter(price_data >= as.numeric(input$price[1]) & price_data <= as.numeric(input$price[2]) & year_data >= input$num[1] & year_data <= input$num[2] & region_data %in% input$region & transmission_data %in% input$gear & diesel_data%in%input$engine & km_data >= as.numeric(input$km[1]) & km_data <= as.numeric(input$km[2]) & kw_data >= as.numeric(input$kw[1]) & kw_data <= as.numeric(input$kw[2]))
     }else if(input$all_types == FALSE & input$svk == TRUE){        
       autobazar_shiny%>%filter(price_data >= as.numeric(input$price[1]) & price_data <= as.numeric(input$price[2]) & year_data >= input$num[1] & year_data <= input$num[2] & diesel_data%in%input$engine & transmission_data%in%input$gear & km_data >= as.numeric(input$km[1]) & km_data <= as.numeric(input$km[2]) & kw_data >= as.numeric(input$kw[1]) & kw_data <= as.numeric(input$kw[2]) & type_data%in%input$type)
     }else if(input$all_types == TRUE & input$svk == TRUE){        
       autobazar_shiny%>%filter(price_data >= as.numeric(input$price[1]) & price_data <= as.numeric(input$price[2]) & year_data >= input$num[1] & year_data <= input$num[2] & diesel_data%in%input$engine & transmission_data%in%input$gear & km_data >= as.numeric(input$km[1]) & km_data <= as.numeric(input$km[2]) & kw_data >= as.numeric(input$kw[1]) & kw_data <= as.numeric(input$kw[2]))
     }else{        
       autobazar_shiny%>%filter(price_data >= as.numeric(input$price[1]) & price_data <= as.numeric(input$price[2]) & year_data >= input$num[1] & year_data <= input$num[2] & region_data %in% input$region & transmission_data%in%input$gear & diesel_data%in%input$engine & km_data >= as.numeric(input$km[1]) & km_data <= as.numeric(input$km[2]) & kw_data >= as.numeric(input$kw[1]) & kw_data <= as.numeric(input$kw[2]) & type_data%in%input$type )
     }
   })  
       
   
    output$bar <-  renderPlot({
        
        #autobazar%>%filter(brand == input$bar & year_data == as.numeric(input$num))%>%group_by(model)%>%count(model)%>%ggplot(.,aes(x=reorder(model,n), y=n, fill=model))+geom_bar(stat="identity")+coord_flip()+ theme(legend.position = "none")
        #data()%>%group_by(model)%>%count(model)%>%ggplot(.,aes(x=reorder(model,n), y=n, fill=model))+geom_bar(stat="identity")+coord_flip()+ theme(legend.position = "none")+labs(x="Model", y="Count", title=paste("Used cars for sell of brand",input$bar), subtitle=paste("Year(s) of production:", list(input$num)), caption="Source: www.AutoBazar.eu")
        if (input$oput == "Prices" & input$by == "Models" & input$comparison == "No"){
            data()%>%group_by(model)%>%mutate(n = n())%>%filter(n >= as.numeric(input$n))%>%ggplot(.,aes(x=reorder(model,price_data), y=price_data, color=model))+geom_boxplot(na.rm=T, show.legend = FALSE)+stat_summary(fun=mean, geom="point", shape=20, size=3, color="darkred", fill="red")+coord_flip()+
            stat_summary(fun=mean, geom="text", color="red", show.legend = FALSE, hjust=-0.1, aes( label=round(..y.., digits=0)))+labs(x="Model", y="EUR", title=paste("Boxplot: Prices of used cars; Brand:", input$bar), subtitle="Red Dot = Mean Price", caption="Visualization by: filip-baumgartner.shinyapps.io/autobazar; Data source: www.AutoBazar.eu")  
    }else if (input$oput == "Counts" & input$by == "Models" & input$comparison == "No"){
            data()%>%group_by(model)%>%count(model)%>%filter(n >= as.numeric(input$n))%>%ggplot(.,aes(x=reorder(model,n), y=n, fill=model))+geom_bar(stat="identity")+geom_text(aes(label=n), check_overlap = T, hjust = -0.1, vjust=0.1)+ coord_flip()+theme(legend.position = "none")+
      labs(x="Model", y="Count", title=paste("Count of used cars for sell; Brand:", input$bar), subtitle=paste("Manufacturing year(s):", list(input$num)), caption="Visualization by: filip-baumgartner.shinyapps.io/autobazar; Data source: www.AutoBazar.eu")
    }else if(input$oput == "Prices" & input$by == "Brands" & input$comparison == "No"){
        data2()%>%group_by(brand)%>%ggplot(.,aes(x=reorder(brand,-price_data), y=price_data, color=brand))+geom_boxplot(na.rm=T, show.legend = FALSE)+stat_summary(fun=mean, geom="point", shape=20, size=3, color="red", fill="red")+coord_flip()+
        stat_summary(fun=mean, geom="text", color="red", show.legend = FALSE, hjust=-0.1, aes( label=round(..y.., digits=0)))+labs(x="Brand", y="EUR", title="Boxplot: Prices of used cars", subtitle="Red Dot = Mean Price", caption="Visualization by: filip-baumgartner.shinyapps.io/autobazar; Data source: www.AutoBazar.eu")  
    }else if(input$oput == "Counts" & input$by == "Brands" & input$comparison == "No"){
        data2()%>%group_by(brand)%>%count(brand)%>%ggplot(.,aes(x=reorder(brand,n), y=n, fill=brand))+geom_bar(stat="identity")+geom_text(aes(label=n), check_overlap = T, hjust = -0.1, vjust=0.1)+coord_flip()+ theme(legend.position = "none")+
       labs(x="Brand", y="Count", title="Count of used cars for sell", subtitle=paste("Manufacturing year(s):", list(input$num)), caption="Visualization by: filip-baumgartner.shinyapps.io/autobazar; Data source: www.AutoBazar.eu") 
    }else if(input$oput == "Distributions"  & input$dist == "kw_data" & input$comparison == "No" & input$dist_gen == FALSE){
      data3()%>%ggplot(.,aes(x=kw_data, fill=brand))+geom_histogram( na.rm=T, binwidth = 10)+facet_wrap(~brand,scales="free")+geom_vline(aes(xintercept=mean(kw_data)),color="red", linetype="dashed", size=1)+theme(legend.position = "none")+
        geom_text(aes(label = "line = mean", x = mean(kw_data), y=mean(kw_data), angle=90), vjust = -0.3)+labs(x="kW", y="Count", title="Distribution by Power (kW); bin = 10 kW", subtitle=paste("Manufacturing year(s):", list(input$num)), caption="Visualization by: filip-baumgartner.shinyapps.io/autobazar; Data source: www.AutoBazar.eu") 
    }else if(input$oput == "Distributions"  & input$dist == "km_data" & input$comparison == "No" & input$dist_gen == FALSE){
      data3()%>%ggplot(.,aes(x=km_data, fill = brand))+geom_histogram( na.rm=T, binwidth = 10000)+facet_wrap(~brand,scales="free")+ geom_vline(aes(xintercept=mean(km_data)),color="red", linetype="dashed", size=1)+theme(legend.position = "none")+
        geom_text(aes(label = "line = mean", x = mean(km_data), y=mean(km_data), angle=90), vjust = -0.3)+labs(x="km", y="Count", title="Distribution by Mileage (km); bin = 10 000 km", subtitle=paste("Manufacturing year(s):", list(input$num)), caption="Visualization by: filip-baumgartner.shinyapps.io/autobazar; Data source: www.AutoBazar.eu") 
    }else if(input$oput == "Distributions"  & input$dist == "price_data" & input$comparison == "No" & input$dist_gen == FALSE){
      data3()%>%ggplot(.,aes(x=price_data, fill = brand))+geom_histogram( na.rm=T, binwidth = 5000)+facet_wrap(~brand,scales="free")+ geom_vline(aes(xintercept=mean(price_data)),color="red", linetype="dashed", size=1)+theme(legend.position = "none")+
        geom_text(aes(label = "line = mean", x = mean(price_data), y=mean(price_data), angle=90), vjust = -0.3)+labs(x="EUR", y="Count", title="Distribution by price; bin = 5 000 EUR", subtitle=paste("Manufacturing year(s):", list(input$num)), caption="Visualization by: filip-baumgartner.shinyapps.io/autobazar; Data source: www.AutoBazar.eu")
    }else if(input$oput == "Distributions"  & input$dist == "kw_data" & input$dist_gen == TRUE & input$comparison == "No"){
      data5()%>%ggplot(.,aes(x=kw_data, fill=brand))+geom_histogram( na.rm=T, binwidth = 10)+geom_vline(aes(xintercept=mean(kw_data)),color="red", linetype="dashed", size=1)+theme(legend.position = "right")+
        geom_text(aes(label = "line = mean", x = mean(kw_data), y=mean(kw_data), angle=90), vjust = -0.3)+labs(x="kW", y="Count", title="Distribution by Power (kW); bin = 10 kW", subtitle=paste("Manufacturing year(s):", list(input$num)), caption="Visualization by: filip-baumgartner.shinyapps.io/autobazar; Data source: www.AutoBazar.eu") 
    }else if(input$oput == "Distributions"  & input$dist == "km_data"  & input$dist_gen == TRUE & input$comparison == "No"){
      data5()%>%ggplot(.,aes(x=km_data, fill = brand))+geom_histogram( na.rm=T, binwidth = 10000)+ geom_vline(aes(xintercept=mean(km_data)),color="red", linetype="dashed", size=1)+theme(legend.position = "right")+
        geom_text(aes(label = "line = mean", x = mean(km_data), y=mean(km_data), angle=90), vjust = -0.3)+labs(x="km", y="Count", title="Distribution by Mileage (km); bin = 10 000 km", subtitle=paste("Manufacturing year(s):", list(input$num)), caption="Visualization by: filip-baumgartner.shinyapps.io/autobazar; Data source: www.AutoBazar.eu") 
      }else if(input$oput == "Distributions"  & input$dist == "price_data" & input$dist_gen == TRUE & input$comparison == "No"){
      data5()%>%ggplot(.,aes(x=price_data, fill = brand))+geom_histogram( na.rm=T, binwidth = 5000)+ geom_vline(aes(xintercept=mean(price_data)),color="red", linetype="dashed", size=1)+theme(legend.position = "right")+
        geom_text(aes(label = "line = mean", x = mean(price_data), y=mean(price_data), angle=90), vjust = -0.3)+labs(x="EUR", y="Count", title="Distribution by price; bin = 5 000 EUR", subtitle=paste("Manufacturing year(s):", list(input$num)), caption="Visualization by: filip-baumgartner.shinyapps.io/autobazar; Data source: www.AutoBazar.eu")
      }else if(input$oput == "Counts" & input$by == "Models" & input$comparison == "Yes"){
       data3()%>%filter(model %in% data4()$model)%>%group_by(get(input$comp_by))%>%ggplot(.,aes(x=model, fill=model))+geom_bar()+geom_text(stat='count', aes(label=..count..), hjust=-0.1)+facet_grid(brand~get(input$comp_by), scales="free")+coord_flip()+ theme(legend.position = "none")+
        labs(x="Model", y="Count", title = "Count of used cars for sell", subtitle=paste("Manufacturing year(s):", list(input$num)), caption="Visualization by: filip-baumgartner.shinyapps.io/autobazar; Data source: www.AutoBazar.eu")
      }else if(input$oput == "Prices" & input$by == "Models" & input$comparison == "Yes"){
      data3()%>%filter(model %in% data4()$model)%>%group_by(get(input$comp_by))%>%ggplot(.,aes(x=reorder(model,price_data) , y=price_data, color=model))+geom_boxplot(na.rm=T, show.legend = FALSE)+stat_summary(fun=mean, geom="point", shape=20, size=3, color="red", fill="red")+facet_grid(brand~get(input$comp_by), scales="free")+coord_flip()+
          stat_summary(fun=mean, geom="text", color="red", show.legend = FALSE, hjust=-0.1, aes( label=round(..y.., digits=0)))+labs(x="Model", y="EUR", title="Boxplot: Prices of used cars", subtitle="Red Dot = Mean Price", caption="Visualization by: filip-baumgartner.shinyapps.io/autobazar; Data source: www.AutoBazar.eu") 
      }else if(input$oput == "Scatter" & input$log_x == FALSE & input$log_y == FALSE){
        data5()%>%ggplot(., aes(x=get(input$scatter_x), y=get(input$scatter_y)))+geom_point(na.rm = T)+stat_smooth(method = "lm", formula = y ~ x , size = 1)+
          labs(x=input$scatter_x, y=input$scatter_y, main="Scatter plot", subtitle=paste("Manufacturing year(s):", list(input$num)), caption="Visualization by: filip-baumgartner.shinyapps.io/autobazar; Data source: www.AutoBazar.eu")
      }else if(input$oput == "Scatter" &  input$log_x == TRUE & input$log_y == FALSE){
        data5()%>%ggplot(., aes(x=log(get(input$scatter_x)), y=get(input$scatter_y)))+geom_point(na.rm = T)+stat_smooth(method = "lm", formula = y ~ x , size = 1)+
          scale_x_log10()+labs(x=input$scatter_x, y=input$scatter_y, main="Scatter plot", subtitle=paste("Manufacturing year(s):", list(input$num)), caption="Visualization by: filip-baumgartner.shinyapps.io/autobazar; Data source: www.AutoBazar.eu")
      }else if(input$oput == "Scatter" & input$log_x == FALSE & input$log_y == TRUE){
        data5()%>%ggplot(., aes(x=get(input$scatter_x), y=log(get(input$scatter_y))))+geom_point(na.rm = T)+stat_smooth(method = "lm", formula = y ~ x , size = 1)+
          scale_y_log10()+ labs(x=input$scatter_x, y=input$scatter_y, main="Scatter plot", subtitle=paste("Manufacturing year(s):", list(input$num)), caption="Visualization by: filip-baumgartner.shinyapps.io/autobazar; Data source: www.AutoBazar.eu")
      }else if(input$oput == "Scatter" &  input$log_x == TRUE & input$log_y == TRUE){
        data5()%>%ggplot(., aes(x=log(get(input$scatter_x)), y=log(get(input$scatter_y))))+geom_point(na.rm = T)+stat_smooth(method = "lm", formula = y ~ x , size = 1)+
          scale_y_log10()+ scale_x_log10()+labs(x=input$scatter_x, y=input$scatter_y, main="Scatter plot", subtitle=paste("Manufacturing year(s):", list(input$num)), caption="Visualization by: filip-baumgartner.shinyapps.io/autobazar; Data source: www.AutoBazar.eu")
        
        }})
    

    
    data_tree_true <- eventReactive(input$go, {
      km_lower = input$km_tree - 30000
      km_upper = input$km_tree + 30000
      kw_lower = input$kw_tree - 20
      kw_upper = input$kw_tree + 20 
      
      top10_brands %>% filter(brand == input$bar2 & year_data == input$year_tree & diesel_data == input$fuel_factor & transmission_data == input$transmission_factor& 
                                 type_data == input$type_factor & quattro == input$quattro_factor & (kw_data > kw_lower & kw_data < kw_upper) & (km_data >km_lower & km_data <km_upper))%>%
        rename(Brand = brand, Model = model,Name = name_data, PRICE = price_data,Year = year_data, Region = region_data, Fuel = diesel_data , Transmission = transmission_data,
               Mileage_km = km_data ,Power_kW = kw_data, Body_Type = type_data, Quattro = quattro)
      
      })

    
    data_tree <- eventReactive(input$go, {data.frame(brand = input$bar2, year_data = input$year_tree, diesel_data= input$fuel_factor, transmission_data = input$transmission_factor, 
                                       km_data = input$km_tree, kw_data = input$kw_tree, type_data = input$type_factor, quattro = input$quattro_factor)}) 
   output$tree <- renderPrint({ print(round(predict(forest, data_tree()),0))})
   output$true <- renderTable(print(data_tree_true()))
   
   
    
    output$markdown <- renderUI({
      HTML(markdown::markdownToHTML(knit('markdown.Rmd', quiet = TRUE)))
    })
    
   
    
    output$modelmarkdown <- renderUI({
     HTML(markdown::markdownToHTML(knit('model_markdown.Rmd', quiet = TRUE)))
   })
    
    }

shinyApp(ui = ui, server = server)
