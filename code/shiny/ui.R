library(shiny)
library(ggplot2)

data <- read.csv(file = "restaurant_score.csv", header = T)
data <- data[order(data$all_restaurant), ]

info <- read.csv(file = "restaurant_info.csv", header = T)
info <- info[order(info$all_restaurant), ]

ui <- fluidPage(
  titlePanel(h1(id = "title","Advises for Fast Food Restaurant",align = "center")),
  tags$style(HTML("#title{font-size: 50px;font-family: Georgia;}")),
  hr(),
  fluidRow(column(4, h2("Input")), column(4, h2("Output"))),
  sidebarLayout(
    sidebarPanel(
      p("Please choose the fast food restaurant name and then click the Button:"),
      
      helpText("Our advises are available for all stores with the same name"),

      selectInput("restaurant",label="Unit",
                  choices=data$all_restaurant, selected="Arby's", multiple=F),
      
      actionButton("calculate_botton", "Button", style = "color: white; background-color: #4040ff" ),
      width=4
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Info",
          h3(htmlOutput(outputId = "reviewNum")),
          h3(htmlOutput(outputId = "score")),
          br(),
          plotOutput(outputId = "scorePlot")
        ),
        
        tabPanel(
          "Advises from Reviews",
          helpText("We find out a method to evaluate the percentile of a restaurant from 6 aspects. 
                    If the percentile in a aspect is less than 50%, we think the restaurant should improve it. 
                    More details are in our summary pdf."),
          tags$style("#mostUrgent {font-size:19px; font-weight: bold;}"),
          htmlOutput(outputId = "mostUrgent"),
          tags$style("#safety1 {font-size:18px; font-weight: bold;}"),
          htmlOutput(outputId = "safety1"),
          htmlOutput(outputId = "safety2"),
          tags$style("#taste1 {font-size:18px; font-weight: bold;}"),
          htmlOutput(outputId = "taste1"),
          htmlOutput(outputId = "taste2"),
          tags$style("#clean1 {font-size:18px; font-weight: bold;}"),
          htmlOutput(outputId = "clean1"),
          htmlOutput(outputId = "clean2"),
          tags$style("#service1 {font-size:18px; font-weight: bold;}"),
          htmlOutput(outputId = "service1"),
          htmlOutput(outputId = "service2"),
          tags$style("#speed1 {font-size:18px; font-weight: bold;}"),
          htmlOutput(outputId = "speed1"),
          htmlOutput(outputId = "speed2"),
          tags$style("#price1 {font-size:18px; font-weight: bold;}"),
          htmlOutput(outputId = "price1"),
          htmlOutput(outputId = "price2")
        ),
        
        tabPanel(
          "Advises from Attributes",
          helpText("Since we treat all restaurants with the same name as a whole,
                   but the attributes for each brach restaurant may be different, 
                   we can't get uniform attributes. 
                   Therefore, we will give some general here.
                   By performing linear regression (using branch restaurant as unit), 
                   we found out several significant attributes:"), 
          tags$style("#branchNum {font-size:17px; font-weight: bold;}"),
          htmlOutput(outputId = "branchNum"),
          tags$style("#BikeParking {font-size:16px; font-weight: bold;}"),
          htmlOutput(outputId = "BikeParking"),
          tags$style("#Caters {font-size:16px; font-weight: bold;}"),
          htmlOutput(outputId = "Caters"),
          tags$style("#NoiseLevel {font-size:16px; font-weight: bold;}"),
          htmlOutput(outputId = "NoiseLevel"),
          tags$style("#OutdoorSeating {font-size:16px; font-weight: bold;}"),
          htmlOutput(outputId = "OutdoorSeating"),
          tags$style("#RestaurantsDelivery {font-size:16px; font-weight: bold;}"),
          htmlOutput(outputId = "RestaurantsDelivery"),
          tags$style("#RestaurantsTableService {font-size:16px; font-weight: bold;}"),
          htmlOutput(outputId = "RestaurantsTableService")
        ),
        
        tabPanel(
          "Contact us",
          p(br(),
            "Thanks for using it!",
            style="font-size:20px; color:#4169e1; "
          ),
          p(br(),
            "Contact information for author/maintainer (Email):",
            br(),
            "Zijun Feng (zfeng66@wisc.edu)",
            br(),
            "Hanlin Tang (htang79@wisc.edu)",
            style="font-size:16px; color:#808080; ")
        )
      )
    )
  )
)
