library(shiny)
library(ggplot2)

data <- read.csv(file = "restaurant_score.csv", header = T)
data <- data[order(data$all_restaurant), ]

info <- read.csv(file = "restaurant_info.csv", header = T)
info <- info[order(info$all_restaurant), ]

atb <- read.csv('fastfood_attributes.csv')
atb <- atb[order(atb$name), ]

server <- function(input, output, session) {
  
  percentiles <- eventReactive(input$calculate_botton, {
    data[data$all_restaurant==input$restaurant,2:7]
  })
  
  num_reviews <- eventReactive(input$calculate_botton, {
    info$all_number[info$all_restaurant==input$restaurant]
  })
  
  avg_stars <- eventReactive(input$calculate_botton, {
    info$all_stars[info$all_restaurant==input$restaurant]
  })
  
  output$reviewNum <- renderText({
    paste0("The total number of reviews is: ", '<span style="color:red;">', num_reviews(), '</span>')
  })
  
  output$score <- renderText({
    paste0("The average score from reviews is: ", '<span style="color:red;">', round(avg_stars(), 2), '</span>')
  })
  
  output$scorePlot <- renderPlot({
    ggplot(info, aes(x=all_stars)) + 
      geom_histogram(binwidth=0.5, colour="black", fill="blue", alpha=.2, center=1)+
      geom_vline(xintercept=avg_stars(), linetype = "twodash", color="red",size = 1)+
      xlab("Score")+ 
      labs(title="Histogram of Average Score for all Fast Food Restaurant",
           subtitle="The red vertical dash line shows the position of this restaurant")+
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
            plot.subtitle = element_text(size = 12, face = "bold", hjust = 0.5),
            axis.text = element_text(size=12),
            axis.title = element_text(size=14, face="bold"))
  })
  
  output$mostUrgent <- renderText({
    p<-percentiles()[!is.na(percentiles())]
    nm<-names(percentiles())[!is.na(percentiles())]
    paste0("The most urgent aspect to improve is: ", '<span style="color:red;">', nm[p==min(p)], '</span>')
  })
  
  output$safety1 <- renderText({
    if (is.na(percentiles()[1])){
      paste0('<span style="color:orange;">', "There is no review about food safety in this restaurant.", '</span>')
    }else if (percentiles()[1]>0.5){
      paste0('<span style="color:green;">', "The food safety percentile among all fast food restaurant is ", round(percentiles()[1]*100, 2), "%.", '</span>')
    }else{
      paste0('<span style="color:red;">', "The food safety percentile among all fast food restaurant is ", round(percentiles()[1]*100, 2), "%.", '</span>')
    }
  })
  
  output$safety2 <- renderText({
    if (is.na(percentiles()[1]) | percentiles()[1]<0.5){
      "1. Be careful with grub, fly, hair during cooking.<br/>2. Don't provide undercooked food to customer."
    }else{
      "There is no big problem with food safety in this restaurant."
    }
  })
  
  output$taste1 <- renderText({
    if (is.na(percentiles()[2])){
      paste0('<span style="color:orange;">', "There is no review about food taste in this restaurant.", '</span>')
    }else if (percentiles()[2]>0.5){
      paste0('<span style="color:green;">', "The food taste percentile among all fast food restaurant is ", round(percentiles()[2]*100, 2), "%.", '</span>')
    }else{
      paste0('<span style="color:red;">', "The food taste percentile among all fast food restaurant is ", round(percentiles()[2]*100, 2), "%.", '</span>')
    }
  })
  
  output$taste2 <- renderText({
    if (is.na(percentiles()[2]) | percentiles()[2]<0.5){
      "1. Try to make the food delicious and juicy.<br/>2. Try to get authentication for signature food."
    }else{
      "There is no big problem with food taste in this restaurant."
    }
  })
  
  output$clean1 <- renderText({
    if (is.na(percentiles()[3])){
      paste0('<span style="color:orange;">', "There is no review about cleaness in this restaurant.", '</span>')
    }else if (percentiles()[3]>0.5){
      paste0('<span style="color:green;">', "The cleaness percentile among all fast food restaurant is ", round(percentiles()[3]*100, 2), "%.", '</span>')
    }else{
      paste0('<span style="color:red;">', "The cleaness percentile among all fast food restaurant is ", round(percentiles()[3]*100, 2), "%.", '</span>')
    }
  })
  
  output$clean2 <- renderText({
    if (is.na(percentiles()[3]) | percentiles()[3]<0.5){
      "1.Clean tables, walls and floor as soon as possible.<br/>2. Organize the items in the restaurant, don't make them in mess."
    }else{
      "There is no big problem with cleaness in this restaurant."
    }
  })
  
  output$service1 <- renderText({
    if (is.na(percentiles()[4])){
      paste0('<span style="color:orange;">', "There is no review about service in this restaurant.", '</span>')
    }else if (percentiles()[4]>0.5){
      paste0('<span style="color:green;">', "The service percentile among all fast food restaurant is ", round(percentiles()[4]*100, 2), "%.", '</span>')
    }else{
      paste0('<span style="color:red;">', "The service percentile among all fast food restaurant is ", round(percentiles()[4]*100, 2), "%.", '</span>')
    }
  })
  
  output$service2 <- renderText({
    if (is.na(percentiles()[4]) | percentiles()[4]<0.5){
      "1. The staffs should be more friendly, patient, helpful and polite.<br/>2. The manager should train and supervise staffs about how to serve customers."
    }else{
      "There is no big problem with service in this restaurant."
    }
  })
  
  output$speed1 <- renderText({
    if (is.na(percentiles()[5])){
      paste0('<span style="color:orange;">', "There is no review about speed in this restaurant.", '</span>')
    }else if (percentiles()[5]>0.5){
      paste0('<span style="color:green;">', "The speed percentile among all fast food restaurant is ", round(percentiles()[5]*100, 2), "%.", '</span>')
    }else{
      paste0('<span style="color:red;">', "The speed percentile among all fast food restaurant is ", round(percentiles()[5]*100, 2), "%.", '</span>')
    }
  })
  
  output$speed2 <- renderText({
    if (is.na(percentiles()[5]) | percentiles()[5]<0.5){
      "1. Improve the efficiency and proficiency of employees.<br/>2. Hire more people if the restaurant is too busy."
    }else{
      "There is no big problem with speed in this restaurant."
    }
  })
  
  output$price1 <- renderText({
    if (is.na(percentiles()[6])){
      paste0('<span style="color:orange;">', "There is no review about price in this restaurant.", '</span>')
    }else if (percentiles()[6]>0.5){
      paste0('<span style="color:green;">', "The price percentile among all fast food restaurant is ", round(percentiles()[6]*100, 2), "%.", '</span>')
    }else{
      paste0('<span style="color:red;">', "The price percentile among all fast food restaurant is ", round(percentiles()[6]*100, 2), "%.", '</span>')
    }
  })
  
  output$price2 <- renderText({
    if (is.na(percentiles()[6]) | percentiles()[6]<0.5){
      "1. Food may be overcharged. Reduce the price if possible<br/>2. The product may not be consistent with your discription."
    }else{
      "There is no big problem with price in this restaurant."
    }
  })
  
  output$branchNum <- renderText({
    res<-table(atb[,"name"])[input$restaurant]
    paste0("There are ", '<span style="color:orange;">', res, '</span>', " branch restaurants in total.")
  })
  
  output$BikeParking <- renderText({
    res<-table(atb[,c("name", "BikeParking")])[input$restaurant,]
    paste0(res["TRUE"], " branch restaurants have bike parking place, which has ", '<span style="color:green;">', "positive", '</span>', " effect on restaurant rate.")
  })
  
  output$Caters <- renderText({
    res<-table(atb[,c("name", "Caters")])[input$restaurant,]
    paste0(res["TRUE"], " branch restaurants can cater parties, which has ", '<span style="color:green;">', "positive", '</span>', " effect on restaurant rate.")
  })
  
  output$NoiseLevel <- renderText({
    res<-table(atb[,c("name", "NoiseLevel")])[input$restaurant,]
    paste0(res["loud"]+res["very"], " branch restaurants are noisy, which has ", '<span style="color:red;">', "negative", '</span>', " effect on restaurant rate.")
  })
  
  output$OutdoorSeating <- renderText({
    res<-table(atb[,c("name", "Caters")])[input$restaurant,]
    paste0(res["TRUE"], " branch restaurants have outdoor seatings, which has ", '<span style="color:green;">', "positive", '</span>', " effect on restaurant rate.")
  })
  
  output$RestaurantsDelivery <- renderText({
    res<-table(atb[,c("name", "RestaurantsDelivery")])[input$restaurant,]
    paste0(res["TRUE"], " branch restaurants can delivery food, which has ", '<span style="color:red;">', "negative", '</span>', " effect on restaurant rate.")
  })
  
  output$RestaurantsTableService <- renderText({
    res<-table(atb[,c("name", "RestaurantsTableService")])[input$restaurant,]
    paste0(res["TRUE"], " branch restaurants have table service, which has ", '<span style="color:green;">', "positive", '</span>', " effect on restaurant rate.")
  })
  
}
