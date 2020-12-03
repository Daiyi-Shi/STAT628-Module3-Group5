library(shiny)
library(ggplot2)

data <- read.csv(file = "restaurant_score.csv", header = T)
data <- data[order(data$all_restaurant), ]

info <- read.csv(file = "restaurant_info.csv", header = T)
info <- info[order(info$all_restaurant), ]

atb <- read.csv('fastfood_attributes.csv')
atb <- atb[order(atb$name), ]

address <- read.csv('fastfood_address.csv')
address$business_id <- as.character(address$business_id)
address$name <- as.character(address$name)
address$full_address <- as.character(address$full_address)
address$postal_code <- as.character(address$postal_code)

get_business_id <- function(restaurant, full_address){
  address_list = address[address$name == restaurant,]
  if(nrow(address_list) > 1){
    address_list = address_list[address_list$full_address == full_address,]
  }
  return(address_list[1,1])
}

server <- function(input, output, session) {
  
  observeEvent(input$restaurant, {
    address_list = address[address$name == input$restaurant,]
    address_list = address_list[order(address_list$postal_code),]
    updateSelectInput(session, inputId = 'address', choices = address_list$full_address)
  })
  
  business_id <- eventReactive(input$calculate_botton, {
    get_business_id(input$restaurant, input$address)
  })
  
  percentiles <- eventReactive(input$calculate_botton, {
    data[data$all_restaurant==business_id(),2:7]
  })
  
  num_reviews <- eventReactive(input$calculate_botton, {
    info$all_number[info$all_restaurant==business_id()]
  })
  
  avg_stars <- eventReactive(input$calculate_botton, {
    info$all_stars[info$all_restaurant==business_id()]
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
      paste0('<span style="color:orange;">', "There is no review about cleanliess in this restaurant.", '</span>')
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
      "1. Food may be overcharged. Reduce the price if possible<br/>2. The product may not be consistent with your description."
    }else{
      "There is no big problem with price in this restaurant."
    }
  })
  
  green_positive = '<span style="color:green;"> positive </span>'
  red_negative = '<span style="color:red;"> negative </span>'
  
  output$BikeParking <- renderText({
    res<-table(atb[,c("business_id", "BikeParking")])[business_id(),]
    condition = res["TRUE"]
    paste0("Your restaurant ",  if(condition) "have " else "does not have ", "bike parking place, which has ", if(condition) green_positive else red_negative, " effect on restaurant rate.")
  })
  
  output$Caters <- renderText({
<<<<<<< HEAD
    res<-table(atb[,c("business_id", "Caters")])[business_id(),]
=======
    business_id = get_business_id(input$restaurant, input$address)
    res<-table(atb[,c("name", "Caters")])[business_id,]
>>>>>>> 6f565b13142dcef2b0600d7efebd1010967f612a
    condition = res["TRUE"]
    paste0("Your restaurant ",  if(condition) "can " else "can not ", "cater parties, which has ", if(condition) green_positive else red_negative, " effect on restaurant rate.")
  })
  
  output$NoiseLevel <- renderText({
<<<<<<< HEAD
    res<-table(atb[,c("business_id", "NoiseLevel")])[business_id(),]
=======
    business_id = get_business_id(input$restaurant, input$address)
    res<-table(atb[,c("name", "NoiseLevel")])[business_id,]
>>>>>>> 6f565b13142dcef2b0600d7efebd1010967f612a
    condition = res["loud"]+res["very"]
    paste0("Your restaurant ", if(condition) "are noisy " else "are quiet ", "which has ", if(condition) red_negative else green_positive, " effect on restaurant rate.")
  })
  
  output$OutdoorSeating <- renderText({
<<<<<<< HEAD
    res<-table(atb[,c("business_id", "Caters")])[business_id(),]
=======
    business_id = get_business_id(input$restaurant, input$address)
    res<-table(atb[,c("name", "Caters")])[business_id,]
>>>>>>> 6f565b13142dcef2b0600d7efebd1010967f612a
    condition = res["TRUE"]
    paste0("Your restaurant ", if(condition) "have " else "does not have ", "outdoor seatings, which has ", if(condition) green_positive else red_negative, " effect on restaurant rate.")
  })
  
  output$RestaurantsDelivery <- renderText({
<<<<<<< HEAD
    res<-table(atb[,c("business_id", "RestaurantsDelivery")])[business_id(),]
=======
    business_id = get_business_id(input$restaurant, input$address)
    res<-table(atb[,c("name", "RestaurantsDelivery")])[business_id,]
>>>>>>> 6f565b13142dcef2b0600d7efebd1010967f612a
    condition = res["TRUE"]
    paste0("Your restaurant ", if(condition) "can " else "can not ", "delivery food, which has ", if(condition) red_negative else green_positive, " effect on restaurant rate.")
  })
  
  output$RestaurantsTableService <- renderText({
<<<<<<< HEAD
    res<-table(atb[,c("business_id", "RestaurantsTableService")])[business_id(),]
=======
    business_id = get_business_id(input$restaurant, input$address)
    res<-table(atb[,c("name", "RestaurantsTableService")])[business_id,]
>>>>>>> 6f565b13142dcef2b0600d7efebd1010967f612a
    condition = res["TRUE"]
    paste0("Your restaurant ", if(condition) "have " else "does not have ", "table service, which has ", if(condition) green_positive else red_negative, " effect on restaurant rate.")
  })
  
}
