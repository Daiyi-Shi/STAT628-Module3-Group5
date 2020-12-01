library(shiny)
library(ggplot2)

data = read.csv('fastfood_attributes.csv')
restaurants = sort(names(table(data$name)))

df = data.frame(table(data$star_y))
df$Var1 = as.numeric(as.character(df$Var1))





restaurants_list = list()
for(restaurant in restaurants){
  restaurants_list[restaurant] = restaurant
}

significant_attributes_list = list(
  c('BikeParking', 'True', '0.20'),
  c('Caters', 'True', '0.38'),
  c('NoiseLevel', 'Loud', '0.31'),
  c('OutdoorSeating', 'True', '0.25'),
  # c('RestaurantsDelivery', 'True', '5'),
  c('RestaurantsTableService', 'True', '0.4')
)

significant_attributes_outputs = list()

for(significant_attributes in significant_attributes_list){
  significant_attributes_outputs[[significant_attributes[1]]] =
    uiOutput(outputId = paste("attr_text_",significant_attributes[1], sep=""))
}

attributes_panel = do.call(
  tabPanel, list("Attributes", significant_attributes_outputs)
  # tabPanel, list("Attributes", textOutput(outputId = "attr_text_NoiseLevel"), textOutput(outputId = "attr_text_BikeParking") ) 
)

distribution_panel = tabPanel(
  "Plot",
  h3("Distribution Plot: "),
  plotOutput(outputId = "densityPlot"),
  br(),
  tags$style("#categoriesUrl2 {color:#808080; }"),
  uiOutput("categoriesUrl2"),
  tags$style("#dataUrl2 {color:#808080; }"),
  uiOutput("dataUrl2")
)

contact_panel = tabPanel(
  "Contact us",
  p(br(),
    "Thanks for using!",
    style="font-size:20px; color:#4169e1; "
  ),
  p(br(),
    "Contact information for author/maintainer (Email):",
    br(),
    "Hanlin Tang (htang79@wisc.edu)", 
    br(),
    "Zijun Feng (zfeng66@wisc.edu)",
    style="font-size:16px; color:#808080; ")
)

ui <- fluidPage(
  
  
  headerPanel('Yelp review analysis and suggestions'),
  sidebarPanel(id = "brand",style = "overflow-y:scroll; max-height: 800px; position:relative;",
               textInput("bussiness_id", label = h3("Bussiness id"), value = "Enter bussiness_id..."),
               actionButton("do", label= "find now!"),
               radioButtons("radio", label = h3("Brand"),
                            choices = restaurants_list, 
                            selected = 1)
  ),
  mainPanel(
    tabsetPanel(
      attributes_panel,
      distribution_panel,
      contact_panel
    )
  )
)

server <- function(input, output) {
  observeEvent(input$do, {
    mask = data$business_id == input$bussiness_id
    if(sum(mask) == 1){
      rate = data[mask, 'star_y']
      one_df = df
      one_df$legend = 'lower'
      one_df[one_df$Var1==rate,'legend'] = 'equal'
      one_df[one_df$Var1>rate,'legend'] = 'higher'
      one_df$legend = factor(one_df$legend, levels=c('lower', 'equal', 'higher'))
      
      output$densityPlot <- renderPlot({
        g <- ggplot(one_df, aes(x = Var1, y=Freq, fill=legend))+
          geom_bar(stat="identity", color="#88ada6", alpha=.25)+
          xlab("rate")+
          ylab("number")+
          labs(title="Yelp review rate distribution",
               subtitle="The blue bar is where your rate falls into.",
               caption = "Data source: See below")+
          scale_x_discrete(limits=c("lower", "equal", "higher"))+
          geom_text(aes(label=Freq), vjust=-0.3, size=3.5)+
          scale_fill_manual(values=c("Red", "Blue", "Green"))+
          theme_minimal()
        return(g)
      })
      
      lapply(significant_attributes_list, function(significant_attributes){
        key = paste("attr_text_",significant_attributes[1], sep="")
        if(significant_attributes[1] == 'NoiseLevel'){
          if(data[mask, significant_attributes[1]]%in%c("Loud", "very")){
            status=paste('<span style="color:red;">', data[mask, significant_attributes[1]], '</span>')
            suggest=paste("<p>On average, restaurant with this status is rated ", significant_attributes[3], ' less than those with <span style="color:green;"> average or quiet </span></p>')
          }else{
            suggest="Everying is Okay!"
            status=paste('<span style="color:green;">', data[mask, significant_attributes[1]], '</span>')
          }
        }else{
          if((!is.na(data[mask, significant_attributes[1]]) && data[mask, significant_attributes[1]] == significant_attributes[2])){
          # if(TRUE){
            suggest="Everying is Okay!"
            status=paste('<span style="color:green;">', data[mask, significant_attributes[1]], '</span>')
          }else{
            status=paste('<span style="color:red;">', data[mask, significant_attributes[1]], '</span>')
            suggest=paste("<p>On average, restaurant with this status is rated ", significant_attributes[3], ' less than those with <span style="color:green;">', significant_attributes[2], '</span></p>')
          }
        }
        text = paste(significant_attributes[1], ": Your Status is", status, ". ", suggest)
        # output[[key]] = p(br(),
        #   "Thanks for using this Body Fat Calculator!",
        #   style="font-size:20px; color:#4169e1; "
        # )
        output[[key]] = renderText(text)
      })
      
    }else{
      print("not found")
    }
  })
  # brand_name=input$brand
  
}

shinyApp(ui = ui, server = server)