# load in packages
library(shiny)
library(shinydashboard)
library(shinyBS)
library(boastUtils)
library(triangle)
library(ggplot2)
library(stats)
library(Rlab)
library(shinyWidgets)
library(dplyr)

# generate the data for bimodal


#betaY <- rbeta(2,5)
#biData <- 

# Define top level objects ----
psuPalette <- c("#1E407C","#BC204B","#3EA39E","#E98300",
                "#999999","#AC8DCE","#F2665E","#99CC00")

source("popPicker.R")

# Define the UI----
ui <- list(
  dashboardPage(
    skin = "blue",
    dashboardHeader(
      title = "Population Picker",
      titleWidth = 250
    ),
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Picker", tabName = "picker", icon = icon("flask"))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "picker",
          withMathJax(),
          h1("Designing the Population Picker"),
          p("This app is for comparing the current form of the population picker
             with a new form that is",
            tags$ol(
              tags$li("an improvement in the coding (e.g., more efficient),"),
              tags$li("provides greater flexibility of cases (can work in apps
                      beyond Law of Large Numbers and Central Limit Theorem),"),
              tags$li("supports more productive meanings of ideas related to the
                      concept of distribution,"),
              tags$li("and, most importantly, is a module that can be called in
                      many different apps.")
            )),

          tags$hr(),
          h2("Proposed Modular Design"),
          # New UI ====
          popPickerUI(namespaceID = "popPicker"),
          tags$hr(),
          p("end of page")
        )
      )
    )
  )
)

# Define the Server----
server <- function(input, output, session){
  # New ----
  # Limit the Genre selection to no more than three, no few than 1
  
  popPickerServer(namespaceID = "popPicker")

  # # Old ----
  # # Function to create density plots for each group
  # # Inputs: Dataframe consisting of columns x and y to define axes, limits for x axis in form c(lower, upper), optional path for symmetric case
  # # Output: ggplot of density
  # makeDensityPlot <- function(data, xlims, path=0){
  #   plot <- ggplot2::ggplot(aes(x = x, y = y), data = data) +
  #     geom_path(color = "#0072B2", size = 1.5) +
  #     xlim(xlims) +
  #     xlab("Value") +
  #     ylab("Density") +
  #     ggtitle("Population Graph") +
  #     theme(axis.text = element_text(size = 18),
  #           plot.title = element_text(size = 18, face = "bold"),
  #           axis.title = element_text(size = 18),
  #           panel.background = element_rect(fill = "white", color = "black")
  #     )
  #   # For case in symmetric where path is 1 causing "box" shape
  #   if (path  == 1) {
  #     plot <- plot +
  #       geom_segment(aes(x=0, y=0, xend=0, yend=1), color="#0072B2", size=1.5)+
  #       geom_segment(aes(x=1, y=0, xend=1, yend=1), color="#0072B2", size=1.5)
  #   }
  #   plot
  # }
  # 
  # # Function to create bar plots for each group
  # # Inputs: x axis label (string), dataframe consisting of either column x or columns x and y to define axes
  # # Output: ggplot of resulting bar plot
  # 
  # 
  # ## Left skewed----
  # leftSkew<-reactive({11-10*input$leftskew})
  # 
  # # Population of left skewed
  # output$plotleft1 <- renderCachedPlot({
  #   # Define parameters for density plot
  #   x <- seq((leftSkew()) - 9 * sqrt((leftSkew())),0, length = input$symsize)
  #   y <- dgamma(-x, shape = (leftSkew()), beta = 1)
  #   data<-data.frame(x=x, y=y)
  # 
  #   # Make Density Plot
  #   makeDensityPlot(data=data, xlims = c((leftSkew()) - 9 * sqrt((leftSkew())), 0))
  # },
  # cacheKeyExpr = {
  #   list(input$leftskew)
  # })
  # 
  # 
  # ## Right skewed----
  # rightSkew<-reactive({11-10*input$rightskew})
  # # Population of right skewed
  # output$plotright1 <- renderCachedPlot({
  #   # Define parameters for density plot
  #   x <- seq(0, (rightSkew()) + 9 * sqrt(rightSkew()), length = input$symsize)
  #   y <- dgamma(x, shape = (rightSkew()), beta = 1)
  #   data<-data.frame(x=x, y=y)
  # 
  #   # Make the density plot
  #   makeDensityPlot(data=data, xlims = c(0, (rightSkew()) + 9 * sqrt((rightSkew()))))
  # },
  # cacheKeyExpr = {
  #   list(input$rightskew)
  # })
  # 
  # 
  # ## Symmetric skewed----
  # inverse<-reactive({round(14.6*input$inverse^3-5.7*input$inverse^2 + input$inverse+.1,3)})
  # # Population of Symmetric skewed
  # output$plotsymmetric1 <- renderCachedPlot({
  #   x <- seq(0, 1, length = input$symsize)
  #   dens <-
  #     dbeta(x,
  #           shape1 = inverse(),
  #           shape2 = inverse())
  #   data <- data.frame(x = x, y = dens)
  # 
  #   # Make density plot separated by case where the peakedness is exactly 1 (causes a "box" shape)
  #   makeDensityPlot(data = data, xlims = c(-0.03, 1.03), path=inverse())
  # },
  # cacheKeyExpr = {
  #   list(input$symsize, input$inverse)
  # })
  # 
  # 
  # ## Bimodal----
  # # Population for bimodel
  # prop<-reactive({input$prop/100})
  # output$plotbiomodel1 <- renderCachedPlot({
  #   # Define parameters for density plot
  #   t <- 1 / (input$bisize * input$bipath)
  #   y <- seq(0, 1, t)
  #   z <- seq(1, 0,-t)
  #   leftdraw <- dbeta(z, 4,14)*.2
  #   rightdraw <- dbeta(y, 4,14) *.2
  #   data<-data.frame(x = seq(0, 5, t*5), y = prop() * leftdraw + (1 - prop()) * rightdraw)
  # 
  #   # Make the density plot
  #   makeDensityPlot(data = data, xlims = c(0,5))
  # },
  # cacheKeyExpr = {
  #   list(input$prop)
  # })
  # 
  # 
  # ## Accident Rate----
  # 
  # # Population of Poisson
  # output$poissonpop <- renderCachedPlot({
  #   data<-data.frame(x=0:ceiling(2*input$poissonmean+5)) # More x's than necessary
  #   data$y<-(input$poissonmean^data$x) * exp(-input$poissonmean)/factorial(data$x) # Get y vals for x's
  #   data<-rbind(data[1:2,], filter(data[-c(1,2), ], y>.0005)) # Filter based on probability
  #   makeBarPlot(xlab= "Number of accidents", data= data)
  # },
  # cacheKeyExpr = {
  #   list(input$poissonmean)
  # })
  # 
  # 
  # ## Astrugulas
  # 
  # # Die results
  # die <- reactive({
  #   die <- c(rep(1, 1), rep(3, 4), rep(4, 4), rep(6, 1))
  # })
  # 
  # # Population of Astragalus
  # output$pop <- renderPlot({
  #   data<-data.frame(x=c(1,3,4,6), y=c(.1,.4,.4,.1))
  #   makeBarPlot(xlab= "Number on roll of astragalus", data= data, levels=1:6)
  # })
  # 
  # # Matrix of sample values
  # drawAdie <-
  #   reactive(matrix(
  #     sample(die(), input$aspath * input$assize,
  #            replace = TRUE),
  #     nrow = input$assize,
  #     ncol = input$aspath
  #   ))
  # 
  # ## iPOD SHUFFLE----
  # 
  # 
  # # Reactive expression to get the number of songs of the chosen type
  # nSongs<-reactive({
  #   if(input$ptype=="Jazz"){
  #     nSongs <- input$s1
  #   }
  #   else if(input$ptype=="Rock"){
  #     nSongs <- input$s2
  #   }
  #   else if(input$ptype=="Country"){
  #     nSongs <- input$s3
  #   }
  #   else{
  #     nSongs <- input$s4
  #   }
  # })
  # 
  # # Set up songs from four types
  # songs <- reactive({
  #   songs <- c(rep(input$s1),
  #              rep(input$s2),
  #              rep(input$s3),
  #              rep(input$s4))
  # })
  # 
  # # Bar plot
  # output$iPodBarPlot <- renderCachedPlot({
  #   # Parameters for bar plot
  #   p <- nSongs() / sum(songs())
  #   data<-data.frame(x = c("Other music (0)", paste(input$ptype,"(1)")), y=c(1-p, p))
  #   data$x<-factor(data$x, levels=data$x) # Done to force sorted order for bars
  # 
  #   # Make bar plot
  #   makeBarPlot(xlab= "Genre", data= data)
  # },
  # cacheKeyExpr = {
  #   list(input$s1, input$s2, input$s3, input$ptype, input$s4, input$ipodsize)
  # })
}

# App Call----
boastUtils::boastApp(ui = ui, server = server)
#shinyApp(ui = ui, server = server)
