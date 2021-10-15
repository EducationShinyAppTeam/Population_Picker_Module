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

# define the bimodal density function
biDens <- function(x, left){
  return(
    56 * (left * x * (1 - x)^6 + (1 - left) * x^6 * (1 - x))
  )
}

# generate the data for bimodal


#betaY <- rbeta(2,5)
#biData <- 

# Define top level objects ----
psuPalette <- c("#1E407C","#BC204B","#3EA39E","#E98300",
                "#999999","#AC8DCE","#F2665E","#99CC00")

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
          fluidRow( # Create the complete picker's row
            column( # Create main column split
              width = 4,
              wellPanel(
                selectInput(
                  inputId = "population",
                  label = "Population Type",
                  choices = list(
                    "Select a population" = "start",
                    "Continuous" = list(
                      "Skewed" = "skew",
                      "Symmetric" = "sym",
                      "Bimodal" = "bimodal",
                      "Triangular" = "tri",
                      "Cauchy" = "cauchy"
                    ),
                    "Discrete" = list(
                      "Accident rate" = "poisson",
                      "Astragalus (bone die)" = "astragalus",
                      "Playlist" = "playlist"
                    )
                  ),
                  selectize = T
                ),
                # Population Specific Inputs ####
                ## Skewness Slider ----
                conditionalPanel(
                  condition = "input.population == 'skew'",
                  sliderInput(
                    inputId = "skewness",
                    label = "Skewness",
                    min = -2,
                    max = 2,
                    step = 0.1,
                    value = 0,
                    ticks = TRUE
                  ),
                ),
                ## "Symmetric-Kurtosis" ----
                conditionalPanel(
                  condition = "input.population == 'sym'",
                  sliderInput(
                    inputId = "kurtosis",
                    label = "Excess Kurtosis",
                    min = -2,
                    max = 2,
                    step = 0.1,
                    value = 0,
                    ticks = TRUE
                  ),
                ),
                ## Bimodal ----
                conditionalPanel(
                  condition = "input.population == 'bimodal'",
                  sliderInput(
                    inputId = "leftMode",
                    label = "Percentage under left mode",
                    min = 10,
                    max = 90,
                    step = 1,
                    value = 50,
                    ticks = TRUE,
                    post = "%"
                  )
                ),
                ### Triangular ----
                conditionalPanel(
                  condition = "input.population == 'tri'",
                  sliderInput(
                    inputId = "lowerBound",
                    label = "Lower bound",
                    min = -5,
                    max = 5,
                    step = 0.5,
                    value = -5,
                    ticks = TRUE
                  ),
                  sliderInput(
                    inputId = "upperBound",
                    label = "Upper bound",
                    min = -5,
                    max = 5,
                    step = 0.5,
                    value = 5,
                    ticks = TRUE
                  ),
                  sliderInput(
                    inputId = "mode",
                    label = "Most probable value",
                    min = -5,
                    max = 5,
                    step = 0.5,
                    value = 0,
                    ticks = TRUE
                  ),
                  conditionalPanel(
                    condition = "input.upperBound <= input.lowerBound",
                    p(tags$em("Note: "), "Lower bound must be less than upper bound.")
                  ),
                  conditionalPanel(
                    condition = "input.mode > input.upperBound",
                    p(tags$em("Note: "), "Most probable value must be between bounds.")
                  ),
                  conditionalPanel(
                   condition = "input.mode < input.lowerBound",
                   p(tags$em("Note: "), "Most probable value must be between bounds.")
                  ),
                ),
                ## Cauchy ----
                conditionalPanel(
                  condition = "input.population == 'cauchy'",
                  sliderInput(
                    inputId = "medianMode",
                    label = "Distribution Median and Mode",
                    min = -2,
                    max = 2,
                    step = 0.5,
                    value = 0,
                    ticks = TRUE
                  ),
                  sliderInput(
                    inputId = "halfWidth",
                    label = "Half of the IQR",
                    min = 0.1,
                    max = 4,
                    step = 0.1,
                    value = 1,
                    ticks = TRUE
                  )
                ),
                ## Poisson ----
                conditionalPanel(
                  condition = "input.population == 'poisson'",
                  sliderInput(
                    inputId = "unitRate",
                    label = "Unit rate (mean)",
                    min = 0,
                    max = 10,
                    step = 0.1,
                    value = 1,
                    ticks = TRUE
                  ),
                  conditionalPanel(
                    condition = "input.unitRate == 0",
                    p(tags$em("Note: "), "when the Unit Rate (Mean) is 0, the Variance is also
                      0, resulting in all cases being the same.")
                  )
                ),
                
                # Playlist ----
                conditionalPanel(
                  condition = "input.population == 'playlist'",
                  p("Enter the number of songs in each genre and which genre you
                    want to track."),
                  fluidRow(
                    column(
                      width = 6,
                      numericInput(
                        inputId = "jazzN",
                        label = "Jazz",
                        value = 1,
                        min = 0,
                        max = NA,
                        step = 1
                      ),
                      numericInput(
                        inputId = "rockN",
                        label = "Rock",
                        value = 1,
                        min = 0,
                        max = NA,
                        step = 1
                      ),
                      numericInput(
                        inputId = "countryN",
                        label = "Country",
                        value = 1,
                        min = 0,
                        max = NA,
                        step = 1
                      ),
                      numericInput(
                        inputId = "hipHopN",
                        label = "Hip-hop",
                        value = 1,
                        min = 0,
                        max = NA,
                        step = 1
                      )
                    ),
                    column(
                      width = 6,
                      radioButtons(
                        inputId = "pickGenre",
                        label = "Genre(s) to track:",
                        choices = list(
                          "Jazz",
                          "Rock",
                          "Country",
                          "Hip-hop"
                        ),
                        selected = "Jazz"
                      )
                    )
                  )
                ),
                fluidRow(
                  box(
                    title = strong("Key terms/instructions"),
                    status = "primary",
                    width = 12,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    tags$ul(tags$b("Instructions"), 
                            tags$li("Click on the dropdown menu
                            to select the population distribution that you would
                            like to work with")), 
                    tags$ul(tags$b("Terms"),
                            tags$li("Kurtosis - The measure of skewness relative 
                                    to the standard normal distribution"),
                            tags$li())
                            
                    
                  )
                )
                
              )
            ),
            column( # Create plot column ----
              width = 8,
              plotOutput("popPlot")
              )
          ),

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
  observe({
    if (length(input$pickGenre) > 3) {
      updateCheckboxGroupInput(
        session = session,
        inputId = "pickGenre",
        selected = tail(input$pickGenre, 3)
      )}
    if (length(input$pickGenre) < 1 ) {
      updateCheckboxGroupInput(
        session = session,
        inputId = "pickGenre",
        selected = "Jazz"
      )}
  })

  # Create the reactive parameters ----
  gammaShape <- reactive({
    ifelse(input$skewness != 0, 4/(input$skewness)^2, 0)
  })
  gammaScale <- reactive({1/sqrt(abs(gammaShape())) })
  gammaMax <- reactive({
    ifelse(input$skewness != 0, max(qgamma(0.999, shape = gammaShape(),
                                      scale = gammaScale()) + 2, 10), 0)
  })
  kurtTheta <- reactive({
    if (input$kurtosis < 0) {
      -3 * (input$kurtosis + 2) / (2 * input$kurtosis)
    } else if (input$kurtosis > 0) {
      6 / input$kurtosis + 4
    } else  {0}
  })
  

  # Reconstruct the plot using the following logic
  ## Step 1a create a data frame with all density columns OR
  ## Step 1b create a routine to create custom data frame that is updated
  ##         or replaced for each run
  ## Step 2 create the two graph commands: 1 for continuous, 1 for discrete
  ## Step 3 add any additional customizations.


  # Create the population plot ----
  output$popPlot <- renderPlot({
    plot <- ggplot2::ggplot(
      data = data.frame(x = seq(from = -5, to = 5, by = 1)),
    mapping = aes(x = x)) +
      ggplot2::theme_bw() +
      xlab("Value") +
      ylab("Density") +
      ggplot2::ggtitle("Population Graph") +
      ggplot2::theme(axis.text = element_text(size = 18),
                     plot.title = element_text(size = 18),
                     axis.title = element_text(size = 18)) +
      ggplot2::scale_x_continuous(expand = expansion(mult = 0, add = 1)) +
      ggplot2::scale_y_continuous(expand = expansion(mult = c(0.01, 0.1), add = 0))

    ## Distribution Specific plots ----
    ###
    if (input$population == "start") {
      plot <- plot + ggplot2::annotate(geom = "text",
                                       x = 0,
                                       y =  0.2,
                        label = "Select a population to explore",
                        color = boastPalette[5],
                        size = 10) +
        ggplot2::geom_hline(yintercept = 0.19, color = "white") +
        ggplot2::theme_void() +
        ggplot2::ggtitle(label = NULL)
    }
    if (input$population == "skew") {
      if (input$skewness > 0) {
        plot <- plot + ggplot2::stat_function(
          data = data.frame(x = seq(from = 0, to = gammaMax(), by = 1)),
          fun = dgamma, args = list(shape = gammaShape(), scale = gammaScale()),
          color = psuPalette[1], size = 1.5)
      } else if (input$skewness < 0) {
        plot <- plot + ggplot2::stat_function(
          data = data.frame(x = seq(from = -1*gammaMax(), to = 0, by = 1)),
          fun = function(x){dgamma(-x, shape = gammaShape(), scale = gammaScale())},
          color = psuPalette[1], size = 1.5)
      } else {
        plot <- plot + ggplot2::stat_function(fun = dnorm,
                                              args = list(mean = 0,
                                                          sd = 1),
                                              color = psuPalette[1],
                                              size = 1.5)
      }
    } else if (input$population == "sym") {
      if (input$kurtosis < 0) {
        plot <- plot + ggplot2::stat_function(
          data = data.frame(x = seq(from = -10, to = 10, by = 1)),
          fun = function(x){dbeta(x = x/20 + 0.5, shape1 = kurtTheta(),
                                  shape2 = kurtTheta())},
          color = psuPalette[1], size = 1.5)
      } else if (input$kurtosis > 0) {
        plot <- plot + ggplot2::stat_function(
          fun = function(x){dt(x = x, df = kurtTheta())},
          color = psuPalette[1], size = 1.5) 
      } else {
        plot <- plot + ggplot2::stat_function(
          data = data.frame(x = seq(from = -10, to = 10, by = 1)),
          fun = dnorm, args = list(mean = 0, sd = 1),
          color = psuPalette[1], size = 1.5)
      }
    } else if (input$population == "bimodal") {
      plot <- plot + stat_function(
        data = data.frame(x = seq(from = 0, to = 1, by = 0.1)),
        fun = biDens,
        args = list(left = input$leftMode/100),
        color = psuPalette[1],
        size = 1.5
      )

    } else if (input$population == "tri") {
      plot <- plot + ggplot2::stat_function(
        fun = triangle::dtriangle, args = list(a = input$lowerBound,
                                               b = input$upperBound,
                                               c = input$mode),
        color = psuPalette[1], size = 1.5)
    } else if (input$population == "cauchy") {
      plot <- plot + ggplot2::stat_function(
        fun = dcauchy, args = list(location = input$medianMode,
                                   scale = input$halfWidth),
        color = psuPalette[1], size = 1.5)
    } else if (input$population == "astragalus") {
      # Population of Astragalus
        
        data<-data.frame(x=c(1,3,4,6), y=c(.1,.4,.4,.1))
        plot <- makeBarPlot(xlab= "Number on roll of astragalus", data= data, levels=1:6)
      
      
      
      # Matrix of sample values for the astragalus population graph 
      drawAdie <-
        reactive(matrix(
          sample(die(), input$aspath * input$assize,
                 replace = TRUE),
          nrow = input$assize,
          ncol = input$aspath
        ))
    } else if(input$population == "playlist") {
      nSongs<-reactive({
        if(input$pickGenre=="Jazz"){
          nSongs <- input$jazzN
        }
        else if(input$pickGenre=="Rock"){
          nSongs <- input$rockN
        }
        else if(input$pickGenre=="Country"){
          nSongs <- input$countryN
        }
        else{
          nSongs <- input$hipHopN
        }
      })
      
      # Set up songs from four types
      songs <- reactive({
        songs <- c(rep(input$jazzN),
                   rep(input$rockN),
                   rep(input$countryN),
                   rep(input$hipHopN))
      })
      
      # Bar plot
        # Parameters for bar plot
        p <- nSongs() / sum(songs())
        data<-data.frame(x = c("Other music (0)", paste(input$pickGenre,"(1)")), y=c(1-p, p))
        data$x<-factor(data$x, levels=data$x) # Done to force sorted order for bars
        
        # Make bar plot
        plot<- makeBarPlot(xlab= "Genre", data= data)
      
      cacheKeyExpr = {
       list(input$jazzN, input$rockN, input$countryN, input$pickGenre, input$hipHopN)
      }
    } else if (input$population == "poisson") {
      
        data<-data.frame(x=0:ceiling(2*input$unitRate+5)) # More x's than necessary
        data$y<-(input$unitRate^data$x) * exp(-input$unitRate)/factorial(data$x) # Get y vals for x's
        data<-rbind(data[1:2,], filter(data[-c(1,2), ], y>.0005)) # Filter based on probability
        plot <- makeBarPlot(xlab= "Number of accidents", data= data)
    }
      
    # else {
    #   plot <- plot + ggplot2::stat_function(fun = dnorm,
    #                                         color = psuPalette[1],
    #                                         size = 1.5)
    # }

    return(plot)
  })


  # Old ----
  # Function to create density plots for each group
  # Inputs: Dataframe consisting of columns x and y to define axes, limits for x axis in form c(lower, upper), optional path for symmetric case
  # Output: ggplot of density
  makeDensityPlot <- function(data, xlims, path=0){
    plot <- ggplot2::ggplot(aes(x = x, y = y), data = data) +
      geom_path(color = "#0072B2", size = 1.5) +
      xlim(xlims) +
      xlab("Value") +
      ylab("Density") +
      ggtitle("Population Graph") +
      theme(axis.text = element_text(size = 18),
            plot.title = element_text(size = 18, face = "bold"),
            axis.title = element_text(size = 18),
            panel.background = element_rect(fill = "white", color = "black")
      )
    # For case in symmetric where path is 1 causing "box" shape
    if (path  == 1) {
      plot <- plot +
        geom_segment(aes(x=0, y=0, xend=0, yend=1), color="#0072B2", size=1.5)+
        geom_segment(aes(x=1, y=0, xend=1, yend=1), color="#0072B2", size=1.5)
    }
    plot
  }

  # Function to create bar plots for each group
  # Inputs: x axis label (string), dataframe consisting of either column x or columns x and y to define axes
  # Output: ggplot of resulting bar plot
  makeBarPlot<-function(xlab, data, levels=as.character(data$x)){
    plot<-ggplot(aes(x=factor(x, levels=levels), y=y), data= data) +
      geom_bar(stat = "identity", fill="#0072B2") +
      ylim(c(0, max(data$y)+.1*max(data$y)))+
      xlab(xlab) +
      ylab("Probability") +
      ggtitle("Population Graph") +
      theme(axis.text = element_text(size=18),
            plot.title = element_text(size=18, face="bold"),
            axis.title = element_text(size=18),
            panel.background = element_rect(fill = "white", color="black"))+
      scale_x_discrete(drop=FALSE)

    plot
  }

  ## Left skewed----
  leftSkew<-reactive({11-10*input$leftskew})

  # Population of left skewed
  output$plotleft1 <- renderCachedPlot({
    # Define parameters for density plot
    x <- seq((leftSkew()) - 9 * sqrt((leftSkew())),0, length = input$symsize)
    y <- dgamma(-x, shape = (leftSkew()), beta = 1)
    data<-data.frame(x=x, y=y)

    # Make Density Plot
    makeDensityPlot(data=data, xlims = c((leftSkew()) - 9 * sqrt((leftSkew())), 0))
  },
  cacheKeyExpr = {
    list(input$leftskew)
  })


  ## Right skewed----
  rightSkew<-reactive({11-10*input$rightskew})
  # Population of right skewed
  output$plotright1 <- renderCachedPlot({
    # Define parameters for density plot
    x <- seq(0, (rightSkew()) + 9 * sqrt(rightSkew()), length = input$symsize)
    y <- dgamma(x, shape = (rightSkew()), beta = 1)
    data<-data.frame(x=x, y=y)

    # Make the density plot
    makeDensityPlot(data=data, xlims = c(0, (rightSkew()) + 9 * sqrt((rightSkew()))))
  },
  cacheKeyExpr = {
    list(input$rightskew)
  })


  ## Symmetric skewed----
  inverse<-reactive({round(14.6*input$inverse^3-5.7*input$inverse^2 + input$inverse+.1,3)})
  # Population of Symmetric skewed
  output$plotsymmetric1 <- renderCachedPlot({
    x <- seq(0, 1, length = input$symsize)
    dens <-
      dbeta(x,
            shape1 = inverse(),
            shape2 = inverse())
    data <- data.frame(x = x, y = dens)

    # Make density plot separated by case where the peakedness is exactly 1 (causes a "box" shape)
    makeDensityPlot(data = data, xlims = c(-0.03, 1.03), path=inverse())
  },
  cacheKeyExpr = {
    list(input$symsize, input$inverse)
  })


  ## Bimodal----
  # Population for bimodel
  prop<-reactive({input$prop/100})
  output$plotbiomodel1 <- renderCachedPlot({
    # Define parameters for density plot
    t <- 1 / (input$bisize * input$bipath)
    y <- seq(0, 1, t)
    z <- seq(1, 0,-t)
    leftdraw <- dbeta(z, 4,14)*.2
    rightdraw <- dbeta(y, 4,14) *.2
    data<-data.frame(x = seq(0, 5, t*5), y = prop() * leftdraw + (1 - prop()) * rightdraw)

    # Make the density plot
    makeDensityPlot(data = data, xlims = c(0,5))
  },
  cacheKeyExpr = {
    list(input$prop)
  })


  ## Accident Rate----

  # Population of Poisson
  output$poissonpop <- renderCachedPlot({
    data<-data.frame(x=0:ceiling(2*input$poissonmean+5)) # More x's than necessary
    data$y<-(input$poissonmean^data$x) * exp(-input$poissonmean)/factorial(data$x) # Get y vals for x's
    data<-rbind(data[1:2,], filter(data[-c(1,2), ], y>.0005)) # Filter based on probability
    makeBarPlot(xlab= "Number of accidents", data= data)
  },
  cacheKeyExpr = {
    list(input$poissonmean)
  })


  ## Astrugulas

  # Die results
  die <- reactive({
    die <- c(rep(1, 1), rep(3, 4), rep(4, 4), rep(6, 1))
  })

  # Population of Astragalus
  output$pop <- renderPlot({
    data<-data.frame(x=c(1,3,4,6), y=c(.1,.4,.4,.1))
    makeBarPlot(xlab= "Number on roll of astragalus", data= data, levels=1:6)
  })

  # Matrix of sample values
  drawAdie <-
    reactive(matrix(
      sample(die(), input$aspath * input$assize,
             replace = TRUE),
      nrow = input$assize,
      ncol = input$aspath
    ))

  ## iPOD SHUFFLE----


  # Reactive expression to get the number of songs of the chosen type
  nSongs<-reactive({
    if(input$ptype=="Jazz"){
      nSongs <- input$s1
    }
    else if(input$ptype=="Rock"){
      nSongs <- input$s2
    }
    else if(input$ptype=="Country"){
      nSongs <- input$s3
    }
    else{
      nSongs <- input$s4
    }
  })

  # Set up songs from four types
  songs <- reactive({
    songs <- c(rep(input$s1),
               rep(input$s2),
               rep(input$s3),
               rep(input$s4))
  })

  # Bar plot
  output$iPodBarPlot <- renderCachedPlot({
    # Parameters for bar plot
    p <- nSongs() / sum(songs())
    data<-data.frame(x = c("Other music (0)", paste(input$ptype,"(1)")), y=c(1-p, p))
    data$x<-factor(data$x, levels=data$x) # Done to force sorted order for bars

    # Make bar plot
    makeBarPlot(xlab= "Genre", data= data)
  },
  cacheKeyExpr = {
    list(input$s1, input$s2, input$s3, input$ptype, input$s4, input$ipodsize)
  })
}

# App Call----
boastUtils::boastApp(ui = ui, server = server)
#shinyApp(ui = ui, server = server)
