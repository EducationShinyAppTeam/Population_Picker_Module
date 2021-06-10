library(shiny)
library(shinydashboard)
library(shinyBS)
library(boastUtils)

library(ggplot2)
library(stats)
library(Rlab)
library(shinyWidgets)
library(dplyr)

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
        id = "tabs",
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
            (thanks Leah!) with a new form that is",
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
          h2("Current, Non-modular design"),
          # Original ====
          # Layout for the population picker
          sidebarLayout(
            sidebarPanel(
              width=6,
              fluidRow(
                column(
                  6,
                  # Select Input for the distribution type
                  selectInput(
                    inputId = "popDist",
                    label = "Population type",
                    list(
                      "Left-skewed" = "leftskewed",
                      "Right-skewed" = "rightskewed",
                      "Symmetric" = "symmetric",
                      "Bimodal" = "bimodal",
                      "Astragalus (Bone Die)" =
                        "astragalus",
                      "Playlist" =
                        "ipodshuffle",
                      "Accident Rate" = "poisson"
                    )
                  ),

                  # Conditional Panel for type of population distribution

                  # Left Skewed
                  conditionalPanel(
                    condition = "input.popDist=='leftskewed'",
                    sliderInput(
                      "leftskew",
                      " Skewness",
                      min = 0,
                      max = 1,
                      value = .5,
                      step = 0.1,
                      ticks=FALSE
                    ),
                    div(style = "position: absolute; left: 0.5em; top: 9em", "min"),
                    div(style = "position: absolute; right: 0.5em; top: 9em", "max"),
                  )
                  ,

                  # Right Skewed
                  conditionalPanel(
                    condition = "input.popDist=='rightskewed'",
                    sliderInput(
                      "rightskew",
                      "Skewness",
                      min = 0,
                      max = 1,
                      value = .5,
                      step = .01,
                      ticks=FALSE
                    ),
                    div(style = "position: absolute; left: 0.5em; top: 9em", "min"),
                    div(style = "position: absolute; right: 0.5em; top: 9em", "max"),
                  ),

                  #Symmetric
                  conditionalPanel(
                    condition = "input.popDist=='symmetric'",
                    sliderInput(
                      "inverse",
                      "Peakedness",
                      min = 0,
                      max = 1,
                      value = .5,
                      step = 0.01,
                      ticks=FALSE
                    ),
                    div(style = "position: absolute; left: 0.5em; top: 9em", "U"),
                    div(style = "position: absolute; left: 0.5em; top: 10em", "Shaped"),
                    div(style = "position: absolute; right: 0.5em; top: 9em", "Bell"),
                    div(style = "position: absolute; right: 0.5em; top: 10em", "Shaped"),
                  ),

                  # Bimodal
                  conditionalPanel(
                    condition = "input.popDist=='bimodal'",

                    sliderInput(
                      "prop",
                      "% under right mode",
                      min = 10,
                      max = 90,
                      value = 50,
                      ticks=F,
                      post="%",
                      #grid_num=9
                      #interval = 1
                    )
                  ),

                  # Poisson
                  conditionalPanel(
                    condition = "input.popDist=='poisson'",

                    sliderInput(
                      "poissonmean",
                      "Mean",
                      min = 0,
                      max = 10,
                      value = 1,
                      step = 0.1
                    ),
                    conditionalPanel(
                      condition="input.poissonmean==0",
                      "Note: When the mean is set to 0, the number of accidents is always 0, so the variance is 0."
                    )
                  ),

                  #iPod shuffle
                  conditionalPanel(
                    condition = "input.popDist == 'ipodshuffle'",
                    column(
                      width = 7,
                      offset = 0,

                      p("Number of songs:"),
                      # Inputs for the probabilites of each music type
                      numericInput(
                        "s1",
                        "Jazz",
                        1,
                        min = 0,
                        max = 200,
                        step = 1,
                        width="75px"
                      ),
                      numericInput(
                        "s2",
                        "Rock",
                        1,
                        min = 0,
                        max = 200,
                        step = 1,
                        width="75px"
                      ),
                      numericInput(
                        "s3",
                        "Country",
                        1,
                        min = 0,
                        max = 200,
                        step = 1,
                        width="75px"
                      ),
                      numericInput(
                        "s4",
                        "Hip-hop",
                        1,
                        min = 0,
                        max = 200,
                        step = 1,
                        width="75px"
                      )
                    ),

                  ) #This parenthesis ends the iPod Shuffle Conditional Panel

                ), #Ends inputs column

                # Inputs for each type:

                column(
                  6,

                  #left skewed
                  conditionalPanel(
                    condition = "input.popDist == 'leftskewed'",
                    # Choose number of paths
                    sliderInput(
                      "leftpath",
                      "Number of paths",
                      min = 1,
                      max = 5,
                      value = 1
                    ),
                    # Choose sample size
                    sliderInput(
                      "leftsize",
                      "Sample size (n)",
                      min = 10,
                      max = 1000,
                      value = 100
                    )

                  ),

                  # Right skewed
                  conditionalPanel(
                    condition = "input.popDist == 'rightskewed'",
                    # Choose the number of sample means
                    sliderInput(
                      "rightpath",
                      "Number of paths",
                      min = 1,
                      max = 5,
                      value = 1
                    ),
                    # Choose the number of sample means
                    sliderInput(
                      "rightsize",
                      "Sample size (n)",
                      min = 10,
                      max = 1000,
                      value = 100
                    )
                  ),

                  # Symmetric
                  conditionalPanel(
                    condition = "input.popDist == 'symmetric'",
                    # Choose the number of paths
                    sliderInput(
                      "sympath",
                      "Number of paths",
                      min = 1,
                      max = 5,
                      value = 1
                    ),
                    # Choose the number of sample means
                    sliderInput(
                      "symsize",
                      "Sample size (n)",
                      min = 10,
                      max = 1000,
                      value = 100
                    )
                  ),
                  # Astragulus
                  conditionalPanel(
                    condition = "input.popDist == 'astragalus'",
                    # Choose number of paths
                    sliderInput(
                      "aspath",
                      'Number of paths',
                      min = 1,
                      max = 5,
                      value = 1
                    ),
                    # Choose sample size
                    sliderInput(
                      "assize",
                      "Number of trials",
                      min = 10,
                      max = 1000,
                      value = 100
                    )
                  ),

                  # Bimodal
                  conditionalPanel(
                    condition = "input.popDist == 'bimodal'",
                    # Choose the number of paths
                    sliderInput(
                      "bipath",
                      "Number of paths",
                      min = 1,
                      max = 5,
                      value = 1
                    ),
                    # Choose the number of sample means
                    sliderInput(
                      "bisize",
                      "Sample size (n)",
                      min = 10,
                      max = 1000,
                      value = 100
                    )
                  ),

                  # Poisson
                  conditionalPanel(
                    condition = "input.popDist == 'poisson'",
                    # Choose the number of paths
                    sliderInput(
                      "poissonpath",
                      "Number of paths",
                      min = 1,
                      max = 5,
                      value = 1
                    ),
                    # Choose the number of sample means
                    sliderInput(
                      "poissonsize",
                      "Sample size (n)",
                      min = 10,
                      max = 1000,
                      value = 100
                    )
                  ),

                  # Playlist
                  conditionalPanel(
                    condition = "input.popDist == 'ipodshuffle'",
                    # Choose number of paths
                    sliderInput(
                      "ipodpath",
                      label = "Number of paths",
                      min = 1,
                      max = 5,
                      value = 1
                    ),
                    # Choose sample size
                    sliderInput(
                      "ipodsize",
                      label = "Sample size (n)",
                      min = 10,
                      max = 1000,
                      value = 100
                    ),
                    # Buttons to choose music type
                    radioButtons(
                      "ptype",
                      "Genre to track:",
                      list("Jazz",
                           "Rock",
                           "Country",
                           "Hip-hop"),
                      selected = "Jazz"
                    )
                  )
                )
              )
            ), #End of column for slider inputs

            mainPanel(
              width = 6,
              # Plots for each distribution; either histogram or density
              conditionalPanel(condition = "input.popDist == 'leftskewed'",
                               plotOutput('plotleft1')),
              conditionalPanel(condition = "input.popDist == 'rightskewed'",
                               plotOutput('plotright1')),
              conditionalPanel(condition = "input.popDist == 'symmetric'",
                               plotOutput('plotsymmetric1')),
              conditionalPanel(condition = "input.popDist == 'astragalus'",
                               plotOutput("pop")),
              conditionalPanel(condition = "input.popDist == 'bimodal'",
                               plotOutput('plotbiomodel1')),
              conditionalPanel(condition = "input.popDist == 'poisson'",
                               plotOutput('poissonpop')),
              conditionalPanel(condition = "input.popDist == 'ipodshuffle'",
                               plotOutput("iPodBarPlot")
              )
            )
          ),
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
                      "Fair die" = "disEquip",
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
                    inputId = "rightMode",
                    label = "Percentage under right mode",
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
                  )
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
                ## Fair die ----
                conditionalPanel(
                  condition = "input.population == 'disEquip'",
                  selectInput(
                    inputId = "numSides",
                    label = "Number of sides",
                    choices = c(4, 6, 8, 10, 12, 20, 48, 120)
                    # Remember to convert input$numSides to number
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
                      checkboxGroupInput(
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
                )
              )
            ),
            column( # Create plot column ----
              width = 8,
              plotOutput("popPlot")
              )
          ),
          ## Removed Controls ----
          tags$hr(style="border-top: dotted 4px;"),
          p("The number of paths (or number of repetitions) and the sample size
            sliders aren't actually essential to the Population Picker. Rather,
            they both reflect aspects of outputs beyond the population graph
            that we might want. Namely, simulated data. As such I have opted to
            remove them from the Population Picker's controls. However, I have
            sought to build the module in such a way that should a person want
            to manipulate both of these aspects beyond the default (i.e., 5
            paths/replicates and a sample size of 100), they can by adding inputs
            to their app and passing the input(s) to the appropriate argument of
            the module."),
          fluidRow(
            column(
              width = 4,
              ## Path Slider ----
              sliderInput(
                inputId = "paths",
                label = "Number of paths",
                min = 1,
                max = 5,
                step = 1,
                value = 1,
                round = TRUE,
                ticks = FALSE
              ),
              ## Sample Size Slider ----
              sliderInput(
                inputId = "size",
                label = HTML(paste0("Sample size (",
                                    tags$em("n"),
                                    ")")),
                min = 10,
                max = 1000,
                step = 5,
                value = 100,
                round = TRUE,
                ticks = FALSE
              )
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
  gammaScale <- reactive({ 1/sqrt(abs(gammaShape())) })
  gammaMax <- reactive({
    ifelse(input$skewness !=0, max(qgamma(0.999, shape = gammaShape(),
                                      scale = gammaScale()) + 2, 10), 0)
  })
  kurtTheta <- reactive({
    if(input$kurtosis < 0) {
      -3 * (input$kurtosis + 2) / (2 * input$kurtosis)
    } else if (input$kurtosis > 0) {
      6 / input$kurtosis + 4
    } else { 0 }
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
    if(input$population == "start"){
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
      if(input$kurtosis < 0) {
        plot <- plot + ggplot2::stat_function(
          data = data.frame(x = seq(from = -10, to = 10, by = 1)),
          fun = function(x){dbeta(x = x/20 + 0.5, shape1 = kurtTheta(),
                                  shape2 = kurtTheta())},
          color = psuPalette[1], size = 1.5)
      } else if (input$kurtosis > 0) {
        plot <- plot + ggplot2::stat_function(
          fun = function(x){dt(x = x, df = kurtTheta())},
          color = psuPalette[1], size = 1.5) +
          ggplot2::stat_function(fun = function(x){
            exp(-log(2) - log(1) - log( cosh( 0.5*pi*(x-0)/1 ) ))}) +
          ggplot2::stat_function(fun = dnorm, color = "red")
      } else {
        plot <- plot + ggplot2::stat_function(
          data = data.frame(x = seq(from = -10, to = 10, by = 1)),
          fun = dnorm, args = list(mean = 0, sd = 1),
          color = psuPalette[1], size = 1.5)
      }
    } else if (input$population == "bimodal") {

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
    plot<-ggplot2::ggplot(aes(x=x, y=y), data= data) +
      geom_path(color="#0072B2", size=1.5) +
      xlim(xlims) +
      xlab("Value") +
      ylab("Density") +
      ggtitle("Population Graph")+
      theme(axis.text = element_text(size=18),
            plot.title = element_text(size=18, face="bold"),
            axis.title = element_text(size=18),
            panel.background = element_rect(fill = "white", color="black")
      )
    # For case in symmetric where path is 1 causing "box" shape
    if(path ==1){
      plot<-plot+
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
