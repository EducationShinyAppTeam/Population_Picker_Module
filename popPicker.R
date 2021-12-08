#' popPickerUI
#'
#' The UI component for the Population Picker Shiny module
#'
#' @param namespaceID Required--the unique namespace for each instance
#' @param discMenu Optional controls for how you want discrete distributions to
#'  be listed--Future Development
#' @param contMenu Optional controls for how you want continuous distributions to
#'  be listed--Future development
#' @return UI components for the population picker
#' @examples
#' popPickerUI(namespaceID = "popPicker")
#'
#' @export
popPickerUI <- function(namespaceID, discMenu = "default", contMenu = "default"){
  ## Discrete choices ----
  discMenu <- tolower(discMenu)
  if (discMenu == "none") {
    discreteList <- NULL
  } else if (discMenu == "default" || discMenu == "examples") {
    discreteList <- list(
      "Accident rate" = "poisson",
      "Astragalus (bone die)" = "astragalus",
      "Fair die" = "disEquip",
      "Playlist" = "playlist"
    )
  }
  ## Continuous choices ----
  contMenu <- tolower(contMenu)
  if (contMenu == "none") {
    continuousList <- NULL
  } else if (contMenu == "shapes") {
    continuousList <- list(
      "Skewed" = "skew",
      "Symmetric" = "sym",
      "Bimodal" = "bimodal",
      "Triangular" = "tri"
    )
  } else if (contMenu == "default") {
    continuousList <- list(
      "Skewed" = "skew",
      "Symmetric" = "sym",
      "Bimodal" = "bimodal",
      "Triangular" = "tri",
      "Cauchy" = "cauchy"
    )
  }
  ## UI elements ----
  tagList(
    fluidRow( # Create the complete picker's row
      column( # Create main column split
        width = 4,
        wellPanel(
          selectInput(
            inputId = NS(namespace = namespaceID, id = "population"),
            label = "Population Type",
            choices = list(
              "Select a population" = "start",
              "Continuous" = continuousList,
              "Discrete" = discreteList
            )
          ),
          ### Skewness Elements ----
          conditionalPanel(
            ns = NS(namespace = namespaceID),
            condition = "input.population == 'skew'",
            sliderInput(
              inputId = NS(namespace = namespaceID, id = "skewness"),
              label = "Skewness",
              min = -2,
              max = 2,
              step = 0.1,
              value = 0,
              ticks = TRUE
            ),
          ),
          ### "Symmetric Elements" ----
          conditionalPanel(
            ns = NS(namespace = namespaceID),
            condition = "input.population == 'sym'",
            sliderInput(
              inputId = NS(namespace = namespaceID, id = "kurtosis"),
              label = "Excess Kurtosis",
              min = -2,
              max = 2,
              step = 0.1,
              value = 0,
              ticks = TRUE
            ),
          ),
          ### Bimodal Elements ----
          conditionalPanel(
            ns = NS(namespace = namespaceID),
            condition = "input.population == 'bimodal'",
            sliderInput(
              inputId = NS(namespace = namespaceID, id = "leftMode"),
              label = "Percentage under left mode",
              min = 10,
              max = 90,
              step = 1,
              value = 50,
              ticks = TRUE,
              post = "%"
            )
          ),
          ### Triangular Elements ----
          conditionalPanel(
            ns = NS(namespace = namespaceID),
            condition = "input.population == 'tri'",
            sliderInput(
              inputId = NS(namespace = namespaceID, id = "lowerBound"),
              label = "Lower bound",
              min = -5,
              max = 5,
              step = 0.5,
              value = -5,
              ticks = TRUE
            ),
            sliderInput(
              inputId = NS(namespace = namespaceID, id = "upperBound"),
              label = "Upper bound",
              min = -5,
              max = 5,
              step = 0.5,
              value = 5,
              ticks = TRUE
            ),
            sliderInput(
              inputId = NS(namespace = namespaceID, id = "mode"),
              label = "Most probable value",
              min = -5,
              max = 5,
              step = 0.5,
              value = 0,
              ticks = TRUE
            ),
            conditionalPanel(
              ns = NS(namespace = namespaceID),
              condition = "input.upperBound <= input.lowerBound",
              p(tags$em("Note: "), "Lower bound must be less than upper bound.")
            ),
            conditionalPanel(
              ns = NS(namespace = namespaceID),
              condition = "input.mode > input.upperBound",
              p(tags$em("Note: "), "Most probable value must be between bounds.")
            ),
            conditionalPanel(
              ns = NS(namespace = namespaceID),
              condition = "input.mode < input.lowerBound",
              p(tags$em("Note: "), "Most probable value must be between bounds.")
            ),
          ),
          ### Cauchy Elements ----
          conditionalPanel(
            ns = NS(namespace = namespaceID),
            condition = "input.population == 'cauchy'",
            sliderInput(
              inputId = NS(namespace = namespaceID, id = "medianMode"),
              label = "Distribution Median and Mode",
              min = -2,
              max = 2,
              step = 0.5,
              value = 0,
              ticks = TRUE
            ),
            sliderInput(
              inputId = NS(namespace = namespaceID, id = "halfWidth"),
              label = "Half of the IQR",
              min = 0.1,
              max = 4,
              step = 0.1,
              value = 1,
              ticks = TRUE
            )
          ),
          ### Poisson Elements----
          conditionalPanel(
            ns = NS(namespace = namespaceID),
            condition = "input.population == 'poisson'",
            sliderInput(
              inputId = NS(namespace = namespaceID, id = "unitRate"),
              label = "Unit rate (mean)",
              min = 0,
              max = 10,
              step = 0.1,
              value = 1,
              ticks = TRUE
            ),
            conditionalPanel(
              ns = NS(namespace = namespaceID),
              condition = "input.unitRate == 0",
              p(tags$em("Note: "), "when the Unit Rate (Mean) is 0, the Variance is also
                      0, resulting in all cases being the same.")
            )
          ),
          ### Fair die Elements ----
          conditionalPanel(
            ns = NS(namespace = namespaceID),
            condition = "input.population == 'disEquip'",
            selectInput(
              inputId = NS(namespace = namespaceID, id = "numSides"),
              label = "Number of sides",
              choices = c(4, 6, 8, 10, 12, 20, 48, 120)
              # Remember to convert input$numSides to number
            )
          ),
          ### Playlist ----
          conditionalPanel(
            ns = NS(namespace = namespaceID),
            condition = "input.population == 'playlist'",
            p("Enter the number of songs in each genre and which genre you
                    want to track."),
            fluidRow(
              column(
                width = 6,
                numericInput(
                  inputId = NS(namespace = namespaceID, id = "jazzN"),
                  label = "Jazz",
                  value = 1,
                  min = 0,
                  max = NA,
                  step = 1
                ),
                numericInput(
                  inputId = NS(namespace = namespaceID, id = "rockN"),
                  label = "Rock",
                  value = 1,
                  min = 0,
                  max = NA,
                  step = 1
                ),
                numericInput(
                  inputId = NS(namespace = namespaceID, id = "countryN"),
                  label = "Country",
                  value = 1,
                  min = 0,
                  max = NA,
                  step = 1
                ),
                numericInput(
                  inputId = NS(namespace = namespaceID, id = "hipHopN"),
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
                  inputId = NS(namespace = namespaceID, id = "pickGenre"),
                  label = "Genre to track:",
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
        ),
        box(
          title = strong("Key terms/instructions"),
          status = "primary",
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          tags$ul(
            tags$strong("Instructions"),
            tags$li("Click on the dropdown menu to select the population
                      distribution that you would like to work with")
          ),
          tags$ul(
            tags$strong("Terms"),
            tags$li("Kurtosis - The measure of skewness relative to the
                      standard normal distribution"),
            tags$li()
          )
        )
      ),
      ## Population plot ----
      column(
        width = 8,
        plotOutput(outputId = NS(namespace = namespaceID, id = "popPlot"))
      )
    )
  )
}

#' popPickerServer
#'
#' The Server component the Population Picker Shiny module
#'
#' @param namespaceID Required--the unique namespace for each instance
#' @return Server components for the population picker
#' @examples
#' tttServer(namespaceID = "popPicker")
#'
#' @export
popPickerServer <- function(namespaceID){
  require(triangle)
  moduleServer(id = namespaceID, function(input, output, session) {
    ## Ensure at least one and no more than 3 music genres selected
    ## Currently not working, Future development?
    # observe({
    #   if (length(input$pickGenre) > 3) {
    #     updateCheckboxGroupInput(
    #       session = session,
    #       inputId = "pickGenre",
    #       selected = tail(input$pickGenre, 3)
    #     )}
    #   if (length(input$pickGenre) < 1 ) {
    #     updateCheckboxGroupInput(
    #       session = session,
    #       inputId = "pickGenre",
    #       selected = "Jazz"
    #     )}
    # })

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
      validate(
        need(
          expr = input$population != "start",
          message = "Select a population to explore")
      )
      ## Base plot ----
      plot <- ggplot(
        data = data.frame(x = seq(from = -5, to = 5, by = 1)),
        mapping = aes(x = x)) +
        theme_bw() +
        xlab("Value") +
        ylab("Density") +
        ggtitle("Population Graph") +
        theme(
          axis.text = element_text(size = 18),
          plot.title = element_text(size = 18),
          axis.title = element_text(size = 18)
        ) +
        scale_x_continuous(expand = expansion(mult = 0, add = 1)) +
        scale_y_continuous(expand = expansion(mult = c(0.01, 0.1), add = 0))

      ## Distribution Specific plots ----
      ### Skewness ----
      if (input$population == "skew") {
        if (input$skewness > 0) {
          plot <- plot + stat_function(
            data = data.frame(x = seq(from = 0, to = gammaMax(), by = 1)),
            fun = dgamma,
            args = list(shape = gammaShape(), scale = gammaScale()),
            color = psuPalette[1],
            size = 1.5
          )
        } else if (input$skewness < 0) {
          plot <- plot + stat_function(
            data = data.frame(x = seq(from = -1*gammaMax(), to = 0, by = 1)),
            fun = function(x){dgamma(-x, shape = gammaShape(), scale = gammaScale())},
            color = psuPalette[1],
            size = 1.5
          )
        } else {
          plot <- plot + stat_function(
            fun = dnorm,
            args = list(mean = 0, sd = 1),
            color = psuPalette[1],
            size = 1.5
          )
        }
        ### Symmetric/Kurtosis ----
      } else if (input$population == "sym") {
        if (input$kurtosis < 0) {
          plot <- plot + stat_function(
            data = data.frame(x = seq(from = -10, to = 10, by = 1)),
            fun = function(x){dbeta(x = x/20 + 0.5, shape1 = kurtTheta(),
                                    shape2 = kurtTheta())},
            color = psuPalette[1],
            size = 1.5
          )
        } else if (input$kurtosis > 0) {
          plot <- plot + stat_function(
            fun = function(x){dt(x = x, df = kurtTheta())},
            color = psuPalette[1],
            size = 1.5
          )
        } else {
          plot <- plot + stat_function(
            data = data.frame(x = seq(from = -10, to = 10, by = 1)),
            fun = dnorm,
            args = list(mean = 0, sd = 1),
            color = psuPalette[1],
            size = 1.5
          )
        }
        ### Bimodal ----
      } else if (input$population == "bimodal") {
        plot <- plot + stat_function(
          data = data.frame(x = seq(from = 0, to = 1, by = 0.1)),
          fun = biDens,
          args = list(left = input$leftMode/100),
          color = psuPalette[1],
          size = 1.5
        )
        ### Triangular ----
      } else if (input$population == "tri") {
        plot <- plot + stat_function(
          fun = triangle::dtriangle,
          args = list(a = input$lowerBound, b = input$upperBound, c = input$mode),
          color = psuPalette[1],
          size = 1.5
        )
        ### Cauchy ----
      } else if (input$population == "cauchy") {
        plot <- plot + stat_function(
          fun = dcauchy,
          args = list(location = input$medianMode, scale = input$halfWidth),
          color = psuPalette[1],
          size = 1.5
        )
        ### Astragalus ----
      } else if (input$population == "astragalus") {
        data <- data.frame(x = c(1,3,4,6), y = c(.1,.4,.4,.1))
        plot <- makeBarPlot(
          xlab = "Number on roll of astragalus",
          data = data,
          levels = 1:6
        )
        # Matrix of sample values for the astragalus population graph
        drawAdie <-
          reactive(matrix(
            sample(die(), input$aspath * input$assize,
                   replace = TRUE),
            nrow = input$assize,
            ncol = input$aspath
          ))
        ### Playlist ----
      } else if (input$population == "playlist") {
        nSongs <- reactive({
          switch(
            EXPR = input$pickGenre,
            "Jazz" = input$jazzN,
            "Rock" = input$rockN,
            "Country" = input$countryN,
            "Hip-hop" = input$hipHopN
          )
        })
        # Set up songs from four types
        songs <- reactive({
          c(
            rep(input$jazzN),
            rep(input$rockN),
            rep(input$countryN),
            rep(input$hipHopN)
          )
        })

        # Parameters for bar plot
        p <- nSongs() / sum(songs())
        data <- data.frame(
          x = c("Other music (0)", paste(input$pickGenre,"(1)")),
          y = c(1 - p, p)
        )
        data$x <- factor(data$x, levels = data$x) # Done to force sorted order for bars

        # Make bar plot
        plot <- makeBarPlot(xlab = "Genre", data = data)
        ### Poisson ----
      } else if (input$population == "poisson") {
        data <- data.frame(x = 0:ceiling(2*input$unitRate + 5)) # More x's than necessary
        data$y <- (input$unitRate^data$x) * exp(-input$unitRate)/factorial(data$x) # Get y vals for x's
        data <- rbind(data[1:2,], filter(data[-c(1,2), ], y > 0.0005)) # Filter based on probability
        plot <- makeBarPlot(xlab = "Number of accidents", data = data)
        ### Fair die ----
      } else if (input$population == "disEquip") {
        N <- as.numeric(input$numSides)
        data <- data.frame(
          x = seq.int(from = 1, to = N, by = 1),
          y = rep(1/N, times = N)
        )
        plot <- makeBarPlot(
          xlab = "Value rolled",
          data = data,
          levels = 1:N
        )
      }
      return(plot)
    })
    ## Helper Functions ----
    ### Bimodal density function
    biDens <- function(x, left){
      return(
        56 * (left * x * (1 - x)^6 + (1 - left) * x^6 * (1 - x))
      )
    }

    ### makeBarPlot ----
    makeBarPlot <- function(xlab, data, levels = as.character(data$x)){
      plot <- ggplot(
        mapping = aes(x = factor(x, levels = levels), y = y),
        data = data
        ) +
        geom_bar(stat = "identity", fill = psuPalette[1]) +
        ylim(c(0, max(data$y) + 0.1*max(data$y)) ) +
        xlab(xlab) +
        ylab("Probability") +
        ggtitle("Population Graph") +
        theme(
          axis.text = element_text(size = 18),
          plot.title = element_text(size = 18, face = "bold"),
          axis.title = element_text(size = 18),
          panel.background = element_rect(fill = "white", color = "black")
        ) +
        scale_x_discrete(drop = FALSE)
      return(plot)
    }

    ### Create data ----
    dataGenerator <- eventReactive(
      eventExpr = c(input$population, input$skewness, input$kurtosis,
                    input$leftMode, input$lowerBound, input$upperBound,
                    input$mode, input$medianMode, input$halfWidth),
      valueExpr = {
        if (input$population == "skew" & input$skewness < 0) {
          paste0("rgamma(size, shape = ", gammaShape(),
                 ", scale = ", gammaScale(), ")")
        } else if (input$population == "skew" & input$skewness > 0) {
          paste0("-1*rgamma(size, shape = ", gammaShape(),
                 ", scale = ", gammaScale(), ")")
        } else if (input$population == "skew" & input$skewness == 0) {
          "rnorm(size, mean = 0, sd = 1)"
        } else if (input$population == "sym" & input$kurtosis < 0) {
          paste0("0.5 + 20*rbeta(size, shape1 = ", kurtTheta(),
                 ", shape2 = ", kurtTheta(), ")")
        } else if (input$population == "sym" & input$kurtosis > 0) {
          paste0("rt(size, df = ", kurtTheta(), ")")
        } else if (input$population == "sym" & input$kurtosis == 0) {
          "rnorm(size, mean = -15, sd = 1)"
        } else if (input$population == "bimodal") {
          list(alpha = 2, beta = 7, left = input$leftMode/100)
        } else if (input$population == "tri") {
          paste0("rtriangle(size, a = ", input$lowerBound,
                 ", b = ", input$upperBound, ", c = ", input$mode, ")")
        } else if (input$population == "cauchy") {
          paste0("rcauchy(size, location = ", input$medianMode,
                 ", scale = ", input$halfWidth, ")")
        } else {
          "1:5"
        }
      }
    )

    return(
      list(
        pop = reactive({input$population}),
        dataFunction = reactive({dataGenerator()})
      )
    )




    # From Adam
    # unsure of triangular data
    # switch("input.population",
    #               "skew" = sample(1:10, size = 100, replace = TRUE, prob = 10:1),
    #               "sym" = rnorm(n = 10000, mean = 0, sd = 1),
    #               "bimodal" = nn <- 1e4, set.seed(1), betas<-rbeta(nn,2,2),
    #               sims = c(betas[1:(nn/2)]*2+1,
    #                        betas[(nn/2+1):nn]*2+3),
    #               "cauchy" = x_dcauchy <- seq(0, 1, by = 0.02),
    #               y_dcauchy <- dcauchy(x_dcauchy, scale = 5)
    # )

  })
}