#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application
fluidPage(

    # Application title
    titlePanel("MONTE CARLO SIMULATION: Approximation of π"),

    # Sidebar with a slider input for number of points
    sidebarLayout(
        sidebarPanel(
           sliderInput("slider_points",
                        "Number of points:",
                        min = 0,
                        max = 1000,
                        value = 0,
                        step = 10),
            sliderInput('marker_size',
                         'Point size:',
                         value = 2,
                         min = 0,
                         max = 10,
                         step = 1),
            checkboxInput('show_outline',
                          'Show outline',
                          value = TRUE),
            actionButton("randomize", "Randomize Points"),
            h3(''),
            textOutput("circ_points"),
            textOutput("all_points"),
            h3(''),
            textOutput("actual_pi"),
            textOutput("pi_val"),
            h3(''),
            textOutput("abs_err"),
            textOutput("per_err")
        ),

        # Show a plot of the generated distribution
        mainPanel(

          plotOutput("monte_carlo", height = "480px", width = "480px")
        )
    ),
    fluidRow(
      column(5,
             plotOutput("err_plot", height = "225px", width = "840px")
      )
    )
)
