#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define server logic required to draw a histogram
function(input, output, session) {
    
    n_max = 1000
    
    # Setup the colors differentiating the points
    inside_color = 'blue'
    outside_color = 'black'
    
    # Initialize the value for x_rand, y_rand
    set.seed(pi)
    x_rand_int <- runif(n_max, -1, 1)
    y_rand_int <- runif(n_max, -1, 1)
    color_rand_int <- ifelse(sqrt(x_rand_int**2 + y_rand_int**2) <= 1, inside_color, outside_color)
    
    # Initialize the approximation line plot
    approx_list = vector()
    for (n in 1:n_max){
      circle_point <- color_rand_int[1:n]
      approx_list[n] <- 4*length(circle_point[circle_point == inside_color])/n
    }
    
    # Create the data frame for initial approximation
    err_df <- data.frame(n = 1:n_max, pi_approx = approx_list)

    # Initialize reactive value for x_rand, y_rand
    x_rand <- reactiveVal(x_rand_int)
    y_rand <- reactiveVal(y_rand_int)
    color_rand <- reactiveVal(color_rand_int)
    err_plot <- reactiveVal(err_df)
    
    # Randomize points for every button click
    observeEvent(input$randomize, {
      x_rand(runif(n_max, -1, 1))
      y_rand(runif(n_max, -1, 1))
      color_rand(ifelse(sqrt(x_rand()**2 + y_rand()**2) <= 1, inside_color, outside_color))
    })
    
    observeEvent(input$randomize, {
      approx_list = vector()
      for (n in 1:n_max){
        circle_point <- color_rand()[1:n]
        approx_list[n] <- 4*length(circle_point[circle_point == inside_color])/n
      }
      err_plot(data.frame(n = 1:n_max, pi_approx = approx_list))
    })
    
    # Value of marker size from slider
    marker_size <- reactive({
      marker_size <- input$marker_size
    })
    
    # Value of number of points from slider
    number_of_points <- reactive({
      number_of_points <- input$slider_points
    })
    
    # Number of x values to be plotted
    x_scatter <- reactive({
      x_rand()[0:number_of_points()]
    })
    
    # Number of y values to be plotted
    y_scatter <- reactive({
      y_rand()[0:number_of_points()]
    })
    
    # Indicates the color between inside and outside the circle
    color_scatter <- reactive({
      color_rand()[0:number_of_points()]
    })
    
    # Compute the approximation of pi values on ratio of area
    pi_approx <- reactive({
      4*length(color_scatter()[color_scatter() == inside_color])/number_of_points()
    })
    
    # Generate the scatter plot simulation  
    output$monte_carlo <- renderPlot({
        
        # Create data for the circle
        radius <- 1
        center_x <- 0
        center_y <- 0
        n_points <- 100  # Number of points to approximate the circle
        
        # Calculate points on the circle
        theta <- seq(0, 2 * pi, length.out = n_points)
        x_circle <- center_x + radius * cos(theta)
        y_circle <- center_y + radius * sin(theta)
        
        # Create a data frame for the circle
        circle_data <- data.frame(x = x_circle, y = y_circle)
        
        # Create data for the square
        square_size <- 2 * radius
        x_square <- c(center_x - square_size / 2, center_x + square_size / 2, center_x + square_size / 2, center_x - square_size / 2)
        y_square <- c(center_y - square_size / 2, center_y - square_size / 2, center_y + square_size / 2, center_y + square_size / 2)
        
        # Create a data frame for the square
        square_data <- data.frame(x = x_square, y = y_square)
        
        # The data frame of the scatter data with its color
        scatter_data <- data.frame(x = x_scatter(), 
                                   y = y_scatter(), 
                                   color = color_scatter())
        
        # Whether to show the outline or not
        outline <- ifelse(input$show_outline, 'black', 'white')
        
        # Draw the scatterplot based on the number of points in the slider
        ggplot() +
          geom_path(data = circle_data, aes(x, y), color = outline) +  # Circle
          geom_polygon(data = square_data, aes(x, y), fill = NA, color = outline) +  # Square
          geom_point(data = scatter_data, aes(x, y), color = color_scatter(), size = marker_size()) +  # Scatterplot
          coord_fixed(ratio = 1) +  # Ensure an equal aspect ratio for a circular appearance
          labs(x = "X-axis", y = "Y-axis") +  # Axis labels
          theme_void() + 
          theme(legend.position = "none")
    })
    
    # Reactive texts based on the settings
    
    output$circ_points <- renderText({
      sprintf('Points inside the circle: %.0f', length(color_scatter()[color_scatter() == inside_color]))
    })
    
    output$all_points <- renderText({
      sprintf('Total number of points: %.0f', number_of_points())
    })
    
    output$actual_pi <- renderText({
      sprintf('Actual value of π: %.10f', pi)
    })
    
    output$pi_val <- renderText({
      ifelse(number_of_points() == 0, 'Add some points', sprintf('Approximate value of π: %.10f', pi_approx()))
    })
    
    output$abs_err <- renderText({
      ifelse(number_of_points() == 0, 'Absolute error: -', sprintf('Absolute error: %.10f', abs(pi - pi_approx())))
    })
    
    output$per_err <- renderText({
      ifelse(number_of_points() == 0, 'Percentage error: -', sprintf('Percentage error: %.2f%%', abs(pi_approx() - pi)/pi*100))
    })
    
    # Create plot of approxiate pi versus the actual value of pi
    output$err_plot <- renderPlot({
        p <- ggplot() + 
          geom_line(data = err_plot(), aes(n, pi_approx), color = inside_color) + 
          geom_hline(yintercept = pi, linetype = "dashed", color = 'black') + 
          geom_segment(
            aes(x = number_of_points(), y = pi, xend = number_of_points(), yend = pi_approx()),
            color = "red", size = 0.5) + 
          geom_point(aes(number_of_points(), pi_approx()), color = inside_color, size = 2.5) + 
          labs(x = 'Number of Points', y = 'π Approximation') + 
          ylim(2.25, max(4.25)) +
          geom_text(aes(x = -10, y = pi, label = 'π'), vjust = -0.5, color = "black", size = 4, fontface = "bold") + 
          geom_text(aes(x = number_of_points(), y =  pi_approx(), label =round(pi_approx(),2)), 
                    vjust = -1, color = inside_color, size = 4, fontface ="bold")

        suppressWarnings(print(p))
    })
}







