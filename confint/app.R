#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Plot function

colorArea <- function(from, to, density, ..., 
                      col="blue", dens=NULL) {
    y_seq <- seq(from, to, length.out=500)
    d <- c(0, density(y_seq, ...), 0)
    polygon(c(from, y_seq, to), d, col = col, density=dens)
}

confint_plot <- function(alpha = 0.05, 
                         m = 0) {
    
    curve(dnorm(x), from=-4, to=4, 
          ylab = "", xlab = "", 
          xaxt = "n", yaxt = "n", bty = "n", 
          xlim = c(-4, 4))
    
    m <- min(m, alpha)
    crit_low <- max(qnorm(m), -4)
    crit_high <- min(qnorm(1-alpha+m), 4)
    
    colorArea(from=crit_low, to=crit_high, dnorm, 
              col="lightgrey")
    colorArea(from=-4, to=crit_low, dnorm, 
              col="white")
    colorArea(from=4, to=crit_high, dnorm, 
              col="white")
        
    abline(v = 0, lty = 2)
    axis(side=1, at=c(-4, crit_low, crit_high, 4), 
         cex.axis = 1.5,
         labels = c(expression(-infinity), 
                    expression(z[m]), 
                    expression(z[1-alpha+m]),  
                    expression(-infinity)))
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Confidence interval"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("alpha", 
                        "Confidence level 1-alpha", 
                        value = 0.95, min = 0.5, max = 1, step = 0.01),
            sliderInput("m", "m", value = 0, 
                        min = 0, max = 0.5, step = 0.001)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("confintPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$confintPlot <- renderPlot({
        confint_plot(alpha = 1-input$alpha, 
                     m = input$m)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
