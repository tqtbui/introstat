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
    y_seq <- seq(from, to, length.out=1000)
    d <- c(0, density(y_seq, ...), 0)
    polygon(c(from, y_seq, to), d, col = col, density=dens)
}

probability_plot <- function(ul = 1, ll = 0,  
                             type = c("Discrete", "Continuous")) {
    
    if (type == "Discrete") {
        x <- seq(-4, 20, by = 1)
        y <- dpois(x, lambda = 4)
        
        if (ll > ul) {
            plot(x, y, 
                 col = 1, type = "h", 
                 xlab = "x", ylab = "p(x)", 
                 xlim = c(-4, 20), 
                 ylim = c(0, 0.25), 
                 bty = "n", xaxt = "n", 
                 main = "P(a\u2264 X \u2264 b)")
        } else  {
            idx <- which(x >= ll & x <= ul)
            plot(x[-idx], y[-idx], 
                 col = 1, type = "h", 
                 xlab = "x", ylab = "p(x)", 
                 xlim = c(-4, 20), 
                 ylim = c(0, 0.25), 
                 bty = "n", xaxt = "n", 
                 main = "P(a\u2264 X \u2264 b)")
            
            segments(x0 = x[idx], y0 = rep(0, length(idx)), 
                     x1 = x[idx], y1 = y[idx], 
                     col = "green")
        }
        
    } else if (type == "Continuous") {
        
        curve(dchisq(x, df = 4), 
              from = 0, to = 20, 
              ylab = "f(x)", xlab = "x", 
              xlim = c(-4, 20), 
              bty = "n", 
              ylim = c(0, 0.25), 
              main = "P(a\u2264 X \u2264 b)")
        
        if (ll <= ul) {
            colorArea(from = ll, to = ul, dchisq, 
                      col = "green", df = 4)
        }
        
        segments(x0 = -4, y0 = 0, x1 = 0, y1 = 0)
    }
    
    abline(v = c(ll, ul), col = "grey", lty = 2)
    
    axis(side=1, at=c(-4, -3, 0, 5, 10, 15, 20, ll, ul), 
         cex.axis = 1,
         labels = c("", expression(-3),
                    expression(0), expression(5), 
                    expression(10), expression(15), 
                    expression(20), 
                    expression(italic(a)), expression(italic(b))))
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Calculating probability"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("type", "Type of random variable", 
                        choices = c("Discrete", "Continuous")),
            sliderInput("ul", "Upper limit", value = 6.5, 
                        min = -4, max = 20, step = 0.01),
            sliderInput("ll", "Lower limit", value = 2.5, 
                        min = -4, max = 20, step = 0.01)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("probabilityPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$probabilityPlot <- renderPlot({
        probability_plot(type = input$type, 
                         ul = input$ul, 
                         ll = input$ll)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
