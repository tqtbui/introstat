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

t_col <- function(color, percent = 50, name = NULL) {
    #      color = color name
    #    percent = % transparency
    #       name = an optional name for the color
    
    ## Get RGB values for named color
    rgb.val <- col2rgb(color)
    
    ## Make new color using input color as base and alpha set by transparency
    t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
                 max = 255,
                 alpha = (100 - percent) * 255 / 100,
                 names = name)
    
    ## Save the color
    invisible(t.col)
}

errors_plot <- function(diff = 2, 
                        alpha = 0.05, 
                        which.side = c("two-sided", 
                                       "one-sided-upper", 
                                       "one-sided-lower")) {
    
    max1 <- 4
    min1 <- -4
    
    max2 <- 4+diff
    min2 <- -4+diff
    
    curve(dnorm(x), from=min1, to=max1, 
          ylab = "", xlab = "", 
          xaxt = "n", yaxt = "n", bty = "n", 
          xlim = c(min(min1, min2), max(max1, max2)))
    curve(dnorm(x, mean = diff), 
          from = min2, to = max2, add = TRUE)
    abline(v=0, lty=2)
    abline(v=diff, lty=2)
    
    cols <- c(t_col("red"), t_col("blue"))
    
    which.side <- match.arg(which.side)
    if (which.side == "two-sided") {
        t.stat <- qnorm(1-alpha/2)
        colorArea(from = min1, to=-abs(t.stat), 
                  dnorm, col = cols[1])
        colorArea(from = abs(t.stat), to=max1, 
                  dnorm, col = cols[1])
        if (diff > 0) {
            colorArea(from = min2, to = abs(t.stat), 
                      dnorm, mean = diff, col = cols[2])
        } else {
            colorArea(from = -abs(t.stat), to = max2, 
                      dnorm, mean = diff, col = cols[2])
        }
    } else if (which.side == "one-sided-upper") {
        t.stat <- qnorm(1-alpha)
        colorArea(from = t.stat, to=max1, 
                  dnorm, col = cols[1])
        if (diff > 0) {
            colorArea(from = min2, to = t.stat, 
                      dnorm, mean = diff, col = cols[2])
        } else {
            colorArea(from = t.stat, to = max2, 
                      dnorm, mean = diff, col = cols[2])
        }
    } else if (which.side == "one-sided-lower") {
        t.stat <- qnorm(alpha)
        colorArea(from = min1, to=t.stat, 
                  dnorm, col = cols[1])
        if (diff > 0) {
            colorArea(from = min2, to = t.stat, 
                      dnorm, mean = diff, col = cols[2])
        } else {
            colorArea(from = t.stat, to = max2, 
                      dnorm, mean = diff, col = cols[2])
        }
    }
    
    axis(side = 1, 
         at = c(min(min1, min2), max(max1, max2), 
                0, diff, t.stat), 
         labels = c(expression(-infinity), 
                    expression(infinity), 
                    expression(mu[0]), 
                    expression(mu[1]), 
                    expression(t)))
    
    legend("topleft", 
           c("Type I error", "Type II error"), 
           bty = "n", 
           fill = cols, 
           cex = 1)
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Types of errors"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("which.side", 
                        "Test side", c("two-sided", 
                                       "one-sided-upper", 
                                       "one-sided-lower")),
            sliderInput("alpha", 
                        "Significance level", value = 0.05, 
                        min = 0, max = 0.5, step = 0.001),
            sliderInput("diff", "Mean difference", value = 2, 
                        min = -4, max = 4, step = 0.01)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("testPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$testPlot <- renderPlot({
        errors_plot(which.side = input$which.side, 
                    alpha = input$alpha, 
                    diff = input$diff)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
