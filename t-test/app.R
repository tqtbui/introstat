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

t_test_plot <- function(which.side = c("two-sided", 
                                       "one-sided-upper", 
                                       "one-sided-lower"), 
                        which.test = c("z-test","t-test"), 
                        alpha = 0.05, 
                        t.stat = -4, 
                        df = 1) {
    
    which.side <- match.arg(which.side)
    which.test <- match.arg(which.test)
    
    if (which.test == "z-test") {
        dens <- dnorm
        quant <- qnorm
        crit.symb <- parse(text = "z")[[1]]
    } else if (which.test == "t-test") {
        dens <- dt
        quant <- qt
        formals(dens)$df <- df
        formals(quant)$df <- df
        crit.symb <- parse(text = "t")[[1]]
    }
    
    max.range <- max(quant(0.995), 4)
    
    curve(dens(x), from=-max.range, to=max.range, 
          ylab = "", xlab = "", 
          xaxt = "n", yaxt = "n", bty = "n", 
          xlim = c(-max.range, max.range))
    
    if (which.side == "two-sided") {
        crit <- quant(1-alpha/2)
        
        colorArea(from=-crit, to=crit, dens, 
                  col="lightgrey")
        colorArea(from=-max.range, to=-crit, dens, 
                  col="white")
        colorArea(from=crit, to=max.range, dens, 
                  col="white")
        
        axis(side=1, at=c(-max.range,-crit,crit,max.range, t.stat), 
             cex.axis = 1.5,
             labels = c(expression(-infinity), 
                        as.expression(substitute(A [B],
                                                 list(A = as.name(crit.symb), 
                                                      B = as.name(format(round(alpha/2,3), nsmall=2))))), 
                        as.expression(substitute(A [B],
                                                 list(A = as.name(crit.symb), 
                                                      B = as.name(format(round(1-alpha/2,3), nsmall=2))))),
                        expression(infinity), 
                        "t"))
        
        if (abs(t.stat) > crit) {
            col.p <- "red"
        } else {
            col.p <- "blue"
        }
        
        abline(v = t.stat, lty = 2, col=col.p)
        colorArea(from=-max.range, to=-abs(t.stat), dens, 
                  col=t_col(col.p))
        colorArea(from=abs(t.stat), to=max.range, dens, 
                  col=t_col(col.p))
        
    } else if (which.side == "one-sided-upper") {
        crit <- quant(1-alpha)
        
        colorArea(from=-max.range, to=crit, dens, 
                  col="lightgrey")
        colorArea(from=crit, to=max.range, dens, 
                  col="white")
        
        axis(side=1, at=c(-max.range,crit,max.range, t.stat), 
             cex.axis = 1.5,
             labels = c(expression(-infinity), 
                        as.expression(substitute(A [B],
                                                 list(A = as.name(crit.symb), 
                                                      B = as.name(format(round(1-alpha,3), nsmall=2))))), 
                        expression(infinity), 
                        "t"))
        
        if (t.stat > crit) {
            col.p <- "red"
        } else {
            col.p <- "blue"
        }
        
        abline(v = t.stat, lty = 2, col=col.p)
        colorArea(from=t.stat, to=max.range, dens, 
                  col=t_col(col.p))
        
    } else if (which.side == "one-sided-lower") {
        crit <- quant(alpha)
        
        colorArea(from=crit, to=max.range, dens, 
                  col="lightgrey")
        colorArea(from=-max.range, to=crit, dens, 
                  col="white")
        
        axis(side=1, at=c(-max.range,crit,max.range, t.stat), 
             cex.axis = 1.5,
             labels = c(expression(-infinity), 
                        as.expression(substitute(A [B],
                                                 list(A = as.name(crit.symb), 
                                                      B = as.name(format(round(alpha,3), nsmall=2))))), 
                        expression(infinity), 
                        "t"))
        
        if (t.stat < crit) {
            col.p <- "red"
        } else {
            col.p <- "blue"
        }
        
        abline(v = t.stat, lty = 2, col=col.p)
        colorArea(from=-max.range, to=t.stat, dens, 
                  col=t_col(col.p))
    }
    
    legend("topleft", 
           c(expression(paste("Reject ", H[0])), 
             expression(paste("Do not reject ", H[0]))), 
           bty = "n", 
           fill = c(t_col("red"), t_col("blue")), 
           cex = 1, title = as.expression(bquote(underline("p-value"))))
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Wald-type tests"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("which.test", 
                        "Test type", c("z-test", "t-test")),
            numericInput("df", "Degree of freedom", value = 1, 
                         min = 1, max = 1000),
            selectInput("which.side", 
                        "Test side", c("two-sided", 
                                       "one-sided-upper", 
                                       "one-sided-lower")),
            selectInput("alpha", 
                        "Significance level", 
                        c(0.05, 0.01, 0.1)),
            sliderInput("t.stat", "Test statistic", value = 2, 
                        min = -10, max = 10, step = 0.01)
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
        t_test_plot(which.side = input$which.side, 
                    which.test = input$which.test, 
                    alpha = as.numeric(input$alpha), 
                    t.stat = input$t.stat, 
                    df = input$df)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
