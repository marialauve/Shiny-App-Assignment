#
# This is the server logic of a Shiny web application.
# You can run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
shinyServer(function(input, output) {
    model <- reactive({
        brushed_data <- brushedPoints(iris, input$brush1,
                                      xvar = "Sepal.Length", yvar = "Petal.Length")
        if(nrow(brushed_data) < 2){
            return(NULL)
        }
        lm(Petal.Length ~ Sepal.Length, data = brushed_data)
    })
    
    output$slopeOut <- renderText({
        if(is.null(model())){
            "No Model Found"
        } else {
            model()[[1]][2]
        }
    })
    
    output$intOut <- renderText({
        if(is.null(model())){
            "No Model Found"
        } else {
            model()[[1]][1]
        }
    })
    
    output$plot1 <- renderPlot({
        plot(iris$Sepal.Length, iris$Petal.Length, xlab = "Sepal Length",
             ylab = "Petal Length", main = "Iris Measurements",
             cex = 1.5, pch = 16, bty = "n", col = factor(iris$Species))
        legend(list(x = 6.75, y = 3), c("Setosa", "Versicolor", "Verginica"),
               pch = 19, col=1:3)
            if(!is.null(model())){
            abline(model(), col = "blue", lwd = 2)
        }
    })
})