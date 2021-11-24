#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

shinyServer(function(input, output) {
    airquality$Wind1 <- ifelse(airquality$Wind - 10> 0, airquality$Wind - 10,0)
    model1 <- lm(Ozone ~ Wind, data = airquality)
    model2 <- lm(Ozone ~ Wind1 + Wind, data = airquality)
    
    model1pred <- reactive({
        WindInput <- input$sliderWind
        predict(model1, newdata = data.frame(Wind = WindInput))
    })
    model2pred <- reactive({
        WindInput <- input$sliderWind
        predict(model2, newdata = 
                data.frame(Wind = WindInput,
                           Wind1 = ifelse(WindInput - 10 > 0,
                                          WindInput - 10, 0)))
    })
    
    output$plot1 <- renderPlot({
        WindInput <- input$sliderWind
        
        plot(airquality$Wind, airquality$Ozone, xlab = "Wind [mph]",
             ylab = "Ozone [ppb]", bty = "n", pch = 16,
             xlim = c(0, 25), ylim = c(0, 200))
        if(input$showModel1){
            abline(model1, col = "red", lwd = 2)
        }
        if(input$showModel2){
            model2lines <- predict(model2, newdata = data.frame(
                Wind = 1:21, Wind1 = ifelse(1:21 - 10 > 0, 1:21 - 10, 0)
            ))
            lines(1:21, model2lines, col = "blue", lwd = 2)
        }
        legend(15, 160, c("Model 1 Prediction", "Model 2 Prediction"), pch = 16,
               col = c("red", "blue"), bty = "n", cex = 1.2)
        points(WindInput, model1pred(), col = "red", pch = 16, cex = 2)
        points(WindInput, model2pred(), col = "blue", pch = 16, cex = 2)
    })
    output$pred1 <- renderText({
        model1pred()
    })
    output$pred2 <- renderText({
        model2pred()
    })
}) 
