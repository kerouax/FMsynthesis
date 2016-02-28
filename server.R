library(shiny)
library(ggplot2)
library(formula.tools)

server <- function(input, output) {
    x = seq(0, 8*pi, by=0.001)
    
    f1 = function(x) {(input$amp1*0.5 + 1)*sin(as.numeric(input$freq1)*x + 
                         as.numeric(input$type1)*pi/2 + as.numeric(input$offset1)*pi)}
    f2 = function(x) {(input$amp2*0.5 + 1)*sin(as.numeric(input$freq2)*x +
                         as.numeric(input$type2)*pi/2 + as.numeric(input$offset2)*pi)}
    f3 = function(x) {sin(as.numeric(input$freq3)*x + 
                         as.numeric(input$type3)*pi/2 + as.numeric(input$offset3)*pi)}
    f4 = function(x) {f1(x) + f2(x) + f3(x)}

        
    # Create vectors: breaks and corresponding labels as multiples of pi/2
    vec.breaks <- seq(from = 0, to = 16*pi/2, by = pi/2)
    pi.halfs <- c(paste(expression(pi), "/2"),
                  paste(seq(from = 3, to = 21, by = 2), "*" , expression(pi), "/2"))
    pi.fulls <- c(paste(expression(pi)),
                  paste(seq(from = 2, to = 11, by = 1), "*" , expression(pi)))
    vec.expr <- parse(text = c("0", rbind(pi.halfs, pi.fulls)))[1:17]

        
    g = ggplot(data=data.frame(x), aes(x)) +
        stat_function(fun=f4, col="orangered", lwd=0.6, linetype="solid", 
                      geom="area", alpha=0.6) +
        labs(x = "Frequency", y = "Amplitude") +
        scale_x_continuous(breaks=vec.breaks, labels=vec.expr) +
        coord_cartesian(ylim = c(-3.5, 3.5), xlim=c(0, 8*pi)) +
        theme_dark() 
    
    
    output$resultWave <- renderPlot({g})
}
