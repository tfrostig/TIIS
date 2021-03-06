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

shadeCurve <- function(temp.df, lim.start, lim.end, fill = "red", shaded = 0.5){
  geom_area(data = subset(temp.df, x >= lim.start & x < lim.end),
            aes(y = y, x = x), fill = fill, color = NA, alpha = shaded)
}


createDF <- function(mean.h, true.sig, n) { 
  if (is.na(mean.h)) { 
    return(data.frame('y' = NA,
                      'x' = NA))  
  }  
  if (!is.na(mean.h)) { 
    true.mean.sig <- true.sig / sqrt(n)
    x <- seq(mean.h - 4 * true.mean.sig, mean.h + 4 * true.mean.sig, 0.01)
    return(data.frame('y' = dnorm(x, mean.h, true.mean.sig),
                      'x' = x))
  }
}

createAlphaLimits <- function(temp.plot, type.hyp, alpha, temp.df, mean.h, true.mean.sig, col) { 
  if (type.hyp == 1) { 
    k <- qnorm(1 - alpha, mean.h, true.mean.sig)
    return(temp.plot +  shadeCurve(temp.df = temp.df, 
                                   lim.start = qnorm(1 - alpha, mean.h, true.mean.sig), 
                                   lim.end   = Inf, 
                                   fill      = col,
                                   shaded    = 0.25) + 
             annotate("text", x = k, y = 0.05, label = "K", parse = TRUE, size = 5))
  }  
  if (type.hyp == 2) { 
    k <- qnorm(alpha, mean.h, true.mean.sig)
    return(temp.plot +  shadeCurve(temp.df = temp.df, 
                                   lim.start = -Inf, 
                                   lim.end   = k, 
                                   fill      = col,
                                   shaded    = 0.25) + 
             annotate("text", x = k, y = 0.05, label = "K", parse = TRUE, size = 5)) 
  }
  if (type.hyp == 3) { 
    k1 <-  qnorm(alpha / 2, mean.h, true.mean.sig)
    k2 <-  qnorm(1 - alpha / 2, mean.h, true.mean.sig)
    return(temp.plot + 
             shadeCurve(temp.df = temp.df, 
                        lim.start = -Inf, 
                        lim.end   = k1, 
                        fill      = col,
                        shaded    = 0.25) + 
             shadeCurve(temp.df   = temp.df, 
                        lim.start = k2, 
                        lim.end   = Inf, 
                        fill      = col,
                        shaded    = 0.25) +
             annotate("text", x = k1, y = 0.05, label = "K[1]", parse = TRUE, size = 5) + 
             annotate("text", x = k2, y = 0.05, label = "K[2]", parse = TRUE, size = 5))
  }
}


createPvalLimits <- function(temp.plot, type.hyp, alpha, temp.df, mean.h, true.mean.sig, col) { 
  if (type.hyp == 1) { 
    return(temp.plot +  shadeCurve(temp.df = temp.df, 
                                   lim.start = qnorm(1 - alpha, mean.h, true.mean.sig), 
                                   lim.end   = Inf, 
                                   fill      = col,
                                   shaded    = 0.25))
  }  
  if (type.hyp == 2) { 
    return(temp.plot +  shadeCurve(temp.df = temp.df, 
                                   lim.start = -Inf, 
                                   lim.end   = qnorm(1 - alpha, mean.h, true.mean.sig), 
                                   fill      = col,
                                   shaded    = 0.25)) 
  }
  if (type.hyp == 3) { 
    k1 <- qnorm(alpha, mean.h, true.mean.sig)
    k2 <- qnorm(1 - alpha, mean.h, true.mean.sig)
    return(temp.plot + 
             shadeCurve(temp.df = temp.df, 
                        lim.start = -Inf, 
                        lim.end   = k1, 
                        fill      = col,
                        shaded    = 0.25) + 
             shadeCurve(temp.df   = temp.df, 
                        lim.start = k2, 
                        lim.end   = Inf, 
                        fill      = col,
                        shaded    = 0.25)) 
    
  }
}



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  dat_h0 <- reactive({
    createDF(input$H0 , input$Sigma, input$n)
  })
  
  dat_h1 <- reactive({
    createDF(input$H1 , input$Sigma, input$n)
  })
  
  output$distPlot <- renderPlot({
    temp.plot <- ggplot() + 
      geom_line(data = dat_h0(), aes(y = y, x = x), col = 'blue')
    if (!is.na(input$mean)) { 
      temp.plot <- temp.plot +  geom_vline(xintercept = input$mean, col = 'red') +
        annotate('text',
                 x = input$mean,
                 y = 0.1,
                 label = "bar(x)" ,parse = TRUE, size = 10)
    }
    temp.plot <- createAlphaLimits(temp.plot,
                                   temp.df = dat_h0(),
                                   type.hyp = input$radio,
                                   alpha    = input$alpha,
                                   mean.h   = input$H0,
                                   true.mean.sig = input$Sigma / sqrt(input$n),
                                   col = 'blue')
    pval      <- pnorm(input$mean, input$H0, input$Sigma / sqrt(input$n))
    temp.plot <- createPvalLimits(temp.plot, 
                                  temp.df  = dat_h0(),
                                  type.hyp = input$radio,
                                  alpha    = min(pval, 1 - pval),
                                  mean.h   = input$H0,
                                  true.mean.sig = input$Sigma / sqrt(input$n),
                                  col = 'red')
    if (!is.na(input$H1)) { 
      temp.plot <- temp.plot + 
        geom_line(data = dat_h1(), aes(y = y, x = x), col = 'orange') 
      temp.plot <- createAlphaLimits(temp.plot, 
                                     temp.df = dat_h1(), 
                                     type.hyp = input$radio, 
                                     alpha = input$alpha,
                                     mean.h = input$H0, 
                                     true.mean.sig = input$Sigma / sqrt(input$n),
                                     col = 'orange')
      
    }
    
    if (input$radio == 3) { 
      temp.plot <- temp.plot +  
        geom_vline(xintercept = input$mean + qnorm(1 -input$alpha) * input$Sigma / sqrt(input$n), 
                   col = 'red', linetype = 'dashed', size = 1.2) + 
        geom_vline(xintercept = input$mean  - qnorm(1 -input$alpha) * input$Sigma / sqrt(input$n), 
                   col = 'red', linetype = 'dashed', size = 1.2)
    }
    temp.plot + 
      theme(text = element_text(size=20), legend.position = 'bottom') + 
      scale_color_manual() +
      guides(color = guide_legend(override.aes = list(size = 4,
                                                      shape = 21,
                                                      labels = c("Alpha", "P-value", "Power"),
                                                      color = c("blue", "red", "orange"),
                                                      fill = c("blue", "red", "orange")),
                                  order = 1))
  })
  
})
