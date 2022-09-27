shinyServer(function(input, output) {

  output$plot1 <- renderPlot({
    ggplot(df, aes(x=area, y=poptotal)) + geom_point()
  })

  output$plot2 <- renderPlot({
    ggplot(df, aes(x=area, y=poptotal)) + geom_line()
  })

})
