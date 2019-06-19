library(shiny)


ui<-fluidPage(
  titlePanel('Regression models of iris'),
  sidebarLayout(
    sidebarPanel(
      helpText(),
      selectInput('x', 'Build a regression model of iris',
                  choices = c("",names(iris)[-5])),
      actionButton("analysis"," Analysis ")
    ),
    mainPanel(
      verbatimTextOutput("text"),
      plotOutput('regPlot')
    )
  )
)


require(ggplot2)

server<-function(input, output) {
  regEquation=reactive({
    options(digits = 2)
    fit <- eval(parse(text=paste0("lm( Sepal.Length ~",input$x,",data = iris)")))
    b   <- coef(fit)
    equation=paste0("Sepal.Length = ",round(b[2],2),input$x," + ",round(b[1],2))
    equation
  })
  output$regPlot <- renderPlot({
    input$analysis
    
    
    
    isolate({
      ggplot(data=iris,aes_string(req(input$x),"Sepal.Length"))+
        geom_point()+
        geom_smooth(method="lm")+
        ggtitle(regEquation())
    })
  })
  
  output$text=renderPrint({
    input$analysis
    
    isolate({
      options(digits = 2)
      fit <- eval(parse(text=paste0("lm(Sepal.Length ~",req(input$x),",data = iris)")))
      b   <- coef(fit)
      summary(fit)
    })
  })
}



shinyApp(ui = ui, server = server)