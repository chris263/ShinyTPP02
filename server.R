shinyServer(
  function(input, output) {
    output$flowChart <- renderImage({
      list(src="data/cornStuntFlow.PNG",
           width=600,
           hegth=1000,
           alt="Flow Chart CS"
             )
    })
    output$notaBP <- renderPlot({
      makeG0(myDFF,"Nota",input$tpp)
    })
    output$notaHist <- renderPlot({
      makeH0(myDFF,"Nota",input$tpp)
    })
    output$yieldBP <- renderPlot({
      makeG0(myDFF,"yield",input$tpp)
    })
    output$yieldHist <- renderPlot({
      makeH0(myDFF,"yield",input$tpp)
    })
    output$tbl = renderDT(
      finalTabela2 , options = list(lengthChange = FALSE)
    )
  }  
)