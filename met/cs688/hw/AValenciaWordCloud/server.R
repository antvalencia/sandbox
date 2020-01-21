# Example: Shiny app that search Wikipedia web pages
# File: server.R 
library(shiny)
library(tm)
library(stringi)
library(proxy)
library(wordcloud)
source("WikiSearch.R")

shinyServer(
  function(input, output) {
    terms <- reactive({
      # Change when the "update" button is pressed...
      input$update
      # ...but not for anything else
      isolate({
        withProgress({
          setProgress(message = "Processing corpus...")
          SearchWiki(input$select)
        })
      })
    })
    
    output$plot <- renderPlot(
      {
        v <- terms()
        
        freq <- colSums(as.matrix(v))
        ord <- order(freq, decreasing = TRUE)
        wordcloud(
          names(freq[head(ord, n=50)]),
          freq[head(ord, n=50)],
          scale=c(4,0.9),
          colors=brewer.pal(6, "Dark2")
        )
      }
    )
  }
)

