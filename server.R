#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(rtable)
library(shiny)
library(ReporteRsjars)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  file= reactive(input$file)
  
  data = reactive({
    if (is.null(file())){
      NULL
    } else {
      readLines(file()$name) %>% as.list()
    }
  })
  
  output$value = renderPrint(
    data()
  )
  
  myDtm = reactive({
    clean_data = clean_text(data())
    stem(clean_data)
  })
  
  df = reactive({
    freq_table(myDtm())
  })
  
  output$table1 = renderDataTable(
    df()
  )
  
  output$plot1 = renderPlot({
    hist_data = df()[1:input$n1,]
    ggplot(hist_data, aes(reorder(word, -freq), freq))+
      geom_bar(stat="identity") +
      theme_minimal() +
      theme(axis.text.x=element_text(angle=45, hjust=1)) +
      ylab("Frequency") + xlab("")
  })
  
  output$plot2 = renderPlot({
    wordcloud(df()$word, df()$freq, 
              max.words = input$n2, scale = c(4,0.5),
              colors = brewer.pal(8, "Dark2") )
  })
  
  dtmss = reactive({
    removeSparseTerms(myDtm(), input$sparsity)       
  })
  
  d = reactive(dist((dtmss()), method = "euclidian"))
  
  fit = reactive(hclust(d = d(), method = "ward.D"))
  
  output$plot3 = renderPlot({
    # remove sparse terms
    plot(fit(), hang=-1, xlab = "", sub ="")
    rect.hclust(fit(), input$k, border="red") # draw dendogram with red borders around the 5 clusters   
    groups = cutree(fit(), input$k)   # "k=" defines the number of clusters you are using 
  })
  
  output$plot4 = renderPlot({
    kfit = kmeans(d(), input$k)   
    clusplot(as.matrix(d()), kfit$cluster, main = "",
             color=T, shade=T, labels=2, lines=0)
  })
  
  output$plot5  = renderPlot({
    termDocMatrix = as.matrix(dtmss())
    termDocMatrix[termDocMatrix>=1] = 1
    termMatrix = termDocMatrix %*% t(termDocMatrix)
    g = graph.adjacency(termMatrix, weighted=T, mode = "undirected")
    V(g)$label = V(g)$name
    V(g)$degree = degree(g)
    # remove loops
    g = simplify(g)
    V(g)$label.cex = log(rank(V(g)$degree)) + 1
    V(g)$label.color = rgb(0, 0, .2, .8)
    V(g)$frame.color = NA
    egam = (log(E(g)$weight)+.4) / max(log(E(g)$weight)+.4)
    E(g)$color = rgb(.5, .5, 0, egam)
    E(g)$width = egam*2
    plot(g)
  })
  
  observe({
    input$reset
    session$sendCustomMessage(type = "resetFileInputHandler", "file")   
  })
  

    

  
})
