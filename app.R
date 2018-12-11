
library(shiny)
library(tm)
library(wordcloud)
library(stringr)


library(graph)

library(Rgraphviz)
library(png)
library(grid)
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel(title=div(img(src="data.png"), "WordCloud")),
  
  
  
  
  
  
  
  
  
  
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      fileInput("wc",
                "choisissez une base", multiple = FALSE,accept = "text/plain"),
      actionButton("update","Creez un World Cloud")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("wordcloud",    plotOutput("wc_plot")),
                  tabPanel("Plot", fluidPage(
                    column(width = 3,sliderInput("slider",NULL,value = 0.1,min = 0.01,max = 0.5,step = 0.01)),
                    column(width = 9,imageOutput("plot"),align = "center")
                                             
                                             ))
                  
                 
      )
      
      
      
      
      
      
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  observeEvent(input$update, { 
    
    wc_data=reactive({
      withProgress({
        setProgress(message = "telechargement ")
        wc_file=input$wc
        return(wc_file)
        
        
      })
      
    })
    
    
    
    wordcloud_rep=repeatable(wordcloud)
    
    output$wc_plot=renderPlot(withProgress({
      if (!is.null(wc_data())){
        wc_text=readLines(wc_data()$datapath)
        
      }
      else{
        wc_text="bonjour la vie"
      }
      setProgress(message = "creating wordclowd")
      
      docs <- Corpus(VectorSource(wc_text))
      toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
      docs <- tm_map(docs, toSpace, "/")
      docs <- tm_map(docs, toSpace, "@")
      docs <- tm_map(docs, toSpace, "\\|")
      # Convertir le texte en minuscule
      docs <- tm_map(docs, content_transformer(tolower))
      # Supprimer les nombres
      docs <- tm_map(docs, removeNumbers)
      # Supprimer les mots vides anglais
      docs <- tm_map(docs, removeWords, stopwords("english"))
      # Supprimer votre propre liste de mots non d?sir?s
      docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
      # Supprimer les ponctuations
      docs <- tm_map(docs, removePunctuation)
      # Supprimer les espaces vides suppl?mentaires
      docs <- tm_map(docs, stripWhitespace)
      # Text stemming
      # docs <- tm_map(docs, stemDocument)
      
      
      wordcloud(docs,min.freq = 2,colors = rainbow(4),random.order = FALSE,scale = c(3,0.5))
    })
    
    
    
    
    
    
    
    
    
    )
    
    
    
    
    
    output$plot <- renderImage({
      wc_corpus=readLines(wc_data()$datapath)
      corpus1 <- Corpus(VectorSource(wc_corpus))
      tdm1 <- TermDocumentMatrix(corpus1, control=list(stemming=TRUE))
      freq.terms1 <- findFreqTerms(tdm1, lowfreq = 2)
      png("sortie.png")
      plot(tdm1, term = freq.terms1, corThreshold = input$slider, weighting = T)
      dev.off()
      return(list(
        src = "sortie.png")
      )
      
    },deleteFile = T)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

