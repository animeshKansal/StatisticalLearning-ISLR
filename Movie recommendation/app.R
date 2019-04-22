library(shiny)
library(proxy)
library(recommenderlab)
library(reshape2)
library(shinydashboard)
library(wordcloud)

data("MovieLense")
RatingsMovies <- MovieLense[, colCounts(MovieLense) > 20]


search <- (MovieLenseMeta$title)


movie_recommendation <- function(m1, m2, m3, m4, m5, m6, m7, Model) {
  movieVector <-  c(m1, m2, m3, m4, m5, m6, m7)
  
  emptyVector <- na.omit(as(RatingsMovies, "matrix"))
  fillVector <- rbind(emptyVector, c(5, 4))
  fillVector[!colnames(fillVector) %in% movieVector] = NA
  fillVector = as(fillVector, "realRatingMatrix")
  
  if(Model == "ibcf")
    Model <- readRDS("ibcf.rds")
  else{
    if(Model == "ubcf")
      Model <- readRDS("ubcf.rds")
    else
      Model <- readRDS("popular.rds")
    
  }
  Predictions <-
    predict(object = Model,
            newdata = fillVector,
            n = 5)
  PredictionsMatrix <- sapply(Predictions@items,
                              function(x) {
                                colnames(RatingsMovies)[x]
                              })
  
  
  
  MovieRecommendMatrix <- factor(table(PredictionsMatrix))
  MovieRecommendOrder <-
    sort(MovieRecommendMatrix, decreasing = TRUE)
  MovieRecommendOrder <- head(MovieRecommendOrder, n = 5)
  MovieRecommendOrder <-
    data.frame(names(MovieRecommendOrder), MovieRecommendOrder)
  return(MovieRecommendOrder$names.MovieRecommendOrder.)
  
}
server <- function(input, output) {
  output$table <- renderTable({
    movie_recommendation(
      input$input_movie,
      input$input_movie2,
      input$input_movie3,
      input$input_movie4,
      input$input_movie5,
      input$input_movie6,
      input$input_movie7,
      "ibcf"
    )
    
  })
  
  
  output$table2 <- renderTable({
    movie_recommendation(
      input$input_movie,
      input$input_movie2,
      input$input_movie3,
      input$input_movie4,
      input$input_movie5,
      input$input_movie6,
      input$input_movie7,
      "ubcf"
    )
  })
  
  output$table3 <- renderTable({
    movie_recommendation(
      input$input_movie,
      input$input_movie2,
      input$input_movie3,
      input$input_movie4,
      input$input_movie5,
      input$input_movie6,
      input$input_movie7,
      "popular"
    )
  })
  
  output$wordCloud <- renderPlot(
    {
      
      if (input$input_movie == "Select Input")
        return()
      
      
      movieVector <- c(
        input$input_movie,
        input$input_movie2,
        input$input_movie3,
        input$input_movie4,
        input$input_movie5,
        input$input_movie6,
        input$input_movie7
      )
      
      MovieSubset <-  MovieLenseMeta[MovieLenseMeta$title %in% movieVector,]
      drops <- c("title", "year", "url")
      MovieSubset <-  MovieSubset[ , !(names(MovieSubset) %in% drops)]
      MovieSubset <- setNames(nm=c('genre','count'),stack(colSums(MovieSubset))[2:1])
      
      
      layout(matrix(c(1,2), nrow =2) , heights = c(1,4))
      par(mar=rep(0,4))
      plot.new()
      wordcloud(words = MovieSubset$genre,
                freq = MovieSubset$count,
                random.order = TRUE,
                random.color = TRUE,
                rot.per = 0.30,
                min.freq = 2,
                colors = brewer.pal(8, "Dark2"), 
                main = "Top genres by number of ratings")
      
    }
  )
  
  
  
  
}

MovieList <- MovieLenseMeta$title
MovieList <- c(MovieList,"Select Input")

ui = shinyUI(fluidPage(
  titlePanel("Movie Recommender System"),
  fluidRow(column(
    4,
    h3("Select Movies:"),
    wellPanel(
      selectInput("input_movie", "Movie #1", selected = "Select Input",
                  MovieList),
      selectInput("input_movie2", "Movie #2", selected = "Select Input",
                  MovieList),
      selectInput("input_movie3", "Movie #3", selected = "Select Input",
                  MovieList),
      selectInput("input_movie4", "Movie #4", selected = "Select Input",
                  MovieList),
      selectInput("input_movie5", "Movie #5", selected = "Select Input",
                  MovieList),
      selectInput("input_movie6", "Movie #6", selected = "Select Input",
                  MovieList),      
      selectInput("input_movie7", "Movie #7", selected = "Select Input",
                  MovieList),
    
      
   
      
      submitButton("Get Recommendations")
    )
  ),
  
  column(
    4,
    h3("Simmilar Movies:"),
    tableOutput("table"),    
    br(),
    h3("Users also liked:"),
    tableOutput("table2"),
    br(),
    h3("Popular Movies:"),
    tableOutput("table3")
  
  ),
  
  column(4,
    h3("Your Genre:"),
    plotOutput("wordCloud")
  ))
))
# Run the application
shinyApp(ui = ui, server = server)
