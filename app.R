## app.R ##


# Init --------------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(tm)
library(e1071)
library(DT)
library(wordcloud)
source(file = 'helpers.R', encoding = "UTF-8")


# UI ----------------------------------------------------------------------

ui <- dashboardPage(
  skin = 'green',
  dashboardHeader(title = 'Nooki'),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Learn", tabName = "learn", icon = icon("book")),
      menuItem("Knowledge", tabName = "know", icon = icon("graduation-cap"))
    )
  ),
  dashboardBody(
    
    tabItems(

               # Home tab content
               tabItem(tabName = "home",
                       fluidPage(
                       fluidRow(h1("Welcome! My Name is Nooki")),
                       fluidRow(
                         column(6,
                                h4('I am a machine learning app for text classification.'))),
                       fluidRow(
                         column(6,
                                h4('I can learn from human knowledge and apply it automatically',
                                   icon('book')))))),
               

               # Learn tab content
               tabItem(tabName = 'learn',
                       fluidPage(
                       sidebarPanel(
                         fileInput("file1", "1. Input your learning file:"),
                         uiOutput("names"),
                         uiOutput("names2"),
                         selectInput("language", "4. Select a language",
                                     choices = list('none','danish','dutch',"english", 'finnish',
                                                    'french','german','hungarian','italian',  
                                                    'norwegian', 'portuguese', 'russian',
                                                    'spanish', 'swedish')),
                         h5(strong("5. ")),
                         actionButton("fun", "Run model"),
                         downloadButton("downModel", "Download Model"),
                         h1(),
                         fileInput("file2", "6. Input new file:"),
                         uiOutput("names3"),
                         h1(),
                         h5(strong("8. ")),
                         actionButton("predict", "Predict"),
                         h5(strong("9. ")),
                         downloadButton("downloadData","Download Data")
                       ),
                       
                       mainPanel(
                         fluidRow(
                           box(title = 'Learning File Data Preview',
                               dataTableOutput('data1'),
                               collapsible = TRUE,
                               width = 13),
                           box(title = 'Model Accuracy',
                               plotlyOutput("ggbar"),
                               collapsible = TRUE,
                               width = 13),
                           box(title = 'New File Data Preview',
                               dataTableOutput('data2'),
                               collapsible = TRUE,
                               width = 13),
                           box(title = 'Classification',
                               plotlyOutput("ggbar2"),
                               collapsible = TRUE,
                               width = 13),
                           box(title = 'Word Cloud',
                               uiOutput("CLFilter"),
                               sliderInput("freq",
                                           "Minimum Frequency:",
                                           min = 1,  max = 50, value = 15),
                               sliderInput("max",
                                           "Maximum Number of Words:",
                                           min = 1,  max = 300,  value = 100),
                               plotOutput("cloud.plot"),
                               collapsible = TRUE,
                               width = 13))
                         ))),
               
               # Knowledge tab content
               tabItem(tabName = 'know',
                       fluidPage(
                         sidebarPanel(
                           fileInput("file3", "1. Input your model file:"),
                           fileInput("file4", "2. Input new file:"),
                           uiOutput("names4"),
                           h5(strong("4. ")),
                           actionButton("predict2", "Predict"),
                           h5(strong("5. ")),
                           downloadButton("downloadData2","Download Data")
                           ),
                         
                         mainPanel(
                           box(title = 'Model General Overview',
                               verbatimTextOutput("model.char1"),
                               verbatimTextOutput("model.char2"),
                               verbatimTextOutput("model.char3"),
                               width = 13),
                           box(title = 'New Data Preview',
                               dataTableOutput('data3'),
                               collapsible = TRUE,
                               width = 13),
                           box(title = 'Classification',
                               plotlyOutput("ggbar3"),
                               collapsible = TRUE,
                               width = 13),
                           box(title = 'Word Cloud',
                               uiOutput("CLFilter2"),
                               sliderInput("freq2",
                                           "Minimum Frequency:",
                                           min = 1,  max = 50, value = 15),
                               sliderInput("max2",
                                           "Maximum Number of Words:",
                                           min = 1,  max = 300,  value = 100),
                               plotOutput("cloud.plot2"),
                               collapsible = TRUE,
                               width = 13))
                         )
                       )
               )
    )
  )


# SERVER ------------------------------------------------------------------

server <- function(input, output) {

# Learn Server ------------------------------------------------------------

  # Assign an object for the selected data
  data <- reactive({
    file1 <- input$file1
    if(is.null(file1)){return()}
    read.csv(file1$datapath, sep = ",",
             stringsAsFactors = FALSE,
             encoding = 'UTF-8')
  })
  
  output$data1 <- renderDataTable({
    if(is.null(data())){return()}
      datatable(data(), options = list(orderClasses = TRUE,
                                       scrollX = TRUE,
                                       lengthMenu = c(5, 10, 25, 50, 100)))
  })
  
  # Assign to an object the names of the columns of the selected data
  datanames <- reactive({
    if(is.null(data())){return()}
    as.list(names(data()))
  })
  
  # Creates a first filter using the names of the data
  output$names <- renderUI({
    selectInput("variable", "2. Select the text",
                datanames())
  })
  
  # Select a the variable of the data using the name selected in the first filter
  txt <- reactive({
    var1 <- input$variable
    if(is.null(var1)){return()}
    data()[,var1]
  })
  
  # Creates a second filter using the names of the data
  output$names2 <- renderUI({
    selectInput("variable2", "3. Select a classifier",
                datanames())
  })
  
  # Select the variable of the data using the name selected in the second filter
  clsfr <- reactive({
    var1 <- input$variable2
    if(is.null(var1)){return()}
    data()[,var1]
  })
  
  # create DTM
  firstdtm <- eventReactive(input$fun, {
    withProgress(message = "Preparing data", value = 0.1, {
      out <- txt() %>%
        clean.text(language = paste(input$language)) %>% 
        dtm.transform
    })
      
  })
  
  # transform a document term matrix to a data frame ready to run a model
  finalData <- reactive({
    if(is.null(firstdtm())) {return()}
    withProgress(message = "Preparing data", value = 0.3, {
      firstdtm() %>%
        as.matrix() %>% 
        data.frame()
    })
  })
  
  # create an index of the number of rows of the data
  index <- reactive({
    if(is.null(finalData())){return()}
    1:nrow(finalData())
  })
  
  # create a subset of train and test data
  testindex <- reactive({
    if(is.null(index())){return()}
    set.seed(182); sample(index(), trunc(length(index())/3))
  })
  
  testSet <- reactive({
    if(is.null(testindex())){return()}
    data.frame(finalData()[testindex(),])
  })
  
  trainSet <- reactive({
    if(is.null(testindex())){return()}
    data.frame(finalData()[-testindex(),],
               CL = as.factor(clsfr()[-testindex()]))
  })
  
  # Create an SVM model and predict into test data
  svm.model <- reactive({
    if(is.null(trainSet())){return()}
    withProgress(message = "Creating SVM Model...", value = 0.5, {
      svm(CL ~ ., data = trainSet(), cost = 10, gamma = 0.1, kernel = "sigmoid")
    })
  })
  
  svm.predict <- reactive({
    if(is.null(svm.model())){return()}
    withProgress(message = "Creating SVM Model...", value = 0.99, {
      predict(svm.model(),testSet())
    })
  })
  
  # create a Confusion Matrix 
  confusion_Matrix <- reactive({
    if(is.null(svm.predict())){return()}
    table(predict = svm.predict(), real = clsfr()[testindex()])
  })
  

  # Create a data frame comparing the real data vs the predicted classification
  # GGPLOT2 format (melt) for compare.class see helpers.R
  df <- reactive({
    if(is.null(confusion_Matrix())){return()}
    compare.class(clsfr()[testindex()], svm.predict())
  })
  
  # Calculate the accuracy of the model
  accuracy <- reactive({
    if(is.null(confusion_Matrix())){return()}
    sum(diag(confusion_Matrix())) / sum(confusion_Matrix())
  })
  
  # Create de first bar ggplot
  ggbarplot <- reactive({
    if(is.null(df())){return()}
    g <- ggplot(df(), aes(CL, Freq, fill = variable))
    g + geom_bar(stat = "identity", position = "dodge") +
      ggtitle("Real vs Model") +
      labs(x = "Class", y = "Frequency") +
      theme_bw() +
      theme(plot.title = element_text(lineheight = .8, face = "bold")) +
      ggplot2::annotate("text", label = paste("Accuracy: ", round(accuracy()*100, digits = 2), "%"),
                        x = 2,
                        y = max(df()$Freq),
                        size = 5, colour = "black")
  })
  
  output$ggbar <- renderPlotly({
    if(is.null(ggbarplot())){return()}
    ggplotly(ggbarplot())
  })
  
  output$accuracy <- reactive({
    if(is.null(accuracy())){return()}
    paste('Model accuracy: ', round(accuracy()*100, digits = 2), '%')})
  
  # Assign an object for the selected new data
  new.data <- reactive({
    file2 <- input$file2
    if(is.null(file2)){return()}
    read.csv(file2$datapath, sep = ",",
             stringsAsFactors = FALSE,
             encoding = 'UTF-8')
  })
  
  output$data2 <- renderDataTable({
    if(is.null(new.data())){return()}
    datatable(new.data(), options = list(orderClasses = TRUE,
                                         scrollX = TRUE,
                                         lengthMenu = c(5, 10, 25, 50, 100)))
  })
  
  # Assign to an object the names of the columns of the selected data
  datanames2 <- reactive({
    if(is.null(new.data())){return()}
    as.list(names(new.data()))
  })
  
  # Creates a first filter using the names of the data
  output$names3 <- renderUI({
    selectInput("variable3", "7. Select the text to classify",
                datanames2())
  })
  
  # Select a the variable of the data using the name selected in the first filter
  txt2 <- reactive({
    var3 <- input$variable3
    if(is.null(var3)){return()}
    new.data()[,var3]
  })
  
  # create DTM
  lastdtm <- eventReactive(input$predict, {
    withProgress(message = "Preparing data", value = 0.1, {
      out <- txt2() %>%
        clean.text(language = paste(input$language)) %>% 
        VectorSource %>% 
        Corpus %>% 
        DocumentTermMatrix(control = list(dictionary = names(finalData())))
    })
  })
  
  # Download model button
  output$downModel <- downloadHandler(
    filename = function() {paste0("VoltronModel",date(), ".rds")},
    content = function(file){
      write_rds(list(Text = input$variable,
                     Classifier = input$variable2,
                     Accuracy = accuracy(),
                     Language = input$language,
                     Dictionary = names(finalData()),
                     Model = svm.model()),
                file, compress = "xz")
    }
  )
  
  # transform a document term matrix to a data frame ready to predict
  lastData <- reactive({
    if(is.null(lastdtm())) {return()}
    withProgress(message = "Preparing data", value = 0.3, {
      lastdtm() %>%
        as.matrix() %>% 
        data.frame()
    })
  })
  
  
  # The final prediction with the svm model
  finalClass <- reactive({
    if(is.null(lastData())){return()}
    withProgress(message = "Predicting...", value = 0.99, {
      predict(svm.model(), lastData())
    })
  })
  
  classData <- reactive({
    if(is.null(finalClass())){return()}
    data.frame(new.data(), PREDICTION = finalClass())
  })
  
  # Download data button
  output$downloadData <- downloadHandler(
    filename = function() {paste0(input$variable2, ".csv")},
    content = function(file){
      write.csv(classData(), file)
    }
  )
  
  
  # A ggplot to visualize the results (Frequency of classification)
  ggbarplot2 <- reactive({
    if(is.null(finalClass())){return()}
    withProgress(message = "Graphing results...", value = 0.25, {
      df <- data.frame(frequency = summary(finalClass()))
    })
    withProgress(message = "Graphing results...", value = 0.5, {
      df$CL <- row.names(df)
    })
    withProgress(message = "Graphing results...", value = 0.75, {
      g <- ggplot(df, aes(CL, frequency))
    })
    withProgress(message = "Graphing results...", value = 0.99, {
      g + geom_bar(stat = "identity", fill = "orangered2") +
        ggtitle("New Classification") +
        labs(x = "Class", y = "Frequency") +
        theme_bw() +
        theme(plot.title = element_text(lineheight = .8, face = "bold"))
    })
  })
  
  output$ggbar2 <- renderPlotly({
    if(is.null(ggbarplot2())){return()}
    ggplotly(ggbarplot2())
  })
  
  # creates a filter with the classification labels
  uniqueClass <- reactive({
    if(is.null(finalClass())){return()}
    as.list(unique(paste(finalClass())))
  })
  
  classFilter <- reactive({
    if(is.null(uniqueClass())){return()}
    selectInput("class_Filter", "Select a Class",
                uniqueClass())
  })
  
  output$CLFilter <- renderUI({
    if(is.null(classFilter())){return()}
    classFilter()
  })
  
  # word cloud
  cloud.idx <- reactive({
    if(is.null(classFilter())){return()}
    grep(input$class_Filter, paste(finalClass()))
  })
  
  cloud_plot <- reactive({
    if(is.null(lastdtm())){return()}
    withProgress(message = "Creating Word Cloud...", value = .30, {
      my.data <- as.matrix(lastdtm()[cloud.idx(),])
    })
    withProgress(message = "Creating Word Cloud...", value = .60, {
      my.data <- sort(colSums(my.data), decreasing = TRUE)
    })
    withProgress(message = "Creating Word Cloud...", value = .90, {
      set.seed(182); wordcloud(names(my.data), my.data, scale = c(8,1),
                               min.freq = input$freq, max.words = input$max, random.order = FALSE,
                               rot.per=0.35, colors = brewer.pal(9, "Set1"))
    })
  })
  
  output$cloud.plot <- renderPlot({
    if(is.null(cloud_plot())){return()}
    cloud_plot()
  })
  
  # Knowledge Server --------------------------------------------------------
  
  # Assign an object for the selected data
  upModel <- reactive({
    file <- input$file3
    if(is.null(file)){return()}
    read_rds(file$datapath)
  })
  # Model variable characteristics
  output$model.char1 <- reactive({
    file <- input$file3
    if(is.null(file)){return("")}
    paste("Model used to classify", upModel()$Text, "with", upModel()$Classifier)
  })
  # Model Accuracy
  output$model.char2 <- reactive({
    file <- input$file3
    if(is.null(file)){return("")}
    paste("Accuracy:", round(upModel()$Accuracy*100, 2), "%")
  })
  # Model Language
  output$model.char3 <- reactive({
    file <- input$file3
    if(is.null(file)){return("")}
    paste("Language:", upModel()$Language)
  })
  
  # Assign an object for the selected new data
  new.data.know <- reactive({
    file4 <- input$file4
    if(is.null(file4)){return()}
    read.csv(file4$datapath, sep = ",",
             stringsAsFactors = FALSE,
             encoding = 'UTF-8')
  })
  
  output$data3 <- renderDataTable({
    if(is.null(new.data.know())){return()}
    datatable(new.data.know(), options = list(orderClasses = TRUE,
                                              scrollX = TRUE,
                                              lengthMenu = c(5, 10, 25, 50, 100)))
  })
  
  # Assign to an object the names of the columns of the selected data
  datanames3 <- reactive({
    if(is.null(new.data.know())){return()}
    as.list(names(new.data.know()))
  })
  
  # Creates a first filter using the names of the data
  output$names4 <- renderUI({
    selectInput("variable4", "3. Select the text to classify",
                datanames3())
  })
  
  # Select a the variable of the data using the name selected in the first filter
  txt3 <- reactive({
    var4 <- input$variable4
    if(is.null(var4)){return()}
    new.data.know()[,var4]
  })
  
  # create DTM
  knowdtm <- eventReactive(input$predict2, {
    withProgress(message = "Preparing data", value = 0.1, {
      out <- txt3() %>%
        clean.text(language = upModel()$Language) %>%
        VectorSource %>%
        Corpus %>%
        DocumentTermMatrix(control = list(dictionary = upModel()$Dictionary))
    })
  })
  
  # transform a document term matrix to a data frame ready to predict
  knowData <- reactive({
    if(is.null(knowdtm())) {return()}
    withProgress(message = "Preparing data", value = 0.3, {
      knowdtm() %>%
        as.matrix() %>% 
        data.frame()
    })
  })
  
  # The final prediction with the svm model
  finalClass2 <- reactive({
    if(is.null(knowData())){return()}
    withProgress(message = "Predicting...", value = 0.99, {
      predict(upModel()$Model, knowData())
    })
  })
  
  
  classData.know <- reactive({
    if(is.null(finalClass2())){return()}
    data.frame(new.data.know(), PREDICTION = finalClass2())
  })
  
  # Download data button
  output$downloadData2 <- downloadHandler(
    filename = function() {paste0(upModel()$Classifier, ".csv")},
    content = function(file){
      write.csv(classData.know(), file)
    }
  )
  
  # A ggplot to visualize the results (Frequency of classification)
  ggbarplot3 <- reactive({
    if(is.null(finalClass2())){return()}
    withProgress(message = "Graphing results...", value = 0.25, {
      df <- data.frame(frequency = summary(finalClass2()))
    })
    withProgress(message = "Graphing results...", value = 0.5, {
      df$CL <- row.names(df)
    })
    withProgress(message = "Graphing results...", value = 0.75, {
      g <- ggplot(df, aes(CL, frequency))
    })
    withProgress(message = "Graphing results...", value = 0.99, {
      g + geom_bar(stat = "identity", fill = "orangered2") +
        ggtitle("New Classification") +
        labs(x = "Class", y = "Frequency") +
        theme_bw() +
        theme(plot.title = element_text(lineheight = .8, face = "bold"))
    })
  })
  
  output$ggbar3 <- renderPlotly({
    if(is.null(ggbarplot3())){return()}
    ggplotly(ggbarplot3())
  })
  
  # creates a filter with the classification labels
  uniqueClass2 <- reactive({
    if(is.null(finalClass2())){return()}
    as.list(unique(paste(finalClass2())))
  })
  
  classFilter2 <- reactive({
    if(is.null(uniqueClass2())){return()}
    selectInput("class_Filter", "Select a Class",
                uniqueClass2())
  })
  
  output$CLFilter2 <- renderUI({
    if(is.null(classFilter2())){return()}
    classFilter2()
  })
  
  # word cloud
  cloud.idx2 <- reactive({
    if(is.null(classFilter2())){return()}
    grep(input$class_Filter, paste(finalClass2()))
  })
  
  cloud_plot2 <- reactive({
    if(is.null(knowdtm())){return()}
    withProgress(message = "Creating Word Cloud...", value = .30, {
      my.data <- as.matrix(knowdtm()[cloud.idx2(),])
    })
    withProgress(message = "Creating Word Cloud...", value = .60, {
      my.data <- sort(colSums(my.data), decreasing = TRUE)
    })
    withProgress(message = "Creating Word Cloud...", value = .90, {
      set.seed(182); wordcloud(names(my.data), my.data, scale = c(8,1),
                               min.freq = input$freq2, max.words = input$max2, random.order = FALSE,
                               rot.per=0.35, colors = brewer.pal(9, "Set1"))
    })
  })
  
  output$cloud.plot2 <- renderPlot({
    if(is.null(cloud_plot2())){return()}
    cloud_plot2()
  })
  
}

shinyApp(ui, server)