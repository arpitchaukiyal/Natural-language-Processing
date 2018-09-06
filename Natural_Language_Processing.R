rm(list = ls())

require("shiny")
library(shiny)
require("DT")
library(DT)
require("rvest")
library(rvest)
require("purrr")
library(purrr)
require("scales")
library(scales)
require("tm")
library(tm)
require("stringr")
library(stringr)
require("stringi")
library(stringi)
require("dplyr")
library(dplyr)
require("tidytext")
library(tidytext)
require("ggplot2")
library(ggplot2)
require("data.table")
library(data.table)
rm(list = ls())

ui <- fluidPage(
   h4(align = "right", "ArpitChaukiyal_NLP"),
   navlistPanel(widths = c(3,7),
    
                tabPanel(title = "Raw Data",
                    h4("Raw Train Data"),
                    tags$hr(),
                    dataTableOutput(outputId = "rawdatatrain"),
                    tags$hr(), tags$hr(),
                    h4("Raw Test Data"),
                    tags$hr(),
                    dataTableOutput(outputId = "rawdatatest")
                    ),
                
                tabPanel(title = "Normalized data",
                         h4("Normalize Train Data"),
                         tags$hr(),
                         dataTableOutput(outputId = "normalizetrain"),
                         tags$hr(), tags$hr(),
                         h4("Normalized Test Data"),
                         dataTableOutput(outputId = "normalizetest")
                    ),
               
                tabPanel(title = "Tagged Data",
                    h4("Tagged Train Data"),
                    tags$hr(),
                    dataTableOutput(outputId = "taggedtrain"),
                    tags$hr(), tags$hr(),
                    h4("Tagged Test Data"),
                    dataTableOutput(outputId = "taggedtest")
                   ),

                tabPanel(title = "Sentiment Score",
                    h4("Sentiment score for the Train data"),
                    tags$hr(),
                    dataTableOutput(outputId = "sentimenttrain"),
                    tags$hr(), tags$hr(),
                    h4("Sentiment score for the Test data"),
                    dataTableOutput(outputId = "sentimenttest")
                   ),
                
               tabPanel(title = "Review Summary",
                   h4("Average sentiment score"),
                   tags$hr(),
                   textOutput(outputId = "avgsentiment"),
                   tags$hr(),
                   h5("Summary Statistic for Sentiment"),
                   verbatimTextOutput(outputId = "summarysentiment"),
                   h5("Histogram for Sentiment score of each review"),
                   plotOutput(outputId = "histsentiment"),
                   tags$hr(),
                   fixedRow(sliderInput(inputId = "binset1", label = "Select the bin size",
                                        min = 5, max = 30, value = 10)),
                   
                   h4("Average Rating score"),
                   tags$hr(),
                   textOutput(outputId = "avgrating"),
                   tags$hr(),
                   h5("Summary Statistic for Rating"),
                   verbatimTextOutput(outputId = "summaryrating"),
                   h5("Histogram for Rating score of each review"),
                   plotOutput(outputId = "histrating"),

                   fixedRow(sliderInput(inputId = "binset", label = "Select the bin size",
                               min = 5, max = 30, value = 10))
                  ),
               
               tabPanel(title = "Tags Summary",
                  tabsetPanel(id = "Sentiments",
                    tabPanel(title = "Sentiments Summary",
                        h4("Average Sentiment score for Tags"),
                        tags$hr(),
                        dataTableOutput(outputId = "avgtagsentiment"),
                        tags$hr(),
                        h4("Summary Statistic for Sentiment of different Tags"),
                        
                        h5("1. Service"),
                        verbatimTextOutput(outputId = "summarysentimentservice"),
                        h5("Histogram for Sentiment score of Service reviews"),
                        plotOutput(outputId = "histsentimentservice"),
                        tags$hr(),
                        sliderInput(inputId = "binsetservice", label = "Select the bin size",
                                    min = 5, max = 30, value = 10),
                        tags$hr(),
                        # 
                        h5("2. Price"),
                        verbatimTextOutput(outputId = "summarysentimentprice"),
                        h5("Histogram for Sentiment score of Price reviews"),
                        plotOutput(outputId = "histsentimentprice"),
                        tags$hr(),
                        sliderInput(inputId = "binsetprice", label = "Select the bin size",
                                    min = 5, max = 30, value = 10),
                        tags$hr(),
                        # 
                        h5("3. Handling"),
                        verbatimTextOutput(outputId = "summarysentimenthandling"),
                        h5("Histogram for Sentiment score of Handling reviews"),
                        plotOutput(outputId = "histsentimenthandling"),
                        tags$hr(),
                        sliderInput(inputId = "binsethandling", label = "Select the bin size",
                                    min = 5, max = 30, value = 10),
                        tags$hr(),
                        # 
                        h5("4. Interior"),
                        verbatimTextOutput(outputId = "summarysentimentinterior"),
                        h5("Histogram for Sentiment score of Service reviews"),
                        plotOutput(outputId = "histsentimentinterior"),
                        tags$hr(),
                        sliderInput(inputId = "binsetinterior", label = "Select the bin size",
                                    min = 5, max = 30, value = 10),
                        tags$hr(),
                        #
                        h4("Average Sentiment score Train Data"),
                        tags$hr(),
                        textOutput(outputId = "avgsentiment1"),
                        tags$hr(),
                        h5("Summary Statistic for Sentiment"),
                        verbatimTextOutput(outputId = "summarysentiment1"),
                        h5("Histogram for Sentiment score of each review"),
                        plotOutput(outputId = "histsentiment1"),
                        tags$hr(),
                        sliderInput(inputId = "binsetsent", label = "Select the bin size",
                                    min = 5, max = 30, value = 10),
                        tags$hr()
                    ),
                        ###
                    tabPanel(title =  "Rating Summary" ,    
                        h4("Average Rating score for Tags"),
                        tags$hr(),
                        dataTableOutput(outputId = "avgtagrating"),
                        tags$hr(),
                        h5("Summary Statistic for Rating of different Tags"),
                        
                        h5("1. Service"),
                        verbatimTextOutput(outputId = "summaryratingservice"),
                        h5("Histogram for Rating score of Service reviews"),
                        plotOutput(outputId = "histratingservice"),
                        tags$hr(),
                        sliderInput(inputId = "binsetservice1", label = "Select the bin size",
                                    min = 5, max = 30, value = 10),
                        tags$hr(),
                        # 
                        h6("2. Price"),
                        verbatimTextOutput(outputId = "summaryratingprice"),
                        h5("Histogram for Rating score of Price reviews"),
                        plotOutput(outputId = "histratingprice"),
                        tags$hr(),
                        sliderInput(inputId = "binsetprice1", label = "Select the bin size",
                                    min = 5, max = 30, value = 10),
                        tags$hr(),
                        # 
                        h6("3. Handling"),
                        verbatimTextOutput(outputId = "summaryratinghandling"),
                        h5("Histogram for Sentiment score of Handling reviews"),
                        plotOutput(outputId = "histratinghandling"),
                        tags$hr(),
                        sliderInput(inputId = "binsethandling1", label = "Select the bin size",
                                    min = 5, max = 30, value = 10),
                        tags$hr(),
                        # 
                        h6("4. Interior"),
                        verbatimTextOutput(outputId = "summaryratinginterior"),
                        h5("Histogram for Sentiment score of Service reviews"),
                        plotOutput(outputId = "histratinginterior"),
                        tags$hr(),
                        sliderInput(inputId = "binsetinterior1", label = "Select the bin size",
                                    min = 5, max = 30, value = 10),
                        tags$hr(),
                        
                        ###
                        h4("Average Rating score"),
                        tags$hr(),
                        textOutput(outputId = "avgrating1"),
                        tags$hr(),
                        h5("Summary Statistic for Rating"),
                        verbatimTextOutput(outputId = "summaryrating1"),
                        h5("Histogram for Rating score of each review"),
                        plotOutput(outputId = "histrating1"),

                        fixedRow(sliderInput(inputId = "binsetrat", label = "Select the bin size",
                                             min = 5, max = 30, value = 10))
                    )
               )
            ), 
                  tabPanel(title = "Model and Prediction",
                           tags$h4("The Prediction Model for Rating"),
                           hr(),
                           verbatimTextOutput(outputId = "modelsummary"),
                           hr(),
                           h4("Prediction of the rating for 2017"),
                           hr(),
                           dataTableOutput(outputId = "predicteddata"),
                           hr(),
                           h4("Accuracy"),
                           hr(),
                           textOutput(outputId = "accuracy"),
                           hr(),
                           h4("Confusion matrix"),
                           hr(),
                           verbatimTextOutput(outputId = "confusion")
                           
                  ),
              
              tabPanel(title = "TF-IDF",
                       tags$h4("TF-IDF of Training dataset"),
                       hr(),
                      dataTableOutput(outputId = "tf_idf"),
                      hr(),
                      h4("Visualization for TF-IDF"),
                      hr(),
                      plotOutput(outputId = "tfidf")
                  )
       )
   )


server <- function(input, output){
    
    # URL for the scraping the reviews of the Toyota Camry for years 2012 to 2016.
    url_train <- 'https://www.cars.com/research/toyota-camry-201%d/consumer-reviews/?nr=500&pg=1'

    # Here the map_df function from 'purrr' library is used to generate url dynamically for years
    # 2012 t0 2015
    Camry_Train <- map_df(2:6, function(yr){
                                    page <- read_html(sprintf(url_train, yr))
                                    data.frame(Year = paste("201", yr, sep = ""),
                                               Rating = html_nodes(page, 'article')%>% html_node('cars-star-rating') %>% html_attr("rating"),
                                               Review = html_text(html_nodes(page, '.review-card-text')),
                                               stringsAsFactors = FALSE)
                                }
                          )
    # URL for the scraping the reviews of the Toyota Camry for year 2017.
    url_test <- 'https://www.cars.com/research/toyota-camry-2017/consumer-reviews/?nr=500&pg=1'

    page2<-read_html(url_test)
    Camry_Test <- data.frame(Year = "2017",
                             Rating = html_nodes(page2, 'article')%>% html_node('cars-star-rating') %>% html_attr("rating"),
                             Review = html_text(html_nodes(page2, '.review-card-text')),
                             stringsAsFactors = FALSE)
    
    
    extract_sentiment  <- function(tagged) {
        Camry_Train <- gettrainsentiments()
        return(data.frame(Tag = tagged, Mean = mean(Camry_Train[Camry_Train$Tag %like% tagged, ]$Sentiment, 
                                                    is.na = T)))
    }
    
    extract_rating  <- function(tagged) {
        Camry_Train <- gettrainsentiments()
        return(data.frame(Tag = tagged, Mean = mean(as.numeric(Camry_Train[Camry_Train$Tag %like% tagged, ]$Rating), 
                                                    is.na = T)))
    }

    
    normaltrain <- reactive(
        {
            Camry_Train <- Camry_Train %>%
                mutate(Normalized_Review = gsub('[[:punct:] ]+', ' ', Camry_Train$Review))
            # Camry_Train$Normalized_Review <- gsub('[[:punct:] ]+', ' ', Camry_Train$Review)
            Camry_Train$Normalized_Review <- tolower(Camry_Train$Normalized_Review)
            return(Camry_Train)
        }
    )
    
    normaltest <- reactive(
        {
            Camry_Test <- Camry_Test %>%
                mutate(Normalized_Review = gsub('[[:punct:] ]+', ' ', Camry_Test$Review))
            Camry_Test$Normalized_Review <- tolower(Camry_Test$Normalized_Review)
            return(Camry_Test)
        }
    )
    
    maketagtrain <- reactive(
        {
            Camry_Train <- normaltrain()
            tag<-c("service","price", "handling", "interior")
            term_regex <- paste0('(', paste(tag, collapse = '|'), ')')
            Camry_Train <- Camry_Train %>% 
                mutate(Tag = sapply(str_extract_all(Normalized_Review, term_regex),
                                    function(x) paste(unique(x), collapse=' ')))
            return(Camry_Train)
        }
    )
    
    maketagtest <- reactive(
        {
            Camry_Test <- normaltest()
            tag<-c("service","price", "handling", "interior")
            term_regex <- paste0('(', paste(tag, collapse = '|'), ')')
            Camry_Test <- Camry_Test %>%
                mutate(Tag = sapply(str_extract_all(Normalized_Review, term_regex),
                                    function(x) paste(unique(x), collapse=' ')))
            return(Camry_Test)
        }
    )
    
    gettrainsentiments <- reactive(
        {
            Camry_Train <- maketagtrain()
            Camry_Train1 <- Camry_Train %>% mutate(Line = row_number()) %>%
                            unnest_tokens(word, Normalized_Review) %>%
                            anti_join(stop_words, by = "word") %>%
                            inner_join(get_sentiments("afinn"), by = "word") %>%
                            group_by(Line) %>% mutate(Sentiment = mean(score))
            
            Camry_Train1 <- Camry_Train1 %>% group_by(Line) %>% count(Sentiment)
            Camry_Train1$n <- NULL
            
            Camry_Train <- Camry_Train %>% mutate(Line = row_number())
            Camry_Train <- Camry_Train %>% left_join(Camry_Train1, by = "Line")
            Camry_Train$Line <- NULL
            Camry_Train$Sentiment[is.na(Camry_Train$Sentiment)] <- 0
            return(Camry_Train)
        }
    )
    
    gettestsentiments <-  reactive(
        {
            Camry_Test <- maketagtest()
            Camry_Test1 <- Camry_Test %>% mutate(Line = row_number()) %>%
                unnest_tokens(word, Normalized_Review) %>%
                anti_join(stop_words, by = "word") %>%
                inner_join(get_sentiments("afinn"), by = "word") %>%
                group_by(Line) %>% mutate(Sentiment = mean(score))
            
            Camry_Test1 <- Camry_Test1 %>% group_by(Line) %>% count(Sentiment)
            Camry_Test1$n <- NULL
            
            Camry_Test <- Camry_Test %>% mutate(Line = row_number())
            Camry_Test <- Camry_Test %>% left_join(Camry_Test1, by = "Line")
            Camry_Test$Line <-  NULL
            Camry_Test$Sentiment[is.na(Camry_Test$Sentiment)] <- 0
            return(Camry_Test)
        }
    )
    
    getbin <- reactive(
        input$binset
    )
    
    getbin1 <- reactive(
        input$binset1
    )
    
    getavgsentiment <- reactive(
        {
            Camry_Train <- gettrainsentiments()
            return(mean(Camry_Train$Sentiment))
        }
    )
    
    getsummarysentiment <- reactive(
        {
            Camry_Train <- gettrainsentiments()
            return(summary(Camry_Train$Sentiment))
        }
    )
 ###   
    getavgrating <- reactive(
        {
            Camry_Train <- gettrainsentiments()
            return(mean(as.numeric(Camry_Train$Rating)))
        }
    )
    
    getsummaryrating <- reactive(
        {
            Camry_Train <- gettrainsentiments()
            return(summary(as.numeric(Camry_Train$Rating)))
        }
    ) 
       
    output$rawdatatrain <- renderDataTable(
        datatable(
            {
                return(Camry_Train)
            },
            options = list(searching = FALSE)
        )
    )
    
    ####################
    getservicedata <- reactive(
        {
            Camry_Train <- gettrainsentiments()
            Camry_Train <- Camry_Train[Camry_Train$Tag %like% "service", ]
            return(Camry_Train)
        }
    )
    
    getpricedata <- reactive(
        {
            Camry_Train <- gettrainsentiments()
            Camry_Train <- Camry_Train[Camry_Train$Tag %like% "price", ]
            return(Camry_Train)
        }
    )
    
    gethandlingdata <- reactive(
        {
            Camry_Train <- gettrainsentiments()
            Camry_Train <- Camry_Train[Camry_Train$Tag %like% "handling", ]
            return(Camry_Train)
        }
    )
    
    getinteriordata <- reactive(
        {
            Camry_Train <- gettrainsentiments()
            Camry_Train <- Camry_Train[Camry_Train$Tag %like% "interior", ]
            return(Camry_Train)
        }
    )
    
    getbinsetservice <-  reactive(
        {
            return(input$binsetservice)
        }
    )
    
    getbinsetprice <- reactive(
        {
            return(input$binsetprice)
        }
    )

    getbinsethandling <- reactive(
        {
            return(input$binsethandling)
        }
    ) 
    
    getbinsetinterior <- reactive(
        {
            return(input$binsetinterior) 
        }
    )
    
    getbinsetsent <- reactive(
        input$binsetsent
    )
    
    getbinsetrat <- reactive(
        input$binsetrat
    )
    
    
    getbinsetservice1 <-  reactive(
        {
            return(input$binsetservice1)
        }
    )
    
    getbinsetprice1 <- reactive(
        {
            return(input$binsetprice1)
        }
    )
    
    getbinsethandling1 <- reactive(
        {
            return(input$binsethandling1)
        }
    ) 
    
    getbinsetinterior1 <- reactive(
        {
            return(input$binsetinterior1) 
        }
    )
    ###################
    makemodel <- reactive(
        {
            Camry_Train <- gettrainsentiments()
            model <- glm(as.factor(Rating) ~ Sentiment, data = Camry_Train, family = binomial(link = "logit"))
            return(model)
        }
    )
    
    gettfidfdata <- reactive(
        {
            Camry_Train <- maketagtrain()
            Camry_Train1 <- Camry_Train %>% mutate(Line = row_number()) %>%
                unnest_tokens(word, Normalized_Review) %>%
                anti_join(stop_words, by = "word") %>%
                inner_join(get_sentiments("afinn"), by = "word") %>%
                group_by(Line) %>% mutate(Sentiment = mean(score))
            
            Camry_Data <- Camry_Train1
            Camry_Data <- Camry_Data[!Camry_Data$Tag == "", ] 
            Camry_Data <- Camry_Data %>% unnest_tokens(Tags1, Tag)
            Camry_Data1 <- Camry_Data[, c("word", "Tags1")]

            toyota1 <- Camry_Data1 %>% count(Tags1, word, sort = TRUE) %>% ungroup() %>%
                bind_tf_idf(word, Tags1, n) %>% arrange(desc(tf_idf))
            
            return(toyota1)
        }
    )
    
    output$rawdatatest <- renderDataTable(
        datatable(
            {
                return(Camry_Test)
            },
            options = list(searching = FALSE)
        )
    )
    
    output$normalizetrain <- renderDataTable(
        datatable(
            {
                normaltrain()
            },
            options = list(searching = FALSE)
        )
    )
    
    output$normalizetest <- renderDataTable(
        datatable(
            {
                normaltest()
            },
            options = list(searching = FALSE)
        )
    )
    
    output$taggedtrain <- renderDataTable(
        datatable(
            {
                maketagtrain()
            },
            options = list(searching = FALSE)
        )
    )
    
    output$taggedtest <- renderDataTable(
        datatable(
            {
                maketagtest()
            },
            options = list(searching = FALSE)
        )
    )
    
    output$sentimenttrain <-  renderDataTable(
        datatable(
            {
                gettrainsentiments()
            }
        )
    )
    
    output$sentimenttest <-  renderDataTable(
        datatable(
            {
                gettestsentiments()
            }
        )
    )
    
    output$avgsentiment <- renderText(
        {
            text <- paste("The average sentiment score for the training data is", as.character(getavgsentiment()))
            return(text)
        }
    )
    
    output$summarysentiment <- renderPrint(
        {
            getsummarysentiment()
        }
    )
    
   output$histsentiment <- renderPlot(
       ggplot(data = gettrainsentiments(), aes(gettrainsentiments()$Sentiment)) + 
           geom_histogram(bins = getbin1(), color = "black", fill = "red")
   )
   
   ####
   output$avgrating <- renderText(
       {
           text <- paste("The average rating score for the training data is", as.character(getavgrating()))
           return(text)
       }
   )
   
   output$summaryrating <- renderPrint(
       {
           getsummaryrating()
       }
   )
   
   output$histrating <- renderPlot(
       ggplot(data = gettrainsentiments(), aes(as.numeric(gettrainsentiments()$Rating))) + 
           geom_histogram(bins = getbin(), color = "black", fill = "red")
   )
   
   ####
   output$avgtagsentiment <- renderDataTable(
       datatable(
           {
                avgsentimentdata <- rbind(extract_sentiment("service"), 
                                            extract_sentiment("price"),
                                            extract_sentiment("interior"),
                                            extract_sentiment("handling"))
                return(avgsentimentdata)
           }
       )
   )
   
   output$avgtagrating <- renderDataTable(
       datatable(
           {
               return(rbind(extract_rating("service"),
                            extract_rating("price"),
                            extract_rating("interior"),
                            extract_rating("handling")))
           }
       )
   )
   
   output$summarysentimentservice <- renderPrint(
       {
           servicedata <- getservicedata()
           return(summary(servicedata$Sentiment))
       }
   )
   
   output$histsentimentservice <- renderPlot(
       {
           servicedata <- getservicedata()
           ggplot(data = servicedata, aes(servicedata$Sentiment)) + 
               geom_histogram(bins = getbinsetservice(), color = "black", fill = "red")
       }
   )
#### -> for price tag
   
   output$summarysentimentprice <- renderPrint(
       {
           pricedata <- getpricedata()
           return(summary(pricedata$Sentiment))
       }
   )
   
   output$histsentimentprice <- renderPlot(
       {
           pricedata <- getpricedata()
           ggplot(data = pricedata, aes(pricedata$Sentiment)) + 
               geom_histogram(bins = getbinsetprice(), color = "black", fill = "red")
       }
   )
 
### -> for handling tag
   output$summarysentimenthandling <- renderPrint(
       {
           handlingdata <- gethandlingdata()
           return(summary(handlingdata$Sentiment))
       }
   )
   
   output$histsentimenthandling <- renderPlot(
       {
           handlingdata <- gethandlingdata()
           ggplot(data = handlingdata, aes(handlingdata$Sentiment)) + 
               geom_histogram(bins = getbinsethandling(), color = "black", fill = "red")
       }
   )
   
 ### -> for interior tag
   output$summarysentimentinterior <- renderPrint(
       {
           interiordata <- getinteriordata()
           return(summary(interiordata$Sentiment))
       }
   )
   
   output$histsentimentinterior <- renderPlot(
       {
           interiordata <- getinteriordata()
           ggplot(data = interiordata, aes(interiordata$Sentiment)) + 
               geom_histogram(bins = getbinsetinterior(), color = "black", fill = "red")
       }
   )
   
 ### for training dataset
   output$avgsentiment1 <- renderText(
       {
           text <- paste("The average sentiment score for the training data is", as.character(getavgsentiment()))
           return(text)
       }
   )
   
   output$summarysentiment1 <- renderPrint(
       {
           getsummarysentiment()
       }
   )
   
   output$histsentiment1 <- renderPlot(
       ggplot(data = gettrainsentiments(), aes(gettrainsentiments()$Sentiment)) + 
           geom_histogram(bins = getbinsetsent(), color = "black", fill = "red")
   )
 
   ### for rating summary of the individual tags
   ### for service tag
   
   output$summaryratingservice <- renderPrint(
       {
           servicedata <- getservicedata()
           return(summary(as.numeric(servicedata$Rating)))
       }
   )
   
   output$histratingservice <- renderPlot(
       {
           servicedata <- getservicedata()
           ggplot(data = servicedata, aes(as.numeric(servicedata$Rating))) + 
               geom_histogram(bins = getbinsetservice1(), color = "black", fill = "red")
       }
   )
   
   ### -> for price tag  
   output$summaryratingprice <- renderPrint(
       {
           pricedata <- getpricedata()
           return(summary(as.numeric(pricedata$Rating)))
       }
   )
   
   output$histratingprice <- renderPlot(
       {
           pricedata <- getpricedata()
           ggplot(data = pricedata, aes(as.numeric(pricedata$Rating))) + 
               geom_histogram(bins = getbinsetprice1(), color = "black", fill = "red")
       }
   )
   
   ### -> for handling tag  
   output$summaryratinghandling <- renderPrint(
       {
           handlingdata <- gethandlingdata()
           return(summary(as.numeric(handlingdata$Rating)))
       }
   )
   
   output$histratinghandling <- renderPlot(
       {
           handlingdata <- gethandlingdata()
           ggplot(data = handlingdata, aes(as.numeric(handlingdata$Rating))) + 
               geom_histogram(bins = getbinsethandling1(), color = "black", fill = "red")
       }
   )
   
   ### -> for interior tag
   
   output$summaryratinginterior <- renderPrint(
       {
           interiordata <- getinteriordata()
           return(summary(as.numeric(interiordata$Rating)))
       }
   )
   
   output$histratinginterior <- renderPlot(
       {
           interiordata <- getinteriordata()
           ggplot(data = interiordata, aes(as.numeric(interiordata$Rating))) + 
               geom_histogram(bins = getbinsetinterior1(), color = "black", fill = "red")
       }
   )
 
 ### -> for the Train data 
   output$avgrating1 <- renderText(
       {
           text <- paste("The average rating score for the training data is", as.character(getavgrating()))
           return(text)
       }
   )
   
   output$summaryrating1 <- renderPrint(
       {
           getsummaryrating()
       }
   )
   
   output$histrating1 <- renderPlot(
       ggplot(data = gettrainsentiments(), aes(as.numeric(gettrainsentiments()$Rating))) + 
           geom_histogram(bins = getbinsetrat(), color = "black", fill = "red")
   )
   
   output$modelsummary <- renderPrint(
       {
           model <- makemodel()
           return(summary(model))
       }
   )
   
   output$predicteddata <- renderDataTable(
       datatable(
           {
               Camry_Train <- gettrainsentiments()
               model <- glm(as.factor(Rating) ~ Sentiment, data = Camry_Train, family = binomial(link = "logit"))
               Camry_Test <- gettestsentiments()
               modelprediction <- data.frame(Predicted_Rating = round(predict(model, Camry_Test)))
               
               #there are some variables give rating greater than 5, those variable 
               #belogs to the class = "5" hence the below code is used
               ifelse(modelprediction$Predicted_Rating > 5, as.character(5),
                                                            as.character(modelprediction$Predicted_Rating))
               return(cbind(modelprediction, Camry_Test$Rating))
               
           }
       )
   )
   
   output$tf_idf <- renderDataTable(
       datatable(
           {
               return(gettfidfdata())
           }
       )
   )

   output$tfidf <- renderPlot(
       {
            tfidfdata <- gettfidfdata()
            graph <- tfidfdata %>% 
                    group_by(Tags1) %>% 
                    top_n(10, tf_idf) %>% 
                    ungroup() %>%
                    mutate(word = reorder(word, tf_idf)) %>%
                    ggplot(aes(word, tf_idf, fill = Tags1)) +
                    geom_col(show.legend = FALSE) +
                    labs(x = NULL, y = "tf-idf") +
                    facet_wrap(~Tags1, ncol = 2, scales = "free") +
                    coord_flip()
            
            return(graph)
       }
   )
   
   output$accuracy <- renderText(
       {
           Camry_Train <- gettrainsentiments()
           model <- glm(as.factor(Rating) ~ Sentiment, data = Camry_Train, family = binomial(link = "logit"))
           Camry_Test <- gettestsentiments()
           modelprediction <- data.frame(Predicted_Rating = round(predict(model, Camry_Test)))
           
           #there are some variables give rating greater than 5, those variable 
           #belogs to the class = "5" hence the below code is used
           ifelse(modelprediction$Predicted_Rating > 5, 5,
                  as.numeric(modelprediction$Predicted_Rating))
           matrx <- table(x =cbind(modelprediction, y = as.numeric(Camry_Test$Rating)))
           accuracy <-sum(diag(matrx)/sum(matrx))
           
           return(paste("The Accuracy of the model is (as calculated from the confusion matrix):",
                        as.character(accuracy)))
       }
   )
   
   output$confusion <- renderPrint(
       {
           Camry_Train <- gettrainsentiments()
           model <- glm(as.factor(Rating) ~ Sentiment, data = Camry_Train, family = binomial(link = "logit"))
           Camry_Test <- gettestsentiments()
           modelprediction <- data.frame(Predicted_Rating = round(predict(model, Camry_Test)))
           
           #there are some variables give rating greater than 5, those variable 
           #belogs to the class = "5" hence the below code is used
           ifelse(modelprediction$Predicted_Rating > 5, 5,
                  as.numeric(modelprediction$Predicted_Rating))
           matrx <- table(x =cbind(modelprediction, y = as.numeric(Camry_Test$Rating)))
           accuracy <-sum(diag(matrx)/sum(matrx))
           return(print(matrx))
       }
   )
}

shinyApp(ui = ui, server = server)
