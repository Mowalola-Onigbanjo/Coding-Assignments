################################################
# Team 8 Assingment
################################################

# installing requiered packages
#install.packages("textreadr")

# library allow us to read unstructured data
library(textreadr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(stringr)
library(ggplot2)


#########################
#Importing all .txt files from one directory # a txt works like a csv file with multiple rows
#########################

file_folder="C:/Users/bmi/Downloads/otterai"                          # use the text folder location
setwd(file_folder)
nm <- list.files(path=file_folder)
# use option key in list of right click to copy path

# rbind will read the data from all the documents
my_txt_text <- do.call(rbind, lapply(nm, function(x) paste(read_document(file=x))))

#translate the load file into dataframe
mydf <- data.frame(line=1:33, text=my_txt_text )
colnames(mydf) <- c("ID","success","Q1","Q2","Q3","Q4","Q5","Q6")
mydf$ALL <- str_c(mydf$Q1,mydf$Q2,mydf$Q3,mydf$Q4, mydf$Q5, mydf$Q6,sep=" ")   # text for all questions


####################
# token
####################

# tokenizating the whole answers
token_list <- mydf %>%
  unnest_tokens(word, ALL)           
#no punctutation, no upper case letters
#print(token_list)

#word frequency
frequencies_tokens <- mydf %>%
  unnest_tokens(word, ALL) %>%
  count(word, sort=TRUE)
#print(frequencies_tokens)

# we will use the anti_join(stop_words) to remove the stop words
data(stop_words)
frequencies_tokens_nostop <- mydf%>%
  unnest_tokens(word, ALL) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  count(word, sort=TRUE)
#print(frequencies_tokens_nostop)

# creating a filter for junk words
my_junk <- data_frame(word=c("yeah","chocolate","chocolates"))


##########################
# top words 
#########################
# token frequency histograms, filtering top 15 words

word_list_f <- function(choice_q,topn){                   # word list function
  mydf %>% 
    unnest_tokens(word,choice_q)      %>%
    anti_join(stop_words)         %>%
    anti_join(my_junk)            %>%
    count(word, sort=TRUE)        %>%
    mutate(word=reorder(word, n)) %>% 
    top_n(topn)  
}

word_hist_f <- function(word_list,choice_q){                        # word histogram function
  graph <- word_list   %>%
    ggplot(aes(word, n))+
    geom_col()+
    labs(title=cat("Top words for ",choice_q))+
    xlab(NULL)+
    coord_flip()
  show(graph)
}


freq_list <- word_list_f("ALL",15)                        # top words for each question
word_hist_f(freq_list,"ALL")

Q1_freq_list <- word_list_f("Q1",15)  
word_hist_f(Q1_freq_list,"Q1")

Q2_freq_list <- word_list_f("Q2",15)  
word_hist_f(Q2_freq_list,"Q2")

Q3_freq_list <- word_list_f("Q3",15)  
word_hist_f(Q3_freq_list,"Q3")

Q4_freq_list <- word_list_f("Q4",15)  
word_hist_f(Q4_freq_list,"Q4")

Q5_freq_list <- word_list_f("Q5",15)  
word_hist_f(Q5_freq_list,"Q5")

Q6_freq_list <- word_list_f("Q6",15)  
word_hist_f(Q6_freq_list,"Q6")



####################################
# Sentiment Analysis
####################################

# function for sentiment method

sentiment_f <- function(choice_q,choice_s,topn){
  graph <- mydf %>%
    unnest_tokens(word, choice_q)          %>% 
    anti_join(stop_words)              %>%
    anti_join(my_junk)                 %>%
    inner_join(get_sentiments(choice_s)) %>%
    count(word, sentiment, sort=T)     %>%
    ungroup() %>%
    top_n(topn) %>%
    group_by(sentiment) %>%
    ungroup() %>%
    mutate(word=reorder(word, n)) %>%
    ggplot(aes(word, n, fill=sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~sentiment, scales = "free_y")+
    labs(y="Sentiment for Chocolate", x=NULL)+
    coord_flip()
  show(graph)
}

mydf_fplot_bing <-sentiment_f("ALL","bing",15)               # bing method 
mydf_fplot_nrc <- sentiment_f("ALL","nrc",15)                # nrc method




####################################
# Sentiment Cloud Analysis
####################################
library(RColorBrewer)
library(wordcloud)
library(reshape2)
wordcloud_f <- function(choice_q,choice_s,topn) {
  wordcloud <- mydf %>%
  unnest_tokens(word,choice_q) %>%
  inner_join(get_sentiments(choice_s)) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=topn, scale=c(1.5,1.5),
                   #fixed.asp =TRUE, 
                   title.size=1)
  }


# creating sentiment cloud for nrc feelings
mydf_cloud_nrc <- wordcloud_f("ALL","nrc",15)

# creating sentiment cloud for bing feelings
mydf_cloud_bing <- wordcloud_f("ALL","bing",15)


# ####################################
# # Bigrams   The sample size is too small to generate good results.
# ####################################
# 
# # filtering data for bigrams
# bigrams <- mydf %>%
#   unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
#   count(bigram, sort = TRUE) #this has many stop words, need to remove them 
# 
# 
# # separating bigram into word1 and word2
# bigrams_separated <- bigrams %>%
#   separate(bigram, c("word1", "word2"), sep = " ")%>%
#   filter(!word1 %in% stop_words$word) %>%
#   filter(!word2 %in% stop_words$word) %>%
#   filter(!word1 %in% NA) %>%
#   filter(!word2 %in% NA)  %>%
#   count(word1, word2, sort = TRUE) 
# 
# library(igraph)
# library(ggraph)
# bigram_graph <- bigrams_separated %>%
#   top_n(10)
#   graph_from_data_frame()
# ggraph(bigram_graph, layout = "fr") +
#     geom_edge_link()+
#     geom_node_point()+
#     geom_node_text(aes(label=name), vjust =1, hjust=1)
# bigram_graph



#########################
# naive bayes
#########################
library(quanteda)
library(quanteda.textmodels)
library(tm)

# split the docs into training and testing data
mydf.train <- dfm(mydf[1:33,"ALL"])
mydf.test <- dfm(mydf[33,"ALL"])

# building the Naive Bayes model: ## don't forget to change vector C for 1 for
#business success and 0 for business faliure 
c_vector <- mydf$success[1:33]
NB_classifier <- textmodel_nb(mydf.train,c_vector)
NB_classifier
summary(NB_classifier)

pred <- predict(NB_classifier, mydf.test,force = TRUE)
pred




####################################
# shiny
####################################
library(shinydashboard)
library(shiny)
library(plotly)

####### UI ######
ui <- fluidPage(
  
  # Application title
  titlePanel("TEAM 8: Survey of Customerized Chocolate"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("choose_q", "Choose Question Number 1-6: ",                                   # choose question number
                  choices=c("Q1","Q2","Q3","Q4","Q5","Q6","ALL"), selected ="ALL"),
      sliderInput("top_n", "Choose the number of words to show: ", min=1,max=40, value=15),     # choose number of words
      radioButtons("sentiment", "Choose sentiment method: ",                                    # choose sentiment
                  choices=c("BING"="bing","NRC" ="nrc"), inline = TRUE), 
      submitButton("Show Results")                                                              # show result
    ),
    
    # main panel with charts
    mainPanel(
      tabsetPanel(
        tabPanel("Graphic Analystics",                  # tab of graph
          verticalLayout(
            h2("Top words of the questions"),
            splitLayout(
              column(10,dataTableOutput('wordlist')),
              plotOutput("wordhist")),
            h2("Sentiment of the words"),
            plotOutput("sentiment"),
            h2("Word Cloud"),
            plotOutput("wordcloud")
          )
        ),
        tabPanel("Survey",                              # tab of survey
          verticalLayout(
            textInput("Q1", "What is the tastiest gift you've given or received on Valentine's Day?", value="steak with wine"),
            textInput("Q2", "If you are a chocolate, what kind will you be?", value="white chocolate with rasberry"),
            textInput("Q3", "To create an ultimate chocolate, what would you add?", value="walnuts, mashmallow and juice"),
            textInput("Q4", "To create an ultimate chocolate, what would you add?", value="walnuts, mashmallow and juice"),
            textInput("Q5", "When you feel sad, what food will cheer you up?", value="beer"),
            textInput("Q6", "Do you like to drink black coffee?", value="Yes, I keep awake only with my 2 cups of coffee daily."),
            textInput("Q7", "If we can customize the chocolat to fit your needs, Will you be interested?", value= "If you have free samples, I will try."),
            actionButton("guess","Let's guess if you will buy our chocolate."),
            textOutput("guess")
          )
        )
      )
    )
  )
)

server <- function(input, output){
  output$wordlist <- renderDataTable(word_list_f(input$choose_q,input$top_n))                   # word list
  output$wordhist <- renderPlot({word_hist_f(word_list_f(input$choose_q,input$top_n),input$choose_q)})         # word histogram
  output$sentiment <- renderPlot({sentiment_f(input$choose_q,input$sentiment, input$top_n)})    # sentiment
  output$wordcloud <- renderPlot({wordcloud_f(input$choose_q,input$sentiment, input$top_n)})    # word cloud
  
  observeEvent(input$guess,{
    user_test <- dfm(str_c(input$Q1,input$Q2,input$Q3,input$Q1,input$Q4,input$Q5,input$Q6,input$Q7,sep=" "))
    user_pred <- predict(NB_classifier, user_test, force = TRUE)
    if (user_pred ==1){
      output$guess <- renderText({"Yes, you are interested in our chocolate!"})
    }else{
      output$guess <- renderText({"No, you are not going to buy our chocolate."})
    }
  })
  
}

shinyApp(ui = ui, server = server)

