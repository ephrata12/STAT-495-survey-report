# required libraries
library(shiny)
library(fmsb)
library(irr)
library(ggplot2)
library(viridis)
library(plotly)
library(tidyverse)
library(DT)
library(ggrepel)
library(wordcloud)
library(dplyr)

# dataset for radar chart
final_data <- read.csv("data/final_data.csv")



# dataset for Cohen's Kappa
responses_two_removed <- read.csv("data/cleaned_survey_responses.csv")

# datasets for word cloud
goals_word_freqs <- read_csv("data/goals_word_freqs.csv")
interest_word_freqs <- read_csv("data/interest_word_freqs.csv")
goals_bigram_freqs <- read_csv("data/goals_bigram_freqs.csv")
interest_bigram_freqs <- read_csv("data/interest_bigram_freqs.csv")

# load names 
questions <- c("goals_word_freqs", "interest_word_freqs", "goals_bigram_freqs", "interest_bigram_freqs")
questions_names <- c("goals", "interest", "goals (bigrams)", "interest (bigrams)")
names(questions) <- questions_names

# replace dots with spaces
format_colnames <- function(col_names) {
  gsub("\\.", " ", col_names)
}

# ui
ui <- fluidPage(
  titlePanel("Data Acumen Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      # show this only when Radar Chart is selected
      conditionalPanel(
        condition = "input.tabs == 'Radar Chart'",
        radioButtons("category", "Select a category:", choices = names(categories), selected = "Data Description and Visualization")
      ),
      
      # show this only when Cohen's Kappa is selected
      conditionalPanel(
        condition = "input.tabs == \"Cohen's Kappa\"",
        selectInput("topic1", "Select Topic 1:", choices = format_colnames(colnames(responses_two_removed)), multiple = FALSE),
        uiOutput("topic2_ui"),
        actionButton("calc_kappa", "Calculate Kappa")
      ),
      
      # show this table-based legend for both Radar Chart and Cohen's Kappa
      conditionalPanel(
        condition = "input.tabs == 'Radar Chart' || input.tabs == \"Cohen's Kappa\"",
        tableOutput("legendTable")
      ),
      
      # show this only when Word Cloud is selected
      conditionalPanel(
        condition = "input.tabs == 'Word Cloud'",
        selectInput("question", "Choose a question:", choices = questions),
        sliderInput("freq", "Minimum Frequency:", min = 1, max = 7, value = 2),
        sliderInput("max", "Maximum Number of Words:", min = 1, max = 100, value = 10)
      )
    ),
    
    mainPanel(
      tabsetPanel(id = "tabs",
                  tabPanel("Radar Chart",
                           plotOutput("radarPlot", height = "700px", width = "900px")
                  ),
                  tabPanel("Cohen's Kappa",
                           uiOutput("kappaResult"),
                           plotlyOutput("heatmapPlot")
                  ),
                  tabPanel("Word Cloud",
                           plotOutput("plot")
                  )
      )
    )
  )
)

# Server function to render the table
server <- function(input, output, session) {
  
  # legend for both Radar Chart and Cohen's Kappa
  output$legendTable <- renderTable({
    data.frame(
      Numeric = 0:4,
      Description = c("Unfamiliar", "Low Confidence", "Fair Confidence", 
                      "Lots of Experience - Low Confidence", 
                      "Lots of Experience - Fair Confidence")
    )
  })
  
  # radar chart plot
  output$radarPlot <- renderPlot({
    selected_columns <- categories[[input$category]]
    selected_data <- final_data[, selected_columns]
    
    # update the chart to have axis labels 0 to 4
    radarchart(selected_data, axistype = 1,
               pcol = c("blue", "purple"),
               pfcol = c(adjustcolor("blue", alpha.f = 0.5), adjustcolor("purple", alpha.f = 0.5)), plwd = 2,
               cglcol = "grey", cglty = 1, axislabcol = "black", 
               caxislabels = seq(0, 4, 1)) # axis labels from 0 to 4
    
    mtext("The radar chart illustrates the mode of students' self-reported experience and confidence levels \nacross various dimensions of data acumen.", side = 1, line = 3, cex = 1.2)
  })
  
  # reactive first topic
  first_selection <- reactive({ input$topic1 })
  
  # exclude the first topic from second topic selection
  output$topic2_ui <- renderUI({
    req(first_selection())
    selectInput("topic2", "Select Topic 2:", choices = setdiff(format_colnames(colnames(responses_two_removed)), first_selection()), multiple = FALSE)
  })
  
  # function to interpret Kappa value
  interpret_kappa <- function(kappa_value) {
    if (kappa_value < 0) { return("Poor agreement") }
    else if (kappa_value >= 0 && kappa_value <= 0.2) { return("Slight agreement") }
    else if (kappa_value > 0.2 && kappa_value <= 0.4) { return("Fair agreement") }
    else if (kappa_value > 0.4 && kappa_value <= 0.6) { return("Moderate agreement") }
    else if (kappa_value > 0.6 && kappa_value <= 0.8) { return("Substantial agreement") }
    else { return("Almost perfect agreement") }
  }
  
  # observe button click and calculate Kappa
  observeEvent(input$calc_kappa, {
    selected_topic1 <- input$topic1
    selected_topic2 <- input$topic2
    req(selected_topic1, selected_topic2)
    
    # reformat the responses from selected topics
    topic1_responses <- responses_two_removed[[gsub(" ", ".", selected_topic1)]]
    topic2_responses <- responses_two_removed[[gsub(" ", ".", selected_topic2)]]
    
    # convert the responses to numeric levels 
    label_mapping <- c("Unfamiliar with this topic" = 0, 
                       'Unfamiliar with this tool' = 0,
                       "Some experience / Low confidence" = 1, 
                       "Some experience / Fairly confident" = 2, 
                       "Lots of experience / Low confidence" = 3, 
                       "Lots of experience / Fairly confident" = 4)
    
    topic1_responses_numeric <- as.numeric(factor(topic1_responses, 
                                                  levels = names(label_mapping), 
                                                  labels = label_mapping, 
                                                  ordered = TRUE))
    topic2_responses_numeric <- as.numeric(factor(topic2_responses, 
                                                  levels = names(label_mapping), 
                                                  labels = label_mapping, 
                                                  ordered = TRUE))
    
    df <- data.frame(topic1_responses_numeric, topic2_responses_numeric)
    
    # calculate Cohen's Kappa
    kappa_value <- kappa2(df)$value
    
    # display Kappa result
    output$kappaResult <- renderUI({
      div(style = "text-align: center; font-size: 16px; font-weight: bold;",
          p(paste("Cohen's Kappa:", round(kappa_value, 3))),
          p(interpret_kappa(kappa_value))
      )
    })
    
    # plot the heatmap
    contingency_table <- table(topic1_responses_numeric, topic2_responses_numeric)
    df_table <- as.data.frame(as.table(contingency_table))
    
    output$heatmapPlot <- renderPlotly({
      p <- ggplot(df_table, aes(x = as.factor(topic1_responses_numeric), 
                                y = as.factor(topic2_responses_numeric), 
                                fill = Freq)) +
        geom_tile(color = "white") +
        scale_fill_viridis(name = "Frequency", option = "plasma") +
        labs(title = "Heatmap of Topic Agreement", 
             x = "Topic 1 Response ", 
             y = "Topic 2 Response ") +
        theme_minimal() +
        theme(axis.text.x = element_text(size = 10), 
              axis.text.y = element_text(size = 10), 
              plot.title = element_text(hjust = 0.5, size = 14))
      
      ggplotly(p, tooltip = "Freq") %>% config(displayModeBar = FALSE)
    })
  })
  
  # react to wordcloud question input to determine dataset
  varquestion <- reactive({
    switch(input$question, 
           "goals_word_freqs" = goals_word_freqs, 
           "interest_word_freqs" = interest_word_freqs, 
           "goals_bigram_freqs" = goals_bigram_freqs, 
           "interest_bigram_freqs" = interest_bigram_freqs)
  })
  
  # define the color palette
  my_palette <- viridis::viridis(100)  # or use rainbow(100) for different colors
  
  # word cloud plot
  output$plot <- renderPlot({
    varquestion() %>%
      with(wordcloud(words = word, 
                     freq = n,
                     min.freq = input$freq,
                     max.words = input$max,
                     random.order = TRUE,
                     scale = c(4, 0.5),
                     rot.per = 0.15,
                     colors = my_palette,  # using the defined palette
                     family = "sans"), 
           height = 800, width = '100%')
  })
}

# run the application
shinyApp(ui = ui, server = server)
