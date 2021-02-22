#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
bg <- readRDS("model_components/bg_short.Rds")
tg <- readRDS("model_components/tg_short.Rds")
qg <- readRDS("model_components/qg_short.Rds")

clean_line <- function(line){ 
    # This is the function I used to preprocess the corpus
    # It also has to be applied to user input of course
    line <- line %>% 
        gsub(pattern="\\s+", replacement=" ") %>%
        tolower %>%
        gsub(pattern="[^a-z ]+",replacement="") %>%
        strsplit(split=" ") %>%
        unlist
    line[line != ""]
}

last_n <- function(words, n=3){
    N <- length(words)
    take <- min(n,N) #  make sure not to take more words than there are
    words[(N-take+1):N]
}

predict_word <- function(sentence,maxn=3){
    words <- last_n(sentence,n=maxn)
    N <- length(words)
    # print(N)
    if (N == 3){ # Look if we find a quadgram
        canidates <- qg %>% filter(First == words[1],Second == words[2],Third == words[3])
        if (nrow(canidates) == 0){
            predict_word(sentence, maxn - 1)
        }
        else {
            # print("quad prediction")
            canidates$Fourth[1]
        }
    }
    else if (N == 2){ # looking for a trigram now
        canidates <- tg %>% filter(First == words[1],Second == words[2])
        if (nrow(canidates) == 0){
            predict_word(sentence, maxn - 1)
        }
        else {
            # print("tri prediction")
            canidates$Third[1]
        }
    }
    else if (N == 1){ # looking for bigram
        canidates <- bg %>% filter(First == words[1])
        if (nrow(canidates) == 0){
            predict_word(sentence, maxn - 1)
        }
        else {
            # print("bi prediction")
            canidates$Second[1]
        }
    }
    else { # return most frequent word
        # print("default prediction")
        "the"
    }
}


ui <- fluidPage(

    # Application title
    titlePanel("Text Prediction"),


    sidebarLayout(
        sidebarPanel(
            h1("Word Prediction App"),
            "Enter a sentence and see what the model predicts should come next.",
            "No need to press any buttons, the predicted word will appear after a few moments.",
            "You can also see the simplified view the model actually gets as an input.
            In particular everything is lowercase and numbers and special characters are removed."
        ),


        mainPanel(
            textInput("sentence", tags$b("Write here:"),
                      value = "Hello User!"),
            tags$b("simplified words the model sees:"), textOutput("model_view"),
            br(),
            tags$b("Next Word:"),
            textOutput("predicted_word")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$model_view <- renderText({
        clean_line(input$sentence)
    })
    output$predicted_word <- renderText({
        clean_line(input$sentence) %>% last_n %>% predict_word
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
