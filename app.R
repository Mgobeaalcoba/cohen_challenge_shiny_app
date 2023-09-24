library(shiny)
library(ggplot2)
library(dplyr)
library(openxlsx)

# Data to be used
dataset_csv <- read.csv("languages_dataset_cleaning.csv")

# Data filter: programming languages born between 1950 and 2023
filtered_data <- dataset_csv %>%
  filter(appeared >= 1950 & appeared <= 2023)

# Sort the dataset by "github_language_repos" in descending order and select the top 50 rows
top_50_languages <- head(arrange(filtered_data, desc(github_language_repos)), 50)

# Keep only the columns "number_of_users" and "github_language_repos" that are not null to calculate quartiles
vector_number_of_users <- na.omit(filtered_data$number_of_users)
vector_github_language_repos <- na.omit(filtered_data$github_language_repos)

# Replace NA with "Without classification" in the github_language_type column
filtered_data$github_language_type <- ifelse(
  is.na(filtered_data$github_language_type),
  "Without classification",
  filtered_data$github_language_type
)
filtered_data$github_language_repos <- ifelse(
  is.na(filtered_data$github_language_repos),
  -1,
  filtered_data$github_language_repos
)
filtered_data$number_of_users <- ifelse(
  is.na(filtered_data$number_of_users),
  -1,
  filtered_data$number_of_users
)

# Calculate quartiles for number_of_users and github_language_repos
q_users <- quantile(vector_number_of_users, probs = c(0.25, 0.5, 0.75))
q_repos <- quantile(vector_github_language_repos, probs = c(0.25, 0.5, 0.75))

# Function to categorize by the number of users and number of repositories
categorize_by_quartiles <- function(value, quartiles) {
  if (value < 0) {
    return("Without info")
  } else if (value <= quartiles[1]) {
    return("Low")
  } else if (value <= quartiles[2]) {
    return("Medium low")
  } else if (value <= quartiles[3]) {
    return("Medium high")
  } else {
    return("High")
  }
}

# Create a new categorical column for number_of_users
filtered_data$category_by_users <- sapply(filtered_data$number_of_users,
                                          categorize_by_quartiles,
                                          quartiles = q_users)

# Create a new categorical column for github_language_repos
filtered_data$category_by_repos <- sapply(filtered_data$github_language_repos,
                                          categorize_by_quartiles,
                                          quartiles = q_repos)


# Define UI for the application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Programming language origin by year"),
  
  # Giving a tabset appearance to the app
  tabsetPanel(
    type = "tabs",
    # Each tabPanel call specifies input for contents of tab
    tabPanel("Hist plot", # Tab title
             
             # Sidebar with a slider input for the number of bins
             sidebarLayout(
               sidebarPanel(
                 sliderInput(
                   inputId = "bins",
                   label = "Number of bins:",
                   min = 1,
                   max = 73,
                   value = 10
                 ),
                 selectInput(
                   inputId = "category_by_repos",
                   label = "Select category according to the number of repositories in Github:",
                   choices = unique(filtered_data$category_by_repos),
                   selected = unique(filtered_data$category_by_repos)[1],
                   multiple = TRUE
                 ),
                 selectInput(
                   inputId = "github_language_type",
                   label = "Select github language type:",
                   choices = unique(filtered_data$github_language_type),
                   selected = unique(filtered_data$github_language_type)[1],
                   multiple = TRUE
                 )
               ),
               mainPanel(plotOutput("distPlot"))
             )),
    tabPanel("Bar plot", # Tab title
             
             # Sidebar with a slider input for the number of bins
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "github_language_type2",
                   label = "Select github language type:",
                   choices = unique(filtered_data$github_language_type),
                   selected = unique(filtered_data$github_language_type)[1],
                   multiple = TRUE
                 ), 
                 sliderInput(
                   inputId = "appeared",
                   label = "Last activity registered by the programming language:",
                   min = min(top_50_languages$appeared),
                   max = max(top_50_languages$appeared),
                   value = max(top_50_languages$appeared),
                   step = 7
                 )
               ),
               mainPanel(plotOutput("barPlot"))
             )),
    tabPanel("About", 
             p(HTML("")),
             p(HTML("This is a Shiny Application built for a Cohen Aliados Financieros challenge by Mariano Gobea Alcoba.")),
             p(HTML("To prepare it, first select a dataset of my interest among those available in TidyTuesday and I have downloaded it to my project using the code written in download_dataset.R")),
             p(HTML("Once I had the dataset in my possession, I went through a process of debugging and cleaning it to remove junk information. The code can be observed in cleaning_dataset.R")),
             p(HTML("Then I decided to perform a brief and summarized EDA to understand my dataset, its main columns, and the number of rows, as well as the amount of null or N/A data in each of them. The code can be found in eda.R")),
             p(HTML("The code for the app is available on my <a href= https://github.com/Mgobeaalcoba/cohen_challenge_shiny_app>Github</a>")),
             p(HTML("I set out to answer the following two questions in this Shiny App:")),
             p(HTML("1. What are the 10 most used programming languages?")),
             p(HTML("2. How many programming languages were created each year? And what was the most productive year?")),
             p(HTML("</br>For any questions or inquiries, you can find me on <a href=https://www.linkedin.com/in/mariano-gobea-alcoba//> my LinkedIn profile</a> or on <a href = https://github.com/Mgobeaalcoba>my Github profile</a>.")),
             p(HTML(""))
    )
    
  ),
  # Add a download button for the CSV file
  downloadButton("downloadCSV", "Download CSV (language_dataset_cleaning.csv)")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$distPlot <- renderPlot({
    # The "Selected" variables will serve to subset our data based on user input;
    # they store the user-selected input.
    BINSelected <- input$bins
    GLTYPESelected <- input$github_language_type
    CBREPOSelected <- input$category_by_repos
    
    # Generate bins based on input$bins from ui.R
    x    <- filtered_data$appeared
    bins <- seq(min(x), max(x), length.out = BINSelected + 1)
    
    # Generate a subset for interactive filtering
    hist_data <-
      subset(
        filtered_data,
        github_language_type %in% GLTYPESelected &
          category_by_repos %in% CBREPOSelected
      )
    
    p <- ggplot(data = hist_data, 
                aes(x = appeared)) +
      geom_histogram(
        breaks = bins,
        fill = 'darkgray',
        color = 'white',
        binwidth = diff(bins)[1],
        boundary = min(bins) - diff(bins)[1] / 2
      ) +
      xlab('Appeared year') +
      ylab('Frequency') +
      ggtitle('Histogram of programming language appearances') +
      theme_minimal() +
      scale_x_continuous(breaks = seq(min(bins), max(bins), by = 5))
    
    print(p)
  })
  
  output$barPlot <- renderPlot({
    # The "Selected" variables will serve to subset our data based on user input;
    # they store the user-selected input.
    GLTYPESelected <- input$github_language_type2
    APPEAREDSelected <- input$appeared
    
    # Generate a subset for interactive filtering
    hist_data2 <-
      subset(
        top_50_languages,
        github_language_type %in% GLTYPESelected &
          appeared <= APPEAREDSelected
      )
    
    # Create a bar chart using ggplot2
    q <- ggplot(hist_data2, 
                aes(x = reorder(title, -github_language_repos), 
                    y = github_language_repos)) +
      geom_bar(stat = "identity", 
               fill = "darkgray") +
      xlab("Programming Language") +
      ylab("Number of Repositories on GitHub (in millions)") +
      ggtitle("50 Programming Languages with the Most Repositories on GitHub") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), 
            panel.background = element_rect(fill = "white")) +
      scale_y_continuous(labels = scales::comma_format(scale = 1e-6))
    
    print(q)
  })
  
  # Function to download the CSV file when the button is clicked
  output$downloadCSV <- downloadHandler(
    filename = function() {
      "language_dataset_cleaning.csv"  # Name of the downloaded file
    },
    content = function(file) {
      # Copy the file from its location in the project
      file.copy("./languages_dataset_cleaning.csv", file)
    }
  )
  
}

# Run the application
shinyApp(ui = ui, server = server)
