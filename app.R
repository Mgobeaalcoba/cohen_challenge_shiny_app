library(shiny)
library(ggplot2)
library(dplyr)

# Data to be used
dataset_csv <- read.csv("languages_dataset_cleaning.csv")

# Data filter: programming languages born between 1950 y 2023
filtered_data <- dataset_csv %>%
  filter(appeared >= 1950 & appeared <= 2023)

# Ordena el conjunto de datos por "github_language_repos" en orden descendente y selecciona las 20 primeras filas
top_50_languages <- head(arrange(filtered_data, desc(github_language_repos)), 50)

# Voy a quedarme solo con las columnas de number_of_users y github_language_repos que no son nulas para calcular los quartiles:
vector_number_of_users <- na.omit(filtered_data$number_of_users)
vector_github_language_repos <-
  na.omit(filtered_data$github_language_repos)

# Reemplazar NA por "Without classification" en la columna github_language_type
filtered_data$github_language_type <-
  ifelse(
    is.na(filtered_data$github_language_type),
    "Without classification",
    filtered_data$github_language_type
  )
filtered_data$github_language_repos <-
  ifelse(
    is.na(filtered_data$github_language_repos),
    -1,
    filtered_data$github_language_repos
  )
filtered_data$number_of_users <-
  ifelse(is.na(filtered_data$number_of_users),
         -1,
         filtered_data$number_of_users)

# Calcula los cuartiles para number_of_users y github_language_repos
q_users <-
  quantile(vector_number_of_users, probs = c(0.25, 0.5, 0.75))
q_repos <-
  quantile(vector_github_language_repos, probs = c(0.25, 0.5, 0.75))


# Función para categorizar por cantidad de usuarios y cantidad de repos
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

# Crea una nueva columna categórica para number_of_users
filtered_data$category_by_users <-
  sapply(filtered_data$number_of_users,
         categorize_by_quartiles,
         quartiles = q_users)

# Crea una nueva columna categórica para github_language_repos
filtered_data$category_by_repos <-
  sapply(filtered_data$github_language_repos,
         categorize_by_quartiles,
         quartiles = q_repos)

# Obtener y visualizar los nombres de las columnas para comprobar que se hayan creado las nuevas columnas:
# cat(colnames(filtered_data),"\n")

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Programming language origin by year"),
  
  #Giving a tabset appearance to the app
  tabsetPanel(
    type = "tabs",
    #Each tabPanel call specifies input for contents of tab
    tabPanel("Hist plot", #Tab title
             
             # Sidebar with a slider input for number of bins
             sidebarLayout(
               sidebarPanel(
                 sliderInput(
                   inputId = "bins",
                   label = "Quantity bins:",
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
    tabPanel("Bar plot", #Tab title
             
             # Sidebar with a slider input for number of bins
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
             p(HTML("This is a Shiny Application built to a Cohen Aliados Financieros's challenge by Mariano Gobea Alcoba.")),
             p(HTML("To prepare it, first select a dataset of my interest among those available in TidyTuesday and I have downloaded it to my project using the code written in download_dataset.R")),
             p(HTML("Once I had the dataset in my possession, I went through a process of debugging and cleaning it in order to remove junk information. The mime can be observed in cleaning_dataset.R")),
             p(HTML("Then I decided to do a brief and summarized EDA in order to know my dataset, its main columns and number of rows, how much null or n/a data I had in each of them. It can be found in eda.R")),
             p(HTML("Code for the app is available on my <a href= https://github.com/Mgobeaalcoba/cohen_challenge_shiny_app>Github</a>")),
             p(HTML("I set out to answer the following two questions in this Shiny App:")),
             p(HTML("1. What are the 10 most used programming languages?")),
             p(HTML("2. How many programming languages were created each year? And what was the most fruitful year?")),
             p(HTML("</br>For any questions or inquiries, you can find me at <a href=https://www.linkedin.com/in/mariano-gobea-alcoba//> my LinkedIn profile</a> or on <a href = https://github.com/Mgobeaalcoba>my Github profile</a>."))
    )
    
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  output$distPlot <- renderPlot({
    #The "Selected" variables will serve to subset out data in function of
    # the input: they are a way of storing the input selected
    BINSelected <- input$bins
    GLTYPESelected <- input$github_language_type
    CBREPOSelected <- input$category_by_repos
    
    # generate bins based on input$bins from ui.R
    x    <- filtered_data$appeared
    bins <- seq(min(x), max(x), length.out = BINSelected + 1)
    
    # generate a subset for interactive filter:
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
      ggtitle('Histogram of programming language appeared') +
      theme_minimal() +
      scale_x_continuous(breaks = seq(min(bins), max(bins), by = 5))  # Establece las marcas del eje X
    
    print(p)
  })
  
  output$barPlot <- renderPlot({
    #The "Selected" variables will serve to subset out data in function of
    # the input: they are a way of storing the input selected
    GLTYPESelected <- input$github_language_type2
    APPEAREDSelected <- input$appeared
    
    # generate a subset for interactive filter:
    hist_data2 <-
      subset(
        top_50_languages,
        github_language_type %in% GLTYPESelected &
          appeared <= APPEAREDSelected
      )
    
    # Crear un gráfico de barras utilizando ggplot2
    q <- ggplot(hist_data2, 
                aes(x = reorder(title, -github_language_repos), 
                    y = github_language_repos)) +
      geom_bar(stat = "identity", 
               fill = "darkgray") +
      xlab("Lenguaje de Programación") +
      ylab("Número de Repositorios en GitHub (en millones)") +
      ggtitle("50 Lenguajes de Programación con más Repositorios en GitHub") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), 
            panel.background = element_rect(fill = "white")) +  # Rotar etiquetas del eje x si es necesario  # Rotar etiquetas del eje x si es necesario
      scale_y_continuous(labels = scales::comma_format(scale = 1e-6)) 
    
    
    print(q)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
