library(shiny)
library(ggplot2)
library(dplyr) 

# Data to be used
dataset_csv <- read.csv("languages_dataset_cleaning.csv")

# Data filter: programming languages born between 1950 y 2023
filtered_data <- dataset_csv %>% 
  filter(appeared >= 1950 & appeared <= 2023) 

# Voy a quedarme solo con las columnas de number_of_users y github_language_repos que no son nulas para calcular los quartiles: 
vector_number_of_users <- na.omit(filtered_data$number_of_users)
vector_github_language_repos <- na.omit(filtered_data$github_language_repos)

# Reemplazar NA por "Without classification" en la columna github_language_type
filtered_data$github_language_type <- ifelse(is.na(filtered_data$github_language_type), "Without classification", filtered_data$github_language_type)
filtered_data$github_language_repos <- ifelse(is.na(filtered_data$github_language_repos), -1, filtered_data$github_language_repos)
filtered_data$number_of_users <- ifelse(is.na(filtered_data$number_of_users), -1, filtered_data$number_of_users)

# Calcula los cuartiles para number_of_users y github_language_repos
q_users <- quantile(vector_number_of_users, probs = c(0.25, 0.5, 0.75))
q_repos <- quantile(vector_github_language_repos, probs = c(0.25, 0.5, 0.75))

# # Los cuartiles están almacenados en q_users y q_repos
# # Imprimir cuartiles por consola
# cat("Cuartiles de number_of_users:", q_users, "\n")
# cat("Cuartiles de github_language_repos:", q_repos, "\n")
# cat(q_users[1],"\n")
# cat(q_users[2],"\n")
# cat(q_users[3],"\n")
# cat(q_repos[1],"\n")
# cat(q_repos[2],"\n")
# cat(q_repos[3],"\n")

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
filtered_data$category_by_users <- sapply(filtered_data$number_of_users, categorize_by_quartiles, quartiles = q_users)

# Crea una nueva columna categórica para github_language_repos
filtered_data$category_by_repos <- sapply(filtered_data$github_language_repos, categorize_by_quartiles, quartiles = q_repos)

# Obtener y visualizar los nombres de las columnas para comprobar que se hayan creado las nuevas columnas:
# cat(colnames(filtered_data),"\n")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Programming language origin by year"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "bins",
                  label = "Quantity bins:",
                  min = 1,
                  max = 73,
                  value = 10
      ),
      selectInput(inputId = "category_by_repos",
                  label = "Select category according to the number of repositories in Github:",
                  choices = unique(filtered_data$category_by_repos),
                  selected = unique(filtered_data$category_by_repos)[1]
      ),
      selectInput(inputId = "github_language_type",
                  label = "Select github language type:",
                  choices = unique(filtered_data$github_language_type),
                  selected = unique(filtered_data$github_language_type)[1]
      )
    ),
    mainPanel(
      plotOutput("distPlot")
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
      subset(filtered_data,
             github_language_type == GLTYPESelected &
               category_by_repos == CBREPOSelected
      )
    
    p <- ggplot(data = hist_data, aes(x = appeared)) +
      geom_histogram(breaks = bins, 
                     fill = 'darkgray', 
                     color = 'white',
                     binwidth = diff(bins)[1],  # Ancho de los bins
                     boundary = min(bins) - diff(bins)[1]/2) +
      xlab('Appeared year') +
      ylab('Frequency') +
      ggtitle('Histogram of programming language appeared') +
      theme_minimal() +
      scale_x_continuous(breaks = seq(min(bins), max(bins), by = 5 ))  # Establece las marcas del eje X
    
    print(p)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
