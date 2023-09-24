library(ggplot2)

# Cargar el dataset desde el archivo CSV
dataset <- read.csv("languages_dataset_cleaning.csv")

# Define la cantidad de bins que deseas en el histograma
numero_de_bins <- 10 

# Filtra los valores de 'appeared' que deseas incluir en el histograma
filtered_data <- dataset %>% 
  filter(appeared >= 1950 & appeared <= 2023)  # Reemplaza 'valor_minimo' y 'valor_maximo' por los valores deseados

# Filtra las filas donde 'appeared' no es nulo
dataset_filtrado <- filtered_data %>%
  filter(!is.na(appeared))

# Crea el histograma de frecuencia
ggplot(filtered_data, aes(x = appeared)) +
  geom_histogram(binwidth = (max(filtered_data$appeared) - min(filtered_data$appeared)) / numero_de_bins,
                 fill = "blue", 
                 color = "black") +
  labs(title = "Histograma de 'appeared'",
       x = "appeared",
       y = "Frecuencia") +
  theme_minimal()

# Crea el count plot
ggplot(filtered_data, aes(x = appeared)) +
  geom_bar() +
  labs(title = "Count Plot de 'appeared'",
       x = "appeared",
       y = "Count") +
  theme_minimal()

# Exploration

value <- length(dataset$pldb_id)

min(dataset$appeared)
max(dataset$appeared)

min(dataset$last_activity)
max(dataset$last_activity)

min(dataset$number_of_users)
max(dataset$number_of_users)

min(dataset$github_language_repos, na.rm = TRUE)
max(dataset$github_language_repos, na.rm = TRUE)

# max(lines_data$pldb_id)

# Supongamos que tienes un DataFrame llamado 'data' con las variables 'appeared' y 'pldb_id'
# Agrupa por 'appeared' y cuenta la cantidad de 'pldb_id' en cada grupo
data_summary <- dataset %>%
  group_by(appeared) %>%
  summarise(count = n())

# Crea el gráfico de líneas
ggplot(data_summary, aes(x = appeared, y = count)) +
  geom_line() +
  geom_point() +  # Agrega puntos para cada punto de datos
  labs(title = "Cantidad de pldb_id por appeared",
       x = "appeared",
       y = "Cantidad de pldb_id") +
  theme_minimal()

print(value)

# Ver las primeras filas del dataset
# head(dataset)

# Obtener y visualizar los nombres de las columnas
colnames(dataset)

# Resumen estadístico del dataset
# summary(dataset)

# Obtener la cantidad de filas y columnas
n_filas <- nrow(dataset)
n_columnas <- ncol(dataset)

# Obtener el tipo de dato en cada columna
tipos_de_dato <- sapply(dataset, class)

# Obtener la cantidad de datos nulos o NA en cada columna
datos_nulos <- sapply(dataset, function(x) sum(is.na(x)))

# Crear un resumen de la información
informacion <- data.frame(
  Cantidad_de_Filas = n_filas,
  Cantidad_de_Columnas = n_columnas,
  Tipo_de_Dato = tipos_de_dato,
  Datos_Nulos_o_NA = datos_nulos
)

# Mostrar el resumen
print(informacion)

