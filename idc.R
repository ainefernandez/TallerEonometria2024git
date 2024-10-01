# Cargar las librerías necesarias
library(ggplot2)
library(dplyr)

# Configurar parámetros de simulación
set.seed(123)  # para reproducibilidad
n_simulations <- 20  # número de simulaciones
sample_size <- 30  # tamaño de la muestra en cada simulación
true_mean <- 50  # media verdadera de la distribución
true_sd <- 10  # desviación estándar verdadera de la distribución
confidence_level <- 0.95  # nivel de confianza
z_score <- qnorm((1 + confidence_level) / 2)  # Z-score para el nivel de confianza

# Función para calcular el intervalo de confianza para la media
calculate_ci <- function(sample_data) {
  sample_mean <- mean(sample_data)
  sample_std <- sd(sample_data)
  margin_of_error <- z_score * (sample_std / sqrt(length(sample_data)))
  lower_bound <- sample_mean - margin_of_error
  upper_bound <- sample_mean + margin_of_error
  return(c(lower_bound, sample_mean, upper_bound))
}

# Simular y calcular intervalos de confianza
simulations <- replicate(n_simulations, {
  sample_data <- rnorm(sample_size, mean = true_mean, sd = true_sd)
  calculate_ci(sample_data)
})

# Convertir los resultados en un data frame
ci_df <- data.frame(
  Simulation = 1:n_simulations,
  Lower = simulations[1, ],
  Mean = simulations[2, ],
  Upper = simulations[3, ]
)

# Visualizar los intervalos de confianza
ggplot(ci_df, aes(x = Simulation)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, color = "blue") +
  geom_point(aes(y = Mean), color = "red", size = 0.5) +
  geom_hline(yintercept = true_mean, linetype = "dashed", color = "black") +
  labs(title = "Intervalos al 95% de confianza para la media",
       x = "Simulación",
       y = "Media") +
  theme_minimal()

