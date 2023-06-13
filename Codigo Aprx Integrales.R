
# Cargar el paquete en tu sesión de R
library(kableExtra)
library(tidyverse)
library(knitr)

# Definimos las funciones que vamos a integrar
f1 <- function(x) exp(-x^3)
f2 <- function(x) ifelse(x == 0, 1, (1 - cos(x))/x)

# Definimos los tamaños de muestra
n_values <- c(100, 1000, 10000, 100000)

# Calculamos las aproximaciones para cada tamaño de muestra y cada función
approximations <- tibble(
  Número_de_puntos = rep(n_values, 2),
  Función = rep(c("f1 = e^(-x^3)", "f2 = (1 - cos(x))/x"), each = length(n_values)),
  Aproximación = mapply(function(n, f) {
    x <- runif(n, 0, 1)
    if (f == "f1 = e^(-x^3)") {
      mean(f1(x))
    } else {
      mean(f2(x))
    }
  }, Número_de_puntos, Función)
)

# Calculamos los valores exactos utilizando la función integrate de R
exact_values <- tibble(
  Función = c("f1 = e^(-x^3)", "f2 = (1 - cos(x))/x"),
  Valor_verdadero = sapply(c(f1, f2), function(f) integrate(f, 0, 1)$value)
)

# Unimos los dos data frames y calculamos los errores
results <- left_join(approximations, exact_values, by = "Función") %>%
  mutate(Error = abs(Aproximación - Valor_verdadero))

# Imprimimos los resultados con formato de tabla usando kable
results %>%
  kable("html") %>%
  kable_styling("striped", full_width = F)
