
# Cargar paquetes ---------------------------------------------------------

library(ggplot2)
library(gganimate)

# Preparar tema -----------------------------------------------------------

tema_espirales <- function() {
  theme_void() +
    theme(
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "black"),
      plot.background = element_rect(fill = "black"),
      plot.title = element_text(color = "grey60", hjust = 0.5)
    )
}

# Obtener números primos --------------------------------------------------

primos <- RcppAlgos::primeSieve(2, 86028121, 8)

# Dibujar espiral ---------------------------------------------------------

datos_polares <- function(n = NULL) {
  if (is.null(n)) {
    r <- theta <- primos
  } else {
    r <- theta <- primos[1:n]
  }
  d <-
    data.frame(
      x = r * cos(theta),
      y = r * sin(theta),
      C = r - length(r),
      n = 1:length(r),
      tiempo = max(ceiling(log(r))) - ceiling(log(r)) + 1
    )
}

# Cien elementos ----------------------------------------------------

# r <- theta <- primos[1:1e2]
# dt <- data.frame(x = r * cos(theta), y = r * sin(theta))
dt = datos_polares(1e2)
titulo = paste("Números primos;", scales::comma(length(r)), "elementos")

p_1e2 <- ggplot(datos_polares(1e2), aes(x, y)) +
  geom_point(size = 1, color = "#5dc0d7", alpha = 1) +
  labs(title = titulo) +
  tema_espirales()

ggsave("primos_1e2.png", p_1e2, device = "png", width = 11, height = 11)

# Mil ---------------------------------------------------------------------

r <- theta <- primos[1:1e3]
dt <- data.frame(x = r * cos(theta), y = r * sin(theta))
titulo = paste("Números primos;", scales::comma(length(r)), "elementos")

p_1e3 <- ggplot(dt, aes(x, y)) +
  geom_point(size = 0.25, color = "#5dc0d7", alpha = 1) +
  labs(title = titulo) +
  tema_espirales()

ggsave("primos_1e3.png", p_1e3, device = "png", width = 11, height = 11)

# Diez mil ----------------------------------------------------------------

r <- theta <- primos[1:1e4]
dt <- data.frame(x = r * cos(theta), y = r * sin(theta))
titulo = paste("Números primos;", scales::comma(length(r)), "elementos")

p_1e4 <- ggplot(dt, aes(x, y)) +
  geom_point(size = 0.1, color = "#5dc0d7", alpha = 1) +
  labs(title = titulo) +
  tema_espirales()

ggsave("primos_1e4.png", p_1e4, device = "png", width = 11, height = 11)

# Cien mil ----------------------------------------------------------------

r <- theta <- primos[1:1e5]
dt <- data.frame(x = r * cos(theta), y = r * sin(theta))
titulo <- paste("Números primos;", scales::comma(length(r)), "elementos")

p_1e5 <- ggplot(dt, aes(x, y)) +
  geom_point(size = 0.01, color = "#5dc0d7", alpha = 0.9) +
  labs(title = titulo) +
  tema_espirales()

ggsave("primos_1e5.png", p_1e5, device = "png", width = 12, height = 12)

# Un millón ---------------------------------------------------------------

r <- theta <- primos[1:1e6]
dt <- data.frame(x = r * cos(theta), y = r * sin(theta))
titulo <- paste("Números primos;", scales::comma(length(r)), "elementos")

p_1e6 <- ggplot(dt, aes(x, y)) +
  geom_point(size = 0.001, color = "#5dc0d7", alpha = 0.9) +
  labs(title = titulo) +
  tema_espirales()

ggsave("primos_1e6.png", p_1e6, device = "png", width = 12, height = 12)

# Cinco millones ----------------------------------------------------------

r <- theta <- primos
dt <- data.frame(x = r * cos(theta), y = r * sin(theta))
titulo <- paste("Números primos;", scales::comma(length(r)), "elementos")

p_5e6 <- ggplot(dt, aes(x, y)) +
  geom_point(size = 0.0001, color = "#5dc0d7", alpha = 0.9) +
  labs(title = titulo) +
  tema_espirales()

ggsave("primos_5e6.png", p_5e6, device = "png", width = 12, height = 12)

# Golden ratio ------------------------------------------------------------

# source: https://stackoverflow.com/a/28309226

golden.ratio = pi * (3 - sqrt(5))
fibonacci.angle = 360 / (golden.ratio^2)
r <- 1 * sqrt(primos[1:3e3])
t <- primos[1:3e3] * fibonacci.angle
dt <- data.frame(x = r * cos(theta), y = r * sin(theta))
titulo <- paste("Números primos con la golden ratio;",
               scales::comma(length(t)),
               "elementos")

ggplot(dt, aes(x, y)) +
  geom_point(size = 0.3, color = "gold") +
  labs(title = titulo) +
  tema_espirales()

# Animar espiral ----------------------------------------------------------

r <- theta <- primos[1:1e3]
dt <-
  data.frame(
    x = r * cos(theta),
    y = r * sin(theta),
    n = 1:length(r),
    C = max(ceiling(log(r))) - ceiling(log(r)) + 1
  )

# g_1e2 <- ggplot(dt, aes(x, y, group = seq_along(x))) +
#   geom_point(size = 1, color = "#5dc0d7", alpha = 1) +
#   tema_espirales() +
#   # Animation setting
#   labs(
#     title = paste("Números primos; {frame_along} elementos")
#   ) +
#   transition_reveal(n) +
#   ease_aes()

ggplot(dt, aes(x, y, group = seq_along(C))) +
  geom_point(size = 0.5,
             color = "#5dc0d7") +
  tema_espirales() +
  # Animation setting
  labs(title = paste("Números primos; {frame_along} elementos")) +
  transition_reveal(n) +
  view_follow()

animate(g_1e2, width = 960, height = 960)

# Versión con todos los números -------------------------------------------


is.prime <- function(n) {
  n = ifelse(!is.integer(n), as.integer(n), n)
  n = abs(n)
  if (n == 1) {
    FALSE
  } else if (n == 2L || all(n %% 2L:ceiling(sqrt(n)) != 0)) {
    TRUE
  } else {
    FALSE
  }
}

n <- 1:1e6
t <- n * 1
p <- ifelse(sapply(n, is.prime), "Primo", "No primo")

dt <- data.frame(x = t * cos(t), y = t * sin(t), p, n)

# ggplot(dt, aes(x, y, color = p)) +
#   geom_point(alpha = 0.7,
#              show.legend = F,
#              size = 0.1) +
#   scale_color_manual(values = c("#d7745d", "#5dc0d7")) +
#   coord_equal() +
#   labs(title = paste("Espirales con",
#                      scales::scientific(length(n)),
#                      "elementos")) +
#   tema_espirales()

n <- 1:1e7
p.in <- sapply(n, is.prime)




golden.ratio = (sqrt(5) + 1) / 2
fibonacci.angle = 360 / (golden.ratio ^ 2)
c = 1
num_points = 630
x = rep(0, num_points)
y = rep(0, num_points)

for (n in 1:num_points) {
  r = c * sqrt(n)
  theta = fibonacci.angle * (n)
  x[n] = r * cos(theta)
  y[n] = r * sin(theta)
}
plot(x,y,axes=FALSE,ann=FALSE,pch=19,cex=1)


# try ---------------------------------------------------------------------

r <- theta <- primos[1:1e6]
dt <- data.frame(x = r * cos(theta), y = r * sin(theta), C = r - length(r))
titulo = paste("Números primos;", scales::comma(length(r)), "elementos")

ggplot(dt, aes(x, y)) +
  geom_point(aes(size = C, alpha = C), color = "#5dc0d7", show.legend = F) +
  scale_size(range = c(0, 1)) +
  labs(title = titulo) +
  tema_espirales()

