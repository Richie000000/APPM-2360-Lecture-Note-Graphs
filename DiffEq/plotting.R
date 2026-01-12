library(ggplot2)
library(ggvfields)
library(latex2exp)
library(glue)
library(dplyr)
library(tidyverse)

#### ---Function to create a direction field--- ####
plot_direction_field <- function(xlim, ylim, fun, equation) {
  ggplot() +
    geom_vector_field(
      show.legend = FALSE, 
      fun = fun, 
      xlim = xlim, 
      ylim = ylim, 
      L = 0.8,
      arrow = grid::arrow(length = unit(0, "npc")),
      col = "dodgerblue4",
      linewidth = 2,
    ) +
    labs(
      title = TeX(glue("Direction Field: ${equation}$")),
      x = "t",
      y = "y"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, family = "mono")
    ) +
    geom_vline(
      xintercept = 0, 
      linetype = "solid", 
      color = "black"
    ) +
    geom_hline(
      yintercept = 0, 
      linetype = "solid", 
      color = "black"
    )
}  
  
#### Define the ODE ####

# ODE: y' = y
ode_1 <- function(v) {
  t <- v[1]
  y <- v[2]
  dt <- 1
  dy <- y
  return(c(dt, dy))
}

#### Create the direction field ####

plot_direction_field(c(-5, 5), c(-3, 3), ode_1, "y' = y")

ggsave("direction_field_1.png")

#### Create functions of solution curves ####
example_solution_1 <- function(t) {
  2 * exp(t)
}

example_solution_2 <- function(t) {
  -exp(t - 2)
}

#### Add solution curves to the direction field ####

plot_direction_field(c(-5, 5), c(-3, 3), ode_1, "y' = y") +
  geom_function(
    fun = example_solution_1, 
    col = "green4",
    xlim = c(-5, log(3 / 2)),
    linewidth = 0.9
  ) +
  annotate(
    "text",
    x = 0.5,
    y = example_solution_1(0.5), 
    label = TeX("$y = 2e^t$")
  ) +
  geom_function(
    fun = example_solution_2,
    col = "dodgerblue3",
    xlim = c(-5, log(3) + 2),
    linewidth = 0.9
  ) +
  annotate(
    "text",
    x = 3.1, 
    y = example_solution_2(3.1), 
    label = TeX("$y = -e^{t - 2}$")
  )

ggsave("solution_curves_1.png")
    
