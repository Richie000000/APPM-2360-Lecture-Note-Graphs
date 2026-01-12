library(ggplot2)
library(ggvfields)
library(latex2exp)
library(glue)
library(dplyr)
library(tidyverse)
library(roxygen2)

#' Plot a direction field
#' 
#' Create a plot of the direction field of an ordinary differential equation
#' 
#' @param xlim the x-axis bounds of the graph; double of length 2
#' @param ylim the y-axis bounds of the graph; double of length 2
#' @param fun function for the ODE
#' @param equation string representing the ODE, written with MathJax syntax
#' 
#' @return None
#' 
#' @example 
#' 
#' example_ODE <- function(v) {
#'  t <- v[1]
#'  y <- v[2]
#'  dt <- t
#'  dy <- -y
#'  return(c(dt, dy))
#' }
#' 
#' plot_direction_field(c(-10, 10), c(-5, 5), example_ode, "y' = \frac{-y}{t}")
#' 
#' @export

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

roxygen2::roxygenise()
  
# ODE: y' = y
diff_eq <- function(v) {
  t <- v[1]
  y <- v[2]
  dt <- 1
  dy <- y
  return(c(dt, dy))
}

typeof(diff_eq)

plot_direction_field(c(-5, 5), c(-3, 3), diff_eq, "y' = y")

ggsave("direction_field_1.png")

example_solution_1 <- function(t) {
  2 * exp(t)
}

example_solution_2 <- function(t) {
  -exp(t - 2)
}

plot_direction_field(c(-5, 5), c(-3, 3), diff_eq, "y' = y") +
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
    x = 3, 
    y = example_solution_2(3), 
    label = TeX("$y = -e^{t - 2}$")
  )

ggsave("field_w_solution_curve.png")
    
