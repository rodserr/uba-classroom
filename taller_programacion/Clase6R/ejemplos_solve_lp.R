library(lpSolveAPI)
library(tidyverse)

# Ej 1----
obj_1 <- c("F" = 3, "S" = 2)

cons_list_1 <- list(
  material_1 = c(.4, .5),
  material_2 = c(0, .2),
  material_3 = c(.6, .3)
)

cons_rhs_1 <- c(material_1 = 20, material_2 = 5, material_3 = 21)
cons_type_1 <- rep('<=', 3)

solve_lp(obj_1, cons_list_1, cons_rhs_1, cons_type_1, optimization_type = 'max', model_name = 'Beneficio')

# Ej 2-----
obj_2 <- c("Bread" = 2, "Milk" = 3.5, "Cheese" = 8, "Potato" = 1.5, "Fish" = 11, "Yogurt" = 1)

cons_list_2 <- list(
  protein = c(4, 8, 7, 1.3, 8, 9.2),
  fat = c(1, 5, 9, 0.1, 7, 1),
  carbo = c(15, 11.7, 0.4, 22.6, 0, 17),
  calories = c(90, 120, 106, 97, 130, 180),
  fish_min = c(0, 0, 0, 0, 1, 0),
  milk_min = c(0, 1, 0, 0, 0, 0)
)

cons_rhs_2 <- c(protein = 10, fat = 8, carbo = 10, calories = 300, fish_min = 0.5, milk_min = 1)
cons_type_2 <- c('<=', '>=', '>=', '>=', '>=', '<=')

solve_lp(obj_2, cons_list_2, cons_rhs_2, cons_type_2, optimization_type = 'min', model_name = 'Dieta')

# Ejercicio 3----
obj_3 <- c("4p" = 30, "3p" = 40, "Bloque" = 80)

cons_list_3 <- list(
  patas = c(4, 3, -20),
  asiento = c(1, 1, -10),
  respaldo = c(1, 0, -2),
  produccion_min = c(1, 1, 0)
)

cons_rhs_3 <- c(patas = 200, asiento = 500, respaldo = 100, produccion_min = 1000)
cons_type_3 <- c('<=', '<=', '<=', '>=')

solve_lp(obj_3, cons_list_3, cons_rhs_3, cons_type_3, optimization_type = 'min', model_name = 'Dieta')

# Ej 4----
obj_4 <- c("A" = 25, "B" = 20)

cons_list_4 <- list(
  recurso = c(20, 12),
  tiempo = c(4, 4)
)

cons_rhs_4 <- c(recurso = 1800, tiempo = 8*60)
cons_type_4 <- c('<=', '<=')

solve_lp(obj_4, cons_list_4, cons_rhs_4, cons_type_4, optimization_type = 'max', model_name = 'Ganancia')
