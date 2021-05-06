###########################################################
#                TALLER DE PROGRAMACIÓN                   #
#        Especialización en Métodos Cuantitativos         #
#  para la Gestión y Análisis de Datos en Organizaciones  #
#         FACULTAD DE CIENCIAS ECONÓMICAS                 #
#            UNIVERSIDAD DE BUENOS AIRES                  #
###########################################################

## Asignatura: TALLER DE PROGRAMACIÓN
## Año Lectivo: 2021
## Docente: Rodrigo Del Rosso

rm(list = ls()) ## LIMPIAR LA CONSOLA
.rs.restartR()
gc()

################################
####### SETEO DE CARPETA #######
################################

setwd("....")  ### Setear ruta de trabajo

## CARGAR PAQUETES ##

install.packages("ompr.roi")
library(lpSolveAPI)
library(ompr)
library(magrittr)
library(ROI)
library(ROI.plugin.glpk)
library(ompr.roi)


suppressPackageStartupMessages({
  library(lpSolve)
  library(lpSolveAPI)
  library(ompr)
  library(magrittr)
  library(ROI)
  library(ROI.plugin.glpk)
  library(ompr.roi)
  library(dplyr)
  library(ggplot2)
  library(purrr)
  library(knitr)
})

###########################################################
## EJEMPLO 1: BENEFICIO MÁXIMO - RMC Inc. (ASWCM, p.236) ##
###########################################################

## Armamos los coeficientes de la función objetivo -> C
C <- c(40, 30)

# matrix(c(4,2,1,2),nrow = 2,byrow = T)

# Crear la matriz B de restricciones
A <- matrix(c(0.4, 0.5,
              0, 0.2,
              0.6, 0.3),
            nrow = 3,
            byrow = TRUE)

# Vector de lados derechos de las restricciones
B <- c(20, 5, 21)

# Dirección de las desigualdades de las restricciones
sentido_desigualdad  <- c("<=", "<=", "<=")

# Resolviendo con la función del paquete
?lp  # pedimos un help sobre la función del paquete para ver los argumentos

optimo <- lp(direction="max",
             objective.in = C,
             const.mat = A,
             const.dir = sentido_desigualdad,
             const.rhs = B,
             all.int = F)

optimo$status

# Print status: 0 = exitoso, 2 = no hay solución (esto es opcional)
print(optimo$status)

optimo$solution

# Mostrar las soluciones (la solución es una lista de objetos)
decision <- optimo$solution
names(decision) <- c("aditivo", "solvente")
print(decision)

optimo$objval

# Imprimir el valor de la función objetivo en el óptimo
print(paste("Beneficio máximo: ", optimo$objval, sep=""))

#####################################
## EJEMPLO 2: PROBLEMA DE LA DIETA ##
#####################################

# SETEAR LOS COEFICIENTES DE LA FUNCIÓN OBJETIVO
f.obj <- c(2, 3.5, 8, 1.5, 11, 1)

# SETEAR MATRIZ CORRRESPONDIENTE A LOS COEFICIENTES DE LAS RESTRICCIONES (POR FILAS)
f.con <- matrix(c(90.0, 120.0, 106.0, 97.0, 130.0, 180.0,    # CANTIDAD DE CALORÃAS
                  4.0,   8.0,   7.0,  1.3,  8.0,    9.2,     # PROTEÃNAS (GRAMOS)
                  15.0,  11.7,   0.4, 22.6,  0.0,   17.0,    # CARBOHIDRATOS (GRAMOS)
                  1.0,   5.0,   9.0,  0.1,  7.0,    1.0,     # GRASA (GRAMOS)
                  0.0,   0.0,   0.0,  0.0,  1.0,    0.0,     # PESCADO (UNIDADES)
                  0.0,   1.0,   0.0,  0.0,  0.0,    0.0),    # LECHE (UNIDADES)
                nrow = 6, byrow = TRUE)

# SETEAR LOS SIGNOS DE (DES)IGUALDADES
f.dir <- c(">=", "<=", ">=", ">=", ">=", "<=")

# SETAR LOS COEFICIENTES DEL LADO DERECHO
f.rhs <- c(300.0, 10.0, 10.0, 8.0, 0.5, 1.0)

# RESULTADOS (VALOR FINAL Z)
solucion = lp(direction = "min",
              objective.in =  f.obj,
              const.mat =  f.con,
              const.dir =  f.dir,
              const.rhs =  f.rhs)

solucion$status ## si es 0 significa que fue exitoso

print(solucion)

round(solucion$objval,1)
trunc(solucion$objval)

# VALORES FINAL DE VARIABLES
decision <- solucion$solution
names(decision) <- c("Bread","Milk","Cheese","Potato","Fish","Yogurt")
decision

# LIMPIAR LOS OBJETOS CREADOS
rm(f.obj,f.con,f.dir,f.rhs,solucion)

####################################################
## EJEMPLO 3: PRODUCCIÓN DE DOS MODELOS DE SILLAS ##
####################################################

# SETEAR LOS COEFICIENTES DE LA FUNCIÓN OBJETIVO
f.obj <- c(30, 40, 80)

# SETEAR MATRIZ CORRRESPONDIENTE A LOS COEFICIENTES DE LAS RESTRICCIONES (POR FILAS)
f.con <- matrix(c(1, 1, -10,
                  4, 3, -20,
                  1, 0, -2,
                  1, 1, 0),
                nrow=4, byrow=TRUE)

# SETEAR LOS SIGNOS DE (DES)IGUALDADES
f.dir  <- c("<=", "<=", "<=", ">=")

# SETAR LOS COEFICIENTES DEL LADO DERECHO
f.rhs <- c(500, 200, 100, 1000)

# ENCONTRAR LA SOLUCION ÓPTIMA
optimum <-  lp(direction = "min",
               objective.in = f.obj,
               const.mat = f.con,
               const.dir = f.dir,
               const.rhs = f.rhs,
               all.int = T)

# IMPRIMIR STATUS:
print(optimum$status)

# MOSTRAR LOS VALORES ÓPTIMOS PARA x_4p, x_3p and x_w
best_sol <- optimum$solution
names(best_sol) <- c("x_4p", "x_3p", "x_w")
print(best_sol)

# CHEQUEAR EL VALOR DE LA FUNCIÓN OBJETIVO EN EL ÓPTIMO
print(paste("Costo Total: ", optimum$objval, sep = ""))

## LIMPIAR LOS OBJETOS CREADOS
rm(optimum, f.dir, best_sol)

# RESOLVER EL EJEMPLO 2 MEDIANTE LA LIBRERÍA lpSolveAPI

# SETEAR 4 RESTRICCIONES Y 3 VARIABLES DE DECISIÓN
lprec <- make.lp(nrow = 4, ncol = 3)

# SETEAR EL TIPO DE PROBLEMA
lp.control(lprec, sense = "min")

# SETEAR EL TIPO DE VARIABLES DE DECISIÓN
set.type(lprec, 1:3, type = c("integer"))

# SETEAR LOS COEFICIENTES DE LA FUNCIÓN OBJETIVO
set.objfn(lprec, f.obj)

print(lprec)

f.dir  <- c("<=", "<=", "<=", ">=")

# AGREGAR RESTRICCIONES
add.constraint(lprec, f.con[1, ], f.dir[1], f.rhs[1])
add.constraint(lprec, f.con[2, ], f.dir[2], f.rhs[2])
add.constraint(lprec, f.con[3, ], f.dir[3], f.rhs[3])
add.constraint(lprec, f.con[4, ], f.dir[4], f.rhs[4])

# PODRÍA IMPLEMENTARSE CON UN LOOP
for(i in 1:4){
  add.constraint(lprec, f.con[i, ], f.dir[i], f.rhs[i])
}

# EXHIBIR LA MATRIZ DEL PROBLEMA LP A RESOLVER
print(lprec)

# RESOLVER EL PROBLEMA
solve(lprec)

# EXHIBIR LOS VALORES ÓPTIMOS DE LAS VARIABLE DE DECISIÓN
get.variables(lprec)
# EXHIBIR EL VALOR ÓPTIMO DE LA FUNCIÓN OBJETIVO
get.objective(lprec)

# NOTAR QUE LOS LÍMITES POR DEFAULT DE LAS VARIABLES DE DECISIÃ“N SON
# c(0, 0, 0) Y c(Inf, Inf, Inf)
get.bounds(lprec)

# LOS LÍMITES PUEDEN SETEARSE CON LA SIGUIENTE FUNCIÓN COMENTADA CON #
# lpSolveAPI::set.bounds()

####################################################
## EJEMPLO 4: PRODUCCIÓN DE DOS MODELOS DE SILLAS ##
####################################################

## SETEAR LOS COEFICIENTES DE LAS VARIABLES DE DECISIÓN
f.obj  <- c(25,20)

## CREAR LA MATRIZ CON LOS COEFICIENTES DE LAS RESTRICCIONES
f.con <- matrix(c(20, 12,
                  4,  4),
                nrow = 2,
                byrow = TRUE)

## DEFINIR LAS RESTRICCIONES
time_constraint <- (8 * 60)
resource_constraint <- 1800

## VALOR LÍMITE DE CADA RESTRICCIÓN
f.rhs <- c(resource_constraint, time_constraint)

## DIRECCIÓN DE LAS RESTRICCIONES
f.dir  <- c("<=", "<=")

## ENCONTRAR LA SOLUCIÓN ÓPTIMA
optimum <-  lp(direction = "max",
               f.obj,
               f.con,
               f.dir,
               f.rhs)

## MOSTRAR LOS VALORES ÓPTIMOS PARA x1 Y x2
optimum$solution

## Check the value of objective function at optimal point
optimum$objval

## LIMPIAR LOS OBJETOS CREADOS
rm(list = ls())

################
## EJEMPLO 5: ##
################

# COEFICIENTES DE LA FUNCIÓN OBJETIVO
f.obj <- c(5, 7)

# MATRIZ CORRESPONDIENTE A LOS COEFICIENTES DE LAS RESTRICCIONES POR FILAS
f.con <- matrix(c(1, 0,
                  2, 3,
                  1, 1),
                nrow = 3,
                byrow = TRUE)

# SIGNOS DE DESIGUALDADES
f.dir <- c("<=","<=","<=")

# COEFICIENTES DEL LADO DERECHO DE LAS DESIGUALDADES
f.rhs <- c(16,19,8)

# VALOR ÓPTIMO DE LA FUNCIÓN OBJETIVO
lp("max", f.obj, f.con, f.dir, f.rhs)

# VALORES ÓPTIMOS DE LAS VARIABLES DE DECISIÓN
lp("max", f.obj, f.con, f.dir, f.rhs)$solution

# SENSIBILIDADES
lp("max", f.obj, f.con, f.dir, f.rhs, compute.sens = TRUE)$sens.coef.from
lp("max", f.obj, f.con, f.dir, f.rhs, compute.sens = TRUE)$sens.coef.to

# VALORES DEL PROBLEMA DUAL

# DUALES DE LAS RESTRICCIONES Y LAS VARIABLES SON MIXTAS
lp("max", f.obj, f.con, f.dir, f.rhs, compute.sens = TRUE)$duals

# LÍMITES INFERIOR Y SUPERIOR DE LOS DUALES
lp("max", f.obj, f.con, f.dir, f.rhs, compute.sens = TRUE)$duals.from
lp("max", f.obj, f.con, f.dir, f.rhs, compute.sens = TRUE)$duals.to

############################################
## ROI: UTILIZACIÓN Y PLANTEO DE PROBLEMA ##
############################################

n <- 10
W <- 2
v <- runif(n)
w <- runif(n)

model <- MIPModel() %>%
  add_variable(x[i], i = 1:n, type = "binary") %>%
  set_objective(sum_expr(v[i] * x[i], i = 1:n)) %>%
  add_constraint(sum_expr(w[i] * x[i], i = 1:n) <= W)

model

## UTILIZAR EL OPERADOR PIPE %>% ##

MIPModel() %>%
  add_variable(x) %>%
  set_objective(x) %>%
  add_constraint(x <= 1)

## TIPOS DE VARIABLES ##

MIPModel() %>%
  add_variable(x, type = "integer") %>%
  add_variable(y, type = "continuous") %>%
  add_variable(z, type = "binary")

## LÍMITES DE VARIABLES ##

MIPModel() %>%
  add_variable(x, lb = 10) %>%
  add_variable(y, lb = 5, ub = 10)

## VARIABLES INDEXADAS ##

MIPModel() %>%
  add_variable(x[i], i = 1:10) %>%  # CREAR 10 VARIABLES DE DECISION
  set_objective(x[5]) %>%
  add_constraint(x[5] <= 10)

## SUMACIÓN DE VARIABLES ##

sum_expr(x[i], i = 1:3)

MIPModel() %>%
  add_variable(x[i], i = 1:3) %>%
  set_objective(sum_expr(x[i], i = 1:3)) %>%
  add_constraint(sum_expr(x[i], i = 1:3) <= 10)

## CUANTIFICADORES ##

MIPModel() %>%
  # CREAR VARIABLES x_{i, j} PARA TODAS LAS COMBINACIONES DE i Y j DONDE
  # i = 1:10 Y j = 1:10.
  add_variable(x[i, j], type = "binary", i = 1:10, j = 1:10) %>%

  # AGREGAR VARIABLES y_i DONDE i = 1:10 TOMANDO VALORES PARES i mod 2 = 0
  add_variable(y[i], type = "binary", i = 1:10, i %% 2 == 0) %>%

  # SE MAXIMIZARÁ TODAS LAS x_{i,j} DONDE i = j + 1
  set_objective(sum_expr(x[i, j], i = 1:10, j = 1:10, i == j + 1)) %>%

  # PARA CADA i ENTRE 1 Y 10 CON i mod 2 = 0
  # SE ADICIONA UNA RESTRICCIÓN \sum_j x_{i,j}
  add_constraint(sum_expr(x[i, j], j = 1:10) <= 1, i = 1:10, i %% 2 == 0) %>%

  # POR SUPUESTO SE PUEDEN OMITIR FILTROS O AGREGAR MÁS DE 1
  add_constraint(sum_expr(x[i, j], j = 1:10) <= 2, i = 1:10)

## LIMITES ESPECIALES SOBRE UN SUBCONJUNTO DE VARIABLES ##

MIPModel() %>%
  add_variable(x[i, j], i = 1:10, j = 1:10,
               type = "integer", lb = 0, ub = 1) %>%
  set_objective(sum_expr(x[i, j], i = 1:10, j = 1:10)) %>%
  add_constraint(x[i, i] == 0, i = 1:10) %>%
  # esto establece el ub en 0 sin agregar nuevas restricciones
  set_bounds(x[i, i], ub = 0, i = 1:10)

## PARAMETROS EXTERNOS DEL MODELO ##

n <- 5 # CANTIDAD DE NUESTRAS VARIABLES
costs <- rpois(n, lambda = 3) # VECTOR DE COSTOS
max_elements <- 3
MIPModel() %>%
  add_variable(x[i], type = "binary", i = 1:n) %>%
  set_objective(sum_expr(costs[i] * x[i], i = 1:n)) %>%
  add_constraint(sum_expr(x[i], i = 1:n) <= max_elements)

## EXTRAER SOLUCIONES DEL MODELO ##

set.seed(1)
n <- 5
weights <- matrix(rpois(n * n, 5), ncol = n, nrow = n)
result <- MIPModel() %>%
  add_variable(x[i, j], i = 1:n, j = 1:n, type = "binary") %>%
  set_objective(sum_expr(weights[i, j] * x[i, j], i = 1:n, j = 1:n)) %>%
  add_constraint(sum_expr(x[i, j], j = 1:n) == 1, i = 1:n) %>%
  solve_model(with_ROI("glpk", verbose = TRUE))

get_solution(result, x[i, j]) %>%
  dplyr::filter(value == 1)

get_solution(result, x[2, j])

#############################################
## EJEMPLO 6: ASIGNAR ESTUDIANTES A CURSOS ##
#############################################

n <- 40     ## CANTIDAD DE ESTUDIANTES
m <- 40     ## CURSOS
capacity <- rep.int(11, m) ## CADA CURSO CON IGUAL CAPACIDAD

## CADA ESTUDIANTE TIENE TRES PREFERENCIAS ##

set.seed(1234)
preference_data <- lapply(seq_len(n), function(x) sample(seq_len(m), 3))

print(preference_data)

## CREAR UNA FUNCIÓN PARA CONSULTAR LA PREFERENCIA DE CADA ESTUDIANTE
preferences <- function(student){
  preference_data[[student]]
}

## PREFERENCIAS DEL PRIMER ESTUDIANTE
preferences(13)

# LA PONDERACIÓN DE UN ESTUDIANTE ELIGIENDO UN CURSO
# SI EL CURSO NO ES PREFERIDO, SE PENALIZA LA PONDERACIÓN CON EL VALOR DE -100000

weight <- function(student, course){
  p <- which(as.numeric(course) == preferences(as.numeric(student)))
  as.integer(if(length(p) == 0){
    return(-100000)
  } else {
    return(p)
  })
}

preferences(1)

weight(1, 3)

weight(1, 23)  # esta no fue una elecciÃ³n del estudiante 1, por lo que le damos una gran penalizaciÃ³n

plot_data <- expand.grid(
  course = seq_len(m),
  weight = 1:3
) %>% rowwise() %>%
  mutate(count = sum(map_int(seq_len(n), ~weight(.x, course) == weight))) %>%
  mutate(course = factor(course), weight = factor(weight))
ggplot(plot_data, aes(x = course, y = count, fill = weight)) +
  geom_bar(stat = "identity") +
  viridis::scale_fill_viridis(discrete = TRUE) +
  geom_hline(yintercept = 11)

## MODELO ##

model <- MIPModel() %>%

  # 1 iff student i is assigned to course m
  add_variable(x[i, j], i = 1:n, j = 1:m, type = "binary") %>%

  # maximize the preferences
  set_objective(sum_expr(weight(i, j) * x[i, j], i = 1:n, j = 1:m)) %>%

  # we cannot exceed the capacity of a course
  add_constraint(sum_expr(x[i, j], i = 1:n) <= capacity[j], j = 1:m) %>%

  # each student needs to be assigned to one course
  add_constraint(sum_expr(x[i, j], j = 1:m) == 1, i = 1:n)

model

result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))


matching <- result %>%
  get_solution(x[i,j]) %>%
  filter(value > .9) %>%
  select(i, j) %>%
  rowwise() %>%
  mutate(weight = weight(as.numeric(i), as.numeric(j)),
         preferences = paste0(preferences(as.numeric(i)), collapse = ",")) %>% ungroup

DT::datatable(matching)

head(matching)

matching %>%
  group_by(weight) %>%
  summarise(count = n())

plot_data <- matching %>%
  mutate(course = factor(j), weight = factor(weight, levels = c(1, 2, 3))) %>%
  group_by(course, weight) %>%
  summarise(count = n()) %>%
  tidyr::complete(weight, fill = list(count = 0))
ggplot(plot_data, aes(x = course, y = count, fill = weight)) +
  geom_bar(stat = "identity") +
  viridis::scale_fill_viridis(discrete = TRUE) +
  geom_hline(yintercept = 11)

######################################
## EJEMPLO 7: PROBLEMA DEL VIAJANTE ##
######################################

n <- 10

max_x <- 500
max_y <- 500

set.seed(123456)
cities <- data.frame(id = 1:n, x = runif(n, max = max_x), y = runif(n, max = max_y))

ggplot(cities, aes(x, y)) +
  geom_point()

distance <- as.matrix(stats::dist(select(cities, x, y), diag = TRUE, upper = TRUE))
dist_fun <- function(i, j) {
  vapply(seq_along(i), function(k) distance[i[k], j[k]], numeric(1L))
}

## MODELO ##

model <- MIPModel() %>%
  # we create a variable that is 1 iff we travel from city i to j
  add_variable(x[i, j], i = 1:n, j = 1:n,
               type = "integer", lb = 0, ub = 1) %>%

  # a helper variable for the MTZ formulation of the tsp
  add_variable(u[i], i = 1:n, lb = 1, ub = n) %>%

  # minimize travel distance
  set_objective(sum_expr(dist_fun(i, j) * x[i, j], i = 1:n, j = 1:n), "min") %>%

  # you cannot go to the same city
  set_bounds(x[i, i], ub = 0, i = 1:n) %>%

  # leave each city
  add_constraint(sum_expr(x[i, j], j = 1:n) == 1, i = 1:n) %>%
  #
  # visit each city
  add_constraint(sum_expr(x[i, j], i = 1:n) == 1, j = 1:n) %>%

  # ensure no subtours (arc constraints)
  add_constraint(u[i] >= 2, i = 2:n) %>%
  add_constraint(u[i] - u[j] + 1 <= (n - 1) * (1 - x[i, j]), i = 2:n, j = 2:n)
model

result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))


solution <- get_solution(result, x[i, j]) %>%
  filter(value > 0)
kable(head(solution, 3))

paths <- select(solution, i, j) %>%
  rename(from = i, to = j) %>%
  mutate(trip_id = row_number()) %>%
  tidyr::gather(property, idx_val, from:to) %>%
  mutate(idx_val = as.integer(idx_val)) %>%
  inner_join(cities, by = c("idx_val" = "id"))
kable(head(arrange(paths, trip_id), 4))


ggplot(cities, aes(x, y)) +
  geom_point() +
  geom_line(data = paths, aes(group = trip_id)) +
  ggtitle(paste0("Optimal route with cost: ", round(objective_value(result), 2)))

###########################################
## EJEMPLO 8: WAREHOUSE LOCATION PROBLEM ##
###########################################

set.seed(1234)
grid_size <- 1000
n <- 100
customer_locations <- data.frame(
  id = 1:n,
  x = round(runif(n) * grid_size),
  y = round(runif(n) * grid_size)
)

m <- 20
warehouse_locations <- data.frame(
  id = 1:m,
  x = round(runif(m) * grid_size),
  y = round(runif(m) * grid_size)
)

fixedcost <- round(rnorm(m, mean = grid_size * 10, sd = grid_size * 5))

fixedcost

transportcost <- function(i, j) {
  customer <- customer_locations[i, ]
  warehouse <- warehouse_locations[j, ]
  round(sqrt((customer$x - warehouse$x)^2 + (customer$y - warehouse$y)^2))
}
transportcost(1, 3)


library(ggplot2)
p <- ggplot(customer_locations, aes(x, y)) +
  geom_point() +
  geom_point(data = warehouse_locations, color = "red", alpha = 0.5, shape = 17) +
  scale_x_continuous(limits = c(0, grid_size)) +
  scale_y_continuous(limits = c(0, grid_size)) +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(), panel.grid = element_blank())
p + ggtitle("Warehouse location problem",
            "Black dots are customers. Light red triangles show potential warehouse locations.")

model <- MIPModel() %>%
  # 1 iff i gets assigned to warehouse j
  add_variable(x[i, j], i = 1:n, j = 1:m, type = "binary") %>%

  # 1 iff warehouse j is built
  add_variable(y[j], j = 1:m, type = "binary") %>%

  # maximize the preferences
  set_objective(sum_expr(transportcost(i, j) * x[i, j], i = 1:n, j = 1:m) +
                  sum_expr(fixedcost[j] * y[j], j = 1:m), "min") %>%

  # every customer needs to be assigned to a warehouse
  add_constraint(sum_expr(x[i, j], j = 1:m) == 1, i = 1:n) %>%

  # if a customer is assigned to a warehouse, then this warehouse must be built
  add_constraint(x[i,j] <= y[j], i = 1:n, j = 1:m)
model


result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))

matching <- result %>%
  get_solution(x[i,j]) %>%
  filter(value > .9) %>%
  select(i, j)


plot_assignment <- matching %>%
  inner_join(customer_locations, by = c("i" = "id")) %>%
  inner_join(warehouse_locations, by = c("j" = "id"))
customer_count <- matching %>% group_by(j) %>% summarise(n = n()) %>% rename(id = j)
plot_warehouses <- warehouse_locations %>%
  mutate(costs = fixedcost) %>%
  inner_join(customer_count, by = "id") %>%
  filter(id %in% unique(matching$j))
p +
  geom_segment(data = plot_assignment, aes(x = x.y, y = y.y, xend = x.x, yend = y.x)) +
  geom_point(data  = plot_warehouses, color = "red", size = 3, shape = 17) +
  ggrepel::geom_label_repel(data  = plot_warehouses,
                            aes(label = paste0("fixed costs:", costs, "; customers: ", n)),
                            size = 2, nudge_y = 20) +
  ggtitle(paste0("Cost optimal warehouse locations and customer assignment"),
          "Big red triangles show warehouses that will be built, light red are unused warehouse locations.
Dots represent customers served by the respective warehouses.")

sum(fixedcost[unique(matching$j)])

###############################
####### OBJETOS CREADOS #######
###############################

rm(list=ls(all=TRUE)) ## elimina todos los objetos creados
q()
y
