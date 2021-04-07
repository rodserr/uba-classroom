library(lpSolveAPI)

solve_lp <- function(obj_function, constrains, constrains_rhs, constrain_type, 
                     optimization_type = 'max', model_name = 'lp_model'){
  
  # obj_function: vector nombrado de los coeficientes de la funcion objetivo
  # constrains: lista nombrada con los coeficientes de las restricciones (LHS)
  # constrains_rhs; lista nombrada con los valores de las restricciones (RHS)
  # constrain_type: vector de longitud igual al numero de restricciones definiendo el tipo de restriccion '<=' '>=' '='
  # optimization_type: string definiendo el tipo de optimizacion 'max' o 'min'
  # model_name: Nombre del modelo a imprimir en los resultados
  
  # return: lista nombrada con los resultados de la optimizacion y el analisis de sensibilidad
  
  n_coef_obj <- length(obj_function)
  
  # Funcion Objetivo
  lp <- make.lp(nrow = 0, ncol = n_coef_obj)
  
  name.lp(lp, model_name)
  
  colnames(lp) <- names(obj_function)
  
  aux <- lp.control(lp, simplextype = 'primal', sense = optimization_type, pivoting = 'steepestedge')
  
  set.objfn(lp, obj = unname(obj_function))
  
  # Restricciones
  set.type(lp, columns = 1:n_coef_obj, type = 'real')
  
  for(cons in 1:length(constrains)){
    
    add.constraint(lp, xt = constrains[[cons]], type = constrain_type[[cons]], rhs = constrains_rhs[[cons]])
    
  }
  
  set.bounds(lp, lower = rep(0, n_coef_obj), upper = rep(Inf, n_coef_obj), columns = 1:n_coef_obj)
  
  rownames(lp) <- names(constrains)
  
  # Resolver y extraer resultados
  solve(lp)
  
  Var.Dec. <- matrix(get.variables(lp), 1, n_coef_obj)
  
  colnames(Var.Dec.) <- colnames(lp)
  rownames(Var.Dec.) <- 'Produccion Optima'
  
  Func.Obj. <- get.objective(lp)
  
  Resultado.lp <- list(Problema = lp, Optimo = Func.Obj., Var.Dec. = Var.Dec.)
  
  # Analisis de Sensibilidad: RHS Restricciones
  SensRHS <- matrix(NA, nrow(lp), 5)
  rownames(SensRHS) <- rownames(lp)
  colnames(SensRHS) <- c('Holgura', 'Precio Sombra', 'LI rhs', 'rhs Actual', 'LS rhs')
  
  SensRHS[, 'Holgura'] <- get.rhs(lp)-get.constraints(lp)
  SensRHS[, 'Precio Sombra'] <- get.sensitivity.rhs(lp)$duals[1:nrow(lp)]
  SensRHS[, 'rhs Actual'] <- get.rhs(lp)
  
  SensRHS[SensRHS[, 'Holgura'] == 0, 'LI rhs'] <- get.sensitivity.rhs(lp)$dualsfrom[1:nrow(lp)][SensRHS[, 'Holgura'] == 0]
  SensRHS[SensRHS[, 'Holgura'] == 0, 'LS rhs'] <- get.sensitivity.rhs(lp)$dualstill[1:nrow(lp)][SensRHS[, 'Holgura'] == 0]
  SensRHS[SensRHS[, 'Holgura'] > 0, 'LI rhs'] <- get.constraints(lp)[SensRHS[, 'Holgura'] > 0]
  SensRHS[SensRHS[, 'Holgura'] > 0, 'LS rhs'] <- Inf
  
  # Analisis de Sensibilidad: Coeficientes de la ecuacion
  SensObj <- matrix(NA, ncol(lp), 4)
  rownames(SensObj) <- colnames(lp)
  colnames(SensObj) <- c('Min.Coef.Obj.', 'Coef.Obj.', 'Max.Coef.Obj.', 'Precio Reducido')
  
  for (i in 1:ncol(lp)){
    SensObj[i, 'Coef.Obj.'] <- get.column(lp, i)$column[1]
  }
  
  SensObj[, 'Min.Coef.Obj.'] <- get.sensitivity.obj(lp)$objfrom
  SensObj[, 'Max.Coef.Obj.'] <- get.sensitivity.obj(lp)$objtill
  
  SensObj[, 'Precio Reducido'] = -get.sensitivity.rhs(lp)$duals[(nrow(lp)+1):(nrow(lp)+ncol(lp))]
  
  list(
    Resultado.lp = Resultado.lp,
    SensRHS = SensRHS,
    SensObj = SensObj
  )
  
}

# TOYCO----
obj <- c("Trenes" = 3, "Camiones" = 2, "Autos" = 5)

cons_list <- list(
  operacion_1 = c(1, 2, 1),
  operacion_2 = c(3, 0, 2),
  operacion_3 = c(1, 4, 0)
)

cons_rhs <- c(operacion_1 = 430, operacion_2 = 460, operacion_3 = 420)
cons_type <- rep('<=', 3)

solve_lp(obj, cons_list, cons_rhs, cons_type, model_name = 'TOYCO')

# Bank One----

obj <- c("Personal" = .026, "Automovil" = .0509,
         "Casa" = .0864, "Agricola" = .06875, "Comercial" = .078)

cons_list <- list(
  max_prestamo = rep(1, length(obj)),
  ag_com_benefits = c(.4, .4, .4, -.6, -.6),
  casa_benefits = c(.5, .5, -.5, 0, 0),
  bad_debts = c(.06, .03, -.01, .01, -.02)
)

cons_rhs <- c(12, 0, 0, 0)
cons_type <- rep('<=', 4)

solve_lp(obj, cons_list, cons_rhs, cons_type, model_name = 'Bank One')

# Inventario Ropa----

obj <- c(
  "Abrigos" = 30, "Camperas" = 40, "Pantalones" = 20, "Guantes" = 10,
  "Abrigos_short" = -15, "Camperas_short" = -20, "Pantalones_short" = -10, "Guantes_short" = -8
  )

cons_list <- list(
  corte = c(.30, .30, .25, .15, rep(0, 4)),
  aislamiento = c(.25, .35, .30, .10, rep(0, 4)),
  costura = c(.45, .50, .40, .22, rep(0, 4)),
  empaque = c(.15, .15, .1, .05, rep(0, 4)),
  demanda_abrigo = c(1, 0, 0, 0, 1, 0, 0, 0),
  demanda_campera = c(0, 1, 0, 0, 0, 1, 0, 0),
  demanda_pantalones = c(0, 0, 1, 0, 0, 0, 1, 0),
  demanda_guantes = c(0, 0, 0, 1, 0, 0, 0, 1)
)

cons_rhs <- c(1000, 1000, 1000, 1000, 800, 750, 600, 500)
cons_type <- c(rep('<=', 4), rep('=', 4))
  
solve_lp(obj, cons_list, cons_rhs, cons_type, model_name = 'Inventario de Ropa')

# Produccion Multi-periodo----
obj <- c(
  "Prod mes 1" = 50, "Prod mes 2" = 45, "Prod mes 3" = 55, 
  "Prod mes 4" = 48, "Prod mes 5" = 52, "Prod mes 6" = 50,
  "Remanente mes 1" = 8, "Remanente mes 2" = 8, "Remanente mes 3" = 8, 
  "Remanente mes 4" = 8, "Remanente mes 5" = 8, "Remanente mes 6" = 8
)

cons_list <- list(
  demanda_mes_1 = c(1, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0),
  demanda_mes_2 = c(0, 1, 0, 0, 0, 0, 1, -1, 0, 0, 0, 0),
  demanda_mes_3 = c(0, 0, 1, 0, 0, 0, 0, 1, -1, 0, 0, 0),
  demanda_mes_4 = c(0, 0, 0, 1, 0, 0, 0, 0, 1, -1, 0, 0),
  demanda_mes_5 = c(0, 0, 0, 0, 1, 0, 0, 0, 0, 1, -1, 0),
  demanda_mes_6 = c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, -1)
)

cons_rhs <- c(100, 250, 190, 140, 220, 110)
cons_type <- rep('=', 6)

solve_lp(obj, cons_list, cons_rhs, cons_type, optimization_type = 'min', model_name = 'Multi-periodo')
