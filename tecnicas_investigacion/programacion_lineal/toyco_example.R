library(lpSolveAPI)
library(tidyverse)

# Funcion Objetivo
TOYCO <- make.lp(nrow = 0, ncol = 3)

name.lp(TOYCO, 'TOYCO')

colnames(TOYCO) <- c("Trenes", "Camiones", "Autos")

aux <- lp.control(TOYCO, simplextype = 'primal', sense = 'max', pivoting = 'steepestedge')

set.objfn(TOYCO, obj = c(3, 2, 5))

# Restricciones
set.type(TOYCO, columns = 1:3, type = 'real')

add.constraint(TOYCO, xt = c(1, 2, 1), type = "<=", rhs = 430)
add.constraint(TOYCO, xt = c(3, 0, 2), type = "<=", rhs = 460)
add.constraint(TOYCO, xt = c(1, 4, 0), type = "<=", rhs = 420)

set.bounds(TOYCO, lower = rep(0, 3), upper = rep(Inf, 3), columns = 1:3)

rownames(TOYCO) <- c("Operacion 1", "Operacion 2", "Operacion 3")

# Resolver y extraer resultados
solve(TOYCO)

Var.Dec. <- matrix(get.variables(TOYCO), 1, 3)

colnames(Var.Dec.) <- colnames(TOYCO)
rownames(Var.Dec.) <- 'Produccion Optima'

Func.Obj. <- get.objective(TOYCO)

Resultado.TOYCO <- list(Problema = TOYCO, Optimo = Func.Obj., Var.Dec. = Var.Dec.)
rm(Func.Obj., Var.Dec.)
Resultado.TOYCO

# Analisis de Sensibilidad: RHS Restricciones
SensRHS <- matrix(NA, nrow(TOYCO), 5)
rownames(SensRHS) <- rownames(TOYCO)
colnames(SensRHS) <- c('Holgura', 'Precio Sombra', 'LI rhs', 'rhs Actual', 'LS rhs')

SensRHS[, 'Holgura'] <- get.rhs(TOYCO)-get.constraints(TOYCO)
SensRHS[, 'Precio Sombra'] <- get.sensitivity.rhs(TOYCO)$duals[1:nrow(TOYCO)]
SensRHS[, 'rhs Actual'] <- get.rhs(TOYCO)

SensRHS[SensRHS[, 'Holgura'] == 0, 'LI rhs'] <- get.sensitivity.rhs(TOYCO)$dualsfrom[1:nrow(TOYCO)][SensRHS[, 'Holgura'] == 0]
SensRHS[SensRHS[, 'Holgura'] == 0, 'LS rhs'] <- get.sensitivity.rhs(TOYCO)$dualstill[1:nrow(TOYCO)][SensRHS[, 'Holgura'] == 0]
SensRHS[SensRHS[, 'Holgura'] > 0, 'LI rhs'] <- get.constraints(TOYCO)[SensRHS[, 'Holgura'] > 0]
SensRHS[SensRHS[, 'Holgura'] > 0, 'LS rhs'] <- Inf

# Analisis de Sensibilidad: Coeficientes de la ecuacion
SensObj <- matrix(NA, ncol(TOYCO), 4)
rownames(SensObj) <- colnames(TOYCO)
colnames(SensObj) <- c('Min.Coef.Obj.', 'Coef.Obj.', 'Max.Coef.Obj.', 'Precio Reducido')

for (i in 1:ncol(TOYCO)){
  SensObj[i, 'Coef.Obj.'] <- get.column(TOYCO, i)$column[1]
}
rm(i)

SensObj[, 'Min.Coef.Obj.'] <- get.sensitivity.obj(TOYCO)$objfrom
SensObj[, 'Max.Coef.Obj.'] <- get.sensitivity.obj(TOYCO)$objtill

SensObj[, 'Precio Reducido'] = -get.sensitivity.rhs(TOYCO)$duals[(nrow(TOYCO)+1):(nrow(TOYCO)+ncol(TOYCO))]

