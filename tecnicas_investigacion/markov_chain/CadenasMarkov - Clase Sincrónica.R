# __________________________________________________________________________________________________
# E72: Especializacion en Metodos Cuantitativos         ____________________________________________
# para la Gestion y Analisis de Datos en Organizaciones ____________________________________________
# __________________________________________________________________________________________________
# TECNICAS DE INVESTIGACION OPERATIVA PARA LA GESTION ______________________________________________
# __________________________________________________________________________________________________
# Resolucion de ejemplos de Cadena de Markov usando los paquetes "markovchain" y "diagram" _________
# __________________________________________________________________________________________________
# Preparado por R. Darío Bacchini __________________________________________________________________
# __________________________________________________________________________________________________
# Cargo paquetes -----------------------------------------------------------------------------------
  library(markovchain)
  library(diagram)
# __________________________________________________________________________________________________
# Ejemplo Frio/Calor -------------------------------------------------------------------------------
  rm(list=ls()); graphics.off()
  
  # Creo la matriz de transicion 
    estados<- c("Calor", "Frio")
    MatrizTrans<- matrix(c(0.60, 0.40,
                           0.30, 0.70), 
                         byrow=T, ncol=2)
    colnames(MatrizTrans) = rownames(MatrizTrans) = estados
    MatrizTrans
  
  # Creo objeto Markov chain
    FrioCalor <- new("markovchain",transitionMatrix = MatrizTrans)
    
  # Creo diagrama de transición
    plotmat(t(FrioCalor@transitionMatrix),
            box.type = 'diamond', shadow.size = 0, txt.col = "blue",txt.font = 4,
            box.lwd = 1.55,box.size = 0.08,box.col = 'light yellow',box.cex = 0.75,
            arr.col = 'red',arr.width = 0.2, arr.length = 0.25, 
            arr.type = 'triangle',arr.lwd = 1.2, arr.lcol = 'red',
            cex.txt = 0.8, curve=0.05,
            self.cex = 0.8, 
            self.shiftx = c(0.10,0.10),
            self.shifty = c(+0.00,+0.00),
            self.arrpos = c(1.5*pi,1.5*pi,0.5*pi),
            main="Diagrama de Transición: Frio Calor", cex = 1.2)
    
  # Transicion en dos pasos
    FrioCalor^2
    
  # Clasificacion de estados
    summary(FrioCalor)
    
  # Analisis de Largo Plazo: distribucion estacionaria
    FrioCalor
    FrioCalor^2
    FrioCalor^3
    FrioCalor^4
    FrioCalor^8
    FrioCalor^9
    FrioCalor^15
    (DistribucionEstacionaria = steadyStates(FrioCalor))
    
  # Analisis de Largo Plazo: Tiempo medio del primer retorno
    (TiempoMedioPrimerRetorno = 1/DistribucionEstacionaria)
# __________________________________________________________________________________________________
# Ejemplo Sumpermercados ---------------------------------------------------------------------------
  rm(list=ls()); graphics.off()
    
  # Creo la matriz de transicion 
    estados<- c("Gran Food", "Mi Super")
    MatrizTrans<- matrix(c(0.90, 0.10,
                           0.20, 0.80),
                         byrow=T, ncol=2)
    colnames(MatrizTrans) = rownames(MatrizTrans) = estados
    MatrizTrans
    
  # Creo objeto Markov chain
    CuotaMercado <- new("markovchain",transitionMatrix = MatrizTrans)
    
  # Creo diagrama de transición    
    plotmat(t(CuotaMercado@transitionMatrix),
            box.type = 'diamond', shadow.size = 0, txt.col = "blue",txt.font = 4,
            box.lwd = 1.55,box.size = 0.08,box.col = 'light yellow',box.cex = 0.75,
            arr.col = 'red',arr.width = 0.2, arr.length = 0.25, 
            arr.type = 'triangle',arr.lwd = 1.2, arr.lcol = 'red',
            cex.txt = 0.8, curve=0.05,
            self.cex = 0.8, 
            self.shiftx = c(0.10,0.10),
            self.shifty = c(+0.00,+0.00),
            self.arrpos = c(1.5*pi,1.5*pi,0.5*pi),
            main="Diagrama de Transición: Cuota Mercado", cex = 1.2)
    
  # Transicion en dos y ocho pasos
    (P2 = CuotaMercado^2)
    (P8 = CuotaMercado^8)
    Estado0 = c(1000,0)
    Estado2 = Estado0 %*% P2 # OJO porque P2 es un objeto Markov chain, no una matriz
    (Estado2 = Estado0 %*% P2@transitionMatrix)
    (Estado8 = Estado0 %*% P8@transitionMatrix)
    
  # Analisis de Largo Plazo: distribucion estacionaria
    CuotaMercado
    CuotaMercado^2
    CuotaMercado^8
    CuotaMercado^40
    (DistribucionEstacionaria = steadyStates(CuotaMercado))
    
  # Grafico 30 transiciones, y veo la convergencia
    EstadoInicial = c(1,0)
    GranFood = c()
    MiSuper = c()
    n = 30
    for(k in 1:n){
      paso = EstadoInicial*CuotaMercado^k #Calculo k-esima semana
      GranFood[k] <- paso[1,1] # Leo fila 1, columna 1
      MiSuper[k] <- paso[1,2]  # Leo fila 1, columna 2
    }
    plot(GranFood, col='red', ylim=c(0,1), type ="l",ylab="Probabilidades",
          xlab="Paso", main="Probabilidades partiendo de estado Gran Food") 
    lines(MiSuper,col='blue',type="l")
    abline(h=steadyStates(CuotaMercado)[1],lty=2)
    abline(h=steadyStates(CuotaMercado)[2],lty=2)
    legend(n*0.75,1,legend=estados,col=c("red", "blue"),lty=1,cex=0.8)
    
  # Analisis de Largo Plazo: Tiempo medio del primer retorno
    (TiempoMedioPrimerRetorno = 1/DistribucionEstacionaria)
# __________________________________________________________________________________________________    
# Ejemplo Sumpermercados: Campania publicitaria Mi Super -------------------------------------------
  
  # Creo la nueva matriz de transicion 
    MatrizTrans2 = matrix(c(0.85, 0.15,
                           0.20, 0.80),
                         byrow=T, ncol=2)
    colnames(MatrizTrans2) = rownames(MatrizTrans2) = estados
    MatrizTrans2
    
  # Creo objeto Markov chain
    CuotaMercado2 = new("markovchain",transitionMatrix = MatrizTrans2)
    
  # Analisis de Largo Plazo: distribucion estacionaria
    (DistribucionEstacionaria2 = steadyStates(CuotaMercado2))
    
  # Distribucion de mercado
    (Antes = DistribucionEstacionaria*6000)
    (Despues = DistribucionEstacionaria2*6000)
    (Despues = round(DistribucionEstacionaria2*6000))
    
  # Análisis de conveniencia de la campania
    Costo = 4500 #Por semana
    (ClientesGanados = Despues[2] - Antes[2])
    Rentabilidad = 10 #Por cliente
    (Ganancia = ClientesGanados * Rentabilidad - Costo)
# __________________________________________________________________________________________________
# Ejemplo Sumpermercados: SQuick -------------------------------------------------------------------
  
  # Creo la nueva matriz de transicion 
    MatrizTrans3 = matrix(c(0.85, 0.10, 0.05,
                            0.20, 0.75, 0.05,
                            0.15, 0.10, 0.75),
                          byrow=T, ncol=3)
    colnames(MatrizTrans3) = rownames(MatrizTrans3) = c(estados,"SQuick")
    MatrizTrans3
    
  # Creo objeto Markov chain
    CuotaMercado3 = new("markovchain",transitionMatrix = MatrizTrans3)
    
  # Diagrama de transicion
    plotmat(t(CuotaMercado3@transitionMatrix),
            box.type = 'diamond', shadow.size = 0, txt.col = "blue",txt.font = 4,
            box.lwd = 1.55,box.size = 0.08,box.col = 'light yellow',box.cex = 0.75,
            arr.col = 'red',arr.width = 0.2, arr.length = 0.25, 
            arr.type = 'triangle',arr.lwd = 1.2, arr.lcol = 'red',
            cex.txt = 0.8, curve=0.05,
            self.cex = 0.8, 
            self.shiftx = c(+0.00,+0.00,-0.12),
            self.shifty = c(-0.10,-0.10,+0.00),
            self.arrpos = c(1.5*pi,1.5*pi,0.5*pi),
            main="Diagrama de Transición: Cuota Mercado", cex = 1.2)
    
  # Analisis de Largo Plazo: distribucion estacionaria
    (DistribucionEstacionaria3 = steadyStates(CuotaMercado3)) 
    round(DistribucionEstacionaria3*1000,0)
    round(DistribucionEstacionaria*1000)-round(DistribucionEstacionaria3[1:2]*1000)
# __________________________________________________________________________________________________
# Ejemplo Cuentas por Pagar ------------------------------------------------------------------------
  rm(list=ls()); graphics.off()
    
  # Creo la matriz de transicion 
    estados<- c("0-30", "31-90", "Pagado", "Incobrable")
    MatrizTrans<- matrix(c(0.30, 0.30, 0.40, 0.00,
                           0.30, 0.10, 0.40, 0.20,
                           0.00, 0.00, 1.00, 0.00,
                           0.00, 0.00, 0.00, 1.00), 
                         byrow=T, ncol=4)
    colnames(MatrizTrans) = rownames(MatrizTrans) = estados
    MatrizTrans
    
  # Creo objeto Markov chain
    CuentasCobrar <- new("markovchain",transitionMatrix = MatrizTrans)
      
  # Creo diagrama de transición
    plotmat(t(CuentasCobrar@transitionMatrix),
            box.type = 'diamond', shadow.size = 0, txt.col = "blue",txt.font = 4,
            box.lwd = 1.55,box.size = 0.07,box.col = 'light yellow',box.cex = 0.75,
            arr.col = 'red',arr.width = 0.2, arr.length = 0.25, 
            arr.type = 'triangle',arr.lwd = 1.2, arr.lcol = 'red',
            cex.txt = 0.8, curve=0.05,
            self.cex = 0.8, 
            self.shiftx = c(+0.00,-0.10,+0.00,+0.10),
            self.shifty = c(+0.10,+0.00,+0.10,+0.00),
            self.arrpos = NULL,
            main="Diagrama de Transición: Cuentas por Cobrar", cex = 1.2)
    
  # Clasificacion de estados
    summary(CuentasCobrar)
  
  # Particion de la Matriz de Transicion
    N = CuentasCobrar@transitionMatrix[transientStates(CuentasCobrar),transientStates(CuentasCobrar)]
    A = CuentasCobrar@transitionMatrix[transientStates(CuentasCobrar),absorbingStates(CuentasCobrar)]
    
  # Tiempo esperado en estados transitorios y hasta la absorción
    I = diag(nrow(N))
    (TE = solve(I-N))
    (TA = rowSums(TE))
    
  # Probabilidades de absorcion
    (PA = TE %*% A) 
    absorptionProbabilities(CuentasCobrar)
    
  # Distribución de montos esperados (condicionales) a largo plazo
    Estado0 = c(1000,2000) # Solamente "no absorbentes"
    (MontosEsperados = round(Estado0 %*% PA,2))
    
# __________________________________________________________________________________________________
# Ejemplo: Producción en Secuencia (Taha 17.6-1) ---------------------------------------------------
    rm(list=ls()); graphics.off()
    
  # Creo la matriz de transicion 
    estados<- c("maq. 1", "insp. 1", "maq. 2", "insp. 2", "Descarte", "Bueno")
    Matriz<- matrix(data = c(0.00,0.95,0.00,0.00,0.05,0.00,
                             0.07,0.00,0.90,0.00,0.03,0.00,
                             0.00,0.00,0.00,0.95,0.05,0.00,
                             0.00,0.00,0.07,0.00,0.03,0.90,
                             0.00,0.00,0.00,0.00,1.00,0.00,
                             0.00,0.00,0.00,0.00,0.00,1.00),
                    byrow = T, nrow=6)
    colnames(Matriz) = rownames(Matriz) = estados
    Matriz
    
  # Creo objeto Markov chain
    Prod.Secuencia <- new("markovchain",transitionMatrix = Matriz)
    
  # Creo diagrama de transición
    plotmat(t(Prod.Secuencia@transitionMatrix),
            box.lwd = 1.5,box.size = 0.055,box.col = 'light yellow',box.cex = 0.75,
            box.type = 'diamond',txt.col = "blue",txt.font = 4,
            arr.col = 'red',arr.width = 0.2, arr.length = 0.25, arr.type = 'triangle',
            arr.lwd = 1.2, arr.lcol = 'red',
            cex.txt = 0.75,
            self.cex = 0.8, self.shiftx = -0.03,self.shifty = -0.06,
            main="Diagrama de Transiciones: Proceso en secuencia.", cex = 1.25)
    
  # Clasificacion de estados
    summary(Prod.Secuencia)
    
  # Particion de la Matriz de Transicion
    (N = Prod.Secuencia@transitionMatrix[transientStates(Prod.Secuencia),
                                        transientStates(Prod.Secuencia)])
    (A = Prod.Secuencia@transitionMatrix[transientStates(Prod.Secuencia),
                                        absorbingStates(Prod.Secuencia)])
    
  # Visitas esperadas en estados transitorios y hasta la absorción
    I = diag(nrow(N))
    (TE = solve(I-N))
    (TA = rowSums(TE))
    
  # Probabilidades de absorcion
    (PA = TE %*% A) 
    absorptionProbabilities(Prod.Secuencia)
    
  # Distribución de cantidades esperadas (condicionales) a largo plazo
    Estado0 = c(1000,0,0,0) # Secuancial, entonces todos inician en maq. 1
    (CatindadesEsperadas = round(Estado0 %*% PA,0))
    
# __________________________________________________________________________________________________    