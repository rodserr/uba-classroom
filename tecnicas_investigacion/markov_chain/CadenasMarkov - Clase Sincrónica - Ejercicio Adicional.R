# __________________________________________________________________________________________________
# E72: Especializacion en Metodos Cuantitativos         ____________________________________________
# para la Gestion y Analisis de Datos en Organizaciones ____________________________________________
# __________________________________________________________________________________________________
# TECNICAS DE INVESTIGACION OPERATIVA PARA LA GESTION ______________________________________________
# __________________________________________________________________________________________________
# Resolucion de Ejercicio de Cadena de Markov usando los paquetes "markovchain" y "diagram" ________
# Aplicación a Análisis de Riesgo Crediticio _______________________________________________________
# __________________________________________________________________________________________________
# Preparado por R. Darío Bacchini __________________________________________________________________
# __________________________________________________________________________________________________
# Cargo paquetes y limpio espacio de trabajo -------------------------------------------------------
  library(markovchain)
  library(diagram)
  library(expm)
  rm(list=ls()); graphics.off()
# __________________________________________________________________________________________________
# Ingreso matriz de transición y creo objeto markov chain ------------------------------------------
  # Creo la matriz de transicion 
    estados<- c("A", "B", "C", "D")
    MatrizTrans = matrix(c(0.9601, 0.0153, 0.0246, 0.0000,
                           0.0195, 0.8502, 0.1085, 0.0218,
                           0.0103, 0.0946, 0.6905, 0.2046,
                           0.0000, 0.0000, 0.0000, 1.0000), 
                       byrow=T, ncol=4)
    colnames(MatrizTrans) = rownames(MatrizTrans) = estados
    MatrizTrans
  # Creo objeto Markov chain
    Ratings = new("markovchain",transitionMatrix = MatrizTrans)

## a) Creo diagrama de transición ------------------------------------------------------------------
  plotmat(t(Ratings@transitionMatrix),
          box.type = 'diamond', shadow.size = 0, txt.col = "blue",txt.font = 4,
          box.lwd = 1.55,box.size = 0.07,box.col = 'light yellow',box.cex = 0.75,
          arr.col = 'red',arr.width = 0.2, arr.length = 0.25, 
          arr.type = 'triangle',arr.lwd = 1.2, arr.lcol = 'red',
          cex.txt = 0.8, curve=0.05,
          self.cex = 0.8, 
          self.shiftx = c(+0.00,-0.10,+0.00,+0.10),
          self.shifty = c(+0.10,+0.00,+0.10,+0.00),
          self.arrpos = NULL,
          main="Diagrama de Transición: Ratings Crediticios", cex = 1.2)

  
## b) Calsificiación de Estados --------------------------------------------------------------------
  # Clasificacion de estados
  summary(Ratings)
  # Largo Plazo
  steadyStates(Ratings)
  Ratings^5
  Ratings^30
  Ratings^50
    
## c) Default en o antes del año 5 -----------------------------------------------------------------
  
  # Transicion en cinco pasos
    P5 = Ratings^5
    P5@transitionMatrix["A","D"]

## d) Default en el año 5 --------------------------------------------------------------------------
    # Particiono matriz
      P.NoD.1 = Ratings[transientStates(Ratings),transientStates(Ratings)]
      P.D.1 = Ratings[transientStates(Ratings),absorbingStates(Ratings)]
    # Transiciones de 1 a 4 entre estados transitorios, y en el último paso voy a "default"
      (P.NoD.1 %^% 4) %*% P.D.1
      
    # Chequeo que al agregar las probabilidades como la anterior se llega al resultado del punto c) 
      (P.NoD.1 %^% 0) %*% P.D.1+ #Default durante el año 1
      (P.NoD.1 %^% 1) %*% P.D.1+ #No Default durante el año 1, y default en año 2
      (P.NoD.1 %^% 2) %*% P.D.1+ #No Default hasta el año 2, y default en año 3
      (P.NoD.1 %^% 3) %*% P.D.1+ #No Default hasta el año 3, y default en año 4
      (P.NoD.1 %^% 4) %*% P.D.1- #No Default hasta el año 4, y default en año 5
      P5[1:3,4]                  #Leo acumulado de P5: de A, B, C a D en 5 años

## e) Grafico 30 transiciones ----------------------------------------------------------------------
    EstadoInicial = c(1,0,0,0)
    n = 30
    # Creo matriz con cuatro columnas (estados) y n+1 filas (tiempos)
      Dist.Cond.A = matrix(NA, nrow = n+1, ncol = ncol(Ratings@transitionMatrix))
      colnames(Dist.Cond.A) = estados
      rownames(Dist.Cond.A) = 0:n
    # Asigno las distribuciones condicionadas a cada fila (tiempo): la inicial es dada, y las otras 
    # se calculan a partir de las potencias de "P"
      Dist.Cond.A[1,] = EstadoInicial
      for(k in 1:n){
        paso = EstadoInicial*Ratings^k #Calculo k-esimo anio
        Dist.Cond.A[k+1,] = paso[1,] # Leo fila 1 de EsatdoInicial*P^k
      }
    # Grafico
      plot(0:n,Dist.Cond.A[,1], col='darkgreen', ylim=c(0,1), type ="l", ylab="Probabilidades",
           xlab="Año", main="Probabilidades partiendo de estado A") 
      lines(0:n,Dist.Cond.A[,2],col='gold3',type="l")
      lines(0:n,Dist.Cond.A[,3],col='orange3',type="l")
      lines(0:n,Dist.Cond.A[,4],col='red',type="l")
      legend(n*0.85,1,legend=estados,col=c("darkgreen","gold3","orange3", "red"),lty=1,cex=0.8)
    
