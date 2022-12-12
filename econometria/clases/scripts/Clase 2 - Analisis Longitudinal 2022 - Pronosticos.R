  #************************************************************
  #           PRONOSTICOS
  #************************************************************

  #------------------------------------------------------------
  #   Ejemplo 1
  #------------------------------------------------------------
  # Pronóstico de la ecuación de Mincer
  # Cargar Datos_EPH_CABA_3Tri2021.xls
  mincer=Datos_EPH_CABA_3Tri2021
  reg_m=lm(log(salario)~hombre+educa+I(hombre*educa)+edad+I(edad^2), data=mincer)
  summary(reg_m)
  
  # Elimino I(hombre*educa) que no es significativa
  reg_m=lm(log(salario)~hombre+educa+edad+I(edad^2), data=mincer)
  summary(reg_m)
  
  # Predecir para mujeres y varones el salario, considerar
  # 14 años de educacion y 40 años de edad.
  # Usamos predict()
  # Si lo usamos con la base de datos original
  # los valores obtenidos coinciden con los valores de y_hat
  # que los encuentro en rminc4$fittedvalues
  head(mincer,5)
  head(predict(reg_m,mincer),5)
  head(reg_m$fitted.values,5)
  
  # Pero para tener los salarios y no log(salario) debo
  # calcular exp(log(salario))
  head(exp(predict(reg_m,mincer)),5)
  
  # Para resolver con las características deseadas
  # Se puede generar un nuevo data frame 
  # 14 años de educacion y 40 años de edad.
  mincer_pred=mincer[1:2,]
  # Se debe guardar la edición porque si no se pierde
  mincer_pred=edit(mincer_pred)
  predict(reg_m,mincer_pred)
  exp(predict(reg_m,mincer_pred))

  # También se puede generar un data frame con los datos
  Pred=matrix(c(40,14,0,40,14,1),ncol=3,byrow=T)
  Pred=as.data.frame(Pred)
  colnames(Pred)=c("edad","educa","hombre")
  View(Pred)
  Pred_hat=exp(predict(reg_m, Pred))
  Pred_hat
  #------------------------------------------------------------
  #   Ejemplo 2
  #------------------------------------------------------------
  # Importar Table_7_9_Modificada.xls
  pollos=Table_7_9_Modificada
  names(pollos)=c("anio", "q_pollo", "ingreso", "p_pollo" , "p_cerdo", "p_vac", "p_subs") 
  # Recordar que los mejores modelos lin-lin y log-log fueron 
  # (Ver Clase 5 de la nivelación)
  # Aunque el test RESET validaba sólo la regresión log-log
  reg_p=lm(q_pollo~p_pollo+ p_cerdo+p_vac, data=pollos)
  summary(reg_p)
  reg_log=lm(log(q_pollo)~log(ingreso)+log(p_pollo), data=pollos)
  summary(reg_log)

  # Voy a comparar ambos modelos a partir de la bondad del
  # pronóstico que presenta cada uno.
  # Pronostico dentro de la muestra: Son los Y_hat que ya
  # tengo en reg_pF
  q_pollo_f1=predict(reg_p, data=pollos)
  Y_hat1=cbind(reg_p$fitted.values, q_pollo_f1)
  colnames(Y_hat1)=c("y_hat","predicción")
  head(Y_hat1,5)
  
  # Para evaluar qué modelo pronostica mejor dentro 
  # de la muestra
  C1=Eval_Pron(pollos$q_pollo,reg_p$fitted.values,"Lineal") 
  # Fijarse que como estoy explicando logaritmos
  # para oder comparar de aplicar exp(y_hat)
  C2=Eval_Pron(pollos$q_pollo,exp(reg_log$fitted.values),"Log") 
  options(scipen=5)
  Reporte=cbind(C1,C2)   ;  Reporte
  
 # install.packages("stargazer")
  library(stargazer)
  stargazer(Reporte,type="text", title="Evaluación de Pronósticos")
  #stargazer(Reporte,type="latex", title="Evaluación de Pronósticos")
  # También se puede utilizar, en lugar de Eval_Pron el comando
  # accuracy del paquete "forecast"
  install.packages("forecast")
  library(forecast)
  accuracy(exp(reg_log$fitted.values),pollos$q_pollo)
  # Fijarse que en accuracy(x_F,x_A) las serie van al revés que en 
  # Eval_Pron(x_A,x_F)
  
  
  # Ahora vamos a comparar los modelos en el pronóstico
  # dentro de la muestra
  # Divido los datos en "Muestra" y "Pronóstico"

  X_M=pollos[1:18,]
  X_P=pollos[19:23,]
  # Valores reales para la comparación
  Y_A=pollos$q_pollo[19:23]
  
  # Regreso con "Muestra" y pronostico con  "Pronóstico"
  # Modelo lineal
  reg_p2=lm(q_pollo~p_pollo+ p_cerdo+p_vac, data=X_M)
  Plin_2=predict(reg_p2, X_P)   
  
  # Modelo log-log
  reg_log2=lm(log(q_pollo)~log(ingreso)+log(p_pollo),data=X_M)
  Plog_2=exp(predict(reg_log2, X_P))
  
  C1=Eval_Pron(Y_A,Plin_2,"P_Lineal")
  C2=Eval_Pron(Y_A,Plog_2,"P_Log")
  Reporte=cbind(C1,C2)   ;   Reporte
  stargazer(Reporte,type="text", title="Evaluación de Pronósticos")
  stargazer(Reporte,type="latex", title="Evaluación de Pronósticos")  
