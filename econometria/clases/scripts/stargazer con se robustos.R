  ###########################################################
  #             Variables Instrumentales
  ###########################################################
  # install.packages("AER")
  library(AER)
#  install.packages("lmtest")
  library(lmtest)
#  install.packages("sandwich")
  library(sandwich)
#  install.packages("stargazer")
  library(stargazer)
  #------------------------------------------------------------
  #   Ejemplo 1
  #------------------------------------------------------------
  # Cargar archivo STATA "educ_padres.dta"
  mco_p=lm(logsal~educa+expe+expe2+hombre, data=educ_padres)
  summary(mco_p)

  # Test de Heterocedasticidad Breusch Pagan Godfrey
  bptest(mco_p)
  # A pesar de que no se detecta heterocedasticidad, en 
  # corte transversal, reporto siempre errores robustos
  # Para coeftest necesito lmtest y sandwich
  coeftest(mco_p, vcov = vcovHC(mco_p, type="HC1"))
  # NO hay mucha diferencia en este caso en los std. err.
  
  #********************************************************
  # También puede presentarse en stargazer con 
  # errores estándar robustos
  rob1=sqrt(diag(vcovHC(mco_p, type = "HC1")))
  stargazer(mco_p, 
            se=list(rob1),
            title="Regresión MCO", type="text", 
            df=FALSE, digits=5)
  #********************************************************
  
  # Ahora realizamos la regresión con variables
  # instrumentales (instrumento educa con educ_padres)
   iv_p=ivreg(logsal~educa+expe+expe2+hombre|educa_p+
               expe+expe2+hombre, data=educ_padres)
  summary(iv_p, diagnostics=T)
  # summary(iv_p, vcov = sandwich, diagnostics = TRUE)
  coeftest(iv_p, vcov = vcovHC(iv_p, type="HC1"))
  
  #********************************************************
  # También puede presentarse en stargazer
  # con errores estándar robustos
  rob2=sqrt(diag(vcovHC(iv_p, type = "HC1")))
  stargazer(iv_p, 
            se=list(rob2),
            title="IV Regression", type="text", 
            df=FALSE, digits=5)
  #********************************************************
  
  
  #********************************************************
  # También puede presentarse en stargazer
  # ambas salidascon errores estándar robustos
  stargazer(mco_p , iv_p, 
            se=list(rob1, rob2),
            title="Reporte Regresiones MCO vs. Vi ", type="text", 
            df=FALSE, digits=5)
  #********************************************************
  