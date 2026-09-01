datos_Td = read.csv("C:\\Users\\hp\\Desktop\\TFG\\CSV_ECV_Td_2024\\esudb24d.csv")
datos_Tr = read.csv("C:\\Users\\hp\\Desktop\\TFG\\CSV_ECV_Tr_2024\\esudb24r.csv")

datos_Td_utiles = datos_Td[,c('DB030','DB040')]

names(datos_Td_utiles)[names(datos_Td_utiles) == 'DB030'] <- 'HB030'

 
datos_Tr_nivel_individuo = datos_Tr[,c('RB030','RB090','RB050','RB081')]

library(dplyr)
datos_Tr_nivel_individuo = datos_Tr_nivel_individuo %>%
  mutate(
    identif_personas = RB030 %% 100,
    HB030  = RB030 %/% 100,
  )

datos_Tr_nivel_individuo$RB090 = factor(datos_Tr_nivel_individuo$RB090,
                               levels = c(1, 2),
                               labels = c("Male", "Female")
)

names(datos_Tr_nivel_individuo)[names(datos_Tr_nivel_individuo) == 'RB090'] <- 'Género'

names(datos_Tr_nivel_individuo)[names(datos_Tr_nivel_individuo) == 'RB050'] <- 'Pesos'

names(datos_Tr_nivel_individuo)[names(datos_Tr_nivel_individuo) == 'RB081'] <- 'Edad'

datos_Tr_nivel_individuo <- datos_Tr_nivel_individuo %>%
  mutate(
    grupo_edad = cut(
      Edad,
      breaks = c(-1, 18, 35, 50, 65, max(datos_Tr$RB081)+1),
      right = FALSE,
      labels = c("0-17", "18-34", "35-49", "50-64", "65+")
    )
  )



#Ahora juntamos el sexo y el número de personas con datos_Th_interesantes
datos_Th_nivel_individuo = datos_Tr_nivel_individuo %>%
  left_join(datos_Th %>% dplyr::select(HB030,HB120,HY020,HY022,HY023,HY030N,HY040N,HY050N,HY060N,HY070N,
                                HY090N,HY110N,HY120N,HY130N,HY131N,HY145N,HY170N,HY010,HY040G,
                                HY050G,HY060G,HY070G,HY080G,HY081G,HY090G,HY100G,HY110G,HY120G,
                                HY130G,HY140G,HS021,HS022,HS040,HS060,HS090,HS110,
                                HS120,HD080,HH010,HH021,HH050,HH070,HI010,
                                HX060,HX240,vhRentaa,vhRentaAIa,vhMATDEP,
                                HC190,HC300), 
            by = "HB030")

#datos_Th_nivel_individuo$vhMATDEP <- as.factor(datos_Th_nivel_individuo$vhMATDEP)

datos_Th_nivel_individuo <- datos_Th_nivel_individuo %>%
  mutate(across(c(HS021,HS022,HS040,HS060,HS090,HS110,HS120,HD080,
                  HH010,HH021,HH050,HI010,
                  vhMATDEP, HC190, HC300), as.factor))


# Personas en hogar (HB120)
names(datos_Th_nivel_individuo)[names(datos_Th_nivel_individuo) == 'HB120'] <- 'Personas_en_hogar'
datos_Th_nivel_individuo <- datos_Th_nivel_individuo %>%
  mutate(Personas_en_hogar_agrupado = case_when(
    Personas_en_hogar >= 6 ~ "6+",
    TRUE ~ as.character(Personas_en_hogar)
  )) %>%
  mutate(Personas_en_hogar_agrupado = factor(Personas_en_hogar_agrupado,
                                             levels = c("1","2","3","4","5","6+")))



# Realizo el join para juntar identif. geográfico a dataset
datos_Th_nivel_individuo= datos_Th_nivel_individuo %>%
  left_join(datos_Td_utiles %>% dplyr::select(HB030,DB040), by = "HB030")

datos_Th_nivel_individuo <- datos_Th_nivel_individuo %>%
  mutate(across(c(identif_personas,HB030,Personas_en_hogar_agrupado,DB040), as.factor))

#Retoques finales
datos_Th_nivel_individuo$Personas_en_hogar <- NULL

#Eliminamos las siguientes variables porq son iguales a las que acaban en N, cuya diferencia es neto y bruto
datos_Th_nivel_individuo$HY040G <- NULL #solo difiere en un 5% de la HY040N
datos_Th_nivel_individuo$HY050G <- NULL
datos_Th_nivel_individuo$HY060G <- NULL
datos_Th_nivel_individuo$HY070G <- NULL
datos_Th_nivel_individuo$HY080G <- NULL
datos_Th_nivel_individuo$HY081G <- NULL
datos_Th_nivel_individuo$HY100G <- NULL
datos_Th_nivel_individuo$HY110G <- NULL
datos_Th_nivel_individuo$HY120G <- NULL
datos_Th_nivel_individuo$HY130G <- NULL
datos_Th_nivel_individuo$HY131G <- NULL

names(datos_Th_nivel_individuo)[names(datos_Th_nivel_individuo) == 'DB040'] <- 'CCAA'
names(datos_Th_nivel_individuo)[names(datos_Th_nivel_individuo) == 'HB030'] <- 'Identif_hogar'

names(datos_Tr_nivel_individuo)[names(datos_Tr_nivel_individuo) == 'HB030'] <- 'Identif_hogar'


summary(datos_Th_nivel_individuo)


#########################################################################
# Aqui calculamos la estimación de los tamaños poblacionales por áreas
#########################################################################

# Calculamos la suma de pesos por dominio
totales_poblacionales_estimados <- datos_Th_nivel_individuo %>%
  
  group_by(CCAA, Género, Personas_en_hogar_agrupado) %>%
  #Sumamos los pesos dentro de cada grupo
  summarise(suma_pesos = sum(Pesos, na.rm = TRUE), .groups = 'drop') %>%
  #### .groups='drop' --> Haz la suma para cada combinación de ccaa/sexo/hogar, 
  ####                    dame el resultado y, cuando termines, destruye todos los grupos. 
  ####                    Quiero una tabla completamente limpia
  
  # Identificamos los dominios
  mutate(id_dominio = paste(CCAA, Género, Personas_en_hogar_agrupado, sep = "_")) %>%
  #Ordenamos los resultados 
  arrange(CCAA, Género, Personas_en_hogar_agrupado)

# Extraer el vector final
#vector_pesos <- totales_poblacionales_estimados$suma_pesos



# ------------------------------------------------------------------------------
# PASO 1: Identificadores de Dominio en Microdatos
# ------------------------------------------------------------------------------
# Creamos una columna única que defina los dominios en la encuesta
datos_Th_nivel_individuo$id_dominio <- paste(datos_Th_nivel_individuo$CCAA, 
                                             datos_Th_nivel_individuo$Género, 
                                             datos_Th_nivel_individuo$Personas_en_hogar_agrupado, 
                                             sep = "_")




# Código para ver a qué dominio pertenecen los NAs de una columna
res <- aggregate(is.na(mi_columna) ~ id_dominio, data = datos_Th_nivel_individuo, sum)
# quitar los que tienen 0 NA
res <- res[res$`is.na(mi_columna)` > 0, ]
#res




# ------------------------------------------------------------------------------
# PASO 2: Cálculo de tamaños poblacionales (domsize)
# ------------------------------------------------------------------------------
# El paquete sae exige exactamente estas dos columnas: identificador y tamaño,
# para aplicar después la función direct,
# creamos un df donde guarde el ID del dominio y su tamaño poblacional estimado(gorroN_d)
df_domsize <- data.frame(
  Domain = totales_poblacionales_estimados$id_dominio,
  Size = totales_poblacionales_estimados$suma_pesos
)


# ------------------------------------------------------------------------------
# PASO 3: Construcción de la matriz X 
# ------------------------------------------------------------------------------

# Definimos la lista con todas tus variables (factores y numéricas)
# creamos un vector con el nombre de las variables primero numéricas y después factores
#variables_interes <- c(names(datos_Th_nivel_individuo)[sapply(datos_Th_nivel_individuo, is.numeric)], 
#                    names(datos_Th_nivel_individuo)[sapply(datos_Th_nivel_individuo, is.factor)])


variables_interes <- c(names(datos_Th_nivel_individuo)[sapply(datos_Th_nivel_individuo, is.factor)])

variables_interes <- variables_interes[!(variables_interes %in% 
                                           c("RB030","Pesos","Género","identif_personas","Identif_hogar","Personas_en_hogar_agrupado","CCAA"))]


# Inicializamos el data frame base con la columna de unos
matriz_X <- data.frame(
            Domain = df_domsize$Domain,
            Intercept = 1
            )

library(sae)


for (var in variables_interes) {
  
  # A. SI LA VARIABLE ES UN FACTOR 
  if (is.factor(datos_Th_nivel_individuo[[var]]) || is.character(datos_Th_nivel_individuo[[var]])) {
    
    # Extraemos todos los niveles de la variable
    niveles <- levels(as.factor(datos_Th_nivel_individuo[[var]]))
    
    # Omitimos el primer nivel (para evitar colinealidad)
    niveles_a_crear <- niveles[-1]
    
    for (nivel in niveles_a_crear) {
      
      # Creamos el nombre de la variable dummy 
      nombre_dummy <- paste0(var, "_", nivel)
      
      # 1) Creamos la columna en el dataframe (manteniendo los NAs intactos)
      datos_Th_nivel_individuo[[nombre_dummy]] <- ifelse(
        is.na(datos_Th_nivel_individuo[[var]]), 
        NA, 
        ifelse(datos_Th_nivel_individuo[[var]] == nivel, 1, 0)
      )
      
      # 2) Aplicamos la estimación directa a la nueva dummy
      estimacion_temp <- direct(
                          y = datos_Th_nivel_individuo[[nombre_dummy]], 
                          dom = datos_Th_nivel_individuo$id_dominio, 
                          sweight = datos_Th_nivel_individuo$Pesos, 
                          domsize = df_domsize
                          )
      
      # 3) Extraemos y renombramos el resultado para guardarlo
      est_reducida <- estimacion_temp[, c("Domain", "Direct")]
      colnames(est_reducida)[2] <- nombre_dummy
      
      # 4) Unimos a nuestro data frame general (matriz_X)
      matriz_X <- merge(matriz_X, est_reducida, by = "Domain", all.x = TRUE)
      
      # 5) BORRAMOS LA COLUMNA para liberar memoria RAM
      datos_Th_nivel_individuo[[nombre_dummy]] <- NULL
    }
    
  } else {
    
    # B. SI LA VARIABLE ES NUMÉRICA 
    estimacion <- direct(
                  y = datos_Th_nivel_individuo[[var]], 
                  dom = datos_Th_nivel_individuo$id_dominio, 
                  sweight = datos_Th_nivel_individuo$Pesos, 
                  domsize = df_domsize
                  )
    
    # 2) Extraemos solo el nombre del dominio y el valor estimado ('Direct')
    est_reducida <- estimacion[, c("Domain", "Direct")]
    
    # 3) Renombramos la columna 'Direct' con el nombre de la variable real
    colnames(est_reducida)[2] <- var
    
    # 4) Unimos los resultados al data frame con un left join == all.x 
    matriz_X <- merge(matriz_X, est_reducida, by = "Domain", all.x = TRUE)
    
  }
}

# Para liberar memoria en caso de necesitarlo
rm(nivel,niveles,niveles_a_crear,var,nombre_dummy,estimacion,estimacion_temp,est_reducida)

# 3.1 comprobamos si hay Na's en la matriz. Si es así, los imputaremos por la mediana de dicha área.

sum(is.na(matriz_X))
which(is.na(matriz_X), arr.ind = TRUE)

# Se observa que los NA's provienen de la variable HS150 en dos áreas concretas,
# luego decidimos eliminar esas dos columnas porque la matrix para el eblup no puede contener NA's
# Otra opción sería imputa pero lo descartamos.
#matriz_X$HS150_2 <- NULL
#matriz_X$HS150_3 <- NULL

# Comprobación final:
sum(is.na(matriz_X))==0


# 4. Formateo final de la matriz_X
rownames(matriz_X) <- matriz_X$Domain
matriz_X <- as.matrix(matriz_X[, -1])

# Visualizamos las dimensiones y los primeros datos
print(paste("La matriz tiene", nrow(matriz_X), "filas y", ncol(matriz_X), "columnas."))
#head(matriz_X)



# Antes de aplicar el modelo eblupFH a la matriz_X de covariables a nivel de área, 
# comprobamos que no haya multicolinealidad fuerte entre dichas covariables.
library(car)
df_matriz_X <- as.data.frame(matriz_X[,-1])

solo_estim_dir_Hajek <- estim_dir_Hajek$estimacion
df_matriz_X$y <- solo_estim_dir_Hajek  #variable respuesta a nivel de area

modelo <- lm(y ~ ., data = df_matriz_X)
vif(modelo)

########################################
# El Factor de Inflación de la Varianza (VIF) se utiliza para estudiar la multicolinealidad 
# porque cuantifica directamente cuánto aumenta la varianza (y por ende el error estándar) 
# de un coeficiente de regresión estimado debido a la correlación con otras variables independientes. 
# Permite identificar variables redundantes que inestabilizan el modelo.
########################################

#Como vif nos dice que hay variables que son comb. lineales, veámos cuales son: 
alias(modelo)

# Observamos que no hay comb. lineales entre las variables cualitativas.


#Buscamos y eliminamos aquellas variables que tengan un vif>10, pues implica multicolinealidad
modelo_prueba = lm(y~., data = subset(df_matriz_X, select = -c(HS021_2,HS021_3,HS060_2,HS120_2,HS120_3,HS120_4,HS120_5,HS120_6,HC300_2,HC300_3,HC300_4)))
vif(modelo_prueba)
summary(vif(modelo_prueba))
hist(vif(modelo_prueba))

# Modelo AIC
modelo_aic <- step(modelo_prueba, direction = "both")

# Modelo BIC
modelo_bic <- step(modelo_prueba,direction = "both", k = log(nrow(subset(df_matriz_X, select = -c(HS021_2,HS021_3,HS060_2,HS120_2,HS120_3,HS120_4,HS120_5,HS120_6,HC300_2,HC300_3,HC300_4)))))

formula(modelo_aic)
formula(modelo_bic)

# Variables en común:HS022_2,HS040_2,HS110_3,HD080_2,HH010_2,HH010_3,HH021_4,HH050_2,HI010_2,HI010_3
# TODAS LAS DEL BIC ESTÁN EN EL AIC, luego son las que más explican

# Tras estudiar ambos modelos se opta por el modelo seleccionado mediante el criterio Bayesian Information Criterion (BIC)
# porque proporciona una especificación más estable que la obtenida 
# con el Akaike Information Criterion (AIC). Mientras que el AIC tiende a incluir un 
# mayor número de variables con el objetivo de maximizar el ajuste, el BIC introduce 
# una penalización más fuerte por la complejidad del modelo, favoreciendo la selección 
# de un subconjunto reducido de covariables con mayor capacidad explicativa conjunta. 
# En contextos como el análisis con modelos de áreas pequeñas, donde existe riesgo de 
# multicolinealidad y sobreajuste debido al elevado número de variables iniciales, 
# esta mayor parsimonia resulta especialmente deseable, ya que contribuye a mejorar 
# la estabilidad de las estimaciones y la interpretabilidad del modelo final.

vif(modelo_bic)
# Se observa que hay menor colinealidad en las variables del modelo BIC


# Observamos si son significativas las variables del BIC con:
summary(modelo_bic)

matriz_X_final <- model.matrix(
                  ~ HS022_2 + HS040_2 + HS110_3 + HD080_2 + HH010_2 + 
                    HH010_3 + HH021_4 + HH050_2 + HI010_2 + HI010_3,
                  data = df_matriz_X )


estim.FH.res <- eblupFH(solo_estim_dir_Hajek~matriz_X_final-1,vardir= estim_dir_Hajek$estimacion_var)
estim.FH.mse <- mseFH(solo_estim_dir_Hajek~matriz_X_final-1,vardir= estim_dir_Hajek$estimacion_var)
#povinc.FH <- povinc.FH.res$eblup


estim.FH.cv = 100*sqrt(estim.FH.mse$mse)/estim.FH.res$eblup

cbind(directa.cv = estim_dir_Hajek$CV, FH = estim.FH.cv ,muestra = table(datos_Th_nivel_individuo$id_dominio) )

#Limpiamos la memoria
rm(datos_Td_utiles,totales_poblacionales_estimados,matriz_X,modelo,modelo_aic,modelo_bic)



# Representacion gráfica de estimacion y ECM
estim_dir_Hajek$estim_EBLUP = estim.FH.res$eblup

df_aux = merge(n_d, estim_dir_Hajek[,c(-1,-2,-3)], by= "id_dominio")

orden = order(df_aux$Freq)

#Ordenamos el df mediante el orden definido
df_ordenado = df_aux[orden, ]
rm(df_aux)

x_ordenado = 1:nrow(df_ordenado)

rango_y = range(c(df_ordenado$estimacion, df_ordenado$estim_EBLUP), na.rm = TRUE)
# Representamos gráfico (en porcentaje df_ordenado$estimacion*100)
plot(df_ordenado$estimacion ~x_ordenado,
     ylab = "Estimación",
     xlab = "Dominios",
     #main = "Estimación Hajek y EBLUP",
     type = "b",
     col = 'green',
     lty = 1,
     pch = 19,
     lwd = 1.7)

lines(df_ordenado$estim_EBLUP ~ x_ordenado, 
      type = "b", 
      pch = 18,           
      col = "darkblue",   
      lty = 1, 
      lwd = 1.6)            

legend("topright", 
       legend = c("Hajek", "EBLUP"),
       col = c("green", "darkblue"), 
       pch = c(19, 18), 
       lty = 1, 
       lwd = c(1, 2),
       bty = "n")          

rm(df_ordenado,rango_y)

# Representamos el gráfico de área y ECM, ordenando el área por tamaño muesrtal (en porcentaje df_ordenado$estimacion_var *100)
estim_dir_Hajek$estim_EBLUP_ECM <- estim.FH.mse$mse

df_aux = merge(n_d, estim_dir_Hajek[,c(-1,-2,-3)], by= "id_dominio")

#Ordenamos el df mediante el orden definido
df_ordenado = df_aux[orden, ]
rm(df_aux)

x_ordenado = 1:nrow(df_ordenado)

rango_y = range(c(df_ordenado$estimacion, df_ordenado$estim_EBLUP_ECM), na.rm = TRUE)
plot(df_ordenado$estimacion_var ~x_ordenado,
     ylab = "ECM",
     xlab = "Dominios",
     type = "b",
     col = 'green',
     lty = 1,
     pch = 19,
     lwd = 1.5)

lines(df_ordenado$estim_EBLUP_ECM ~ x_ordenado, 
      type = "b", 
      pch = 18,           
      col = "darkblue",   
      lty = 1, 
      lwd = 1.5)            

legend("topright", 
       legend = c("Hajek", "EBLUP"),
       col = c("green", "darkblue"), 
       pch = c(19, 18), 
       lty = 1, 
       lwd = c(1, 2),
       bty = "n") 


############################
# CV de la estimación FH
############################

estim_dir_Hajek <- estim_dir_Hajek %>% 
  mutate(FH_CV = 100*sqrt(estim_EBLUP_ECM)/estim_EBLUP)

estim_dir_Hajek <- estim_dir_Hajek %>%
  arrange(Freq)%>%
  mutate(Calidad_FH = ifelse(FH_CV > 20, "Inaceptable (CV > 20%)", "Aceptable (CV <= 20%)"))

estim_dir_Hajek <- estim_dir_Hajek %>%
  arrange(Freq) %>%
  mutate(
    FH_Dominio_Index = row_number()
  )

library(ggplot2)

ggplot(estim_dir_Hajek, aes(x = FH_Dominio_Index, y = FH_CV)) +
  geom_point(aes(color = Calidad_FH), alpha = 0.7, size = 2.5) +
  geom_hline(
    aes(yintercept = 20, color = "Umbral Eurostat (CV = 20)"), linetype = "solid", linewidth = 1) +
  scale_color_manual(
    name = NULL,
    values = c("Aceptable (CV <= 20%)" = "#3b82f6",
               "Inaceptable (CV > 20%)" = "#ef4444",
               "Umbral Eurostat (CV = 20)" = "black"),
    breaks = c(
      "Aceptable (CV <= 20%)",
      "Inaceptable (CV > 20%)",
      "Umbral Eurostat (CV = 20)"),
    labels = c(
      "CV ≤ 20% (Aceptable)",
      "CV > 20% (Inaceptable)",
      "Umbral Eurostat (CV = 20)")
  ) +
  labs(
    title = "Coeficiente de Variación (CV) por dominios",
    x = "Dominios (ordenados de menor a mayor tamaño muestral)",
    y = "Coeficiente de Variación (CV %)",
    color = NULL
  ) +
  theme_minimal(base_family = "serif") +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
    axis.title = element_text(size = 11),
    legend.position = c(0.83, 0.85),
    legend.background = element_rect(fill = "white", color = "grey80"),
    panel.grid.major = element_line(color = "#e2e8f0", linetype = "dashed")
  )



############################################
# Comprobación normalidad para estim. ECM
############################################

# Los residuos estandarizados son importantes porque en FH cada dominio tiene una
# varianza de muestreo distinta.
###############################
# 1. Residuals vs Fitted
###############################
# Extraemos beta_goro porque fitted = X %*% beta
beta_gorro_FH = estim.FH.res$fit$estcoef$beta

fitted_FH = as.vector(matriz_X_final %*% beta_gorro_FH)

# Calculamos los residuos
residuos_FH = estim_dir_Hajek$estimacion - fitted_FH

# Gráfico de fitted vs residuals de eblup FH a nivel de área 
plot(fitted_FH,residuos_FH)
lines(lowess(fitted_FH, residuos_FH),
      col = "red",
      lwd = 2)

# Observamos cuales son los más extremos
which(abs(residuos_FH) > 0.2)

#########################################
# 2. Residuals estandarizados vs Fitted
#########################################
# Calculamos los residuos estandarizados
sigma2_u_gorro = estim.FH.res$fit$refvar
residuos_estand_FH = residuos_FH / sqrt(estim_dir_Hajek$estimacion_var + sigma2_u_gorro)

# Graficamos
plot(fitted_FH,residuos_estand_FH)
lines(lowess(fitted_FH, residuos_estand_FH),
      col = "red",
      lwd = 2)

# Observamos cuales son los más extremos
which(abs(residuos_estand_FH) > 2)


################################
# 3. Q-Q plot 
################################

qqnorm(residuos_estand_FH)
qqline(residuos_estand_FH, col='red')

# Para identificar los valores más extremos
id <- order(abs(residuos_estand_FH), decreasing = TRUE)[1:5]
qq <- qqnorm(residuos_estand_FH, plot.it = FALSE)
text(qq$x[id],
     qq$y[id],
     labels = id,
     pos = 4)
# Conclusión
#La Figura X presenta el gráfico Q-Q de los residuos estandarizados del modelo Fay-Herriot. 
#Se observa que la mayor parte de los residuos se alinean razonablemente con la recta de referencia, 
#lo que sugiere que la hipótesis de normalidad es adecuada para la mayoría de los dominios. 
#No obstante, se aprecian desviaciones en ambas colas, especialmente en la cola superior, 
#lo que indica la presencia de algunos dominios extremos. 
#En conjunto, el supuesto de normalidad puede considerarse aceptablemente satisfecho, 
#aunque conviene analizar la influencia de los dominios con residuos estandarizados más elevados.

##############################################
# 4. Histograma de residuos estandarizados 
##############################################
hist(residuos_estand_FH)

########################################################
# Cálculo de la normalidad de las estimaciones directas
########################################################
hist(estim_dir_Hajek$estimacion, freq = FALSE)
hist(estim_dir_Hajek$estimacion)

qqnorm(estim_dir_Hajek$estimacion)
qqline(estim_dir_Hajek$estimacion, col = "red")


