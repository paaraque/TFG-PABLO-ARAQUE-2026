datos_Td = read.csv("C:\\Users\\hp\\Desktop\\TFG\\CSV_ECV_Td_2024\\esudb24d.csv")

#head(datos_Td)
#summary(datos_Td)
#table(datos_Td['DB100'])

datos_Th = read.csv("C:\\Users\\hp\\Desktop\\TFG\\CSV_ECV_Th_2024\\esudb24h.csv")
#datos_Tp = read.csv("C:\\Users\\hp\\Desktop\\TFG\\CSV_ECV_Tp_2024\\esudb24p.csv")
datos_Tr = read.csv("C:\\Users\\hp\\Desktop\\TFG\\CSV_ECV_Tr_2024\\esudb24r.csv")

datos_Th_interesante = datos_Th[,c('HB010','HB020','HB030','HB120','vhPobreza')]

library(dplyr)
# Cambio del nombre de la columna HB030 (identificador del hogar) por DB030 para hacer un join
names(datos_Th_interesante)[names(datos_Th_interesante) == 'HB030'] <- 'DB030'

# Realizo el join para juntar tasa de pobreza e identif. geográfico
datos_Th_interesante = datos_Th_interesante %>%
                       left_join(datos_Td %>% dplyr::select(DB030, DB040), by = "DB030")

# Para sacar el número de personas por hogar, el sexo y los pesos muestrales, utiliz datos_Tr
sexo_inquilinos = datos_Tr[,c('RB030','RB090','RB050')]
sexo_inquilinos = sexo_inquilinos %>%
                  mutate(
                      Identif_personas = RB030 %% 100,
                      DB030  = RB030 %/% 100,
                        )
sexo_inquilinos$RB090 = factor(sexo_inquilinos$RB090,
                        levels = c(1, 2),
                        labels = c("Male", "Female")
                        )
names(sexo_inquilinos)[names(sexo_inquilinos) == 'RB050'] <- 'Pesos'

#Ahora juntamos el sexo y el número de personas con datos_Th_interesantes
sexo_inquilinos = sexo_inquilinos %>%
                  left_join(datos_Th_interesante %>% dplyr::select(HB020,DB030,HB120,vhPobreza, DB040), by = "DB030")


df_final = sexo_inquilinos
names(df_final)[names(df_final) == 'RB090'] <- 'Género'
names(df_final)[names(df_final) == 'DB030'] <- 'Identif_hogar'
names(df_final)[names(df_final) == 'DB040'] <- 'CCAA'
names(df_final)[names(df_final) == 'HB020'] <- 'País'
names(df_final)[names(df_final) == 'HB120'] <- 'Personas_en_hogar'



# Al desagregar por las variables anteriores nos encontramos con áreas sin individuos
# luego agrupamos a las personas que viven con 6 o + en una misma categoria
df_final <- df_final %>%
  mutate(Personas_en_hogar_agrupado = case_when(
    Personas_en_hogar >= 6 ~ "6+",
    TRUE ~ as.character(Personas_en_hogar)
  )) %>%
  mutate(Personas_en_hogar_agrupado = factor(Personas_en_hogar_agrupado,
                            levels = c("1","2","3","4","5","6+")))


#Vemos el número de observaciones por CCAA, sexo y personas en el hogar:
table(df_final$CCAA, df_final$Género, df_final$Personas_en_hogar_agrupado)


### Estimador Hájeck:

### Income 
library(srvyr)
estim_dir_Hajek <- df_final %>% 
                  group_by(CCAA, Género, Personas_en_hogar_agrupado) %>%
  
                  summarise(
                    numerador = sum(Pesos * vhPobreza, na.rm = TRUE),
                    N_d_gorro = sum(Pesos, na.rm = TRUE),
                    estimacion = numerador / N_d_gorro,
                    numerador_var = sum(Pesos*(Pesos-1)*((vhPobreza - estimacion)**2)),
                    estimacion_var = numerador_var / (N_d_gorro**2)
                    ) %>%
                  
                  ungroup() %>%
                  mutate( CV = sqrt(estimacion_var) / estimacion * 100 ) %>%
                  dplyr::select( CCAA, Género, Personas_en_hogar_agrupado, 
                                 estimacion, estimacion_var,  CV )

cat('Las áreas con un CV >20% son consideradas áreas pequeñas. ')
sum(estim_dir_Hajek$CV>30)



estim_dir_Hajek$id_dominio <- paste(estim_dir_Hajek$CCAA, 
                                    estim_dir_Hajek$Género, 
                                    estim_dir_Hajek$Personas_en_hogar_agrupado, 
                                    sep = "_")


rownames(estim_dir_Hajek) <- estim_dir_Hajek$id_dominio


# ¡OJO!!! en df_final %>% filter(CCAA == "ES43",Personas_en_hogar_agrupado == "6+", Género == "Female")
# todas las mujeres tienen vhPobreza = 1 --> estimacion_Hajeck es 1 


#Pasamos ahora a calcular estimador directo Horvitz-Thompson para la tasa de pobreza.
# n = 72774 indiv. y hay 19 CCAA
library(sae)

#ccaa.dir.res = direct(y = df_final$vhPobreza, dom = df_final$CCAA,
#                      sweight = df_final$Pesos, domsize = Nd[,-2] )



#Vemos aquellas CCAA con CV >20% las cuales consideraremos areas pequeñas
#ccaa.dir.cv = ccaa.dir.res$CV
#sum(ccaa.dir.cv>20)


# Definir antes totales_poblacionales

##############################################
# Como tenemos una varianza = 0 en la estimación, correspondiente al dominio ES43_Female_6+, 
# la suavizaremos de la siguiente manera:
# log(\hat{V}[\hat\barY_d^DIR]) = \alpha_0 +\alpha_1 n_d + e_d

log_vector_var_Hajeck = log(estim_dir_Hajek$estimacion_var[-132])

df_final$id_dominio = paste(df_final$CCAA, 
                  df_final$Género, 
                  df_final$Personas_en_hogar_agrupado, 
                  sep = "_")

n_d = as.data.frame(table(id_dominio = df_final$id_dominio)) # donde Freq es el tam muestral

n_d_sin_132 = n_d[n_d$id_dominio != "ES43_Female_6+",]

# Observamos la relación entre las variables a ajustar plot(log_vector_var_Hajeck ~ n_d_sin_132)
orden_aux = order(n_d_sin_132$Freq) # para ordenar de mayor a menor tamaño muestral
# Ordenar los valores del eje Y en base a ese orden
y_ordenado <- log_vector_var_Hajeck[orden_aux]
# Crear el eje X con los números del 1 al 277 (longitud del vector)
x_ordenado <- 1:length(y_ordenado)
# Dibujar el gráfico con el nuevo eje X secuencial
plot(y_ordenado ~ x_ordenado)

# Hay una relación clara: al aumentar n_d disminuye log-varianza
# Sugiere algo de heterocedasticidad, podríamos probar con log(n_d)

modelo_regres = lm(log_vector_var_Hajeck~n_d_sin_132$Freq)


# LLevamos a cabo una visualizacion del modelo de regresion 
# de la normalidad de los residuos 
qqnorm(modelo_regres$residuals)
qqline(modelo_regres$residuals)

# Llevamos a cabo otro modelo para el suavizado de la varianza
modelo_regres_log = lm(log_vector_var_Hajeck~log(n_d_sin_132$Freq))

# LLevamos a cabo una visualizacion del modelo de regresion 
# de la normalidad de los residuos con el segundo modelo
qqnorm(modelo_regres$residuals)
qqline(modelo_regres$residuals)

# Para ver por cual nos decidimos, utilizamos AIC y BIC
# cuanto más cercano a 0 mejor porque son -2log(L), con L=verosimilitud
AIC(modelo_regres, modelo_regres_log)
BIC(modelo_regres, modelo_regres_log)

# Estima mejor el segundo modelo, luego es el que usamos
sigma2_gorro = summary(modelo_regres_log)$sigma^2

nueva_var = exp(modelo_regres_log$coefficients[1] + 
                  modelo_regres_log$coefficients[2]* n_d$Freq[n_d$id_dominio == "ES43_Female_6+"]
                + 1/2 * sigma2_gorro ) 
# Añadimos 0.5*sigma2_gorro como una corrección del sesgo


estim_dir_Hajek$estimacion_var[estim_dir_Hajek$id_dominio=="ES43_Female_6+"] = nueva_var

rm(datos_Th_interesante,modelo_regres,modelo_regres_log,
   n_d_sin_132,log_vector_var_Hajeck,nueva_var)

######################################
# Gráfico CV de estimaciones directas
######################################
# MEtemos el tamaño muestral por dominio en Hájek
estim_dir_Hajek = estim_dir_Hajek %>%
  left_join(n_d %>% dplyr::select(id_dominio,Freq), 
            by = "id_dominio")

# Clasificación de dominios según el umbral institucional de Eurostat
estim_dir_Hajek <- estim_dir_Hajek %>%
  arrange(Freq)%>%
  mutate(Calidad_Hajek = ifelse(CV > 20, "Inaceptable (CV > 20%)", "Aceptable (CV <= 20%)"))

estim_dir_Hajek <- estim_dir_Hajek %>%
  arrange(Freq) %>%
  mutate(
    Dominio_Index = row_number()
  )

library(ggplot2)

ggplot(estim_dir_Hajek, aes(x = Dominio_Index, y = CV)) +
  geom_point(aes(color = Calidad_Hajek), alpha = 0.7, size = 2.5) +
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

# Tabla para observar los CV más altos
head(estim_dir_Hajek[order(-estim_dir_Hajek$CV),c("id_dominio","Freq","CV","estimacion","estimacion_var")])




#Representamos el gráfico domino y estimación, ordenando el dominio por tamaño muestral (de menor a mayor)
# DEbemos crear un nuevo df juntando por id_domino el tamaño muestral, la estimacion y la estimacion de la varianza
df_aux = merge(n_d, estim_dir_Hajek[,c(-1,-2,-3)], by= "id_dominio")

orden = order(df_aux$Freq)

#Ordenamos el df mediante el orden definido
df_ordenado <- df_aux[orden, ]
rm(df_aux)

x_ordenado = 1:nrow(df_ordenado)

# Representamos gráfico (en porcentaje df_ordenado$estimacion*100)
plot(df_ordenado$estimacion ~x_ordenado,
     ylab = "Estimación",
     xlab = "Dominios",
     main = "Estimación Hajek",
     type = "b",
     col = 'lightgreen',
     lty = 1,
     pch = 1,
     lwd = 2)

# Representamos el gráfico de área y ECM, ordenando el área por tamaño muesrtal (en porcentaje df_ordenado$estimacion_var *100)
plot(df_ordenado$estimacion_var ~x_ordenado,
     ylab = "ECM",
     xlab = "Dominios",
     main = "ECM estimados Hajek",
     type = "b",
     col = 'blue',
     lty = 1,
     pch = 1,
     lwd = 2)









