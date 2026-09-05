######################################
# Fase 1: Creación de datasets útiles
######################################

# Librerías:
library(sae)
library(dplyr)
library(ggplot2)
library(car)
library(srvyr)
library(lme4)
library(e1071)

# En primer lugar cargamos los csv de la ECV (csv en el repositorio)
ruta_base   <- "."   
ruta_datos_Td <- file.path(ruta_base, "data", "CSV_ECV_Td_2024", "esudb24d.csv")
ruta_datos_Th <- file.path(ruta_base, "data", "CSV_ECV_Th_2024", "esudb24h.csv")
ruta_datos_Tr <- file.path(ruta_base, "data", "CSV_ECV_Tr_2024", "esudb24r.csv")

ruta_salidas <- file.path(ruta_base, "outputs")
dir.create(ruta_salidas, showWarnings = FALSE, recursive = TRUE)

datos_Td <- read.csv(ruta_datos_Td)
datos_Th <- read.csv(ruta_datos_Th)
datos_Tr <- read.csv(ruta_datos_Tr)

# Realizamos unas comprobaciones iniciales para ver si se han cargado bien
head(datos_Td)
summary(datos_Td)

head(datos_Th)
summary(datos_Th)

head(datos_Tr)
summary(datos_Tr)

# Una vez cargados los datos,creamos un dataframe (df) reducido con las variables 
# más útiles y realizamos ciertas transformaciones para poder unir información.
datos_Th_interesantes <- datos_Th[,c('HB010','HB020','HB030','HB120','vhPobreza')]


# Cambio del nombre de la columna HB030 (identificador del hogar) por DB030 para hacer un join
names(datos_Th_interesantes)[names(datos_Th_interesantes) == 'HB030'] <- 'DB030'

# Realizo el join para juntar tasa de pobreza e identif. geográfico
datos_Th_interesantes <- datos_Th_interesantes %>%
  left_join(datos_Td %>% dplyr::select(DB030, DB040), by = "DB030")

# Para sacar el número de personas por hogar, el género y los pesos muestrales, utiliz datos_Tr


datos_Tr_interesantes <- datos_Tr[,c('RB030','RB090','RB050')]

# En la columna RB030 vienen juntas el identificador del hogar y el identificador 
# de la persona (últimos 2 dígitos) 

datos_Tr_interesantes <- datos_Tr_interesantes %>%
  mutate(
    Identif_personas = RB030 %% 100,
    DB030  = RB030 %/% 100,
  )
datos_Tr_interesantes$RB090 <- factor(datos_Tr_interesantes$RB090,
                               levels = c(1, 2),
                               labels = c("Male", "Female")
)
names(datos_Tr_interesantes)[names(datos_Tr_interesantes) == 'RB050'] <- 'Pesos'

#Ahora juntamos el género y el número de personas con datos_Th_interesantes
datos_Tr_interesantes <- datos_Tr_interesantes %>%
  left_join(datos_Th_interesantes %>% dplyr::select(HB020,DB030,HB120,vhPobreza, DB040), by = "DB030")


datos_hajek <- datos_Tr_interesantes
names(datos_hajek)[names(datos_hajek) == 'RB090'] <- 'Género'
names(datos_hajek)[names(datos_hajek) == 'DB030'] <- 'Identif_hogar'
names(datos_hajek)[names(datos_hajek) == 'DB040'] <- 'CCAA'
names(datos_hajek)[names(datos_hajek) == 'HB020'] <- 'País'
names(datos_hajek)[names(datos_hajek) == 'HB120'] <- 'Personas_en_hogar'



# Al desagregar por las variables anteriores nos encontramos con áreas sin individuos
# luego agrupamos a las personas que viven con 6 o + personas en una misma categoria
datos_hajek <- datos_hajek %>%
  mutate(Personas_en_hogar_agrupado = case_when(
    Personas_en_hogar >= 6 ~ "6+",
    TRUE ~ as.character(Personas_en_hogar)
  )) %>%
  mutate(Personas_en_hogar_agrupado = factor(Personas_en_hogar_agrupado,
                                             levels = c("1","2","3","4","5","6+")))


# Vemos el número de observaciones por CCAA, género y personas en el hogar:
table(datos_hajek$CCAA, datos_hajek$Género, datos_hajek$Personas_en_hogar_agrupado)

##############################
# Datos a nivel de individuo
datos_Td_utiles <- datos_Td[,c('DB030','DB040')]

names(datos_Td_utiles)[names(datos_Td_utiles) == 'DB030'] <- 'HB030'


datos_Tr_nivel_individuo <- datos_Tr[,c('RB030','RB090','RB050','RB081')]

datos_Tr_nivel_individuo <- datos_Tr_nivel_individuo %>%
  mutate(
    Identif_personas = RB030 %% 100,
    HB030  = RB030 %/% 100,
  )

datos_Tr_nivel_individuo$RB090 <- factor(datos_Tr_nivel_individuo$RB090,
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
datos_Th_nivel_individuo <- datos_Tr_nivel_individuo %>%
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
datos_Th_nivel_individuo <- datos_Th_nivel_individuo %>%
  left_join(datos_Td_utiles %>% dplyr::select(HB030,DB040), by = "HB030")

datos_Th_nivel_individuo <- datos_Th_nivel_individuo %>%
  mutate(across(c(Identif_personas,HB030,Personas_en_hogar_agrupado,DB040), as.factor))

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





############################################
# Fase 2: Análisis descriptivo de los datos
############################################


names(datos_Th_nivel_individuo)[names(datos_Th_nivel_individuo) == 'Identif_hogar'] <- 'HB030'
datos_Th_nivel_individuo <- datos_Th_nivel_individuo %>%
  mutate(across(c(HB030), as.numeric))

datos_graficar <- datos_Th_nivel_individuo %>%
  left_join(datos_Th %>% dplyr::select(HB030,vhPobreza), 
            by = "HB030")

names(datos_Th_nivel_individuo)[names(datos_Th_nivel_individuo) == 'HB030'] <- 'Identif_hogar'
datos_Th_nivel_individuo <- datos_Th_nivel_individuo %>%
  mutate(across(c(Identif_hogar), as.factor))

# Variable pobreza
porcentajes <- round(100 * table(datos_Th$vhPobreza) / sum(table(datos_Th$vhPobreza)), 1)

pie(table(datos_Th$vhPobreza),main = "Personas en riesgo de pobreza",
    labels = paste0(c("No", "Si"), " (", porcentajes, "%)"),
)
rm(porcentajes)

# Número de miembros del hogar
barplot(table(datos_Th_nivel_individuo$Personas_en_hogar_agrupado),ylim = 
          c(0,21000),
        xlab = "Número de miembros del hogar")

# Género

porcentajes <- round(100 * table(datos_Th_nivel_individuo$Género) / sum(table(datos_Th_nivel_individuo$Género)), 1)
etiquetas <- paste0(c("Hombre", "Mujer"), " (", porcentajes, "%)")

pie(table(datos_Th_nivel_individuo$Género), 
    labels = etiquetas, 
    main = "Género",
    col = c("red","blue"))
rm(porcentajes,etiquetas)

# CCAA
barplot(table(datos_Th_nivel_individuo$CCAA), 
        xlab = "Número de miembros del hogar",
        yaxt = "n",
        ylim = c(0, 14200))  

#Dibuja el eje Y manualmente 
axis(side = 2, at = seq(0, 14200, by = 2000), las = 1)



# Ahora cruzamos con la variable vhPobreza las anteriores
# Creamos la tabla cruzada (Filas: Pobreza, Columnas: Miembros del hogar)
tabla_cruzada <- table(datos_graficar$vhPobreza, 
                       datos_Th_nivel_individuo$Personas_en_hogar_agrupado)

# Calculamos los porcentajes por columna (margin = 2 indica que cada barra sumará 100%)
# Multiplicamos por 100 para que el eje Y muestre valores de 0 a 100
tabla_porcentajes <- prop.table(tabla_cruzada, margin = 2) * 100

# Dibujamos el barplot apilado
bp <- barplot(tabla_porcentajes, 
              beside = TRUE, 
              names.arg = c("1", "2", "3", "4", "5", "6+"),
              xlab = "Número de miembros del hogar",
              ylab = "Porcentaje (%)",
              col = c("lightblue", "salmon"),
              ylim = c(0, 115), # Subimos el límite para que quepan los textos
              yaxt = "n",
              yaxs = "i")

# Ajustamos el eje Y para que quede perfecto y sin espacios vacíos
axis(side = 2, at = seq(0, 100, by = 20), las = 1)


# Redondeamos los porcentajes a 1 decimal para que no ocupen espacio
porcentajes_texto <- paste0(round(tabla_porcentajes, 1), "%")

# Dibujamos el texto encima de cada barra
# pos = 3 coloca el texto encima de la coordenada Y 
text(x = bp, y = tabla_porcentajes, label = porcentajes_texto, pos = 3, cex = 0.8, col = "black")

# Leyenda
legend("top", legend = c("No en riesgo de pobreza", "En riesgo de pobreza"), 
       fill = c("lightblue", "salmon"), horiz = TRUE, bty = "n",cex = 0.7)
rm(porcentajes_texto,bp,tabla_cruzada,tabla_porcentajes)


# Ahora cruzamos con la variable vhPobreza las anteriores

# Pobreza y género
# Creamos la tabla cruzada (Filas: Pobreza, Columnas: Miembros del hogar)
tabla_cruzada <- table(datos_graficar$vhPobreza, 
                       datos_graficar$Género)

#tabla_porcentajes = prop.table(tabla_cruzada, margin = 2) * 100

# Extraer datos de la columna "Female" y calcular porcentajes
datos_female <- tabla_cruzada[, "Female"]
porcentajes_female <- round(100 * datos_female / sum(datos_female), 1)
labels_female <- paste0(c("No en riesgo de pobreza", "En riesgo de pobreza"), " (", porcentajes_female, "%)")

pie(datos_female, 
    labels = labels_female, 
    main = "Mujeres", 
    col = c("darkblue", "lightyellow"),
    cex = 0.7)


# Extraer datos de la columna "Male" y calcular porcentajes
datos_male <- tabla_cruzada[, "Male"]
porcentajes_male <- round(100 * datos_male / sum(datos_male), 1)
labels_male <- paste0(c("No en riesgo de pobreza", "En riesgo de pobreza"), " (", porcentajes_male, "%)")

pie(datos_male, 
    labels = labels_male, 
    main = "Hombres", 
    col = c("darkred", "lightgreen"),
    cex = 0.7)

rm(datos_female,datos_male,tabla_cruzada,porcentajes_female,porcentajes_male,labels_female,labels_male)



# Pobreza y CCAA
# Ahora cruzamos con la variable vhPobreza las anteriores
# Creamos la tabla cruzada (Filas: Pobreza, Columnas: Miembros del hogar)
tabla_cruzada <- table(datos_graficar$vhPobreza, 
                       datos_graficar$CCAA)

# Calculamos los porcentajes por columna (margin = 2 indica que cada barra sumará 100%)
# Multiplicamos por 100 para que el eje Y muestre valores de 0 a 100
tabla_porcentajes <- prop.table(tabla_cruzada, margin = 2) * 100

# Dibujamos el barplot apilado
bp <- barplot(tabla_porcentajes, 
              beside = TRUE, 
              xlab = "CCAA",
              ylab = "Porcentaje (%)",
              col = c("lightblue", "salmon"),
              ylim = c(0, 120), # Subimos el límite para que quepan los textos
              yaxt = "n",
              yaxs = "i")

# Ajustamos el eje Y para que quede perfecto y sin espacios vacíos
axis(side = 2, at = seq(0, 100, by = 20), las = 1)


# Redondeamos los porcentajes a 1 decimal para que no ocupen espacio
porcentajes_texto <- paste0(round(tabla_porcentajes, 1), "%")

# Dibujamos el texto encima de cada barra
# pos = 3 coloca el texto encima de la coordenada Y 
text(x = bp, y = tabla_porcentajes, label = porcentajes_texto, pos = 3, cex = 0.8, col = "black")

# Leyenda
legend("top", legend = c("No en riesgo de pobreza", "En riesgo de pobreza"), 
       fill = c("lightblue", "salmon"), horiz = TRUE, bty = "n",cex = 0.7)
rm(porcentajes_texto,bp,tabla_cruzada,tabla_porcentajes)




##################################
# Fase 3: Estimador directo Hajek
##################################

estim_dir_Hajek <- datos_hajek %>% 
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


#Vemos aquellas CCAA con CV >20% las cuales consideraremos areas pequeñas


##############################################
# Como tenemos una varianza = 0 en la estimación, correspondiente al dominio ES43_Female_6+, 
# la suavizaremos de la siguiente manera:
# log(\hat{V}[\hat\barY_d^DIR]) = \alpha_0 +\alpha_1 n_d + e_d

log_vector_var_Hajeck <- log(estim_dir_Hajek$estimacion_var[-132])

datos_hajek$id_dominio <- paste(datos_hajek$CCAA, 
                            datos_hajek$Género, 
                            datos_hajek$Personas_en_hogar_agrupado, 
                            sep = "_")

n_d <- as.data.frame(table(id_dominio = datos_hajek$id_dominio)) # donde Freq es el tam muestral

n_d_sin_132 <- n_d[n_d$id_dominio != "ES43_Female_6+",]

# Observamos la relación entre las variables a ajustar plot(log_vector_var_Hajeck ~ n_d_sin_132)
orden_aux <- order(n_d_sin_132$Freq) # para ordenar de mayor a menor tamaño muestral
# Ordenar los valores del eje Y en base a ese orden
y_ordenado <- log_vector_var_Hajeck[orden_aux]
# Crear el eje X con los números del 1 al 277 (longitud del vector)
x_ordenado <- 1:length(y_ordenado)
# Dibujar el gráfico con el nuevo eje X secuencial
plot(y_ordenado ~ x_ordenado)

# Hay una relación clara: al aumentar n_d disminuye log-varianza
# Sugiere algo de heterocedasticidad, podríamos probar con log(n_d)

modelo_regres <- lm(log_vector_var_Hajeck~n_d_sin_132$Freq)


# LLevamos a cabo una visualizacion del modelo de regresion 
# de la normalidad de los residuos 
qqnorm(modelo_regres$residuals)
qqline(modelo_regres$residuals)

# Llevamos a cabo otro modelo para el suavizado de la varianza
modelo_regres_log <- lm(log_vector_var_Hajeck~log(n_d_sin_132$Freq))

# LLevamos a cabo una visualizacion del modelo de regresion 
# de la normalidad de los residuos con el segundo modelo
qqnorm(modelo_regres$residuals)
qqline(modelo_regres$residuals)

# Para ver por cual nos decidimos, utilizamos AIC y BIC
# cuanto más cercano a 0 mejor porque son -2log(L), con L=verosimilitud
AIC(modelo_regres, modelo_regres_log)
BIC(modelo_regres, modelo_regres_log)

# Estima mejor el segundo modelo, luego es el que usamos
sigma2_gorro <- summary(modelo_regres_log)$sigma^2

nueva_var <- exp(modelo_regres_log$coefficients[1] + 
                  modelo_regres_log$coefficients[2]* n_d$Freq[n_d$id_dominio == "ES43_Female_6+"]
                + 1/2 * sigma2_gorro ) 
# Añadimos 0.5*sigma2_gorro como una corrección del sesgo


estim_dir_Hajek$estimacion_var[estim_dir_Hajek$id_dominio=="ES43_Female_6+"] <- nueva_var

# Recalculamos el CV de esa fila
estim_dir_Hajek$CV[estim_dir_Hajek$id_dominio=="ES43_Female_6+"] <- sqrt(estim_dir_Hajek$estimacion_var[estim_dir_Hajek$id_dominio == "ES43_Female_6+"]) / 
  abs(estim_dir_Hajek$estimacion[estim_dir_Hajek$id_dominio == "ES43_Female_6+"])


rm(datos_Th_interesantes,modelo_regres,modelo_regres_log,
   n_d_sin_132,log_vector_var_Hajeck,nueva_var)

######################################
# Gráfico CV de estimaciones directas
######################################
# Metemos el tamaño muestral por dominio en Hájek
estim_dir_Hajek <- estim_dir_Hajek %>%
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




# Representamos el gráfico dominio y estimación, ordenando el dominio por tamaño muestral (de menor a mayor)
# Debemos crear un nuevo df juntando por id_domino el tamaño muestral, la estimacion y la estimacion de la varianza
df_aux <- merge(n_d, estim_dir_Hajek[,c(-1,-2,-3)], by= "id_dominio")

orden <- order(df_aux$Freq)

# Ordenamos el df mediante el orden definido
df_ordenado <- df_aux[orden, ]
rm(df_aux)

x_ordenado <- 1:nrow(df_ordenado)

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

# df donde guardaremos todas las estimaciones, tanto Hájek, FH y EB
resultados_finales <- estim_dir_Hajek 

sacar_df = resultados_finales[,c("id_dominio", "estimacion", "estimacion_var", "CV")] %>%
  left_join(n_d %>% dplyr::select(id_dominio,Freq), 
            by = "id_dominio")
#write.csv(sacar_df, file = "", row.names = FALSE)
write.csv(sacar_df, file = file.path(ruta_salidas, "estim_hajek.csv"), row.names = FALSE)
rm(sacar_df)



#############################
# Fase 4: estimador EBLUP FH
#############################

#########################################################################
# Aqui calculamos la estimación de los tamaños poblacionales por áreas


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




# PASO 1: Identificadores de dominio en microdatos

# Creamos una columna única que defina los dominios en la encuesta
datos_Th_nivel_individuo$id_dominio <- paste(datos_Th_nivel_individuo$CCAA, 
                                             datos_Th_nivel_individuo$Género, 
                                             datos_Th_nivel_individuo$Personas_en_hogar_agrupado, 
                                             sep = "_")





# PASO 2: Cálculo de tamaños poblacionales (domsize)

# El paquete sae exige exactamente estas dos columnas: identificador y tamaño,
# para aplicar después la función direct,
# creamos un df donde guarde el ID del dominio y su tamaño poblacional estimado(gorroN_d)
df_domsize <- data.frame(
  Domain = totales_poblacionales_estimados$id_dominio,
  Size = totales_poblacionales_estimados$suma_pesos
)



# PASO 3: Construcción de la matriz X a nivel de área


# Definimos la lista con todas tus variables (factores y numéricas)
# creamos un vector con el nombre de las variables primero numéricas y después factores
#variables_interes <- c(names(datos_Th_nivel_individuo)[sapply(datos_Th_nivel_individuo, is.numeric)], 
#                    names(datos_Th_nivel_individuo)[sapply(datos_Th_nivel_individuo, is.factor)])


variables_interes <- c(names(datos_Th_nivel_individuo)[sapply(datos_Th_nivel_individuo, is.factor)])

variables_interes <- variables_interes[!(variables_interes %in% 
                                           c("RB030","Pesos","Género","Identif_personas","Identif_hogar","Personas_en_hogar_agrupado","CCAA"))]


# Inicializamos el data frame base con la columna de unos
matriz_X_area <- data.frame(
  Domain = df_domsize$Domain,
  Intercept = 1
)


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
      
      # 4) Unimos a nuestro data frame general (matriz_X_area)
      matriz_X_area <- merge(matriz_X_area, est_reducida, by = "Domain", all.x = TRUE)
      
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
    matriz_X_area <- merge(matriz_X_area, est_reducida, by = "Domain", all.x = TRUE)
    
  }
}

# Para liberar memoria en caso de necesitarlo
rm(nivel,niveles,niveles_a_crear,var,nombre_dummy,estimacion,estimacion_temp,est_reducida)

# 3.1 comprobamos si hay Na's en la matriz. Si es así, los imputaremos por la mediana de dicha área.

sum(is.na(matriz_X_area))
which(is.na(matriz_X_area), arr.ind = TRUE)

# Se observa que los NA's provienen de la variable HS150 en dos áreas concretas,
# luego decidimos eliminar esas dos columnas porque la matrix para el eblup no puede contener NA's
# Otra opción sería imputa pero lo descartamos.
#matriz_X_area$HS150_2 <- NULL
#matriz_X_area$HS150_3 <- NULL

# Comprobación final:
sum(is.na(matriz_X_area))==0


# 4. Formateo final de la matriz_X_area
rownames(matriz_X_area) <- matriz_X_area$Domain
matriz_X_area <- as.matrix(matriz_X_area[, -1])

# Visualizamos las dimensiones y los primeros datos
print(paste("La matriz tiene", nrow(matriz_X_area), "filas y", ncol(matriz_X_area), "columnas."))
#head(matriz_X_area)



# Antes de aplicar el modelo eblupFH a la matriz_X_area de covariables a nivel de área, 
# comprobamos que no haya multicolinealidad fuerte entre dichas covariables.

df_matriz_X <- as.data.frame(matriz_X_area[,-1])

solo_estim_dir_Hajek <- estim_dir_Hajek$estimacion
df_matriz_X$y <- solo_estim_dir_Hajek  #variable respuesta a nivel de area

modelo_area <- lm(y ~ ., data = df_matriz_X)
vif(modelo_area)

########################################
# El Factor de Inflación de la Varianza (VIF) se utiliza para estudiar la multicolinealidad 
# porque cuantifica directamente cuánto aumenta la varianza (y por ende el error estándar) 
# de un coeficiente de regresión estimado debido a la correlación con otras variables independientes. 
# Permite identificar variables redundantes que inestabilizan el modelo.
########################################

#Como vif nos dice que hay variables que son comb. lineales, veámos cuales son: 
alias(modelo_area)

# Observamos que no hay comb. lineales entre las variables cualitativas.


#Buscamos y eliminamos aquellas variables que tengan un vif>10, pues implica multicolinealidad
modelo_prueba <- lm(y~., data = subset(df_matriz_X, select = -c(HS021_2,HS021_3,HS060_2,HS120_2,HS120_3,HS120_4,HS120_5,HS120_6,HC300_2,HC300_3,HC300_4)))
vif(modelo_prueba)
summary(vif(modelo_prueba))
hist(vif(modelo_prueba))

# Modelo AIC
modelo_area_aic <- step(modelo_prueba, direction = "both")

# Modelo BIC
modelo_area_bic <- step(modelo_prueba,direction = "both", k = log(nrow(subset(df_matriz_X, select = -c(HS021_2,HS021_3,HS060_2,HS120_2,HS120_3,HS120_4,HS120_5,HS120_6,HC300_2,HC300_3,HC300_4)))))

formula(modelo_area_aic)
formula(modelo_area_bic)

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

vif(modelo_area_bic)
# Se observa que hay menor colinealidad en las variables del modelo BIC


# Observamos si son significativas las variables del BIC con:
summary(modelo_area_bic)

matriz_X_area_final <- model.matrix(
  ~ HS022_2 + HS040_2 + HS110_3 + HD080_2 + HH010_2 + 
    HH010_3 + HH021_4 + HH050_2 + HI010_2 + HI010_3,
  data = df_matriz_X )


estim.FH.res <- eblupFH(solo_estim_dir_Hajek~matriz_X_area_final-1,vardir= estim_dir_Hajek$estimacion_var)
estim.FH.mse <- mseFH(solo_estim_dir_Hajek~matriz_X_area_final-1,vardir= estim_dir_Hajek$estimacion_var)

estim.FH.cv <- 100*sqrt(estim.FH.mse$mse)/estim.FH.res$eblup

cbind(directa.cv = estim_dir_Hajek$CV, FH = estim.FH.cv ,muestra = table(datos_Th_nivel_individuo$id_dominio) )

#Limpiamos la memoria
rm(datos_Td_utiles,totales_poblacionales_estimados,matriz_X_area,modelo_area,modelo_area_aic,modelo_area_bic)


# Representacion gráfica de estimacion y ECM
resultados_finales$estim_EBLUP <- estim.FH.res$eblup

df_aux <- merge(n_d, resultados_finales[,c(-1,-2,-3)], by= "id_dominio")

orden <- order(df_aux$Freq)

#Ordenamos el df mediante el orden definido
df_ordenado <- df_aux[orden, ]
rm(df_aux)

x_ordenado <- 1:nrow(df_ordenado)

rango_y <- range(c(df_ordenado$estimacion, df_ordenado$estim_EBLUP), na.rm = TRUE)
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
resultados_finales$estim_EBLUP_ECM <- estim.FH.mse$mse

df_aux <- merge(n_d, resultados_finales[,c(-1,-2,-3)], by= "id_dominio")

#Ordenamos el df mediante el orden definido
df_ordenado <- df_aux[orden, ]
rm(df_aux)

x_ordenado <- 1:nrow(df_ordenado)

rango_y <- range(c(df_ordenado$estimacion, df_ordenado$estim_EBLUP_ECM), na.rm = TRUE)
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


# CV de la estimación FH
resultados_finales <- resultados_finales %>% 
  mutate(FH_CV = 100*sqrt(estim_EBLUP_ECM)/estim_EBLUP)

resultados_finales <- resultados_finales %>%
  arrange(Freq)%>%
  mutate(Calidad_FH = ifelse(FH_CV > 20, "Inaceptable (CV > 20%)", "Aceptable (CV <= 20%)"))

resultados_finales <- resultados_finales %>%
  arrange(Freq) %>%
  mutate(
    FH_Dominio_Index = row_number()
  )


ggplot(resultados_finales, aes(x = FH_Dominio_Index, y = FH_CV)) +
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


# Los residuos estandarizados son importantes porque en FH cada dominio tiene una
# varianza de muestreo distinta.

# 1. Residuos vs Fitted

# Extraemos beta_goro porque fitted = X %*% beta
beta_gorro_FH <- estim.FH.res$fit$estcoef$beta

fitted_FH <- as.vector(matriz_X_area_final %*% beta_gorro_FH)

# Calculamos los residuos
residuos_FH <- resultados_finales$estimacion - fitted_FH

# Gráfico de fitted vs residuals de eblup FH a nivel de área 
plot(fitted_FH,residuos_FH)
lines(lowess(fitted_FH, residuos_FH),
      col = "red",
      lwd = 2)

# Observamos cuales son los más extremos
which(abs(residuos_FH) > 0.2)


# 2. Residuos estandarizados vs Fitted

# Calculamos los residuos estandarizados
sigma2_u_gorro <- estim.FH.res$fit$refvar
residuos_estand_FH <- residuos_FH / sqrt(resultados_finales$estimacion_var + sigma2_u_gorro)

# Graficamos
plot(fitted_FH,residuos_estand_FH)
lines(lowess(fitted_FH, residuos_estand_FH),
      col = "red",
      lwd = 2)

# Observamos cuales son los más extremos
which(abs(residuos_estand_FH) > 2)



# 3. Q-Q plot 
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
#Se observa que la mayor parte de los residuos se alinean razonablemente con la recta de referencia, 
#lo que sugiere que la hipótesis de normalidad es adecuada para la mayoría de los dominios. 
#No obstante, se aprecian desviaciones en ambas colas, especialmente en la cola superior, 
#lo que indica la presencia de algunos dominios extremos. 
#En conjunto, el supuesto de normalidad puede considerarse aceptablemente satisfecho, 
#aunque conviene analizar la influencia de los dominios con residuos estandarizados más elevados.


# 4. Histograma de los residuos estandarizados 

hist(residuos_estand_FH, prob = TRUE)



########################################
# Fase 5: estimacion EB nivel individuo
########################################

# Transformación necesaria para tener la variable vhRentaa / unidades de consumo

indices_OCD <- datos_Th[,c("vhRentaa","HX240","HB030")]
names(indices_OCD)[names(indices_OCD) == 'HB030'] <- 'DB030' #Identificador del hogar
names(indices_OCD)[names(indices_OCD) == 'HX240'] <- 'unds_consumo'

# Creamos una nueva columna dividiendo la renta entre las unidades de consumo

indices_OCD$renta <- indices_OCD$vhRentaa/indices_OCD$unds_consumo

names(datos_Th_nivel_individuo)[names(datos_Th_nivel_individuo) == 'HX240'] <- 'unds_consumo'
datos_Th_nivel_individuo$renta <- datos_Th_nivel_individuo$vhRentaa/datos_Th_nivel_individuo$unds_consumo

# Buscamos el k óptimo para log(renta + k)
hist(datos_Th_nivel_individuo$renta, prob = TRUE)

# Filtramos por aquellos individuos con renta mayor o igual a 0

final_datos_Th_individuo <- datos_Th_nivel_individuo %>%
  filter(vhRentaa >= 0) 

hist(final_datos_Th_individuo$renta, prob = TRUE)

#################################################################
# Selección de la constante k óptimo por simetría de residuos


# Rejilla de valores candidatos para k

# El mínimo válido es el que garantiza vhRenta + k > 0 para todas las obs.
# Si hay valores negativos o cero en vhRenta, k debe superar ese mínimo.
minimo_valido <- min(final_datos_Th_individuo[["renta"]]) 
cat("Valor mínimo válido de k:", minimo_valido, "\n")

# Ajusta el paso según la escala vhRentaa, empezando por 1700 porque el resto de valores da 0
grid_k <- seq(from = minimo_valido, to = minimo_valido + 15000, by = 500)


# Función que calcula el skewness de log(vhRenta + k) para un k dado

evaluar_k <- function(k_val, datos, var_renta) {
  y_transf <- log(datos[[var_renta]] + k_val)
  data.frame(
    k        = k_val,
    skewness = e1071::skewness(y_transf),
    kurtosis = e1071::kurtosis(y_transf)
  )
}


# Recorrer la rejilla 

resultados_k <- do.call(rbind, lapply(grid_k, function(k_val) {
  evaluar_k(k_val, final_datos_Th_individuo, "renta")
}))


# Elegir el k que minimiza |skewness|

k_optimo <- resultados_k$k[which.min(abs(resultados_k$skewness))]
cat("k óptimo (mínima |skewness|):", k_optimo, "\n")
print(resultados_k[which.min(abs(resultados_k$skewness)), ])


# Gráfico: skewness en función de k

ggplot(resultados_k, aes(x = k, y = skewness)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_vline(xintercept = k_optimo, linetype = "dotted", color = "blue") +
  labs(
    title = "Asimetría de log(vhRenta + k) según el valor de k",
    x = "k (constante de desplazamiento)",
    y = "Skewness"
  ) +
  theme_minimal()


# Histograma final con el k óptimo

final_datos_Th_individuo$y_transf_final <- log(final_datos_Th_individuo[["renta"]] + k_optimo)

media_y <- mean(final_datos_Th_individuo$y_transf_final)
sd_y    <- sd(final_datos_Th_individuo$y_transf_final)

xlim_centrado <- media_y + c(-4, 4) * sd_y

png(file.path(ruta_salidas, "histograma_log_renta_k_optimo.png"), width = 700, height = 500)
hist(final_datos_Th_individuo$y_transf_final,
     main = paste("Histograma de log(renta + k), k =", k_optimo),
     xlab = "log(renta + k)",
     xlim = xlim_centrado,
     prob = TRUE)
dev.off()


# Comparación visual: histograma sin transformar vs transformado

png(file.path(ruta_salidas, "histograma_comparacion_k_optimo.png"), width = 1000, height = 500)
par(mfrow = c(1, 2))
hist(final_datos_Th_individuo$renta,
     main = "Histograma de la renta sin transformar", xlab = "Renta",
     prob = TRUE)
hist(final_datos_Th_individuo$y_transf_final, 
     main = paste("log(renta + k), k =", k_optimo),
     xlab = "log(renta + k)",
     prob = TRUE,
     xlim = xlim_centrado)
dev.off()


# k queda guardado para usar después en ebBHF

#k_optimo <- 8000
cat("Usando k =", k_optimo, "\n")

#Q: para salir del entorno temporal


# Limpieza

rm(grid_k,minimo_valido,evaluar_k)


# Nueva variable respuesta: Y_di = log(renta + k_optimo)


###############################################
# Gráficos comparativa resultados

# 2 boxplot 
boxplot(final_datos_Th_individuo$y_transf_final ~ final_datos_Th_individuo$Género)
ggplot(final_datos_Th_individuo, aes(x = Género, y = y_transf_final, fill = Género)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Comparación de ingresos transformados por género",
       x = "Género",
       y = "Ingresos transformados") +
  scale_fill_brewer(palette = "Set2")


ggplot(final_datos_Th_individuo, aes(x = CCAA, y = y_transf_final, fill = CCAA)) +
  geom_boxplot(fill = "white", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Comparación de ingresos transformados por CCAA",
       x = "CCAA",
       y = "Renta transformada") +
  scale_fill_brewer(palette = "Greys")




####################################################
# Elección de variables auxiliares a nivel individuo

datos_Tr_nivel_individuo$Identif_hogar <- as.factor(datos_Tr_nivel_individuo$Identif_hogar)
datos_Tr_nivel_individuo$Identif_personas <- as.factor(datos_Tr_nivel_individuo$Identif_personas)

datos_Th_nivel_individuo$Identif_hogar <-  datos_Th_nivel_individuo$RB030%/%100
datos_Th_nivel_individuo$Identif_hogar <- as.factor(datos_Th_nivel_individuo$Identif_hogar)

datos_Th_nivel_individuo <- datos_Th_nivel_individuo %>%
  left_join(datos_Tr_nivel_individuo %>% dplyr::select(grupo_edad,Identif_hogar,Identif_personas)%>%
              distinct(), 
            by = c("Identif_hogar","Identif_personas"))



variables_interes <- c(names(datos_Th_nivel_individuo)[sapply(datos_Th_nivel_individuo, is.factor)])

variables_interes <- variables_interes[!(variables_interes %in% 
                                           c("RB030","Pesos","Género","Identif_personas","Identif_hogar","Personas_en_hogar_agrupado","CCAA","y_trans_final","vhRentaa","vhRentaaAla","vhMATDEP"))]

# Estudio previo de NAs de variables de interes
colSums(is.na(datos_Th_nivel_individuo[, variables_interes]))

# Nos quedamos con aquellas variables con menos de 100 NAs
variables_interes <- c("HS022","HS090","HS110","HD080","HH021","HC190","grupo_edad")

# Imputamos los NA 
# Función para calcular la moda
moda <- function(x) {
  ux <- na.omit(x)
  names(sort(table(ux), decreasing = TRUE))[1]
}

# Imputación por la moda
for (var in variables_interes) {
  
  # Número de NA
  num_na <- sum(is.na(datos_Th_nivel_individuo[[var]]))
  
  if (num_na > 0) {
    
    # Calculamos la moda
    valor_moda <- moda(datos_Th_nivel_individuo[[var]])
    
    # Imputamos los NA
    datos_Th_nivel_individuo[[var]][
      is.na(datos_Th_nivel_individuo[[var]])
    ] <- valor_moda
    
    cat(var, ": se imputaron", num_na,
        "NA con la moda (", valor_moda, ")\n")
  }
}

rm(moda,valor_moda,var,num_na)

formula_X <- as.formula(
  paste("~", paste(variables_interes, collapse = " + "))
)

matriz_X_individuos <- model.matrix(formula_X, data = final_datos_Th_individuo)

# Renombramos columnas
for (v in variables_interes) {
  colnames(matriz_X_individuos) <- sub(paste0("^", v), paste0(v, "_"), colnames(matriz_X_individuos))
}
rm(v)

# Comprobamos si hay Na's en la matriz.
sum(is.na(matriz_X_individuos))

# Visualizamos las dimensiones y los primeros datos
print(paste("La matriz tiene", nrow(matriz_X_individuos), "filas y", ncol(matriz_X_individuos), "columnas."))


# Antes de aplicar el modelo eblupFH a la matriz_X de covariables a nivel de área, 
# comprobamos que no haya multicolinealidad fuerte entre dichas covariables.
df_matriz_X_indiv <- as.data.frame(matriz_X_individuos[,-1])


df_matriz_X_indiv$y <- final_datos_Th_individuo$y_transf_final  #variable respuesta a nivel de individuo

modelo_indiv <- lm(y ~ ., data = df_matriz_X_indiv)
vif(modelo_indiv)


# Observamos que no hay comb. lineales entre las variables cualitativas.
alias(modelo_indiv)

# Decidimos quedarnos con aquellas que más relación tenga con la renta
# luego eliminamos HS090,HD080 y HH190
modelo_indiv_final <- lm(y~., data = subset(df_matriz_X_indiv, select = -c(HS090_2,HS090_3,HD080_2,HD080_3,HC190_2)))
vif(modelo_indiv_final)
summary(modelo_indiv_final) # todas las categorías son significativas
hist(vif(modelo_indiv_final))

rm(modelo_indiv,formula_X)

# Modelo AIC
modelo_indiv_aic <- step(modelo_indiv_final, direction = "both")

# Modelo BIC
modelo_indiv_bic <- step(modelo_indiv_final,direction = "both", k = log(nrow(subset(df_matriz_X_indiv, select = -c(HS090_2,HS090_3,HD080_2,HD080_3,HC190_2)))))

formula(modelo_indiv_aic)
formula(modelo_indiv_bic)
# Ambas coinciden y son significativas

# Por tanto la matriz X nivel individuo final es:
matriz_X_indiv_final <- model.matrix(
  ~ HS022_2 + HS110_2 + HS110_3 + HH021_2 + HH021_3 + 
    HH021_4 + HH021_5 + `grupo_edad_18-34` + `grupo_edad_35-49` + 
    `grupo_edad_50-64` + `grupo_edad_65+`,
  data = df_matriz_X_indiv )


rm(df_matriz_X_indiv,df_matriz_X,modelo_indiv_aic,modelo_indiv_bic,matriz_X_individuos)


# Función de normalización de nombres 

limpiar_nombres <- function(nombres) {
  nombres <- gsub("`", "", nombres)      # quita backticks literales si los hubiera
  nombres <- gsub("-", "_", nombres)     # "18-34" -> "18_34"
  nombres <- gsub("\\+", "plus", nombres) # "65+"   -> "65plus"
  nombres
}

colnames(matriz_X_indiv_final) <- limpiar_nombres(colnames(matriz_X_indiv_final))
matriz_X_indiv_final <- matriz_X_indiv_final[, colnames(matriz_X_indiv_final) != "(Intercept)", drop = FALSE]


print(colnames(matriz_X_indiv_final))

# Observamos los residuos del modelo final (antes con todas las variables auxiliares posibles)
qqplot(residuals(modelo_indiv_final));qqline(residuals(modelo_indiv_final), col = 'red')

hist(residuals(modelo_indiv_final))


#####################################################
# Pasamos ahora a modelo errores anidados con el EB 

# Parámetros 

var_area  <- "id_dominio"
var_peso  <- "Pesos"
var_renta <- "renta"

vars_raw_finales <- c("HS022", "HS110", "HH021", "grupo_edad") # variables auxiliares


# Comprobaciones antes de construir el pseudo-censo

stopifnot(all(!is.na(datos_Th_nivel_individuo[[var_area]])))
stopifnot(all(final_datos_Th_individuo[[var_peso]] >= 1))
# Hay 3 con peso 0, luego eliminamos dichas filas
casos_peso_cero <- final_datos_Th_individuo[
  final_datos_Th_individuo[[var_peso]] == 0,
]
print(casos_peso_cero[, c(var_area, var_peso, var_renta)])

cat("Número de observaciones excluidas por peso = 0:",
    nrow(casos_peso_cero), "\n")

# Excluir del data.frame antes de construir el pseudo-censo y ajustar el modelo
datos_Th_nivel_individuo <- datos_Th_nivel_individuo[
  datos_Th_nivel_individuo[[var_peso]] > 0,
]

# Volver a comprobar
stopifnot(all(final_datos_Th_individuo[[var_peso]] >= 1))

# Limpieza de matriz_X_nivel_individuo
#matriz_X_indiv_final <- matriz_X_indiv_final[-c(4271, 9640, 9641),]

areas <- sort(unique(final_datos_Th_individuo[[var_area]]))
cat("Número de dominios:", length(areas), "\n")


# Redondeo estocástico del peso

set.seed(2024)
redondeo_estocastico <- function(w) {
  parte_entera  <- floor(w)
  parte_decimal <- w - parte_entera
  ajuste <- rbinom(length(w), size = 1, prob = parte_decimal)
  parte_entera + ajuste
}


# Construir el pseudo-censo (Xnonsample) por dominio,
# liberando memoria en cada iteración

#dir_censos_csv <- "pseudo_censo_csv_por_area"
dir_censos_csv <- file.path(ruta_salidas, "pseudo_censo_csv_por_area")
dir.create(dir_censos_csv, showWarnings = FALSE)

niveles_HS022      <- levels(as.factor(final_datos_Th_individuo$HS022))
niveles_HS110      <- levels(as.factor(final_datos_Th_individuo$HS110))
niveles_HH021      <- levels(as.factor(final_datos_Th_individuo$HH021))
niveles_grupo_edad <- levels(as.factor(final_datos_Th_individuo$grupo_edad))

cols_finales <- colnames(matriz_X_indiv_final)

for (a in areas) {
  
  cat("Construyendo y codificando censo, dominio:", a, "\n")
  
  datos_area <- final_datos_Th_individuo[
    final_datos_Th_individuo[[var_area]] == a,
    c(var_area, var_peso, vars_raw_finales)
  ]
  
  n_replicas_totales <- redondeo_estocastico(datos_area[[var_peso]])
  n_replicas_totales[n_replicas_totales < 1] <- 1
  n_extra <- n_replicas_totales - 1
  idx_extra <- rep(seq_len(nrow(datos_area)), times = n_extra)
  
  censo_area <- datos_area[idx_extra, c(var_area, vars_raw_finales), drop = FALSE]
  rownames(censo_area) <- NULL
  
  # Forzar mismos niveles que la muestra completa
  censo_area$HS022      <- factor(censo_area$HS022,      levels = niveles_HS022)
  censo_area$HS110      <- factor(censo_area$HS110,      levels = niveles_HS110)
  censo_area$HH021      <- factor(censo_area$HH021,      levels = niveles_HH021)
  censo_area$grupo_edad <- factor(censo_area$grupo_edad, levels = niveles_grupo_edad)
  
  mm_area <- model.matrix(~ HS022 + HS110 + HH021 + grupo_edad, data = censo_area)
  mm_area <- mm_area[, colnames(mm_area) != "(Intercept)", drop = FALSE]
  
  nombres_mm <- colnames(mm_area)
  nombres_mm <- sub("^HS022", "HS022_", nombres_mm)
  nombres_mm <- sub("^HS110", "HS110_", nombres_mm)
  nombres_mm <- sub("^HH021", "HH021_", nombres_mm)
  nombres_mm <- sub("^grupo_edad", "grupo_edad_", nombres_mm)
  nombres_mm <- limpiar_nombres(nombres_mm)
  colnames(mm_area) <- nombres_mm
  
  faltantes <- setdiff(cols_finales, colnames(mm_area))
  if (length(faltantes) > 0) {
    cat("  -> Faltan columnas en dominio", a, ":", paste(faltantes, collapse=", "), "\n")
  }
  mm_area <- mm_area[, cols_finales, drop = FALSE]
  
  chunk <- cbind(id_dominio = a, as.data.frame(mm_area))
  
  write.csv(
    chunk,
    file.path(dir_censos_csv, paste0("Xnonsample_area_", a, ".csv")),
    row.names = FALSE
  )
  
  cat("  -> filas:", nrow(chunk), "\n")
  
  rm(datos_area, censo_area, idx_extra, n_replicas_totales, n_extra, mm_area, chunk)
  gc(verbose = FALSE)
}



# Línea de pobreza 

mediana_ponderada <- function(x, w) {
  ord <- order(x); x <- x[ord]; w <- w[ord]
  cw <- cumsum(w) / sum(w)
  x[which(cw >= 0.5)[1]]
}
linea_pobreza <- 0.6 * mediana_ponderada(
  final_datos_Th_individuo[[var_renta]],
  final_datos_Th_individuo[[var_peso]]
)
cat("Línea de pobreza:", linea_pobreza, "\n")

indicador_fgt0 <- function(y) mean(y < linea_pobreza)


# Aplicación de ebBHF

stopifnot(all(final_datos_Th_individuo[[var_renta]] + k_optimo > 0))

df_para_ebBHF <- as.data.frame(matriz_X_indiv_final)
df_para_ebBHF[[var_renta]] <- final_datos_Th_individuo[[var_renta]]

df_para_ebBHF$id_dominio <- final_datos_Th_individuo[[var_area]]

stopifnot(nrow(df_para_ebBHF) == nrow(final_datos_Th_individuo))

# Construir la fórmula con nombres explícitos 
nombres_covariables <- colnames(matriz_X_indiv_final)

formula_ebBHF <- as.formula(
  paste(var_renta, "~", paste(nombres_covariables, collapse = " + "), "- 1")
)

print(formula_ebBHF)

resultados_por_area <- vector("list", length(areas))

for (i in seq_along(areas)) {
  
  a <- areas[i]
  cat("Ejecutando ebBHF, dominio:", a, "(", i, "/", length(areas), ")\n")
  
  Xnonsample_a <- read.csv(
    file.path(dir_censos_csv, paste0("Xnonsample_area_", a, ".csv")),
    stringsAsFactors = FALSE
  )
  
  # read.csv puede cambiar el tipo de id_dominio (numérico/carácter);
  # aseguramos que coincide con el tipo de 'areas' y de df_para_ebBHF$id_dominio
  Xnonsample_a$id_dominio <- as(Xnonsample_a$id_dominio, class(areas))
  
  set.seed(2024)
  res_a <- ebBHF(
    formula    = formula_ebBHF,
    dom        = id_dominio,
    selectdom  = a,                 # <- un único dominio en esta llamada
    Xnonsample = Xnonsample_a,
    MC         = 100,
    data       = df_para_ebBHF,     # <- muestra completa, todas las áreas
    transform  = "BoxCox",
    lambda     = 0,
    constant   = k_optimo,
    indicator  = indicador_fgt0
  )
  
  resultados_por_area[[i]] <- res_a$eb
  
  rm(Xnonsample_a, res_a)
  gc(verbose = FALSE)
}

resultado_eb_final <- do.call(rbind, resultados_por_area)
resultado_eb_final

# Renombramos

resultado_eb_final <- resultado_eb_final %>%
  rename(id_dominio = domain, EB = eb )

write.csv(resultado_eb_final, file.path(ruta_salidas, "EB_por_area.csv"), row.names = FALSE)



# Juntamos con demás estimaciones
resultados_finales <- resultados_finales %>%
  left_join(resultado_eb_final %>%dplyr::select(id_dominio, EB),
            by = "id_dominio"
  )

###########################################
# Gráfico: Hajek vs EBLUP (FH) vs EB (BHF)



# Graficamos directos, FH y EB
df_aux <- merge(n_d, resultados_finales[, c(-1, -2, -3)], by = "id_dominio")
orden <- order(df_aux$Freq)
df_ordenado <- df_aux[orden, ]
rm(df_aux)

x_ordenado <- 1:nrow(df_ordenado)
rango_y <- range(
  c(df_ordenado$estimacion, df_ordenado$estim_EBLUP, df_ordenado$EB),
  na.rm = TRUE
)
plot(df_ordenado$estimacion ~ x_ordenado,
     ylab = "Estimación",
     xlab = "Dominios",
     ylim = rango_y,
     type = "b",
     col = "green",
     lty = 1,
     pch = 19,
     lwd = 1.7)

lines(df_ordenado$estim_EBLUP ~ x_ordenado,
      type = "b",
      pch = 18,
      col = "darkblue",
      lty = 1,
      lwd = 1.5)

lines(df_ordenado$EB ~ x_ordenado,
      type = "b",
      pch = 17,
      col = "darkred",
      lty = 1,
      lwd = 1.5)

legend("topright",
       legend = c("Hajek", "EBLUP (FH)", "EB (BHF)"),
       col = c("green", "darkblue", "darkred"),
       pch = c(19, 18, 17),
       lty = 1,
       lwd = c(1.7, 1.6, 1.6),
       bty = "n")

rm(rango_y)




############################
# Ahora para estimar el ECM
#############################
B_boot <- 200   # nº de réplicas bootstrap  
MC_mc  <- 50    

resultados_mse_por_area <- vector("list", length(areas))

for (i in seq_along(areas)) {
  
  a <- areas[i]
  cat("Ejecutando pbmseebBHF, dominio:", a, "(", i, "/", length(areas), ")\n")
  
  Xnonsample_a <- read.csv(
    file.path(dir_censos_csv, paste0("Xnonsample_area_", a, ".csv")),
    stringsAsFactors = FALSE
  )
  
  # Asegurar mismo tipo que id_dominio en df_para_ebBHF / areas
  Xnonsample_a$id_dominio <- as(Xnonsample_a$id_dominio, class(areas))
  
  set.seed(2024)
  
  res_a <- tryCatch({
    pbmseebBHF(
      formula    = formula_ebBHF,
      dom        = id_dominio,
      selectdom  = a,                 # <- un único dominio en esta llamada
      Xnonsample = Xnonsample_a,
      B          = B_boot,
      MC         = MC_mc,
      data       = df_para_ebBHF,     # <- muestra completa, todas las áreas
      transform  = "BoxCox",
      lambda     = 0,
      constant   = k_optimo,
      indicator  = indicador_fgt0
    )
  }, error = function(e) {
    cat("  -> ERROR en dominio", a, ":", conditionMessage(e), "\n")
    NULL
  })
  
  if (!is.null(res_a)) {
    resultados_mse_por_area[[i]] <- res_a$mse
  }
  
  rm(Xnonsample_a, res_a)
  gc(verbose = FALSE)
}

prueba_mse <- do.call(rbind, resultados_mse_por_area)

# Renombramos
prueba_mse <- prueba_mse%>%rename(id_dominio = domain,estim_EB_ECM = mse)


# Juntamos con demás estimaciones
resultados_finales <- resultados_finales %>% 
  left_join(prueba_mse %>%dplyr::select(id_dominio, estim_EB_ECM),
            by = "id_dominio")


# CV
resultados_finales$EB_CV <- 100 * sqrt(resultados_finales$estim_EB_ECM) / resultados_finales$EB


prueba_mse = prueba_mse %>%
  left_join(n_d %>% dplyr::select(id_dominio, Freq), 
            by = "id_dominio")

prueba_mse = prueba_mse %>%
  left_join(resultados_finales %>% dplyr::select(id_dominio, estimacion_var, estim_EBLUP_ECM), 
            by = "id_dominio")

##################### 
# Gráficos ECM
#####################
orden = order(prueba_mse$Freq)

#Ordenamos el df mediante el orden definido
df_ordenado <- prueba_mse[orden, ]

# Nos quedamos solo con los 50 primeros dominios
df_50 <- df_ordenado[1:50, ]
#df_50 <- tail(df_ordenado, 50)

#x_ordenado = 1:nrow(df_ordenado)
# Eje X
x_50 <- 1:nrow(df_50)

# Límites del eje Y para ampliar las diferencias
y_min <- min(c(df_50$estimacion_var,
               df_50$estim_EBLUP_ECM,
               df_50$estim_EB_ECM), na.rm = TRUE)

y_max <- max(c(df_50$estimacion_var,
               df_50$estim_EBLUP_ECM,
               df_50$estim_EB_ECM), na.rm = TRUE)

# Un pequeño margen
margen <- (y_max - y_min) * 0.05

# Representamos el gráfico de área y ECM, ordenando el área por tamaño muesrtal (en porcentaje df_ordenado$estimacion_var *100)
plot(df_50$estimacion_var ~x_50, #df_ordenado$estimacion_var~x_ordenado
     ylab = "ECM",
     xlab = "Dominios",
     type = "b",
     col = 'green',
     lty = 1,
     pch = 19,
     lwd = 1.5,
     ylim = c(y_min - margen, y_max + margen))

lines(df_50$estim_EBLUP_ECM ~ x_50, #df_ordenado$estim_EBLUP_ECM~x_ordenado
      type = "b", 
      pch = 18,           
      col = "darkblue",   
      lty = 1, 
      lwd = 1.5) 

lines(df_50$estim_EB_ECM ~ x_50, #df_ordenado$estim_EB_ECM~x_ordenado
      type = "b", 
      pch = 17,           
      col = "red",   
      lty = 1, 
      lwd = 1) 


legend("topright", 
       legend = c("Hajek", "EBLUP", "EB"),
       col = c("green", "darkblue","red"), 
       pch = c(19, 18, 17), 
       lty = 1, 
       lwd = c(1.5, 1.5, 1.5),
       bty = "n") 



x_ordenado = 1:nrow(df_ordenado)
plot(df_ordenado$estimacion_var~x_ordenado, #df_ordenado$estimacion_var~x_ordenado
     ylab = "ECM",
     xlab = "Dominios",
     type = "b",
     col = 'green',
     lty = 1,
     pch = 19,
     lwd = 1.5)

lines(df_ordenado$estim_EBLUP_ECM~x_ordenado, #df_ordenado$estim_EBLUP_ECM~x_ordenado
      type = "b", 
      pch = 18,           
      col = "darkblue",   
      lty = 1, 
      lwd = 1.5) 

lines(df_ordenado$estim_EB_ECM~x_ordenado, #df_ordenado$estim_EB_ECM~x_ordenado
      type = "b", 
      pch = 17,           
      col = "red",   
      lty = 1, 
      lwd = 1) 


###############
# Gráfico CV
###############

orden = order(resultados_finales$Freq)

#Ordenamos el df mediante el orden definido
df_auxiliar <- resultados_finales[orden, ]
x_ordenado_auxiliar <- 1:nrow(df_auxiliar)

# Representamos el gráfico de área y ECM, ordenando el área por tamaño muesrtal (en porcentaje df_ordenado$estimacion_var *100)
plot(df_auxiliar$CV ~x_ordenado_auxiliar, #df_ordenado$estimacion_var~x_ordenado
     ylab = "CV estimados",
     xlab = "Dominios",
     type = "b",
     col = 'green',
     lty = 1,
     pch = 19,
     lwd = 1.5)

lines(df_auxiliar$FH_CV ~x_ordenado_auxiliar, #df_ordenado$estim_EBLUP_ECM~x_ordenado
      type = "b", 
      pch = 18,           
      col = "darkblue",   
      lty = 1, 
      lwd = 1.5) 

lines(df_auxiliar$EB_CV ~x_ordenado_auxiliar, #df_ordenado$estim_EB_ECM~x_ordenado
      type = "b", 
      pch = 17,           
      col = "red",   
      lty = 1, 
      lwd = 1) 


legend("topright", 
       legend = c("Hajek", "EBLUP", "EB"),
       col = c("green", "darkblue","red"), 
       pch = c(19, 18, 17), 
       lty = 1, 
       lwd = c(1.5, 1.5, 1.5),
       bty = "n")




