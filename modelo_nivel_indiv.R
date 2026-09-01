
################################################################################
# A nivel de individuo
################################################################################
#0)Transformación necesaria para tener la variable vhRentaa / unidades de consumo

indices_OCD<-datos_Th[,c("vhRentaa","HX240","HB030")]
names(indices_OCD)[names(indices_OCD) == 'HB030'] <- 'DB030' #Identificador del hogar
names(indices_OCD)[names(indices_OCD) == 'HX240'] <- 'unds_consumo'

# Creamos una nueva columna dividiendo la renta entre las unidades de consumo

indices_OCD$renta<- indices_OCD$vhRentaa/indices_OCD$unds_consumo

names(datos_Th_nivel_individuo)[names(datos_Th_nivel_individuo) == 'HX240'] <- 'unds_consumo'
datos_Th_nivel_individuo$renta <- datos_Th_nivel_individuo$vhRentaa/datos_Th_nivel_individuo$unds_consumo

# 1) Buscamos el c óptimo para log(renta + c)
hist(datos_Th_nivel_individuo$renta, prob = TRUE)

# Filtramos por aquellos individuos con renta mayor o igual a 0

final_datos_Th_individuo <- datos_Th_nivel_individuo %>%
  filter(vhRentaa >= 0) 

hist(final_datos_Th_individuo$renta, prob = TRUE)

# ============================================================
# 2b. Selección del constante k óptimo por simetría de residuos
# ============================================================

library(lme4)
library(e1071)   # para skewness()
library(ggplot2)

###############################################
# Rejilla de valores candidatos para k
###############################################
# El mínimo válido es el que garantiza vhRenta + k > 0 para todas las obs.
# Si hay valores negativos o cero en vhRenta, k debe superar ese mínimo.
minimo_valido <- min(final_datos_Th_individuo[["renta"]]) 
cat("Valor mínimo válido de k:", minimo_valido, "\n")

# Ajusta el paso según la escala vhRentaa, empezando por 1700 porque el resto de valores da 0
grid_k <- seq(from = minimo_valido, to = minimo_valido + 15000, by = 500)

###############################################
# Función que calcula el skewness de log(vhRenta + k) para un k dado
###############################################
evaluar_k <- function(k_val, datos, var_renta) {
  y_transf <- log(datos[[var_renta]] + k_val)
  data.frame(
    k        = k_val,
    skewness = e1071::skewness(y_transf),
    kurtosis = e1071::kurtosis(y_transf)
  )
}

###############################################
# Recorrer la rejilla (esto reajusta el modelo para cada k)
###############################################
resultados_k <- do.call(rbind, lapply(grid_k, function(k_val) {
  evaluar_k(k_val, final_datos_Th_individuo, "renta")
}))

###############################################
# Elegir el k que minimiza |skewness|
###############################################
k_optimo <- resultados_k$k[which.min(abs(resultados_k$skewness))]
cat("k óptimo (mínima |skewness|):", k_optimo, "\n")
print(resultados_k[which.min(abs(resultados_k$skewness)), ])

###############################################
# Gráfico: skewness en función de k
###############################################
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

#ggsave("C:/Users/hp/Desktop/TFG/skewness_vs_k.png", width = 7, height = 5)

###############################################
# Histograma final con el k óptimo
###############################################
datos_Th_nivel_individuo$y_transf_final <<- log(datos_Th_nivel_individuo[["vhRentaa"]] + k_optimo)

media_y <- mean(datos_Th_nivel_individuo$y_transf_final)
sd_y    <- sd(datos_Th_nivel_individuo$y_transf_final)



png("C:/Users/hp/Desktop/TFG/histograma_log_renta_k_optimo.png", width = 700, height = 500)
hist(datos_Th_nivel_individuo$y_transf_final, breaks = 40,
     main = paste("Histograma de log(vhRenta + k), k =", k_optimo),
     xlab = "log(vhRenta + k)",
     xlim = xlim_centrado)
dev.off()

###############################################
# Comparación visual: histograma sin transformar vs transformado
###############################################
png("C:/Users/hp/Desktop/TFG/histograma_comparacion_k_optimo.png", width = 1000, height = 500)
par(mfrow = c(1, 2))
hist(datos_Th_nivel_individuo[["vhRentaa"]], breaks = 40,
     main = "vhRenta (sin transformar)", xlab = "vhRenta",
     prob = TRUE)
hist(datos_Th_nivel_individuo$y_transf_final, breaks = 40,
     main = paste("log(vhRenta + k), k =", k_optimo),
     xlab = "log(vhRenta + k)",
     prob = TRUE,
     xlim = xlim_centrado)
dev.off()

###############################################
# k queda guardado para usar después en ebBHF
###############################################
#k_optimo <<- 8000
cat("Usando k =", k_optimo, "\n")

Q # para salir del entorno temporal

###############################################
# Limpieza
###############################################
rm(grid_k,minimo_valido,evaluar_k)



# Nueva variable respuesta: Y_di = log(renta + k_optimo)
###############################################
# Gráficos comparativa resultados
###############################################

# 2 boxplot (2 librerías distintas)
boxplot(final_datos_Th_individuo$y~final_datos_Th_individuo$Género)
ggplot(final_datos_Th_individuo, aes(x = Género, y = y, fill = Género)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Comparación de ingresos transformados por género",
       x = "Género",
       y = "Ingresos transformados") +
  scale_fill_brewer(palette = "Set2")


ggplot(final_datos_Th_individuo, aes(x = CCAA, y = y, fill = CCAA)) +
  geom_boxplot(fill = "white", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Comparación de ingresos transformados por CCAA",
       x = "CCAA",
       y = "Renta transformada") +
  scale_fill_brewer(palette = "Greys")




####################################################
# Elección de variables auxiliares a nivel individuo
####################################################
datos_Tr_nivel_individuo$Identif_hogar = as.factor(datos_Tr_nivel_individuo$Identif_hogar)
datos_Tr_nivel_individuo$identif_personas = as.factor(datos_Tr_nivel_individuo$identif_personas)

datos_Th_nivel_individuo$Identif_hogar =  datos_Th_nivel_individuo$RB030%/%100
datos_Th_nivel_individuo$Identif_hogar = as.factor(datos_Th_nivel_individuo$Identif_hogar)

datos_Th_nivel_individuo = datos_Th_nivel_individuo %>%
  left_join(datos_Tr_nivel_individuo %>% dplyr::select(grupo_edad,Identif_hogar,identif_personas)%>%
              distinct(), 
            by = c("Identif_hogar","identif_personas"))



variables_interes <- c(names(datos_Th_nivel_individuo)[sapply(datos_Th_nivel_individuo, is.factor)])

variables_interes <- variables_interes[!(variables_interes %in% 
                                           c("RB030","Pesos","Género","identif_personas","Identif_hogar","Personas_en_hogar_agrupado","CCAA","y_trans_final","vhRentaa","vhRentaaAla","vhMATDEP"))]

# Estudio previo de NAs de variables de interes
colSums(is.na(datos_Th_nivel_individuo[, variables_interes]))

# Nos quedamos con aquellas variables con menos de 100 NAs
variables_interes = c("HS022","HS090","HS110","HD080","HH021","HC190","grupo_edad")

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

rm(moda,v,valor_moda,var,num_na)

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
library(car)
df_matriz_X_indiv <- as.data.frame(matriz_X_individuos[,-1])


df_matriz_X_indiv$y <- final_datos_Th_individuo$y  #variable respuesta a nivel de individuo

modelo <- lm(y ~ ., data = df_matriz_X_indiv)
vif(modelo)


# Observamos que no hay comb. lineales entre las variables cualitativas.
alias(modelo)

# Decidimos quedarnos con aquellas que más relación tenga con la renta
# luego eliminamos HS090,HD080 y HH190
modelo_final = lm(y~., data = subset(df_matriz_X_indiv, select = -c(HS090_2,HS090_3,HD080_2,HD080_3,HC190_2)))
vif(modelo_final)
summary(modelo_final) # todas las categorías son significativas
hist(vif(modelo_final))

rm(modelo,formula_X)

# Modelo AIC
modelo_aic <- step(modelo_final, direction = "both")

# Modelo BIC
modelo_bic <- step(modelo_final,direction = "both", k = log(nrow(subset(df_matriz_X_indiv, select = -c(HS090_2,HS090_3,HD080_2,HD080_3,HC190_2)))))

formula(modelo_aic)
formula(modelo_bic)
# Ambas coinciden y son significativas

# Por tanto la matriz X nivel individuo final es:
matriz_X_indiv_final <- model.matrix(
  ~ HS022_2 + HS110_2 + HS110_3 + HH021_2 + HH021_3 + 
    HH021_4 + HH021_5 + `grupo_edad_18-34` + `grupo_edad_35-49` + 
    `grupo_edad_50-64` + `grupo_edad_65+`,
  data = df_matriz_X_indiv )


rm(df_matriz_X_indiv,df_matriz_X,modelo_aic,modelo_bic,matriz_X_individuos,matriz_X)

# --------------------------------------------------------
# Función de normalización de nombres 
# --------------------------------------------------------
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




# Pasamos ahora a modelo errores anidados con el EB 


# --------------------------------------------------------
# 0. Parámetros 
# --------------------------------------------------------
var_area  <- "id_dominio"
var_peso  <- "Pesos"
var_renta <- "renta"

vars_raw_finales <- c("HS022", "HS110", "HH021", "grupo_edad") # variables auxiliares

# --------------------------------------------------------
# 1. Comprobaciones antes de construir el pseudo-censo
# --------------------------------------------------------
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

# --------------------------------------------------------
# 2. Redondeo estocástico del peso
# --------------------------------------------------------
set.seed(2024)
redondeo_estocastico <- function(w) {
  parte_entera  <- floor(w)
  parte_decimal <- w - parte_entera
  ajuste <- rbinom(length(w), size = 1, prob = parte_decimal)
  parte_entera + ajuste
}

# --------------------------------------------------------
# 3. Construir el pseudo-censo (Xnonsample) por dominio,
#    liberando memoria en cada iteración
# --------------------------------------------------------
dir_censos_csv <- "pseudo_censo_csv_por_area"
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


# --------------------------------------------------------
# 5. Línea de pobreza (con pesos, sobre la muestra)
# --------------------------------------------------------
mediana_ponderada <- function(x, w) {
  ord <- order(x); x <- x[ord]; w <- w[ord]
  cw <- cumsum(w) / sum(w)
  x[which(cw >= 0.5)[1]]
}
z <- 0.6 * mediana_ponderada(
  final_datos_Th_individuo[[var_renta]],
  final_datos_Th_individuo[[var_peso]]
)
cat("Línea de pobreza (z):", z, "\n")

indicador_fgt0 <- function(y) mean(y < z)

# --------------------------------------------------------
# 6. Aplicar ebBHF
# --------------------------------------------------------
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

library(sae)
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
library(dplyr)
resultado_eb_final <- resultado_eb_final %>%
  rename(id_dominio = domain)

write.csv(resultado_eb_final, "C:/Users/hp/Desktop/TFG/EB_por_area.csv", row.names = FALSE)


#resultados_EB <- resultados_EB %>%
#  rename(n_d = sampsize)

# Juntamos con demás estimaciones
estim_dir_Hajek <- estim_dir_Hajek %>%
  left_join(resultado_eb_final %>%dplyr::select(id_dominio, eb),
            by = "id_dominio"
  )

############################
# Ahora para estimar el ECM
#############################
B_boot <- 100   # nº de réplicas bootstrap paramétrico 
MC_mc  <- 50    

#resultados_eb_por_area  <- vector("list", length(areas))
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
    #resultados_eb_por_area[[i]]  <- res_a$est$eb
    resultados_mse_por_area[[i]] <- res_a$mse
  }
  
  rm(Xnonsample_a, res_a)
  gc(verbose = FALSE)
}

# Renombramos
resultados_mse_por_area <- resultados_mse_por_area %>%
  rename(id_dominio = domain)



 #resultado_eb_final  <- do.call(rbind, resultados_eb_por_area)
prueba_mse <- do.call(rbind, resultados_mse_por_area)


# Renombramos
resultado_mse_final <- resultado_mse_final %>%
  rename(id_dominio = domain)
#resultado_mse_final <- resultado_mse_final %>%
#  rename(n_d = sampsize)

# Juntamos con demás estimaciones
estim_dir_Hajek <- estim_dir_Hajek %>% ##############METER CAMBIO AQUI eb
  left_join(resultado_mse_final %>%dplyr::select(id_dominio, eb),
            by = "id_dominio"
  )


# --------------------------------------------------------
# Unir estimación puntual + ECM en una sola tabla
# --------------------------------------------------------
resultado_final <- merge(
  resultado_eb_final,
  resultado_mse_final,
  by = "id_dominio"
)

# CV opcional: coeficiente de variación (%) por dominio
resultado_final$cv <- 100 * sqrt(resultado_final$mse) / resultado_final$eb

estim_dir_Hajek$EB_CV <-100 * sqrt(estim_dir_Hajek$mse) / estim_dir_Hajek$eb




# Graficamos directos, FH y EB
df_aux <- merge(n_d, estim_dir_Hajek[, c(-1, -2, -3)], by = "id_dominio")
orden <- order(df_aux$Freq.x)
df_ordenado <- df_aux[orden, ]
rm(df_aux)

x_ordenado <- 1:nrow(df_ordenado)
rango_y <- range(
  c(df_ordenado$estimacion, df_ordenado$estim_EBLUP, df_ordenado$eb),
  na.rm = TRUE
)

# --------------------------------------------------------
# Gráfico: Hajek vs EBLUP (FH) vs EB (BHF)
# --------------------------------------------------------
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

lines(df_ordenado$eb ~ x_ordenado,
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











prueba_mse <- prueba_mse%>%rename(id_dominio = domain)






prueba_mse = prueba_mse %>%
  left_join(n_d %>% dplyr::select(id_dominio, Freq), 
            by = "id_dominio")
prueba_mse = prueba_mse %>%
  left_join(estim_dir_Hajek %>% dplyr::select(id_dominio, estimacion_var, estim_EBLUP_ECM), 
            by = "id_dominio")
##################### 
# ECM
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
               df_50$mse), na.rm = TRUE)

y_max <- max(c(df_50$estimacion_var,
               df_50$estim_EBLUP_ECM,
               df_50$mse), na.rm = TRUE)

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

lines(df_50$mse ~ x_50, #df_ordenado$mse~x_ordenado
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

lines(df_ordenado$mse~x_ordenado, #df_ordenado$mse~x_ordenado
      type = "b", 
      pch = 17,           
      col = "red",   
      lty = 1, 
      lwd = 1) 







plot(df_ordenado$mse~x_ordenado, #df_ordenado$estimacion_var~x_ordenado
     ylab = "ECM",
     xlab = "Dominios",
     type = "b",
     col = 'red',
     lty = 1,
     pch = 15,
     lwd = 1.5)


resultado_eb_final = resultado_eb_final %>%
  left_join(prueba_mse %>% dplyr::select(id_dominio, mse), 
            by = "id_dominio")





###############
# CV
###############
#####################
orden = order(estim_dir_Hajek$Freq)

#Ordenamos el df mediante el orden definido
df_auxiliar <- estim_dir_Hajek[orden, ]
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

lines(df_auxiliar$EB_CV ~x_ordenado_auxiliar, #df_ordenado$mse~x_ordenado
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
