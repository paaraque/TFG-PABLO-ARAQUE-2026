## ============================================================
## 1. LIBRERÍAS
## ============================================================
library(lme4)      # modelo de error anidado (lme)
library(lmerTest)
library(e1071)      # skewness
library(car)        # vif
library(ggplot2)     # gráficos QQ

## ============================================================
## 2. final_datos_Th_individuo DE EJEMPLO (sustituye por tus final_datos_Th_individuo reales)
## ============================================================
# final_datos_Th_individuo debe tener: renta, area (factor), y variables auxiliares x1, x2, ...
# final_datos_Th_individuo <- read.csv("tu_fichero.csv")
# final_datos_Th_individuo$area <- as.factor(final_datos_Th_individuo$area)

## ============================================================
## 3. PASO A: BÚSQUEDA DE k (GRID SEARCH)
## ============================================================

# Variables auxiliares candidatas iniciales (ajusta a tu caso)
variables_interes = c("HS022","HS090","HS110","HD080","HH021","HC190","grupo_edad")

# Estudio previo de NAs de variables de interes
colSums(is.na(final_datos_Th_individuo[, variables_interes]))

library(tseries)

# Función que ajusta el modelo con un k dado y devuelve criterios de normalidad
evaluar_k_REML <- function(k, final_datos_Th_individuo, vars, area = "id_dominio") {
  final_datos_Th_individuo$y <- log(final_datos_Th_individuo$renta + k)
  
  formula_fija <- as.formula(paste("y ~", paste(vars, collapse = " + ")))
  
  modelo <- tryCatch(
    lme(fixed = formula_fija,
        random = as.formula(paste0("~1 | ", area)),
        data = final_datos_Th_individuo,
        method = "REML"),
    error = function(e) NULL
  )
  
  if (is.null(modelo)) {
    return(data.frame(k = k, skew_e = NA, shapiro_p_e = NA,
                      skew_u = NA, shapiro_p_u = NA))
  }
  
  # Residuos de nivel 1 (unidad, e_ij)
  res_e <- resid(modelo, type = "normalized")
  
  # Efectos aleatorios de área (u_i)
  res_u <- ranef(modelo)[[1]]
  
  sh_u <- if (length(res_u) >= 3 && length(res_u) <= 5000) shapiro.test(res_u)$p.value else NA
  
  data.frame(
    k = k,
    skew_e = skewness(res_e),
    #shapiro_p_e = sh_e,
    skew_u = skewness(res_u),
    shapiro_p_u = sh_u
  )
}

# Rango de k: debe cumplir k > min(renta)
k_min <- if (min_renta <= 0) -min(final_datos_Th_individuo$renta, na.rm = TRUE) + 1 else 0
k_grid <- c(k_min, seq(500, 12000, by = 500))

resultados_k <- do.call(rbind, lapply(k_grid, evaluar_k_REML,
                                      final_datos_Th_individuo = final_datos_Th_individuo, vars = variables_interes))

# Criterio: minimizar |skewness| de los residuos de unidad (el más influyente)
resultados_k <- resultados_k[!is.na(resultados_k$skew_e), ]
k_optimo <- resultados_k$k[which.min(abs(resultados_k$skew_e))]

cat("k óptimo (por asimetría mínima de residuos e_ij):", k_optimo, "\n")
print(resultados_k[order(abs(resultados_k$skew_e)), ], row.names = FALSE)

# Gráfico: asimetría vs k
ggplot(resultados_k, aes(x = k, y = skew_e)) +
  geom_line() + geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_vline(xintercept = k_optimo, linetype = "dotted", color = "blue") +
  labs(title = "Asimetría de residuos e_ij en función de k",
       x = "k", y = "Skewness")




final_datos_Th_individuo$y <- log(final_datos_Th_individuo$renta + k_optimo)
formula_fija <- as.formula(paste("y ~", paste(variables_interes, collapse = " + ")))

modelo_k_optimo <- lme(fixed = formula_fija,
                       random = ~1 | "id_dominio",
                       data = final_datos_Th_individuo,
                       method = "REML")

res_e_k <- resid(modelo_k_optimo, type = "normalized")
res_u_k <- unlist(ranef(modelo_k_optimo)[[1]])


qqnorm(res_e_k)
qqline(res_e_k, col = "red")

hist(final_datos_Th_individuo$res_e_k)
hist(final_datos_Th_individuo$y)


qqnorm(res_u_k)
qqline(res_u_k, col = "red")


# Observamos en el q-q plot colas pesadas, sin embargo esos outliers representan
# una extremadamente pequeña (1%) y el histograma refleja simetría, luego es aceptable
# luego podemos proceder a la elección óptima de variables de interés

final_datos_Th_individuo$res_e_k <- res_e_k
outliers <- final_datos_Th_individuo[abs(final_datos_Th_individuo$res_e_k) > 3, ]
nrow(outliers)
round(nrow(outliers) / nrow(final_datos_Th_individuo) * 100, 2)   # % de la muestra












## ============================================================
## 4. PASO B: SELECCIÓN DE VARIABLES AUXILIARES (con k fijado)
## ============================================================

#final_datos_Th_individuo$y <- log(final_datos_Th_individuo$renta + k_optimo)

# Modelo completo con todas las candidatas
formula_completa <- as.formula(paste("y ~", paste(variables_interes, collapse = " + "), "+ (1 | id_dominio)"))

modelo_completo <-lmer(formula_completa, data = final_datos_Th_individuo, REML = FALSE)  # ML para comparar modelos
summary(modelo_completo)

# --- Selección stepwise manual basada en AIC ---
modelo_step <- step(modelo_completo, reduce.random = TRUE, reduce.fixed = TRUE)
print(modelo_step)

# Extraer el modelo final resultante de la selección
mejor_modelo <- get_model(modelo_step)
summary(mejor_modelo)

# --- Variables finales seleccionadas (solo efectos fijos, sin intercept ni random) ---
vars_finales <- all.vars(formula(mejor_modelo))
vars_finales <- setdiff(vars_finales, c("y", "area"))
vars_finales

# --- Comprobar multicolinealidad de las variables seleccionadas ---
modelo_lm_aux <- lm(as.formula(paste("y ~", paste(vars_finales, collapse = " + "))),
                    data = datos)
car::vif(modelo_lm_aux)


## ============================================================
## 5. REAJUSTAR CON REML Y REVISAR NORMALIDAD FINAL
## ============================================================

formula_final <- as.formula(
  paste("y ~", paste(vars_finales, collapse = " + "), "+ (1 | area)")
)

modelo_final <- lmer(formula_final, data = datos, REML = TRUE)
summary(modelo_final)

# Residuos finales
res_e_final <- resid(modelo_final, scaled = TRUE)   # residuos normalizados (nivel unidad)
res_u_final <- ranef(modelo_final)$area[, 1]         # efectos aleatorios de área

# Tests de normalidad
shapiro.test(res_e_final)
shapiro.test(res_u_final)

# QQ-plots
par(mfrow = c(1, 2))
qqnorm(res_e_final, main = "QQ-plot residuos e_ij"); qqline(res_e_final, col = "red")
qqnorm(res_u_final, main = "QQ-plot efectos de área u_i"); qqline(res_u_final, col = "red")
par(mfrow = c(1, 1))

# Homocedasticidad: residuos vs valores ajustados
plot(fitted(modelo_final), res_e_final,
     xlab = "Valores ajustados", ylab = "Residuos normalizados",
     main = "Homocedasticidad")
abline(h = 0, col = "red", lty = 2)

## ============================================================
## 6. (OPCIONAL) RE-ITERAR k CON EL MODELO FINAL DE VARIABLES
## ============================================================
# La función evaluar_k() del Paso A usaba nlme::lme; adáptala también a lmer
# si quieres mantener consistencia con este paso:

evaluar_k_lmer <- function(k, datos, vars, area = "area") {
  datos$y <- log(datos$renta + k)
  formula_k <- as.formula(paste("y ~", paste(vars, collapse = " + "), "+ (1 |", area, ")"))
  
  modelo <- tryCatch(
    lmer(formula_k, data = datos, REML = TRUE),
    error = function(e) NULL, warning = function(w) NULL
  )
  if (is.null(modelo)) {
    return(data.frame(k = k, skew_e = NA, shapiro_p_e = NA))
  }
  
  res_e <- resid(modelo, scaled = TRUE)
  sh_e <- if (length(res_e) >= 3 && length(res_e) <= 5000) shapiro.test(res_e)$p.value else NA
  
  data.frame(k = k, skew_e = e1071::skewness(res_e), shapiro_p_e = sh_e)
}

resultados_k_v2 <- do.call(rbind, lapply(k_grid, evaluar_k_lmer,
                                         datos = datos, vars = vars_finales))
resultados_k_v2 <- resultados_k_v2[!is.na(resultados_k_v2$skew_e), ]
k_optimo_v2 <- resultados_k_v2$k[which.min(abs(resultados_k_v2$skew_e))]
cat("k óptimo tras fijar variables:", k_optimo_v2, "\n")



# formula_final <- formula(mejor_modelo)
# modelo_final <- lme(fixed = formula_final,
#                     random = ~1 | area,
#                     data = final_datos_Th_individuo,
#                     method = "REML")
# 
# summary(modelo_final)
# 
# # Residuos finales
# res_e_final <- resid(modelo_final, type = "normalized")
# res_u_final <- ranef(modelo_final)[[1]]
# 
# # Tests de normalidad
# shapiro.test(res_e_final)
# shapiro.test(unlist(res_u_final))
# 
# # QQ-plots
# par(mfrow = c(1, 2))
# qqnorm(res_e_final, main = "QQ-plot residuos e_ij"); qqline(res_e_final, col = "red")
# qqnorm(unlist(res_u_final), main = "QQ-plot efectos de área u_i"); qqline(unlist(res_u_final), col = "red")
# par(mfrow = c(1, 1))
# 
# # Homocedasticidad: residuos vs valores ajustados
# plot(fitted(modelo_final), res_e_final,
#      xlab = "Valores ajustados", ylab = "Residuos normalizados",
#      main = "Homocedasticidad")
# abline(h = 0, col = "red", lty = 2)

## ============================================================
## 6. (OPCIONAL) RE-ITERAR k CON EL MODELO FINAL DE VARIABLES
## ============================================================
# Repite el paso A pero usando vars_finales en lugar de variables_interes,
# para comprobar que k_optimo no cambia sustancialmente.

resultados_k_v2 <- do.call(rbind, lapply(k_grid, evaluar_k_REML,
                                         final_datos_Th_individuo = final_datos_Th_individuo, vars = vars_finales))
resultados_k_v2 <- resultados_k_v2[!is.na(resultados_k_v2$skew_e), ]
k_optimo_v2 <- resultados_k_v2$k[which.min(abs(resultados_k_v2$skew_e))]
cat("k óptimo tras fijar variables:", k_optimo_v2, "\n")