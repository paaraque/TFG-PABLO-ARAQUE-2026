###############################################
# Análisis descriptivo de los datos
###############################################
names(datos_Th_nivel_individuo)[names(datos_Th_nivel_individuo) == 'Identif_hogar'] <- 'HB030'
datos_Th_nivel_individuo <- datos_Th_nivel_individuo %>%
  mutate(across(c(HB030), as.numeric))

datos_graficar = datos_Th_nivel_individuo %>%
  left_join(datos_Th %>% dplyr::select(HB030,vhPobreza), 
            by = "HB030")

names(datos_Th_nivel_individuo)[names(datos_Th_nivel_individuo) == 'HB030'] <- 'Identif_hogar'
datos_Th_nivel_individuo <- datos_Th_nivel_individuo %>%
  mutate(across(c(Identif_hogar), as.factor))

# Variable pobreza
porcentajes = round(100 * table(datos_Th$vhPobreza) / sum(table(datos_Th$vhPobreza)), 1)

pie(table(datos_Th$vhPobreza),main = "Personas en riesgo de pobreza",
    labels = paste0(c("No", "Si"), " (", porcentajes, "%)"),
    )
rm(porcentajes)

# Número de miembros del hogar
barplot(table(datos_Th_nivel_individuo$Personas_en_hogar_agrupado),ylim = 
          c(0,21000),
        xlab = "Número de miembros del hogar")

# Género

porcentajes = round(100 * table(datos_Th_nivel_individuo$Género) / sum(table(datos_Th_nivel_individuo$Género)), 1)
etiquetas = paste0(c("Hombre", "Mujer"), " (", porcentajes, "%)")

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





sacar_df = estim_dir_Hajek[,c(4,5,6,7)] %>%
  left_join(n_d %>% dplyr::select(id_dominio,Freq), 
            by = "id_dominio")
write.csv(sacar_df, file = "C:/Users/hp/Desktop/TFG/estim_hajek.csv", row.names = FALSE)
rm(sacar_df)
