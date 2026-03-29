### Crear base de datos de ingresos ENSANUT


# ====================================================
# CÁLCULO DE GINI NACIONAL Y ESTATAL - ENSANUT 2012
# ====================================================
library(dplyr)
library(DescTools)

# 1. Cargar datos (ajusta la ruta)
datos <- read.csv("/Users/dmares/Downloads/2012_Hogar_integrantes.csv")



# 1. Filtrar datos válidos
df <- datos %>%
  select(entidad, ingreso = h226b, factor = pondei, periodo = h226a) %>%
  filter(
    periodo %in% 1:5,               # Solo periodos con ingreso
    ingreso > 0, ingreso < 999000,  # Elimina códigos de missing
    !is.na(entidad),
    factor > 0
  )

# 2. Convertir a ingreso mensual
df <- df %>%
  mutate(
    ingreso_mensual = case_when(
      periodo == 1 ~ ingreso * 30.4,
      periodo == 2 ~ ingreso * 4.33,
      periodo == 3 ~ ingreso * 2,
      periodo == 4 ~ ingreso,
      periodo == 5 ~ ingreso / 12,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(ingreso_mensual > 0)

# 3. Truncar al percentil 99.9 para controlar outliers extremos
p999 <- quantile(df$ingreso_mensual, probs = 0.999, na.rm = TRUE)
df <- df %>%
  mutate(ingreso_mensual = ifelse(ingreso_mensual > p999, p999, ingreso_mensual))

# 4. Calcular Gini nacional
library(DescTools)
gini_nac <- df %>%
  summarise(
    gini = Gini(ingreso_mensual, weights = factor),
    ingreso_prom = weighted.mean(ingreso_mensual, factor, na.rm = TRUE),
    poblacion = sum(factor, na.rm = TRUE)
  ) %>%
  mutate(año = 2012)

print(gini_nac)

# 5. Gini por estado
gini_estatal_2012 <- df %>%
  group_by(entidad) %>%
  summarise(
    gini = Gini(ingreso_mensual, weights = factor),
    ingreso_prom = weighted.mean(ingreso_mensual, factor, na.rm = TRUE),
    poblacion = sum(factor, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(año = 2012)

# Mostrar primeros estados
head(gini_estatal)

# Guardar resultados
saveRDS(gini_estatal, "gini_estatal_ensanut2012.rds")
write.csv(gini_estatal, "gini_estatal_ensanut2012.csv", row.names = FALSE)



# ====================================================
# CÁLCULO DE GINI NACIONAL Y ESTATAL - ENSANUT 2018
# ====================================================


library(dplyr)
library(DescTools)

# 1. Seleccionar variables y filtrar valores válidos
df_ingreso <- res18 %>%
  select(
    entidad = ENT,
    periodo = P3_26_1,
    ingreso = P3_26_2,
    factor = FACTOR
  ) %>%
  filter(
    !is.na(entidad),
    ingreso > 0, ingreso < 500000,        # elimina códigos de missing (99999, 999999)
    periodo %in% c(1, 2, 3, 5),           # solo periodos con ingreso (excluye 6 y 9)
    factor > 0
  )

# 2. Convertir a ingreso mensual según periodicidad
df_ingreso <- df_ingreso %>%
  mutate(
    ingreso_mensual = case_when(
      periodo == 1 ~ ingreso,                # mensual
      periodo == 2 ~ ingreso * 2,            # quincenal a mensual
      periodo == 3 ~ ingreso * 4.33,         # semanal a mensual
      periodo == 5 ~ ingreso * 30.4,         # diario a mensual
      TRUE ~ NA_real_
    )
  ) %>%
  filter(ingreso_mensual > 0, ingreso_mensual < 300000)  # tope conservador

# 3. Truncar al percentil 99.5 para eliminar extremos
p99.5 <- quantile(df_ingreso$ingreso_mensual, probs = 0.995, na.rm = TRUE)
df_ingreso <- df_ingreso %>%
  mutate(ingreso_mensual = ifelse(ingreso_mensual > p99.5, p99.5, ingreso_mensual))

# 4. Gini nacional
gini_nac <- df_ingreso %>%
  summarise(
    gini = Gini(ingreso_mensual, weights = factor),
    ingreso_prom = weighted.mean(ingreso_mensual, factor, na.rm = TRUE),
    poblacion = sum(factor, na.rm = TRUE)
  ) %>%
  mutate(año = 2018)

print(gini_nac)

# 5. Gini por estado
gini_estatal <- df_ingreso %>%
  group_by(entidad) %>%
  summarise(
    gini = Gini(ingreso_mensual, weights = factor),
    ingreso_prom = weighted.mean(ingreso_mensual, factor, na.rm = TRUE),
    poblacion = sum(factor, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(año = 2018)

head(gini_estatal)

# 6. Guardar resultados
saveRDS(gini_estatal, "gini_estatal_ensanut2018.rds")
write.csv(gini_estatal, "gini_estatal_ensanut2018.csv", row.names = FALSE)




# ====================================================
# CÁLCULO DE GINI NACIONAL Y ESTATAL - ENSANUT 2019
# ====================================================

hogar19 <- read.csv2("/Users/dmares/Downloads/2019_hogar_ensanut2020_ww.csv", stringsAsFactors = FALSE)
grep("H0326", names(hogar19), value = TRUE)
# Ver los primeros valores de H0326 (si existe)
table(hogar19$H0326, useNA = "ifany")

library(dplyr)
library(DescTools)

# 1. Seleccionar y filtrar
df_ingreso <- hogar19 %>%
  select(
    entidad = ENTIDAD,
    ingreso_cod = H0326,
    factor = ponde_f          # factor de expansión del hogar
  ) %>%
  filter(
    !is.na(entidad),
    ingreso_cod %in% 1:5,     # solo hogares con ingreso declarado
    factor > 0
  ) %>%
  mutate(
    ingreso_hogar = case_when(
      ingreso_cod == 1 ~ 3000,   # punto medio de 1-5999
      ingreso_cod == 2 ~ 8000,   # punto medio de 6000-9999
      ingreso_cod == 3 ~ 12000,  # punto medio de 10000-13999
      ingreso_cod == 4 ~ 18000,  # punto medio de 14000-21999
      ingreso_cod == 5 ~ 30000,  # valor convencional para "22000 o más"
      TRUE ~ NA_real_
    )
  )

# 2. Gini nacional
gini_nac <- df_ingreso %>%
  summarise(
    gini = Gini(ingreso_hogar, weights = factor),
    ingreso_prom = weighted.mean(ingreso_hogar, factor, na.rm = TRUE),
    poblacion = sum(factor, na.rm = TRUE)
  ) %>%
  mutate(año = 2019)

print(gini_nac)

# 3. Gini por estado
gini_estatal_2019 <- df_ingreso %>%
  group_by(entidad) %>%
  summarise(
    gini = Gini(ingreso_hogar, weights = factor),
    ingreso_prom = weighted.mean(ingreso_hogar, factor, na.rm = TRUE),
    poblacion = sum(factor, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(año = 2019)

head(gini_estatal)

# 4. Guardar resultados
saveRDS(gini_estatal, "gini_estatal_ensanut2019.rds")
write.csv(gini_estatal, "gini_estatal_ensanut2019.csv", row.names = FALSE)



# ====================================================
# CÁLCULO DE GINI NACIONAL Y ESTATAL - ENSANUT 2021
# ====================================================

library(dplyr)
library(DescTools)

# 1. Leer base de hogares 2021
hogar21 <- read.csv2("/Users/dmares/Downloads/2021_hogar_ensanut2021_w_14_12_2021.csv", stringsAsFactors = FALSE)

# 2. Verificar nombres de columnas
names(hogar21)

# 3. Confirmar variable de ingreso y factor
# Busca H0326 y ponde_f (o similar)
table(hogar21$H0326, useNA = "ifany")

df_ingreso <- hogar21 %>%
  select(
    entidad = entidad,
    ingreso_cod = h0327,          # ¡variable correcta!
    factor = ponde_f
  ) %>%
  filter(
    !is.na(entidad),
    ingreso_cod %in% 1:5,
    factor > 0
  ) %>%
  mutate(
    ingreso_hogar = case_when(
      ingreso_cod == 1 ~ 3000,
      ingreso_cod == 2 ~ 8000,
      ingreso_cod == 3 ~ 12000,
      ingreso_cod == 4 ~ 18000,
      ingreso_cod == 5 ~ 30000,
      TRUE ~ NA_real_
    )
  )

# 2. Gini nacional
gini_nac_2021 <- df_ingreso %>%
  summarise(
    gini = Gini(ingreso_hogar, weights = factor),
    ingreso_prom = weighted.mean(ingreso_hogar, factor, na.rm = TRUE),
    poblacion = sum(factor, na.rm = TRUE)
  ) %>%
  mutate(año = 2021)

print(gini_nac_2021)

# 3. Gini por estado
gini_estatal_2021 <- df_ingreso %>%
  group_by(entidad) %>%
  summarise(
    gini = Gini(ingreso_hogar, weights = factor),
    ingreso_prom = weighted.mean(ingreso_hogar, factor, na.rm = TRUE),
    poblacion = sum(factor, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(año = 2021)

head(gini_estatal_2021)

# 4. Guardar
saveRDS(gini_estatal_2021, "gini_estatal_ensanut2021.rds")
write.csv(gini_estatal_2021, "gini_estatal_ensanut2021.csv", row.names = FALSE)





# ====================================================
# CÁLCULO DE GINI NACIONAL Y ESTATAL - ENSANUT 2022
# ====================================================

library(dplyr)
library(DescTools)

# 1. Leer base de hogares 2022
hogar22 <- read.csv2("/Users/dmares/Downloads/2022_hogar_ensanut2022_w.csv", stringsAsFactors = FALSE)

# 2. Verificar nombres de columnas (opcional)
names(hogar22)

# 3. Confirmar variable de ingreso y factor
# Busca h0327 y ponde_f
table(hogar22$h0327, useNA = "ifany")

# 4. Seleccionar y filtrar
df_ingreso <- hogar22 %>%
  select(
    entidad = entidad,           # ajusta si el nombre es diferente
    ingreso_cod = h0327,
    factor = ponde_f
  ) %>%
  filter(
    !is.na(entidad),
    ingreso_cod %in% 1:5,
    factor > 0
  ) %>%
  mutate(
    ingreso_hogar = case_when(
      ingreso_cod == 1 ~ 3000,
      ingreso_cod == 2 ~ 8000,
      ingreso_cod == 3 ~ 12000,
      ingreso_cod == 4 ~ 18000,
      ingreso_cod == 5 ~ 30000,
      TRUE ~ NA_real_
    )
  )

# 5. Gini nacional
gini_nac_2022 <- df_ingreso %>%
  summarise(
    gini = Gini(ingreso_hogar, weights = factor),
    ingreso_prom = weighted.mean(ingreso_hogar, factor, na.rm = TRUE),
    poblacion = sum(factor, na.rm = TRUE)
  ) %>%
  mutate(año = 2022)

print(gini_nac_2022)

# 6. Gini por estado
gini_estatal_2022 <- df_ingreso %>%
  group_by(entidad) %>%
  summarise(
    gini = Gini(ingreso_hogar, weights = factor),
    ingreso_prom = weighted.mean(ingreso_hogar, factor, na.rm = TRUE),
    poblacion = sum(factor, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(año = 2022)

head(gini_estatal_2022)

# 7. Guardar
saveRDS(gini_estatal_2022, "gini_estatal_ensanut2022.rds")
write.csv(gini_estatal_2022, "gini_estatal_ensanut2022.csv", row.names = FALSE)



# ====================================================
# CÁLCULO DE GINI NACIONAL Y ESTATAL - ENSANUT 2023
# ====================================================


library(dplyr)
library(DescTools)

# 1. Leer base de hogares 2023
hogar23 <- read.csv2("/Users/dmares/Downloads/2023_hogar_ensanut2023_w_n.csv", stringsAsFactors = FALSE)

# 2. Verificar nombres de columnas (opcional)
names(hogar23)

# 3. Confirmar variable de ingreso y factor
# Busca h0327 y ponde_f
table(hogar23$h0327, useNA = "ifany")

# 4. Seleccionar y filtrar
df_ingreso <- hogar23 %>%
  select(
    entidad = entidad,           # ajusta si el nombre es diferente
    ingreso_cod = h0327,
    factor = ponde_f
  ) %>%
  filter(
    !is.na(entidad),
    ingreso_cod %in% 1:5,
    factor > 0
  ) %>%
  mutate(
    ingreso_hogar = case_when(
      ingreso_cod == 1 ~ 3000,
      ingreso_cod == 2 ~ 8000,
      ingreso_cod == 3 ~ 12000,
      ingreso_cod == 4 ~ 18000,
      ingreso_cod == 5 ~ 30000,
      TRUE ~ NA_real_
    )
  )

# 5. Gini nacional
gini_nac_2023 <- df_ingreso %>%
  summarise(
    gini = Gini(ingreso_hogar, weights = factor),
    ingreso_prom = weighted.mean(ingreso_hogar, factor, na.rm = TRUE),
    poblacion = sum(factor, na.rm = TRUE)
  ) %>%
  mutate(año = 2023)

print(gini_nac_2023)

# 6. Gini por estado
gini_estatal_2023 <- df_ingreso %>%
  group_by(entidad) %>%
  summarise(
    gini = Gini(ingreso_hogar, weights = factor),
    ingreso_prom = weighted.mean(ingreso_hogar, factor, na.rm = TRUE),
    poblacion = sum(factor, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(año = 2023)

head(gini_estatal_2023)

# 7. Guardar
saveRDS(gini_estatal_2023, "gini_estatal_ensanut2023.rds")
write.csv(gini_estatal_2023, "gini_estatal_ensanut2023.csv", row.names = FALSE)





#######################
###### Interpolar
########################


library(dplyr)

# 1. Crear data frame con los años conocidos (nacional)
gini_conocidos <- data.frame(
  año = c(2012, 2018, 2019, 2021, 2022, 2023),
  gini = c(0.506, 0.515, 0.401, 0.375, 0.378, 0.385)
)

# 2. Definir años a interpolar (2013-2017 y 2020)
años_a_interpolar <- c(2013:2017, 2020)

# 3. Interpolación lineal (usando todos los puntos)
#    Nota: esto interpola entre los años conocidos más cercanos,
#    pero al usar todos los puntos, la línea puede no pasar exactamente por los puntos conocidos.
#    Para que pase exactamente, podemos interpolar por tramos.

# Opción A: Interpolación por tramos (recomendada)
# Tramo 2012-2018 (individual)
tramo1 <- approx(c(2012, 2018), c(0.506, 0.515), xout = 2013:2017)
# Tramo 2019-2023 (hogar)
tramo2 <- approx(c(2019, 2021, 2022, 2023), c(0.401, 0.375, 0.378, 0.385), xout = 2020)

# Resultado
gini_interpolado <- data.frame(
  año = c(tramo1$x, tramo2$x),
  gini = c(tramo1$y, tramo2$y)
)

# 4. Unir con los conocidos
gini_completo <- bind_rows(
  gini_conocidos,
  gini_interpolado
) %>%
  arrange(año)

print(gini_completo)


# ====================================================
# INTERPOLACIÓN DE GINI ESTATALES (2012-2023)
# ====================================================

library(dplyr)
library(tidyr)

# ----------------------------------------------------
# PASO 1: CARGAR LOS GINI ESTATALES DE CADA AÑO
# ----------------------------------------------------
# ====================================================
# UNIÓN E INTERPOLACIÓN DE GINI ESTATALES (2012-2023)
# ====================================================

library(dplyr)
library(tidyr)

gini_list <- list(
  gini_estatal_2012,
  gini_estatal_2018,
  gini_estatal_2019,
  gini_estatal_2021,
  gini_estatal_2022,
  gini_estatal_2023
)

gini_conocidos <- bind_rows(gini_list) %>%
  select(entidad, año, gini)

años_todos <- 2012:2023
estados_todos <- 1:32

gini_completo <- expand.grid(
  entidad = estados_todos,
  año = años_todos
) %>%
  arrange(entidad, año) %>%
  left_join(gini_conocidos, by = c("entidad", "año"))

interpolar_tramos <- function(gini, año) {
  n <- length(gini)
  gini_interp <- rep(NA, n)
  
  # Tramo 1: 2012-2018
  idx1 <- which(año >= 2012 & año <= 2018)
  años1 <- año[idx1]
  gini1 <- gini[idx1]
  if (sum(!is.na(gini1)) >= 2) {
    interp1 <- approx(años1[!is.na(gini1)], gini1[!is.na(gini1)], xout = años1, rule = 2)
    gini_interp[idx1] <- interp1$y
  }
  
  # Tramo 2: 2019-2023
  idx2 <- which(año >= 2019 & año <= 2023)
  años2 <- año[idx2]
  gini2 <- gini[idx2]
  if (sum(!is.na(gini2)) >= 2) {
    interp2 <- approx(años2[!is.na(gini2)], gini2[!is.na(gini2)], xout = años2, rule = 2)
    gini_interp[idx2] <- interp2$y
  }
  
  return(gini_interp)
}

# Aplicar a cada estado
gini_interpolado <- gini_completo %>%
  group_by(entidad) %>%
  mutate(
    gini_interp = interpolar_tramos(gini, año)
  ) %>%
  ungroup()

gini_interpolado %>%
  filter(entidad %in% 1:3) %>%
  print(n = 36)

# ----------------------------------------------------
# PASO 6: GUARDAR RESULTADOS
# ----------------------------------------------------
saveRDS(gini_interpolado, "gini_estatal_interpolado_2012_2023.rds")
write.csv(gini_interpolado, "gini_estatal_interpolado_2012_2023.csv", row.names = FALSE)

# ----------------------------------------------------
# PASO 7: (OPCIONAL) VISUALIZAR UN ESTADO
# ----------------------------------------------------
library(ggplot2)

estado_ejemplo <- 9  # CDMX
gini_interpolado %>%
  filter(entidad == estado_ejemplo) %>%
  ggplot(aes(x = año)) +
  geom_point(aes(y = gini, color = "Original"), size = 3) +
  geom_line(aes(y = gini_interp, color = "Interpolado")) +
  labs(
    title = paste("Gini interpolado - Entidad", estado_ejemplo),
    x = "Año", y = "Gini"
  ) +
  scale_x_continuous(breaks = 2012:2023) +
  theme_minimal()



# Ver qué estados y años tienen NA en gini_interp
na_summary <- gini_interpolado %>%
  filter(is.na(gini_interp)) %>%
  group_by(entidad) %>%
  summarise(
    años_con_na = list(año),
    n_na = n()
  ) %>%
  ungroup()

print(na_summary)

interpolar_tramos_robusta <- function(gini, año) {
  n <- length(gini)
  gini_interp <- rep(NA, n)
  
  # Tramo 1: 2012-2018
  idx1 <- which(año >= 2012 & año <= 2018)
  años1 <- año[idx1]
  gini1 <- gini[idx1]
  puntos1 <- which(!is.na(gini1))
  
  if (length(puntos1) >= 2) {
    interp1 <- approx(años1[puntos1], gini1[puntos1], xout = años1, rule = 2)
    gini_interp[idx1] <- interp1$y
  } else if (length(puntos1) == 1) {
    # Un solo punto: repetir ese valor para todos los años del tramo
    gini_interp[idx1] <- gini1[puntos1]
  }
  # Si no hay puntos, se queda NA (lo imputaremos después)
  
  # Tramo 2: 2019-2023
  idx2 <- which(año >= 2019 & año <= 2023)
  años2 <- año[idx2]
  gini2 <- gini[idx2]
  puntos2 <- which(!is.na(gini2))
  
  if (length(puntos2) >= 2) {
    interp2 <- approx(años2[puntos2], gini2[puntos2], xout = años2, rule = 2)
    gini_interp[idx2] <- interp2$y
  } else if (length(puntos2) == 1) {
    gini_interp[idx2] <- gini2[puntos2]
  }
  
  return(gini_interp)
}

# Aplicar nuevamente
gini_interpolado <- gini_completo %>%
  group_by(entidad) %>%
  mutate(
    gini_interp = interpolar_tramos_robusta(gini, año)
  ) %>%
  ungroup()

# Gini nacional (ya lo tienes de la interpolación nacional)
gini_nacional <- data.frame(
  año = 2012:2023,
  gini_nac = c(0.506, 0.508, 0.509, 0.511, 0.512, 0.514, 0.515, 0.401, 0.388, 0.375, 0.378, 0.385)
)

# Unir e imputar NA con el valor nacional
gini_interpolado <- gini_interpolado %>%
  left_join(gini_nacional, by = "año") %>%
  mutate(
    gini_interp = ifelse(is.na(gini_interp), gini_nac, gini_interp)
  ) %>%
  select(-gini_nac)


sum(is.na(gini_interpolado$gini_interp))  # debe ser 0



# Ver los primeros registros de los primeros 3 estados
gini_interpolado %>%
  filter(entidad %in% 1:3) %>%
  print(n = 36)

