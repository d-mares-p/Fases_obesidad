## analisis espacial

library(sf)          # manejo de shapefiles
library(spdep)       # matrices de pesos espaciales
library(spatialreg)  # modelos espaciales cross-section
library(splm)        # modelos espaciales de panel [citation:2]
library(plm)         # modelos de panel estándar
library(dplyr)       # manipulación de datos
library(tidyr)       # para reshape de datos
library(ggplot2)     # visualización


datos <- RegresionFinal
mx <- st_read("/Users/dmares/Documents/Doctorado unam/Seminario 7mo sem/Codigos/areas_geoestadisticas_estatales.shp")


names(mx)
names(datos)

# unir ambos archivos suponiendo que en shapefile la clave se llama "CVE_ENT"
mx <- mx %>% rename(CVE = CVE_ENT)  
mx_data <- mx %>% left_join(datos, by = "CVE")


#formato panel
vars_fijas <- c("CVE", "Estado")
vars_anuales <- names(datos)[grepl("\\d{4}$", names(datos))]  # columnas que terminan en 4 dígitos

# Crear un data.frame con CVE y las variables anuales
datos_panel <- datos %>% select(CVE, all_of(vars_anuales))

# Convertir a formato largo usando pivot_longer de tidyr
# Primero, necesitamos separar el nombre de la variable del año
# Ejemplo: "Inci2014" -> variable = "Inci", año = 2014

datos_long <- datos_panel %>%
  pivot_longer(
    cols = -CVE,
    names_to = "variable_year",
    values_to = "valor"
  ) %>%
  # Separar el nombre de la variable y el año
  mutate(
    year = as.numeric(substr(variable_year, nchar(variable_year)-3, nchar(variable_year))),
    variable = substr(variable_year, 1, nchar(variable_year)-4)
  ) %>%
  select(-variable_year) %>%
  # Ahora convertir de nuevo a ancho pero con variables como columnas
  pivot_wider(
    id_cols = c(CVE, year),
    names_from = variable,
    values_from = valor
  )

# Verificar resultado
head(datos_long)
summary(datos_long)

# Unir con shapefile (para tener geometría)
mx_data <- mx %>% left_join(datos_long, by = "CVE")

# Convertir a objeto pdata.frame para plm/splm
library(plm)
panel_data <- pdata.frame(datos_long, index = c("CVE", "year"))

# Verificar balance
library(plm)
pdim(datos_long, index = c("CVE", "year"))  # debe indicar n=32, T=10, N=320

# Los nombres de fila deben ser los códigos de estado (ej. "01", "02", ...)
all(rownames(coords) %in% unique(datos_long$CVE))  # debe ser TRUE



# A. Matriz de distancias euclidianas entre centroides
dist_matrix <- as.matrix(dist(coords))
inv_dist <- 1 / dist_matrix
diag(inv_dist) <- 0
W_inv <- inv_dist / rowSums(inv_dist)
listw_inv <- mat2listw(W_inv, style = "W")

#B. matriz de k vecinos más cercanos
knn <- knearneigh(coords, k = 5)
nb_knn <- knn2nb(knn)
listw_knn <- nb2listw(nb_knn, style = "W")

#C. matriz de distancia con cutoff Convertir distancias a kilómetros (aprox: 1 grado ≈ 111 km)
dist_km <- dist_matrix * 111
cutoff <- 400  # km
W_cut <- ifelse(dist_km <= cutoff & dist_km > 0, 1 / dist_km, 0)
diag(W_cut) <- 0
W_cut <- W_cut / rowSums(W_cut)
listw_cut <- mat2listw(W_cut, style = "W")


##################
###### regresiones con matriz inversa sin ingreso
form_sin_ingr <- Inci ~ gini + pobreza + CrecimientoVA + escolaridad + densidad + IDS + CrecimientoGastosalud


# SAR
sar_inv_sin <- spml(form_sin_ingr, data = datos_long, index = c("CVE", "year"),
                    listw = listw_inv, model = "within", effect = "twoways",
                    spatial.error = "none", lag = TRUE)
summary(sar_inv_sin)

# SEM
sem_inv_sin <- spml(form_sin_ingr, data = datos_long, index = c("CVE", "year"),
                    listw = listw_inv, model = "within", effect = "twoways",
                    spatial.error = "b", lag = FALSE)
summary(sem_inv_sin)

# SDM
sdm_inv_sin <- spml(form_sin_ingr, data = datos_long, index = c("CVE", "year"),
                    listw = listw_inv, model = "within", effect = "twoways",
                    spatial.error = "none", lag = TRUE, Durbin = TRUE)
summary(sdm_inv_sin)

sac_inv <- spml(form_sin_ingr, data = datos_long, index = c("CVE", "year"),
                listw = listw_inv, model = "within", effect = "twoways",
                spatial.error = "b", lag = TRUE)
summary(sac_inv)




##################
###### regresiones con k-vecinos
# SAR con k-vecinos (k=5)
sar_knn_sin <- spml(form_sin_ingr, data = datos_long, index = c("CVE", "year"),
                    listw = listw_knn, model = "within", effect = "twoways",
                    spatial.error = "none", lag = TRUE)
summary(sar_knn_sin)

# SEM con k-vecinos
sem_knn_sin <- spml(form_sin_ingr, data = datos_long, index = c("CVE", "year"),
                    listw = listw_knn, model = "within", effect = "twoways",
                    spatial.error = "b", lag = FALSE)
summary(sem_knn_sin)

# SDM con k-vecinos (opcional)
sdm_knn_sin <- spml(form_sin_ingr, data = datos_long, index = c("CVE", "year"),
                    listw = listw_knn, model = "within", effect = "twoways",
                    spatial.error = "none", lag = TRUE, Durbin = TRUE)
summary(sdm_knn_sin)

# SAC con k-vecinos
sac_knn_sin <- spml(form_sin_ingr, data = datos_long, index = c("CVE", "year"),
                    listw = listw_knn, model = "within", effect = "twoways",
                    spatial.error = "b", lag = TRUE)
summary(sac_knn_sin)





#regresiones con matriz queen y errores robustos
# Modelo SAR con matriz de contigüidad reina
sar_queen <- spml(form_sin_ingr, data = datos_long, index = c("CVE", "year"),
                  listw = listw_queen, model = "within", effect = "twoways",
                  spatial.error = "none", lag = TRUE)
summary(sar_queen)


#checamos base de datos
colSums(is.na(datos_long))
aggregate(datos_long[, c("Inci", "densidad", "IDS")], by = list(datos_long$CVE), summary)






#checamos la matriz de contigüidad reina
plot(st_geometry(shp), border = "grey", main = "Vecindad tipo Reina")
plot(nb_queen, coords, add = TRUE, col = "blue", points = FALSE)
# Identificar estados sin vecinos o con pocos vecinos
card_queen <- card(nb_queen)
names(card_queen) <- shp$CVE_ENT  # ajusta el nombre de la variable ID
print("Número de vecinos por estado:")
print(sort(card_queen))


#checamos otras dos matrices .gal
library(spdep)
nb_gal1 <- read.gal("/Users/dmares/Documents/Doctorado unam/Seminario 7mo sem/Geoda6SemID.gal", override.id = TRUE)
nb_gal2 <- read.gal("/Users/dmares/Documents/Doctorado unam/Seminario 7mo sem/Queen1.gal", override.id = TRUE)
nb_gal3 <- read.gal("/Users/dmares/Documents/Doctorado unam/Seminario 6to sem/Geoda6Sem.gal", override.id = TRUE)
nb_gal4 <- read.gal("/Users/dmares/Documents/Doctorado unam/Seminario 5to semestre/contiguidad.gal", override.id = TRUE)
nb_gal5 <- read.gal("/Users/dmares/Documents/Doctorado unam/Seminario 6to sem/Geoda6SemSYM.gal", override.id = TRUE)


id_gal1 <- attr(nb_gal1, "region.id")
print(id_gal1)

id_gal2 <- attr(nb_gal2, "region.id")
print(id_gal2)

id_gal3 <- attr(nb_gal3, "region.id")
print(id_gal3)

id_gal4 <- attr(nb_gal4, "region.id")
print(id_gal4)

id_gal5 <- attr(nb_gal5, "region.id")
print(id_gal5)




listw_gal1 <- nb2listw(nb_gal1, style = "W")
listw_gal2 <- nb2listw(nb_gal2, style = "W")
listw_gal3 <- nb2listw(nb_gal3, style = "W")
listw_gal4 <- nb2listw(nb_gal4, style = "W")
listw_gal5 <- nb2listw(nb_gal5, style = "W")



card_gal1 <- card(nb_gal1)
names(card_gal1) <- attr(nb_gal1, "region.id")
sort(card_gal1)

card_gal2 <- card(nb_gal2)
names(card_gal2) <- attr(nb_gal2, "region.id")
sort(card_gal2)

card_gal3 <- card(nb_gal3)
names(card_gal3) <- attr(nb_gal3, "region.id")
sort(card_gal3)

card_gal4 <- card(nb_gal4)
names(card_gal4) <- attr(nb_gal4, "region.id")
sort(card_gal4)

card_gal5 <- card(nb_gal5)
names(card_gal5) <- attr(nb_gal5, "region.id")
sort(card_gal5)




#antes que nada, primero probar por qué rho es negativa
ols_ind <- plm(form, data = datos_long, index = c("CVE", "year"), 
               model = "within", effect = "individual")
res_ind <- residuals(ols_ind)
index_df <- attr(res_ind, "index")
cve_vector <- index_df[[1]]  # primera columna = CVE
res_avg <- aggregate(res_ind, by = list(cve_vector), FUN = mean)
names(res_avg) <- c("CVE", "resid_prom")
res_avg <- res_avg[match(rownames(coords), res_avg$CVE), ]
moran.test(res_avg$resid_prom, listw = listw_inv, randomisation = FALSE)


# Ordenar según el orden de los estados en la matriz de pesos (coords)
res_avg <- res_avg[match(rownames(coords), res_avg$CVE), ]

# Aplicar test de Moran
moran.test(res_avg$resid_prom, listw = listw_inv, randomisation = FALSE)








# Calcular valores propios de la matriz (requiere matriz densa)
eigenW <- eigen(W_inv, only.values = TRUE)$values
range(Re(eigenW))  # Debe estar entre -1 y 1 aproximadamente




#calcular centroides
coords <- st_centroid(shp) %>% st_coordinates()
rownames(coords) <- shp$CVE


# Crear lista de vecinos por contigüidad (reina)
nb <- poly2nb(mx, queen = TRUE)
# Verificar resumen
summary(nb)
# Convertir a lista de pesos estandarizada (W style: cada fila suma 1)
listw <- nb2listw(nb, style = "W", zero.policy = TRUE)

# Opcional: convertir a matriz para usos posteriores
W_matrix <- nb2mat(nb, style = "W", zero.policy = TRUE)


# Asegura que mx_data (con geometría) esté ordenado por CVE
mx_data <- mx_data[order(mx_data$CVE), ]
nb <- poly2nb(mx_data, queen = TRUE)
listw <- nb2listw(nb, style = "W")



# ====================================================
# 7. Enfoque de Mundlak para decidir entre FE y RE
# ====================================================
# Variables explicativas (sin incluir la dependiente)
vars_x <- c("ingr", "gini", "pobreza", "escolaridad", "IDS", 
            "densidad", "CrecimientoVA", "CrecimientoGastosalud")

# Calcular medias por estado (para el enfoque de Mundlak)
panel_data <- panel_data %>%
  group_by(CVE) %>%
  mutate(across(all_of(vars_x), 
                mean, 
                na.rm = TRUE, 
                .names = "mean_{.col}")) %>%
  ungroup()

# Estimar modelo con medias (efectos aleatorios aumentado)
form_mundlak <- as.formula(paste(
  "Inci ~", 
  paste(c(vars_x, paste0("mean_", vars_x)), collapse = " + ")
))

mundlak_model <- plm(form_mundlak, 
                     data = panel_data, 
                     model = "random",
                     index = c("CVE", "year"))

install.packages("car")
library(car)

# Test de significancia conjunta de las medias
# Esto es equivalente al test de Hausman robusto
#Chi significativa se rechaza la hipotesis nula de que las medias con cero, se acepta efecto fijos
linearHypothesis(mundlak_model, 
                 paste0("mean_", vars_x, " = 0"),
                 test = "Chisq")


# Verificar datos completos
summary(panel_data[, c("Inci", vars_x)])
# Si hay NA, podemos imputar o eliminar. En este caso, asumimos que ya están limpios.
# Verificar que las variables WX existen
names(panel_data)



# ====================================================
# Fase 4: Crear rezagos espaciales de las variables X
# ====================================================

# Convertir panel_data a data.frame normal y asegurar tipos
panel_data_df <- as.data.frame(panel_data)
panel_data_df$year <- as.numeric(as.character(panel_data_df$year))  # de factor a numérico
panel_data_df <- panel_data_df %>% arrange(CVE, year)  # ordenar para consistencia

# Asegurar que la matriz de pesos W_mat tenga el mismo orden que los CVE únicos
# (Asumimos que los CVE en panel_data_df están en el orden correcto después de arrange)
unique_CVE <- unique(panel_data_df$CVE)
if (!all(unique_CVE == rownames(W_mat))) {
  # Reordenar filas y columnas de W_mat para que coincidan con el orden de unique_CVE
  W_mat <- W_mat[unique_CVE, unique_CVE]
}

# Función para crear WX (ya definida, pero la incluimos por claridad)
create_WX <- function(data, var_name, W_mat, index) {
  # data: data.frame normal con columnas CVE, year y var_name
  # W_mat: matriz de pesos con nombres de fila/columna igual a los CVE
  # index: vector con nombres de las columnas id y tiempo (no se usa directamente)
  
  # Obtener datos en formato wide por año
  data_wide <- data %>%
    select(CVE, year, all_of(var_name)) %>%
    pivot_wider(id_cols = CVE, names_from = year, values_from = all_of(var_name))
  
  # Ordenar por CVE (debe coincidir con el orden de W_mat)
  data_wide <- data_wide[order(data_wide$CVE), ]
  
  # Crear matriz de datos (solo años)
  X_mat <- as.matrix(data_wide[, -1])
  
  # Calcular WX para cada año (W %*% X de ese año)
  WX_mat <- W_mat %*% X_mat
  
  # Convertir de vuelta a formato largo
  result <- as.data.frame(WX_mat)
  names(result) <- names(data_wide)[-1]
  result$CVE <- data_wide$CVE
  
  result_long <- result %>%
    pivot_longer(cols = -CVE, names_to = "year", values_to = paste0("W_", var_name)) %>%
    mutate(year = as.numeric(year))
  
  return(result_long)
}

# Aplicar a cada variable X
WX_vars <- c()
for (var in vars_x) {
  WX_data <- create_WX(panel_data_df, var, W_mat, c("CVE", "year"))
  panel_data_df <- panel_data_df %>% left_join(WX_data, by = c("CVE", "year"))
  WX_vars <- c(WX_vars, paste0("W_", var))
}

# Reconstruir pdata.frame con las nuevas variables
library(plm)
panel_data <- pdata.frame(panel_data_df, index = c("CVE", "year"))

# Verificar que se crearon correctamente
head(panel_data)




#checar convergencia:
# Modelo SDM sin opciones de control adicionales (valores por defecto)
sdm_model <- spml(form_sdm, 
                  data = panel_data,
                  index = c("CVE", "year"),
                  listw = listw,
                  model = "within",
                  effect = "twoways",
                  lag = TRUE,
                  spatial.error = "none",
                  LeeYu = TRUE)

sdm_model <- spml(form_sdm, 
                  data = panel_data,
                  index = c("CVE", "year"),
                  listw = listw,
                  model = "within",
                  effect = "twoways",
                  lag = TRUE,
                  spatial.error = "none",
                  LeeYu = TRUE)
summary(sdm_model)

install.packages("MASS")  # si no lo tienes
library(MASS)


# Coeficientes del modelo
coefs <- sdm_model$coefficients
print(coefs)

# Matriz de varianza-covarianza (para las simulaciones)
vcov_matrix <- vcov(sdm_model)

# Identificar el número de variables X (sin contar lambda)
n_X <- 8  # porque tienes 8 variables explicativas

# Separar coeficientes: el primero es lambda (rho), luego vienen beta_X y beta_WX
rho <- coefs[1]
beta_X <- coefs[2:(1+n_X)]
beta_WX <- coefs[(2+n_X):(1+2*n_X)]

# Nombres de las variables (deben coincidir con el orden en coefs)
vars_x <- c("ingr", "gini", "pobreza", "escolaridad", "IDS", "densidad", 
            "CrecimientoVA", "CrecimientoGastosalud")


# Convertir listw a matriz (si no lo hiciste antes)
W_mat <- listw2mat(listw)
N <- nrow(W_mat)          # número de estados (32)
T <- length(unique(panel_data$year))  # número de años (10)


# Número de simulaciones
R <- 1000

# Preparar matriz identidad
I_N <- Diagonal(N)

# Precalcular (I - ρW)⁻¹ para el ρ observado (aunque en simulación variará)
# Para los efectos puntuales usamos el ρ estimado
rho_est <- rho

# Matriz de impactos para cada variable (usando el ρ estimado)
# Primero, calcular (I - ρW)⁻¹
inv_rho <- solve(I_N - rho_est * W_mat)

effects_results <- data.frame(
  Variable = vars_x,
  Directo = NA,
  Indirecto = NA,
  Total = NA,
  p_Directo = NA,
  p_Indirecto = NA,
  p_Total = NA
)

# Simulación de coeficientes
set.seed(123)
coef_sim <- mvrnorm(R, coefs, vcov_matrix)

for (i in 1:n_X) {
  # Coeficientes para esta variable (estimados)
  beta_i <- beta_X[i]
  theta_i <- beta_WX[i]
  
  # Matriz de impactos para la variable i (con ρ estimado)
  S_i <- inv_rho %*% (beta_i * I_N + theta_i * W_mat)
  
  # Efectos promedio
  directo <- mean(diag(S_i))
  total <- mean(rowSums(S_i))
  indirecto <- total - directo
  
  effects_results[i, "Directo"] <- directo
  effects_results[i, "Indirecto"] <- indirecto
  effects_results[i, "Total"] <- total
  
  # Simulación para p-valores
  directo_sim <- numeric(R)
  indirecto_sim <- numeric(R)
  total_sim <- numeric(R)
  
  for (r in 1:R) {
    rho_sim <- coef_sim[r, 1]
    beta_sim <- coef_sim[r, 2:(1+n_X)]
    theta_sim <- coef_sim[r, (2+n_X):(1+2*n_X)]
    
    # Matriz (I - ρ_sim W)⁻¹
    inv_rho_sim <- solve(I_N - rho_sim * W_mat)
    
    # Matriz de impactos para la variable i en esta simulación
    S_sim <- inv_rho_sim %*% (beta_sim[i] * I_N + theta_sim[i] * W_mat)
    
    directo_sim[r] <- mean(diag(S_sim))
    total_sim[r] <- mean(rowSums(S_sim))
    indirecto_sim[r] <- total_sim[r] - directo_sim[r]
  }
  
  # p-valores (dos colas)
  effects_results[i, "p_Directo"] <- mean(abs(directo_sim - mean(directo_sim)) >= abs(directo))
  effects_results[i, "p_Indirecto"] <- mean(abs(indirecto_sim - mean(indirecto_sim)) >= abs(indirecto))
  effects_results[i, "p_Total"] <- mean(abs(total_sim - mean(total_sim)) >= abs(total))
}

# Mostrar resultados con formato
effects_results %>%
  mutate(across(where(is.numeric), ~ round(., 4)))


residuos <- residuals(sdm_model)
resid_matrix <- matrix(residuos, nrow = N, ncol = T, byrow = FALSE)

moran_results <- data.frame(
  Year = unique(panel_data$year),
  Moran_I = NA,
  p_value = NA
)

for (t in 1:T) {
  moran_test <- moran.test(resid_matrix[, t], listw = listw, randomisation = FALSE)
  moran_results[t, "Moran_I"] <- moran_test$estimate[1]
  moran_results[t, "p_value"] <- moran_test$p.value
}
print(moran_results)


# Modelo SAR (solo WY)
sar_model <- spml(as.formula(paste("Inci ~", paste(vars_x, collapse = " + "))), 
                  data = panel_data, index = c("CVE", "year"), listw = listw,
                  model = "within", effect = "twoways", lag = TRUE, 
                  spatial.error = "none", LeeYu = TRUE)

# Modelo SEM (error espacial)
sem_model <- spml(as.formula(paste("Inci ~", paste(vars_x, collapse = " + "))), 
                  data = panel_data, index = c("CVE", "year"), listw = listw,
                  model = "within", effect = "twoways", lag = FALSE, 
                  spatial.error = "b", LeeYu = TRUE)

# Comparación con log-likelihood
# Extraer log-verosimilitud de los modelos
logLik_sar <- sar_model$logLik
logLik_sem <- sem_model$logLik
logLik_sdm <- sdm_model$logLik

# Número de parámetros (incluye el rezago espacial, coeficientes de X, y posibles parámetros de error)
# Para SAR: 1 (rho) + length(vars_x) + (efectos fijos implícitos, pero no cuentan en k porque son parte de la especificación within)
# Para SEM: 1 (lambda) + length(vars_x)
# Para SDM: 1 (rho) + 2*length(vars_x)

k_sar <- 1 + length(vars_x)
k_sem <- 1 + length(vars_x)
k_sdm <- 1 + 2 * length(vars_x)

# Número de observaciones (320)
n_obs <- nrow(panel_data)

# Calcular AIC y BIC
AIC_sar <- -2 * logLik_sar + 2 * k_sar
BIC_sar <- -2 * logLik_sar + k_sar * log(n_obs)

AIC_sem <- -2 * logLik_sem + 2 * k_sem
BIC_sem <- -2 * logLik_sem + k_sem * log(n_obs)

AIC_sdm <- -2 * logLik_sdm + 2 * k_sdm
BIC_sdm <- -2 * logLik_sdm + k_sdm * log(n_obs)

# Mostrar resultados
cat("AIC: SAR =", AIC_sar, ", SEM =", AIC_sem, ", SDM =", AIC_sdm, "\n")
cat("BIC: SAR =", BIC_sar, ", SEM =", BIC_sem, ", SDM =", BIC_sdm, "\n")



LR_sar_sdm <- 2 * (logLik_sdm - logLik_sar)
p_value_sar_sdm <- pchisq(LR_sar_sdm, df = length(vars_x), lower.tail = FALSE)
cat("LR test SAR vs SDM: χ² =", LR_sar_sdm, ", gl =", length(vars_x), ", p =", p_value_sar_sdm, "\n")




moran.test(residuals(sdm_model), listw = listw, randomisation = FALSE)


# Instalar/actualizar splm si es necesario
# install.packages("splm")

# Modelo SAC con efectos aleatorios (para luego comparar)
sac_model_re <- spreml(form_sdm, 
                       data = panel_data,
                       index = c("CVE", "year"),
                       w = listw,        # matriz de pesos
                       lag = TRUE,        # incluye WY
                       errors = "semre",  # error espacial con efectos aleatorios
                       method = "BFGS")

summary(sac_model_re)









#corremos modelos con k=2 vecinos
knn2 <- knearneigh(coords, k = 2)
nb_knn2 <- knn2nb(knn2)
listw_knn2 <- nb2listw(nb_knn2, style = "W")

sar_knn2 <- spml(form_sin_ingr, data = datos_long, index = c("CVE", "year"),
                 listw = listw_knn2, model = "within", effect = "twoways",
                 spatial.error = "none", lag = TRUE)
summary(sar_knn2)

knn3 <- knearneigh(coords, k = 3)
nb_knn3 <- knn2nb(knn3)
listw_knn3 <- nb2listw(nb_knn3, style = "W")

sar_knn3 <- spml(form_sin_ingr, data = datos_long, index = c("CVE", "year"),
                 listw = listw_knn3, model = "within", effect = "twoways",
                 spatial.error = "none", lag = TRUE)
summary(sar_knn3)




#####################################
#######se hace análisis espacial con k=3
#################################

form_sin_ingr <- Inci ~ gini + pobreza + CrecimientoVA + escolaridad + densidad + IDS + CrecimientoGastosalud
knn3 <- knearneigh(coords, k = 3)
nb_knn3 <- knn2nb(knn3)
listw_knn3 <- nb2listw(nb_knn3, style = "W")

#estimar modelos sin errores robustos
# SAR
sar_k3 <- spml(form_sin_ingr, data = datos_long, index = c("CVE", "year"),
               listw = listw_knn3, model = "within", effect = "twoways",
               spatial.error = "none", lag = TRUE)
summary(sar_k3)
# SEM
sem_k3 <- spml(form_sin_ingr, data = datos_long, index = c("CVE", "year"),
               listw = listw_knn3, model = "within", effect = "twoways",
               spatial.error = "b", lag = FALSE)
summary(sem_k3)
# SAC (SARAR)
sac_k3 <- spml(form_sin_ingr, data = datos_long, index = c("CVE", "year"),
               listw = listw_knn3, model = "within", effect = "twoways",
               spatial.error = "b", lag = TRUE)
summary(sac_k3)
# SDM
sdm_k3 <- spml(form_sin_ingr, data = datos_long, index = c("CVE", "year"),
               listw = listw_knn3, model = "within", effect = "twoways",
               spatial.error = "none", lag = TRUE, Durbin = TRUE)
summary(sdm_k3)
# Función bootstrap para remuestreo de estados completos
boot_spml <- function(data, indices, model_type = "sar", listw = listw_knn3, form = form_sin_ingr) {
  estados_unicos <- unique(data$CVE)
  estados_boot <- sample(estados_unicos, size = length(estados_unicos), replace = TRUE)
  
  data_boot <- do.call(rbind, lapply(estados_boot, function(cve) {
    subset(data, CVE == cve)
  }))
  
  tryCatch({
    if (model_type == "sar") {
      mod <- spml(form, data = data_boot, index = c("CVE", "year"),
                  listw = listw, model = "within", effect = "twoways",
                  spatial.error = "none", lag = TRUE)
    } else if (model_type == "sem") {
      mod <- spml(form, data = data_boot, index = c("CVE", "year"),
                  listw = listw, model = "within", effect = "twoways",
                  spatial.error = "b", lag = FALSE)
    } else if (model_type == "sac") {
      mod <- spml(form, data = data_boot, index = c("CVE", "year"),
                  listw = listw, model = "within", effect = "twoways",
                  spatial.error = "b", lag = TRUE)
    } else if (model_type == "sdm") {
      mod <- spml(form, data = data_boot, index = c("CVE", "year"),
                  listw = listw, model = "within", effect = "twoways",
                  spatial.error = "none", lag = TRUE, Durbin = TRUE)
    }
    return(coef(mod))
  }, error = function(e) return(rep(NA, length(coef(sar_k3)))))  # usar sar_k3 como referencia para longitud
}

# Ejecutar bootstrap para cada modelo (R=200 para prueba, aumentar a 1000 en versión final)
remove.packages("boot")
install.packages("boot")
library(boot)

sar_k3 <- spml(form_sin_ingr, data = datos_long, index = c("CVE", "year"),
               listw = listw_knn3, model = "within", effect = "twoways",
               spatial.error = "none", lag = TRUE)


boot_spml <- function(data, indices, model_type = "sar", listw = listw_knn3, form = form_sin_ingr) {
  estados_unicos <- unique(data$CVE)
  estados_boot <- sample(estados_unicos, size = length(estados_unicos), replace = TRUE)
  
  data_boot <- do.call(rbind, lapply(estados_boot, function(cve) {
    subset(data, CVE == cve)
  }))
  
  tryCatch({
    if (model_type == "sar") {
      mod <- spml(form, data = data_boot, index = c("CVE", "year"),
                  listw = listw, model = "within", effect = "twoways",
                  spatial.error = "none", lag = TRUE)
    } else if (model_type == "sem") {
      mod <- spml(form, data = data_boot, index = c("CVE", "year"),
                  listw = listw, model = "within", effect = "twoways",
                  spatial.error = "b", lag = FALSE)
    } else if (model_type == "sac") {
      mod <- spml(form, data = data_boot, index = c("CVE", "year"),
                  listw = listw, model = "within", effect = "twoways",
                  spatial.error = "b", lag = TRUE)
    } else if (model_type == "sdm") {
      mod <- spml(form, data = data_boot, index = c("CVE", "year"),
                  listw = listw, model = "within", effect = "twoways",
                  spatial.error = "none", lag = TRUE, Durbin = TRUE)
    }
    return(coef(mod))
  }, error = function(e) {
    # En caso de error, devolver NA con la longitud correcta
    return(rep(NA, length(coef(sar_k3))))
  })
}


set.seed(123)
boot_sar_prueba <- boot(datos_long, boot_spml, R = 50, model_type = "sar", 
                        listw = listw_knn3, form = form_sin_ingr)
# Si no hay error, procede con R=200


#como hay error, usar errores robustos
library(splm)
library(lmtest)


##################
# Asegurar que listw_knn3 está definido (k=3)
knn3 <- knearneigh(coords, k = 3)
nb_knn3 <- knn2nb(knn3)
listw_knn3 <- nb2listw(nb_knn3, style = "W")


# SAR 
sar_k3 <- spml(form_sin_ingr, data = datos_long, index = c("CVE", "year"),
               listw = listw_knn3, model = "within", effect = "twoways",
               spatial.error = "none", lag = TRUE)

# SEM
sem_k3 <- spml(form_sin_ingr, data = datos_long, index = c("CVE", "year"),
               listw = listw_knn3, model = "within", effect = "twoways",
               spatial.error = "b", lag = FALSE)

# SAC (SARAR)
sac_k3 <- spml(form_sin_ingr, data = datos_long, index = c("CVE", "year"),
               listw = listw_knn3, model = "within", effect = "twoways",
               spatial.error = "b", lag = TRUE)

# SDM
sdm_k3 <- spml(form_sin_ingr, data = datos_long, index = c("CVE", "year"),
               listw = listw_knn3, model = "within", effect = "twoways",
               spatial.error = "none", lag = TRUE, Durbin = TRUE)


impacts(sar_k3, listw = listw_knn3, time = 10)
impacts(sac_k3, listw = listw_knn3, time = 10)
impacts(sdm_k3, listw = listw_knn3, time = 10)



# Coeficientes espaciales
summary(sar_k3)$Coef[1, ]  # lambda del SAR
summary(sem_k3)$Coef[1, ]  # rho del SEM
# En SAC hay dos: lambda (lag) y rho (error)
summary(sac_k3)$Coef[1:2, ]
# En SDM el lambda es el primero (igual que en SAR)
summary(sdm_k3)$Coef[1, ]



#obtener impactos para mapear
fix_estados <- fixef(sar_k3, effect = "individual")
df_fix <- data.frame(CVE = names(fix_estados), efecto_fijo = as.numeric(fix_estados))
write.csv(df_fix, "efectos_fijos_individuales.csv", row.names = FALSE)
residuos <- residuals(sar_k3)
index_df <- attr(residuos, "index")
cve_vector <- index_df[[1]]
res_avg <- aggregate(residuos, by = list(cve_vector), FUN = mean)
names(res_avg) <- c("CVE", "resid_prom")
write.csv(res_avg, "residuos_promedio.csv", row.names = FALSE)


#para exportar y hacer mapa en Qgis
# Efectos fijos individuales (por estado)
fix_estados <- fixef(sar_k3, effect = "individual")
df_fix <- data.frame(CVE = names(fix_estados), efecto_fijo = as.numeric(fix_estados))
write.csv(df_fix, "efectos_fijos_individuales.csv", row.names = FALSE)
# Residuos promedio por estado
residuos <- residuals(sar_k3)
index_df <- attr(residuos, "index")
cve_vector <- index_df[[1]]
res_avg <- aggregate(residuos, by = list(cve_vector), FUN = mean)
names(res_avg) <- c("CVE", "resid_prom")
write.csv(res_avg, "residuos_promedio.csv", row.names = FALSE)


# Calcular logLik, AIC y BIC para cada modelo
# Nota: La función logLik existe para objetos spml, y AIC/BIC se derivan de ella.

# SAR
logLik_sar <- logLik(sar_k3)
aic_sar <- AIC(sar_k3)
bic_sar <- BIC(sar_k3)

# SEM
logLik_sem <- logLik(sem_k3)
aic_sem <- AIC(sem_k3)
bic_sem <- BIC(sem_k3)

# SAC
logLik_sac <- logLik(sac_k3)
aic_sac <- AIC(sac_k3)
bic_sac <- BIC(sac_k3)

# SDM
logLik_sdm <- logLik(sdm_k3)
aic_sdm <- AIC(sdm_k3)
bic_sdm <- BIC(sdm_k3)

# Crear tabla comparativa
tabla_modelos <- data.frame(
  Modelo = c("SAR", "SEM", "SAC", "SDM"),
  logLik = c(logLik_sar, logLik_sem, logLik_sac, logLik_sdm),
  AIC = c(aic_sar, aic_sem, aic_sac, aic_sdm),
  BIC = c(bic_sar, bic_sem, bic_sac, bic_sdm)
)

print(tabla_modelos)



# Función para extraer logLik de objetos spml (basado en la estructura)
logLik.spml <- function(object) {
  # La log-verosimilitud suele estar en object$logLik
  if (!is.null(object$logLik)) {
    return(object$logLik)
  } else {
    # Alternativa: calcular a partir de residuos y número de parámetros
    # pero es más seguro extraerla del objeto
    stop("No se pudo encontrar logLik en el objeto")
  }
}

# Aplicar
ll_sar <- as.numeric(logLik_sar)
ll_sem <- as.numeric(logLik_sem)
ll_sdm <- as.numeric(logLik_sdm)
# SAC no tiene
ll_sac <- NA

# Número de parámetros
k_sar <- length(coef(sar_k3))
k_sem <- length(coef(sem_k3))
k_sac <- length(coef(sac_k3))
k_sdm <- length(coef(sdm_k3))

# Calcular AIC y BIC
aic_sar <- -2 * ll_sar + 2 * k_sar
bic_sar <- -2 * ll_sar + log(nrow(datos_long)) * k_sar

aic_sem <- -2 * ll_sem + 2 * k_sem
bic_sem <- -2 * ll_sem + log(nrow(datos_long)) * k_sem

aic_sdm <- -2 * ll_sdm + 2 * k_sdm
bic_sdm <- -2 * ll_sdm + log(nrow(datos_long)) * k_sdm

# Para SAC, dejamos NA
aic_sac <- NA
bic_sac <- NA

# Tabla
tabla_modelos <- data.frame(
  Modelo = c("SAR", "SEM", "SAC", "SDM"),
  logLik = c(ll_sar, ll_sem, ll_sac, ll_sdm),
  AIC = c(aic_sar, aic_sem, aic_sac, aic_sdm),
  BIC = c(bic_sar, bic_sem, bic_sac, bic_sdm),
  lambda = c(coef(sar_k3)[1], NA, coef(sac_k3)[1], coef(sdm_k3)[1]),
  rho = c(NA, coef(sem_k3)[1], coef(sac_k3)[2], NA)
)
print(tabla_modelos)





# TABLA COMPARATIVA DE MODELOS (k=3, sin ingreso)
# Versión corregida (logLik manual para Pooled)
# ==================================================

library(plm)
library(spdep)
library(splm)

# Asegurar que la fórmula y la matriz de pesos están definidas
form_sin_ingr <- Inci ~ gini + pobreza + CrecimientoVA + escolaridad + densidad + IDS + CrecimientoGastosalud

# Modelo Pooled (sin efectos fijos)
pooled <- plm(form_sin_ingr, data = datos_long, index = c("CVE", "year"), model = "pooling")

# Modelos espaciales con efectos fijos two-ways (si no existen, estímalos)
# SAR
sar_k3 <- spml(form_sin_ingr, data = datos_long, index = c("CVE", "year"),
               listw = listw_knn3, model = "within", effect = "twoways",
               spatial.error = "none", lag = TRUE)

# SEM
sem_k3 <- spml(form_sin_ingr, data = datos_long, index = c("CVE", "year"),
               listw = listw_knn3, model = "within", effect = "twoways",
               spatial.error = "b", lag = FALSE)

# SAC
sac_k3 <- spml(form_sin_ingr, data = datos_long, index = c("CVE", "year"),
               listw = listw_knn3, model = "within", effect = "twoways",
               spatial.error = "b", lag = TRUE)

# SDM
sdm_k3 <- spml(form_sin_ingr, data = datos_long, index = c("CVE", "year"),
               listw = listw_knn3, model = "within", effect = "twoways",
               spatial.error = "none", lag = TRUE, Durbin = TRUE)

# ==================================================
# Función para extraer coeficientes con formato
# ==================================================
extraer_coef <- function(modelo, tipo) {
  s <- summary(modelo)
  coefs <- s$Coef
  # Extraer parámetros espaciales según el tipo
  if (tipo == "SAR") {
    esp <- coefs["lambda", , drop = FALSE]
    coefs <- coefs[!rownames(coefs) %in% c("lambda"), , drop = FALSE]
  } else if (tipo == "SEM") {
    esp <- coefs["rho", , drop = FALSE]
    coefs <- coefs[!rownames(coefs) %in% c("rho"), , drop = FALSE]
  } else if (tipo == "SAC") {
    esp <- coefs[c("lambda", "rho"), , drop = FALSE]
    coefs <- coefs[!rownames(coefs) %in% c("lambda", "rho"), , drop = FALSE]
  } else if (tipo == "SDM") {
    esp <- coefs["lambda", , drop = FALSE]
    coefs <- coefs[!rownames(coefs) %in% c("lambda"), , drop = FALSE]
  } else {
    esp <- NULL
  }
  list(coefs = coefs, esp = esp)
}

# Para el modelo pooled (no espacial)
pool_s <- summary(pooled)
pool_coefs <- pool_s$coefficients

# ==================================================
# Construir tabla base con todas las variables
# ==================================================
vars_comunes <- c("gini", "pobreza", "CrecimientoVA", "escolaridad", 
                  "densidad", "IDS", "CrecimientoGastosalud")
# Para SDM, pueden aparecer variables con prefijo "lag_"
# Las identificaremos más adelante

tabla <- data.frame(
  Variable = c(vars_comunes, "lambda", "rho", "logLik", "R²"),
  Pooled = NA_character_,
  SAR = NA_character_,
  SEM = NA_character_,
  SAC = NA_character_,
  SDM = NA_character_,
  stringsAsFactors = FALSE
)

# ==================================================
# Llenar modelo Pooled
# ==================================================
for (v in vars_comunes) {
  if (v %in% rownames(pool_coefs)) {
    coef <- pool_coefs[v, 1]
    se <- pool_coefs[v, 2]
    pval <- pool_coefs[v, 4]
    star <- ifelse(pval < 0.01, "***", ifelse(pval < 0.05, "**", ifelse(pval < 0.1, "*", "")))
    tabla[tabla$Variable == v, "Pooled"] <- sprintf("%.3f%s\n(%.3f)", coef, star, se)
  }
}
# R² del modelo Pooled
tabla[tabla$Variable == "R²", "Pooled"] <- sprintf("%.3f", summary(pooled)$r.squared[1])
# logLik manual para pooled (MCO)
n_pool <- length(residuals(pooled))
rss_pool <- sum(residuals(pooled)^2)
ll_pool <- -n_pool/2 * (log(2*pi) + log(rss_pool/n_pool) + 1)
tabla[tabla$Variable == "logLik", "Pooled"] <- sprintf("%.3f", ll_pool)

# ==================================================
# Llenar modelos espaciales
# ==================================================
modelos <- list(
  SAR = list(obj = sar_k3, tipo = "SAR"),
  SEM = list(obj = sem_k3, tipo = "SEM"),
  SAC = list(obj = sac_k3, tipo = "SAC"),
  SDM = list(obj = sdm_k3, tipo = "SDM")
)

for (nom in names(modelos)) {
  m <- modelos[[nom]]
  extraido <- extraer_coef(m$obj, m$tipo)
  
  # Coeficientes de las variables
  if (!is.null(extraido$coefs) && nrow(extraido$coefs) > 0) {
    for (v in rownames(extraido$coefs)) {
      coef <- extraido$coefs[v, 1]
      se <- extraido$coefs[v, 2]
      pval <- extraido$coefs[v, 4]
      star <- ifelse(pval < 0.01, "***", ifelse(pval < 0.05, "**", ifelse(pval < 0.1, "*", "")))
      # Si la variable no existe en la tabla, añadirla
      if (!v %in% tabla$Variable) {
        nueva_fila <- data.frame(Variable = v, Pooled = NA, SAR = NA, SEM = NA, SAC = NA, SDM = NA, stringsAsFactors = FALSE)
        tabla <- rbind(tabla, nueva_fila)
      }
      tabla[tabla$Variable == v, nom] <- sprintf("%.3f%s\n(%.3f)", coef, star, se)
    }
  }
  
  # Parámetros espaciales
  if (!is.null(extraido$esp) && nrow(extraido$esp) > 0) {
    for (v in rownames(extraido$esp)) {
      coef <- extraido$esp[v, 1]
      se <- extraido$esp[v, 2]
      pval <- extraido$esp[v, 4]
      star <- ifelse(pval < 0.01, "***", ifelse(pval < 0.05, "**", ifelse(pval < 0.1, "*", "")))
      tabla[tabla$Variable == v, nom] <- sprintf("%.3f%s\n(%.3f)", coef, star, se)
    }
  }
  
  # R² (pseudo R² como correlación al cuadrado entre observado y predicho)
  if (!is.null(m$obj$fitted.values)) {
    pred <- as.numeric(m$obj$fitted.values)
    obs <- datos_long$Inci
    r2 <- cor(obs, pred)^2
    tabla[tabla$Variable == "R²", nom] <- sprintf("%.3f", r2)
  } else {
    tabla[tabla$Variable == "R²", nom] <- "NA"
  }
  
  # logLik (acceder directamente al campo)
  if (!is.null(m$obj$logLik)) {
    ll <- as.numeric(m$obj$logLik)[1]  # tomar el primer elemento
    tabla[tabla$Variable == "logLik", nom] <- sprintf("%.3f", ll)
  } else {
    tabla[tabla$Variable == "logLik", nom] <- "NA"
  }
}

# ==================================================
# Reordenar filas
# ==================================================
lag_vars <- grep("^lag_", tabla$Variable, value = TRUE)
orden_base <- c(vars_comunes, lag_vars, "lambda", "rho", "logLik", "R²")
orden_base <- intersect(orden_base, tabla$Variable)
tabla <- tabla[match(orden_base, tabla$Variable), ]

# ==================================================
# Mostrar tabla
# ==================================================
print(tabla)

# Opcional: exportar a CSV
write.csv(tabla, "tabla_modelos_comparativa.csv", row.names = FALSE, na = "")






# ==================================================
# TABLA DE IMPACTOS DIRECTOS, INDIRECTOS Y TOTALES CON SIGNIFICANCIA
# Modelos SAR, SDM y SAC (k=3)
# ==================================================

# Datos de impactos SAR (valores exactos)
impactos_sar <- data.frame(
  Variable = c("gini", "pobreza", "CrecimientoVA", "escolaridad", 
               "densidad", "IDS", "CrecimientoGastosalud"),
  Directo = c(0.592155123, -0.017646711, -0.686849005, -1.208978204, 
              0.037195261, -0.004585391, 0.008398309),
  Indirecto = c(-0.1213317746, 0.0036157869, 0.1407344216, 0.2477179802, 
                -0.0076212581, 0.0009395404, -0.0017208021),
  Total = c(0.470823348, -0.014030924, -0.546114584, -0.961260224, 
            0.029574003, -0.003645851, 0.006677507),
  stringsAsFactors = FALSE
)

# Redondear a 4 decimales
impactos_sar[, 2:4] <- round(impactos_sar[, 2:4], 4)

# Añadir significancia basada en los p-valores del modelo SAR
# (extraídos de summary(sar_k3))
pvals_sar <- c(0.640242, 0.302708, 0.152474, 0.203511, 0.001544, 1.587e-06, 0.950493)
significancia <- ifelse(pvals_sar < 0.01, "***", ifelse(pvals_sar < 0.05, "**", ifelse(pvals_sar < 0.1, "*", "ns")))

# Crear columnas con asteriscos
impactos_sar$Directo_sig <- paste0(impactos_sar$Directo, significancia)
impactos_sar$Indirecto_sig <- paste0(impactos_sar$Indirecto, significancia)
impactos_sar$Total_sig <- paste0(impactos_sar$Total, significancia)
impactos_sar$Modelo <- "SAR"

# SDM (mismos valores y significancia que SAR, porque los rezagos no fueron significativos)
impactos_sdm <- impactos_sar
impactos_sdm$Modelo <- "SDM"

# SAC (no significativo, ponemos "ns" y valores NA)
impactos_sac <- data.frame(
  Variable = impactos_sar$Variable,
  Directo = NA_real_,
  Indirecto = NA_real_,
  Total = NA_real_,
  Directo_sig = "ns",
  Indirecto_sig = "ns",
  Total_sig = "ns",
  Modelo = "SAC",
  stringsAsFactors = FALSE
)

# Combinar
impactos_completo <- rbind(
  impactos_sar[, c("Variable", "Modelo", "Directo_sig", "Indirecto_sig", "Total_sig")],
  impactos_sdm[, c("Variable", "Modelo", "Directo_sig", "Indirecto_sig", "Total_sig")],
  impactos_sac[, c("Variable", "Modelo", "Directo_sig", "Indirecto_sig", "Total_sig")]
)

# Pivotar a formato ancho para comparar modelos
library(tidyr)
impactos_wide <- pivot_wider(impactos_completo,
                             id_cols = Variable,
                             names_from = Modelo,
                             values_from = c(Directo_sig, Indirecto_sig, Total_sig),
                             names_glue = "{Modelo}_{.value}")

# Reordenar columnas
orden_cols <- c("Variable")
for (mod in c("SAR", "SDM", "SAC")) {
  orden_cols <- c(orden_cols, paste0(mod, "_Directo_sig"), 
                  paste0(mod, "_Indirecto_sig"), paste0(mod, "_Total_sig"))
}
impactos_wide <- impactos_wide[, intersect(orden_cols, colnames(impactos_wide))]

# Mostrar tabla
print(impactos_wide)

# Exportar a CSV
write.csv(impactos_wide, "tabla_impactos_con_significancia.csv", row.names = FALSE)







################################
# ==================================================
# MATRIZ DE CONTIGÜIDAD TIPO REINA
# ==================================================
library(sf)
library(spdep)
library(plm)
library(splm)

# Asegurar que el shapefile 'shp' está cargado y tiene un ID único (ej. CVE)
# Crear lista de vecinos por contigüidad Reina
nb_queen <- poly2nb(shp, queen = TRUE)
# Asignar los IDs de las entidades (ajusta el nombre de la columna ID)
attr(nb_queen, "region.id") <- shp$CVE

# Ver conectividad
print(sort(card(nb_queen)))  # Mostrará valores bajos (1,2,...) como antes

# Convertir a lista de pesos estandarizada por filas
listw_queen <- nb2listw(nb_queen, style = "W")

# Nota: algunos estados pueden tener 0 vecinos si el shapefile está muy fragmentado.
# Si hay ceros, poly2nb ya los asigna como list(), pero nb2listw fallará.
# En ese caso, se puede forzar un mínimo de 1 vecino con knn, pero aquí asumimos que no hay ceros.


# ==================================================
# ESTIMACIÓN DE MODELOS CON MATRIZ REINA
# ==================================================

# Fórmula (sin ingreso, como acordamos)
form_sin_ingr <- Inci ~ gini + pobreza + CrecimientoVA + escolaridad + densidad + IDS + CrecimientoGastosalud

# Modelo Pooled (MCO agrupado, no espacial)
pooled_queen <- plm(form_sin_ingr, data = datos_long, index = c("CVE", "year"), model = "pooling")

# Modelos espaciales con efectos fijos two-ways
# SAR
sar_queen <- spml(form_sin_ingr, data = datos_long, index = c("CVE", "year"),
                  listw = listw_queen, model = "within", effect = "twoways",
                  spatial.error = "none", lag = TRUE)

# SEM
sem_queen <- spml(form_sin_ingr, data = datos_long, index = c("CVE", "year"),
                  listw = listw_queen, model = "within", effect = "twoways",
                  spatial.error = "b", lag = FALSE)

# SAC (puede fallar si la matriz es mal condicionada)
sac_queen <- tryCatch(
  spml(form_sin_ingr, data = datos_long, index = c("CVE", "year"),
       listw = listw_queen, model = "within", effect = "twoways",
       spatial.error = "b", lag = TRUE),
  error = function(e) NULL
)

# SDM
sdm_queen <- tryCatch(
  spml(form_sin_ingr, data = datos_long, index = c("CVE", "year"),
       listw = listw_queen, model = "within", effect = "twoways",
       spatial.error = "none", lag = TRUE, Durbin = TRUE),
  error = function(e) NULL
)


# ==================================================
# FUNCIÓN PARA EXTRAER COEFICIENTES CON FORMATO
# ==================================================
extraer_coef <- function(modelo, tipo) {
  if (is.null(modelo)) return(list(coefs = NULL, esp = NULL))
  s <- summary(modelo)
  coefs <- s$Coef
  if (tipo == "SAR") {
    esp <- coefs["lambda", , drop = FALSE]
    coefs <- coefs[!rownames(coefs) %in% c("lambda"), , drop = FALSE]
  } else if (tipo == "SEM") {
    esp <- coefs["rho", , drop = FALSE]
    coefs <- coefs[!rownames(coefs) %in% c("rho"), , drop = FALSE]
  } else if (tipo == "SAC") {
    esp <- coefs[c("lambda", "rho"), , drop = FALSE]
    coefs <- coefs[!rownames(coefs) %in% c("lambda", "rho"), , drop = FALSE]
  } else if (tipo == "SDM") {
    esp <- coefs["lambda", , drop = FALSE]
    coefs <- coefs[!rownames(coefs) %in% c("lambda"), , drop = FALSE]
  } else {
    esp <- NULL
  }
  list(coefs = coefs, esp = esp)
}



# Asegurar librerías
library(splm)
library(plm)

# Definir fórmula y matriz (asumiendo que listw_queen ya está creada)
form_sin_ingr <- Inci ~ gini + pobreza + CrecimientoVA + escolaridad + densidad + IDS + CrecimientoGastosalud

# Modelos
sar_q <- spml(form_sin_ingr, data = datos_long, index = c("CVE", "year"),
              listw = listw_queen, model = "within", effect = "twoways",
              spatial.error = "none", lag = TRUE)

sem_q <- spml(form_sin_ingr, data = datos_long, index = c("CVE", "year"),
              listw = listw_queen, model = "within", effect = "twoways",
              spatial.error = "b", lag = FALSE)

# SAC (puede fallar, si falla, usa NULL)
sac_q <- tryCatch(
  spml(form_sin_ingr, data = datos_long, index = c("CVE", "year"),
       listw = listw_queen, model = "within", effect = "twoways",
       spatial.error = "b", lag = TRUE),
  error = function(e) NULL
)

# SDM (puede fallar)
sdm_q <- tryCatch(
  spml(form_sin_ingr, data = datos_long, index = c("CVE", "year"),
       listw = listw_queen, model = "within", effect = "twoways",
       spatial.error = "none", lag = TRUE, Durbin = TRUE),
  error = function(e) NULL
)


summary(sar_q)
summary(sem_q)
summary(sac_q)
summary(sdm_q)

impacts(sar_q, listw = listw_queen, time = 10)
impacts(sdm_q, listw = listw_queen, time = 10)





# ==================================================
# CÁLCULO DE LOGLIK Y PSEUDO R² PARA MODELOS CON MATRIZ REINA
# ==================================================

# Crear una lista con los modelos y sus nombres
modelos_lista <- list(
  Pooled = pooled_queen,
  SAR = sar_q,
  SEM = sem_q,
  SAC = sac_q,
  SDM = sdm_q
)

# Función para calcular pseudo R² (correlación al cuadrado entre observado y predicho)
pseudo_r2 <- function(modelo, datos) {
  if (is.null(modelo)) return(NA)
  # Extraer valores predichos (si existen)
  if (!is.null(modelo$fitted.values)) {
    pred <- as.numeric(modelo$fitted.values)
    obs <- datos$Inci
    return(cor(obs, pred, use = "complete.obs")^2)
  } else {
    return(NA)
  }
}

# Función para extraer logLik (con manejo de errores)
extraer_logLik <- function(modelo) {
  if (is.null(modelo)) return(NA)
  # Para objetos spml, el logLik está en modelo$logLik
  if (!is.null(modelo$logLik)) {
    return(as.numeric(modelo$logLik)[1])
  }
  # Para objetos plm (Pooled), calcular manualmente
  if (inherits(modelo, "plm")) {
    n <- length(residuals(modelo))
    rss <- sum(residuals(modelo)^2)
    return(-n/2 * (log(2*pi) + log(rss/n) + 1))
  }
  return(NA)
}

# Aplicar a cada modelo
resultados <- data.frame(
  Modelo = names(modelos_lista),
  logLik = sapply(modelos_lista, extraer_logLik),
  R2 = sapply(modelos_lista, function(m) pseudo_r2(m, datos_long))
)

# Redondear para presentación
resultados$logLik <- round(resultados$logLik, 3)
resultados$R2 <- round(resultados$R2, 3)

# Mostrar
print(resultados)








###################
#####   Modelos sin gasto en salud y cambio en VA
form_sin_gs <- Inci ~ gini + pobreza +  escolaridad + densidad + IDS 


# Modelos
sar_A <- spml(form_sin_gs, data = datos_long, index = c("CVE", "year"),
              listw = listw_queen, model = "within", effect = "twoways",
              spatial.error = "none", lag = TRUE)

sem_A <- spml(form_sin_gs, data = datos_long, index = c("CVE", "year"),
              listw = listw_queen, model = "within", effect = "twoways",
              spatial.error = "b", lag = FALSE)

# SAC (puede fallar, si falla, usa NULL)
sac_A <- tryCatch(
  spml(form_sin_gs, data = datos_long, index = c("CVE", "year"),
       listw = listw_queen, model = "within", effect = "twoways",
       spatial.error = "b", lag = TRUE),
  error = function(e) NULL
)

# SDM (puede fallar)
sdm_A <- tryCatch(
  spml(form_sin_gs, data = datos_long, index = c("CVE", "year"),
       listw = listw_queen, model = "within", effect = "twoways",
       spatial.error = "none", lag = TRUE, Durbin = TRUE),
  error = function(e) NULL
)


summary(sar_A)
summary(sem_A)
summary(sac_A)
summary(sdm_A)







####################
###Regresión solo con densidad y IDS

DIDS <- Inci ~   densidad    + escolaridad + pobreza 

# SAR 
sar_D <- spml(DIDS, data = datos_long, index = c("CVE", "year"),
               listw = listw_knn3, model = "within", effect = "twoways",
               spatial.error = "none", lag = TRUE)

summary(sar_D)

# SEM
sem_D <- spml(DIDS, data = datos_long, index = c("CVE", "year"),
               listw = listw_knn3, model = "within", effect = "twoways",
               spatial.error = "b", lag = FALSE)

summary(sem_D)

# SAC (SARAR)
sac_D <- spml(DIDS, data = datos_long, index = c("CVE", "year"),
               listw = listw_knn3, model = "within", effect = "twoways",
               spatial.error = "b", lag = TRUE)

summary(sac_D)

# SDM
sdm_D <- spml(DIDS, data = datos_long, index = c("CVE", "year"),
               listw = listw_knn3, model = "within", effect = "twoways",
               spatial.error = "none", lag = TRUE, Durbin = TRUE)

summary(sdm_D)

impacts(sar_D, listw = listw_knn3, time = 10)
impacts(sac_D, listw = listw_knn3, time = 10)
impacts(sdm_D, listw = listw_knn3, time = 10)



# Coeficientes espaciales
summary(sar_D)$Coef[1, ]  # lambda del SAR
summary(sem_D)$Coef[1, ]  # rho del SEM
# En SAC hay dos: lambda (lag) y rho (error)
summary(sac_D)$Coef[1:2, ]
# En SDM el lambda es el primero (igual que en SAR)
summary(sdm_D)$Coef[1, ]



#obtener impactos para mapear
fix_estados <- fixef(sar_D, effect = "individual")
df_fix <- data.frame(CVE = names(fix_estados), efecto_fijo = as.numeric(fix_estados))
write.csv(df_fix, "efectos_fijos_individuales.csv", row.names = FALSE)
residuos <- residuals(sar_D)
index_df <- attr(residuos, "index")
cve_vector <- index_df[[1]]
res_avg <- aggregate(residuos, by = list(cve_vector), FUN = mean)
names(res_avg) <- c("CVE", "resid_prom")
write.csv(res_avg, "residuos_promedioD.csv", row.names = FALSE)

