###########
##   ¡¡¡regresiones!!!!
#########


library(readr)
library(dplyr)
library(tidyr)


# Cargar la base (asumiendo que está en un archivo CSV)
base_original <- basedatos


# 1. Lista de grupos de variables
vars_groups <- list(
  Inci = "inci",
  ingr = "ingr",
  gini = "gini",
  pobreza = "pobreza",
  CambioVA = "cambio_va",
  CambioGastosalud = "cambio_gasto",
  escolaridad = "escolaridad",
  IDS = "ids",
  densidad = "densidad"
)

# 2. Función para pivotar cada grupo
pivot_group <- function(df, prefix, newname) {
  # Encontrar columnas que empiecen con el prefijo exacto
  cols <- grep(paste0("^", prefix), names(df), value = TRUE)
  
  df %>%
    select(CVE, Estado, all_of(cols)) %>%
    pivot_longer(
      cols = all_of(cols),
      names_to = "año",
      names_prefix = prefix,
      values_to = newname
    ) %>%
    mutate(año = as.numeric(paste0("20", año)))
}

# 3. INICIALIZAR lista_dfs ANTES del loop
lista_dfs <- list()

# 4. Aplicar a todos los grupos
for (nombre in names(vars_groups)) {
  cat("Procesando:", nombre, "\n")
  lista_dfs[[nombre]] <- pivot_group(base_original, nombre, vars_groups[[nombre]])
}

# 5. Verificar que todos los grupos se procesaron
names(lista_dfs)

# 6. Unir todos los data frames
base_long <- lista_dfs[[1]]  # empezar con Inci

for (i in 2:length(lista_dfs)) {
  base_long <- base_long %>%
    left_join(lista_dfs[[i]], by = c("CVE", "Estado", "año"))
}

# 7. Limpiar y renombrar
base_long <- base_long %>%
  rename(entidad = Estado, cve = CVE) %>%
  select(entidad, cve, año, everything()) %>%
  arrange(entidad, año)

# 8. Verificar
glimpse(base_long)
ncol(base_long)  # Debe ser: 3 (entidad, cve, año) + 9 variables = 12 columnas

# 9. Declarar panel
library(plm)
base_panel <- pdata.frame(base_long, index = c("entidad", "año"))
is.pbalanced(base_panel)
names(base_panel)


base_long %>%
  select(entidad, año, inci, ingr, gini, pobreza) %>%
  head(10)


library(dplyr)

# Identificar columnas que deben ser numéricas (todas excepto entidad y cve)
cols_numericas <- setdiff(names(base_panel), c("entidad", "cve"))

# Convertir a numeric
base_panel <- base_panel %>%
  mutate(across(all_of(cols_numericas), as.numeric))

# Verificar
glimpse(base_panel)



###Modelo pool
pooled <- plm(inci ~ ingr + gini + pobreza + cambio_va + cambio_gasto + 
                escolaridad + ids + densidad,
              data = base_panel,
              model = "pooling")
summary(pooled)

#efectos fijos
fe_ind <- plm(inci ~ ingr + gini + pobreza + cambio_va + cambio_gasto + 
                escolaridad + ids + densidad,
              data = base_panel,
              model = "within")
summary(fe_ind)

#efectos aleatorios
re <- plm(inci ~ ingr + gini + pobreza + cambio_va + cambio_gasto + 
            escolaridad + ids + densidad,
          data = base_panel,
          model = "random")
summary(re)

#diagnóstico
# Test de Hausman (FE vs RE)
phtest(fe_ind, re)

# Test de Breusch-Pagan (pooled vs RE)
plmtest(pooled, type = "bp")

# Test de autocorrelación de Wooldridge
pwartest(fe_ind)

# Test de dependencia transversal de Pesaran
pcdtest(fe_ind, test = "cd")




library(haven)
library(spdep)

# 1. Leer el archivo .dta
wrn <- read_dta("~/Downloads/wrn.dta")

# 2. Ver estructura
glimpse(wrn)

# 3. Convertir a matriz (asumiendo que es una matriz cuadrada de 32x32)
# Si wrn es un data frame con una columna de identificación y el resto la matriz
if ("CVE" %in% names(wrn)) {
  mat_pesos <- as.matrix(wrn[, -1])  # quitar columna de identificación
  rownames(mat_pesos) <- colnames(mat_pesos) <- wrn$CVE
} else {
  mat_pesos <- as.matrix(wrn)
}

# 4. Verificar dimensiones
dim(mat_pesos)  # debe ser 32 x 32

# 5. Convertir a objeto listw (necesario para modelos espaciales)
listw <- mat2listw(mat_pesos, style = "W")  # "W" para estandarización por filas

# 6. Verificar
listw

# Modelo SLX (solo derrames de X)
library(splm)

# Crear variables espacialmente rezagadas
base_panel <- base_panel %>%
  group_by(año) %>%
  mutate(
    W_ingr = lag.listw(listw, ingr),
    W_gini = lag.listw(listw, gini),
    W_pobreza = lag.listw(listw, pobreza),
    W_escolaridad = lag.listw(listw, escolaridad),
    W_ids = lag.listw(listw, ids),
    W_densidad = lag.listw(listw, densidad)
  ) %>%
  ungroup()

# SLX con efectos fijos
slx_fe <- plm(inci ~ ingr + gini + pobreza + escolaridad + ids + densidad +
                W_ingr + W_gini + W_pobreza + W_escolaridad + W_ids + W_densidad,
              data = base_panel,
              index = c("entidad", "año"),
              model = "within")
summary(slx_fe)




# Ver si hay combinaciones entidad-año duplicadas
duplicados <- base_panel %>%
  group_by(entidad, año) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1)

base_panel_unico <- base_panel %>%
  group_by(entidad, año) %>%
  slice(1) %>%
  ungroup()

# Verificar que ya no hay duplicados
base_panel_unico %>%
  group_by(entidad, año) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1)

print(duplicados)
library(plm)
base_panel_clean <- pdata.frame(base_panel_unico, index = c("entidad", "año"))
is.pbalanced(base_panel_clean)  # Debe ser TRUE


library(plm)
library(splm)

# 1. Asegurar que el panel está ordenado por entidad y año
base_panel_ordenado <- base_panel_clean %>%
  arrange(entidad, año) %>%
  pdata.frame(index = c("entidad", "año"))

# 2. Verificar que los nombres en listw coinciden con las entidades del panel
# Las entidades en el panel son factores, necesitamos sus niveles
entidades_panel <- levels(base_panel_ordenado$entidad)
cat("Entidades en panel:", paste(entidades_panel, collapse = ", "), "\n")

# Nombres en la matriz de pesos
nombres_listw <- attr(listw, "region.id")
cat("Nombres en listw:", paste(nombres_listw, collapse = ", "), "\n")

# 3. Si no coinciden, necesitas renombrar las filas de la matriz
# Por ejemplo, si listw usa códigos numéricos y el panel usa nombres
# Puedes crear un vector de correspondencia




#Modelo SAR (rezago espacial de Y)
# SAR con efectos fijos

sar_fe <- spml(inci ~ ingr + gini + pobreza + escolaridad + ids + densidad,
               data = base_panel_ordenado,
               listw = listw,
               model = "within",
               effect = "individual",
               lag = TRUE,
               spatial.error = "none")
summary(sar_fe)



# SEM
sem_fe <- spml(inci ~ ingr + gini + pobreza + escolaridad + ids + densidad,
               data = base_panel_clean,
               listw = listw,
               model = "within",
               effect = "individual",
               lag = FALSE,
               spatial.error = "b")
summary(sem_fe)

# SAC
sac_fe <- spml(inci ~ ingr + gini + pobreza + escolaridad + ids + densidad,
               data = base_panel_clean,
               listw = listw,
               model = "within",
               effect = "individual",
               lag = TRUE,
               spatial.error = "b")
summary(sac_fe)

library(splm)

# SDM con efectos fijos
sdm_fe <- spml(inci ~ ingr + gini + pobreza + escolaridad + ids + densidad,
               data = base_panel_ordenado,
               listw = listw,
               model = "within",
               effect = "individual",
               lag = TRUE,        # incluye rezago de Y
               lagX = TRUE,        # incluye rezagos de todas las X
               spatial.error = "none")
summary(sdm_fe)


#comparar modelos
# Lista de modelos
modelos <- list(
  SLX = slx_fe,
  SAR = sar_fe,
  SEM = sem_fe,
  SAC = sac_fe,
  SDM = sdm_fe
)

# Crear lista sin SAC
modelos_sin_sac <- modelos[names(modelos) != "SAC"]

# Aplicar la función de comparación
comparacion <- do.call(rbind, 
                       lapply(names(modelos_sin_sac), function(n) {
                         extraer_info_modelos(modelos_sin_sac[[n]], n)
                       }))

print(comparacion)

# Ordenar por AIC
comparacion %>% arrange(AIC)


sac_fe2 <- spml(inci ~ ingr + gini + pobreza + escolaridad + ids + densidad,
                data = base_panel_clean,
                listw = listw,
                model = "within",
                effect = "individual",
                lag = TRUE,
                spatial.error = "b",
                control = list(threshold = 1e-4, maxit = 1000))
summary(sac_fe2)


#parece que sar es el mejor modelo, se sacan efectos

# Ver dimensiones de W
dim(W)  # Debe ser 32x32

# Ver valor de rho
print(rho)  # Debe estar entre -1 y 1 (típicamente < 1 en valor absoluto)

# Ver si W tiene la estructura correcta
W[1:5, 1:5]  # primeras filas y columnas

# Ver la estructura del objeto sar_fe
str(sar_fe)

# Buscar dónde está rho
# Probablemente en sar_fe$coefficients o sar_fe$rho
# Intentemos:
rho <- sar_fe$rho
if (is.null(rho)) {
  # Si no está, buscar en los coeficientes
  coef_names <- names(coef(sar_fe))
  rho_name <- grep("rho", coef_names, value = TRUE)
  if (length(rho_name) > 0) {
    rho <- coef(sar_fe)[rho_name]
  } else {
    # Último recurso: buscar en la lista completa
    rho <- sar_fe$arcoef
  }
}

print(rho)
summary(sar_fe)


#SAR otra vez
# Este es tu SAR según la nomenclatura estándar
sar_std <- spml(inci ~ ingr + gini + pobreza + escolaridad + ids + densidad,
                data = base_panel_ordenado,
                listw = listw,
                model = "within",
                effect = "individual",
                lag = TRUE,              # Esto es rho en estándar
                spatial.error = "none")
summary(sar_std)

# Extraer rho
# Ver la estructura completa del modelo
str(sar_std)

# Buscar el coeficiente espacial (debe llamarse "rho" o "lambda")
# En tu output aparece como "lambda", pero en el objeto puede llamarse diferente
names(sar_std)

# Intentar extraer de diferentes maneras
rho <- sar_std$coefficients[length(sar_std$coefficients)]  # último coeficiente
if (is.null(rho)) rho <- sar_std$rho
if (is.null(rho)) rho <- sar_std$arcoef
if (is.null(rho)) rho <- sar_std$errcomp

print(rho)

# Opción 1: Buscar en los coeficientes por nombre
coef_names <- names(coef(sar_std))
print(coef_names)

# El coeficiente espacial suele llamarse "lambda" o "rho" en los nombres
lambda <- coef(sar_std)["lambda"]
if (is.na(lambda)) lambda <- coef(sar_std)["rho"]

# Opción 2: Extraer del summary manualmente
lambda <- 0.577870  # del output

# Opción 3: Explorar la estructura del objeto
names(sar_std)
sar_std$arcoef  # podría estar aquí
sar_std$errcomp  # o aquí


# Asignar lambda (coeficiente espacial)
lambda_espacial <- 0.577870

# Coeficientes de las X (sin el espacial)
beta <- coef(sar_std)[!names(coef(sar_std)) %in% c("lambda", "rho")]

# Matriz de pesos
W <- as.matrix(listw2mat(listw))
I <- diag(nrow(W))

# Calcular matriz de multiplicadores
S <- solve(I - lambda_espacial * W)

# Calcular efectos
efectos <- data.frame(
  variable = names(beta),
  directo = NA,
  indirecto = NA,
  total = NA
)

for (i in 1:length(beta)) {
  M <- S * beta[i]
  efectos$directo[i] <- mean(diag(M))
  efectos$indirecto[i] <- mean(rowSums(M) - diag(M))
  efectos$total[i] <- efectos$directo[i] + efectos$indirecto[i]
}

print(efectos)




library(sf)        # para manejar datos espaciales
library(ggplot2)   # para graficar
library(dplyr)

# 1. Extraer los efectos indirectos totales por estado
# Usamos la matriz de multiplicadores S = (I - ρW)^(-1)
# y la multiplicamos por los coeficientes de las variables

# Coeficientes del modelo (solo variables significativas)
beta_ids <- coef(sar_std)["ids"]

# Matriz de multiplicadores
W <- as.matrix(listw2mat(listw))
I <- diag(nrow(W))
rho <- 0.57787  # coeficiente espacial

S <- solve(I - rho * W)

# Efectos totales de ids por estado (incluye directo + indirecto recibido)
efectos_ids <- S %*% beta_ids

# Crear data frame con resultados
spillovers <- data.frame(
  cve = rownames(W),
  efecto_total = as.vector(efectos_ids)
)

# 2. Unir con geometrías de los estados
# Necesitas un shapefile de México a nivel estatal
# Si no lo tienes, puedes descargarlo:
# shp <- st_read("https://www.inegi.org.mx/contenidos/productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marcogeo/889463807469/889463807469_s.zip")

# Suponiendo que tienes 'estados_mexico' con geometrías y columna CVE_ENT
mapa_spillovers <- estados_mexico %>%
  left_join(spillovers, by = c("CVE_ENT" = "cve"))

# 3. Graficar
ggplot(mapa_spillovers) +
  geom_sf(aes(fill = efecto_total)) +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red",
    midpoint = 0,
    name = "Efecto total\nde IDS"
  ) +
  labs(
    title = "Efectos totales de IDS sobre obesidad (modelo SAR)",
    subtitle = "Incluye efectos directos e indirectos"
  ) +
  theme_minimal()




# Ver dimensiones de S
dim(S)  # Debe ser 32x32

# Ver longitud de beta_ids
length(beta_ids)  # Debe ser 1 (es un escalar)

# El problema es que estás multiplicando una matriz 32x32 por un escalar
# Debería ser: efectos_ids <- S %*% (beta_ids * ids_anio)
# Pero beta_ids es escalar, entonces:


# Ver años disponibles
unique(base_panel_clean$año)

# Ver estructura
glimpse(base_panel_clean)

# Buscar la columna ids
names(base_panel_clean)


# Crear vector de mapeo
años_reales <- 2014:2023
names(años_reales) <- 1:10

# Ver mapeo
print(años_reales)

# Extraer IDS para un año específico (ej. el índice 1 = 2014)
anio_indice <- 1
anio_real <- años_reales[anio_indice]

cat("Índice:", anio_indice, "-> Año real:", anio_real, "\n")

# Extraer IDS para el índice 1
ids_anio <- base_panel_clean %>%
  filter(año == anio_indice) %>%
  arrange(entidad) %>%
  pull(ids)

# Verificar
length(ids_anio)  # Debe ser 32
head(ids_anio)


# Parámetros del modelo
beta_ids <- 0.078542  # coeficiente de IDS
rho <- 0.57787        # coeficiente espacial

# Matriz de pesos
W <- as.matrix(listw2mat(listw))
I <- diag(nrow(W))

# Matriz de multiplicadores
S <- solve(I - rho * W)

# Calcular efectos totales para el año índice 1 (2014)
efectos_2014 <- S %*% (beta_ids * ids_anio)

# Convertir a vector
efectos_vector <- as.vector(efectos_2014)

# Crear data frame con resultados
spillovers_2014 <- data.frame(
  cve = rownames(W),
  efecto_total = efectos_vector,
  año = 2014
)

head(spillovers_2014)



# Lista para guardar resultados
lista_spillovers <- list()

for (i in 1:10) {
  
  año_real <- años_reales[i]
  cat("Procesando año:", año_real, "(índice", i, ")\n")
  
  # Extraer IDS para este índice
  ids_i <- base_panel_clean %>%
    filter(año == i) %>%
    arrange(entidad) %>%
    pull(ids)
  
  # Calcular efectos
  efectos_i <- S %*% (beta_ids * ids_i)
  
  # Guardar
  lista_spillovers[[i]] <- data.frame(
    cve = rownames(W),
    efecto_total = as.vector(efectos_i),
    año = año_real
  )
}

# Unir todos
spillovers_todos <- do.call(rbind, lista_spillovers)
head(spillovers_todos)



# Ver estructura
glimpse(spillovers_todos)

# Ver primeros registros
head(spillovers_todos)

# Ver resumen por año
spillovers_todos %>%
  group_by(año) %>%
  summarise(
    min = min(efecto_total),
    mean = mean(efecto_total),
    max = max(efecto_total)
  )




# Ver si la lista tiene elementos
length(lista_spillovers)

# Si es 10, entonces sí se creó
if (length(lista_spillovers) == 10) {
  # Unir manualmente
  spillovers_todos <- do.call(rbind, lista_spillovers)
  cat("Objeto creado con", nrow(spillovers_todos), "filas\n")
} else {
  cat("La lista tiene", length(lista_spillovers), "elementos, no 10\n")
}

# Ver primeras filas
head(spillovers_todos)

# Ver estructura
glimpse(spillovers_todos)

# Ver años disponibles
unique(spillovers_todos$año)

# Resumen por año
library(dplyr)
spillovers_todos %>%
  group_by(año) %>%
  summarise(
    min = min(efecto_total),
    media = mean(efecto_total),
    max = max(efecto_total)
  )


library(sf)
library(ggplot2)
library(viridis)

# Cargar shapefile
estados_mexico <- st_read("/Users/dmares/Documents/Doctorado unam/Codigos/areas_geoestadisticas_estatales.shp")

# Asegurar formato de clave
estados_mexico <- estados_mexico %>%
  mutate(CVE_ENT = sprintf("%02d", as.numeric(CVE_ENT)))

# Crear mapas para cada año
años_unicos <- unique(spillovers_todos$año)

for (anio in años_unicos) {
  
  cat("Creando mapa para", anio, "...\n")
  
  # Filtrar datos del año
  spillovers_anio <- spillovers_todos %>%
    filter(año == anio)
  
  # Unir con geometrías
  mapa_anio <- estados_mexico %>%
    left_join(spillovers_anio, by = c("CVE_ENT" = "cve"))
  
  # Crear gráfico
  p <- ggplot(mapa_anio) +
    geom_sf(aes(fill = efecto_total), color = "gray40", size = 0.2) +
    scale_fill_viridis(
      option = "plasma",
      name = "Efecto total\nde IDS",
      direction = -1
    ) +
    labs(
      title = paste("Efectos totales de IDS sobre obesidad -", anio),
      subtitle = "Modelo SAR con efectos fijos"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.key.width = unit(2, "cm"),
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank()
    )
  
  # Guardar
  ggsave(paste0("mapa_spillovers_", anio, ".png"), 
         p, width = 10, height = 8, dpi = 300)
  
  # Mostrar (opcional)
  print(p)
}

cat("✅ Mapas generados para todos los años\n")





# 1. Verificar que el shapefile se cargó correctamente
estados_mexico <- st_read("/Users/dmares/Documents/Doctorado unam/Codigos/areas_geoestadisticas_estatales.shp")
glimpse(estados_mexico)

# 2. Ver claves en shapefile
unique(estados_mexico$CVE_ENT)

# 3. Ver claves en spillovers
unique(spillovers_todos$cve)



# Convertir claves en spillovers a formato de dos dígitos
spillovers_todos <- spillovers_todos %>%
  mutate(cve = sprintf("%02d", as.numeric(cve)))

# Verificar
unique(spillovers_todos$cve)


# 4. Probar un año específico manualmente
anio <- 2014
spillovers_anio <- spillovers_todos %>% filter(año == anio)


# 5. Unir manualmente
mapa_prueba <- estados_mexico %>%
  left_join(spillovers_anio, by = c("CVE_ENT" = "cve"))

# 6. Verificar si hay NA
summary(mapa_prueba$efecto_total)

# 7. Si hay muchos NA, puede ser que las claves no coincidan
#    Intentar convertir formato
estados_mexico <- estados_mexico %>%
  mutate(CVE_ENT = as.character(CVE_ENT),
         CVE_ENT = ifelse(nchar(CVE_ENT) == 1, paste0("0", CVE_ENT), CVE_ENT))

ggplot(mapa_prueba) +
  geom_sf(aes(fill = efecto_total)) +
  scale_fill_viridis_c() +
  labs(title = paste("Efectos totales IDS -", anio))

#mapa resumen, promedio de años
# Promedio por estado de todos los años
spillovers_prom <- spillovers_todos %>%
  group_by(cve) %>%
  summarise(efecto_prom = mean(efecto_total))

mapa_prom <- estados_mexico %>%
  left_join(spillovers_prom, by = c("CVE_ENT" = "cve"))

ggplot(mapa_prom) +
  geom_sf(aes(fill = efecto_prom), color = "gray40") +
  scale_fill_viridis(option = "plasma", name = "Efecto promedio") +
  labs(title = "Efectos indirectos promedio (spillovers)  sobre obesidad (2014-2023)") +
  theme_minimal()
