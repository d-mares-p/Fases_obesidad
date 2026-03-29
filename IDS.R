### Crear el IDS
###################

library(tidyr)
library(dplyr)
library(readr)

# 1. Cargar la base desde Downloads
mortandad <- Mortalidad_09  # Asegúrate que este objeto existe

# 2. Ver estructura
glimpse(mortandad)
names(mortandad)

# --- NUEVO: Convertir columnas de ingreso a numérico ---
# Identificar columnas que empiezan con "ingr"
columnas_ingreso <- grep("^ingr", names(mortandad), value = TRUE)

# Aplicar parse_number a cada una
for (col in columnas_ingreso) {
  mortandad[[col]] <- parse_number(as.character(mortandad[[col]]))
}

# Verificar que ahora sean numéricas
sapply(mortandad[, columnas_ingreso], class)
# -------------------------------------------------------

# 3. Transformar a formato largo
datos_long <- mortandad %>%
  pivot_longer(
    cols = -`Entidad federativa`,  # nombre exacto de la columna
    names_to = "variable",
    values_to = "valor"
  ) %>%
  # Separar la variable en tipo (EV, Inci, ingr) y año
  mutate(
    tipo = case_when(
      grepl("^EV", variable) ~ "ev",
      grepl("^Inci", variable) ~ "incidencia",
      grepl("^ingr", variable) ~ "ingreso",
      TRUE ~ NA_character_
    ),
    año = as.numeric(paste0("20", substr(variable, nchar(variable)-1, nchar(variable))))
  ) %>%
  filter(!is.na(tipo)) %>%  # eliminar filas no reconocidas
  select(-variable) %>%
  # Pasar a formato ancho por tipo
  pivot_wider(
    id_cols = c(`Entidad federativa`, año),
    names_from = tipo,
    values_from = valor
  ) %>%
  rename(
    entidad = `Entidad federativa`
  ) %>%
  # Ordenar
  arrange(entidad, año)

# 4. Verificar
head(datos_long)
summary(datos_long)

# 5. Calcular ICDS estatal (opción 1)
icds_estatal <- datos_long %>%
  group_by(año) %>%
  mutate(
    incidencia_nac = mean(incidencia, na.rm = TRUE),
    ev_nac = mean(ev, na.rm = TRUE),
    icds_estatal = ((incidencia - incidencia_nac) / incidencia_nac) * 
      ((ev - ev_nac) / ev_nac) * 100
  ) %>%
  ungroup() %>%
  select(año, entidad, icds_estatal)

# 6. Unir a la base principal
base_final <- datos_long %>%
  left_join(icds_estatal, by = c("año", "entidad"))

# 7. Guardar en Downloads
write_csv(base_final, 
          file = "~/Downloads/base_modelos_completa_2014_2023.csv")

saveRDS(base_final, 
        file = "~/Downloads/base_modelos_completa_2014_2023.rds")

# 8. Ver primeras filas
head(base_final, 10)

# 9. Mensaje de confirmación
cat("\n✅ Base procesada y guardada en:\n")
cat("   ~/Downloads/base_modelos_completa_2014_2023.csv\n")
cat("   ~/Downloads/base_modelos_completa_2014_2023.rds\n")
cat("\n📊 Dimensiones:", nrow(base_final), "filas x", ncol(base_final), "columnas\n")





library(readxl)
library(tidyverse)

# Leer el archivo saltando las primeras 7 filas
datos_raw <- read_excel("Downloads/Mortalidad_09.xlsx", 
                     sheet = "IDS",   
                     col_names = FALSE)

# Ver las primeras filas
names(datos_raw)

datos <- datos_raw %>%
  select(estado = 1,                # primera columna con nombre del estado
         EV2014:EV2023,              # esperanza de vida 2014-2023
         Inci14:Inci23)              # incidencia 2014-2023











# Cargar librerías necesarias
library(tidyverse)

# 1. Crear el dataframe con los datos que proporcionaste
# (Si los tienes en un archivo, puedes leerlo con read_csv)
datos_raw <- data.frame(
  stringsAsFactors = FALSE,
  check.names = FALSE,
  `Entidad federativa` = c("Aguascalientes", "Baja California", "Baja California Sur",
                           "Campeche", "Chiapas", "Chihuahua", "Ciudad de México",
                           "Coahuila de Zaragoza", "Colima", "Durango", "Guanajuato",
                           "Guerrero", "Hidalgo", "Jalisco", "México",
                           "Michoacán de Ocampo", "Morelos", "Nayarit", "Nuevo León",
                           "Oaxaca", "Puebla", "Querétaro", "Quintana Roo",
                           "San Luis Potosí", "Sinaloa", "Sonora", "Tabasco",
                           "Tamaulipas", "Tlaxcala",
                           "Veracruz de Ignacio de la Llave", "Yucatán", "Zacatecas"),
  EV2014 = c(76.5,75.9,76.3,75.1,73.3,76.1,77.1,76.9,75.8,75.3,74.8,73.3,74.5,76,74.8,74,74.6,75.5,77.9,73.6,73.9,75.7,75.6,74.9,75.5,76.5,73.9,75.9,73.9,74.1,75,74.5),
  EV2015 = c(76.5,75.9,76.3,75.1,73.2,76.1,77.1,76.9,75.8,75.3,74.7,73.2,74.4,76,74.8,73.9,74.5,75.4,77.9,73.5,73.9,75.7,75.5,74.9,75.4,76.5,73.8,75.9,73.8,74,74.9,74.5),
  EV2016 = c(76.3,76.2,76.2,74.5,72.7,76,76.7,76.7,75.5,75.1,74.5,73,73.6,75.5,74.8,73.4,74.3,75.1,77.8,72.8,73.6,75.6,75.6,74.7,75.6,76.2,73.3,75.7,73.7,73.6,74.5,74.1),
  EV2017 = c(76.3,76.3,76.4,74.5,72.5,76,76.8,76.8,75.5,75.1,74.4,72.8,73.5,75.5,74.8,73.3,74.2,75.1,78,72.7,73.5,75.6,75.7,74.7,75.7,76.3,73.2,75.7,73.5,73.5,74.4,74.1),
  EV2018 = c(76.6,76.2,76.8,74.1,72.6,76.3,76.6,76.8,75.8,75.2,74.6,72.8,73.6,75.7,74.8,73.8,74,75.2,77.4,73,73.6,76,75.6,74.9,75.8,76.4,73.4,75.5,73.9,73.4,74.6,74),
  EV2019 = c(76.5,76.1,76.7,74.1,72.6,76.2,76.5,76.7,75.7,75.1,74.6,72.8,73.5,75.6,74.7,73.8,74,75.2,77.3,73,73.6,75.9,75.5,74.9,75.7,76.3,73.4,75.5,73.8,73.4,74.6,74),
  EV2020 = c(70.1,68.9,72.5,68,67.9,69.5,69.2,69.9,72.1,69.8,68.3,69.1,68.2,71.2,66.7,69.4,68.4,71.6,71.6,68.8,67.3,70.4,68.6,68.4,70.2,69.9,67,70.5,65.2,69.3,70.7,67.7),
  EV2021 = c(71.4,71.2,69.6,68.2,67.7,72.4,70.3,72.8,69.7,71,67.6,69.4,67.1,69.4,66.8,66.5,66.1,70.1,71.1,66.9,65.3,67.8,68.3,68,71.1,71.1,69.2,72.5,65.2,68.9,69.4,67),
  EV2022 = c(76.5,76.2,76.8,74.1,72.6,76.3,76.5,76.8,75.8,75.2,74.5,72.7,73.5,75.7,74.7,73.7,73.9,75.3,77.4,72.9,73.5,75.9,75.6,74.9,75.8,76.4,73.4,75.5,73.8,73.3,74.6,74),
  EV2023 = c(76.7,76.4,77,74.3,72.8,76.4,76.6,76.9,76,75.3,74.7,72.9,73.7,75.8,74.9,73.9,74.1,75.4,77.6,73.1,73.7,76.1,75.8,75.1,75.9,76.6,73.6,75.7,74,73.5,74.8,74.2),
  Inci14 = c(265.95,671.93,858.66,311.7,207.69,558.51,328.99,555.54,733.49,143.35,10.23,122.11,490.75,201.34,281.3,262.63,452.09,540.54,182.18,259.42,96.47,137.66,805.1,220.58,458.24,840.01,263.88,176.09,121.53,184.83,369.01,479.68),
  Inci15 = c(428.22,613.92,753.87,570.34,224.03,376.19,572.56,243.46,682.56,467.49,56.88,276.84,503.2,453.86,322.47,323.46,425.86,365.75,442.62,279.29,272.45,606.39,683.12,493.88,591.94,754.9,323.92,542.28,424.31,217.9,356.25,469.9),
  Inci16 = c(816.33,665.94,926.08,746.92,235.84,490.59,720.39,259.5,679.47,429.69,149.98,321.16,567.8,506.98,368.7,331.1,453.93,752.53,502.29,266.71,283.01,530.03,724.67,620.38,658.78,833.79,320.16,595.26,412.34,286.04,503.43,496.28),
  Inci17 = c(785.2,832.42,873.64,829.69,240.11,795.92,935.84,507.57,807.17,436.78,259.83,423.37,559.48,650.01,398.68,454.54,443.1,864.75,595.85,307.03,344.39,546.06,749.34,819.53,857.64,807.09,358.53,861.01,466.62,422.03,982.31,468.63),
  Inci18 = c(628.39,940.29,709.85,651.8,233.02,862.29,819.9,287.61,875.01,444.01,300.82,376.75,564.52,614.71,421.68,531.81,493.67,870.99,703.01,307.49,307.31,562.89,903.4,800.42,864.71,874.9,325.05,684.67,586.75,462.77,996.5,516.99),
  Inci19 = c(503.6,847.88,731.49,567.66,179.57,892.43,840.97,528.4,885.56,391.7,280.98,377.36,542.82,552.1,401.03,455.11,447.65,889.55,706.33,313,303.66,520.97,748.45,750.64,916.38,803.49,398.17,720.15,322.03,404.21,831.52,523.33),
  Inci20 = c(253.58,447.36,357.27,262.04,114.93,465.21,419.14,260.6,474.81,236.7,181.5,229.42,345.32,326.98,216.3,278.84,253.61,366.3,371.58,168.28,202.11,319.22,629.1,501.34,417.46,434.77,232.05,385.99,266.74,217.26,344.47,314.93),
  Inci21 = c(341.39,530.03,491.93,420.64,159.66,615.06,495.91,430.14,660.27,299.42,223.59,263.29,437.12,507.44,298.99,262.98,328.52,530.8,380.26,200.28,280.93,355.24,796.81,493.33,465.24,514.38,305.66,504.24,357.92,349.15,476.02,388.28),
  Inci22 = c(662.97,873.22,1227.47,678.46,251.46,756.57,860.97,758.35,1035.7,626.42,483.18,535.6,528.84,897.03,473.29,446.32,565.45,848.27,789.43,364.05,366.97,615.11,1142.3,877.9,630.23,764.21,545.18,726.45,671.42,665.09,745.89,374.46),
  Inci23 = c(599.92,838.05,856.48,631.5,256.84,772.59,879.2,632.21,1062.29,661.41,405.82,519.98,357,818.03,531.56,316.3,689.69,831.8,705.45,350.16,360.99,548.2,883.16,885.03,630.81,847.69,488.84,704.78,577.16,514.59,728.18,341.67)
)

