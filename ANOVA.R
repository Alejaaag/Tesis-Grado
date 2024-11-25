
# Cargar los paquetes necesarios
library(readxl)
library(dplyr)

# Leer el archivo Excel 
#Ruta al archivo
file_path <- "Documents/TESIS DE GRADO/CAMBIO.xlsx"  
datos <- read_excel(file_path, sheet = "Hoja1")

# Lista de variables categóricas a incluir en el modelo
variables_categoricas <- c(
  "ESTU_METODO_PRGM",
  "ESTU_PAGOMATRICULABECA",
  "ESTU_PAGOMATRICULACREDITO",
  "ESTU_HORASSEMANATRABAJA",
  "ESTU_NACIONALIDAD",
  "ESTU_GENERO",
  "ESTU_PAGOMATRICULAPADRES",
  "ESTU_PAGOMATRICULAPROPIO",
  "FAMI_EDUCACIONPADRE",
  "FAMI_ESTRATOVIVIENDA",
  "FAMI_TIENECOMPUTADOR",
  "FAMI_TIENEINTERNET",
  "FAMI_EDUCACIONMADRE",
  "INST_ORIGEN"
)
#ANOVA
# Crear una lista para almacenar los resultados del ANOVA por universidad
resultados_anova <- list()

# Obtener las universidades únicas
universidades <- unique(datos$INST_NOMBRE_INSTITUCION)

# Realizar ANOVA para cada universidad
for (uni in universidades) {
  # Filtrar los datos para la universidad actual
  datos_uni <- datos %>% filter(INST_NOMBRE_INSTITUCION == uni)
  
  # Convertir variables categóricas a factores
  datos_uni <- datos_uni %>%
    mutate(across(all_of(variables_categoricas), as.factor))
  
  # Verificar variables con al menos dos niveles
  variables_validas <- variables_categoricas[sapply(variables_categoricas, function(var) {
    length(unique(datos_uni[[var]])) > 1
  })]
  
  # Filtrar datos sin NA y solo con variables válidas
  datos_filtrados <- datos_uni %>%
    select(c(PROMEDIO, all_of(variables_validas))) %>%
    na.omit()
  
  # Crear el modelo ANOVA multifactorial
  if (nrow(datos_filtrados) > 0 && length(variables_validas) > 0) {
    formula <- as.formula(paste("PROMEDIO ~", paste(variables_validas, collapse = " + ")))
    modelo <- aov(formula, data = datos_filtrados)
    
    # Guardar el resumen del modelo
    resultados_anova[[uni]] <- summary(modelo)
    
    # Imprimir los resultados para la universidad actual
    cat("\n\n--- Resultados ANOVA para la universidad:", uni, "---\n")
    print(summary(modelo))
  } else {
    # Si no hay suficientes datos o variables válidas, agregar un mensaje
    resultados_anova[[uni]] <- "No hay suficientes datos o variables categóricas válidas para realizar el análisis ANOVA."
    cat("\n\n--- No hay suficientes datos para la universidad:", uni, "---\n")
  }
}
# Convertir las variables categóricas a factores
datos <- datos %>%
  mutate(across(all_of(variables_categoricas), as.factor))

# Verificar variables categóricas con al menos dos niveles
variables_validas <- variables_categoricas[sapply(variables_categoricas, function(var) {
  length(unique(datos[[var]])) > 1
})]

# Filtrar datos sin NA y solo con variables válidas
datos_filtrados <- datos %>%
  select(c(PROMEDIO, all_of(variables_validas))) %>%
  na.omit()

# Prueba de Tukey 
if (nrow(datos_filtrados) > 0 && length(variables_validas) > 0) {
  # Crear la fórmula del modelo
  formula <- as.formula(paste("PROMEDIO ~", paste(variables_validas, collapse = " + ")))
  
  # Modelo necesario para la prueba de Tukey
  modelo <- aov(formula, data = datos_filtrados)
  
  # Prueba de Tukey
  prueba_tukey <- TukeyHSD(modelo)
  
  # Resultados de la prueba de Tukey
  cat("\n--- Resultados Prueba de Tukey General ---\n")
  print(prueba_tukey)
} else {
  cat("\n--- No hay suficientes datos o variables categóricas válidas para realizar la prueba de Tukey. ---\n")
}

