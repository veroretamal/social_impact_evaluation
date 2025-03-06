
# Instalación de paquetes y liberías
# Verificar e instalar paquetes si no están instalados
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("lubridate")) install.packages("lubridate")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("corrplot")) install.packages("corrplot")
if (!require("dplyr")) install.packages("dplyr")
if (!require("summarytools")) install.packages("summarytools")
if (!require("psych")) install.packages("psych")
if (!require("wordcloud")) install.packages("wordcloud")
if (!require("pscl")) install.packages("pscl")
if (!require("pROC")) install.packages("pROC")
if (!require("factoextra")) install.packages("factoextra")
if (!require("plotly")) install.packages("plotly")
library(plotly)
library(factoextra)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(readr)
library(dplyr)
library(summarytools)
library(car)
library(tidyr)
library(readxl)
library(broom)
library(psych)
library(wordcloud)
library(RColorBrewer)
library(pscl)
library(pROC)
library(corrplot)
library(reshape2)


# Cargar archivo XLS
df <- read_excel("EIS/eis.xlsx")


## Revisar data frame
colnames(df)
glimpse(df)

## Resumen estadistico general
dfSummary(df)

## Verificar valores nulos
colSums(is.na(df))

## Eliminación de valores nulos si es necesario
df <- df %>% drop_na()  # Eliminar filas con valores NA 

# Resumen estadístico
descriptivos <- df_clean %>%
  select(Población_total_region, Pob_indígena, Porcentaje_rural, Tasa_desempleo) %>%
  psych::describe()
print(descriptivos)

# Distribución de la población total por región
ggplot(df, aes(x = Región, y = Población_total_region)) +
  geom_boxplot(aes(fill = Región), alpha = 0.7, color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, color = "red") +  # Puntos para la media
  theme_minimal(base_size = 14) +
  labs(title = "Distribución de la Población Total por Región", y = "Población Total", x = "Región") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
  scale_fill_viridis_d()

# Boxplot de Tasa de Desempleo por Región
ggplot(df, aes(x = Región, y = Tasa_desempleo, fill = Región)) +
  geom_boxplot(alpha = 0.7, color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, color = "red") +  # Puntos para la media
  theme_minimal(base_size = 14) +
  labs(title = "Distribución de la Tasa de Desempleo por Región", x = "Región", y = "Tasa de Desempleo (%)") +
  theme(legend.position = "none") +
  scale_fill_viridis_d()

# Gráfico de barras de la población indígena por región
ggplot(df, aes(x = Región, y = Pob_indígena, fill = Región)) +
  geom_bar(stat = "identity", alpha = 0.7, color = "black") +
  geom_text(aes(label = Pob_indígena), vjust = -0.5, size = 4, color = "black") +  # Etiquetas en las barras
  theme_minimal(base_size = 14) +
  labs(title = "Población Indígena por Región", x = "Región", y = "Población Indígena") +
  theme(legend.position = "none") +
  scale_fill_viridis_d()

# Gráfico de dispersión entre Tasa de Desempleo y Porcentaje Rural
ggplot(df, aes(x = Porcentaje_rural, y = Tasa_desempleo, color = Región)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +  # Línea de tendencia con intervalo de confianza
  theme_minimal(base_size = 14) +
  labs(title = "Relación entre Tasa de Desempleo y Porcentaje Rural", x = "Porcentaje Rural (%)", y = "Tasa de Desempleo (%)") +
  scale_color_viridis_d()

# Resumen general Opinión Proyectos por Región
ggplot(df, aes(x = fct_infreq(Opinion_proyecto), fill = Región)) +  # Agrupar las opiniones
  geom_bar(position = "dodge") +  # Mejor visualización con barras separadas
  theme_minimal() +
  labs(title = "Opinión sobre Proyectos por Región", x = "Opinión del Proyecto", y = "Cantidad") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Análisis de Correlación entre Variables de Línea Base y Opinión del Proyecto
df$Opinion_proyecto_num <- ifelse(df$Opinion_proyecto == "Positiva", 1, 
                                  ifelse(df$Opinion_proyecto == "Neutra", 0, -1))

# Calcular la matriz de correlación
correlacionesf <- cor(df[, c("Población_total_region", "Pob_indígena", "Porcentaje_rural", 
                            "Tasa_desempleo", "Confianza_instituciones", "Num_actores_clave", 
                            "Participación_ciudadana", "Conocimiento_proyecto", 
                            "Confianza_responsables", "Opinion_proyecto_num")])

print(correlacionesf)

# Visualizar la matriz de correlación
corrplot(correlaciones, method = "circle", type = "upper", tl.col = "black", tl.srt = 45, 
         addCoef.col = "white", number.cex = 0.7)  # Agregar coeficientes de correlación

# Análisis de Regresión Lineal para Cuantificar la Influencia
modelo <- lm(Opinion_proyecto_num ~ Pob_indígena + Porcentaje_rural + Tasa_desempleo, data = df)
summary(modelo)

# Visualización de relaciones
# Gráfico de dispersión entre Pob_indígena y Opinión del Proyecto
ggplot(df, aes(x = Pob_indígena, y = Opinion_proyecto_num, color = Tasa_desempleo)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  theme_minimal() +
  labs(title = "Relación entre Población Indígena y Opinión del Proyecto", x = "Población Indígena", 
       y = "Opinión del Proyecto (Numérica)", color = "Tasa de Desempleo")

# Gráfico de violín de opinión por organización comunitaria
ggplot(df, aes(x = Organización_comunitaria, y = Opinion_proyecto_num, fill = Organización_comunitaria)) +
  geom_violin(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribución de la Opinión del Proyecto por Nivel de Organización Comunitaria", 
       x = "Organización Comunitaria", y = "Opinión del Proyecto (Numérica)", fill = "Organización Comunitaria")

# Análisis de la Relación entre Conocimiento del Proyecto y Opinión
ggplot(df, aes(x = factor(Conocimiento_proyecto), y = Opinion_proyecto_num, fill = factor(Conocimiento_proyecto))) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Opinión del Proyecto según Conocimiento del Proyecto", x = "Conocimiento del Proyecto", 
       y = "Opinión del Proyecto (Numérica)", fill = "Conocimiento") +
  theme(legend.position = "none")

# Análisis de Regresión Lineal para Predecir la Opinión del Proyecto
modelo <- lm(Opinion_proyecto_num ~ Población_total_region + Pob_indígena + Porcentaje_rural + 
               Tasa_desempleo + Confianza_instituciones + Num_actores_clave + 
               Participación_ciudadana + Conocimiento_proyecto + Confianza_responsables, 
             data = df)
summary(modelo)


# Análisis desempleo versus Opinión del Proyecto

p <- ggplot(df, aes(x = Tasa_desempleo, y = Opinion_proyecto_num, color = Región)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  theme_minimal() +
  labs(title = "Relación entre Tasa de Desempleo y Opinión del Proyecto",
       x = "Tasa de Desempleo", y = "Opinión del Proyecto (Numérica)",
       color = "Región")

# Convertir el gráfico de ggplot2 a interactivo con plotly
ggplotly(p)



# Confianza / Opinión
ggplot(df, aes(x = Opinion_proyecto, y = Confianza_instituciones)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  theme_minimal() +
  labs(title = "Confianza en Instituciones según Opinión del Proyecto", 
       x = "Opinión sobre el Proyecto", y = "Confianza en Instituciones")

# Relación entre Conocimiento del Proyecto y el Proyecto
ggplot(df, aes(x = Nombre_proyecto, y = Conocimiento_proyecto, fill = Nombre_proyecto)) +
  geom_violin(alpha = 0.6) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Conocimiento del Proyecto", x = "Proyecto", y = "Conocimiento del Proyecto")

# Análisis de variables Confianza responsables, Conocimiento Proyecto, Numero de actores clave y Participación ciudadana
df_long_num <- df %>% 
  pivot_longer(cols = c(Num_actores_clave, Participación_ciudadana, 
                        Conocimiento_proyecto, Confianza_responsables), 
               names_to = "Variable", values_to = "Valor")

ggplot(df_long_num, aes(x = Nombre_proyecto, y = Valor, fill = Nombre_proyecto)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~Variable, scales = "free") +
  labs(title = "Distribución de Variables Numéricas por Proyecto", x = "Proyecto", y = "Valor")

# Cargar las bibliotecas necesarias
library(tidyverse)
library(corrplot)
library(factoextra)

# Convertimos las variables a formato largo
df_long_cat <- df %>%
  pivot_longer(cols = c(Riesgo_desplazamiento, Impacto_cohesión_social, 
                        Cambio_empleo_local, Impacto_seguridad_servicios, 
                        Conflictos_potenciales), 
               names_to = "Variable", values_to = "Categoría")

# Gráfico de barras para variables categóricas
ggplot(df_long_cat, aes(x = Categoría, fill = Categoría)) +
  geom_bar() +
  facet_wrap(~Variable, scales = "free_x") +
  theme_minimal() +
  labs(title = "Distribución de Variables de Impacto", x = "Categoría", y = "Frecuencia") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)) +
  scale_fill_brewer(palette = "Set3")

# Gráfico de barras para Riesgo_desplazamiento por proyecto
ggplot(df, aes(x = Nombre_proyecto, fill = Riesgo_desplazamiento)) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(title = "Distribución del Riesgo de Desplazamiento por Proyecto",
       x = "Proyecto", y = "Frecuencia", fill = "Riesgo de Desplazamiento") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)) +
  scale_fill_brewer(palette = "Set1")

# Gráfico de barras para Impacto_cohesión_social por proyecto
ggplot(df, aes(x = Nombre_proyecto, fill = Impacto_cohesión_social)) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(title = "Distribución del Impacto en la Cohesión Social por Proyecto",
       x = "Proyecto", y = "Frecuencia", fill = "Impacto en la Cohesión Social") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)) +
  scale_fill_brewer(palette = "Set2")

# Gráfico de caja para Percepción_ambiental por proyecto
ggplot(df, aes(x = Nombre_proyecto, y = Percepción_ambiental, fill = Nombre_proyecto)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribución de la Percepción Ambiental por Proyecto",
       x = "Proyecto", y = "Percepción Ambiental", fill = "Proyecto") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)) +
  scale_fill_brewer(palette = "Pastel1")

# Gráfico de barras para Conflictos_potenciales por proyecto
ggplot(df, aes(x = Nombre_proyecto, fill = Conflictos_potenciales)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Proporción de Conflictos Potenciales por Proyecto",
       x = "Proyecto", y = "Proporción", fill = "Conflictos Potenciales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_brewer(palette = "Set2")

# Análisis de Correlación entre Variables de Impacto
# Convertir variables categóricas a numéricas para el análisis de correlación
df_cor <- df %>%
  mutate(
    Riesgo_desplazamiento_num = case_when(
      Riesgo_desplazamiento == "Alto" ~ 3,
      Riesgo_desplazamiento == "Medio" ~ 2,
      Riesgo_desplazamiento == "Bajo" ~ 1
    ),
    Impacto_cohesión_social_num = case_when(
      Impacto_cohesión_social == "Positivo" ~ 1,
      Impacto_cohesión_social == "Neutro" ~ 0,
      Impacto_cohesión_social == "Negativo" ~ -1
    ),
    Cambio_empleo_local_num = case_when(
      Cambio_empleo_local == "Aumento" ~ 1,
      Cambio_empleo_local == "Sin cambio" ~ 0,
      Cambio_empleo_local == "Disminución" ~ -1
    ),
    Impacto_seguridad_servicios_num = case_when(
      Impacto_seguridad_servicios == "Positivo" ~ 1,
      Impacto_seguridad_servicios == "Neutro" ~ 0,
      Impacto_seguridad_servicios == "Negativo" ~ -1
    ),
    Conflictos_potenciales_num = ifelse(Conflictos_potenciales == "Sí", 1, 0)
  )

# Calcular la matriz de correlación
correlaciones <- cor(df_cor[, c("Riesgo_desplazamiento_num", "Impacto_cohesión_social_num", 
                                "Cambio_empleo_local_num", "Percepción_ambiental", 
                                "Impacto_seguridad_servicios_num", "Conflictos_potenciales_num")])
print(correlaciones)
# Visualizar la matriz de correlación
corrplot(correlaciones, method = "circle", type = "upper", tl.col = "black", tl.srt = 45,
         title = "Matriz de Correlación de Variables de Impacto", mar = c(0,0,1,0))

# Análisis de Componentes Principales (PCA)
# PCA para variables de impacto
pca_result <- prcomp(df_cor[, c("Riesgo_desplazamiento_num", "Impacto_cohesión_social_num", 
                                "Cambio_empleo_local_num", "Percepción_ambiental", 
                                "Impacto_seguridad_servicios_num", "Conflictos_potenciales_num")], scale = TRUE)

# Visualizar los resultados del PCA
fviz_pca_biplot(pca_result, label = "var", col.var = "black", col.ind = df$Nombre_proyecto) +
  labs(title = "Análisis de Componentes Principales (PCA)", 
       subtitle = "Distribución de Proyectos según Variables de Impacto") +
  theme_minimal() +
  theme(axis.text = element_text(size = 10), 
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

# Análisis variable Impacto calidad de vida por proyecto
tabla_calidad_vida <- table(df$Nombre_proyecto, df$Impacto_calidad_vida)
print(tabla_calidad_vida)
prop.table(tabla_calidad_vida, margin = 1) * 100

# Visualización Impacto en calidad de vida por proyecto
ggplot(df, aes(x = Nombre_proyecto, fill = Impacto_calidad_vida)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  labs(title = "Proporción de Impacto en Calidad de Vida por Proyecto", 
       x = "Proyecto", y = "Proporción", fill = "Impacto en Calidad de Vida") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)) +
  scale_fill_brewer(palette = "Set3")

# Heatmap de Impacto en Calidad de Vida por Proyecto
tabla_melt <- melt(tabla_calidad_vida)

ggplot(tabla_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  labs(title = "Impacto en Calidad de Vida por Proyecto", 
       x = "Proyecto", y = "Impacto en Calidad de Vida", fill = "Frecuencia") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))



# Análisis de Satisfacción con la Mitigación

# Boxplot para ver la distribución de la satisfacción dentro de cada proyecto
ggplot(df, aes(x = Nombre_proyecto, y = Satisfacción_mitigación, fill = Nombre_proyecto)) +
  geom_boxplot(alpha = 0.7, outlier.colour = "red", outlier.shape = 16, outlier.size = 3) + 
  theme_minimal() +
  labs(title = "Distribución de la Satisfacción con las Acciones de Mitigación por Proyecto",
       x = "Proyecto",
       y = "Satisfacción con la Mitigación") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_fill_brewer(palette = "Set3")

# Gráfico de barras para la percepción del cumplimiento de compromisos por proyecto
ggplot(df, aes(x = Nombre_proyecto, fill = Cumplimiento_compromisos)) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(title = "Percepción del Cumplimiento de Compromisos por Proyecto",
       x = "Proyecto",
       y = "Cantidad de Respuestas") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set2") +
  geom_text(stat = 'count', aes(label = ..count..), position = position_dodge(0.8), vjust = -0.5)

# Prueba de Chi-cuadrado (para ver si hay relación entre Cumplimiento de Compromisos y Nombre del Proyecto)
tabla_proyecto_cumplimiento <- table(df$Nombre_proyecto, df$Cumplimiento_compromisos)
chi_result <- chisq.test(tabla_proyecto_cumplimiento)

# Mostrar el resultado de la prueba de chi-cuadrado
print(chi_result)

# ANOVA para ver si la Satisfacción con la Mitigación varía significativamente entre los proyectos
anova_satisfaccion <- aov(Satisfacción_mitigación ~ Nombre_proyecto, data = df)
summary(anova_satisfaccion)

# Verificar los supuestos del ANOVA: homogeneidad de varianzas (Test de Levene) y normalidad de residuos (Test de Shapiro-Wilk)
# Prueba de Levene

leveneTest(Satisfacción_mitigación ~ Nombre_proyecto, data = df)

# Prueba de normalidad de los residuos
shapiro.test(residuals(anova_satisfaccion))

# ANOVA para comparar satisfacción con la mitigación según el cumplimiento de compromisos
anova_mitigacion <- aov(Satisfacción_mitigación ~ Cumplimiento_compromisos, data = df)
summary(anova_mitigacion)

# Gráfico de violín para analizar densidad de valores dentro de cada categoría de cumplimiento
ggplot(df, aes(x = Cumplimiento_compromisos, y = Satisfacción_mitigación, fill = Cumplimiento_compromisos)) +
  geom_violin(alpha = 0.6) +
  geom_jitter(color = "black", width = 0.2, alpha = 0.5) +
  theme_minimal() +
  labs(title = "Distribución de la Satisfacción con la Mitigación según Cumplimiento de Compromisos",
       x = "Cumplimiento de Compromisos",
       y = "Satisfacción con la Mitigación") +
  scale_fill_brewer(palette = "Pastel1")
