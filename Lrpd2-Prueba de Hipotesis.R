# ==========================================================
# ANÁLISIS SOBRE LA PRUEBA DE HIPÓTESIS DE INVESTIGACIÓN
# UTILIZANDO MÉTODOS ESTADÍSTICOS APROPIADOS
# ==========================================================
# Efectividad de un protocolo de enfermería en la prevención
# de úlceras por presión en pacientes hospitalizados
# ==========================================================

# ==============================
# DATOS
# ==============================

datos <- matrix(c(12, 38,
                  4, 46),
                nrow = 2,
                byrow = TRUE)

rownames(datos) <- c("Sin Protocolo", "Con Protocolo")
colnames(datos) <- c("Presentó Úlcera", "No Presentó Úlcera")

resultado <- chisq.test(datos)

riesgo_sin <- 12 / 50
riesgo_con <- 4 / 50
riesgo_relativo <- riesgo_con / riesgo_sin

# ==============================
# GRÁFICA BONITA
# ==============================

png("grafico_ulceras.png", width=800, height=600)

barplot(datos[,1],
        beside = TRUE,
        names.arg = c("Sin Protocolo", "Con Protocolo"),
        main = "Incidencia de Úlceras por Presión",
        ylab = "Número de Pacientes",
        col = c("#FF6B6B", "#4D96FF"),
        border = "black",
        ylim = c(0,15))

grid(nx = NA, ny = NULL)

dev.off()

# ==============================
# HTML COMPLETO ORDENADO
# ==============================

html <- paste0('
<!DOCTYPE html>
<html>
<head>
<title>Prevención de Úlceras por Presión</title>
<style>
body { font-family: Arial; margin: 60px; line-height: 1.9; }
h1 { text-align: center; }
h2 { text-align: center; font-weight: normal; color: gray; }
h3 { color: #1B3C73; margin-top: 30px; }
table { border-collapse: collapse; width: 85%; margin: auto; }
th, td { border: 1px solid black; padding: 10px; text-align: center; }
th { background-color: #EAEAEA; }
p { text-align: justify; }
.center { text-align: center; }
</style>
</head>
<body>

<h1>ANÁLISIS SOBRE LA PRUEBA DE HIPÓTESIS DE INVESTIGACIÓN UTILIZANDO MÉTODOS ESTADÍSTICOS APROPIADOS</h1>
<h2>Efectividad de un protocolo de enfermería en la prevención de úlceras por presión</h2>

<p class="center"><b>Estudiantes:</b></p>
<p class="center">Huayta Quispe Nadia</p>
<p class="center">Huaraca Santos Ashlyn</p>
<p class="center">Sanchez Campos Luis Danilo</p>

<hr>

<h3>1. Introducción</h3>

<p>Las úlceras por presión constituyen una complicación frecuente en pacientes hospitalizados con movilidad limitada. 
Desde el ámbito de la enfermería, la prevención es una responsabilidad fundamental, ya que impacta directamente en la seguridad del paciente, la calidad del cuidado y la reducción de complicaciones clínicas. 
El presente estudio tiene como finalidad evaluar si la aplicación de un protocolo sistemático de cambios posturales reduce la incidencia de úlceras por presión en comparación con pacientes que no reciben dicha intervención.</p>

<h3>2. Planteamiento de Hipótesis</h3>

<p><b>Hipótesis Nula (H₀):</b> No existe relación significativa entre la aplicación del protocolo de cambios posturales y la aparición de úlceras por presión.</p>

<p><b>Hipótesis Alternativa (H₁):</b> Existe relación significativa entre la aplicación del protocolo de cambios posturales y la aparición de úlceras por presión.</p>

<h3>3. Datos Observados</h3>

<table>
<tr><th>Grupo</th><th>Presentó Úlcera</th><th>No Presentó Úlcera</th></tr>
<tr><td>Sin Protocolo</td><td>12</td><td>38</td></tr>
<tr><td>Con Protocolo</td><td>4</td><td>46</td></tr>
</table>

<p>Se evaluaron 100 pacientes hospitalizados, distribuidos en dos grupos de 50 pacientes cada uno.</p>

<h3>4. Resultados Estadísticos</h3>

<table>
<tr><th>Estadístico Chi-cuadrado</th><td>', round(resultado$statistic,3), '</td></tr>
<tr><th>Grados de libertad</th><td>', resultado$parameter, '</td></tr>
<tr><th>Valor p</th><td>', round(resultado$p.value,5), '</td></tr>
<tr><th>Riesgo Relativo</th><td>', round(riesgo_relativo,3), '</td></tr>
</table>

<h3>5. Explicación Detallada de los Resultados</h3>

<p>El estadístico Chi-cuadrado compara las frecuencias observadas con las frecuencias esperadas bajo la hipótesis nula. 
El valor obtenido fue ', round(resultado$statistic,3), ' con 1 grado de libertad. 
El valor p fue ', round(resultado$p.value,5), '. 
Dado que es ligeramente mayor a 0.05, no se rechaza la hipótesis nula al 95% de confianza. 
Sin embargo, el resultado se encuentra muy cercano al nivel de significancia, lo que indica una tendencia estadística.</p>

<p>El riesgo relativo fue ', round(riesgo_relativo,3), ', lo que significa que el grupo con protocolo presenta aproximadamente un 33% del riesgo de desarrollar úlceras en comparación con el grupo sin intervención, representando una reducción aproximada del 67% del riesgo.</p>

<h3>6. Gráfica</h3>

<p class="center"><img src="grafico_ulceras.png" width="700"></p>

<h3>7. Interpretación de la Gráfica</h3>

<p>En la gráfica se observa claramente que el grupo sin protocolo presenta mayor número de casos de úlceras (12 pacientes), mientras que el grupo con protocolo muestra únicamente 4 casos. 
La diferencia visual evidencia una reducción considerable asociada a la intervención de enfermería.</p>

<h3>8. Interpretación General</h3>

<p>De manera integral, los resultados sugieren que la implementación del protocolo de cambios posturales contribuye a disminuir la incidencia de úlceras por presión. 
Aunque estadísticamente no se alcanzó significancia estricta al nivel del 5%, la reducción observada y el riesgo relativo indican relevancia clínica importante en la práctica de enfermería.</p>

<h3>9. Conclusión General</h3>

<p>Se concluye que el protocolo de cambios posturales muestra un efecto protector frente a la aparición de úlceras por presión en pacientes hospitalizados. 
La evidencia respalda la importancia de su aplicación sistemática como estrategia preventiva dentro del cuidado hospitalario, contribuyendo a mejorar la calidad de atención y la seguridad del paciente.</p>

</body>
</html>
')

writeLines(html, "LRPD2_Analisis_Hipotesis.html")
browseURL("LRPD2_Analisis_Hipotesis.html")