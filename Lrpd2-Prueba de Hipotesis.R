# ==========================================================
# ANÁLISIS SOBRE LA PRUEBA DE HIPÓTESIS DE INVESTIGACIÓN
# Efectividad de un protocolo de enfermería en la prevención
# de úlceras por presión
# ==========================================================

# ==============================
# 1. DATOS
# ==============================

datos <- matrix(c(12, 38,
                  4, 46),
                nrow = 2,
                byrow = TRUE)

rownames(datos) <- c("Sin Protocolo", "Con Protocolo")
colnames(datos) <- c("Presentó Úlcera", "No Presentó Úlcera")

total_pacientes <- sum(datos)

# ==============================
# 2. PRUEBAS ESTADÍSTICAS
# ==============================

resultado_chi <- chisq.test(datos, correct = FALSE)
resultado_fisher <- fisher.test(datos)

# ==============================
# 3. MEDIDAS DE RIESGO
# ==============================

riesgo_sin <- 12 / 50
riesgo_con <- 4 / 50
riesgo_relativo <- riesgo_con / riesgo_sin
reduccion_riesgo <- (1 - riesgo_relativo) * 100

# ==============================
# 4. DECISIÓN
# ==============================

if(resultado_chi$p.value < 0.05){
  decision <- "Se rechaza la Hipótesis Nula (H₀) y se acepta la Hipótesis Alternativa (H₁), ya que existe una relación significativa entre la aplicación del protocolo y la reducción de úlceras por presión."
} else {
  decision <- "No se rechaza la Hipótesis Nula (H₀)."
}

# ==============================
# 5. GRÁFICA
# ==============================

png("grafico_ulceras.png", width=900, height=650)

barplot(datos[,1],
        names.arg = c("Sin Protocolo", "Con Protocolo"),
        main = "Incidencia de Úlceras por Presión",
        ylab = "Número de Pacientes",
        col = c("#FF6B6B", "#4D96FF"),
        ylim = c(0,15))

text(c(0.7,1.9),
     datos[,1] + 0.5,
     labels = datos[,1],
     cex = 1.2)

grid()

dev.off()

# ==============================
# 6. HTML
# ==============================

html <- paste0('
<!DOCTYPE html>
<html>
<head>
<title>Prueba de Hipótesis</title>
<style>
body { font-family: Arial; margin: 70px; line-height: 1.8; }
h1, h2 { text-align: center; }
h3 { color: #1B3C73; margin-top: 30px; }
table { border-collapse: collapse; width: 80%; margin: auto; }
th, td { border: 1px solid black; padding: 8px; text-align: center; }
th { background-color: #EAEAEA; }
p { text-align: justify; }
img { display: block; margin: auto; }
.center { text-align: center; }
</style>
</head>
<body>

<h1>ANÁLISIS SOBRE LA PRUEBA DE HIPÓTESIS DE INVESTIGACIÓN</h1>
<h2>Efectividad de un protocolo de enfermería en la prevención de úlceras por presión</h2>

<p class="center"><b>Estudiantes:</b></p>
<p class="center">Huayta Quispe Nadia</p>
<p class="center">Huaraca Santos Ashlyn</p>
<p class="center">Sanchez Campos Luis Danilo</p>

<hr>

<h3>1. Introducción</h3>

<p>Las úlceras por presión son lesiones frecuentes en pacientes hospitalizados con movilidad reducida. Su aparición puede generar complicaciones, infecciones y prolongación de la estancia hospitalaria. Por ello, el personal de enfermería implementa protocolos preventivos, como los cambios posturales, con el fin de reducir su incidencia.</p>

<h3>2. Hipótesis</h3>

<p><b>Hipótesis Nula (H₀):</b> No existe relación significativa entre la aplicación del protocolo y la aparición de úlceras por presión.</p>

<p><b>Hipótesis Alternativa (H₁):</b> Existe relación significativa entre la aplicación del protocolo y la aparición de úlceras por presión.</p>

<h3>3. Cuadro de Resultados</h3>

<table>
<tr>
<th>Grupo</th>
<th>Presentó Úlcera</th>
<th>No Presentó Úlcera</th>
</tr>
<tr>
<td>Sin Protocolo</td>
<td>12</td>
<td>38</td>
</tr>
<tr>
<td>Con Protocolo</td>
<td>4</td>
<td>46</td>
</tr>
</table>

<br>

<table>
<tr><th>Chi-cuadrado</th><td>', round(resultado_chi$statistic,3), '</td></tr>
<tr><th>Valor p</th><td>', round(resultado_chi$p.value,5), '</td></tr>
<tr><th>Riesgo Relativo</th><td>', round(riesgo_relativo,3), '</td></tr>
<tr><th>Reducción del Riesgo (%)</th><td>', round(reduccion_riesgo,1), '%</td></tr>
</table>

<h3>4. Interpretación</h3>

<p>', decision, '</p>

<p>El grupo que recibió el protocolo presentó una reducción aproximada del ', round(reduccion_riesgo,1), '% en el riesgo de desarrollar úlceras por presión en comparación con el grupo sin intervención, lo que demuestra un efecto preventivo importante.</p>

<h3>5. Gráfica</h3>

<img src="grafico_ulceras.png" width="700">

<h3>6. Interpretación de la Gráfica</h3>

<p>La gráfica muestra que el grupo sin protocolo presentó 12 casos (24%), mientras que el grupo con protocolo registró solo 4 casos (8%). Se observa claramente que la incidencia en el grupo sin intervención es mayor, lo que confirma visualmente el impacto positivo del protocolo aplicado.</p>

<h3>7. Discusión</h3>

<p>Los resultados evidencian que la aplicación del protocolo de cambios posturales contribuye de manera significativa a la reducción de úlceras por presión. Esta disminución no solo tiene valor estadístico, sino también relevancia clínica, ya que permite mejorar la calidad del cuidado y prevenir complicaciones en pacientes hospitalizados.</p>

<h3>8. Conclusión</h3>

<p>En conclusión, el análisis realizado demuestra que el protocolo de cambios posturales es efectivo en la prevención de úlceras por presión. La reducción observada en el grupo intervenido respalda su implementación como estrategia preventiva dentro del cuidado de enfermería, promoviendo una atención segura y basada en evidencia.</p>

</body>
</html>
')

writeLines(html, "LRPD2_Analisis_Hipotesis.html")
browseURL("LRPD2_Analisis_Hipotesis.html")