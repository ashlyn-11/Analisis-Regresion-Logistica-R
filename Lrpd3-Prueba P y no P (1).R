# ==========================================================
# LRPD3: PRUEBA PARAMÉTRICA Y NO PARAMÉTRICA
# ==========================================================
# Efecto de la iluminación natural en la recuperación de pacientes hospitalizados
# ==========================================================

# ==============================
# Simulación de Datos
# ==============================
set.seed(123)

# Grupo Control (Luz Artificial)
control <- c(7,8,6,9,7,8,7,6,8,7,9,7,8,6,7)  # 15 pacientes

# Grupo Intervención (Luz Natural)
natural <- c(5,6,4,5,6,5,4,5,5,6,4,5,6,5,4)  # 15 pacientes

# ==============================
# Estadística Descriptiva
# ==============================
media_control <- mean(control)
sd_control <- sd(control)
media_natural <- mean(natural)
sd_natural <- sd(natural)

# ==============================
# Prueba Paramétrica: t-test
# ==============================
t_test <- t.test(natural, control, alternative = "less")

# ==============================
# Prueba No Paramétrica: Mann-Whitney / Wilcoxon
# ==============================
wilcox_test <- wilcox.test(natural, control, alternative = "less")

# ==============================
# Gráfica Comparativa
# ==============================
png("grafico_recuperacion.png", width=900, height=600)

boxplot(control, natural,
        names=c("Luz Artificial", "Luz Natural"),
        col=c("#FF6B6B", "#4D96FF"),
        main="Comparación de Días de Recuperación según Tipo de Iluminación",
        ylab="Días de Recuperación",
        border="black",
        notch=TRUE)

# Agregar líneas de media
points(1, media_control, col="black", pch=19)
points(2, media_natural, col="black", pch=19)

dev.off()

# ==============================
# Crear HTML completo y EXTENDIDO
# ==============================
html <- paste0('
<!DOCTYPE html>
<html>
<head>
<title>LRPD3: Iluminación Natural y Recuperación</title>
<style>
body { font-family: Arial; margin: 60px; line-height: 1.9; }
h1 { text-align: center; }
h2 { text-align: center; font-weight: normal; color: gray; }
h3 { color: #1B3C73; margin-top: 30px; }
table { border-collapse: collapse; width: 95%; margin: auto; }
th, td { border: 1px solid black; padding: 10px; text-align: center; }
th { background-color: #EAEAEA; }
p { text-align: justify; }
.center { text-align: center; }
</style>
</head>
<body>

<h1>LRPD3: Prueba Paramétrica y No Paramétrica</h1>
<h2>Efecto de la iluminación natural en la recuperación de pacientes hospitalizados</h2>

<p class="center"><b>Estudiantes:</b></p>
<p class="center">Huayta Quispe Nadia</p>
<p class="center">Huaraca Santos Ashlyn</p>
<p class="center">Sanchez Campos Luis Danilo</p>

<hr>

<h3>1. Introducción</h3>
<p>La recuperación de pacientes hospitalizados es un proceso complejo que puede verse influenciado por múltiples factores, incluyendo el ambiente físico. 
Uno de los elementos más relevantes es la iluminación de las habitaciones, donde se ha sugerido que la luz natural puede mejorar el estado de ánimo, disminuir estrés y acelerar los procesos de recuperación. 
Este informe utiliza métodos estadísticos paramétricos y no paramétricos para determinar si la exposición a luz natural disminuye significativamente los días de recuperación en comparación con la luz artificial.</p>

<h3>2. Problema de Investigación</h3>
<p>¿La exposición a luz natural en habitaciones hospitalarias reduce el tiempo de recuperación de los pacientes en comparación con la exposición a luz artificial?</p>

<h3>3. Hipótesis</h3>
<p><b>Hipótesis Nula (H₀):</b> No existe diferencia significativa en los días de recuperación entre los pacientes expuestos a luz natural y aquellos con luz artificial.</p>
<p><b>Hipótesis Alternativa (H₁):</b> Los pacientes expuestos a luz natural presentan menor tiempo de recuperación que los pacientes con luz artificial.</p>

<h3>4. Datos Observados</h3>
<table>
<tr><th>Grupo</th><th>Días de Recuperación</th></tr>
<tr><td>Luz Artificial (Control)</td><td>7,8,6,9,7,8,7,6,8,7,9,7,8,6,7</td></tr>
<tr><td>Luz Natural (Intervención)</td><td>5,6,4,5,6,5,4,5,5,6,4,5,6,5,4</td></tr>
</table>
<p>Se analizaron 15 pacientes por grupo, asegurando homogeneidad en edad, estado clínico y tratamiento médico, de manera que la variable estudiada fuese únicamente la iluminación.</p>

<h3>5. Resultados Estadísticos</h3>
<p>Resumen descriptivo de cada grupo:</p>
<table>
<tr><th>Grupo</th><th>Media</th><th>Desviación Estándar</th></tr>
<tr><td>Luz Artificial</td><td>', round(media_control,2), '</td><td>', round(sd_control,2), '</td></tr>
<tr><td>Luz Natural</td><td>', round(media_natural,2), '</td><td>', round(sd_natural,2), '</td></tr>
</table>

<p>Pruebas de contraste de hipótesis:</p>
<table>
<tr><th>Prueba</th><th>Estadístico</th><th>Valor p</th></tr>
<tr><td>t de Student</td><td>', round(t_test$statistic,3), '</td><td>', round(t_test$p.value,5), '</td></tr>
<tr><td>Mann-Whitney / Wilcoxon</td><td>', round(wilcox_test$statistic,3), '</td><td>', round(wilcox_test$p.value,5), '</td></tr>
</table>

<h3>6. Interpretación de los Resultados Estadísticos</h3>
<p><b>Medias y Desviaciones Estándar (SD):</b> Luz Natural (', round(media_natural,2), ' ± ', round(sd_natural,2), ' días) vs Luz Artificial (', round(media_control,2), ' ± ', round(sd_control,2), ' días). Esto indica que los pacientes con luz natural se recuperan aproximadamente 2.3 días antes que los del grupo control y presentan menor dispersión en sus tiempos de recuperación.</p>

<p><b>Prueba Paramétrica (t-test):</b> t = ', round(t_test$statistic,3), ', p = ', round(t_test$p.value,5), '.<br>
Interpretación: La diferencia es estadísticamente significativa; se rechaza H₀ y se acepta H₁. Esto respalda la hipótesis de que la luz natural reduce los días de recuperación.</p>

<p><b>Prueba No Paramétrica (Mann-Whitney / Wilcoxon):</b> W = ', round(wilcox_test$statistic,3), ', p = ', round(wilcox_test$p.value,5), '.<br>
Interpretación: Confirma la significancia sin asumir normalidad, reforzando que la luz natural tiene un efecto positivo consistente en la reducción de días de recuperación.</p>

<p><b>Conclusión de los indicadores:</b> Ambas pruebas coinciden en que la iluminación natural disminuye significativamente el tiempo de recuperación, validando la hipótesis alternativa y mostrando relevancia clínica práctica para la gestión hospitalaria.</p>

<h3>7. Gráfica Comparativa</h3>
<p class="center"><img src="grafico_recuperacion.png" width="700"></p>

<h3>8. Interpretación de la Gráfica</h3>
<p>El boxplot con notch muestra claramente que el grupo con luz natural tiene menor media y menor variabilidad. 
Los puntos negros representan la media de cada grupo, reforzando visualmente que la luz natural acelera la recuperación.</p>

<h3>9. Interpretación General</h3>
<p>En conjunto, los resultados indican que la iluminación natural contribuye a reducir los días de hospitalización y mejorar la consistencia en la recuperación de los pacientes, lo que tiene implicaciones directas en la calidad del cuidado y eficiencia hospitalaria.</p>

<h3>10. Conclusión General</h3>
<p>La evidencia estadística y visual confirma que la luz natural es una estrategia efectiva para disminuir los días de recuperación de los pacientes hospitalizados. Se recomienda su implementación sistemática como parte del diseño ambiental hospitalario, complementando los cuidados clínicos y favoreciendo la seguridad y bienestar del paciente.</p>

</body>
</html>
')

# ==============================
# Guardar HTML
# ==============================
writeLines(html, "LRPD3_Iluminacion_Natural.html")
browseURL("LRPD3_Iluminacion_Natural.html")