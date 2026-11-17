# Teoría de Juegos: laboratorio interactivo

Aplicación Shiny del Trabajo Fin de Grado de Mario Martín Rodríguez (Universidad de Granada),
con los juegos y algoritmos del trabajo explicados paso a paso.

**Versión en línea:** https://mario10ugr.shinyapps.io/tfg_shiny/

## Cómo lanzarla en tu ordenador

1. Descarga el repositorio (botón **Code › Download ZIP**) y descomprímelo, o clónalo con git.
2. Instala los paquetes una sola vez, en la consola de R:

   ```r
   install.packages(c("shiny", "bslib", "lpSolve"))
   ```

   (`fontawesome`, que dibuja los iconos, se instala automáticamente con Shiny.)
   El aviso sobre Rtools se puede ignorar.

3. En RStudio, abre `app.R` (File › Open File…) y pulsa **Run App**.
   Para verla a pantalla completa, elige «Run External» en el menú del botón Run App
   o pulsa «Open in Browser» en el visor.

## Cómo está organizada

Cada juego se elige en el menú superior. Dentro de cada uno, las pestañas llevan de la
mano: primero se juega, luego se ve qué pasa, después la explicación paso a paso y, al
final, una pestaña «Matemáticas» con la teoría. Cada pestaña cabe en una pantalla:
no hace falta desplazarse. En pantallas muy pequeñas (menos de 1000 × 600 píxeles)
la página vuelve a desplazarse con normalidad.

## Estructura

```
├── app.R                    menú superior y montaje de las páginas
├── R/
│   ├── logica_*.R           cálculos (equilibrios, minimax, alfa-beta, tres en raya, aprendizaje)
│   ├── svg.R                dibujo de matrices con flechas y de árboles
│   ├── ui_comun.R           menú, estructura de página, pestañas y controles paso a paso
│   ├── formato.R, graficos.R
│   └── pag_*.R              una página por fichero
└── www/estilo.css           estilos
```

Los ficheros `logica_*.R` no dependen de Shiny y se pueden usar en los experimentos
de la Parte II:

```r
source("R/formato.R"); source("R/logica_k_en_raya.R")
kr_decidir(integer(9), 1L, kr_config(3, 3), metodo = "alfabeta")$nodos   # 18297
```

Probada con R 4.3 y 4.4, Shiny 1.14 y bslib 0.12, en pantallas de 1366 × 768 y 1920 × 1080.
