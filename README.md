# EDM_segundaEntrega

Sitio Quarto de la segunda entrega de **EDM — Estadística Descriptiva Multivariada**
(Universidad Nacional de Colombia).

## Estructura del repositorio

```text
├── _quarto.yml                        # Configuración del sitio (navbar, sidebar, temas)
├── styles.css                         # Estilos del sitio (claro/oscuro, impresión, responsive)
├── _setup.R                           # Datos y ayudantes comunes a todas las páginas
├── index.qmd                          # Portada
├── about.qmd                          # Información del sitio
├── entregas/                          # Productos de las entregas
│   └── analisis_correspondencias.qmd  # EACS: correspondencias simples
├── proyecto/                          # Componentes del proyecto de investigación (futuro)
├── scripts/                           # Código de procesamiento (futuro)
├── _borradores/                       # Material de trabajo (no se publica)
├── data/                              # Datos (no versionados)
└── renv.lock                          # Versiones de las dependencias
```

> `EDM_PetFInder/` es el repositorio de referencia que sirve de plantilla. Queda
> excluido del render y de Git: no forma parte de este sitio.

## Vista previa y render

```bash
# Restaurar dependencias de R (solo la primera vez)
Rscript -e 'renv::restore()'

# Vista previa con recarga automática
quarto preview

# Construir el sitio en _site/
quarto render
```

## Cómo agregar una sección nueva

1. Crear la página dentro de `entregas/` o `proyecto/`, por ejemplo
   `entregas/nueva_seccion.qmd`.
2. En `_quarto.yml`, agregarla en dos lugares:
   - `website.navbar` (menú de navegación),
   - `website.sidebar.contents` (barra lateral).
3. Si la página comparte datos o funciones con las demás, cargar el setup común
   en su primer bloque:

   ````markdown
   ```{r}
   #| label: setup
   #| echo: false
   #| include: false
   source(here::here("_setup.R"))
   ```
   ````

4. Poner en `_setup.R` solo lo que se reutiliza; el código propio de la sección
   se queda en su `.qmd`.

Las carpetas `proyecto/` y `entregas/` ya están contempladas en
`project.render`, así que cualquier `.qmd` nuevo en ellas se publica sin tocar
más configuración.
