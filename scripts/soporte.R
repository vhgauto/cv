nacimiento <- as.Date("1989-02-28")

separador <- function(X) {
  cat(
    "::: {.dotted-word-flex}\n<span class='seccion h1'>",
    X,
    "</span>\n:::",
    sep = ""
  )
}

item <- function(tipo, inicio, fin, titulo, ubicacion, detalle, borde = FALSE) {
  if (borde) {
    grid_borde <- "::: {.grid .borde-abajo}"
  } else {
    grid_borde <- "::: {.grid}"
  }

  cat(
    grid_borde,
    "\n::: {.g-col-3}\n<tipo>",
    tipo,
    "</tipo><br>{{< iconify subway calendar >}} &nbsp; <rango>",
    inicio,
    "{{< iconify ic sharp-arrow-right >}}",
    fin,
    "</rango>\n:::\n::: {.g-col-9}\n<b>",
    titulo,
    "</b>.{{< iconify ic sharp-location-on>}}",
    ubicacion,
    "</i>.<br>",
    detalle,
    "\n:::\n:::",
    sep = ""
  )
}

pub <- readr::read_tsv("datos/publicaciones.tsv", show_col_types = FALSE)
años_pub <- unique(pub$año) |>
  sort() |>
  rev()

publicaciones <- function(X, borde = FALSE) {
  p <- dplyr::filter(pub, año == X)$publicacion
  p <- paste0("- ", p)
  p <- stringr::str_flatten(p, collapse = "\n ")
  p <- stringr::str_replace_all(p, '\"(.+)\"', '"**\\1**"')
  p <- stringr::str_replace_all(
    p,
    'Víctor Gauto',
    '<fondo-verde>Víctor Gauto</fondo-verde>'
  )
  p <- stringr::str_replace_all(
    p,
    'Gauto, Víctor',
    '<fondo-verde>Gauto, Víctor</fondo-verde>'
  )
  p <- stringr::str_replace_all(
    p,
    "doi\\.org/(.+)",
    "[\\1 {{< iconify weui:link-filled >}}](https://doi.org/\\1)"
  )

  if (X != min(años_pub)) {
    grid_borde <- "\n::: {.grid .borde-abajo}"
  } else {
    grid_borde <- "\n::: {.grid}"
  }

  cat(
    grid_borde,
    "\n::: {.g-col-3}\n<tipo>",
    X,
    "</tipo>\n:::\n::: {.g-col-9}\n",
    p,
    "\n:::\n:::"
  )
}

habilidades <- function(tipo, descrip, borde = FALSE) {
  if (borde) {
    grid_borde <- "::: {.grid .borde-abajo}"
  } else {
    grid_borde <- "::: {.grid}"
  }

  cat(
    grid_borde,
    "\n::: {.g-col-3}\n<tipo>",
    tipo,
    "</tipo>\n:::\n::: {.g-col-9}\n",
    descrip,
    "\n:::\n:::",
    sep = ""
  )
}

keywords <- function() {
  p <- paste0("`", sort(readLines("datos/keywords.tsv")), "`") |>
    stringr::str_flatten(collapse = "&nbsp;&nbsp;&nbsp;")
  cat("<center>\n", p, "\n</center>\n", sep = "")
}

programación <- function(color = "black") {
  paste0(
    "{{< iconify fa-brands:r-project style='color:#1E69BF;' >}} R &nbsp; &nbsp; 
  {{< iconify logos:python >}} Python &nbsp; &nbsp; 
  {{< quarto >}} Quarto &nbsp; &nbsp; 
  {{< iconify material-icon-theme:tex >}} LaTeX &nbsp; &nbsp; 
  {{< iconify material-icon-theme:typst >}} Typst &nbsp; &nbsp; 
  {{< iconify devicon:git >}} git &nbsp; &nbsp; 
  {{< iconify ri:github-fill style='color: ",
    color,
    ";' >}} GitHub"
  )
}

github <- function() {
  cat("[Ver en {{< iconify ri:github-fill >}}](https://github.com/vhgauto/cv)")
}
