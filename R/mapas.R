#' Mapas
#'
#' Mapas de Argentina, sus provincias, departamentos y paises limítrofes. Los
#' mapas de Argentina, las provincias y departamentos surgen del repositorio
#' público del [Instituto Geográfico Nacional](https://www.ign.gob.ar/NuestrasActividades/InformacionGeoespacial/CapasSIG),
#' mientas que el mapa de países limítrofes es parte del repositorio
#' [Natural Earth](https://www.naturalearthdata.com/).
#'
#' @param provincias vector de caracteres con los nombres de provincias a filtrar.
#' Si es `NULL`, devuelve todas las provincias de Argentina.
#' @param departamentos lógico. Si es `TRUE` grafica los departamentos.
#'
#' @return Devuelve una tibble con las variables necesarias para generar un mapa
#' utilizando [ggplot2] y [sf].
#'
#' @examples
#' library(ggplot2)
#'
#' # Solo Argentina
#' ggplot() +
#'   geom_sf(data = mapa_argentina())
#'
#' # Argentina y sus provincias
#' ggplot() +
#'   geom_sf(data = mapa_provincias())
#'
#' # Algunas provincias
#' ggplot() +
#'   geom_sf(data = mapa_provincias(provincias = c("La Pampa", "Córdoba")))
#'
#' # Algunas provincias y sus departamentos
#' ggplot() +
#'   geom_sf(data = mapa_provincias(provincias = c("La Pampa", "Córdoba"),
#'                                  departamentos = TRUE))
#'
#' @export
#' @rdname mapas
mapa_argentina <- function() {
  argentina
}

#' @export
#' @rdname mapas
mapa_provincias <- function(provincias = NULL, departamentos = FALSE) {
  mapa <- argentina_provincias

  if (!is.null(provincias)) {
    no_hay <- !(provincias %in% unique(mapa$name))
    if (any(no_hay)) {
      stop("La(s) provincia(s) ", paste(provincias[no_hay], collapse = " "),
           " no se encontraron entre la lista de posibles provincias:\n",
           paste0(unique(mapa$nam), collapse = ", "))
    }
    mapa <- mapa[mapa$name %in% provincias, ]
  }

  if (departamentos) {
    departamentos <- mapa_departamentos(provincias)
    mapa <- rbind(mapa, departamentos)
  }

  mapa
}




#' @export
#' @rdname mapas
mapa_argentina_limitrofes <- function() {
  mapa_argentina_limitrofes_data
}

#' @export
#' @rdname mapas
mapa_departamentos <- function(provincias = NULL) {
  mapa <- get_mapa(mapas$depto)
  mapa <- mapa[, c("gid", "geometry")]
  colnames(mapa) <- c("name", "geometry")
  if (!is.null(provincias)) {
    prov <- argentina_provincias
    prov <- prov[prov$name %in% provincias, ]
    mapa <- suppressWarnings(suppressMessages(sf::st_intersection(mapa, prov)))
    mapa <- mapa[, !grepl("\\.1$",  colnames(mapa))]
    mapa <- mapa[sf::st_geometry_type(mapa) != "POINT", ]
  }

  mapa


}

get_mapa <- function(mapa) {
  file <- file.path(dir_mapas(), mapa$rds)

  if (!file.exists(file)) {
    message("Descargando mapa")
    descargar_mapa(mapa)
  }

  readRDS(file)
}


descargar_mapa <- function(mapa) {
  dir <- tempdir()
  file <- normalizePath(file.path(dir, "shape.zip"), mustWork = FALSE)
  utils::download.file(mapa$url, file, mode = "wb")
  dir <- normalizePath(file.path(dir, "uncompress"), mustWork = FALSE)
  utils::unzip(file, exdir = dir)
  sf <- sf::read_sf(normalizePath(file.path(dir, mapa$file), mustWork = FALSE))
  dir <- dir_mapas()
  sf::st_crs(sf) <- 4326
  saveRDS(sf, normalizePath(file.path(dir, mapa$rds), mustWork = FALSE))
}

borrar_cache <- function() {
  dir <- dir_mapas()
  unlink(dir, TRUE)
}


dir_mapas <- function() {
  dir <- normalizePath(file.path(rappdirs::user_data_dir("agromet", "inta"), "mapas"), mustWork = FALSE)
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  dir
}



mapas <- list(
  pais = list(
    url = "http://ramsac.ign.gob.ar/api/v1/capas-sig/Geodesia+y+demarcaci%C3%B3n/L%C3%ADmites/pais/shp",
    file = "pais.shp",
    rds = "pais.Rds"
  ),
  provincias = list(
    url = "http://ramsac.ign.gob.ar/api/v1/capas-sig/Geodesia+y+demarcaci%C3%B3n/L%C3%ADmites/provincia/shp",
    file = "provincia.shp",
    rds = "provincias.Rds"
  ),
  depto = list(
    url = "http://ramsac.ign.gob.ar/api/v1/capas-sig/Geodesia+y+demarcaci%C3%B3n/L%C3%ADmites/linea_de_limite_070110/shp",
    file = "linea_de_limite_070110.shp",
    rds = "departamentos.Rds"
  )
)



