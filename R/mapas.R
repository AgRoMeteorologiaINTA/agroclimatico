#' Mapas
#'
#' @param provincias Vector de provincias para filtrar. Si es `NULL`,
#' no filtra.
#' @param departamentos Lógico indicando si dibujar los departamenteos o no.
#'
#' @export
#' @rdname mapas
mapa_argentina <- function() {
  get_mapa(mapas$pais)
}

#' @export
#' @rdname mapas
mapa_provincias <- function(provincias = NULL, departamentos = FALSE) {
  mapa <- get_mapa(mapas$provincias)

  if (!is.null(provincias)) {
    no_hay <- !(provincias %in% unique(mapa$nam))
    if (any(no_hay)) {
      stop("La(s) provincia(s) ", paste(provincias[no_hay], collapse = " "),
           " no se encontraron entre la lista de posibles provincias:\n",
           paste0(unique(mapa$nam), collapse = ", "))
    }
    mapa <- mapa[mapa$nam %in% provincias, ]
  }

  if (departamentos) {
    departamentos <- mapa_departamentos(provincias)
    mapa <- rbind(mapa, departamentos)
  }

  mapa
}


#' @export
#' @rdname mapas
coord_argentina <- function(xlim = c(-77, -50), ylim = c(-57, -20), expand = FALSE, ...) {
  coord_sf(xlim = xlim, ylim = ylim, expand = expand, ...)
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

  if (!is.null(provincias)) {
    prov <- get_mapa(mapas$provincias)
    prov <- prov[prov$nam %in% provincias, ]
    mapa <- sf::st_intersection(mapa, prov)
    mapa <- mapa[, !grepl("\\.1$",  colnames(mapa))]
    mapa <- mapa[, colnames(mapa) != "vlp"]
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
  file <- file.path(dir, "shape.zip")
  download.file(mapa$url, file)
  zip::unzip(file, exdir = file.path(dir, "uncompress"))
  sf <- sf::read_sf(file.path(dir, "uncompress", mapa$file))
  dir <- dir_mapas()

  saveRDS(sf, file.path(dir, mapa$rds))
}

borrar_cache <- function() {
  dir <- dir_mapas()
  unlink(dir, TRUE)
}


dir_mapas <- function() {
  dir <- file.path(rappdirs::user_data_dir("agromet", "inta"), "mapas")
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



