## code to prepare `mapas` dataset goes here
library(magrittr)

mapa_argentina_limitrofes_data <- rnaturalearth::ne_countries(scale = 50, country = c("chile", "uruguay",
                                                                                      "paraguay", "brazil", "bolivia"),
                                                              returnclass = "sf")

mapa_argentina_limitrofes_data <- sf::st_crop(mapa_argentina_limitrofes_data,
                                              xmin = -77, xmax = 0, ymin = -57, ymax = 20)
mapa_argentina_limitrofes_data <- mapa_argentina_limitrofes_data[, c("name", "geometry")]


argentina_provincias <- rnaturalearth::ne_states(country = c("argentina", "falkland islands"),
                                                 returnclass = "sf")
argentina_provincias <- argentina_provincias[, c("name", "geometry")]

argentina_provincias$name <- ifelse(argentina_provincias$name == "Falkland Islands",
                                    "Islas Malvinas", argentina_provincias$name)

argentina <- rnaturalearth::ne_countries(country = c("argentina"), returnclass = "sf",
                                         scale = 10)

argentina <- argentina[, c("name", "geometry")]
arg_buffer <- sf::st_buffer(argentina, 0.7)
arg_buffer_limite <- sf::st_difference(arg_buffer, argentina)

argentina <- rnaturalearth::ne_countries(country = c("argentina", "falkland islands"), returnclass = "sf",
                                         scale = 10)
argentina <- argentina[, c("name", "geometry")]

arg_topo <- metR::GetTopography(-75+360, -50+360, -20, -60, resolution = 1/10)
arg_topo[, lon := metR::ConvertLongitude(lon)]


## code to prepare `estaciones_nh` dataset goes here

col_metadatos_nh <- c("codigo_nh", "estacion", "provincia", "organismo", "lat", "lon", "altura")

estaciones_nh <- as.data.frame(data.table::fread(file = "data-raw/LISTADO_SMN-INTA.csv",
                                                 col.names = col_metadatos_nh))
estaciones_nh$codigo_nh <- gsub(".DAT", "", estaciones_nh$codigo_nh, fixed = TRUE)

estaciones_nh$codigo_nh <- gsub("NH", "", estaciones_nh$codigo_nh)

# De http://surferhelp.goldensoftware.com/topics/colors.htm?Highlight=colors
surfer_definition <- readLines("data-raw/surfer_colors")
surfer_definition <- surfer_definition[vapply(surfer_definition, nchar, 1) > 0]


colors <- strsplit(surfer_definition, "=")

surfer_names <- vapply(colors, function(x) x[[1]], "char")

surfer_colors <- vapply(colors, function(x) x[[2]], "char") %>%
  strsplit(" ") %>%
  vapply(function(x) rgb(x[1], x[2], x[3], x[4], maxColorValue = 255), "char") %>%
  setNames(surfer_names)



frame <- arg_buffer
bbox <- sf::st_bbox(frame)


lons <- seq(bbox[["xmin"]], bbox[["xmax"]], length.out = 100)
lats <- seq(bbox[["ymin"]], bbox[["ymax"]], length.out = 100)


points <- expand.grid(lon = lons,
                      lat = lats)
points_sf <- sf::st_as_sf(points, coords = c("lon", "lat"), crs = 4326)
inside <-  lengths(suppressMessages(sf::st_intersects(points_sf, frame))) != 0
arg_grid <- points[inside, ]

usethis::use_data(arg_buffer, arg_buffer_limite, arg_grid, mapa_argentina_limitrofes_data,
                  arg_topo, estaciones_nh, surfer_colors, argentina, argentina_provincias,
                  overwrite = TRUE, internal = TRUE)

## code to prepare `datos_nh_mensual` dataset goes here
library(data.table)

escape_unicode <- function(x) {
  if (!is.character(x)) {
    return(x)
  }

  stringi::stri_escape_unicode(x)
}

datos_nh_mensual <- Sys.glob("data-raw/NH*csv") |>
  lapply(fread) |>
  rbindlist() |>
  _[year(fecha) == 2019] |>
  _[, .(precipitacion_mensual = sum(precipitacion_pluviometrica, na.rm = TRUE),
        temperatura_media_mensual = mean(temperatura_abrigo_150cm, na.rm = TRUE)),
    by = .(id, mes = lubridate::floor_date(fecha, "month"))] |>
  _[, codigo_nh := gsub("NH", "", id)] |>
  _[, id := NULL] |>
  _[metadatos_nh(), on = .NATURAL] |>
  _[, lapply(.SD, escape_unicode)]



usethis::use_data(datos_nh_mensual, overwrite = TRUE)
