library(sf)

zones <- st_read('data-raw/zone2016_kom_v01_clean.shp') %>%
  st_transform(crs = 4326)

st_write(
  zones,
  dsn = "data/zones.json",
  driver = "GeoJSON",
  delete_dsn = TRUE
)

readr::read_csv("data-raw/nli_zone0221_transpose.csv") %>%
  dplyr::mutate(
    urban = dplyr::recode(
      urban,
      "1" = "Capital",
      "2" = "Capital suburb",
      "3" = "Provincial city",
      "4" = "Provincial town",
      "5" = "Rural area"
    )
  ) %>%
  feather::write_feather("data/zones.feather")

# Much faster
#z_ <- geojsonsf::sf_geojson(zones_[1:500, ])
#cat(z_, file = "data/zones.json")

# plotly.js is very insistent on there being a top-level
# id that is linked to the location attribute
# https://github.com/plotly/plotly.js/issues/4154#issuecomment-526198848
# TODO: make this easier!!
zones_ <- geojsonio::geojson_read("data/zones.json")
zones_$features <- Map(function(x, y) {
  x$id <- y; x
}, zones_$features, as.character(zones$ID2))
jsonlite::write_json(
  zones_, path = "data/zones.json",
  auto_unbox = TRUE
)
