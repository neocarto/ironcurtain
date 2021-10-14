library(sf)
library(rnaturalearth)

land <- ne_download(
  scale = 110,
  type = "land",
  category = "physical",
  returnclass = "sf"
)

ocean <- ne_download(
  scale = 110,
  type = "ocean",
  category = "physical",
  returnclass = "sf"
)

graticule = st_graticule(
  crs = st_crs(4326),
  ndiscr = 100,
  lon = seq(-180, 180, by = 10),
  lat = seq(-90, 90, by = 5),
  margin = 0.01
)

bb <-
  st_as_sfc(st_bbox(c(
    xmin = -50 ,
    xmax = 70,
    ymin = 20,
    ymax = 80
  ),
  crs = st_crs(4326)))

sf::sf_use_s2(FALSE)
ocean <- st_intersection(ocean, bb)
ocean <-  st_segmentize(ocean, 100)
land <- st_intersection(land, bb)
land <-  st_segmentize(land, 100)
graticule <- st_intersection(graticule, bb)
sf::sf_use_s2(TRUE)


ortho <- "+proj=ortho +lat_0=-10 +lon_0=15 +x_0=0 +y_0=0
          +ellps=WGS84 +units=m +no_defs"

ocean <- st_transform(ocean, ortho)
land <- st_transform(land, ortho)
graticule =  st_transform(graticule, ortho)