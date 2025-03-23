# PART I: 7 LIDAR DATA SOURCES

# 0. PACKAGES
#------------

install.packages("pacman")
pacman::p_load(
    terra, tidyverse, sf,
    terrainr, osmdata, marmap,
    rayshader
)

# 1. Switzerland 0.5m
#---------------------

links_data <- readr::read_csv(
    "ch.swisstopo.swissalti3d-pcTysSaP.csv"
) # PLEASE ENTER YOUR FILE NAME

print(links_data)

# combine the column name (valid link in our case)
# with the data in the first column into a vector

links_vec <- c(
    names(links_data)[1],
    links_data[[1]]
)

print(links_vec)

# Download each file with a loop

name <- "swissalti3d_"

lapply(seq_along(links_vec), function(i) {
    download.file(
        links_vec[i],
        destfile = paste0(name, i, ".tif"),
        mode = "wb"
    )
})

# Get the names of the raster files

main_dir <- getwd()

raster_files <- list.files(
    path = main_dir,
    pattern = name,
    full.names = TRUE
)

print(raster_files)

# Load all the files as a single virtual raster file

dem <- terra::vrt(raster_files)

# Aggregate

dem_final <- terra::aggregate(
    dem,
    fact = 2
)

# Quick plot

terra::plot(dem_final)

# 2. Germany 1m
#--------------

dem <- terra::rast("E440N5400/E440N5400.tif") # PLEASE ENTER YOUR FILE NAME

# 8.568134,48.831447,8.609333,48.855962

xmin <- 8.568134
ymin <- 48.831447
xmax <- 8.609333
ymax <- 48.855962

bbox <- sf::st_sfc(
    sf::st_polygon(
        list(
            cbind(
                c(
                    xmin, xmax,
                    xmax, xmin, xmin
                ),
                c(
                    ymin, ymin,
                    ymax, ymax, ymin
                )
            )
        )
    ),
    crs = 4326
) |>
    sf::st_as_sf() |>
    sf::st_transform(terra::crs(dem))

dem_final <- terra::crop(
    dem, bbox
)

terra::plot(dem_final)

# 3. Spain 2m
#------------

raster_file <- list.files(
    path = main_dir,
    pattern = "MDT02",
    full.names = TRUE
)

dem <- terra::rast(raster_file)
print(dem)

dem_final <- terra::aggregate(
    dem,
    fact = 2
)

terra::plot(dem_final)

# 4. United Kingdom 1m
#---------------------

unzip("lidar_composite_dtm-2022-1-ST59sw.zip") # PLEASE ENTER YOUR FILE NAME

list.files(
    path = main_dir,
    pattern = "ST59sw",
    full.names = TRUE
)

dem <- terra::rast("ST59sw_DTM_1m.tif") # PLEASE ENTER YOUR FILE NAME
print(dem)

dem_final <- terra::aggregate(
    dem,
    fact = 2
)

terra::plot(dem_final)

# 5. Poland 1m
#-------------

dem_final <- terra::rast(
    "DigitalTerrainModelFormatTIFF.tiff" # PLEASE ENTER YOUR FILE NAME
)

terra::plot(dem_final)

# 6. Argentina 5m
#----------------

unzip("4172-05-3-b.zip") # PLEASE ENTER YOUR FILE NAME

list.files(
    path = main_dir,
    pattern = "4172-05-3-b", # PLEASE ENTER YOUR FILE NAME
    full.names = TRUE
)

dem_final <- terra::rast(
    "4172-05-3-b.img" # PLEASE ENTER YOUR FILE NAME
)

print(dem_final)

terra::plot(dem_final)

# 7. United States 1m
#--------------------

# -119.608755,37.735833,-119.575710,37.757144

xmin <- -119.608755
ymin <- 37.735833
xmax <- -119.575710
ymax <- 37.757144

bb <- sf::st_sfc(
    sf::st_polygon(
        list(
            cbind(
                c(xmin, xmax, xmax, xmin, xmin),
                c(ymin, ymin, ymax, ymax, ymin)
            )
        )
    ),
    crs = 4326
)

dem <- terrainr::get_tiles(
    data = bb,
    output_prefix = "yosemite",
    side_length = 8e3,
    resolution = 1,
    services = "elevation",
    verbose = TRUE
)

dem_final <- terra::rast(dem$elevation)
terra::plot(dem_final)

# PART II: RENDERING 3D MAP

# 3D MAP

# raster to matrix
mat <- rayshader::raster_to_matrix(
    dem_final
)

# texture
cols <- marmap::etopo.colors(n = 16)
pie(rep(1, length(cols)), col = cols)
pal <- cols[c(1, 4:5, 10:12)]
pie(rep(1, length(pal)), col = pal)

# OpenStreetMap features

if (terra::crs(dem_final) != "EPSG:4326") {
    dem_new <- terra::project(
        dem_final, "EPSG:4326"
    )
}

# Get the extent
dem_ext <- terra::ext(dem_new)

# Create a bounding box
dem_bbox <- c(
    dem_ext$xmin, dem_ext$ymin,
    dem_ext$xmax, dem_ext$ymax
)

# OSM roads

osm_data_roads <- osmdata::opq(dem_bbox) |>
    osmdata::add_osm_feature("highway") |>
    osmdata::osmdata_sf()

print(osm_data_roads)

roads <- osm_data_roads$osm_lines |>
    sf::st_transform(
        terra::crs(dem_final)
    )

print(roads)

# OSM rivers

osm_data_rivers <- osmdata::opq(dem_bbox) |>
    osmdata::add_osm_feature("waterway") |>
    osmdata::osmdata_sf()

rivers <- osm_data_rivers$osm_lines |>
    sf::st_transform(
        terra::crs(dem_final)
    )

# OSM buildings
osm_data_buildings <- osmdata::opq(dem_bbox) |>
    osmdata::add_osm_feature("building") |>
    osmdata::osmdata_sf()

buildings <- osm_data_buildings$osm_polygons |>
    sf::st_transform(
        terra::crs(dem_final)
    )

# OSM places
osm_data_places <- osmdata::opq(dem_bbox) |>
    osmdata::add_osm_feature("place") |>
    osmdata::osmdata_sf()

places <- osm_data_places$osm_points |>
    sf::st_transform(
        terra::crs(dem_final)
    )

print(places)

# render scene

# basic
mat |>
    rayshader::height_shade(
        texture = texture
    ) |>
    # add shadow to emphasize the slopes and depressions
    rayshader::add_shadow(
        rayshader::lamb_shade(
            mat,
            zscale = 6
        ), .2
    ) |>
    # add shading by adjusting details, contrast & brightness
    rayshader::add_shadow(
        rayshader::texture_shade(
            mat,
            detail = .9,
            contrast = 9, brightness = 12
        ), .15
    ) |>
    # simulate soft shadows in areas with diffuse lighting
    rayshader::add_shadow(
        rayshader::ambient_shade(mat), .1
    ) |>
    # add roads
    rayshader::add_overlay(
        rayshader::generate_line_overlay(
            roads,
            extent = dem_final,
            linewidth = 2, color = "white",
            heightmap = mat
        ),
        alphalayer = .5
    ) |>
    # add rivers
    rayshader::add_overlay(
        rayshader::generate_line_overlay(
            rivers,
            extent = dem_final,
            linewidth = 5, color = "deepskyblue",
            heightmap = mat
        ) |>
    # add buildings
    rayshader::add_overlay(
        rayshader::generate_polygon_overlay(
            buildings, extent = dem_final,
            linewidth = 2, linecolor = "darkred",
            palette = "darkred", heightmap = mat
        )
    )
    ) |>
    # add places
    rayshader::add_overlay(
    rayshader::generate_label_overlay(
        places, extent = dem_final,
        text_size = 3, point_size = 2,
        color = "white", heightmap = mat,
        data_label_column = "name"
    )
    ) |>
    rayshader::plot_3d(
        mat, zscale = 1,
        solid = FALSE,
        shadow = TRUE,
        shadow_darkness = 1, 
        windowsize = c(600, 600),
        zoom = .5, phi = 85, theta = 0
    )

rayshader::render_camera(zoom = .51)

# render snapshot
rayshader::render_snapshot(
    filename = "snapshot.png",
    software_render = TRUE
)

# render 3D map
url <- "https://dl.polyhaven.org/file/ph-assets/HDRIs/hdr/4k/brown_photostudio_02_4k.hdr"
hdri_file <- basename(url)

download.file(
    url = url,
    destfile = hdri_file,
    mode = "wb"
)

rayshader::render_highquality(
    filename = "swiss-topo.png",
    preview = TRUE,
    light = FALSE,
    environment_light = hdri_file,
    intensity = 2,
    rotate_env = 90,
    parallel = TRUE,
    width = 2000, height = 2000,
    interactive = FALSE
  )
