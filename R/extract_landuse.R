extract_landuse <- function( lat, long, SiteName, landuse2021, buffer = 100  ){
  point <- rgee::ee$Geometry$Point(long, lat)
  point_buffer <- rgee::ee$Geometry$buffer(point, buffer) # in meters

  landuse <- landuse2021$reduceRegion(
    reducer = ee$Reducer$frequencyHistogram(),
    geometry = point_buffer,
    scale = 30
  )

  tmp <- landuse$getInfo()$landcover

  tibble::tibble(cat = names(tmp), pixels = unlist(tmp)) |>
    dplyr::mutate(prop = pixels / sum(pixels),
           SiteName = SiteName,
           range = buffer)
}
