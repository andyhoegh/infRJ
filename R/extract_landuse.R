#' Extract land use from USGS NLCD
#'
#' @param lat Latitude for point
#' @param long Longitude for point
#' @param SiteName Name associated with point
#' @param buffer Distance around point in meters.
#'
#' @return tibble with land use type and proportion
#' @export
#'
#' @examples
#' \dontrun{
#' rgee::ee_Initialize()
#' extract_landuse(45.676998, -111.042931, 'Bozeman', buffer = 100)
#' }
extract_landuse <- function( lat, long, SiteName, buffer = 100  ){
  landuse <- rgee::ee$ImageCollection("USGS/NLCD_RELEASES/2021_REL/NLCD")
  start <- rgee::ee$Date$fromYMD(2021,1,1)
  end <- rgee::ee$Date$fromYMD(2021,12,31)
  landuse2021 = rgee::ee$Image(landuse$filterDate(start,end)$mean())
  point <- rgee::ee$Geometry$Point(long, lat)
  point_buffer <- rgee::ee$Geometry$buffer(point, buffer) # in meters

  landuse <- landuse2021$reduceRegion(
    reducer = rgee::ee$Reducer$frequencyHistogram(),
    geometry = point_buffer,
    scale = 30
  )

  landuse <- landuse2021$reduceRegion(
    reducer = rgee::ee$Reducer$frequencyHistogram(),
    geometry = point_buffer,
    scale = 30
  )

  tibble::tibble(cat = names(landuse$getInfo()$landcover), pixels = unlist(landuse$getInfo()$landcover)) |>
    dplyr::mutate(prop = pixels / sum(pixels),
                  SiteName = SiteName,
                  range = buffer)
}
