spatialQC <- function(chkDf, layerName = "layer", ...){
  func_params <- list(...)
  params <- override_params(func_params)
  if (params$debug) Mar.utils::where_now()
  gpkgName <- ifelse(is.null(params$gpkgName), "spatialQC.gpkg", params$gpkgName)
  chkDf_spatial<-Mar.utils::df_qc_spatial(chkDf, lat.field = "LAT_DD_QC", lon.field = "LON_DD_QC")
  #use coords to determine NAFO
  suppressMessages(chkDf_spatial <- Mar.utils::identify_area(df=chkDf_spatial, agg.poly.shp = Mar.data::NAFOSubunits_sf, agg.poly.field = "NAFO", lat.field = "LAT_DD_QC", lon.field = "LON_DD_QC"))
  colnames(chkDf_spatial)[colnames(chkDf_spatial)=="NAFO"] <- "NAFO_CALC_QC"
  #use coords to determine CFA
  suppressMessages(chkDf_spatial <- Mar.utils::identify_area(agg.poly.shp = Mar.data::SurfClamFAs_sf, df=chkDf_spatial, agg.poly.field = "Name", lat.field = "LAT_DD_QC", lon.field = "LON_DD_QC"))
  colnames(chkDf_spatial)[colnames(chkDf_spatial)=="Name"] <- "CLAM_FISHING_AREA"
  #use coords to determine bank
  suppressMessages(chkDf_spatial <-  Mar.utils::identify_area(agg.poly.shp = Mar.data::banks_sf, df=chkDf_spatial, agg.poly.field = "Name", lat.field = "LAT_DD_QC", lon.field = "LON_DD_QC"))
  colnames(chkDf_spatial)[colnames(chkDf_spatial)=="Name"] <- "BANK"
  
  suppressMessages(chkDf_spatial <- Mar.utils::identify_area(agg.poly.shp = Mar.data::Bathy_sf, df=chkDf_spatial, agg.poly.field = "DEP_MIN_M", lat.field = "LAT_DD_QC", lon.field = "LON_DD_QC"))
  suppressMessages(chkDf_spatial <- Mar.utils::identify_area(agg.poly.shp = Mar.data::Bathy_sf, df=chkDf_spatial, agg.poly.field = "DEP_MAX_M", lat.field = "LAT_DD_QC", lon.field = "LON_DD_QC"))

  if (!(sub("\\d{4}_", "", layerName) %in% c("Commercial_Sample_Profile", "Length_Frequency"))) {
    suppressMessages(Mar.utils::df_sf_to_gpkg(chkDf_spatial, lat.field = "LAT_DD_QC", lon.field = "LON_DD_QC", layerName = layerName, gpkgName = gpkgName, path=params$resultsFolder))
  }
   #no need to return an sf object
  chkDf_spatial <- sf::st_drop_geometry(chkDf_spatial)
  return(chkDf_spatial)
}


addBasemapLayers <- function(...){
  func_params <- list(...)
  params <- override_params(func_params)
  if (params$debug) Mar.utils::where_now()
  
  gpkgName <- ifelse(is.null(params$gpkgName),"addBasemapLayers.gpkg", params$gpkgName) 
  layers <- sf::st_layers(file.path(paste0(params$resultsFolder),gpkgName))
  if(!("landingLocations" %in% layers$name)) {
    landingLocations_sf <- data.frame(Location = c("Mulgrave", "Argentia"),
                                      Longitude = c(-61.3883, -53.9833),
                                      Latitude = c(45.6083, 47.2967))
    landingLocations_sf <- suppressMessages(sf::st_as_sf(landingLocations_sf, coords = c("Longitude", "Latitude"), crs = 4326))
    invisible(Mar.utils::df_sf_to_gpkg(landingLocations_sf, layerName = "landingLocations", gpkgName = params$gpkgName, path=params$resultsFolder))
  }
  
  invisible(Mar.utils::df_sf_to_gpkg(df = Mar.data::banks_sf, layerName = "banks_sf", gpkgName = params$gpkgName, path=params$resultsFolder))
  invisible(Mar.utils::df_sf_to_gpkg(df = Mar.data::SurfClamFAs_sf, layerName = "SurfClamFAs_sf", gpkgName = params$gpkgName, path=params$resultsFolder))
  invisible(Mar.utils::df_sf_to_gpkg(df = Mar.data::NAFOSubunits_sf, layerName = "NAFO_sf", gpkgName = params$gpkgName, path=params$resultsFolder))
  
  if (!("Bathy_sf" %in% layers$name)) invisible(Mar.utils::df_sf_to_gpkg(df = Mar.data::Bathy_sf, layerName = "Bathy_sf", gpkgName = params$gpkgName, path=params$resultsFolder))

}
