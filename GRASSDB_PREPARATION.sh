#location: workshop/FLORA_ECOREGIONS

#importing aridity map
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_climate_aridity/aridity_index.tif output=climate_aridity_index

#setting region and resolution to the aridity map
g.region raster=climate_aridity_index@PERMANENT

#computing inverse of the aridity index
r.mapcalc "climate_aridity_index = 1 - climate_aridity_index" --overwrite

#importing evapotranspiration map
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_climate_aridity/evapotranspiration_yearly.tif output=climate_annual_PET

#reprojecting the human footprint map
gdalwarp -s_srs ESRI:54009 -t_srs EPSG:4326 -r near -of GTiff /home/blas/Dropbox/RESEARCH/DATA/Global_human_footprint/wildareas-v3-2009-human-footprint.tif /home/blas/Dropbox/RESEARCH/DATA/Global_human_footprint/human_footprint.tif

#importing human footprint
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_human_footprint/human_footprint.tif output=human_footprint -o

#importing chelsa
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_climate_CHELSA/bio1.tif output=climate_bio1
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_climate_CHELSA/bio4.tif output=climate_bio4
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_climate_CHELSA/bio5.tif output=climate_bio5
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_climate_CHELSA/bio6.tif output=climate_bio6
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_climate_CHELSA/bio7.tif output=climate_bio7
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_climate_CHELSA/bio12.tif output=climate_bio12
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_climate_CHELSA/bio14.tif output=climate_bio14
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_climate_CHELSA/bio15.tif output=climate_bio15
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_climate_CHELSA/bio17.tif output=climate_bio17
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_climate_CHELSA/bio18.tif output=climate_bio18

#importing envirem
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_climate_ENVIREM/ENVIREM/current_30arcsec_aridityIndexThornthwaite.tif output=climate_aridity_thornthwaite -o
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_climate_ENVIREM/ENVIREM/current_30arcsec_climaticMoistureIndex.tif output=climate_moisture_index -o
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_climate_ENVIREM/ENVIREM/current_30arcsec_continentality.tif output=climate_continentality -o
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_climate_ENVIREM/ENVIREM/current_30arcsec_embergerQ.tif output=climate_embergerQ  -o
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_climate_ENVIREM/ENVIREM/current_30arcsec_growingDegDays0.tif output=climate_growing_degree_days_0 -o
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_climate_ENVIREM/ENVIREM/current_30arcsec_growingDegDays5.tif output=climate_growing_degree_days_5 -o
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_climate_ENVIREM/ENVIREM/current_30arcsec_maxTempColdest.tif output=climate_max_temperature_coldest_month -o
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_climate_ENVIREM/ENVIREM/current_30arcsec_minTempWarmest.tif output=climate_min_temperature_warmest_month -o
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_climate_ENVIREM/ENVIREM/current_30arcsec_PETDriestQuarter.tif output=climate_PET_driest_quarter -o
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_climate_ENVIREM/ENVIREM/current_30arcsec_PETseasonality.tif output=climate_PET_seasonality -o
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_climate_ENVIREM/ENVIREM/current_30arcsec_PETWarmestQuarter.tif output=climate_PET_warmest_quarter -o
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_climate_ENVIREM/ENVIREM/current_30arcsec_PETWettestQuarter.tif output=climate_PET_wettest_quarter -o
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_climate_ENVIREM/ENVIREM/current_30arcsec_thermicityIndex.tif output=climate_thermicity_index -o
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_climate_ENVIREM/ENVIREM/current_30arcsec_topoWet.tif output=topography_wetness -o
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_climate_ENVIREM/ENVIREM/current_30arcsec_tri.tif output=topography_roughness -o

#importing climate velocity
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_climate_velocity_Sandel/Velocity_latlong.tif output=climate_velocity_LGM -o

#importing accesibility to cities and road density
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_human_accesibility_to_cities/2015_accessibility_to_cities_v1.0.tif output=human_accesibility -o
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_human_accesibility_to_cities/connectivity_road_density.tif output=human_road_density -o																																																																																			#importing human population
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_human_population/gpw-v4-population-count-rev11_2020_30_sec_tif/gpw_v4_population_count_rev11_2020_30_sec.tif output=human_population -o

#copying variables from the variables_earth location	
g.copy raster=MASK@variables_earth,MASK
g.copy raster=geo_lat@variables_earth,geo_latitude
g.copy raster=geo_lon@variables_earth,geo_longitude
g.copy raster=landcover@variables_earth,landcover	
g.copy raster=landcover_diversity@variables_earth,landcover_diversity
g.copy raster=landcover_veg_bare@variables_earth,landcover_bare_soil_percent	
g.copy raster=landcover_veg_herb@variables_earth,landcover_herbaceous_percent	
g.copy raster=landcover_veg_tree@variables_earth,landcover_trees_percent
g.copy raster=topo_aspect@variables_earth,topography_aspect
g.copy raster=topo_diversidad@variables_earth,topography_diversity																																																																																																																																																																																																																																																																																																																																																																																									
g.copy raster=topo_posicion@variables_earth,topography_position
g.copy raster=topo_slope@variables_earth,topography_slope
g.copy raster=topo_elevation@variables_earth,topography_elevation
g.copy raster=sun_hours_average@variables_earth,topography_sun_hours_average
g.copy raster=sun_hours_maximum@variables_earth,topography_sun_hours_maximum
g.copy raster=sun_hours_minimum@variables_earth,topography_sun_hours_minimum
g.copy raster=sun_hours_range@variables_earth,topography_sun_hours_range
g.copy raster=sun_rad_average@variables_earth,topography_sun_radiation_average
g.copy raster=sun_rad_maximum@variables_earth,topography_sun_radiation_maximum
g.copy raster=sun_rad_minimum@variables_earth,topography_sun_radiation_minimum
g.copy raster=sun_rad_range@variables_earth,topography_sun_radiation_range

#importing NDVI data
g.mapset mapset=NDVI
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_remote_sensing_NDVI_Copernicus/NDVI_0101.tif output=ndvi_0101
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_remote_sensing_NDVI_Copernicus/NDVI_0111.tif output=ndvi_0111
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_remote_sensing_NDVI_Copernicus/NDVI_0121.tif output=ndvi_0121
r.mapcalc "ndvi_01 = (ndvi_0101 + ndvi_0111 + ndvi_0121) / 3"
g.remove -f type=raster name=ndvi_0101,ndvi_0111,ndvi_0121

r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_remote_sensing_NDVI_Copernicus/NDVI_0201.tif output=ndvi_0201
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_remote_sensing_NDVI_Copernicus/NDVI_0211.tif output=ndvi_0211
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_remote_sensing_NDVI_Copernicus/NDVI_0221.tif output=ndvi_0221
r.mapcalc "ndvi_02 = (ndvi_0201 + ndvi_0211 + ndvi_0221) / 3"
g.remove -f type=raster name=ndvi_0201,ndvi_0211,ndvi_0221

r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_remote_sensing_NDVI_Copernicus/NDVI_0301.tif output=ndvi_0301
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_remote_sensing_NDVI_Copernicus/NDVI_0311.tif output=ndvi_0311
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_remote_sensing_NDVI_Copernicus/NDVI_0321.tif output=ndvi_0321
r.mapcalc "ndvi_03 = (ndvi_0301 + ndvi_0311 + ndvi_0321) / 3" --overwrite
g.remove -f type=raster name=ndvi_0301,ndvi_0311,ndvi_0321

r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_remote_sensing_NDVI_Copernicus/NDVI_0401.tif output=ndvi_0401
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_remote_sensing_NDVI_Copernicus/NDVI_0411.tif output=ndvi_0411
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_remote_sensing_NDVI_Copernicus/NDVI_0421.tif output=ndvi_0421
r.mapcalc "ndvi_04 = (ndvi_0401 + ndvi_0411 + ndvi_0421) / 3"
g.remove -f type=raster name=ndvi_0401,ndvi_0411,ndvi_0421

r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_remote_sensing_NDVI_Copernicus/NDVI_0501.tif output=ndvi_0501
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_remote_sensing_NDVI_Copernicus/NDVI_0511.tif output=ndvi_0511
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_remote_sensing_NDVI_Copernicus/NDVI_0521.tif output=ndvi_0521
r.mapcalc "ndvi_05 = (ndvi_0501 + ndvi_0511 + ndvi_0521) / 3"
g.remove -f type=raster name=ndvi_0501,ndvi_0511,ndvi_0521

r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_remote_sensing_NDVI_Copernicus/NDVI_0601.tif output=ndvi_0601
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_remote_sensing_NDVI_Copernicus/NDVI_0611.tif output=ndvi_0611
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_remote_sensing_NDVI_Copernicus/NDVI_0621.tif output=ndvi_0621
r.mapcalc "ndvi_06 = (ndvi_0601 + ndvi_0611 + ndvi_0621) / 3"
g.remove -f type=raster name=ndvi_0601,ndvi_0611,ndvi_0621

r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_remote_sensing_NDVI_Copernicus/NDVI_0701.tif output=ndvi_0701
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_remote_sensing_NDVI_Copernicus/NDVI_0711.tif output=ndvi_0711
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_remote_sensing_NDVI_Copernicus/NDVI_0721.tif output=ndvi_0721
r.mapcalc "ndvi_07 = (ndvi_0701 + ndvi_0711 + ndvi_0721) / 3"
g.remove -f type=raster name=ndvi_0701,ndvi_0711,ndvi_0721

r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_remote_sensing_NDVI_Copernicus/NDVI_0801.tif output=ndvi_0801
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_remote_sensing_NDVI_Copernicus/NDVI_0811.tif output=ndvi_0811
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_remote_sensing_NDVI_Copernicus/NDVI_0821.tif output=ndvi_0821
r.mapcalc "ndvi_08 = (ndvi_0801 + ndvi_0811 + ndvi_0821) / 3"
g.remove -f type=raster name=ndvi_0801,ndvi_0811,ndvi_0821

r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_remote_sensing_NDVI_Copernicus/NDVI_0901.tif output=ndvi_0901
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_remote_sensing_NDVI_Copernicus/NDVI_0911.tif output=ndvi_0911
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_remote_sensing_NDVI_Copernicus/NDVI_0921.tif output=ndvi_0921
r.mapcalc "ndvi_09 = (ndvi_0901 + ndvi_0911 + ndvi_0921) / 3"
g.remove -f type=raster name=ndvi_0901,ndvi_0911,ndvi_0921

r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_remote_sensing_NDVI_Copernicus/NDVI_1001.tif output=ndvi_1001
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_remote_sensing_NDVI_Copernicus/NDVI_1011.tif output=ndvi_1011
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_remote_sensing_NDVI_Copernicus/NDVI_1021.tif output=ndvi_1021
r.mapcalc "ndvi_10 = (ndvi_1001 + ndvi_1011 + ndvi_1021) / 3"
g.remove -f type=raster name=ndvi_1001,ndvi_1011,ndvi_1021

r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_remote_sensing_NDVI_Copernicus/NDVI_1101.tif output=ndvi_1101
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_remote_sensing_NDVI_Copernicus/NDVI_1111.tif output=ndvi_1111
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_remote_sensing_NDVI_Copernicus/NDVI_1121.tif output=ndvi_1121
r.mapcalc "ndvi_11 = (ndvi_1101 + ndvi_1111 + ndvi_1121) / 3"
g.remove -f type=raster name=ndvi_1101,ndvi_1111,ndvi_1121

r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_remote_sensing_NDVI_Copernicus/NDVI_1201.tif output=ndvi_1201
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_remote_sensing_NDVI_Copernicus/NDVI_1211.tif output=ndvi_1211
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_remote_sensing_NDVI_Copernicus/NDVI_1221.tif output=ndvi_1221
r.mapcalc "ndvi_12 = (ndvi_1201 + ndvi_1211 + ndvi_1221) / 3"
g.remove -f type=raster name=ndvi_1201,ndvi_1211,ndvi_1221



r.mapcalc "ndvi_mean = (ndvi_01 + ndvi_02 + ndvi_03 + ndvi_04 + ndvi_05 + ndvi_06 + ndvi_07 + ndvi_08 + ndvi_09 + ndvi_10 + ndvi_11 + ndvi_12) / 12"

r.mapcalc "ndvi_min = min(ndvi_01, ndvi_02, ndvi_03, ndvi_04, ndvi_05, ndvi_06, ndvi_07, ndvi_08, ndvi_09, ndvi_10, ndvi_11, ndvi_12)"


r.mapcalc "ndvi_max = max(ndvi_01, ndvi_02, ndvi_03, ndvi_04, ndvi_05, ndvi_06, ndvi_07, ndvi_08, ndvi_09, ndvi_10, ndvi_11, ndvi_12)"

r.mapcalc "ndvi_range = ndvi_max - ndvi_min"

g.remove -f type=raster name=ndvi_01@PERMANENT,ndvi_02@PERMANENT,ndvi_03@PERMANENT,ndvi_04@PERMANENT,ndvi_05@PERMANENT,ndvi_06@PERMANENT,ndvi_07@PERMANENT,ndvi_08@PERMANENT,ndvi_09@PERMANENT,ndvi_10@PERMANENT,ndvi_11@PERMANENT,ndvi_12@PERMANENT


#copying ndvi bluemarble maps to permanent
g.copy raster=ndvi_2004_01@ndvi_bluemarble,ndvi_2004_01
g.copy raster=ndvi_2004_02@ndvi_bluemarble,ndvi_2004_02
g.copy raster=ndvi_2004_03@ndvi_bluemarble,ndvi_2004_03
g.copy raster=ndvi_2004_04@ndvi_bluemarble,ndvi_2004_04
g.copy raster=ndvi_2004_05@ndvi_bluemarble,ndvi_2004_05
g.copy raster=ndvi_2004_06@ndvi_bluemarble,ndvi_2004_06
g.copy raster=ndvi_2004_07@ndvi_bluemarble,ndvi_2004_07
g.copy raster=ndvi_2004_08@ndvi_bluemarble,ndvi_2004_08
g.copy raster=ndvi_2004_09@ndvi_bluemarble,ndvi_2004_09
g.copy raster=ndvi_2004_10@ndvi_bluemarble,ndvi_2004_10
g.copy raster=ndvi_2004_11@ndvi_bluemarble,ndvi_2004_11
g.copy raster=ndvi_2004_12@ndvi_bluemarble,ndvi_2004_12

r.mapcalc "ndvi_mean = (ndvi_2004_01 + ndvi_2004_02 + ndvi_2004_03 + ndvi_2004_04 + ndvi_2004_05 + ndvi_2004_06 + ndvi_2004_07 + ndvi_2004_08 + ndvi_2004_09 + ndvi_2004_10 + ndvi_2004_11 + ndvi_2004_12) / 12"

r.mapcalc "ndvi_min = min(ndvi_2004_01, ndvi_2004_02, ndvi_2004_03, ndvi_2004_04, ndvi_2004_05, ndvi_2004_06, ndvi_2004_07, ndvi_2004_08, ndvi_2004_09, ndvi_2004_10, ndvi_2004_11, ndvi_2004_12)"


r.mapcalc "ndvi_max = max(ndvi_2004_01, ndvi_2004_02, ndvi_2004_03, ndvi_2004_04, ndvi_2004_05, ndvi_2004_06, ndvi_2004_07, ndvi_2004_08, ndvi_2004_09, ndvi_2004_10, ndvi_2004_11, ndvi_2004_12)"

r.mapcalc "ndvi_range = ndvi_max - ndvi_min"

#importing vegetation continuous fields to mapset vcf
r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_vegetation_continuous_fields/vcf_2016.tif output=vcf_2016
g.rename raster=vcf_2016.red@vcf,landcover_tree_2016
g.rename raster=vcf_2016.green@vcf,landcover_non_tree_2016
g.rename raster=vcf_2016.blue@vcf,landcover_bare_soil_2016

r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_vegetation_continuous_fields/vcf_2015.tif output=vcf_2015
g.rename raster=vcf_2015.red@vcf,landcover_tree_2015
g.rename raster=vcf_2015.green@vcf,landcover_non_tree_2015
g.rename raster=vcf_2015.blue@vcf,landcover_bare_soil_2015

r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_vegetation_continuous_fields/vcf_2014.tif output=vcf_2014
g.rename raster=vcf_2014.red@vcf,landcover_tree_2014
g.rename raster=vcf_2014.green@vcf,landcover_non_tree_2014
g.rename raster=vcf_2014.blue@vcf,landcover_bare_soil_2014

r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_vegetation_continuous_fields/vcf_2013.tif output=vcf_2013
g.rename raster=vcf_2013.red@vcf,landcover_tree_2013
g.rename raster=vcf_2013.green@vcf,landcover_non_tree_2013
g.rename raster=vcf_2013.blue@vcf,landcover_bare_soil_2013

r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_vegetation_continuous_fields/vcf_2012.tif output=vcf_2012
g.rename raster=vcf_2012.red@vcf,landcover_tree_2012
g.rename raster=vcf_2012.green@vcf,landcover_non_tree_2012
g.rename raster=vcf_2012.blue@vcf,landcover_bare_soil_2012

r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_vegetation_continuous_fields/vcf_2011.tif output=vcf_2011
g.rename raster=vcf_2011.red@vcf,landcover_tree_2011
g.rename raster=vcf_2011.green@vcf,landcover_non_tree_2011
g.rename raster=vcf_2011.blue@vcf,landcover_bare_soil_2011

r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_vegetation_continuous_fields/vcf_2010.tif output=vcf_2010
g.rename raster=vcf_2010.red@vcf,landcover_tree_2010
g.rename raster=vcf_2010.green@vcf,landcover_non_tree_2010
g.rename raster=vcf_2010.blue@vcf,landcover_bare_soil_2010

r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_vegetation_continuous_fields/vcf_2009.tif output=vcf_2009
g.rename raster=vcf_2009.red@vcf,landcover_tree_2009
g.rename raster=vcf_2009.green@vcf,landcover_non_tree_2009
g.rename raster=vcf_2009.blue@vcf,landcover_bare_soil_2010

r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_vegetation_continuous_fields/vcf_2010.tif output=vcf_2010
g.rename raster=vcf_2010.red@vcf,landcover_tree_2010
g.rename raster=vcf_2010.green@vcf,landcover_non_tree_2010
g.rename raster=vcf_2010.blue@vcf,landcover_bare_soil_2010

r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_vegetation_continuous_fields/vcf_2009.tif output=vcf_2009
g.rename raster=vcf_2009.red@vcf,landcover_tree_2009
g.rename raster=vcf_2009.green@vcf,landcover_non_tree_2009
g.rename raster=vcf_2009.blue@vcf,landcover_bare_soil_2009

r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_vegetation_continuous_fields/vcf_2008.tif output=vcf_2008
g.rename raster=vcf_2008.red@vcf,landcover_tree_2008
g.rename raster=vcf_2008.green@vcf,landcover_non_tree_2008
g.rename raster=vcf_2008.blue@vcf,landcover_bare_soil_2008

r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_vegetation_continuous_fields/vcf_2007.tif output=vcf_2007
g.rename raster=vcf_2007.red@vcf,landcover_tree_2007
g.rename raster=vcf_2007.green@vcf,landcover_non_tree_2007
g.rename raster=vcf_2007.blue@vcf,landcover_bare_soil_2007

r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_vegetation_continuous_fields/vcf_2006.tif output=vcf_2006
g.rename raster=vcf_2006.red@vcf,landcover_tree_2006
g.rename raster=vcf_2006.green@vcf,landcover_non_tree_2006
g.rename raster=vcf_2006.blue@vcf,landcover_bare_soil_2006

r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_vegetation_continuous_fields/vcf_2005.tif output=vcf_2005
g.rename raster=vcf_2005.red@vcf,landcover_tree_2005
g.rename raster=vcf_2005.green@vcf,landcover_non_tree_2005
g.rename raster=vcf_2005.blue@vcf,landcover_bare_soil_2005

r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_vegetation_continuous_fields/vcf_2004.tif output=vcf_2004
g.rename raster=vcf_2004.red@vcf,landcover_tree_2004
g.rename raster=vcf_2004.green@vcf,landcover_non_tree_2004
g.rename raster=vcf_2004.blue@vcf,landcover_bare_soil_2004

r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_vegetation_continuous_fields/vcf_2004.tif output=vcf_2004
g.rename raster=vcf_2004.red@vcf,landcover_tree_2004
g.rename raster=vcf_2004.green@vcf,landcover_non_tree_2004
g.rename raster=vcf_2004.blue@vcf,landcover_bare_soil_2004

r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_vegetation_continuous_fields/vcf_2004.tif output=vcf_2004
g.rename raster=vcf_2004.red@vcf,landcover_tree_2004
g.rename raster=vcf_2004.green@vcf,landcover_non_tree_2004
g.rename raster=vcf_2004.blue@vcf,landcover_bare_soil_2004

r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_vegetation_continuous_fields/vcf_2003.tif output=vcf_2003
g.rename raster=vcf_2003.red@vcf,landcover_tree_2003
g.rename raster=vcf_2003.green@vcf,landcover_non_tree_2003
g.rename raster=vcf_2003.blue@vcf,landcover_bare_soil_2003

r.in.gdal input=/home/blas/Dropbox/RESEARCH/DATA/Global_vegetation_continuous_fields/vcf_2002.tif output=vcf_2002
g.rename raster=vcf_2002.red@vcf,landcover_tree_2002
g.rename raster=vcf_2002.green@vcf,landcover_non_tree_2002
g.rename raster=vcf_2002+.blue@vcf,landcover_bare_soil_2002

#in mapset ecoregions

#set region to global
g.region raster=climate_bio1@PERMANENT

#set resolution to 5km
g.region n=90 s=-90 res=0.03472222

#resampling target rasters from PERMANENT
r.resamp.stats input=climate_annual_PET@PERMANENT output=climate_annual_PET

r.resamp.stats input=climate_PET_driest_quarter@PERMANENT output=climate_PET_driest_quarter

r.resamp.stats input=climate_PET_seasonality@PERMANENT output=climate_PET_seasonality

r.resamp.stats input=climate_PET_warmest_quarter@PERMANENT output=climate_PET_warmest_quarter

r.resamp.stats input=climate_PET_wettest_quarter@PERMANENT output=climate_PET_wettest_quarter

r.resamp.stats input=climate_aridity_index@PERMANENT output=climate_aridity_index

r.resamp.stats input=climate_aridity_thornthwaite@PERMANENT output=climate_aridity_thornthwaite

r.resamp.stats input=climate_bio1@PERMANENT output=climate_bio1

r.resamp.stats input=climate_bio12@PERMANENT output=climate_bio12

r.resamp.stats input=climate_bio14@PERMANENT output=climate_bio14

r.resamp.stats input=climate_bio15@PERMANENT output=climate_bio15

r.resamp.stats input=climate_bio17@PERMANENT output=climate_bio17

r.resamp.stats input=climate_bio18@PERMANENT output=climate_bio18

r.resamp.stats input=climate_bio4@PERMANENT output=climate_bio4

r.resamp.stats input=climate_bio5@PERMANENT output=climate_bio5

r.resamp.stats input=climate_bio6@PERMANENT output=climate_bio6

r.resamp.stats input=climate_bio7@PERMANENT output=climate_bio7

r.resamp.stats input=climate_continentality@PERMANENT output=climate_continentality

r.resamp.stats input=climate_embergerQ@PERMANENT output=climate_embergerQ

r.resamp.stats input=climate_growing_degree_days_0@PERMANENT output=climate_growing_degree_days_0

r.resamp.stats input=climate_growing_degree_days_5@PERMANENT output=climate_growing_degree_days_5

r.resamp.stats input=climate_max_temperature_coldest_month@PERMANENT output=climate_max_temperature_coldest_month

r.resamp.stats input=climate_min_temperature_warmest_month@PERMANENT output=climate_min_temperature_warmest_month

r.resamp.stats input=climate_moisture_index@PERMANENT output=climate_moisture_index

r.resamp.stats input=climate_thermicity_index@PERMANENT output=climate_thermicity_index

r.resamp.stats input=climate_velocity_LGM@PERMANENT output=climate_velocity_LGM

r.resamp.stats input=geo_latitude@PERMANENT output=geo_latitude

r.resamp.stats input=geo_longitude@PERMANENT output=geo_longitude

r.resamp.stats input=human_accesibility@PERMANENT output=human_accesibility

r.resamp.stats input=human_footprint@PERMANENT output=human_footprint

r.resamp.stats input=human_population@PERMANENT output=human_population

r.resamp.stats input=human_road_density@PERMANENT output=human_road_density

r.resamp.stats input=landcover_bare_soil_percent@PERMANENT output=landcover_bare_soil_percent

r.resamp.stats input=landcover_diversity@PERMANENT output=landcover_diversity

r.resamp.stats input=landcover_herbaceous_percent@PERMANENT output=landcover_herbaceous_percent

r.resamp.stats input=landcover_trees_percent@PERMANENT output=landcover_trees_percent

r.resamp.stats input=topography_diversity@PERMANENT output=topography_diversity

r.resamp.stats input=topography_elevation@PERMANENT output=topography_elevation

r.resamp.stats input=topography_position@PERMANENT output=topography_position

r.resamp.stats input=topography_roughness@PERMANENT output=topography_roughness

r.resamp.stats input=topography_slope@PERMANENT output=topography_slope

r.resamp.stats input=topography_sun_hours_maximum@PERMANENT output=topography_sun_hours_maximum

r.resamp.stats input=topography_sun_hours_minimum@PERMANENT output=topography_sun_hours_minimum

r.resamp.stats input=topography_sun_hours_range@PERMANENT output=topography_sun_hours_range

r.resamp.stats input=topography_wetness@PERMANENT output=topography_wetness

r.resamp.stats input=geo_longitude@PERMANENT output=geo_longitude

r.resamp.stats input=geo_longitude@PERMANENT output=geo_longitude

r.resamp.stats input=ndvi_mean@PERMANENT output=ndvi_mean

r.resamp.stats input=ndvi_min@PERMANENT output=ndvi_min

r.resamp.stats input=ndvi_max@PERMANENT output=ndvi_max

r.resamp.stats input=ndvi_range@PERMANENT output=ndvi_range

#computing a comon mask
r.mapcalc "propagated_nulls = climate_PET_driest_quarter + climate_PET_seasonality + climate_PET_warmest_quarter + climate_PET_wettest_quarter + climate_annual_PET + climate_aridity_index + climate_aridity_thornthwaite + climate_bio1 + climate_bio12 + climate_bio14 + climate_bio15 + climate_bio17 + climate_bio18 + climate_bio4 + climate_bio5 + climate_bio6 + climate_bio7 + climate_continentality + climate_embergerQ + climate_growing_degree_days_0 + climate_growing_degree_days_5 + climate_max_temperature_coldest_month + climate_min_temperature_warmest_month + climate_moisture_index + climate_thermicity_index + climate_velocity_LGM + geo_latitude + geo_longitude + human_accesibility + human_footprint + human_population + human_road_density + landcover_bare_soil_percent + landcover_diversity + landcover_herbaceous_percent + landcover_trees_percent + ndvi_max + ndvi_mean + ndvi_min + ndvi_range + topography_diversity + topography_elevation + topography_position + topography_roughness + topography_slope + topography_sun_hours_maximum + topography_sun_hours_minimum + topography_sun_hours_range + topography_wetness" --overwrite

#to integer
r.mapcalc "propagated_nulls = int(propagated_nulls)" --overwrite
r.mapcalc "MASK = MASK" --overwrite

#creating mask
r.mask raster=propagated_nulls@ecoregions

#removing propagated_nulls
g.remove -f type=raster name=propagated_nulls@ecoregions

#aridity levels
#creating integer map
r.mapcalc "climate_aridity_index_integer = int(climate_aridity_index * 100)"

r.reclass input=climate_aridity_index_integer@ecoregions output=climate_aridity_index_levels rules=/media/workshop/GRASSDB/ENVIRONMENTAL_VARIABLES/ecoregions/.tmp/x-wing/12914.1

#rules
95 thru 100 = 1 hyperarid
80 thru 95 = 2 arid
50 thru 80= 3 semiarid
44 thru 50 = 4 dry subhumid
35 thru 44 = 5 humid
-900 thru 35 = 6 hyperhumid

#reclass table to map
r.mapcalc "climate_aridity_index_levels = int(climate_aridity_index_levels)" --overwrite

#managing categories AGAIN...
r.category map=climate_aridity_index_levels@ecoregions separator=comma rules=/media/workshop/GRASSDB/ENVIRONMENTAL_VARIABLES/ecoregions/.tmp/x-wing/12914.2
1,hyperarid
2,arid
3,semiarid
4,dry subhumid
5,humid
6,hyperhumid


#to vector
r.to.vect -v --overwrite input=climate_aridity_index_levels@ecoregions output=climate_aridity_index_levels type=area column=aridity_level

#simplifying the vector map
v.generalize --overwrite input=climate_aridity_index_levels@ecoregions type=area output=climate_aridity_index_levels_simplified method=sliding_averaging threshold=0.1

#rebuilding topology
v.build map=climate_aridity_index_levels_simplified@ecoregions

#export to postgis database
v.out.postgis input=climate_aridity_index_levels_simplified@ecoregions output=PG:dbname=flora_ecoregions output_layer=aridity_levels options='GEOMETRY_NAME=geom'


#exporting maps to tif
r.out.gdal input=climate_aridity_index_levels@ecoregions output=/media/workshop/geotif/climate_aridity_index_levels.tif format=GTiff

r.out.gdal input=climate_annual_PET@ecoregions output=/media/workshop/geotif/climate_annual_PET.tif format=GTiff

r.out.gdal input=climate_PET_driest_quarter@ecoregions output=/media/workshop/geotif/climate_PET_driest_quarter.tif format=GTiff

r.out.gdal input=climate_PET_seasonality@ecoregions output=/media/workshop/geotif/climate_PET_seasonality.tif format=GTiff

r.out.gdal input=climate_PET_warmest_quarter@ecoregions output=/media/workshop/geotif/climate_PET_warmest_quarter.tif format=GTiff

r.out.gdal input=climate_PET_wettest_quarter@ecoregions output=/media/workshop/geotif/climate_PET_wettest_quarter.tif format=GTiff

r.out.gdal input=climate_aridity_index@ecoregions output=/media/workshop/geotif/climate_aridity_index.tif format=GTiff

r.out.gdal input=climate_aridity_thornthwaite@ecoregions output=/media/workshop/geotif/climate_aridity_thornthwaite.tif format=GTiff

r.out.gdal input=climate_bio1@ecoregions output=/media/workshop/geotif/climate_bio1.tif format=GTiff

r.out.gdal input=climate_bio12@ecoregions output=/media/workshop/geotif/climate_bio12.tif format=GTiff

r.out.gdal input=climate_bio14@ecoregions output=/media/workshop/geotif/climate_bio14.tif format=GTiff

r.out.gdal input=climate_bio15@ecoregions output=/media/workshop/geotif/climate_bio15.tif format=GTiff

r.out.gdal input=climate_bio17@ecoregions output=/media/workshop/geotif/climate_bio17.tif format=GTiff

r.out.gdal input=climate_bio18@ecoregions output=/media/workshop/geotif/climate_bio18.tif format=GTiff

r.out.gdal input=climate_bio4@ecoregions output=/media/workshop/geotif/climate_bio4.tif format=GTiff

r.out.gdal input=climate_bio6@ecoregions output=/media/workshop/geotif/climate_bio6.tif format=GTiff

r.out.gdal input=climate_bio7@ecoregions output=/media/workshop/geotif/climate_bio7.tif format=GTiff

r.out.gdal input=climate_continentality@ecoregions output=/media/workshop/geotif/climate_continentality.tif format=GTiff

r.out.gdal input=climate_embergerQ@ecoregions output=/media/workshop/geotif/climate_embergerQ.tif format=GTiff

r.out.gdal input=climate_growing_degree_days_0@ecoregions output=/media/workshop/geotif/climate_growing_degree_days_0.tif format=GTiff

r.out.gdal input=climate_growing_degree_days_5@ecoregions output=/media/workshop/geotif/climate_growing_degree_days_5.tif format=GTiff

r.out.gdal input=climate_max_temperature_coldest_month@ecoregions output=/media/workshop/geotif/climate_max_temperature_coldest_month.tif format=GTiff

r.out.gdal input=climate_min_temperature_warmest_month@ecoregions output=/media/workshop/geotif/climate_min_temperature_warmest_month.tif format=GTiff

r.out.gdal input=climate_moisture_index@ecoregions output=/media/workshop/geotif/climate_moisture_index.tif format=GTiff

r.out.gdal input=climate_thermicity_index@ecoregions output=/media/workshop/geotif/climate_thermicity_index.tif format=GTiff

r.out.gdal input=climate_velocity_LGM@ecoregions output=/media/workshop/geotif/climate_velocity_LGM.tif format=GTiff

r.out.gdal input=geo_latitude@ecoregions output=/media/workshop/geotif/geo_latitude.tif format=GTiff

r.out.gdal input=geo_longitude@ecoregions output=/media/workshop/geotif/geo_longitude.tif format=GTiff

r.out.gdal input=human_accesibility@ecoregions output=/media/workshop/geotif/human_accesibility.tif format=GTiff

r.out.gdal input=human_footprint@ecoregions output=/media/workshop/geotif/human_footprint.tif format=GTiff

r.out.gdal input=human_population@ecoregions output=/media/workshop/geotif/human_population.tif format=GTiff

r.out.gdal input=human_road_density@ecoregions output=/media/workshop/geotif/human_road_density.tif format=GTiff

r.out.gdal input=landcover_bare_soil_percent@ecoregions output=/media/workshop/geotif/landcover_bare_soil_percent.tif format=GTiff

r.out.gdal input=landcover_diversity@ecoregions output=/media/workshop/geotif/landcover_diversity.tif format=GTiff

r.out.gdal input=landcover_herbaceous_percent@ecoregions output=/media/workshop/geotif/landcover_herbaceous_percent.tif format=GTiff

r.out.gdal input=landcover_trees_percent@ecoregions output=/media/workshop/geotif/landcover_trees_percent.tif format=GTiff

r.out.gdal input=ndvi_max@ecoregions output=/media/workshop/geotif/ndvi_max.tif format=GTiff

r.out.gdal input=ndvi_mean@ecoregions output=/media/workshop/geotif/ndvi_mean.tif format=GTiff

r.out.gdal input=ndvi_min@ecoregions output=/media/workshop/geotif/ndvi_min.tif format=GTiff

r.out.gdal input=ndvi_range@ecoregions output=/media/workshop/geotif/ndvi_range.tif format=GTiff

r.out.gdal input=topography_diversity@ecoregions output=/media/workshop/geotif/topography_diversity.tif format=GTiff

r.out.gdal input=topography_elevation@ecoregions output=/media/workshop/geotif/topography_elevation.tif format=GTiff

r.out.gdal input=topography_position@ecoregions output=/media/workshop/geotif/topography_position.tif format=GTiff

r.out.gdal input=topography_roughness@ecoregions output=/media/workshop/geotif/topography_roughness.tif format=GTiff

r.out.gdal input=topography_slope@ecoregions output=/media/workshop/geotif/topography_slope.tif format=GTiff

r.out.gdal input=topography_sun_hours_maximum@ecoregions output=/media/workshop/geotif/topography_sun_hours_maximum.tif format=GTiff

r.out.gdal input=topography_sun_hours_minimum@ecoregions output=/media/workshop/geotif/topography_sun_hours_minimum.tif format=GTiff

r.out.gdal input=topography_sun_hours_range@ecoregions output=/media/workshop/geotif/topography_sun_hours_range.tif format=GTiff

r.out.gdal input=topography_wetness@ecoregions output=/media/workshop/geotif/topography_wetness.tif format=GTiff
