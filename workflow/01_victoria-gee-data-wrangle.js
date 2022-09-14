////////// VICTORIA'S CODE FOR WRANGLING DATA TO USE FOR SITE SELECTION
////////// OUTPUTS FILE: fires_stats_20210414.geojson

////// COPIED IMPORTS FROM GEE
var mtbs_perims_DD = ee.FeatureCollection("users/Ginikanda/mtbs_perims_DD"),
    tile_prj_wgs_tile_12_GEDI_2A = ee.Image("users/Ginikanda/tile_prj_wgs_tile_12_GEDI_2A"),
    tile_prj_wgs_tile_7_GEDI_2A = ee.Image("users/Ginikanda/tile_prj_wgs_tile_7_GEDI_2A"),
    tile_prj_wgs_tile_8_GEDI_2A = ee.Image("users/Ginikanda/tile_prj_wgs_tile_8_GEDI_2A"),
    tile_prj_wgs_tile_13_GEDI_2A = ee.Image("users/Ginikanda/tile_prj_wgs_tile_13_GEDI_2A"),
    states = ee.FeatureCollection("TIGER/2016/States"),
    mtbs_fires = ee.FeatureCollection("users/visc2397/mtbs_perimeters_1984-2017"),
    land_management = ee.FeatureCollection("users/visc2397/blm_co_sma_20190520_valid"),
    gaplf2011 = ee.Image("USGS/GAP/CONUS/2011"),
    fired_events = ee.FeatureCollection("projects/cires-gg-earthlab/events_polys_w_stats_lc"),
    neon_aop_all = ee.FeatureCollection("users/visc2397/AOP_flightboxesAllSites");





////////// START OF GEE CODE


////////// STUDY AREA OF INTEREST  ------------------------------


// Filter the US states
var colorado = states.filter(ee.Filter.eq('NAME', 'Colorado'));
// Display CO
var shown = true; // true or false, 1 or 0 
var opacity = 0.2; // number [0-1]
var nameLayer = 'CO'; // string
var visParams = {color: 'black'}; // dictionary: 
//Map.addLayer(colorado, visParams, nameLayer, shown, opacity);



// Buffer a point with a 200 km radius
//ee.Geometry.Point([-105.547, 40.03287])    // NEON Niwot Ridge tower location
var neon_site_buffer =
ee.Geometry.Point([-105.2705, 40.0150])      // Boulder, CO 
.buffer(200000);



// Declare study area to clip other layers
var study_area = neon_site_buffer;


// Add study area polygon feature(s) to the map
Map.addLayer(study_area, {color: '595A59', fillColor: '00000000'}, 
                "Study Area", true, 0.1);
// Center the map on the study area
Map.centerObject(study_area, 11);



////////// LAND COVER ---------------------------------

// Clip land cover data to the study area
var gaplf = gaplf2011
.clip(study_area)

// USGS GAP/LANDFIRE National Terrestrial Ecosystems data 2011
// represents a detailed vegetation and land cover classification
// with 30x30m spatial resolution
var lodgepole = gaplf.eq(149).or(gaplf.eq(150)).rename("lodgepole");
var ponderosa = gaplf.eq(139).or(gaplf.eq(141)).or(gaplf.eq(157)).or(gaplf.eq(158)).rename("ponderosa");
var spruceFir = gaplf.eq(151).or(gaplf.eq(152)).rename("spruceFir");
var dstrb_brnd = gaplf.eq(570).or(gaplf.eq(573)).rename("disturbed_burned");
var dstrb_unsp = gaplf.eq(565).rename("disturbed_unspecific");
var dstrb_logd = gaplf.eq(566).rename("disturbed_logged");
var regen_harv = gaplf.eq(567).or(gaplf.eq(568)).or(gaplf.eq(569)).rename("regenerating_harvested");
var regen_dist = gaplf.eq(574).or(gaplf.eq(575)).or(gaplf.eq(576)).rename("regenerating_disturbed");
// lodgepoole, ponderosa pine, and spruceFir combined
var forest_cover = gaplf.eq(149).or(gaplf.eq(150)).or(gaplf.eq(139))
      .or(gaplf.eq(141)).or(gaplf.eq(157)).or(gaplf.eq(158)).or(gaplf.eq(151)).or(gaplf.eq(152))

Map.addLayer(lodgepole.updateMask(lodgepole), {palette: ["04663B"]}, "Lodgepole Pine");
Map.addLayer(ponderosa.updateMask(ponderosa), {palette: ["41ab5d"]}, "Ponderosa Pine");
Map.addLayer(spruceFir.updateMask(spruceFir), {palette: ["c7e9c0"]}, "Spruce/Fir");
Map.addLayer(forest_cover.updateMask(forest_cover), {palette: ["fb8072"]}, "Forest cover");

Map.addLayer(dstrb_brnd.updateMask(dstrb_brnd), {palette: ["ff0000"]}, "Disturbed, recently burned");
//Map.addLayer(dstrb_logd.updateMask(dstrb_logd), {palette: ["ffff00"]}, "Disturbed, logged");
//Map.addLayer(dstrb_unsp.updateMask(dstrb_unsp), {palette: ["ffff00"]}, "Disturbed, unspecific");
//Map.addLayer(regen_harv.updateMask(regen_harv), {palette: ["ff00ff"]}, "Regenerating, harvested");
Map.addLayer(regen_dist.updateMask(regen_dist), {palette: ["ff00ff"]}, "Regenerating, disturbed, unspecific");



////////// LAND OWNERSHIP -----------------------------------

// USFS land
var land_USFS = land_management
.filter(ee.Filter.eq('adm_manage', "USFS"))
// within the study region
.filterBounds(study_area);
Map.addLayer(land_USFS, {color: 'b3e2cd'}, "USFS land", true, 1);


// BLM land
var land_BLM = land_management
.filter(ee.Filter.eq('adm_manage', "BLM"))
.filterBounds(study_area);
Map.addLayer(land_BLM, {color: 'fdcdac'}, 'BLM Land', false, 1);


// NPS land
var land_NPS = land_management
.filter(ee.Filter.eq('adm_code', "NPS"))
.filterBounds(study_area);
Map.addLayer(land_NPS, {color: 'f4cae4'}, 'NPS Land', false, 1);


// Private land
var land_PRI = land_management
.filter(ee.Filter.eq('adm_code', "PRI"))
.filterBounds(study_area);
Map.addLayer(land_PRI, {color: 'cbd5e8'}, 'PRI Land', false, 1);


////////// VECTOR TO IMAGE? 

// Convert each management agency polygon to an image
// to add it to the layers 





////////// GEDI DATA -----------------------------------------
// values are max canopy height (RH 98)

// How to add these rasters as GEE imports, from Nayani: 
//    var tile_prj_wgs_tile_13_GEDI_2A = ee.Image("users/Ginikanda/tile_prj_wgs_tile_13_GEDI_2A") ;


// // tile_prj_wgs_tile_13_GEDI_2A
// Map.addLayer(
//   tile_prj_wgs_tile_13_GEDI_2A,
//   {min: 1, // minimum range
//   max: 25, // maximum range 
//   palette: [
//     'lightgreen', 'yellow', 'red'],},
//   "tile_prj_wgs_tile_13_GEDI_2A"); // layer name
// 
// // tile_prj_wgs_tile_12_GEDI_2A
// Map.addLayer(
//   tile_prj_wgs_tile_12_GEDI_2A,
//   {min: 1, // minimum range
//   max: 25, // maximum range 
//   palette: [
//     'lightgreen', 'yellow', 'red'],},
//   "tile_prj_wgs_tile_12_GEDI_2A"); // layer name
  
// // tile_prj_wgs_tile_7_GEDI_2A
// Map.addLayer(
//   tile_prj_wgs_tile_7_GEDI_2A,
//   {min: 1, // minimum range
//   max: 25, // maximum range 
//   palette: [
//     'lightgreen', 'yellow', 'red'],},
//   "tile_prj_wgs_tile_7_GEDI_2A"); // layer name
  
// // tile_prj_wgs_tile_8_GEDI_2A
// Map.addLayer(
//   tile_prj_wgs_tile_8_GEDI_2A,
//   {min: 1, // minimum range
//   max: 25, // maximum range 
//   palette: [
//     'lightgreen', 'yellow', 'red'],},
//   "tile_prj_wgs_tile_8_GEDI_2A"); // layer name


var gedi_local = tile_prj_wgs_tile_13_GEDI_2A
.clip(study_area)

Map.addLayer(
  gedi_local,
  {min: 1, // minimum range
  max: 25, // maximum range 
  palette: [ // set color palette
//    'ece2f0', '67a9cf', '014636'],}, // PuBuGn
    'fdd49e', 'fdbb84', 'fc8d59','ef6548'],}, // OrRd
  "gedi_local"); // layer name


// Create binary layer where 1 = GEDI pixel height greater than zero
var gedi_coverage = gedi_local.gt(0.0).unmask().rename("gedi_coverage");
Map.addLayer(
  gedi_coverage,
  {color: 'fdb462'}, 
  "gedi_coverage", true, 1); // layer name

  
////////// MTBS FIRE EVENTS -----------------------------------
// Fire data from 1984 - 2018

// Filter for MTBS events in study area 
var mtbs_local = mtbs_fires
.filterBounds(study_area)
 // only keep fires with some USFS land coverage 
.filterBounds(land_USFS);

// Add MTBS perimeters to the map
Map.addLayer(mtbs_local, {color: 'black'}, "MTBS events", shown, 0.4);

// Count the # of fires meeting all criteria
print('MTBS events in study area:', mtbs_local.size());


////////// FIRED EVENTS ----------------------------------------
// Fire data from January 2001 to May 2019

var fired_local = fired_events
.filterBounds(study_area)
 // only keep fires with some USFS land coverage 
.filterBounds(land_USFS);

// Add MTBS perimeters to the map
Map.addLayer(fired_local, {color: 'blue'}, "FIRED events", shown, 0.4);

// Count the # of fires meeting all criteria
print('FIRED events in study area:', fired_local.size());



////////// NEON AOP FLIGHT BOUNDARIES ------------------------

// Downloaded from https://www.neonscience.org/data-samples/data/spatial-data-maps
// Files last updated May 2020.

var neon_aop = neon_aop_all
.filterBounds(study_area)
.filter(ee.Filter.eq('siteID', "NIWO"))

// Add to the map
Map.addLayer(neon_aop, {color: 'yellow'}, "NEON AOP NIWO", shown, 0.4);

// Add a point for the Mountain Research Station location
var mrs_point = ee.Geometry.Point([-105.531041, 40.029308]);
Map.addLayer(mrs_point, {color: 'black'}, "MRS Location", shown);




////////// SUMMARY TABLE ----------------------------------------

// Combine Bands together into a single Image
var layers_to_reduce = lodgepole
.addBands(ponderosa)
.addBands(spruceFir)
.addBands(gedi_coverage)
.addBands(dstrb_brnd)
.addBands(dstrb_unsp);
//.addBands(dstrb_logd)
//.addBands(regen_harv)
//.addBands(regen_dist);


// How to calculate % cover using other polygons???? 
// .addBands(land_USFS)
// .addBands(land_BLM)
// .addBands(land_NPS)
// .addBands(land_PRI)

print('layers_to_reduce', layers_to_reduce);

// Use reduceRegions functions to calculate % of each 
// forest layer within each fire perimeter
var fire_stats = layers_to_reduce.reduceRegions({
  reducer: ee.Reducer.mean(),
  collection: mtbs_local 
})

print('Table describing layer percentage within EACH fire', fire_stats)


// Export table to Google Drive 
Export.table.toDrive({
  collection: fire_stats,
  description: 'fires_stats_20210414',
  fileFormat: 'GeoJSON'
});