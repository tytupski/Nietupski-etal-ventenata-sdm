/*
Author: Ty Nietupski

Code in support of the manuscript titled "An
  exploration of current and future 
  niche-based invasion projections and
  uncertainty for an invasive grass in the
  western US"

Objective: Calculate norms from nex-dcp30 data
*/

///////////////////////////////////////////////////////////////////////////////////////////////////
//            functions
///////////////////////////////////////////////////////////////////////////////////////////////////
//{
  
// days per month for conversion to mm/month from kg/(m^2*s)
// 1. first, convert to mm/day (mulitply values by 86400), 2. then, mm/month (multiply by days in month)
var daysPerMonth = ee.Dictionary({
  '1': 31,
  '2': 28,
  '3': 31, 
  '4': 30,
  '5': 31,
  '6': 30,
  '7': 31,
  '8': 31,
  '9': 30, 
  '10': 31,
  '11': 30, 
  '12': 31
})

// function to convert the units of all bands of the nex-dcp30 collection
var convertUnits = function(image) {
  // get the month from the image metadata
  var month = image.get('month')
  // get the days in this month from my dictionary
  var days_per_month = daysPerMonth.get(ee.String(ee.Number.parse(month).int()))
  // convert kg/(m^2*s) to mm/month
  var ppt_per_month_mm = image
    .select('pr')
    .multiply(ee.Image.constant(86400))
    .multiply(ee.Image.constant(days_per_month))
    .rename('ppt')
  // convert kelvin to celsius
  var temp_celsius = image
    .select(['tasmax', 'tasmin'])
    .subtract(ee.Image.constant(273.15))
    .rename(['tmax', 'tmin'])
  
  // return only converted bands
  return image
    .addBands(ppt_per_month_mm)
    .addBands(temp_celsius)
    .select(['tmax', 'tmin', 'ppt'])
}

//}
///////////////////////////////////////////////////////////////////////////////////////////////////
//            globals
///////////////////////////////////////////////////////////////////////////////////////////////////
//{

// NEX-DCP30 data set
var dcp30 = ee.ImageCollection('NASA/NEX-DCP30')

// // just to look at the structure and content of the metadata
// var test =  dcp30
//   .filter(ee.Filter.eq('model', 'inmcm4'))
//   .filter(ee.Filter.eq('scenario', 'rcp85'))
//   .filterDate('2012-01-01', '2012-10-01')
//   .map(convertUnits)
// print(test)
// Map.addLayer(test.first(), {}, 'test', false)

// gcms of interest
var gcms = dcp30
  .aggregate_array('model')
  .distinct()
  .remove('BNU-ESM') // some kind of issue we should drop it for
  .remove('GISS-E2-H-CC')
  .remove('GISS-E2-R-CC')
print(gcms)
// var gcms = ee.List(['ACCESS1-0','CCSM4','CESM1-BGC','CESM1-CAM5','CMCC-CM','CNRM-CM5',
//                     'CSIRO-Mk3-6-0','CanESM2','FGOALS-g2','FIO-ESM','GFDL-CM3','GFDL-ESM2G',
//                     'GFDL-ESM2M','GISS-E2-R','HadGEM2-AO','HadGEM2-CC','HadGEM2-ES','IPSL-CM5A-LR',
//                     'IPSL-CM5A-MR','IPSL-CM5B-LR','MIROC-ESM-CHEM','MIROC-ESM','MIROC5','MPI-ESM-LR',
//                     'MPI-ESM-MR','MRI-CGCM3','NorESM1-M','bcc-csm1-1-m','bcc-csm1-1','inmcm4'])

// month sequence (water year)
var months = ee.List([10, 11, 12, 1, 2, 3, 4, 5, 6, 7, 8, 9])


////////// Variables for exporting //////////
// us west of -104 degrees
var geo = ee.Geometry.Rectangle([-125.0166667, 29.2333333333, -104.0, 49.0916666667], 'EPSG:4326')

var out_crs = 'EPSG:4326'
// original trasform was [0.00833333333, 0, -125.02083333, 0, 0.00833333333, 24.0625]
// transform of other files I have
var out_transform = [0.008333333333333333, 0.0, -125.02083333333333,
                    0.0, -0.008333333333333331, 49.09583333333333]
                    
// new names for climate variables
var new_names = ee.List(['ppt', 'tmax', 'tmin'])

//}
///////////////////////////////////////////////////////////////////////////////////////////////////
//            calculate norms
///////////////////////////////////////////////////////////////////////////////////////////////////
//{

// return list of 3 gcms, each with a 12 image image collection of the 30-yr norm data for tmin, tmax
var future = gcms.map(function(gcm) {
  var img_list =  months
      .map(function(m) {
        var col = dcp30
          .filter(ee.Filter.eq('model', gcm))
          .filter(ee.Filter.eq('scenario', 'rcp85'))
          .filterDate('2069-10-01', '2099-10-01')
          .filter(ee.Filter.eq('month', m))
          .map(convertUnits)
          .mean()
        return col
      })
  return ee.ImageCollection.fromImages(img_list)
})

// return list of 3 gcms, each with a 12 image image collection of the 30-yr norm data for tmin, tmax
var current = gcms.map(function(gcm) {
  var img_list =  months
      .map(function(m) {
        var col1 = dcp30
          .filter(ee.Filter.eq('model', gcm))
          .filter(ee.Filter.eq('scenario', 'rcp85'))
          .filterDate('2006-01-01', '2012-10-01')
          .filter(ee.Filter.eq('month', m))
          .map(convertUnits)
        var col2 = dcp30
          .filter(ee.Filter.eq('model', gcm))
          .filter(ee.Filter.eq('scenario', 'historical'))
          .filterDate('1982-10-01', '2006-01-01')
          .filter(ee.Filter.eq('month', m))
          .map(convertUnits)
        return col1
          .merge(col2)
          .mean()
          .set({'month': m, 'model': gcm})
      })
  return ee.ImageCollection.fromImages(img_list)
})


// print out some of the metadata
print('current', current)
// print('future', future)
print('Norm data', ee.ImageCollection(current.get(0)))

// add example norm data to map for ACCESS1-0
Map.addLayer(ee.ImageCollection(current.get(0)).select('ppt').toBands(), {}, '1982-2012 norm ppt (oct-sept)',  false)
Map.addLayer(ee.ImageCollection(future.get(0)).select('ppt').toBands(), {}, '2069-2099 norm ppt (oct-sept)', false)

//}
///////////////////////////////////////////////////////////////////////////////////////////////////
//            reformat and export to drive
///////////////////////////////////////////////////////////////////////////////////////////////////
//{

// the following exports 1 image for each variable (tmin,tmax) 
// by gcm ('GFDL-CM3', 'GFDL-ESM2G', 'GFDL-ESM2M')
// by time period (future, current) combination

// loop through gcms
for(var i = 0; i < gcms.size().getInfo(); i++){
  // get the name of the gcm
  var gcm = gcms.get(i)
  // get the current and future norm data for this gcm
  var fut_col = ee.ImageCollection(future.get(i))
  var current_col = ee.ImageCollection(current.get(i))
  
  // loop through variables (tmin, tmax)
  for(var j = 0; j < new_names.size().getInfo(); j++){
    // names for bands of each exported image
    var month_names = ee.List(['Oct', 'Nov', 'Dec', 'Jan', 
                              'Feb', 'Mar', 'Apr', 'May',
                              'Jun', 'Jul', 'Aug', 'Sept'])
    // name of the variable we're going to export (for name of tiff)
    var var_name = ee.String(new_names.get(j))
    
    // convert collections to images and rename bands
    var fut_img = fut_col
      .select(var_name)
      .toBands()
      .rename(month_names)
    var current_img = current_col
      .select(var_name)
      .toBands()
      .rename(month_names)

    // export future image
    Export.image.toDrive({
      image: fut_img.unmask(-9999),
      description: ee.String(gcm).cat('_').cat(var_name).cat('_norm_2069_2099').getInfo(),
      folder: 'nexdcp30',
      region: geo,
      crs: out_crs,
      crsTransform: out_transform
    })
    
    // export current image
    Export.image.toDrive({
      image: current_img.unmask(-9999),
      description: ee.String(gcm).cat('_').cat(var_name).cat('_norm_1982_2012').getInfo(),
      folder: 'nexdcp30',
      region: geo,
      crs: out_crs,
      crsTransform: out_transform
    })
  }
}

//}
