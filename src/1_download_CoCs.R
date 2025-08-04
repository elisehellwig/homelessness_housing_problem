#' This script downloads Continuum of Care data from HUD. To download the 
#' geometries of all CoCs for a particular year use 1_download_CoCs_national.sh.
#' To download the Point in Time count data for a year or set of years, use
#' 1_download_CoCs_timeseries.sh. 

#Example files
# https://files.hudexchange.info/reports/published/CoC_GIS_State_Shapefile_CA_2019.zip
# https://www.hudexchange.info/resources/documents/CoC_GIS_National_Boundary_2019.zip


# Parameters --------------------------------------------------------------

args = commandArgs(trailingOnly = TRUE)

yr = as.integer(args[1])

download_national = as.logical(args[2])

download_state = as.logical(args[3])

msg = paste0('Downloading data for ', yr, '.')
message(msg)

# Setup -------------------------------------------------------------------

if (download_state) library('terra')

spatial_path = 'data/spatial/'

data_path = file.path(spatial_path, yr)

if (!dir.exists(data_path)) {
  dir.create(data_path, recursive = TRUE)
}

# Functions ---------------------------------------------------------------

rsleep = function(min=1, max=60) {
  duration = runif(1, min, max)
  message(paste('Sleeping for', round(duration), 'seconds.'))
  Sys.sleep(duration)
  
  invisible()
}

get_national_polygons = function(year, path) {
  
  url = paste0('https://files.hudexchange.info/resources/documents/',
               'CoC_GIS_National_Boundary_', year, '.zip')
  
  dest_file = paste0('national_CoC_polygons_', year, '.zip')
  dest_path = file.path(path, dest_file)
  
  download.file(url, dest_path)
  
  unzip(dest_path, exdir = path)
  
  rsleep()
  
}

get_state_data = function(state, year, path) {
  
  url = paste0('https://files.hudexchange.info/reports/published/',
               'CoC_GIS_State_Shapefile_',  state, '_', year, '.zip')
  
  dest_file = paste0('CoC_data_', state, '_', year ,'.zip')
  dest_path = file.path(path, dest_file)
  
  download.file(url, dest_path)
  
  unzip(dest_path, exdir = path)
  
  unlink(dest_path)
  
  rsleep()
  
}


read_in_state_cocs = function(fns) {
  
  coc_list = lapply(shape_fns, vect)
  
  coc = do.call(rbind, coc_list)
  
  names(coc) = tolower(names(coc))
  
  cols_to_remove = c('st', 'state_name', 'cocname', 'shape_leng', 'shape_area')
  
  for (col in cols_to_remove) {
    if (col %in% names(coc)) {
      coc[, col] = NULL
    }
  }
  
  return(coc)
  
  
}


# National CoCs -----------------------------------------------------------

if (download_national) {
  get_national_polygons(yr, data_path)
  
}


# Get State CoCs ----------------------------------------------------------


if (download_state) {
  sapply(state.abb, get_state_data, year=yr, path=data_path)
  
  shape_fns = list.files(data_path, pattern='\\.shp$', recursive=TRUE, 
                         full.names=TRUE)
  
  cocs = read_in_state_cocs(shape_fns)
  
  cocs_df = as.data.frame(cocs)
  
  out_fn = file.path('data/CoCs/', paste0('coc_data_', yr, '.csv'))
  
  write.csv(cocs_df, out_fn, row.names = FALSE)
  
}



