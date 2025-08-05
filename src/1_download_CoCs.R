#' This script downloads Continuum of Care data from HUD. To download the 
#' geometries of all CoCs for a particular year use 1_download_CoCs_national.sh.
#' To download the Point in Time count data for a year or set of years, use
#' 1_download_CoCs_timeseries.sh. Requires terra package

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

spatial_path = 'data/spatial/'

data_path = file.path(spatial_path, yr)

if (!dir.exists(data_path)) {
  dir.create(data_path, recursive = TRUE)
}

# Functions ---------------------------------------------------------------

rsleep = function(min=1, max=25) {
  duration = runif(1, min, max)
  message(paste('Sleeping for', round(duration), 'seconds.'))
  Sys.sleep(duration)
  
  invisible()
}

download_and_delete = function(url, fn, ex_path) {
  download.file(url, fn)
  rsleep()
  
  unzip(fn, exdir=ex_path)
  
  unlink(fn)
}

get_national_polygons = function(geom, year, path) {
  
  url = paste0('https://files.hudexchange.info/resources/documents/',
               'CoC_GIS_National_Boundary_', year, '.zip')
  
  dest_file = paste0('national_CoC_polygons_', year, '.zip')
  dest_path = file.path(path, dest_file)
  
  download_and_delete(url, dest_path, path)
  
}


get_state_data = function(geom, year, path) {
  
  url = paste0('https://files.hudexchange.info/reports/published/',
               'CoC_GIS_State_Shapefile_',  geom, '_', year, '.zip')
  
  dest_file = paste0('CoC_data_', geom, '_', year ,'.zip')
  dest_path = file.path(path, dest_file)
  
  unzipped_dir = file.path(path, geom)
  
  if (!dir.exists(unzipped_dir)) {
    dir.create(unzipped_dir, recursive=TRUE)
    download_and_delete(url, dest_path, unzipped_dir)
    
  }
  
  
}

get_hud_data = function(geom, FUN, year, path, error_file) {

  #downloads and saves file to folder on computer
  tryCatch(
    { 
      do.call(FUN, list(geom=geom, year=year, path=path))
      #message("Downloaded: ", file_name)
    }, 
    
    error = function(e) {
      df <- data.frame(geom = geom,
                       year = year,
                       path = path,
                       error = as.character(e))
      
      write.table(df, 
                  error_file, 
                  sep=',', 
                  row.names=FALSE, 
                  col.names = FALSE, 
                  append=TRUE)
      
    })
  
  
}


fix_names = function(coc) {
  
  count_id = which(names(coc) == 'sh_pers_hw')
  
  if (length(count_id)>0) {
    count_id = count_id[1]
    
    fixed_names = paste(
      rep(c('sh', 'unsh'), 3),
      'pers',
      rep(c('hwoa', 'hwoc', 'hwac'), each=2),
      sep='_'
    )
    
    names(coc)[count_id:(count_id+5)] = fixed_names
    
  }
  
  return(coc)
}


read_in_state_cocs = function(fns) {
  require('terra')
  
  coc_list = lapply(shape_fns, vect)
  
  coc = do.call(rbind, coc_list)
  
  names(coc) = tolower(names(coc))
  
  if (ncol(coc)>38) {
    coc = fix_names(coc)
    
  }
  
  cols_to_remove = c('st', 'state_name', 'cocname', 'shape_leng', 'shape_area')
  
  for (col in cols_to_remove) {
    if (col %in% names(coc)) {
      coc[, col] = NULL
    }
  }
  
  return(coc)
  
  
}


# error file --------------------------------------------------------------

error_fn = 'logs/coc_errors.csv'

df = data.frame(geom = character(), 
                year = integer(),
                path = character(),
                error = character())

if (!file.exists(error_fn)) write.csv(df, error_fn, row.names = FALSE)

# National CoCs -----------------------------------------------------------

if (download_national) {
  
  get_hud_data('USA', 'get_national_polygons', yr, data_path, error_fn)

}


# Get State CoCs ----------------------------------------------------------


if (download_state) {

  lapply(state.abb, get_hud_data, 'get_state_data', yr, data_path, error_fn)
  
  shape_fns = list.files(data_path, pattern='\\.shp$', recursive=TRUE, 
                         full.names=TRUE)
  
  cocs = read_in_state_cocs(shape_fns)
  
  cocs_df = as.data.frame(cocs)
  
  out_fn = file.path('data/CoCs/', paste0('coc_data_', yr, '.csv'))
  
  write.csv(cocs_df, out_fn, row.names = FALSE)
  
}



