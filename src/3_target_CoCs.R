
# Description -------------------------------------------------------------

#' This script identifies the "primary" CoCs for each of the top MSAs identified
#' in src/2_identify_top_MSAs.R. It attempts to mimic the methodology on Colburn
#' and Aldern (2022), on pages 18-19. However, they do not specify what they
#' mean by primary, so it is an approximation. This script ___

# Setup -------------------------------------------------------------------

#https://en.wikipedia.org/wiki/Metropolitan_statistical_area
#http://hudexchange.info/programs/coc/gis-tools/

library('sf')
library('tidycensus')
library('units')
library('data.table')


# Functions ---------------------------------------------------------------

is_max = function(v) {
  max_id = which.max(v)
  
  is_max_vec = 1:length(v) == max_id
  
  return(is_max_vec)
}


# Format CoC Geometries ---------------------------------------------------

# downloaded from 
# https://www.hudexchange.info/resources/documents/CoC_GIS_National_Boundary_2019.zip

all_cocs = st_read('data/spatial/2019/FY19_CoC_National_Bnd.gdb')

all_cocs = st_make_valid(all_cocs)

all_cocs = st_set_geometry(all_cocs, 'geometry')

names(all_cocs) = tolower(names(all_cocs))

all_cocs = all_cocs[, c('cocnum', 'cocname')]

all_cocs$ST = substr(all_cocs$cocnum, 1, 2)



# Format CoC Data ---------------------------------------------------------

coc_2019 = fread('data/CoCs/coc_data_2019.csv')

coc_2019[, tot_unsh := unsh_pers_hwoa + unsh_pers_hwoc + unsh_pers_hwac]

coc_2019[, tot_sh := sh_pers_hwoa + sh_pers_hwoc + sh_pers_hwac]

coc_2019[, tot_uh := tot_unsh + tot_sh]

coc_2019 = coc_2019[, .(cocnum, fprn, tot_uh, tot_sh, tot_unsh)]


cocs = merge(all_cocs, coc_2019, by='cocnum')

cocs = st_transform(cocs, 5070)

cocs$coc_area = st_area(cocs, by_element=TRUE) |> 
  set_units(km^2) |>
  as.numeric()

# Get overlapping CoCs ----------------------------------------------------

target_msas = st_read('data/spatial/target_MSAs.geojson')

target_msas = st_transform(target_msas, 5070)

msa_cocs0 = st_intersection(target_msas, cocs)

msa_cocs0$area = st_area(msa_cocs0, by_element=TRUE) |> 
  set_units(km^2) |>
  as.numeric()



# Identify primary cocs ---------------------------------------------------

msa_cocs1 = st_drop_geometry(msa_cocs0)
setDT(msa_cocs1)

msa_cocs2 = msa_cocs1[area > coc_area/2]

setorder(msa_cocs2, GEOID, -tot_uh)

msa_cocs2[, ":="(is_max_uh = is_max(tot_uh),
                 is_max_fprn = is_max(fprn)), by=.(GEOID, name)]

msa_cocs2[, max_count := is_max_uh + is_max_fprn]

target_cocs = msa_cocs2[(is_max_fprn) | cocnum == 'IL-511']

fwrite(target_cocs, 'data/CoCs/target_cocs.csv')

