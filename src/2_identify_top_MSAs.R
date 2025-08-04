#' This script downloads geometries for all of the metro/micropolitan
#' statistical areas (MSAs) in the US and then selects the top 35 by population.
#' Six of the 35 are then removed because the CoC they are a part of contains a
#' much larger population than the MSA. This replicates the methodology
#' described by Colburn and Aldern (2022) on pg 18


#https://en.wikipedia.org/wiki/Metropolitan_statistical_area
#http://hudexchange.info/programs/coc/gis-tools/


# Setup -------------------------------------------------------------------

library('sf')
library('tidycensus')

#run in console
#census_api_key('YOUR KEY HERE', install=TRUE)

options(tigris_use_cache = TRUE)

#vars = load_variables(2019, dataset='acs5', cache=TRUE)



# Get Top MSA geographies -------------------------------------------------

#B01001_001

#Total Population for all the MSAs in 2019 according to the 5 year ACS 2019 
# estimates
msa_pop = get_acs(geography = 'cbsa',
                  variables = 'B01001_001',
                  year=2019,
                  geometry=TRUE)

#sort msas by pop (decreasing)
msa_pop = msa_pop[order(msa_pop$estimate, decreasing = TRUE),]

#Top 35 MSAs by population
msa_pop_top = msa_pop[1:35,] 

# excluded due to the fact that the CoC covered an area that was much larger
# than the MSA: Kansas City, Denver, Riverside, Houston, Orlando, Pittsburgh
excluded = c(28140, 19740, 40140, 26420, 36740, 38300)

target_msas = msa_pop_top[!msa_pop_top$GEOID %in% excluded,]

names(target_msas)[4] = 'total_pop'

target_msas$name = gsub(' Metro Area', '', target_msas$NAME)

target_msas = target_msas[, c('GEOID', 'name', 'total_pop', 'geometry')]

st_write(target_msas, 'data/spatial/target_MSAs.geojson', delete_dsn=TRUE)

