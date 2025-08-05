library('tidycensus')
library('data.table')

#https://data.bls.gov/registrationEngine/
#Sys.setenv(BLS_API_KEY='YOUR_KEY_HERE')
api_key = Sys.getenv('BLS_API_KEY')

get_philly_employment_ACS = function(yr, survey) {
  
  employ = get_acs(geography = 'county',
                   #variables = c('B23025_003', 'B23025_005'),
                   table = 'B23025',
                   year = yr,
                   state = '42', #PA
                   county = '101', #Philadelphia
                   survey = survey)
  
  return(employ)
  
}

get_unemployment_BLS = function(id, annual=FALSE, 
                                fn='data/unemployment/county.txt') {
  cnty_unemp = fread(fn)
  
  target_unemp = cnty_unemp[series_id==id]
  
  target_unemp[value=='-', value:='0']
  
  target_unemp[, unemployment:=as.numeric(value)/100]
  
  if (annual) {
    final_unemp = target_unemp[period == 'M13', .(year, unemployment)]
    

  } else {
    target_unemp = target_unemp[period != 'M13']
    
    target_unemp[, date:=paste(year, substr(period, 2, 3), '01', sep='-')]
    
    target_unemp[, date:=as.Date(date)]
    
    final_unemp = target_unemp[, .(date, year, unemployment)]
    
  }
  
  return(final_unemp)
  
}

#BLS
#LAUCN421010000000003
#https://download.bls.gov/pub/time.series/la/la.data.64.County

philly = get_unemployment_BLS('LAUCN421010000000003')

philly_max = philly[, .(unemployment=max(unemployment)), by=year]

philly5 = lapply(2009:2019, get_philly_employment, survey='acs5')
philly1 = lapply(2005:2019, get_philly_employment, survey='acs1')

