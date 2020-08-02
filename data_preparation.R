#Set work directory
setwd("E:/Jeff/columbia/Projects/Lall/Echo-State-RNN")
DATA_FOLDER = "E:/Jeff/columbia/Projects/Lall/Echo-State-RNN/"
SUBSET = "../dataset/TEX_lat_lon_index_key_Subset.txt"
#Load libraries
library(devtools)
#install_github("andrewzm/STRbook")
library("ggplot2")
library("dplyr")
library("STRbook")
library("tidyr")
library(ncdf4); library(readr);library(lubridate)

#########################################################
# Wind Speed ############################################
#########################################################

# open the .nc file
WS0 = nc_open(ncfname <- paste0(DATA_FOLDER, "TEX_monthly_WS_0_5_deg_rectangle.nc")) # <- insert the file path to the .nc file and replace "WS" with "WP" for wind power or "WS" for WSiation

# extract the lat/lon
latitude = as.vector(ncvar_get(WS0,"lat")) # <- replace "Rad" with "WP" for wind power or "Rad" for radiation
longitude = as.vector(ncvar_get(WS0,"lon"))

# extract the date
origin_date = strsplit(ncatt_get(WS0,"time","units")$value, split = c(" "))[[1]][3] # <- replace "WS" with "WP" for wind power or "WS" for WSiation
time = ncvar_get(WS0, varid = "time") # <- replace "WS" with "WP" for wind power or "WS" for WSiation
date = as.Date(time, origin = origin_date)

# load the lat/lon index key  
# TEX_0_5_deg_lat_lon_index_key = read_csv(paste0(DATA_FOLDER, "TEX_0_5_deg_lat_lon_index_key.csv")) # <- insert the file path to the lat/lon index file

# extract the main variable and assign date, lat, and lon to each observation  
WS <- reshape2::melt(ncvar_get(WS0, # <- replace "WS" with "WP" for wind power or "WS" for WSiation
                               varid = "WS", # <- replace "WS" with "WP_CF" for wind power or "ssrd" for WSiation
                               start = c(1,1,1),
                               count = c(-1,-1,-1))) %>%
  setNames(c("lat", "lon", "time_index", "WS")) %>% # <- replace "WS" with "WP" for wind power or "WS" for WSiation
  dplyr::mutate(lon = longitude[lon],
                lat = latitude[lat])

date <- date[WS$time_index]
WS <- WS %>%
  mutate(year = year(date), month = month(date)) %>%
  dplyr::select(lon, lat, year, month, WS)


WS <- WS %>% mutate(date = paste(year, month, sep = "_")) %>%
  dplyr::select(date,lat,lon,WS)

for(i in seq(from=250, to=250.5, by=0.5)){
  d = data.frame(matrix(NA,480,1))
  for (j in seq(from=25, to=25.5, by=0.5)) {
    d = cbind(d, WS %>% 
                dplyr::filter(lon == i, lat == j)%>%
                dplyr::select(WS))
  }
}

lat_25 <- WS %>% 
  dplyr::filter(lon == 250, lat == 25)%>%
  dplyr::select(WS)

for (i in seq(from=250.5, to=270, by=0.5)) {
  lat_25 <- cbind(lat_25, WS %>% 
                    dplyr::filter(lon == i, lat == 25)%>%
                    dplyr::select(WS))
}

lat_25.5 <- WS %>% 
  dplyr::filter(lon == 250, lat == 25.5)%>%
  dplyr::select(WS)

for (i in seq(from=250.5, to=270, by=0.5)) {
  lat_25.5 <- cbind(lat_25.5, WS %>% 
                      dplyr::filter(lon == i, lat == 25.5)%>%
                      dplyr::select(WS))
}

lat_26 <- WS %>% 
  dplyr::filter(lon == 250, lat == 26)%>%
  dplyr::select(WS)

for (i in seq(from=250.5, to=270, by=0.5)) {
  lat_26 <- cbind(lat_26, WS %>% 
                    dplyr::filter(lon == i, lat == 26)%>%
                    dplyr::select(WS))
}

lat_26.5 <- WS %>% 
  dplyr::filter(lon == 250, lat == 26.5)%>%
  dplyr::select(WS)

for (i in seq(from=250.5, to=270, by=0.5)) {
  lat_26.5 <- cbind(lat_26.5, WS %>% 
                      dplyr::filter(lon == i, lat == 26.5)%>%
                      dplyr::select(WS))
}

lat_27 <- WS %>% 
  dplyr::filter(lon == 250, lat == 27)%>%
  dplyr::select(WS)

for (i in seq(from=250.5, to=270, by=0.5)) {
  lat_27 <- cbind(lat_27, WS %>% 
                    dplyr::filter(lon == i, lat == 27)%>%
                    dplyr::select(WS))
}

lat_27.5 <- WS %>% 
  dplyr::filter(lon == 250, lat == 27.5)%>%
  dplyr::select(WS)

for (i in seq(from=250.5, to=270, by=0.5)) {
  lat_27.5 <- cbind(lat_27.5, WS %>% 
                      dplyr::filter(lon == i, lat == 27.5)%>%
                      dplyr::select(WS))
}

lat_28 <- WS %>% 
  dplyr::filter(lon == 250, lat == 28)%>%
  dplyr::select(WS)

for (i in seq(from=250.5, to=270, by=0.5)) {
  lat_28 <- cbind(lat_28, WS %>% 
                    dplyr::filter(lon == i, lat == 28)%>%
                    dplyr::select(WS))
}

lat_28.5 <- WS %>% 
  dplyr::filter(lon == 250, lat == 28.5)%>%
  dplyr::select(WS)

for (i in seq(from=250.5, to=270, by=0.5)) {
  lat_28.5 <- cbind(lat_28.5, WS %>% 
                      dplyr::filter(lon == i, lat == 28.5)%>%
                      dplyr::select(WS))
}

lat_29 <- WS %>% 
  dplyr::filter(lon == 250, lat == 29)%>%
  dplyr::select(WS)

for (i in seq(from=250.5, to=270, by=0.5)) {
  lat_29 <- cbind(lat_29, WS %>% 
                    dplyr::filter(lon == i, lat == 29)%>%
                    dplyr::select(WS))
}

lat_29.5 <- WS %>% 
  dplyr::filter(lon == 250, lat == 29.5)%>%
  dplyr::select(WS)

for (i in seq(from=250.5, to=270, by=0.5)) {
  lat_29.5 <- cbind(lat_29.5, WS %>% 
                      dplyr::filter(lon == i, lat == 29.5)%>%
                      dplyr::select(WS))
}

lat_30 <- WS %>% 
  dplyr::filter(lon == 250, lat == 30)%>%
  dplyr::select(WS)

for (i in seq(from=250.5, to=270, by=0.5)) {
  lat_30 <- cbind(lat_30, WS %>% 
                    dplyr::filter(lon == i, lat == 30)%>%
                    dplyr::select(WS))
}

lat_30.5 <- WS %>% 
  dplyr::filter(lon == 250, lat == 30.5)%>%
  dplyr::select(WS)

for (i in seq(from=250.5, to=270, by=0.5)) {
  lat_30.5 <- cbind(lat_30.5, WS %>% 
                      dplyr::filter(lon == i, lat == 30.5)%>%
                      dplyr::select(WS))
}

lat_31 <- WS %>% 
  dplyr::filter(lon == 250, lat == 31)%>%
  dplyr::select(WS)

for (i in seq(from=250.5, to=270, by=0.5)) {
  lat_31 <- cbind(lat_31, WS %>% 
                    dplyr::filter(lon == i, lat == 31)%>%
                    dplyr::select(WS))
}

lat_31.5 <- WS %>% 
  dplyr::filter(lon == 250, lat == 31.5)%>%
  dplyr::select(WS)

for (i in seq(from=250.5, to=270, by=0.5)) {
  lat_31.5 <- cbind(lat_31.5, WS %>% 
                      dplyr::filter(lon == i, lat == 31.5)%>%
                      dplyr::select(WS))
}

lat_32 <- WS %>% 
  dplyr::filter(lon == 250, lat == 32)%>%
  dplyr::select(WS)

for (i in seq(from=250.5, to=270, by=0.5)) {
  lat_32 <- cbind(lat_32, WS %>% 
                    dplyr::filter(lon == i, lat == 32)%>%
                    dplyr::select(WS))
}

lat_32.5 <- WS %>% 
  dplyr::filter(lon == 250, lat == 32.5)%>%
  dplyr::select(WS)

for (i in seq(from=250.5, to=270, by=0.5)) {
  lat_32.5 <- cbind(lat_32.5, WS %>% 
                      dplyr::filter(lon == i, lat == 32.5)%>%
                      dplyr::select(WS))
}

lat_33 <- WS %>% 
  dplyr::filter(lon == 250, lat == 33)%>%
  dplyr::select(WS)

for (i in seq(from=250.5, to=270, by=0.5)) {
  lat_33 <- cbind(lat_33, WS %>% 
                    dplyr::filter(lon == i, lat == 33)%>%
                    dplyr::select(WS))
}

lat_33.5 <- WS %>% 
  dplyr::filter(lon == 250, lat == 33.5)%>%
  dplyr::select(WS)

for (i in seq(from=250.5, to=270, by=0.5)) {
  lat_33.5 <- cbind(lat_33.5, WS %>% 
                      dplyr::filter(lon == i, lat == 33.5)%>%
                      dplyr::select(WS))
}

lat_34 <- WS %>% 
  dplyr::filter(lon == 250, lat == 34)%>%
  dplyr::select(WS)

for (i in seq(from=250.5, to=270, by=0.5)) {
  lat_34 <- cbind(lat_34, WS %>% 
                    dplyr::filter(lon == i, lat == 34)%>%
                    dplyr::select(WS))
}

lat_34.5 <- WS %>% 
  dplyr::filter(lon == 250, lat == 34.5)%>%
  dplyr::select(WS)

for (i in seq(from=250.5, to=270, by=0.5)) {
  lat_34.5 <- cbind(lat_34.5, WS %>% 
                      dplyr::filter(lon == i, lat == 34.5)%>%
                      dplyr::select(WS))
}

lat_35 <- WS %>% 
  dplyr::filter(lon == 250, lat == 35)%>%
  dplyr::select(WS)

for (i in seq(from=250.5, to=270, by=0.5)) {
  lat_35 <- cbind(lat_35, WS %>% 
                    dplyr::filter(lon == i, lat == 35)%>%
                    dplyr::select(WS))
}

lat_35.5 <- WS %>% 
  dplyr::filter(lon == 250, lat == 35.5)%>%
  dplyr::select(WS)

for (i in seq(from=250.5, to=270, by=0.5)) {
  lat_35.5 <- cbind(lat_35.5, WS %>% 
                      dplyr::filter(lon == i, lat == 35.5)%>%
                      dplyr::select(WS))
}

lat_36 <- WS %>% 
  dplyr::filter(lon == 250, lat == 36)%>%
  dplyr::select(WS)

for (i in seq(from=250.5, to=270, by=0.5)) {
  lat_36 <- cbind(lat_36, WS %>% 
                    dplyr::filter(lon == i, lat == 36)%>%
                    dplyr::select(WS))
}

lat_36.5 <- WS %>% 
  dplyr::filter(lon == 250, lat == 36.5)%>%
  dplyr::select(WS)

for (i in seq(from=250.5, to=270, by=0.5)) {
  lat_36.5 <- cbind(lat_36.5, WS %>% 
                      dplyr::filter(lon == i, lat == 36.5)%>%
                      dplyr::select(WS))
}

lat_37 <- WS %>% 
  dplyr::filter(lon == 250, lat == 37)%>%
  dplyr::select(WS)

for (i in seq(from=250.5, to=270, by=0.5)) {
  lat_37 <- cbind(lat_37, WS %>% 
                    dplyr::filter(lon == i, lat == 37)%>%
                    dplyr::select(WS))
}


lat_37.5 <- WS %>% 
  dplyr::filter(lon == 250, lat == 37.5)%>%
  dplyr::select(WS)

for (i in seq(from=250.5, to=270, by=0.5)) {
  lat_37.5 <- cbind(lat_37.5, WS %>% 
                      dplyr::filter(lon == i, lat == 37.5)%>%
                      dplyr::select(WS))
}

#Combine all the grid points
df = cbind(lat_25,lat_25.5, lat_26, lat_26.5, lat_27, lat_27.5, lat_28, 
           lat_28.5, lat_29, lat_29.5, lat_30, lat_30.5, lat_31, 
           lat_31.5, lat_32, lat_32.5, lat_33, lat_33.5, lat_34,
           lat_34.5, lat_35, lat_35.5, lat_36, lat_36.5, lat_37, lat_37.5)

#Add date column
date=seq(from = as.Date('1979/01/01', format = "%Y/%m/%d"), to  = as.Date('2018/12/01', format = "%Y/%m/%d"), by="month")
date <- format(date, format = "%Y/%m")

#Full dataset
df_monthly = cbind(date, df)
write.csv(df_monthly,file="E:/Jeff/columbia/Projects/Lall/Echo-State-RNN/df_monthly.csv")

