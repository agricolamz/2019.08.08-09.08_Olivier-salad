setwd("/home/agricolamz/work/materials/2019.08.08-09.08_Olivier_salad")
library(tidyverse)
library(rgdal)
library(rgeos)
library(leaflet)

russia <- readOGR(dsn = "map_data/RUS_adm/RUS_adm1.shp")
russia@data$id <- russia@data$NAME_1

ru.points <- fortify(russia, region = 'id')
russia.df <- merge(ru.points, russia@data, by = 'id')
rm(russia, ru.points)
russia.df$country <- "Russia"

gc()

ukraine <- readOGR(dsn = "map_data/UK_adm/ukr_admbnda_adm1_q2_sspe_20171221.shp")
ukraine@data$id <- ukraine@data$ADM1_EN

uk.points <- fortify(ukraine, region = 'id')
ukraine.df <- merge(uk.points, ukraine@data, by = 'id')
rm(ukraine, uk.points)
ukraine.df$country <- "Ukraine"

gc()

belarus <- readOGR(dsn = "map_data/BLR_adm/BLR_adm1.shp")
belarus@data$id <- belarus@data$NAME_1

be.points <- fortify(belarus, region = 'id')
belarus.df <- merge(be.points, belarus@data, by = 'id')
rm(belarus, be.points)
belarus.df$country <- "Belarus"

kazakhstan <- readOGR(dsn = "map_data/KAZ_adm/KAZ_adm1.shp")
kazakhstan@data$id <- kazakhstan@data$NAME_1

ka.points <- fortify(kazakhstan, region = 'id')
kazakhstan.df <- merge(ka.points, kazakhstan@data, by = 'id')
rm(ka.points, kazakhstan)
kazakhstan.df$country <- "Kazakhstan"

rbind(russia.df[, c("id", "lat", "long", "group", "country")],
      ukraine.df[, c("id", "lat", "long", "group", "country")],
      belarus.df[, c("id", "lat", "long", "group", "country")],
      kazakhstan.df[, c("id", "lat", "long", "group", "country")]) ->
  final_df

final_df %>% 
  rename(region_id = id) ->
  final_df  

final_df %>% 
  mutate(long = ifelse(long < 0, long + 360, long)) %>% 
  ggplot(aes(long, lat, group = group, color = country)) +
  geom_polygon() +
  coord_map(projection = 'gilbert', orientation = c(90, 0, 70))+
  theme_minimal()


final_df %>% 
  write_csv("all_map_df.csv")

save(final_df, file = "all_map_df.RData")
