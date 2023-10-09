rm(list=ls())

library(tidyverse)
library(curl)
library(sf)
library(ragg)
library(paletteer)
library(extrafont)
library(readxl)
library(scales)

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          strip.clip="off",
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"),
          plot.subtitle=element_text(colour="Grey40", hjust=0, vjust=1),
          plot.caption=element_text(colour="Grey40", hjust=1, vjust=1, size=rel(0.8)),
          axis.text=element_text(colour="Grey40"),
          axis.title=element_text(colour="Grey20"),
          legend.text=element_text(colour="Grey40"),
          legend.title=element_text(colour="Grey20"))
}

#Download population counts for England & Wales by LSOA mid 2020 (latest available)
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2020sape23dt2/sape23dt2mid2020lsoasyoaestimatesunformatted.xlsx"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

ewpop <- read_excel(temp, sheet="Mid-2020 Persons", range="A5:G34758")

#Download shapefile of LSOA boundaries (2021 to (hopefully) align with pop data)
#Read in shapefile
temp <- tempfile()
temp2 <- tempfile()
source <- "https://stg-arcgisazurecdataprod1.az.arcgis.com/exportfiles-1559-20196/Lower_layer_Super_Output_Areas_2021_EW_BFC_V8_8154990398368723939.zip?sv=2018-03-28&sr=b&sig=Np6DuNXUbq1yE3eqBMwJsBfL9BzrP8D8Ru5g4o7RxC0%3D&se=2023-10-07T17%3A17%3A00Z&sp=r"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

#The actual shapefile has a different name each time you download it, so need to fish the name out of the unzipped file
name <- list.files(temp2, pattern=".shp")[c(1)]
ewshapefile <- st_read(file.path(temp2, name))

#Get centroids of each LSOA, because that's all we need
ewcentroids <- st_centroid(ewshapefile)

#Join together
ewmap.data <- full_join(ewcentroids, ewpop %>% rename("LSOA21CD"="LSOA Code") %>% 
                          select(c(LSOA21CD, `All Ages`)), by="LSOA21CD")

#Extract new polygon which is just the English/Welsh border
ewborder <- st_intersection(ewshapefile %>% 
                              filter(substr(LSOA21CD,1,1)=="E") %>% 
                              st_union(),
                            ewshapefile %>% 
                              filter(substr(LSOA21CD,1,1)=="W") %>% 
                              st_union())

#Plot it just to check
ggplot(ewborder, aes(geometry=geometry))+
  geom_sf()+
  theme_void()

#Find the distance between each LSOA centroid and this line (in km)
borderdist <- data_frame(LSOA21CD=ewcentroids$LSOA21CD, distance=st_distance(ewcentroids, ewborder)) %>% 
  mutate(distance=as.numeric(gsub(" [m]", "", as.character(distance)))/1000)

#plot it just to check (takes ages, but does look very pretty)
full_join(ewshapefile, borderdist, by="LSOA21CD") %>% 
  ggplot(aes(geometry=geometry, fill=distance, colour=distance))+
  geom_sf()+
  scale_fill_paletteer_c("viridis::inferno")+
  scale_colour_paletteer_c("viridis::inferno")+
  theme_void()

#Merge in populations
fulldata <- merge(ewpop %>% rename("LSOA21CD"="LSOA Code") %>% 
                    select(c(LSOA21CD, `All Ages`)), borderdist) %>% 
  mutate(Country=if_else(substr(LSOA21CD,1,1)=="E", "England", "Wales")) %>% 
  rename("Pop"="All Ages")

#Calculate, for each country, % of population within each distance
results <- fulldata %>% 
  group_by(Country) %>% 
  arrange(distance) %>% 
  mutate(cumpop=cumsum(Pop),
         popsum=sum(Pop),
         cumprop=cumpop/popsum) %>% 
  ungroup()

#Plot proportions of population within each distance of the border

agg_png("Outputs/EnglandWalesBorderPops.png", units="in", width=8, height=6, res=800)
ggplot(results, aes(x=distance, y=cumprop, colour=Country))+
  geom_line()+
  scale_x_continuous(name="Distance from England/Wales border (km)")+
  scale_y_continuous(name="Proportion of population", labels=label_percent(accuracy=1))+
  scale_colour_paletteer_d("colorblindr::OkabeIto", name="")+
  theme_custom()+
  theme(panel.grid.major.y=element_line(colour="grey95"))
dev.off()

#Plot absolute populations within each distance of the border

agg_png("Outputs/EnglandWalesBorderPopsv2.png", units="in", width=8, height=6, res=800)
ggplot(results, aes(x=distance, y=cumpop, colour=Country))+
  geom_line()+
  scale_x_continuous(name="Distance from England/Wales border (km)")+
  scale_y_continuous(name="Proportion of population")+
  scale_colour_paletteer_d("colorblindr::OkabeIto", name="")+
  theme_custom()+
  theme(panel.grid.major.y=element_line(colour="grey95"))
dev.off()

