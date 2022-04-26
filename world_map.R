#
# map
#

require(tidyverse)
require(tmap)

bib <- read_tsv("Data/biblio.txt") %>% mutate(Country=factor(Country)) %>% group_by(Country) %>% summarise(n=n()) %>% mutate( frec=round(n/sum(n)*100,2))
anti_join(bib,World,by=c("Country"="name"))

knitr::kable(bib %>% arrange(desc(n)))
write_tsv(bib %>% arrange(desc(n)),"Data/RecordsByCountry.txt")

World1 <- sp::merge(World,bib, by.x="name",by.y="Country",all.x=TRUE)

###########################################################################
## This script will replicate the figures (except the screenshots)
## and write them to files.
## The working directory should be set the the parent folder of this script.
###########################################################################

## install.packages(c("tmap", "tmaptools"))
library("tmap") # required version 2.0 or later
library("tmaptools") # required version 2.0 or later

data("World", package = "tmap")

#############################
## Figure 1
#############################

m1 <- tm_shape(World1) +
  tm_polygons("n", palette = "-Blues", 
              title = "No. Records", contrast = 0.7, border.col = "grey30", id = "name") +
  tm_text("iso_a3", size = "AREA", col = "grey30", root = 3) +
  tm_style("gray") +
  tm_format("World", frame.lwd = 2)
m1
#tmap_save(m1, "bubble.png", width = 6.125, height = 3, scale = .75, dpi = 300, asp = 0, outer.margins = 0)



m3 <- tm_shape(World1, projection = robin) +
  tm_polygons(c("n"),
              palette = "Paired",  # "RdYlGn"
              style = c("fixed"),
              breaks = c(1, 2, 3,4, 6,10,20, Inf),
              title = c("No. Records")) +
  tm_style("natural", earth.boundary = c(-180, -87, 180, 87))  +
  tm_format("World", inner.margins = 0.02, frame = FALSE) +
  tm_legend( bg.color = "gray95", frame = TRUE, position = c("left", "bottom")) # +   tm_credits(c("Robinson projection"), position = c("RIGHT", "BOTTOM"))

m3

tmap_save(m3, "Figures/world_records.png", width = 5, scale = .7, dpi = 300, outer.margins = 0)


