# https://uribo.hatenablog.com/entry/2017/12/08/144549
# https://tsukubar.github.io/r-spatial-guide/spatial-data-mapping.html

library(magrittr)
library(jpndistrict)
library(sf)
library(tidyverse)
library(ggimage)
sf_pref13 <- jpndistrict::jpn_pref(13, district = T) %>% 
  st_simplify(dTolerance = 0.001)

class(sf_pref01)
sf_pref13$geometry
theme_set(theme_void())
ggplot(sf_pref13) + geom_sf(fill = "white" , aes(color = city)) + 
  xlim(138.9, 139.9) + ylim(35.5, 36) + 
  theme(panel.border = element_blank(), 
        axis.title = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        #plot.background = element_rect(colour = "black"), 
        plot.caption = element_text(size = 6)) +
  guides(color = F)
