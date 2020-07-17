rm(list = ls())
library(rjson)
library(tidyverse)
# https://github.com/codeforshinjuku/covid19
# After running ./gitdata.sh patient > patient.json, copy patient.json to the R project directory.
js0 = fromJSON(file = "patient.json")
js0 %>% class
js0 %>% names


Unzip <- function(...) rbind(data.frame(), ...)
df0 = do.call(Unzip,js0)
df0 = df0 %>% mutate(code = rownames(df0))
lf0 = df0 %>% gather(key = Date , value = n_patient , - code ) 
region_code = read_tsv("region_code.tsv") %>% mutate(code  =as.integer(code))
lf0 = lf0 %>% left_join(region_code)
lf0 = lf0 %>% mutate(Date = as.Date(Date , "X%Y.%m.%d") ,
                     code = as.integer((code)) , 
                     city_code = as.character(code5))






library(magrittr)
library(jpndistrict)
library(sf)
library(tidyverse)
library(ggimage)
sf_pref13 <- jpndistrict::jpn_pref(13, district = T) %>% 
  st_simplify(dTolerance = 0.001)
 
lf1 = sf_pref13 %>% right_join(lf0)

ggplot(lf1 %>% filter(Date == max(Date))) + 
  geom_sf(aes(fill = n_patient)) + 
  xlim(138.9, 139.9) + ylim(35.5, 36) + 
  theme(panel.border = element_blank(), 
        axis.title = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        #plot.background = element_rect(colour = "black"), 
        plot.caption = element_text(size = 6)) +
  guides(color = F)


ggplot(lf1 %>% filter(Date >= max(Date) -3)) + 
  geom_sf(aes(fill = n_patient)) + 
  xlim(138.9, 139.9) + ylim(35.5, 36) + 
  theme(panel.border = element_blank(), 
        axis.title = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        #plot.background = element_rect(colour = "black"), 
        plot.caption = element_text(size = 6)) +
  guides(color = F) + facet_wrap(~Date)


library(gifski)
library(gganimate)
library(transformr)



anim = ggplot(lf1 %>% filter(Date >= max(Date) -10)) + 
  geom_sf(aes(fill = n_patient)) + 
  xlim(138.9, 139.9) + ylim(35.5, 36) + 
  theme(panel.border = element_blank(), 
        axis.title = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        #plot.background = element_rect(colour = "black"), 
        plot.caption = element_text(size = 6)) +
  guides(color = F) + transition_time(time = Date) + 
  ease_aes("sine-in-out") + 
  labs(title = "{frame_time}")
ga = animate(anim , fps = 5 , width = 800 , height = 800)
ga
anim_save(filename = "tmp.gif", ga)