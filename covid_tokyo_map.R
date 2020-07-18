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
if (T) {
  sf0 = lf0 %>% spread(key = code , value = n_patient ,  fill = 0) %>% mutate(Date = as.Date(Date , "X%Y.%m.%d"))
  sf0 = data.frame(Date = seq(from = min(sf0$Date) , to = max(sf0$Date)  , by = "1 day")) %>% left_join(sf0) 
  # https://stackoverflow.com/questions/40040834/replace-na-with-previous-or-next-value-by-group-using-dplyr
  lf0 = sf0 %>% gather(key = code , value = n_patient , - Date)
  lf0 = lf0 %>% group_by(code) %>% fill(n_patient , .direction = "up")

} else {
  lf0 = lf0 %>% spread(key = Date , value = n_patient ,  fill = 0) %>% gather(key = Date , value = n_patient , - code)
}
#region_code = read_tsv("region_code.tsv") %>% mutate(code  =as.integer(code))
region_code = read_tsv("region_code.tsv") %>% mutate(code  =as.character(code))

lf0 = lf0 %>% left_join(region_code)
lf0 = lf0 %>% mutate(Date = as.Date(Date , "X%Y.%m.%d") ,
                     code = as.integer((code)) , 
                     city_code = as.character(code5))



lf0 = lf0 %>% arrange(Date) %>% group_by(city_code) %>% mutate(
  diff1 = n_patient - lag(n_patient) ,
  diff7 = n_patient - lag(n_patient , 7)) %>% ungroup



library(magrittr)
library(jpndistrict)
library(sf)
library(tidyverse)
library(ggimage)
sf_pref13 <- jpndistrict::jpn_pref(13, district = T) %>% 
  st_simplify(dTolerance = 0.001)
 
lf1 = sf_pref13 %>% right_join(lf0)
theme_set(theme_void())
library(ggrepel)
ggplot(lf1 %>% filter(Date == as.Date("2020-5-23"))) + 
  geom_sf(aes(fill =diff7)) + 
  xlim(138.9, 139.9) + ylim(35.5, 36) + 
  #geom_text_repel(aes(x  = lng, y = lat,label = city) , seed = 123) +   
  theme(panel.border = element_blank(), 
        axis.title = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        #plot.background = element_rect(colour = "black"), 
        plot.caption = element_text(size = 6)) +
  scale_fill_distiller(palette = "Spectral") +
  guides(fill = guide_legend(title = "new patients (seven days)")) +
  guides(color = F) +  theme(plot.title= element_text(hjust = 0.5)) + labs(title="a")

if (F) {
  
  ggplot(lf1 %>% filter(Date >= max(Date) -3)) + 
    geom_sf(aes(fill = diff7)) + 
    xlim(138.9, 139.9) + ylim(35.5, 36) + 
    theme(panel.border = element_blank(), 
          axis.title = element_blank(), axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          #plot.background = element_rect(colour = "black"), 
          plot.caption = element_text(size = 6)) +
    
    guides(color = F) + facet_wrap(~Date)
  
}
library(gifski)
library(gganimate)
library(transformr)



anim = ggplot(lf1 %>% filter(Date  >= as.Date("2020-4-7"))) + 
  geom_sf(aes(fill = diff7)) + 
  xlim(138.9, 139.9) + ylim(35.5, 36) + 
  theme(panel.border = element_blank(), 
        axis.title = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        #plot.background = element_rect(colour = "black"), 
        plot.caption = element_text(size = 6)) +
  guides(color = F) + transition_time(time = Date) + 
  scale_fill_distiller(palette = "Spectral") +
  guides(fill = guide_legend(title = "new infections (seven days)  ")) +
  #ease_aes("sine-in-out") + 
  labs(title = "{frame_time}") +  theme(plot.title= element_text(hjust = 0.5 , size = 12))
ga = animate(anim , fps = 1.5 , width = 800 , height = 250 , end_pause = 10)
ga
anim_save(filename = "covid_tokyo85.gif", ga)

