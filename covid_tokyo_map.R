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
windowsFonts("HGP" = windowsFont("HGP創英角ゴシックUB"))
windowsFonts("MEI"=windowsFont("Meiryo"))
#windowsFonts("BIZ"=  windowsFont("BIZ UDPゴシック"))
windowsFonts("BIZ"=  windowsFont("BIZ UDPGothic"))
windowsFonts("POP"=  windowsFont("HGPSoeiKakupoptai"))
if (F) {
  theme_set(theme_void() +  theme(text = element_text(family ="run_font" ), 
                                  plot.title= element_text(hjust = 0.5 , size = 14 , family ="YuGo" ) 
                                  #,                                axis.text.x = element_text(angle = 45)
  )
  )
}
if (F) {
  theme_set(theme_void(base_family = "MEI"))
  theme_set(theme_void(base_family = "BIZ"))
}
theme_set(theme_void(base_family = "POP"))

library(ggrepel)



# https://yutani.rbind.io/post/geom-sf-text-and-geom-sf-label-are-coming/

ggplot(lf1 %>% filter(Date == as.Date("2020-6-23"))) + 
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
  guides(color = F) +  theme(plot.title= element_text(hjust = 0.5)) + labs(title="テスト") + 
  geom_sf_label(family = "POP" , aes(label = city) , size = 3.5   ,color  ="#333333" , 
                label.size = 0 , label.padding = unit(0.3, "lines") , label.r = unit(0.7,"lines"))
  #geom_text_repel(aes(label = city , geometry = geometry) ,   seed = 123, stat = "sf_coordinates" , size = 2 , color = "#555555")

if (F) {
  
  ggplot(lf1 %>% filter(Date >= max(Date) -0)) + 
    geom_sf(aes(fill = diff7)) + 
    xlim(138.9, 139.9) + ylim(35.5, 36) + 
    theme(panel.border = element_blank(), 
          axis.title = element_blank(), axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          #plot.background = element_rect(colour = "black"), 
          plot.caption = element_text(size = 6)) +
    guides(color = F) + facet_wrap(~Date) + my_label
  
}
library(gifski)
library(gganimate)
library(transformr)
if (F) {
my_label = geom_sf_label(#data = lf1, # %>% filter(Date %in% c(min(Date) , max(Date))) , 
                         aes(label = ifelse(Date %in% c(min(Date) , max(Date)) , 
                               substring(str_replace(city ,"区|市|町|村|西多摩郡 ", "" ) , 1,100) , 
                               NULL)) ,
                         size = 3.5  , 
                         #fill = NA ,
                         color  ="#333333" , 
                         label.size = 0 , label.padding = unit(0.3, "lines") , 
                         label.r = unit(0.7,"lines"))
} else {
  
  my_label = 
    geom_label_repel(aes(label = ifelse(Date %in% c(min(Date) , max(Date)) , 
                                        substring(str_replace_all(city ,"区|市|町|村|西多摩郡 ", "" ) , 1,100) , 
                                        "")
                         , geometry = geometry) ,   
                     seed = 1123, stat = "sf_coordinates" , 
                     size = 3 , 
                     family = "POP", 
                     force = 0.0 , 
                     #fill = "#0000ff",
                     segment.color = "#ffffff" ,
                     #color = "#0000ff" , #"#555555" , 
                     label.size = NA , label.padding = unit(0.15, "lines") , 
                     label.r = unit(0.5,"lines"))
  
  my_label = 
    geom_text_repel(aes(label = 
                          substring(str_replace_all(city ,"区|市|町|村|西多摩郡 ", "" ) , 1,100) , 
                        
                        , geometry = geometry) ,   
                    seed = 1123, stat = "sf_coordinates" , 
                    size = 3 , 
                    family = "POP", 
                    force = 0.0 , 
                    #fill = "#0000ff",
                    segment.color = "#ffffff" ,
                    color = "#2B83BA" #"#ffffff"  #"#555555" , 
    )
  my_label = 
    geom_text_repel(aes(label = 
                          substring(str_replace_all(city ,"区|市|村|西多摩郡 ", "" ) , 1,1) , 
                        
                        , geometry = geometry) ,   
                    seed = 1123, stat = "sf_coordinates" , 
                    size = 5 , 
                    alpha = 1 , 
                    family = "POP", 
                    force = 0.0 , 
                    #fill = "#0000ff",
                    segment.color = "#777777" ,
                    color = "#2B83BA" #"#999999"  #"#555555" , 
    )
  
}

#df_pop = read_csv("https://www.toukei.metro.tokyo.lg.jp/jinkouyosoku/ty20rv00rd.csv")
df_pop = read_tsv("pop.tsv")#%>% mutate(city_code = as.character(code5)) %>% select(-code5)


lf1 = lf1 %>% left_join(df_pop)
lf1 = lf1 %>% mutate(norm_diff7 = diff7/pop , 
                     norm_diff7b = norm_diff7 * 10000)

width = 1000
height = 500
library(glue)
experiments = tibble(targets  = c("diff7", "norm_diff7", "norm_diff7b") ,
                     add_header = c("(総数)" , "（人口一人あたり）" ,"(人口１万人あたり)" ))
#for (run_target  in targets) {
for (i in 1:nrow(experiments)) {
  run_target = experiments$targets[i] 
  run_add_header = experiments$add_header[i]
  lf1 = lf1 %>% mutate_(x = run_target)
  maxx = lf1%>% filter(pop > 100000) %>% pull(x) %>% max(na.rm = T)

  
  anim = ggplot(lf1 %>% filter(Date  >= as.Date("2020-4-7"))) + 
    geom_sf(aes(fill = x) , alpha  =1) + 
    xlim(138.9, 139.9) + ylim(35.5, 36) + 
    theme(panel.border = element_blank(), 
          axis.title = element_blank(), axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          #plot.background = element_rect(colour = "black"), 
          plot.caption = element_text(size = 6)) +
    guides(color = F) + 
    #transition_time(time = Date) + 
    #labs(title = "一週間の新規感染者数{run_add_header}\n{frame_time}") +  
    transition_manual(Date) + 
    labs(title = "一週間の新規感染者数{run_add_header}\n{current_frame}") +  
    scale_fill_distiller(palette = "Spectral" , limits = c(0 , maxx)) +
    guides(fill = guide_legend(title = "new infections (seven days)  ")) +
    #ease_aes("sine-in-out") + 

    theme(plot.title= element_text(hjust = 0.5 , size = 12)) +
    my_label

  fps = 3
  ga = animate(anim , fps = fps , width = width , height = height , end_pause = fps * 3 , start_pause = fps)
  ga
  anim_save(filename = glue("covid_tokyo{width}x{height}_{run_target}.gif"), ga)
  
  if (F){
    ga = animate(anim , fps = 1.5 , width = 1200 , height = 400 , end_pause = as.integer(1.5 * fps) , start_pause = 5)
    ga
    anim_save(filename = "covid_tokyo1200x400.gif", ga)
  }
}


