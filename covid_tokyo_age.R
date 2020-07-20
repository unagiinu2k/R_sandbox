rm(list = ls())
#library(rjson)
library(tidyverse)
library(readr)
df  = read_csv("https://stopcovid19.metro.tokyo.lg.jp/data/130001_tokyo_covid19_patients.csv")
#df %>% tail %>% View
df = df %>% mutate(患者_年代 = ifelse(患者_年代 == "'-", "不明",患者_年代))

sf = df %>% group_by(公表_年月日,患者_年代) %>% summarize(n = n()) %>% ungroup
complemented = expand.grid(
  公表_年月日 = seq(from = min(sf$公表_年月日) , to = max(sf$公表_年月日)  , by = "1 day"),
  患者_年代 = unique(sf$患者_年代))
sf = complemented %>% left_join(sf)
sf = sf %>% replace_na(list(n = 0))

sf = sf %>% arrange(公表_年月日,患者_年代)
library(RcppRoll)
sf = sf %>% group_by(患者_年代) %>% mutate(weekly = roll_sumr(n , 7)) %>% ungroup

age_order = c( "10歳未満"  ,"10代"    ,  "20代",     
 "30代"  ,    "40代"    ,  "50代"  ,    "60代"  ,    "70代",
 "80代"    ,  "90代",      "100歳以上" , "不明"    , "'-"      )


sf = sf %>% mutate(患者_年代 = factor(患者_年代 , age_order))
theme_set(theme_minimal())
windowsFonts("YuGo" = windowsFont("游ゴシック"))
windowsFonts("YuGo" = windowsFont("HG創英角ゴシックUB"))


ggplot(sf %>% filter(公表_年月日 >= max(公表_年月日)- 20) , 
       aes(fill = 患者_年代 , x = 患者_年代 ,y  = n)) + geom_bar(stat = "identity")  + 
  labs(x = NULL , y = "daily new infections") + guides(fill = F)  + 
  scale_fill_brewer(palette = "Paired")   +
  theme(text = element_text(family ="YuGo" ), 
    plot.title= element_text(hjust = 0.5 , size = 12) ,
        axis.text.x = element_text(angle = 45))
library(gifski)
library(gganimate)
library(transformr)
days = seq(from = max(sf$公表_年月日) , to = min(sf$公表_年月日)  , by = "-7 day") %>% sort
anim = ggplot(sf %>% filter(公表_年月日 %in% days)  , aes(fill = 患者_年代 , x = 患者_年代 ,y  = weekly)) +
  geom_bar(stat = "identity")  + 
  labs(x = NULL , y = "weekly new infections" ,title = "一週間の都内新規感染者数\n{closest_state}") +
  scale_fill_brewer(palette = "Paired")   + 
  guides(fill = F) +
  scale_y_continuous(labels = scales::comma) + 
  transition_states(公表_年月日)  +
  theme(text = element_text(family ="YuGo" ), 
        plot.title= element_text(hjust = 0.5 , size = 14 , family ="YuGo" ) ,
        axis.text.x = element_text(angle = 45))

ga = animate(anim , fps = 3 , width = 450 , height = 300 , end_pause = 20)
ga
anim_save(filename = "covid_tokyo_age.gif", ga)

ggplot(sf %>% filter(公表_年月日 %in% days)  ,
       aes(fill = 患者_年代 , x = 公表_年月日 ,y  = weekly)) +
  geom_bar(stat = "identity" , position = "fill")  + 
  scale_fill_brewer(palette = "Paired") + 
  labs(x = NULL , y = "weekly new infections" ,title = "{closest_state}") +
  guides(fill = F) +
  scale_y_continuous(labels = scales::comma) + 
  transition_states(公表_年月日)  + coord_flip() +
  theme(plot.title= element_text(hjust = 0.5 , size = 12))

