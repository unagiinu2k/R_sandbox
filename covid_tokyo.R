rm(list = ls())
library(rjson)
library(tidyverse)
# https://github.com/codeforshinjuku/covid19
js0 = fromJSON(file = "patient.json")
js0 %>% class
js0 %>% names


Unzip <- function(...) rbind(data.frame(), ...)
df0 = do.call(Unzip,js0)
df0 = df0 %>% mutate(code = rownames(df0))
lf0 = df0 %>% gather(key = Date , value = n_patient , - code ) 
lf0 = lf0 %>% mutate(Date = as.Date(Date , "X%Y.%m.%d") ,
                     code = as.integer((code)))


#tf = as.tibble(js0)
#iris.frame <- do.call(Unzip, iris.list)
library(readr)
if (F) {
  # https://ecitizen.jp/Sac/13
  region_code = read.csv("clipboard" , sep = "\t")
  
  region_code = region_code[2:4] 
  names(region_code) = c("code5" , "name" , "Kana" , "code")
  region_code = region_code %>% mutate(code = as.integer(code) , 
                                       code5 = as.integer(code5))
  write_tsv(region_code , "region_code.tsv")
  
  
}
if (F) {
  #http://note.omarukun.com/notes/4/
}
if (F){
  library(XML)
}
region_code = read_tsv("region_code.tsv") %>% mutate(code  =as.integer(code))

lf0 = lf0 %>% left_join(region_code)
lf0 %>% head
lf0 %>% dim
lf0 = lf0 %>% arrange(name, Date) %>% group_by(name) %>% mutate(diff7 = n_patient - lag(n_patient , 7)) %>% ungroup
library(ggrepel)
library(ggiraph)

(g = ggplot(lf0 , aes(x = Date , y = n_patient , color = name , tooltip = name, data_id = name))  + 
    #geom_line() + 
    geom_line_interactive() +
    #geom_text_repel(data = lf0 %>% group_by(name) %>% filter(Date == max(Date)) %>% ungroup)+
    #geom_text_repel(data = lf0 %>% group_by(name) %>% filter(Date == max(Date)) %>% ungroup)+
    theme_minimal() + 
    scale_x_date(date_labels = "%m/%d") +
    #    expand_limits(x = as.Date("2020/8/1")) +
    guides(color = F))

girafe(ggobj = g)


(g = ggplot(lf0 , aes(x = Date , y = diff7 , color = name , tooltip = name, data_id = name))  + 
    #geom_line() + 
    geom_line_interactive() +
    theme_minimal() + 
    scale_x_date(date_labels = "%m/%d") +
    #    expand_limits(x = as.Date("2020/8/1")) +
    guides(color = F))



girafe(ggobj = g)
if (F) {
# https://catalog.data.metro.tokyo.lg.jp/dataset/t000003d0000000039/resource/7c79272f-d3eb-434f-8e83-ab16409fe451
  df_pop = read.csv("clipboard" , sep = "\t")
  df_pop %>% head
  df_pop = df_pop %>% transmute_(code5 = "地域コード" , pop = "X2020年")
  df_pop %>% write_tsv("pop.tsv")
  
}
df_pop = read_tsv("pop.tsv")
lf0 = lf0 %>% left_join(df_pop)
lf0 = lf0 %>% mutate(p2p = n_patient / pop ,
                     diff7to_p = diff7 / pop)



(g = ggplot(lf0 , aes(x = Date , y = p2p , color = name , tooltip = name, data_id = name))  + 
    #geom_line() + 
    geom_line_interactive() +
    #geom_text_repel(data = lf0 %>% group_by(name) %>% filter(Date == max(Date)) %>% ungroup)+
    #geom_text_repel(data = lf0 %>% group_by(name) %>% filter(Date == max(Date)) %>% ungroup)+
    theme_minimal() + 
    scale_x_date(date_labels = "%m/%d") +
    #    expand_limits(x = as.Date("2020/8/1")) +
    guides(color = F))

girafe(ggobj = g)


(g = ggplot(lf0 , aes(x = Date , y = diff7 , color = name , tooltip = name, data_id = name))  + 
    #geom_line() + 
    geom_line_interactive() +
    theme_minimal() + 
    scale_x_date(date_labels = "%m/%d") +
    #    expand_limits(x = as.Date("2020/8/1")) +
    guides(color = F))



girafe(ggobj = g)

library(leaflet)

map_dfr()
