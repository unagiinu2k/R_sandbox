rm(list = ls())
library(tidyverse)
library(readr)
if (T) {
#  df = read_csv("https://www.city.adachi.tokyo.jp/documents/47358/131200_city_adachi_covid19_patients0807.csv")
  df = read_csv("https://www.city.adachi.tokyo.jp/documents/47358/131200_city_adachi_covid19_patients0812.csv")
  df = df %>% mutate(g = str_replace_all(備考, "(2020/|し公表)" , ""))
} else {
  df = read_csv("adachi.csv") #with URL
  df = df %>% mutate(診断日 = as.Date(診断日 , "%Y/%m/%d"))
  df = df %>% mutate(url_short = str_replace_all(URL , 
                     "(https://www.city.adachi.tokyo.jp/hodo/juyo/|.html)" , ""))
  df = df %>% mutate(g = url_short)
}
df = df %>% mutate(年代 = str_replace(年代 , "３０代" , "30代") %>%
                       str_replace("４０代"  , "40代"))
start_date  =as.Date("2020-1-1")
end_date  = as.Date("2020-12-31")
#df = df  %>% mutate(is_na_biko = str_count(備考, "集団") == 0)
df = df  %>% mutate(is_na_biko = is.na(g))
df1 = df %>% filter(診断日 >= start_date & 診断日 <= end_date) 
df1 %>% dim

df1$symptom %>% unique
df1$症状 %>% str_split("、") %>% flatten %>% as.character %>%  unique

df1 = df1 %>% mutate(symptom = 　str_replace_all(症状, "なし" , "無症状") %>% 
                       str_replace_all("障害|異常", "")%>%
                     str_replace_all("発熱嗅覚", "発熱・嗅覚"))

symptoms = df1$symptom %>% str_split("、| |・|・|\\s") %>% flatten %>% as.character %>% 
　　  str_replace_all("なし" , "無症状") %>% unique %>% setdiff("")

sf_all = data.frame()
#sf_all_biko = data.frame()
for (s in symptoms) {
  if (F) {
    s = symptoms[1]
  }
  if (T) {
      run_sf = df1 %>% mutate(run_sym =str_count(symptom  ,  s) >0 ) %>% 
    group_by(年代, 性別 , is_na_biko ,run_sym) %>% summarize(n = n()) %>% ungroup %>% mutate(symptom = s)
      
  } else {
    run_sf = df1 %>% mutate(run_sym =str_count(symptom  ,  s) >0 ) %>% 
      group_by(年代, run_sym) %>% summarize(n = n()) %>% ungroup %>% mutate(symptom = s)
  }
  sf_all = bind_rows(sf_all , run_sf)
}
sf_all %>% head
sf2 = sf_all %>% spread(key = run_sym , value = n , fill = 0 )  %>%
  rename(with = "TRUE"  , without = "FALSE")
ordered_sym = sf2 %>% group_by(symptom) %>% summarize(total = sum(with)) %>% ungroup %>% arrange(desc(total))%>% pull(symptom)
sf2 = sf2 %>% mutate(symptom = factor(symptom , ordered_sym))
library(tidyverse)
library(ggrepel)

windowsFonts("YuGo" = windowsFont("游ゴシック"))
#windowsFonts("YuGo" = windowsFont("HG創英角ゴシックUB"))
windowsFonts("POP"=  windowsFont("HGPSoeiKakupoptai"))
theme_set(theme_minimal(base_family = "POP"))

sf2_age  =sf2 %>% group_by(年代, symptom) %>% summarize(with = sum(with) , without = sum(without)) %>% ungroup
ggplot(sf2_age ,  aes(y = 年代, x = with / (with + without) , 
                  fill = 年代, 
                  color = 年代 )) + 
  geom_bar(stat = "identity") + 
  scale_x_continuous(labels = scales::percent , name = "比率") + 
  #coord_flip() +
  labs(y = NULL) +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~symptom) + guides(color = F, fill = F)



sf2_age_na_biko  =sf2 %>% filter(is_na_biko)%>% group_by(年代, symptom ) %>% summarize(with = sum(with) , without = sum(without)) %>% ungroup
ggplot(sf2_age_na_biko ,  aes(y = 年代, x = with / (with + without) , 
                  fill = 年代, 
                  color = 年代 )) + 
  geom_bar(stat = "identity") + 
  scale_x_continuous(labels = scales::percent , name = "比率") + 
  #coord_flip() +
  labs(y = NULL) +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~symptom) + guides(color = F, fill = F)

sf2_age_with_biko  =sf2 %>% filter(!is_na_biko)%>% group_by(年代, symptom ) %>% summarize(with = sum(with) , without = sum(without)) %>% ungroup
ggplot(sf2_age_with_biko ,  aes(y = 年代, x = with / (with + without) , 
                  fill = 年代, 
                  color = 年代 )) + 
  geom_bar(stat = "identity") + 
  scale_x_continuous(labels = scales::percent , name = "比率") + 
  #coord_flip() +
  labs(y = NULL) +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~symptom) + guides(color = F, fill = F)



# delete below 

ggplot(sf2 ,  aes(x = symptom, y = with / (with + without) , color = symptom )) + 
  geom_bar(stat = "identity") + 
  coord_flip() +
  facet_wrap(~年代) + guides(color = F)


ggplot(sf_all ,  aes(x = 年代, y = n , color = symptom , label = symptom)) + 
  geom_point() + geom_text_repel() + guides(color = F)



ggplot(gf1 , aes(x = some   , y  = non , label = g)) + 
  geom_point(size = 5, aes(color = g))  + 
  geom_text_repel(size = 3 , alpha = 0.7 , family = "POP") + 
  guides(color = F , shape = F) + 
  geom_abline(alpha = 0.3) + 
  scale_x_continuous(breaks = seq(from = 0 , to = 20, by = 5) , limits = c(0,20)) + 
  scale_y_continuous(breaks = seq(from = 0 , to = 20, by = 5), limits = c(0,20)) + 
  labs(x = "なんらかの症状があった人数" , 
       y = "無症状だった感染者数" , 
       title = "足立区7月備考あり事例(ほぼ集団感染?)") +
  theme(text = element_text() , 
  plot.title= element_text(hjust = 0.5 , size = 12) ,
  axis.text.x = element_text(angle = 0))

df1 %>% filter(備考 == "2020/07/20に報道機関へ情報提供し公表") %>%  group_by(症状) %>% 
  summarize(n = n()) %>% ungroup　%>% arrange(desc(n))


df1 %>% filter(!is_na_biko) %>%  group_by(症状) %>% 
  summarize(n = n()) %>% ungroup　%>% arrange(desc(n))
df1 %>% filter(is_na_biko) %>%  group_by(症状) %>% 
  summarize(n = n()) %>% ungroup　%>% arrange(desc(n))


df1 %>%  group_by(is_na_biko , 症状) %>% 
  summarize(n = n()) %>% ungroup　%>%
  spread(key = is_na_biko , value = n)
