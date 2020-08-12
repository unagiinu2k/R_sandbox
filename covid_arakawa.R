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
start_date  =as.Date("2020-1-1")
end_date  = as.Date("2020-12-31")
#df = df  %>% mutate(is_na_biko = str_count(備考, "集団") == 0)
df = df  %>% mutate(is_na_biko = is.na(g))
df1 = df %>% filter(診断日 >= start_date & 診断日 <= end_date) 
df1 %>% dim

df1 %>% group_by(is_na_biko) %>% summarize(n = n())
df1 %>% group_by(URL) %>% summarize(n = n()) %>% arrange(desc(n))
gf1 = df1 %>% filter(!is_na_biko) %>% 
  mutate(shojo  = ifelse(症状 == "無症状" , "non", "some")) %>% 
  group_by(g, shojo) %>% summarize(n = n()) %>% 
  arrange(desc(n)) %>% spread(key = shojo , value = n , fill = 0)
library(tidyverse)
library(ggrepel)

windowsFonts("YuGo" = windowsFont("游ゴシック"))
#windowsFonts("YuGo" = windowsFont("HG創英角ゴシックUB"))
windowsFonts("POP"=  windowsFont("HGPSoeiKakupoptai"))
theme_set(theme_minimal(base_family = "POP"))
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
