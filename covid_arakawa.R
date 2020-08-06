rm(list = ls())
library(readr)
df = read_csv("https://www.city.adachi.tokyo.jp/documents/47358/131200_city_adachi_covid19_patients0806.csv")
start_date  =as.Date("2020-7-1")
end_date  = as.Date("2020-7-31")
df = df  %>% mutate(is_na_biko = is.na(備考))
df1 = df %>% filter(診断日 >= start_date & 診断日 <= end_date) 
df1 %>% dim

df1 %>% group_by(is_na_biko) %>% summarize(n = n())
df1 %>% group_by(備考) %>% summarize(n = n()) %>% arrange(desc(n))
gf1 = df1 %>% filter(!is_na_biko) %>% mutate(shojo  = ifelse(症状 == "無症状" , "non", "some")) %>% 
  group_by(備考, shojo) %>% summarize(n = n()) %>% 
  arrange(desc(n)) %>% spread(key = shojo , value = n)
ggplot(gf1 , aes(x = some , y  = non)) + geom_point()

df1 %>% filter(備考 == "2020/07/20に報道機関へ情報提供し公表") %>%  group_by(症状) %>% 
  summarize(n = n()) %>% ungroup　%>% arrange(desc(n))


df1 %>% filter(!is_na_biko) %>%  group_by(症状) %>% 
  summarize(n = n()) %>% ungroup　%>% arrange(desc(n))
df1 %>% filter(is_na_biko) %>%  group_by(症状) %>% 
  summarize(n = n()) %>% ungroup　%>% arrange(desc(n))


df1 %>%  group_by(is_na_biko , 症状) %>% 
  summarize(n = n()) %>% ungroup　%>%
  spread(key = is_na_biko , value = n)
