rm(list = ls())
library(tidyverse)
library(readr)
if (T) {
    df = read_csv("https://www.city.adachi.tokyo.jp/documents/47358/131200_city_adachi_covid19_patients1002.csv")
#  df = read_csv("https://www.city.adachi.tokyo.jp/documents/47358/131200_city_adachi_covid19_patients0807.csv")
  #df = read_csv("https://www.city.adachi.tokyo.jp/documents/47358/131200_city_adachi_covid19_patients0812.csv")
#  df = read_csv("https://www.city.adachi.tokyo.jp/documents/47358/131200_city_adachi_covid19_patients0814_2.csv")
  df = df %>% mutate(g = str_replace_all(備考, "(2020/|し公表)" , ""))
} else {
  df = read_csv("adachi.csv") #with URL
  df = df %>% mutate(診断日 = as.Date(診断日 , "%Y/%m/%d"))
  df = df %>% mutate(url_short = str_replace_all(URL , 
                     "(https://www.city.adachi.tokyo.jp/hodo/juyo/|.html)" , ""))
  df = df %>% mutate(g = url_short)
}
df = df %>% mutate(年代 = str_replace(年代 , "３０代" , "30代") %>%
                        str_replace( "５０代" , "50代") %>%
                       str_replace( "２０代" , "20代") %>%
                       str_replace("8０代", "80代") %>% 
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
                     str_replace_all("発熱嗅覚", "発熱・嗅覚")%>%
                     str_replace_all("嗅覚味覚", "嗅覚・味覚")) %>%
  mutate(symptom = ifelse(str_count(symptom , "急性呼吸器") > 0 , "急性呼吸器症状" , symptom))


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
if (T) {
  df1b = df1
  for (s in symptoms) {
      df1b = df1b %>% mutate(!!s := pmin(1 ,  str_count(symptom  , 　s)  ))
  }
  #https://stats.stackexchange.com/questions/408564/relationship-independence-between-2-binary-varibales
  #https://oku.edu.mie-u.ac.jp/~okumura/stat/fishertest.html
  cor.test(df1b$咳 , df1b$発熱)
  count_a = df1b %>% group_by(咳, 発熱) %>% summarize(n = n())
  count_a
  fisher.test(matrix(count_a$n , nrow = 2))
  cor.test(df1b$味覚 , df1b$嗅覚)
  df1b %>% group_by(味覚, 嗅覚) %>% summarize(n = n())
  count_b = df1b %>% group_by(味覚, 嗅覚) %>% summarize(n = n())
  fisher.test(matrix(count_b$n , nrow = 2))
  
  us
  fisher.test(matrix(df1b %>% group_by(味覚, 発熱) %>% summarize(n = n()) %>% pull(n) , nrow = 2))
  fisher.test(matrix(df1b %>% group_by(嗅覚, 発熱) %>% summarize(n = n()) %>% pull(n) , nrow = 2))
  fisher.test(matrix(df1b %>% group_by(頭痛, 発熱) %>% summarize(n = n()) %>% pull(n) , nrow = 2))
  fisher.test(matrix(df1b %>% group_by(咽頭痛, 発熱) %>% summarize(n = n()) %>% pull(n) , nrow = 2))



}
sf2 = sf_all %>% spread(key = run_sym , value = n , fill = 0 )  %>%
  rename(with = "TRUE"  , without = "FALSE")
ordered_sym = sf2 %>% group_by(symptom) %>% summarize(total = sum(with)) %>% ungroup %>% arrange(desc(total))%>% pull(symptom)
sf2 = sf2 %>% mutate(symptom = factor(symptom , ordered_sym))

sf3 =
library(tidyverse)
library(ggrepel)

windowsFonts("YuGo" = windowsFont("游ゴシック"))
#windowsFonts("YuGo" = windowsFont("HG創英角ゴシックUB"))
windowsFonts("POP"=  windowsFont("HGPSoeiKakupoptai"))
theme_set(theme_minimal(base_family = "POP"))
theme_set(theme_minimal(base_family = "YuGo"))


ggplot(df1 %>% group_by(年代) %>% summarize(n = n()), aes(x = 年代, y = n , 
                                                        fill = 年代, 
                                                        color = 年代 )) +
  geom_bar(stat = "identity") + guides(color = F, fill = F)

ggplot(df1 %>% group_by(年代, is_na_biko) %>% summarize(n = n()) %>% 
         spread(key = is_na_biko , value = n , fill = 0) %>% 
         rename("備考なし" = "TRUE" , "備考あり" = "FALSE")
         , aes(x = 備考なし, y = 備考あり   , label = 年代 , color = 年代) ) +
  geom_point() + geom_text_repel() + guides(color = F, fill = F) + 
  geom_smooth(method = "lm" , se = F)

ggplot(df1 %>% group_by(年代, is_na_biko) %>% summarize(n = n()), aes(x = 年代, y = n , 
                                                        fill = 年代, 
                                                        color = 年代 )) +
  geom_bar(stat = "identity") + guides(color = F, fill = F) + 
  labs(x = NULL , y = "人数") +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~is_na_biko , labeller = labeller(is_na_biko = c("TRUE" = "備考なし" , "FALSE" = "備考あり")))



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



ggplot(sf2_age%>% filter(symptom %in% symptoms[1:12]) ,  aes(y = 年代, x = with / (with + without) , 
                  fill = 年代, 
                  color = 年代 )) + 
  geom_bar(stat = "identity") + 
  scale_x_continuous(labels = scales::percent , name = "比率") + 
  #coord_flip() +
  labs(y = NULL) +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~symptom , nrow = 2) + guides(color = F, fill = F)



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


sf2_age_biko  =sf2 %>% group_by(is_na_biko , 年代, symptom) %>% summarize(with = sum(with) , without = sum(without)) %>% ungroup

sf2_age_biko = sf2_age_biko  %>% mutate(ratio = with / (with + without))

wf2_age_biko  = sf2_age_biko %>% select(-starts_with("with"))%>% 
  spread(key = is_na_biko , value = ratio) %>%
  rename("備考あり" = "FALSE"  , "備考なし" = "TRUE")


ggplot(wf2_age_biko , aes(x = 備考なし ,y  = 備考あり , color = 年代 , label = 年代)) + 
  geom_point()　+ geom_text_repel(size = 2) + facet_wrap(~symptom) + guides(color = F, fill = F)


sf2_biko  =sf2 %>% group_by(is_na_biko , symptom) %>% summarize(with = sum(with) , without = sum(without)) %>% ungroup

sf2_biko = sf2_biko  %>% mutate(ratio = with / (with + without))

wf2_biko  = sf2_biko %>% select(-starts_with("with"))%>% 
  spread(key = is_na_biko , value = ratio) %>%
  rename("備考あり" = "FALSE"  , "備考なし" = "TRUE")


ggplot(wf2_biko , aes(x = 備考なし ,y  = 備考あり , label = symptom , color = symptom)) + 
  geom_point()　+ geom_text_repel(size = 2)  + guides(color = F, fill = F) + geom_abline(alpha = 0.5)



# delete below 
