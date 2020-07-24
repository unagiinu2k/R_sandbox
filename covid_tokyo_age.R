rm(list = ls())
#library(rjson)
library(tidyverse)
library(readr)
df  = read_csv("https://stopcovid19.metro.tokyo.lg.jp/data/130001_tokyo_covid19_patients.csv")
#df %>% tail %>% View
df = df %>% mutate(患者_年代 = ifelse(患者_年代 == "'-", "不明",患者_年代))

df = df %>% mutate(age = ifelse(患者_年代 ==  "10歳未満"  , 0 , as.numeric(str_replace(患者_年代 , "代|歳以上", ""))))
sf = df %>% group_by(公表_年月日,患者_年代 , age) %>% summarize(n = n()) %>% ungroup
complemented = expand.grid(
  公表_年月日 = seq(from = min(sf$公表_年月日) , to = max(sf$公表_年月日)  , by = "1 day"),
  患者_年代 = unique(sf$患者_年代))
sf = complemented %>% left_join(sf)
sf = sf %>% tidyr::replace_na(list(n = 0))

sf = sf %>% arrange(公表_年月日,患者_年代)
library(RcppRoll)
monitor_period = 7
sf = sf %>% group_by(患者_年代) %>% mutate(weekly = roll_sumr(n , monitor_period),
                                       cum = cumsum(n)) %>% ungroup

age_order = c( "10歳未満"  ,"10代"    ,  "20代",     
 "30代"  ,    "40代"    ,  "50代"  ,    "60代"  ,    "70代",
 "80代"    ,  "90代",      "100歳以上" , "不明"    , "'-"      )


sf = sf %>% mutate(患者_年代 = factor(患者_年代 , age_order))


windowsFonts("YuGo" = windowsFont("游ゴシック"))
windowsFonts("YuGo" = windowsFont("HG創英角ゴシックUB"))
windowsFonts("POP"=  windowsFont("HGPSoeiKakupoptai"))
theme_set(theme_minimal(base_family = "POP"))
if (F) {
  ggplot(sf %>% filter(公表_年月日 >= max(公表_年月日)- 20) , 
         aes(fill = 患者_年代 , x = 患者_年代 ,y  = n)) + geom_bar(stat = "identity")  + 
    labs(x = NULL , y = "daily new infections") + guides(fill = F)  + 
    scale_fill_brewer(palette = "Paired")   +
    theme(text = element_text(#family ="YuGo"
    ), 
    plot.title= element_text(hjust = 0.5 , size = 12) ,
    axis.text.x = element_text(angle = 45))
}
library(gifski)
library(gganimate)
library(transformr)
library(glue)
experiments = merge(data.frame(freq = c(1,7) , fps = c(12,5)), 
                    data.frame(start_date = c(min(sf$公表_年月日) , as.Date("2020-6-1"))) ,
                    by = NULL)
for (i in 1:nrow(experiments)) {
  freq = experiments$freq[i]
  fps = experiments$fps[i]
  start_date= experiments$start_date[i]
  days = seq(from = max(sf$公表_年月日) , to = start_date , by = glue("-{freq} day")) %>% sort

  anim = ggplot(sf %>% filter(公表_年月日 %in% days)  , aes(fill = 患者_年代 , x = 患者_年代 ,y  = weekly)) +
    geom_bar(stat = "identity")  + 
    labs(x = NULL , y = "weekly new infections" ,
         #title = "一週間の都内新規感染者数\n{closest_state}"
         title = "一週間の都内新規感染者数\n{current_frame}"
         ) +
    scale_fill_brewer(palette = "Paired")   + 
    guides(fill = F) +
    scale_y_continuous(labels = scales::comma) + 
    #transition_states(公表_年月日)  +
    transition_manual(公表_年月日) + 
    theme(text = element_text(#family ="YuGo"
                              ), 
          plot.title= element_text(hjust = 0.5 , size = 14# , family ="YuGo"
                                   ) ,
          axis.text.x = element_text(angle = 45))
  
  ga = animate(anim , fps = fps , #nframes = length(days) * 10, 
               width = 450 , height = 300 , end_pause = 1 * fps)
  ga
  anim_save(filename = glue("covid_tokyo_age_{i}.gif"), ga)
}



# 6月以降、一日毎にx_i,t+1 = a x_i,t + b x_{i+1} , t + b x_{i-1}
sf= sf%>% arrange(公表_年月日 , 患者_年代)
is_weekly = F
start_date = c("2020-6-7" , "2020-6-20")[2]
if (is_weekly) {
  days = seq(from = max(sf$公表_年月日) , to =  as.Date(start_date) , by =  glue("-7 day")) %>% sort
  sf1 = sf %>% filter(公表_年月日 %in% days)
} else {
  sf1 = sf %>% filter(公表_年月日 >= as.Date(start_date))
}
sf1 = sf1 %>% filter(!(患者_年代 %in% c("10歳未満", "10代" , "不明")))


sf1= sf1%>% arrange(公表_年月日 , 患者_年代) %>% 
   group_by(公表_年月日) %>% 
  mutate(weekly_up = lead(weekly , default = 0) ,
         weekly_down = lag(weekly , default = 0),
         weekly_up2 = lead(weekly ,n = 2 ,  default = 0) ,
         weekly_down2 = lag(weekly , n= 2, default = 0),
         cum_weekly = sum(weekly) , 
         weekly_up_all = cumsum(weekly) - weekly , 
         weekly_down_all = cum_weekly - cumsum(weekly) , 
         cum_cum = sum(cum) , 
         cum_up = lead(cum , default = 0) ,
         cum_down = lag(cum , default = 0),
         cum_up2 = lead(cum ,n = 2 ,  default = 0) ,
         cum_down2 = lag(cum , n= 2, default = 0) ,
         cum_down_all = cumsum(cum) - cum ,
         cum_up_all = cum_cum - cumsum(cum)
         ) %>% ungroup

sf1 = sf1 %>% group_by(患者_年代) %>% 
  mutate(weekly_ahead = lead(weekly) ,
         weekly_ahead2 = lead(weekly  , n = 2) ,
         n_ahead = lead(n)
         #,
         #cum = cumsum(weekly) ,
         #cum_up = cumsum(weekly_up) , 
         #cum_down = cumsum(weekly_down) ,
         #cum_up2 = cumsum(weekly_up2) , 
         #cum_down2 = cumsum(weekly_down2) 
         ) %>% ungroup

#sf1 = sf %>% filter(公表_年月日 >= as.Date("2020-6-7") & !(患者_年代 %in% c( "不明")))
library(lubridate)

sf1 = sf1 %>% mutate(month = month(公表_年月日))
#
ols0 = lm(n  ~ 患者_年代:cum + I(cum_up + cum_up2) + I(cum_down + cum_down2)  + 0 , sf1 %>% filter(weekly > 0 & is.finite(weekly)))
ols0 = lm(n  ~ 患者_年代:cum + cum_up_all + cum_down_all  + 0 , sf1 %>% filter(weekly > 0 & is.finite(weekly)))
ols0 = lm(n_ahead  ~ weekly + weekly_up_all + weekly_down_all  + 0 , sf1 %>% filter(weekly > 0 & is.finite(weekly)))
ols0 = lm(n_ahead  ~ 患者_年代:weekly + weekly_up_all + weekly_down_all  + 0 , sf1 %>% filter(weekly > 0 & is.finite(weekly)))
ols0 = lm(n_ahead  ~ 患者_年代:weekly   + 0 , sf1 %>% filter(weekly > 0 & is.finite(weekly)))
summary(ols0)
if (F) {
  ols0 = lm(I(weekly_ahead / weekly-1) ~ (cum + cum_up + cum_down)  + 0 , sf1 %>% filter(weekly > 0 & is.finite(weekly)))
  ols0 = lm(I(weekly_ahead / weekly-1) ~ 患者_年代:cum + cum_up + cum_down  + 0 , sf1 %>% filter(weekly > 0 & is.finite(weekly)))
  ols0 = lm(I(weekly_ahead / weekly-1) ~I(age>=60):(cum + cum_up + cum_down)  + 0 , sf1 %>% filter(weekly > 0 & is.finite(weekly)))
  ols0 = lm(weekly  ~ as.character(month):(cum + cum_up + cum_down)  + 0 , sf1 %>% filter(weekly > 0 & is.finite(weekly)))
}
if (F) {
  ols0 = lm(weekly_ahead ~患者_年代:(weekly + weekly_up + weekly_down)  , sf1)
  ols0 = lm(weekly_ahead ~I(age >= 60) : (weekly + weekly_up + weekly_down)   , sf1)
  ols0 = lm(weekly_ahead ~weekly + weekly_up + weekly_down   , sf1)
  
  ols0 = lm((weekly_ahead / weekly) ~I(age>=60):(cum + cum_up + cum_down)   , sf1 %>% filter(weekly > 0 & is.finite(weekly)))
  ols0 = lm(weekly_ahead ~weekly + weekly_up + weekly_down + weekly_up2 + weekly_down2  + 0 , sf1)
  ols0 = lm(weekly_ahead ~weekly + I(weekly_up + weekly_down)   , sf1)
  ols0 = lm(weekly_ahead ~weekly + I(weekly_up + weekly_down) + I(weekly_up2+ weekly_down2) + 0  , sf1)
}

library(sjPlot)
library(sjmisc)
library(sjlabelled)