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

lf_total = lf0 %>% group_by(Date) %>% 
  summarize(name = "total" , 
            n_patient = sum(n_patient ,na.rm = T)) 
lf0 = lf0 %>% bind_rows(lf_total)
lf_ex新宿 = lf0 %>% filter(!(name %in% c("新宿区", "total"))) %>% group_by(Date) %>% 
  summarize(name = "ex新宿" , 
            n_patient = sum(n_patient ,na.rm = T)) 
lf0 = lf0 %>% bind_rows(lf_ex新宿)

lf0 = lf0 %>% arrange(Date) %>% group_by(name) %>% mutate(
  diff1 = n_patient - lag(n_patient) ,
  diff_diff1 = diff1 - lag(diff1) , 
  diff7 = n_patient - lag(n_patient , 7)) %>% ungroup
lf1 = lf0 %>% filter(Date >= max(Date) - 30) 
windowsFonts("HGP" = windowsFont("HGP創英角ゴシックUB"))
windowsFonts("MEI"=windowsFont("Meiryo"))
#windowsFonts("BIZ"=  windowsFont("BIZ UDPゴシック"))
windowsFonts("BIZ"=  windowsFont("BIZ UDPGothic"))
windowsFonts("POP"=  windowsFont("HGPSoeiKakupoptai"))
theme_set(theme_minimal(base_family = "POP"))



sf = lf1 %>%  group_by(name) %>% summarise(mean = mean(diff1) , sd = sd(diff1)) %>% ungroup


ordered_names  = sf %>% arrange(mean) %>% pull(name)
lf1 = lf1 %>% mutate(name = factor(name , ordered_names))
top_names  = ordered_names %>% tail(10) #%>% c("ex新宿")


run_diff1 = lf1 %>% filter(name %in% top_names)%>% 
  select(Date , name ,diff1)  %>% spread(key = name , value = diff1) 
run_diff_diff1 = lf1 %>% filter(name %in% top_names)%>% 
  select(Date , name ,diff_diff1)  %>% spread(key = name , value = diff_diff1) 
#run_diff_diff1 = run_diff_diff1 %>% mutate(ex新宿 = total - 新宿区)
cor_diff_diff1 = cor(run_diff_diff1%>% select(-Date)  )
library(reshape2)
cor_dif_diff1_melt = melt(cor_diff_diff1) %>% mutate(Var1 = factor(Var1 , ordered_names) , Var2 = factor(Var2 , ordered_names)) 
# http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
ggplot(cor_dif_diff1_melt , aes(x  = Var1 , y = Var2 , fill = value)) + geom_tile() + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") + labs(x = NULL , y= NULL , title = "直近30日間　新規感染者数の前日からの変化")


cor.test(run_diff_diff1%>% pull(新宿区), run_diff_diff1 %>% pull(世田谷区))
ggplot(lf1 %>% dplyr::filter(name %in% c("新宿区" , "世田谷区"))   , 
       aes(color = name , x = Date , y = diff_diff1)) + geom_line()  
(g = ggplot(run_diff_diff1 , aes(x = 新宿区 ,  y = 世田谷区 , 
                            size = Date , alpha = Date , color = Date , 
                            label = format(Date,  "%m/%d"))) +
  geom_path() + 
  #geom_point(aes(size = as.numeric(Date - min(Date))))  + 
#  geom_point()  + 
  geom_label() + guides(color = F , alpha = F , size = F))

anim = g + transition_reveal(Date) #+ shadow_mark(color = "black" , alpha = 0.3)
fps = 3
ga = animate(anim , fps = fps , #nframes = length(days) * 10, 
             width = 450 , height = 300 , end_pause = 1 * fps)
if (F) {
  ga = animate(anim , nframes = nrow(run_diff_diff1) * 2 , 
               width = 450 , height = 300 , end_pause = 1 * fps)
}
ga
anim_save(filename = glue("covid_tokyo_fluctuation_shinjuku_setagaya_diff_diff.gif"), ga)
(g = ggplot(run_diff1 , aes(x = 新宿区 ,  y = 世田谷区 , 
                            size = Date , alpha = Date , color = Date , 
                            label = format(Date,  "%m/%d"))) +
  geom_path() + 
  #geom_point(aes(size = as.numeric(Date - min(Date))))  + 
#  geom_point()  + 
  geom_label() + guides(color = F , alpha = F , size = F))

anim = g + transition_reveal(Date) #+ shadow_mark(color = "black" , alpha = 0.3)
fps = 3
ga = animate(anim , fps = fps , #nframes = length(days) * 10, 
             width = 450 , height = 300 , end_pause = 1 * fps)

ga
anim_save(filename = glue("covid_tokyo_fluctuation_shinjuku_setagaya_diff.gif"), ga)



# unit root test
library(tseries)
adf.test(run_diff_diff1$新宿区)
adf.test(run_diff_diff1$世田谷区)
if (F) {
  ggcorr(run_diff_diff1)
  library(jtool)
  
  ols1 = lm(Sepal.Length ~ Sepal.Width + Species , iris)
  jtools::summ()
  library(stargazer)
  stargazer(ols1)
  jtools::summ(ols1)
  
  ggplot(lf1 , aes(x = name , y = diff1)) + geom_boxplot()+coord_flip()
  
  
  
  ggplot(sf , aes(x = sd , y = mean , label = name)) + geom_point() + geom_text_repel(alpha = 0.3)
}