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
  
  region_code = region_code[-1,2:4] 
  names(region_code) = c("name" , "Kana" , "code")
  region_code = region_code %>% mutate(code = as.integer((code)))
  write_tsv(region_code , "region_code.tsv")
  
  
}
region_code = read_tsv("region_code.tsv") %>% mutate(code  =as.integer(code))

lf0 = lf0 %>% left_join(region_code)
lf0 %>% head
lf0 %>% dim

?map_dfr

map_dfr()
