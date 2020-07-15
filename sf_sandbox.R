#https://tsukubar.github.io/r-spatial-guide/simple-feature-for-r.html
library(sf)
p1 =  st_point(c(140.112, 36.083))
p1
p1 %>% st_geometry_type()
p1 %>% class
library(mapview)
st_crs(p1)
?st_crs
st_sfc(p1 , crs = 4326) %>% class
?st_sfc
st_crs(p1)

# https://r-spatial.github.io/sf/articles/sf1.html
