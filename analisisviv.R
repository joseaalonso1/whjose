rm(list = ls())
pacman::p_load(readxl,reldist,readr,foreign,data.table,tidyverse,xlsx)

df <- read_csv("finviv.csv", locale = locale(encoding = "ISO-8859-1"), col_names = T) %>% janitor::clean_names() 
df1 <- df %>%
  filter(entidad_federativa %in% c("Ciudad de México") & destino_del_credito%in% c("Vivienda usada","Viviendas nuevas"))

df2 <- df1 %>%  group_by(municipio, organismo, rango_valor_de_la_vivienda_uma) %>% mutate(nv=1)%>% 
  summarise(nv=sum(nv),vam=mean(monto), desvs=sd(monto), mini=min(monto), maxi=max(monto)) %>% 
  arrange( -vam) %>% 
  mutate(vam=vam/1000000,
         mini=mini/1000000,
         maxi=maxi/1000000)
write.csv(df2, "alvcdmx.csv")