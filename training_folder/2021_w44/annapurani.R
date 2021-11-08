library(tidyverse)
library(tidytuesdayR)
library(ggridges)
library(ggtext)
ultra_rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/ultra_rankings.csv')
race <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/race.csv')

glimpse(race)

race%>%
  distinct()%>%
  select(event,country)%>%
  group_by(country)%>%
  count()%>%
  arrange(desc(n))%>%
  data.frame()->countries
  
race%>%
  mutate(Year=year(date))%>%
  distinct()%>%
  filter(country=="United States"|country=="United Kingdom"|country=="France"
         |country=="Australia"|country=="Sweden")%>%
  select(Year,event,country)%>%
  group_by(Year,country)%>%
  count(event)%>%
  summarise(Total=sum(n))%>%
  data.frame()->ridge

ridge%>%
  mutate(country=fct_relevel(country,levels="Sweden", "Australia", "France","United Kingdom","United States"))->ridge1

ggplot(ridge1,aes(x=Total,y=country,fill=stat(quantile)))+
  stat_density_ridges(geom="density_ridges_gradient",
                      calc_ecdf = TRUE)+
  scale_fill_manual(values=c("#961a1a","#d9ead3","#134f5c","#0c343d"))+
    theme(axis.text.y=element_text(color="black", size=12, hjust=.5, face = "bold"),
          axis.text.x=element_text(color="black", size=10),
        axis.title=element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(.5, 1, .5, 1), "cm"),
        plot.background = element_rect(fill="#f4f4f8"),
        panel.background = element_rect(fill="#f4f4f8"),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        legend.position = "none",
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title=element_text(size=16, colour = "black", face="bold"),
        plot.subtitle=element_markdown(size=12, margin=margin(b=15)),
        plot.caption=element_text(hjust=0, size=9, margin=margin(t=15)))+
  labs(title="ULTRA-RUNNING EVENTS OVER THE YEARS",
       subtitle="The number of ultra-running events conducted from 2012 to 2021 by the top five countries that have organized the highest number of events thus far",
       caption = "Data:BjnNowak-Github Repo via Tidy Tuesday | Design: @annapurani93")->ridgeplot

     ggsave("ridgeplot.png",ridgeplot,width=12,height=6.1)
