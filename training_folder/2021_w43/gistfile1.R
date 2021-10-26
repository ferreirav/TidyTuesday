library(tidyverse)
library(ggtext)

pumpkins <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-19/pumpkins.csv')

df1 = pumpkins %>% filter(place!="EXH") %>%
  mutate(weight = parse_number(weight_lbs), place= as.numeric(place)) %>%
  drop_na(place) %>%
  mutate(id2 = id) %>% separate(id2, c("year", "type"), sep="-") %>% 
  filter(place<=100) %>%
  filter(type %in% c("F","P","S")) 
  
# ridgeline plot 
df1 %>% filter(type %in% c("L","F","W")) %>%
  mutate(type= recode(type, "L"="**Long Gourd**<br>(length in inches)", "F"="**Field Pumpkin**<br>(weight in pounds)","W"="**Giant Watermelon**<br>(weight in pounds)")) %>%
  ggplot(aes(y=type, x=weight,fill = factor(stat(quantile)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE,
    jittered_points = TRUE,position = position_raincloud(adjust_vlines = TRUE),
    point_size = 0.4, point_alpha = .2, vline_size = 0
  ) +
  scale_fill_manual("Quantile",values=c("#94B1BF","#0A5133","#C08573","#DAB8B6")) +
  theme_minimal(9) +
  theme(axis.text.y=element_markdown(color="black", size=8, hjust=.5, lineheight = 1.2),
        axis.title=element_blank(),
        plot.margin = unit(c(.5, 1, .5, 1), "cm"),
        legend.position = "none",
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title=element_text(size=11, face="bold"),
        plot.subtitle=element_markdown(size=8, margin=margin(b=15)),
        plot.caption=element_text(hjust=0, size=7, margin=margin(t=15)),
        plot.background = element_rect(fill="#fafafa", color=NA)
        ) +
  labs(title="Long Gourd, Giant Watermelon and Field Pumpkin",
       subtitle="Size of the top 100 from 2013 to 2021, according to *Great Pumpkin Commonwealth (GPC)*",
       caption="#TidyTuesday week 43 | Data from BigPumpkins.com")
