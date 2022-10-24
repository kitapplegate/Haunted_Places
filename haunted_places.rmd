---
title: "Haunted Places"
author: "Kit Applegate"
date: '2022-10-24'
output: gitub_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(
	message = FALSE,
	warning = FALSE,
	include = FALSE
)
```

## Happy Halloween!

```{r read and libraries, message=FALSE, warning=FALSE, include=FALSE}
library(tidyverse)
library(geojsonio)
library(RColorBrewer)
library(rgdal)
library(broom)
library(rgeos)


places <- read.csv("data/haunted_places.csv")

spdf <- geojson_read("data/us_states_hexgrid.geojson",  what = "sp")

spdf@data = spdf@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))

spdf@data = spdf@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))
spdf_fortified <- tidy(spdf, region = "google_name")

centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))

```

```{r wrangle and explore, message=FALSE, warning=FALSE, include=FALSE}


ghost.count <- places %>%
  group_by(state) %>%
  summarise(ghosts = n())


```

```{r viz, echo=FALSE, message=FALSE, warning=FALSE}

library(viridis)
my_palette <- rev(magma(8))[c(-1,-8)]


spdf_fortified <- spdf_fortified %>%
  left_join(. , ghost.count, by=c("id"="state"))

spdf_fortified$bin <- cut( spdf_fortified$ghosts , breaks=c(0, 200, 400, 600, 800, 1000, 1200), labels=c("0-200", "200-400", "400-600", "600-800", "800-1000", "1000+" ), include.lowest = TRUE )
    

ggplot() +
  geom_polygon(data = spdf_fortified, aes(fill = bin, x = long, y = lat, group = group) , size=0, alpha=0.9) +
  geom_text(data=centers, aes(x=x, y=y, label=id), color="white", size=3, alpha=0.6) +
  theme_void() +
  scale_fill_manual( 
    values=my_palette, 
    name="Haunts", 
    guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) 
  ) +
 labs(title = "Haunted Places by State",
      caption = "Data source: http://theshadowlands.net/places/") +
  theme(
    legend.position = c(0.5, 0.9),
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size= 22, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  )

ggsave(
  filename = "haunting.png",
  device = "png", width = 10, height = 7, units = "in")

```
