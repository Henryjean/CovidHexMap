library(tidyverse)  
library(httr) 
library(jsonlite)
library(lubridate)
library(paletteer)
library(extrafont)
library(sf)
library(cowplot)
library(prismatic)
library(here)
library(magick)
library(ggtext)

#custom theme
theme_owen <- function () { 
  theme_minimal(base_size=12, base_family="Gill Sans MT") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = "floralwhite"),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank()
    )
}

#read in pouplation data. add abbreviations 
pop <- read.csv("popdata.csv")
colnames(pop) <- c("StateName", "population")
pop$state <- c(state.abb, "DC", "PR")


#Get current COVID data from the covid tracking project
url <- "https://covidtracking.com/api/states"
df <- fromJSON(url) %>% as.data.frame

#Clean up the date time
df$date <- ymd(substr(df$dateModified, 1, 10))


#Select the variables we care about and merge with the population data
df <- df %>% select(state, positive, hospitalized, death, total, date)
df <- left_join(df, pop, by = "state")


#Calculate total positive cases and death per millino
df$total.positives.per.million <- (df$positive / df$population) * 1000000
df$total.deaths.per.million <- (df$death / df$population) * 1000000


#Get hex boundaries 
us_hex <- st_read("Hex States Shapefile/HexStates.shp") %>%
  janitor::clean_names() %>%
  mutate_if(is.factor, as.character) %>%
  st_transform(crs = 3395)


#Combine hex boundaries with our covid data
df <- left_join(us_hex, df, by = c("state_abbr" = "state"))


#Find the center of each hex (state) so that we can add text 
centers <- 
  st_centroid(us_hex) %>% 
  st_coordinates() %>% 
  as_tibble() %>% 
  set_names(str_to_lower)

df_center <- tibble(abbr = us_hex$state_abbr) %>% 
  bind_cols(centers)

#Combine the centeroid data with the orginial data frame
df <- left_join(df, df_center, by = c("state_abbr" = "abbr"))


#Make the text color white if it's above the median deaths per million and black otherwise
df$textColor <- ifelse(df$total.deaths.per.million >= median(df$total.deaths.per.million, na.rm  = TRUE), "white", "black")


#Create an outline of the United States
us_hex_outline <- us_hex %>%
  st_union() %>%
  st_buffer(dist = 30000)


#Create plot
p <- df %>%
  ggplot() +
  geom_sf(aes(fill = total.deaths.per.million), size = 1, color = 'white') +
  geom_sf(data = us_hex_outline, color = "#F7945DFF", fill = "transparent", size = 1) +
  geom_text(data = df, aes(x, y, label = state_abbr, color = textColor), 
            family = "Gill Sans MT", size = 2.25, fontface = 'bold') +
  geom_text(data = df, aes(x, y, label = round(total.deaths.per.million, 2), color = textColor), 
            family = "Gill Sans MT", size = 2, fontface = 'bold', vjust = 2.5)+ 
  rcartocolor::scale_fill_carto_c(
    name = "Confirmed Deaths Per Million Residents",
    palette = 10,
    trans = "log10", 
    breaks = scales::log_breaks(n = 8)) +
  theme_owen()  +
  scale_color_identity() +
  theme(text=element_text(size=14,  family="Gill Sans MT"), 
        plot.title = element_text(hjust = 0.5, face = "bold",  vjust = 0, size = 15),
        plot.subtitle = element_text(hjust = 0.5, size = 9, vjust = 0), 
        plot.caption = element_text(face = "italic", size = 8, hjust = .5, vjust = 0), 
        legend.spacing.x = unit(0, 'cm'), 
        legend.title=element_text(size=11), 
        legend.text = element_text(size = rel(0.6)), 
        legend.margin=margin(10,0,-1,0),
        legend.position = 'bottom',
        plot.margin = margin(0, -.5, 0, -.5, "cm"), 
        legend.box.margin=margin(-30,0,15,0))  +
  guides(fill=guide_legend(
    keywidth=.5,
    keyheight=.15,
    default.unit="inch", 
    label.position = 'bottom', 
    title.position = 'top',
    title.hjust = .5,
    title.vjust = 0,
    label.vjust = 3,
    nrow = 1)) +
  labs(title = "Confirmed Deaths Due To COVID-19 Per Million Residents", 
       caption  = paste0("Data updated ", format(Sys.time(), "%b %d %X")), 
       subtitle = paste0("Total Confirmed Deaths Due To COVID-19 In The United States: ", scales::comma_format()(sum(df$death, na.rm = TRUE))))


#Use the cowplot package to color in the white area
cowplot::ggdraw(p) + 
  theme(plot.background = element_rect(fill="floralwhite", color = NA))


#Add custom footer image
ggsave("DeathsPerMillion.png", width = 6, height = 6, dpi = 300)
footy <- image_read("Footer.png")
graf <- image_read("DeathsPerMillion.png")
image_composite(graf, footy, offset = "+0+1745") %>% image_write("DeathsPerMillion.png")

