sccei_raw <- read.csv("~/Downloads/Amuna.csv")

#install.packages("wordcloud2")
library(wordcloud2)

colnames(sccei_raw) <-
  c('Text', 'Url')

library(RColorBrewer)
library(wordcloud)
library(wordcloud2)
library(tidytext)
library(tidyverse)
library(sf)
sccei_raw$Text[116]

sccei_cleann <- sccei_raw |>
  filter(!row_number() %in% c(1, 27:29, 55:59, 85:88, 114:116))

view(sccei_cleann)

sccei_clean <- sccei_cleann |> 
  unnest_tokens(input = Text,
                output = "word") |>
  count(word) |>
  arrange(-n) |>
  filter(!(word %in% stop_words$word), 
         !is.na(word))

view(sccei_clean)

sccei_clean1 <- sccei_clean |>
  filter(!str_detect(word, "[:digit:]"),
         !str_detect(word, "data|china|china's|stanford|university|u.s|freeman|california|spogli|center|chinese")) |>
  head(100) 


view(sccei_clean1)

 wordcloud(
  words = sccei_clean1$word,
  freq = sccei_clean1$n,
  max.words = 80,
  random.order = FALSE,
  scale = c(4, 0.5),
  rot.per = 0.35,
  colors = brewer.pal(8, "Dark2"),
  backgroundColor = "bisque"
  )
  title("SCCEI Briefs Word Clouds")





library(ggplot2)
library(maps)
#install.packages('magick')
library(magick)
#install.packages('maps')


normalizePath("~/Downloads/converted_image.png")

file.exists("~/Downloads/converted_image.png")


print(normalizePath("~/Downloads/converted_image.png"))

img <- image_read("~/Downloads/converted_image.png")
print(img)


# img <- image_read("~/Downloads/converted_image.png") |>
#   image_resize("500x500") |>
#   image_convert(type = "grayscale") |>
#   image_threshold("white", "50%")  
#image_write(img, "~/Downloads/converted_image.png", format = "png")


# file.exists(mask)
# print(mask)

mask <- "~/Downloads/china_mask_fixed.png"
wordcloud2(data = sccei_clean1, 
           figPath = 'star',        
           size = 1,              
           color = brewer.pal(8, "PiYG"), 
           backgroundColor = "pink")


sccei_clean2 <- sccei_clean1 |>
  select(-x,-y)

# sccei_clean3 <- sccei_clean2 |>
#   select(-x,-y)


wordcloud2(data = sccei_clean2, 
           size = 1, 
           shape = "star",
           color = "darkred", backgroundColor="bisque")



#install.packages("rnaturalearth")
library(rnaturalearth)
#install.packages('ggwordcloud')
library(ggwordcloud)
library(rnaturalearthdata)

install.packages('rnaturalearthhires')

china_sf <- ne_countries(scale = "medium", country = "China", returnclass = "sf")


bbox <- st_bbox(china_sf)


china_sf <- ne_countries(scale = "small", country = "China", returnclass = "sf")

# Generate evenly distributed sample points INSIDE China's shape
set.seed(123)  # For reproducibility
num_points <- nrow(sccei_clean2) * 4  # Generate extra points for better selection
sample_points <- st_sample(china_sf, num_points, type = "random") |> st_as_sf()

# Convert sample points to dataframe
sample_df <- as.data.frame(st_coordinates(sample_points))

# Keep only as many points as we have words
sample_df <- sample_df %>% head(nrow(sccei_clean2))

# Add words and frequencies back to the valid points
sample_df$word <- sccei_clean2$word[1:nrow(sample_df)]
sample_df$freq <- sccei_clean2$freq[1:nrow(sample_df)]

ggplot() +
  geom_sf(data = china_sf, fill = "aliceblue", color = "darkseagreen") +  # Map of China
  geom_text_wordcloud(data = sample_df, 
                      aes(x = X, y = Y, label = word, size = freq),
                      color = "darkred") +  # Words inside China
  scale_size_area(max_size = 6) +
  theme_void()






















ggplot() +
  geom_sf(data = china_sf, fill = "white", color = "black") +  
  geom_text_wordcloud(data = sccei_clean2, 
                      aes(label = word, size = freq),
                      color = "darkred", family = "serif") +
  scale_size_area(max_size = 5) +
  theme_void() +
  coord_sf(xlim = st_bbox(china_sf)[c(5, 9)],  # Clip to China's bounding box
           ylim = st_bbox(china_sf)[c(2, 4)])



wordcloud2(demoFreq, 
           figPath ="~/Downloads/MapChart_Map.png",
           size = 10,
           color = "darkred", 
           backgroundColor="black")



colnames(sccei_clean2) <- c("word", "freq")
demoFreq <- sccei_clean2

wordcloud2(demoFreq, 
           figPath ="~/Downloads/MapChart_Map.png",
           size = 10,
           color = "darkred", 
           backgroundColor="black")
file.exists("~/Downloads/MapChart_Map.png")







