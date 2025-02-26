library(tidyverse)
library(xtable)

training.csv <- read_csv("data/essentia.data.csv") 
allentown <- read_csv("data/essentia.data.allentown.csv")

# ctrl, shift, c comments many lines 
# variable <- allentown$overall_loudness
# training.csv %>%
#   group_by(artist) %>%
#   summarise(minimum = min(overall_loudness), 
#             LF = quantile(overall_loudness, 0.25) -1.5*(IQR(overall_loudness)),
#             RF = quantile(overall_loudness, 0.25) +1.5*IQR(overall_loudness),
#             maximum = max(overall_loudness)) %>%
#   # create two new rows of range distances
#   mutate(out.of.range = if_else( variable < minimum | 
#                                    variable > maximum, TRUE, FALSE)) %>%
#   mutate(unusual = if_else( variable < LF | 
#                               variable > RF, TRUE, FALSE)) %>%
#   rowwise() %>% #apply row by row
#   mutate(description = if (out.of.range) {"Out of Range"}
#          else if(unusual){"Unusual"}
#          else{"Within Range"}
#   )

#step 2 function
analysis <- function(feature){
  #isolate feature value from allentown song
  variable <- get(feature, allentown)

  
  data <- training.csv %>%
    # group together artists 
    group_by(artist) %>%
    # create a summary of values of the feature to compare
    summarise(minimum = min(get(feature), na.rm = TRUE), 
              LF = quantile(get(feature), 0.25, na.rm = TRUE) -1.5*(IQR(get(feature), na.rm = TRUE)),
              RF = quantile(get(feature), 0.25, na.rm = TRUE) +1.5*IQR(get(feature), na.rm = TRUE),
              maximum = max(get(feature)), na.rm = TRUE) %>%
    # create two new rows of range distances
    mutate(out.of.range = if_else(is.na(variable) | variable < minimum | 
                                     variable > maximum, TRUE, FALSE)) %>%
    mutate(unusual = if_else(is.na(variable) |  variable < LF | 
                                variable > RF, TRUE, FALSE)) %>%
    rowwise() %>% #apply row by row
    mutate(description = if (out.of.range) {"Out of Range"}
           else if(unusual){"Unusual"}
           else{"Within Range"}, na.rm = TRUE)
  
  newRow <- data %>%
    mutate(feature = feature)
    # select(artist,description) %>%
    # mutate(feature = feature)  %>%
    # pivot_wider(values_from = "description", names_from = "artist")
  
  return(newRow)
}

#processing all numeric data Step 3
good.columns <- colnames(select_if(training.csv, is.numeric))
# had to take out Tone because it had a NA value and can't figure out rn how to 
# remove
good.columns <- good.columns[-82] 
processed.data <- good.columns[1:196] %>%
  map_dfr(analysis)

# making side by side bar plots Step 4
# calculating out all of the features within range 
bar.data <- processed.data %>%
  group_by(artist) %>%
  summarise(proportion = length(which(description == "Within Range"))/ n())

plot1 <- ggplot(data=bar.data)+
  geom_col(aes(x=artist, y = proportion, fill=artist),    
           position = position_dodge(.5)) + 
  geom_hline(yintercept = 0)+
  xlab("Artist")+                        
  ylab("Proportion of features within range")+
  ylim(0,1)+                          
  theme_bw() 

plot1


#isolating a singular categorical value 
categorical.columns <- colnames(select_if(training.csv, is.character))
chords_scale <- get("chords_scale", allentown) 
chords_scale

#calculate the proportion of songs with the same scale 
dat.chords.scale <- training.csv |>
  group_by(artist) |>
  summarise(total = length(get("chords_scale")), count = length(grep("major",get("chords_scale")))) |>
  mutate(proportion = count/total) 

# create a bar plot 
plot2 <- ggplot(data=dat.chords.scale)+
  geom_col(aes(x=artist, y = proportion, fill=artist),    
           position = position_dodge(.5)) + 
  geom_hline(yintercept = 0)+
  xlab("Artist")+                        
  ylab("Proportion of song similarity from chords scale ")+
  ylim(0,1)+                          
  theme_bw() 
plot2



# group together the data by artists
combined_data <- bind_rows(
  All.Get.Out <-
    filter(training.csv, artist == "All Get Out"),
  Manchester.Orchestra <-
    filter(training.csv,artist == "Manchester Orchestra") ,
  Front.Bottoms <-
    filter(training.csv,artist == "The Front Bottoms"),
)

# comparing emotional data
ggplot(combined_data) +
  geom_boxplot(aes(x = emotion, y = artist, fill = artist)) +
  geom_vline(xintercept = allentown$emotion, size =1, linetype = "dashed") + 
  labs(
    x = "emotional values",
    y = "Artist"
  ) +
  theme_bw() +
  theme(legend.position = "none")

