library(tidyverse)

training.csv <- read_csv("data/essentia.data.csv")
allentown <- read_csv("data/essentia.data.allentown.csv")

variable <- allentown$overall_loudness
training.csv %>%
  group_by(artist) %>%
  summarise(minimum = min(overall_loudness), 
            LF = quantile(overall_loudness, 0.25) -1.5*(IQR(overall_loudness)),
            RF = quantile(overall_loudness, 0.25) +1.5*IQR(overall_loudness),
            maximum = max(overall_loudness)) %>%
  # create two new rows of range distances
  mutate(out.of.range = if_else( variable < minimum | 
                                   variable > maximum, TRUE, FALSE)) %>%
  mutate(unusual = if_else( variable < LF | 
                              variable > RF, TRUE, FALSE)) %>%
  rowwise() %>% #apply row by row
  mutate(description = if (out.of.range) {"Out of Range"}
         else if(unusual){"Unusual"}
         else{"Within Range"}
  )

analysis <- function(feature){
  variable <- get(feature, allentown)
    
  training.csv %>%
    group_by(artist) %>%
    summarise(minimum = min(get(feature)), 
              LF = quantile(get(feature), 0.25) -1.5*(IQR(get(feature))),
              RF = quantile(get(feature), 0.25) +1.5*IQR(get(feature)),
              maximum = max(get(feature))) %>%
    # create two new rows of range distances
    mutate(out.of.range = if_else( variable < minimum | 
                                     variable > maximum, TRUE, FALSE)) %>%
    mutate(unusual = if_else( variable < LF | 
                                variable > RF, TRUE, FALSE)) %>%
    rowwise() %>% #apply row by row
    mutate(description = if (out.of.range) {"Out of Range"}
           else if(unusual){"Unusual"}
           else{"Within Range"})
}
analysis("tempo")
