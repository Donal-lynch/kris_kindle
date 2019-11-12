library(dplyr)
set.seed(1)

# vector of participants
fam <- data.frame(name = c('ange',
                           'kev',
                           'do',
                           'cat',
                           'aideen',
                           'martin',
                           'claire',
                           'nathan'),
                  couple = rep(1:4, each = 2)) %>% 
  mutate(index = 1:n())



invalid_config <- TRUE
counter <- 0

while (invalid_config){
  
  print(paste('attempt number', counter))
  
  # create random index to merge recursively
  fam_present <- fam %>% 
    mutate(rand_ind = sample(1:n(),
                             size = n(),
                             replace = FALSE)) %>% 
    select(name_g = name, couple_g = couple, rand_ind) %>% 
    merge(y = fam,
          by.x = "rand_ind",
          by.y = "index",
          all = TRUE) %>% 
    select(name_g, name_r = name, couple_g, couple_r = couple)
  
  # Check nobody got themselves or their partner
  invalid_config <-  fam_present %>% 
    mutate(test = (name_g == name_r) | (couple_g == couple_r))%>% 
    pull(test) %>% 
    any()
  
  
  counter <- counter + 1
  # just in case
  if (counter == 100){break}
}


file_loc <- "????"

for(i in nrow(fam_present)){
  
  file_contents <- paste0("Happy Christmas ", fam_present$name_g[i],
                         ". You have to get a present for ", fam_present$name_r[i])
    
  write.table(x = file_contents,
              file = paste0(file_loc,
                            fam_present$name_g[i],
                            '.txt'),
              quote = FALSE,
              row.names = FALSE,
              col.names = FALSE)
}


