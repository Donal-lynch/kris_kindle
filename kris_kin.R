library(dplyr)
set.seed(42)

# vector of participants
fam <- data.frame(name = c('Angela',
                           'Kev',
                           'Donal',
                           'Cat',
                           'Aideen',
                           'Martin',
                           'Claire',
                           'Nathan'),
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


file_loc <- "C:/Users/Home/Documents/Projects/kris_kindle/"

for(i in 1:nrow(fam_present)){
  
  
  joke <- httr::GET("https://icanhazdadjoke.com",
                     httr::user_agent("dadjoke R package (https://github.com/jhollist/dadjoke)"),
                     httr::accept("text/plain")) %>% 
    httr::content("text", encoding = "UTF-8")
  
  
  
  file_contents <- paste0("A random Christmas cracker joke just for you:\n",
                          joke,
                          '\n\n',
                          "Happy Christmas ", fam_present$name_g[i],
                         ". You have to get a present for ", fam_present$name_r[i])
    
  write.table(x = file_contents,
              file = paste0(file_loc,
                            fam_present$name_g[i],
                            '.txt'),
              quote = FALSE,
              row.names = FALSE,
              col.names = FALSE)
}


