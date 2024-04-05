library(dplyr)

#Gives each word a point value based on how many unique mappings it contains. Adds additional points based on how
#rare its mappings are. 
get_word_points <- function(word_index, mappinglist){
  word <- wordlist_v1_1[[word_index,1]] 
  if(wordfrequency[wordfrequency$Word==word, ]$frequency[1] <= 2){      #filters words with a frequency 
    return(0)
  }
  
  if(nchar(word) < 4 || nchar(word) > 13){               #filters out word with less than 3 or more than 13 letters
    return(0)
  }
  
  points <- 0
  rarity <- 0
  for (i in 1:length(scored_words_PG[[word_index]])){
    phoneme <- scored_words_PG[[word_index]][1,i]
    grapheme <- scored_words_PG[[word_index]][2,i]
    row_number <- which(mappinglist$phoneme==inhouse_to_ipa(phoneme) &
                          mappinglist$grapheme==grapheme,)
    
    if(length(row_number) != 0){
      points <- points+100
      rarity <- rarity + 5000/(mappinglist[[row_number, 3]])
    }
  }
  
  if(points >= 200 && rarity > 500){
    points <- points + 5000
  }
  
  if(points >= 200 && rarity > 750){    
    points <- points + 10000
  }
  
  if(points == 100){
    points <- 0
    rarity <- 1
  }
  return(points+rarity)
}

#Returns index of word that currently holds the highest number of points
highest_point <- function(mappinglist){
  final_val <- 0
  final_index <- 0
  for (i in 1:nrow(wordlist_v1_1)){
    cur_val <- get_word_points(i, mappinglist)  
    if(cur_val > final_val){
      final_val <- cur_val
      final_index <- i
    }
  }
  return(final_index)
}

#Returns the list of words using the auxillary methods defined
build_word_list <- function(mappinglist){
  wordlist <- data.frame(words = c())
  while(nrow(mappinglist) > 0){
    print(nrow(mappinglist))
    curr_word <- highest_point(mappinglist)
    wordlist <- rbind(wordlist, wordlist_v1_1[[curr_word,1]])
    mappinglist <- new_mapping_list(scored_words_PG[[curr_word]], mappinglist)
  }
  
  return(wordlist)
}

#Removes the mappings from the last chosen word and creates a new list of mappings without them
new_mapping_list <- function(word, mappinglist){
  for (i in 1:length(word)){
    if (nrow(filter(mappinglist, phoneme == inhouse_to_ipa(word[1,i]), grapheme == word[2, i]) != 0)){
      mappinglist <- mappinglist[!(mappinglist$phoneme == inhouse_to_ipa(word[1,i]) &
                 mappinglist$grapheme == word[2,i]), ]
    }
  }
  return(mappinglist)
}


