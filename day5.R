if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(dplyr, tidyr, readr, stringr, ggplot2, knitr, scales, readxl, httr, jsonlite, magrittr, tidyjson, lubridate, janitor, here, formidabler)

# load input
chain_raw <- read.delim(here::here("input_day5.txt"),header = FALSE)

#chain <- str_sub(chain,1,100)
# Define function that reacts a given chain and returns the length of the final product
length_reaction<-function(chain) {
  pos<-1
  #Go through the chain using a loop. 
  #Notice that a while is better suited because the chain is constantly chhnging length and sometimes you have to take a step back
  while(pos<str_length(chain)) {
    #  print(paste(str_sub(chain,1,pos-1),str_sub(chain,pos,-1)))
    first <- str_sub(chain,pos,pos)
    second <- str_sub(chain,pos+1,pos+1)
    # Check if the mini-sequence reacts
    if (first!=second & (toupper(first)==toupper(second))) {
      if(pos==1) {
        #Update chain after reacting polymers have been deleted 
        chain <- str_sub(chain,pos+2,-1)
      }
      else{
        #Update chain after reacting polymers have been deleted 
        chain <- paste0(str_sub(chain,1,pos-1),str_sub(chain,pos+2,-1))
        #Take a step back to check if the previous polymer reacts with the polymer to the immediate right of the deleted section
        pos<-pos-1
      }
      #    print("Boom")
    }
    else{
      # No reaction, just do to next polymer 
      pos<-pos+1
    }
  }
  return(str_length(chain))
}

##### Part 1
chainpart1 <- as.character(chain_raw[1,1])
length_reaction(chainpart1)

#### Part 2: Iterate function for part 1 across all letters, storing in a data frame the resulting lengths
#Initialize data frame to store lengths
length_final <- data.frame(letter=character(), length=integer())
#Iterate over letters
for(polymer in letters) {
  #Obtain chain from eliminating the letter for this iteration
  sub_chain <- gsub(paste(polymer,toupper(polymer),sep="|"),'',chainpart1)
  # Find after-reaction legth
  length_chain <- length_reaction(sub_chain)
  #Update lengths data frame
  length_final <- bind_rows(length_final,data.frame(letter=polymer,length=length_chain))
} 
max(length_final$length)
