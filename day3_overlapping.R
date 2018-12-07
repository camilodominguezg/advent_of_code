if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(dplyr, tidyr, readr, stringr, ggplot2, knitr, scales, readxl, httr, jsonlite, magrittr, tidyjson, lubridate, janitor, here, formidabler)

# load input
claims_raw <- read.delim(here::here("input.txt"),header = FALSE) %>%
  mutate(ID=as.numeric(sub("# *(.*?) *@.*", "\\1", V1)),
         inch_above = as.numeric(sub(".*@ *(.*?) *,.*", "\\1", V1)),
         inch_left = as.numeric(sub(".*, *(.*?) *: .*", "\\1", V1)),
         inch_high = as.numeric(sub(".*: *(.*?) *x.*", "\\1", V1)),
         inch_wide  = as.numeric(sub(".*x *(.*?)", "\\1", V1)))
claims <- claims_raw %>%
  mutate(sr=inch_above+1, er=inch_above+inch_high, sc=inch_left+1, ec=inch_left+inch_wide) %>%
  select(sr,er,sc,ec)

#function to divide rectangle 1 into non-overlapping rectangles according to overlap with rectangle 2
overlap <- function(r1, r2) {
  if (r1$sr>r2$er | r1$er<r2$sr | r1$sc>r2$ec | r1$ec<r2$sc) {
    rectangles <- bind_rows(c(sr= r1$sr, sc=r1$sc, er=r1$er, ec=r1$ec, layers=0))
  }
  else {
    # Adds 1)overlapping rectangle, 2) rectanlge over overlapping area, 3) rectangle under overlapping area, 
    # 4) rectangle left of overlap (excluding (2) and (3)), 5) rectangle right of overlap (excluding (2) and (3))   
    rectangles <- bind_rows(c(sr= max(r1$sr,r2$sr), sc=max(r1$sc,r2$sc), er=min(r1$er,r2$er), ec=min(r1$ec,r2$ec), layers=1),
                            c(sr= r1$sr, sc=r1$sc, er=r2$sr-1, ec=r1$ec, layers=0),
                            c(sr= r2$er+1, sc=r1$sc, er=r1$er, ec=r1$ec, layers=0),
                            c(sr= max(r2$sr,r1$sr), sc=r1$sc, er=min(r2$er,r1$er), ec=r2$sc-1, layers=0),
                            c(sr= max(r2$sr,r1$sr), sc=r2$ec+1, er=min(r2$er,r1$er), ec=r1$ec, layers=0))
    # Eliminate rectangles with ilogical coordinates (which shouldn't have been added in the first place)
    rectangles %<>%
      filter(sr<=er & sc<=ec)
  }
  rectangles
}

# Create df with rectangles covered
cover <- data.frame(sr=integer(),
                    er=integer(),
                    sc=integer(),
                    ec=integer(),
                    layers=integer())

cover[1,] <- bind_cols(claims[1,],layers=1)

outstanding_claims <- claims[2:nrow(claims),]
while(nrow(outstanding_claims)>0) {
  print(paste("Remaining claims: ",nrow(outstanding_claims)))
  current_claim <- outstanding_claims[1,]
  relevant_cover <- filter(cover, sr<=current_claim$er & sc<=current_claim$ec & er>=current_claim$sr & ec>=current_claim$sc)
  # If the new claim overlaps already layed out claims
  if(nrow(relevant_cover)>0) {
    current_cover <- relevant_cover[1,]
    # Delete overlapping cover (to replace with new rectangles)
    cover %<>%
      filter(!(sr==current_cover$sr & sc==current_cover$sc))
    #Find new division
    new_division <- overlap(current_cover,current_claim) %>%
      mutate(layers=layers+current_cover$layers)
    # Add non-overlapping rectangles in which the old rectangle is divided
    cover %<>%
      bind_rows(new_division)
    #Add pieces left of the claim after eliminating the processed part to the claim list
    retazos <- overlap(current_claim,current_cover) %>%
      filter(layers==0) %>%
      select(-layers)
    outstanding_claims <- bind_rows(retazos,filter(outstanding_claims,row_number()>1))
  }
  # If current claim does not overlap with any laid out claims
  else {
    print("No overlap")
    # Add new claim
    new_cover <- bind_cols(current_claim,layers=1)
    cover<-bind_rows(cover, new_cover)
    #Eliminate claim from outstanding list
    outstanding_claims <- filter(outstanding_claims,row_number()>1)
  }
}

# Compute final rectangles area
cover %<>%
  mutate(area=(er-sr+1)*(ec-sc+1))
cover %>%
  filter(layers>=2) %>%
  summarise(sum(area))

#Part 2
# Select rectangles with only one claim
onelayer <- filter(cover,layers==1)
#Look for the only claim that was not split through the whole process, i.e. the only claim that is untouched in the final list of non-overlapping rectangles
inner_join(claims,onelayer)

filter(claims_raw,inch_above==668, inch_left==880)
