if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(dplyr, tidyr, readr, stringr, ggplot2, knitr, scales, readxl, httr, jsonlite, magrittr, tidyjson, lubridate, janitor, here, formidabler)

#### prepare input
steps <- read.delim(here::here("input_day7.txt"),header = FALSE)
remaining_constraints <- steps %>%
  mutate(pre = sub("Step *(.*?) *m.*", "\\1", V1),
         post = sub(".*step *(.*?) *c.*", "\\1", V1)) %>%
  select(-V1)

########Part 1
#### Initialize status arrays 
#Steps executed so far
executed <- character(0)

while(length(setdiff(LETTERS,executed))!=0) {
  #Find steps without for which pre-requisites have been executed
  with_prereq<-unique(remaining_constraints$post)
  prereq_done <- setdiff(setdiff(LETTERS,executed),with_prereq)
  # Choose next step
  new_step <- sort(prereq_done)[1]
  print(new_step)
  # Include new step in the list of executed steps
  executed<-c(executed,new_step)
  #Delete constraints that had the executed step as a prerequisites
  remaining_constraints %<>%
    filter(pre!=new_step)
}

executed

########## Part 2
#### Initialize status arrays 
remaining_constraints <- steps %>%
  mutate(pre = sub("Step *(.*?) *m.*", "\\1", V1),
         post = sub(".*step *(.*?) *c.*", "\\1", V1)) %>%
  select(-V1)
#Steps executed so far
executing <- data.frame(task=character(),
                        start_time=integer(),
                        end_time=integer(),
                        executor=integer())
executed <- character(0)
current_time <- 0
busy_until <- c(0,0,0,0,0)

while(length(setdiff(LETTERS,executed))!=0) {
#  for (num in 1:10) {
  #Find steps without for which pre-requisites have been executed
  with_prereq<-unique(remaining_constraints$post)
  done_or_in_progress <- c(executed,executing$task)
  prereq_done <- setdiff(setdiff(LETTERS,done_or_in_progress),with_prereq)
  
  # Choose next step
  new_step <- sort(prereq_done)[1]
  print(paste("Task available:",new_step))
  
  # If there are steps that can be executed, Assign new step to a worker
  if(!is.na(new_step)) {
    print("Task available")
    print(paste("Current time is:",current_time))
    idle <- busy_until<=current_time
    print("workers idle:")
    print(idle)
    worker<-match(TRUE,idle)
    print(paste("Selected worker:",worker))
    completion <- current_time+60+match(new_step,LETTERS)
    print(paste("completion time:",completion))
    executing <- bind_rows(executing,data.frame(task=new_step,start_time=current_time,end_time=completion,executor=worker))
    busy_until[worker] <- completion
  }
  else{
    print("No task available")
    #Skip to the next completion time
    idle <- busy_until<=current_time
    current_time<-min(busy_until[!idle])
    print(paste("New time is:",current_time))
    #Update lists of executing and executed tasks
    worker_finished <- match(current_time,busy_until)
    task_finished <- executing$task[executing$executor==worker_finished]
    executing <- filter(executing, task!=task_finished)
    print("Tasks in process")
    print(executing$task)
    # Include new step in the list of executed steps
    executed<-c(executed,task_finished)
    print(paste("Tasks finished",executed))
    busy_until[worker_finished] <- 0
    
    #Delete constraints that had the executed step as a prerequisites
    remaining_constraints %<>%
      filter(pre!=task_finished)
  }
  
  # Skip to the next completion time if all workers are now busy
  idle <- busy_until<=current_time
  if(is.na(match(TRUE,idle))){
    print("All busy")
    current_time<-min(busy_until)
    print(paste("New time is:",current_time))
    #Update lists of executing and executed tasks
    worker_finished <- match(current_time,busy_until)
    task_finished <- executing$task[executing$executor==worker_finished]
    executing <- filter(executing, task!=task_finished)
    print("Tasks in process")
    print(executing$task)
    # Include new step in the list of executed steps
    executed<-c(executed,task_finished)
    print("Tasks finished")
    print(executed)
    busy_until[worker_finished] <- 0
    
    #Delete constraints that had the executed step as a prerequisites
    remaining_constraints %<>%
      filter(pre!=task_finished)
  }
}  
executed

  
