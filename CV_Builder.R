library(vitae)
#install.packages("rlang")
library(googlesheets4)
library(dplyr)
library(glue)
library(knitr)
library(rmarkdown)
library(tinytex)



complete_or_incomplete <- function(x){
    !sum(is.na(x)) %in% c(length(x), 0) 
}


test_consistency <- function(x){ 
    x %>%
    group_by(Topic, Subtopic) %>%
    mutate(complete_StartMonth = complete_or_incomplete(StartMonth),
           complete_StartYear = complete_or_incomplete(StartYear),
           complete_EndMonth = complete_or_incomplete(EndMonth),
           complete_EndYear = complete_or_incomplete(EndYear),
           complete_SubSubtopic = complete_or_incomplete(SubSubtopic),
           complete_Location = complete_or_incomplete(Location),
           complete_ShortDescription = complete_or_incomplete(ShortDescription),
           complete_LongDescription = complete_or_incomplete(LongDescription),
           complete_Link = complete_or_incomplete(Link)
    ) %>%
    ungroup()
}

filter_inconsistent <- function(x) {
    x %>%
        filter(if_any(starts_with("complete"), ~ .)) 
}

remove_inconsistent <- function(x){
    x %>%
    mutate(
        StartMonth = ifelse(complete_StartMonth, NA, StartMonth),
        StartYear = ifelse(complete_StartYear, NA, StartYear),
        EndMonth = ifelse(complete_EndMonth, NA, EndMonth),
        EndYear = ifelse(complete_EndYear, NA, EndYear),
        SubSubtopic = ifelse(complete_SubSubtopic, NA, SubSubtopic),
        Location = ifelse(complete_Location, NA, Location),
        ShortDescription = ifelse(complete_ShortDescription, NA, ShortDescription),
        LongDescription = ifelse(complete_LongDescription, NA, LongDescription),
        Link = ifelse(complete_Link, NA, Link)
    ) 
}
   

#look <- testsheet("www")
testsheet <- function(url) {
    tryCatch(
        {
            # Just to highlight: if you want to use more than one 
            # R expression in the "try" part then you'll have to 
            # use curly brackets.
            # 'tryCatch()' will return the last evaluated expression 
            # in case the "try" part was completed successfully
            
            read_sheet(url)
            return(F)
            
            # The return value of `readLines()` is the actual value 
            # that will be returned in case there is no condition 
            # (e.g. warning or error). 
            # You don't need to state the return value via `return()` as code 
            # in the "try" part is not wrapped inside a function (unlike that
            # for the condition handlers for warnings and error below)
        },
        error=function(cond) {
            # Choose a return value in case of error
            return(T)
        },
        warning=function(cond) {
            # Choose a return value in case of warning
            return(T)
        },
        finally={
            # NOTE:
            # Here goes everything that should be executed at the end,
            # regardless of success or error.
            # If you want more than one expression to be executed, then you 
            # need to wrap them in curly brackets ({...}); otherwise you could
            # just have written 'finally=<expression>' 
        }
    )    
}


readUrl <- function(url) {
    out <- tryCatch(
        {
            # Just to highlight: if you want to use more than one 
            # R expression in the "try" part then you'll have to 
            # use curly brackets.
            # 'tryCatch()' will return the last evaluated expression 
            # in case the "try" part was completed successfully
            
            download.file(url, "AwesomeCV/image.jpg", mode = "wb")
            return(F)
            
            # The return value of `readLines()` is the actual value 
            # that will be returned in case there is no condition 
            # (e.g. warning or error). 
            # You don't need to state the return value via `return()` as code 
            # in the "try" part is not wrapped inside a function (unlike that
            # for the condition handlers for warnings and error below)
        },
        error=function(cond) {
            return(T)
        },
        warning=function(cond) {
            return(T)
        },
        finally={
        }
    )    
}


#cv_builder("https://docs.google.com/spreadsheets/d/1E3_Z900RAWbRnThNNu-_DXQqZN6qHkD2wN7ZYGmKt34/edit?usp=sharing")

cv_builder <- function(x){
    googlesheets4::gs4_deauth()
    
    file <- read_sheet(x) %>%
        mutate(ShortDescription = ifelse(!is.na(Link), paste0(ShortDescription, " Link: (", Link, ")"), ShortDescription)) %>%
        mutate(Date = ifelse(!is.na(StartYear) & !is.na(EndYear), glue::glue('{StartMonth} {StartYear} --> {EndMonth} {EndYear}',.na = ''), ifelse(!is.na(StartYear), glue::glue('{StartMonth} {StartYear}',.na = ''), "")))
    
    metadata <- file %>%
        filter(Topic == "(CV Metadata)")
    
    data <- file %>%
        filter(Topic != "(CV Metadata)")
    
    test <-  readUrl(metadata[metadata$Subtopic == "profilepic", ]$ShortDescription)
    
    yaml <- paste("---",
                  "docname: CV",
                  paste0("name: ", metadata[metadata$Subtopic == "name", ]$ShortDescription),
                  paste0("surname: ", metadata[metadata$Subtopic == "surname", ]$ShortDescription),
                  paste0('position: "', metadata[metadata$Subtopic == "position", ]$ShortDescription, '"'),
                  paste0('address: "', metadata[metadata$Subtopic == "address", ]$ShortDescription, '"'),
                  ifelse(test == 0,paste0('profilepic: "',  "image.jpg", "", '"'), ""),
                  paste0("phone: ", metadata[metadata$Subtopic == "phone", ]$ShortDescription),
                  paste0("www: ", metadata[metadata$Subtopic == "www", ]$ShortDescription),
                  paste0('email: "', metadata[metadata$Subtopic == "email", ]$ShortDescription, '"'),
                  paste0("twitter: ", metadata[metadata$Subtopic == "twitter", ]$ShortDescription),
                  paste0("github: ", metadata[metadata$Subtopic == "github", ]$ShortDescription),
                  paste0("orcid: ", metadata[metadata$Subtopic == "orcid", ]$ShortDescription),
                  paste0("linkedin: ", metadata[metadata$Subtopic == "linkedin", ]$ShortDescription),
                  paste0("date: ", "\"`r format(Sys.time(), '%B %Y')`\""),
                  paste0('headcolor: "', metadata[metadata$Subtopic == "headcolor", ]$ShortDescription, '"'),
                  paste0('aboutme: "', metadata[metadata$Subtopic == "aboutme", ]$ShortDescription, '"'),
                  "output: vitae::awesomecv" ,
                  #"  vitae::markdowncv:",
                  #"    theme: davewhipp",
                  "---",
                  
                  "```{r setup, include=FALSE}",
                  "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)",
                  "library(vitae)",
                  "library(googlesheets4)",
                  "library(dplyr)",
                  "library(glue)",
                  "googlesheets4::gs4_deauth()",
                  "data <- read_sheet('",x, "') %>%
    mutate(ShortDescription = ifelse(!is.na(Link), paste0(ShortDescription, ' (Link: ', Link, ')'), ShortDescription)) %>%
    mutate(Date = ifelse(!is.na(StartYear) & !is.na(EndYear), glue::glue('{StartMonth} {StartYear} --> {EndMonth} {EndYear}',.na = ''), ifelse(!is.na(StartYear), glue::glue('{StartMonth} {StartYear}',.na = ''), '')))" ,
                  "```",
                  sep = "\n")
    
    
    collection <- distinct(data, Topic, Subtopic)
    
    for(row in 1:nrow(collection)){
        if(row == 1){
            yaml_to_loop <- yaml
            previous_Topic <- NA
        }
        Topic2 <- collection[[row, "Topic"]]
        Subtopic2 <- collection[[row, "Subtopic"]]
        Startyear2 <- collection[[row, "StartYear"]]
        Endyear2 <- collection[[row, "EndYear"]]
        filter2 <-  if(!is.na(Subtopic2)){paste0("filter(Topic == \"", Topic2, "\"& Subtopic == \"", Subtopic2,"\") %>%")} else {paste0("filter(Topic == \"", Topic2, "\") %>%")}
        header2 <- if(ifelse(is.na(previous_Topic), T, Topic2 != previous_Topic)) {paste(paste0("# ", Topic2), ifelse(!is.na(Subtopic2), paste0("## ", Subtopic2), ""), sep = "\n")} else {paste0("## ", Subtopic2)}
        entry_data <- paste("data %>%",
                            filter2,
                            #paste0("filter(if(!is.na(", Subtopic2, "))Topic == \"", Topic2, "\"& Subtopic == \"", Subtopic2,"\" else Topic == \"", Topic2, "\") %>%"),
                            "detailed_entries(what = `ShortDescription`, when = Date, with = SubSubtopic, where = Location, why = LongDescription)",
                            sep = "\n"
        )
        
        yaml_to_loop <- paste(yaml_to_loop, 
                              header2,
                              "```{r}",
                              entry_data,
                              "```",
                              "",
                              sep = "\n"
        )
        
        previous_Topic = Topic2 
    }
    
    write(yaml_to_loop, file = "AwesomeCV/AwesomeCV.Rmd", append = F)
    
    rmarkdown::render('AwesomeCV/AwesomeCV.Rmd')
}


