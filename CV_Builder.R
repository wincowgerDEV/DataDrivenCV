library(vitae)
#install.packages("rlang")
library(googlesheets4)
library(dplyr)
library(glue)
library(knitr)
library(tinytex)


cv_builder <- function(x){
    googlesheets4::gs4_deauth()
    
    file <- read_sheet(x) %>%
        mutate(ShortDescription = ifelse(!is.na(Link), paste0(ShortDescription, " Link: (", Link, ")"), ShortDescription)) %>%
        mutate(Date = ifelse(!is.na(StartYear) & !is.na(EndYear), glue::glue('{StartMonth} {StartYear} --> {EndMonth} {EndYear}',.na = ''), ifelse(!is.na(StartYear), glue::glue('{StartMonth} {StartYear}',.na = ''), "")))
    
    metadata <- file %>%
        filter(Topic == "(CV Metadata)")
    
    data <- file %>%
        filter(Topic != "(CV Metadata)")
    
    download.file(metadata[metadata$Subtopic == "profilepic", ]$ShortDescription, "image.jpg", mode = "wb")
    
    yaml <- paste("---",
                  "docname: CV",
                  paste0("name: ", metadata[metadata$Subtopic == "name", ]$ShortDescription),
                  paste0("surname: ", metadata[metadata$Subtopic == "surname", ]$ShortDescription),
                  paste0('position: "', metadata[metadata$Subtopic == "position", ]$ShortDescription, '"'),
                  paste0('address: "', metadata[metadata$Subtopic == "address", ]$ShortDescription, '"'),
                  paste0('profilepic: "', "image.jpg", '"'),
                  paste0("phone: ", metadata[metadata$Subtopic == "name", ]$ShortDescription),
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
                  "data <- read_sheet('https://docs.google.com/spreadsheets/d/1E3_Z900RAWbRnThNNu-_DXQqZN6qHkD2wN7ZYGmKt34/edit?usp=sharing') %>%
    mutate(ShortDescription = ifelse(!is.na(Link), paste0(ShortDescription, ' Link: (', Link, ')'), ShortDescription)) %>%
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
    
    write(yaml_to_loop, file = "./AwesomeCV.Rmd", append = F)
    
    rmarkdown::render('AwesomeCV.Rmd')
}


