library(vitae)
#install.packages("rlang")
library(googlesheets4)
library(dplyr)
library(glue)
library(knitr)
library(tinytex)

googlesheets4::gs4_deauth()
data <- read_sheet("https://docs.google.com/spreadsheets/d/1E3_Z900RAWbRnThNNu-_DXQqZN6qHkD2wN7ZYGmKt34/edit?usp=sharing") %>%
    mutate(ShortDescription = ifelse(!is.na(Link), paste0(ShortDescription, " Link: (", Link, ")"), ShortDescription)) %>%
    mutate(Date = ifelse(!is.na(StartYear) & !is.na(EndYear), glue::glue('{StartMonth} {StartYear} --> {EndMonth} {EndYear}',.na = ''), ifelse(!is.na(StartYear), glue::glue('{StartMonth} {StartYear}',.na = ''), "")))

is.na(data$EndYear)

yaml <- paste("---",
            "docname: CV",
            "name: Win",
            "surname: Cowger",
            'position: "Environmental Scientist"',
            'address: "Moore Institute for Plastic Pollution Research"',
            'profilepic: "./Professional_photo.jpg"',
            "phone: +1 515 298 3869",
            "www: wincowger.com",
            'email: "wincowger@gmail.com"',
            "twitter: Win_OpenData",
            "github: wincowgerDEV",
            "orcid: 0000-0001-9226-3104",
            "linkedin: win-cowger-6273b0136",
            paste0("date: ", "\"`r format(Sys.time(), '%B %Y')`\""),
            'headcolor: "689d6a"',
            'aboutme: "Win is working to use science, data, and software to tackle the global challenges of plastic pollution and waste."',
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


wd <- "G:/My Drive/MooreInstitute/Admin/Code/AwesomeCV"

setwd(wd)

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

write(yaml_to_loop, file = paste0(wd, "/AwesomeCV.Rmd"), append = F)

rmarkdown::render('AwesomeCV.Rmd')
awesomecv(paste0(wd, "/AwesomeCV.Rmd"))
