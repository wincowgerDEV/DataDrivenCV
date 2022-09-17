# An automated way to create a Data Driven CV in a web application. 

Currently online at www.wincowger.shinyapps.io/DataDrivenCV

Example Spreadsheet: https://docs.google.com/spreadsheets/d/1E3_Z900RAWbRnThNNu-_DXQqZN6qHkD2wN7ZYGmKt34/edit?usp=sharing
- These fields must be named verbatim: Topic	Subtopic	StartMonth	StartYear	EndMonth	EndYear	SubSubtopic	Location	ShortDescription	LongDescription
- These values control the metadata for the report and must have a ShortDescription. 
Topic         Subtopic
(CV Metadata)	name
(CV Metadata)	surname
(CV Metadata)	position
(CV Metadata)	address
(CV Metadata)	profilepic
(CV Metadata)	phone
(CV Metadata)	www
(CV Metadata)	email
(CV Metadata)	github
(CV Metadata)	twitter
(CV Metadata)	orcid
(CV Metadata)	linkedin
(CV Metadata)	headcolor
(CV Metadata)	aboutme
- All other combinations of Topic and Subtopic are accepted and the order of things is important because that will control the order of things that are listed in your CV. 

app.R holds the web application code. 

CV_builder.R holds the functions for automating cv generation from a Google Sheet. 

AwesomeCV/ holds the formatting files for the cv  generation. 

Massive credit to the Vitae R package who did most of the heavy lifting here. 
