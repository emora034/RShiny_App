library(Stat2Data)
library(shiny)
library(tidyverse)
library(magrittr)
library(gapminder)
library(shinyjs)
library(plotly)
library(htmltools)
library(htmlwidgets)
library(metricsgraphics)
library(RColorBrewer)
library(readr)
library(DT)
library(iotools)
library(basictabler)

#dataset Renewable Energy Generated in Maryland from data.gov
energy <- read_csv("https://opendata.maryland.gov/api/views/79zg-5xwz/rows.csv?accessType=DOWNLOAD", 
                   na = "0")
energy[is.na(energy)]<-0
PAGE_TITLE<-"Renewable Energy Generated In Maryland "
energy<-energy[-9,]
energy<-energy[-9,]
attach(energy)
energy$`Percent Generation`<-round(energy$`Percent Generation`,0)
energy[] <- lapply(energy, as.integer)
energy<- energy[order(energy$Year),]
options(DT.options = list(pageLength = 5))

#dataset Renewable Energy Generation Capacity: 2006 - 2017 
gen<-read_csv("https://opendata.maryland.gov/api/views/mq84-njxq/rows.csv?accessType=DOWNLOAD",
              na="0")
gen[is.na(gen)]<-0
gen<-gen[order(gen$Year),]


#set skeleton and tabs
headerImagePanel <- function(title, src) {
  div(
    style = "display: inline; position: relative",
    img(src = src, 
      style="width:4.5%; max-width:4.5%; position: left;"
    ),
    h1(title, style="display: inline;",
       style = "font-family: 'Lobster', cursive")
  )
}

datainfo<-tabPanel("About",
                   mainPanel(style = "font-size:20px", #font-family: 'Lobster', cursive",
                            # p{style = "font-family: Lucida Grande,Lucida Sans Unicode"}
                     p(),
                     p("This data set describes the amount of energy generated annually by renewable sources 
                       in Maryland in megawatt hours (MWh). In addition, there is a column which describes 
                       the percent of all energy generated in Maryland coming from renewable sources each year.", style = "font-family: Lucida Grande,Lucida Sans Unicode"),
                     p("Renewable energy generation data comes from PJM's Generation Attribute Tracking System 
                       (PJM GATS). Total generation comes from the U.S. Energy Information Administration's State 
                       Level Generation report, released in October 2016 with revisions in November 2016", style = "font-family: Lucida Grande,Lucida Sans Unicode", 
                       helpText(a("Click here to access the U.S. Energy Information Administration page", target="_blank", href="https://www.eia.gov/electricity/data.php")
    ))
                     )
                             )

dataPanel<-tabPanel("Data",
                   
                        #input selector
                        selectInput(inputId = "dataset",
                                    label="Select a dataset",
                                    choices= c(" ", "Renewable Energy Generation Capacity (2006-2017)", "Renewable Energy Generated (2007-2017)"))
                      ,
                    mainPanel(
                      br(),
                      DT::dataTableOutput("dataTable")
                      )
                    )




plotPanel<-tabPanel("Plots",
                    mainPanel(
                      p("", style = "font-family: Lucida Grande,Lucida Sans Unicode"),
                      p("", style = "font-family: Lucida Grande,Lucida Sans Unicode"),
                    metricsgraphicsOutput("plotdata")
                      )
)

###########################
####### USER SERVER #######
ui<-fluidPage(
  tags$head(
    tags$style(HTML("
                    @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                    "))
    ),
  tags$p(
tags$style(HTML("
                    @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                    "))),
  navbarPage(headerImagePanel(PAGE_TITLE, "https://pics.clipartpng.com/midle/Renewable_Energy_PNG_Clipart-2976.png"),
               datainfo,
               dataPanel,
               plotPanel
)

)

server<-function(input,output){
  #push data table
  output$image<-renderUI(
    tag$img(src="https://www.cancer.gov/images/cdr/live/CDR755927-750.jpg")
  )
  
  datasetInput<- reactive({ req(input$dataset)
    switch(input$dataset,
           "Renewable Energy Generation Capacity (2006-2017)"= gen,
           "Renewable Energy Generated (2007-2017)"= energy
    )
  })
  
  output$dataTable<-renderDataTable(datasetInput())
                                         
                                
  output$plotdata<-renderMetricsgraphics({
    Leukemia%>%
      mjs_plot(x=Time, y=Age, width=500, height=800)%>%
      mjs_point(color_accessor = Status, size_accessor = Age)%>%
      mjs_labs(x="Survival Time in Months", y="Patients Age")
  })
}

#Run app 
shinyApp(ui=ui, server=server)
