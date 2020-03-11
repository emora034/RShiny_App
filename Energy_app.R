library(Stat2Data)
library(shiny)
library(shinythemes)
library(tidyverse)
library(magrittr)
library(shinyjs)
library(plotly)
library(htmltools)
library(DT)
library(iotools)
#library(qrmtools)
library(reshape2)

#dataset Renewable Energy Generated in Maryland from data.gov
energy <- read_csv("https://opendata.maryland.gov/api/views/79zg-5xwz/rows.csv?accessType=DOWNLOAD", 
                   na = "0")
energy[is.na(energy)]<-0
PAGE_TITLE<-"Renewable Energy In Maryland "
energy<-energy[-9,]
energy<-energy[-9,]
attach(energy)
energy$`Percent Generation`<-round(energy$`Percent Generation`,0)
energy[] <- lapply(energy, as.integer)
energy<- energy[order(energy$Year),]
energy$Year<-as.factor(energy$Year)
options(DT.options = list(pageLength = 5))

#dataset Renewable Energy Generation Capacity: 2006 - 2017 
gen<-read_csv("https://opendata.maryland.gov/api/views/mq84-njxq/rows.csv?accessType=DOWNLOAD",
              na="0")
gen[is.na(gen)]<-0
gen<-gen[order(gen$Year),]
gen$Year<-as.factor(gen$Year)

enyears<-levels(energy$Year)
genyears<-levels(gen$Year)
genvar<-gen[,2:13]
envar<-energy[,2:13]

dataplot<-melt(gen, id="Year")
energy2<-energy[,1:13]
dataplot2<-melt(energy2, id="Year")
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

datainfo<-tabPanel("  About",
                   mainPanel(style = "font-size:20px", #font-family: 'Lobster', cursive",
                             # p{style = "font-family: Lucida Grande,Lucida Sans Unicode"}
                             p(),
                             p(helpText("This app contains two datasets regarding the recorded generated renewable energy 
                                        and the estimated generating capacity for each renewable source type.", style = "font-family: Lucida Grande,Lucida Sans Unicode")),
                             p(helpText("The first dataset estimates the installed capacity for renewable energy generation in Maryland, in 
                                        megawatts (MW). Reported data comes from energy generators in Maryland registered to generate 
                                        renewable energy credits (RECs) through the PJM Environmental Information Services (EIS) 
                                        Generation Attributes Tracking System (GATS) (available ", a("here).", target="_blank", 
                                                                                                     href="https://gats.pjm-eis.com/gats2/PublicReports/RenewableGeneratorsRegisteredinGATS")), 
                               style = "font-family: Lucida Grande,Lucida Sans Unicode"
                             ),
                             helpText(
                               p("As renewable energy generators are not required to register in GATS, there may be some renewable energy 
                                 generation capacity installed in Maryland but not generating RECs that is not captured in this estimate.",
                                 style = "font-family: Lucida Grande,Lucida Sans Unicode"),
                               p("The second data set describes the amount of energy generated annually by renewable sources 
                                 in Maryland in megawatt hours (MWh). In addition, there is a column which describes 
                                 the percent of all energy generated in Maryland coming from renewable sources each year.", style = "font-family: Lucida Grande,Lucida Sans Unicode"),
                               p("Renewable energy generation data comes from PJM's Generation Attribute Tracking System 
                                 (PJM GATS). Total generation comes from the U.S. Energy Information Administration's State 
                                 Level Generation report, released in October 2016 with revisions in November 2016.", style = "font-family: Lucida Grande,Lucida Sans Unicode", 
                                 helpText(a("Click here to access the U.S. Energy Information Administration page", target="_blank", href="https://www.eia.gov/electricity/data.php")
                                 ))
                               )
                             ))

dataPanel<-tabPanel("Data Sets",
                    
                    #input selector
                    selectInput(
                      p(),
                      inputId = "dataset",
                      label="Select a dataset",
                      choices= c(" ", "Renewable Energy Generation Capacity (2006-2017) ", "Renewable Energy Generated (2007-2017) "))
                    ,
                    mainPanel(
                      
                      br(),
                      DT::dataTableOutput("dataTable"), align="center"
                    )
)




plotPanel1<-tabPanel("Generation Capacity",
                     
                     selectInput("year", multiple=TRUE,
                                 "Year:", genyears,
                                 selected=head(gen,3)),
                     p(),
                     mainPanel(
                       p(),
                       tableOutput("captable"),
                       p(),
                       p(),
                       plotlyOutput("plot")
                       #metricsgraphicsOutput("plotdata")
                     )
)

plotPanel2<-tabPanel("Energy Generated",
                     useShinyjs(),
                     div(
                       selectInput("year2", multiple=TRUE,
                                   "Year:", enyears,
                                   selected=head(energy,3))),
                     p(),
                     mainPanel(
                       p(),
                       tableOutput("captable2"),
                       p(),
                       p(),
                       p(),
                       plotlyOutput("plot2"),
                       downloadButton("report", "Generate report")
                       #metricsgraphicsOutput("plotdata")
                     )
)


###########################
####### USER SERVER #######
ui<-fluidPage(theme=shinytheme("readable"),
              useShinyjs(),
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
                         plotPanel1,
                         plotPanel2
              )
              
                )

server<-function(input,output){
  #push data table
  output$image<-renderUI(
    tag$img(src="https://www.cancer.gov/images/cdr/live/CDR755927-750.jpg")
  )
  
  datasetInput<- reactive({ req(input$dataset)
    switch(input$dataset,
           "Renewable Energy Generation Capacity (2006-2017) "= gen,
           "Renewable Energy Generated (2007-2017) "= energy
    )
  })
  
  output$dataTable<-renderDataTable(datasetInput())
  
  #capacity page
  genfilter<-reactive({
    req(input$year)
    #req(input$variable)
    gen%>%filter(Year%in%input$year) #colnames(gen)%in%input$variable)
  })
  
  output$captable<-renderTable(genfilter())
  
  
  genplotfilter<-reactive({
    req(input$year)
    #req(input$variable)
    dataplot%>%filter(Year%in%input$year) #colnames(gen)%in%input$variable)
    
  })
  
  output$plot<-plotly::renderPlotly({
    genplotfilter()%>%
      ggplot(aes(x=variable, y=value, color= Year, 
                 group=Year))+geom_bar(position="dodge",stat="identity")+ 
      theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      xlab("Renewable Energy Source")+ylab("Megawatts")
  })  
  
  #generated page
  #capacity page
  genfilter2<-reactive({
    req(input$year2)
    #req(input$variable)
    energy%>%filter(Year%in%input$year2) #colnames(gen)%in%input$variable)
  })
  
  output$captable2<-renderTable(genfilter2())
  
  
  genplotfilter2<-reactive({
    req(input$year2)
    #req(input$variable)
    dataplot2%>%filter(Year%in%input$year2) #colnames(gen)%in%input$variable)
    
  })
  
  output$plot2<-plotly::renderPlotly({
    genplotfilter2()%>%
      ggplot(aes(x=variable, y=value, color= Year, 
                 group=Year))+geom_bar(position="dodge",stat="identity")+ 
      theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      xlab("Renewable Energy Source")+ylab("Megawatts Hours")
  }) 
  output$report <- downloadHandler(
    filename = "report.pdf",
    content = function(file) {
      tempReport <- file.path(tempdir(), "report.Rmd")
      
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      params<-list(selyear=isolate(input$year2)) 
      data=genplotfilter()
      
      rmarkdown::render(tempReport, output_file = file,
                        
                        params = params,
                        
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  #final closing bracket
}


#Run app 
shinyApp(ui=ui, server=server)
