#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(stringi)

covData <- read.csv("./200620COVID19MEXICO.csv")
covData <- covData[covData$RESULTADO == 1, c("ENTIDAD_UM", "FECHA_INGRESO")]
covData$FECHA_INGRESO<- as.Date(covData$FECHA_INGRESO, format = "%Y-%m-%d")

entityCodes <- read.csv("Catalogos_0412.csv")
entityCodes <- entityCodes[, -3]
entityCodes$ENTIDAD_FEDERATIVA <- stri_trans_general(unique(entityCodes$ENTIDAD_FEDERATIVA),"Latin-ASCII")
entityCodes <- rename(entityCodes, ENTIDAD_UM = CLAVE_ENTIDAD)

covData <- merge(x = entityCodes, y = covData, by = intersect(names(entityCodes), names(covData)))
covData <- covData[, -1]
covData <- with(covData, covData[order(ENTIDAD_FEDERATIVA, FECHA_INGRESO), ])
covData <- group_by(.data = covData, ENTIDAD_FEDERATIVA) %>% mutate(CUENTA = row_number())


# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Evolution of COVID-19 cases in Mexico by Federal Entity"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            
            tags$head(tags$style(type="text/css", "
             #loadmessage {
               position: fixed;
               top: 0px;
               left: 0px;
               width: 100%;
               padding: 5px 0px 5px 0px;
               text-align: center;
               font-weight: bold;
               font-size: 100%;
               color: #000000;
               background-color: #CCFF66;
               z-index: 105;
             }
          ")),
            
            p(strong("Help page:",style="color:red"), a("Documentation",href="https://github.com/AlexDeLara/COVID19-Cases-in-Mexico/blob/master/README.md")),
                     
            sliderInput(inputId = "date", label = "Select a date", min = min(covData$FECHA_INGRESO), max = max(covData$FECHA_INGRESO), value = median(covData$FECHA_INGRESO)),
            checkboxGroupInput(inputId = "entities", label = "Select Federal Entities", choices = unique(covData$ENTIDAD_FEDERATIVA), selected = "AGUASCALIENTES"),
            
            conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                            tags$div("Loading...",id="loadmessage"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("entityPlot")
        )
    )
))
