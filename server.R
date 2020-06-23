#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
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

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$entityPlot <- renderPlot({
        
        entities <- input$entities
        date <- as.Date(input$date)
        
        covEntity <- subset(covData, ENTIDAD_FEDERATIVA %in% entities)
        covEntity <- subset(covEntity, FECHA_INGRESO <= date)
        
        g <- ggplot(covEntity, aes(x = FECHA_INGRESO, y = CUENTA, color = as.factor(ENTIDAD_FEDERATIVA)))
        g <- g +  geom_line() + ylim(0, which.max(covEntity$CUENTA)) + labs(x = "DATE", y = "COUNT", color = "Federal Entity")
        g
        
    })

})
