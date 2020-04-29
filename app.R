#setwd("E:/covid_19/covid_app")
#https://curso-r.github.io/my-first-dashboard-with-shiny-csds2019/#47
#  ===== Librerías ====
# https://datos.gob.mx/busca/dataset/informacion-referente-a-casos-covid-19-en-mexico
# https://stackoverflow.com/questions/39436713/r-shiny-reactivevalues-vs-reactive 
# http://www.sthda.com/english/wiki/ggplot2-axis-ticks-a-guide-to-customize-tick-marks-and-labels
#library(dplyr)
library(purrr)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(geojsonio)
library(reshape2)
library(leaflet)
library(rgdal)  ### for maps
library(reshape2) ## Tabla de Graficas
library(BAMMtools) ## Jenks
library(DT)
#  ===== Carga de datos ====
covid_19 <- read.csv( "200428COVID19MEXICO.csv", sep=",", dec=".", header = T,
                      colClasses = c(MUNICIPIO_RES = "character"))
qro_s <- readOGR(dsn= ".", layer = "qro_mun")  #### Utilizar path absoluto no relativo

#names(covid_19)
#str(covid_19)
#  ===== Funciones  ====
#
cod_res <- function(x){
  if  ( x == 1) {x = "Positivo"}
  else if ( x == 2 ) { x = "Negativo"}
  else {x = "Sospechoso"}
  return(x)
}

cod_si_no <- function(x){
  if  ( x == 1) {x = "SI"}
  else if ( x == 2 ) { x = "NO"}
  else if ( x == 97 ) { x = "NO APLICA"}
  else if ( x == 98 ) {x = "SE IGNORA"} 
  else {x = "NO ESPECIFICADO"}
  return(x)
}

cod_sex <- function(x){
  if  ( x == 1) {x = "MUJER"}
  else if ( x == 2 ) { x = "HOMBRE"}
  else if ( x == 99 ) {x = "NO ESPECIFICADO"}
  else {x = "MAL CODIFICADO"}
 
}
#cod_si_no( a)

cod_mpio <- function(x){
  if ( x == "22001" ) { x = "AMEALCO DE BONFIL"}
  else if ( x == "22002" ) { x = "PINAL DE AMOLES"}
  else if ( x == "22003" ) { x = "ARROYO SECO"}
  else if ( x == "22004" ) { x = "CADEREYTA DE MONTES"}
  else if ( x == "22005" ) { x = "COLÓN"}
  else if ( x == "22006" ) { x = "CORREGIDORA"}
  else if ( x == "22007" ) { x = "EZEQUIEL MONTES"}
  else if ( x == "22008" ) { x = "HUIMILPAN"}
  else if ( x == "22009" ) { x = "JALPAN DE SERRA"}
  else if ( x == "22010" ) { x = "LANDA DE MATAMOROS"}
  else if ( x == "22011" ) { x = "EL MARQUÉS"}
  else if ( x == "22012" ) { x = "PEDRO ESCOBEDO"}
  else if ( x == "22013" ) { x = "PEÑAMILLER"}
  else if ( x == "22014" ) { x = "QUERÉTARO"}
  else if ( x == "22015" ) { x = "SAN JOAQUÍN"}
  else if ( x == "22016" ) { x = "SAN JUAN DEL RÍO"}
  else if ( x == "22017" ) { x = "TEQUISQUIAPAN"}
  else if ( x == "22018" ) { x = "TOLIMÁN"}
  else if ( x == "999" ) { x = "NO ESPECIFICADO"}
  else {x = "FORÁNEO"}
  return(x)
}

## Edades
edades <- function (x) {
  a <- 0
  labs <- c(paste(seq(0, 90, by = 10), seq(0 + 10 - 1, 100 - 1, by = 10),
                  sep = "-"), paste(100, "+", sep = ""))  
  a <- cut(x , breaks = c(seq(0, 100, by = 10), Inf ), labels = labs, right = FALSE)
}

cod_t_pac <- function(x){
  if  ( x == 1) {x = "AMBULATORIO"}
  else if ( x == 2 ) { x = "HOSPITALIZADO"}
  else if ( x == 99 ) { x = "NO ESPECIFICADO"}
  else {x = "MAL CODIFICADO"}
  return(x)
}
#  ===== Preproceso ====
#  Filtro para Querétaro 
qro_s@data$Cve_Municipio <- as.integer( as.character (qro_s@data$CVEGEO) )
c_19_qro <- covid_19 %>% filter(ENTIDAD_RES == 22) 
c_19_qro <- c_19_qro %>% mutate (Cve_Municipio = as.integer(paste (ENTIDAD_RES, MUNICIPIO_RES, sep="") ) )

c_19_qro <- c_19_qro %>% 
  mutate (  OBESIDAD = replace (OBESIDAD, OBESIDAD !=1, 0 ),
            NEUMONIA = replace (NEUMONIA, NEUMONIA !=1, 0 ),
            DIABETES = replace (DIABETES, DIABETES !=1, 0 ),
            CARDIOV = replace (CARDIOVASCULAR, CARDIOVASCULAR !=1, 0),
            TABAQ = replace (TABAQUISMO, TABAQUISMO !=1, 0 ),
            HIPERT = replace (HIPERTENSION, HIPERTENSION !=1, 0)  )


#c_19_qro <- c_19_qro %>% mutate (Cve_Municipio = map_int( MUNICIPIO_RES, mpios ))
    # Numérico para suma
c_19_qro$total <- 1
    # Fechas
muerte <- as.Date.character (gsub("-","/" ,c_19_qro$FECHA_DEF), format='%Y/%m/%d')
ingreso <- as.Date.character (gsub("-","/" ,c_19_qro$FECHA_INGRESO), format='%Y/%m/%d')
sint <- as.Date.character (gsub("-","/" ,c_19_qro$FECHA_SINTOMAS), format='%Y/%m/%d')

# Lista de opciones ====
Com_list <- c("DIABETES", "EPOC","ASMA","INMUSUPR","HIPERTENSION","CARDIOVASCULAR","OBESIDAD","RENAL_CRONICA","TABAQUISMO")
  #Com_list <- list("DIABETES", "EPOC","ASMA","INMUSUPR","HIPERTENSION","OTRAS_COM","CARDIOVASCULAR","OBESIDAD","RENAL_CRONICA","TABAQUISMO")
Tipo_list <- c("Positivos", "Negativos", "Sospechosos")
# Queries  ====
        # Value boxes general ====
x_boxinfo <- c_19_qro %>%  group_by(RESULTADO) %>%  summarise( total = sum(total) )
x_boxinfo <- x_boxinfo %>% mutate(RESULTADO = map_chr(RESULTADO, cod_res))
falle <- c_19_qro %>% filter(FECHA_DEF != "9999-99-99" & RESULTADO == 1) %>% summarise( sum(total))
 
  # Positivos general y agrega rango edad y cod sexo ====
x_pos <- c_19_qro %>% filter (RESULTADO == 1) %>% 
                      mutate( r_edad = edades(EDAD),
                              Sexo = map_chr(SEXO, cod_sex),
                              Paciente = map_chr(TIPO_PACIENTE, cod_t_pac) )
        # Map Municipios ====
x_mun <- x_pos %>%  group_by(Cve_Municipio) %>%  summarise( total = sum(total)) 
x_mun <- x_mun %>% mutate (nom_mun = map_chr(Cve_Municipio,cod_mpio)) 
qro_s@data <- left_join(qro_s@data, x_mun, by = "Cve_Municipio")
qro_s@data <- qro_s@data %>% mutate( total = replace(total, is.na(total), 0),
                                     NOMGEO = map_chr(Cve_Municipio,cod_mpio))
  # Decesos Positivos ====

deceso <- x_pos %>% filter(FECHA_DEF != "9999-99-99")

  # Graph positivo gral Edad y sexo ====
ed_sx_G <- x_pos %>% group_by(r_edad, Paciente) %>% summarise(Casos=  sum(total))
#### Graph desglosada Edad y sexo
ed_sx <- deceso %>% group_by(r_edad, Sexo) %>% summarise(Casos=  sum(total))


#FILTRO %>% group_by(r_edad, Sexo) %>% summarise(Casos=  sum(total))
#names(FILTRO)


# UI  =====
ui <- dashboardPage(skin = "blue", #### Start of UI
                    
                    dashboardHeader(title = "Querétaro COVID-19"),
                    dashboardSidebar( 
                      title = "Análisis", ### DashboardSidebar inicio
                                      ## Sidebar content
                                      dashboardSidebar( 
                                        sidebarMenu(id = "menu",sidebarMenuOutput("menu")) 
                                            ) ### Fin sidebar menu
                                      ), ### Fin DashboardSidebar
                    dashboardBody( ### Dashboard Item 
             
                      tabItems( ### Inicia tabs Itmes Laterales 
                        
                            tabItem(tabName = "general", #### Tab General ====
                                    fluidRow(
                                      column( 12,
                                              box(
                                                title = "Informe general",
                                                collapsible = TRUE,
                                                width = "100%",
                                                height = "200%", 
                                                valueBoxOutput("conf", width = 3),
                                                valueBoxOutput("neg", width = 3),
                                                valueBoxOutput("sosp", width = 3),
                                                valueBoxOutput("def", width = 3))
                                      )
                                    ),
                                    br(),
                                    fluidRow(  #### Inicia Fuidrow 1
                                      column(6, 
                                             
                                             box(
                                               title = "Casos por municipio (de residencia)",
                                               collapsible = TRUE,
                                               width = "100%",
                                               height = "500%", 
                                               leafletOutput("mapa1",width="100%",height="400px")
                                             )
                                      ),
                                      column(6, 
                                             box(
                                               title = "Distribución por Rango de edad y Tipo de paciente",
                                               collapsible = TRUE,
                                               width = "100%",
                                               height = "500%", 
                                               plotOutput("plot_1",width="100%",height="400px") 
                                             )
                                      )
                                    ), 
                                    br(),
                                    box(
                                      title = "Información",
                                      collapsible = TRUE,
                                      collapsed = TRUE,
                                      width = "100%",
                                      height = "50%",
                                      h4("Elaborado con datos abiertos de: "),
                                      tags$a(href="https://datos.gob.mx/busca/dataset/informacion-referente-a-casos-covid-19-en-mexico", "Datos abiertos Covid-19 Mx",
                                             target="_blank"),
                                      h4("Mayor información sobre el tablero: "),
                                      tags$a(href="https://www.linkedin.com/in/isaías-morales-78843b10a", "Contacto")
                                      
                                      
                                      
                                    )### Termina Fluidrow 1      
                                  ),   #### Tab General 
                            tabItem(tabName = "detalle", #### Tab Detalle ====
                                    
                                    fluidRow(  #### Inicia Fuidrow Graph edaadeds
                                      column(6, 
                                             box( ## Inicia box 
                                               title = "Distribución por Sexo y Rango de edad",
                                               collapsible = F,
                                               width = "100%",
                                               height = "500%", 
                                               plotOutput("plot_3",width="100%",height="500px") 
                                             ) # Fin Box
                                      ),### Termina Column
                                      column(6,
                                             box( ## Inicia box 
                                               title = "Distribución de otras condiciones médicas",
                                               collapsible = F,
                                               width = "100%",
                                               height = "500%", 
                                               plotOutput("plot_2",width="100%",height="500px") 
                                             )## Acaba box 
                                      )  ### FIn de ccolumn
                                    )#### Fin Fluidrow Grph
                                )   #### Tab Detalle
                  ) ### Fin tabs Itmes Laterales ====
            ) #### Dashboard body
) #### End onf UI

# Server ====
server <- function(input, output) {
  # Menú====
  output$menu <- renderMenu({
    sidebarMenu( # De aquí
      menuItem("Análisis general", tabName = "general",
               icon = icon("chart-bar", lib = "font-awesome")),
      menuItem("Detalle", tabName = "Menu", 
               icon = icon("chart-bar", lib = "font-awesome"),
               menuSubItem( "Gráfica", tabName ="detalle"),
               selectInput("Tipo","Elija una opción", 
                           "choices" = c("Positivos"   = 1,
                                         "Negativos"   = 2),
                           "Positivos") )
    ) # Aqui
  })
  # Reactives =====
   
  data_comor <- reactive({ deceso %>% select(input$morb, total)  })
  data_tipo <- reactive ({c_19_qro %>% filter (RESULTADO == input$Tipo)}) 
       # Valueboxes ====
  output$conf <- renderValueBox({
    valueBox(x_boxinfo$total[1], "Confirmados", 
             icon = icon("band-aid",lib = "font-awesome"),
             color = "red" 
             )
    })
  output$sosp <- renderValueBox({
    valueBox(x_boxinfo$total[3], "Sospechosos", 
             icon = icon("hourglass-half",lib = "font-awesome" ),
             color = "yellow" )})
  output$neg  <- renderValueBox({
    valueBox(x_boxinfo$total[2], "Negativos", 
             icon = icon("thumbs-up",lib = "font-awesome" ),
             color = "green" )})
  output$def  <- renderValueBox({
    valueBox(falle, "Defunciones", 
             icon = icon("ribbon",lib = "font-awesome" ),
             color = "black" )})
       # Maps ====
  output$mapa1 <- renderLeaflet({ 
    #bins <- as.numeric( getJenksBreaks( qro_s@data$total, 3)  )  # Se definen los rangos del intervalo
    bins <- c(0,1,5,10, 50, 100 )
    paleta <- colorBin("Blues", domain = qro_s@data$total , bins = bins)  ## La paleta de colores para los bins
    
    lab_cov <- sprintf(
      "<strong> %s </strong> <br/> %s",
      qro_s@data$NOMGEO, qro_s@data$total ) %>% lapply(htmltools::HTML)
    
    
    
    
    leaflet(qro_s) %>% addProviderTiles("CartoDB.Positron", group= "Mapa base",
                                        options = providerTileOptions(minZoom = 8, maxZoom = 10)) %>%
      fitBounds(-100.59654, 20.01502, -99.04308, 21.67001) %>%  
      addPolygons(
        fillColor = ~paleta(total),
        dashArray = "3", weight = 1, smoothFactor = 0.3,
        opacity = 1.0, fillOpacity = 0.8,
        group = "Municipios",
        label = lab_cov,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"),
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE)) %>%
      addLegend(pal = paleta, values = ~ total, opacity = 0.7, title = "Casos positivos",
                position = "bottomright") %>%
      addLayersControl(
        overlayGroups = c("Mapa Base","Municipios"),
        options = layersControlOptions(collapsed = TRUE))
    
    
  })
  # Gráficas ====
  output$plot_1 <- renderPlot({ 
    
    p <-ggplot(data=ed_sx_G, aes(x=r_edad, y=Casos, fill= Paciente )) + ylim(0,35) +
      geom_bar(stat="identity") + 
      scale_fill_manual (values =c("#223f4d", "#d13d1f", "#999999")) +
      scale_x_discrete(drop=F, name="Rango de edad") +
      theme(axis.text.y = element_text(face="bold", size=10) ,
            axis.text.x = element_text(face="bold", size=10,angle=45) ,
            axis.title.x = element_text(color="black", size=14, face="bold"),
            axis.title.y = element_text(color="black", size=14, face="bold"),
            legend.position = "top",legend.direction = "horizontal") +
              labs (fill = "Tipo de paciente: ")
    p
      
  })
  
  output$plot_2 <- renderPlot({
    G2_D <- data_tipo()
    G2_D <- G2_D %>% summarise(Neumonia = sum(NEUMONIA) ,
                               Obesidad = sum(OBESIDAD), 
                               Diabetes = sum(DIABETES),
                               Cardiovascular = sum(CARDIOV),
                               Tabaquismo =sum(TABAQ), 
                               Hipertesión = sum(HIPERT))
    G2_D <- gather( G2_D)
    G2_D <- G2_D %>% mutate( Porcentaje = round(value/ nrow(data_tipo() ) * 100, 2) )
    
    ggplot(data=G2_D, aes(x=key, y=Porcentaje , fill  = Porcentaje)) + ylim(0,100) +
      geom_bar(stat="identity") + coord_polar() +
      theme(axis.text.y = element_text(face="bold", size=10) ,
            axis.text.x = element_text(face="bold", size=10) ,
            axis.title.x = element_text(color="black", size=14, face="bold"),
            axis.title.y = element_text(color="black", size=14, face="bold"),
            legend.position = "top",legend.direction = "horizontal")+
      labs(fill = "%", x= "") + scale_fill_continuous(trans = 'reverse')
    
    
    })
  
  output$plot_3 <- renderPlot({
    posit <- data_tipo()
      posit  <- posit %>% mutate( r_edad = edades(EDAD),Sexo = map_chr(SEXO, cod_sex))
      G_3 <- posit %>% group_by(r_edad, Sexo) %>% summarise(Casos=  sum(total))
      
      p <- ggplot(data=G_3, aes(x=r_edad, y=Casos, fill= Sexo )) + geom_bar(stat="identity") +
        scale_fill_manual (values =c("#499099", "#72557d", "#999999")) +
        scale_x_discrete(drop=F, name="Rango de edad") +
        theme(axis.text.y = element_text(face="bold", size=10) ,
              axis.text.x = element_text(face="bold", size=12,angle=45) ,
              axis.title.x = element_text(size=14, face="bold"),
              axis.title.y = element_text(size=14, face="bold"),
              legend.position = "top",legend.direction = "horizontal") +
        labs (fill = "Sexo: ")
      p
  })
  
}

# Run the application ====
shinyApp(ui = ui, server = server)
