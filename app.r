# libraries
library(shiny)
library(shinydashboard)
library(leaflet)
library(htmlwidgets)
library(bslib)
library(tidycensus)
library(sf)
library(plotly)
library(forcats)
library(shinyWidgets)
library(listviewer)
library(jsonlite)
library(dlnm)
library(DT)
library(dplyr)
library(shinycssloaders)


#setwd('L:\\Lab\\CPHEA_TempUHI\\SCleland\\Dashboard')
#setwd('C:\\Users\\SCLELA01\\OneDrive - Environmental Protection Agency (EPA)\\Profile\\Documents\\Dashboard_Temp')

# Define color palettes
palettes <- data.frame("Variable" = c("uhi","uhiq","hosp","temp","range","temp.99"), 
                       "Color" = c("RdYlBu","RdYlBu","PuRd","RdYlBu","Purple-Green","Heat"))
palettes_cbsa <- data.frame("CBSAVariable" = c("rr.99","an.heat","mht","ar.heat"), 
                            "Color" = c("Geyser","Fall","Temps","TealRose"))
sub_palettes <- list("PrimaryAll" = "black",
                     "UHIIAll" = c("#66C0E4","#E34D34"),
                     "PrimaryCKD" = c("#CC99BB","#771155"),
                     "PrimaryDiabetes" = c("#77AADD","#114477"),
                     "UHIICKD" = c("#66C0E4","#E34D34","#66C0E4","#E34D34"),
                     "UHIIDiabetes" = c("#66C0E4","#E34D34","#66C0E4","#E34D34"),
                     "PrimaryRace" = c("#117744","#88CCAA"),
                     "PrimarySex" = c( "#777711","#DDDD77"),
                     "UHIIRace" = c("#66C0E4","#E34D34","#66C0E4","#E34D34"),
                     "UHIISex" = c("#66C0E4","#E34D34","#66C0E4","#E34D34"),
                     "PrimaryAge" = c("#771122", "#AA4455", "#DD7788"),
                     "UHIIAge" = c("#66C0E4","#E34D34","#66C0E4","#E34D34","#66C0E4","#E34D34")
                     
)
sub_linetypes <- list("PrimaryAll" = 1,
                     "UHIIAll" = c(2,6),
                     "PrimaryCKD" = c(1,1),
                     "PrimaryDiabetes" = c(1,1),
                     "UHIICKD" = c(2,6,2,6),
                     "UHIIDiabetes" = c(2,6,2,6),
                     "PrimaryRace" = c(1,1),
                     "PrimarySex" = c(1,1),
                     "UHIIRace" = c(2,6,2,6),
                     "UHIISex" = c(2,6,2,6),
                     "PrimaryAge" = c(1,1,1),
                     "UHIIAge" = c(2,6,2,6,2,6)
                     
)

# Read in datasets
zips_shape <- readRDS('./data/simp_05_zip_shapes_with_data.RDS')
cbsa_all <- readRDS('./data/cbsa_rr_an_results.RDS')
cbsa_all$Koppen.Simple <- gsub(",.*$","",cbsa_all$Koppen.Description)
cbsas_shape_full <- readRDS('./data/cbsa_shapes_with_data.RDS') 
load('./data/cbsa_pred_tmean.Rdata')
pooled <- readRDS('./data/full_list_pooled_ests.RDS')
pooled.99 <- readRDS('./data/full_list_pooled_ests_99.RDS')
sub_rr_af_an <- readRDS('./data/subgroup_all_results.RDS')

# Define HTML style for dashboard
box.style <- ".box.box-solid.box-primary>.box-header {
                              color:#fff;
                              background:#F8F8F8}

                            .box.box-solid.box-primary{
                            border-bottom-color:#F8F8F8;
                            border-left-color:#F8F8F8;
                            border-right-color:#F8F8F8;
                            border-top-color:#F8F8F8;}"
tabbox.style <- ".nav-tabs {background: #f4f4f4;}
                .nav-tabs-custom .nav-tabs li.active:hover a, .nav-tabs-custom .nav-tabs li.active a {background-color: #fff;
                                               border-color: #fff;}
                .nav-tabs-custom .nav-tabs li.active {border-top-color: 
                                                      #314a6d;}"

# User interface (UI) function
ui <- navbarPage("Dashboard",id="tabs",selected="Overall & Subpopulation Results",
                 header = tagList(
                   useShinydashboard(),
                   shinyjs::useShinyjs()
                 ),
                 theme = bslib::bs_theme(bootswatch = "minty"),
                 tabPanel(title="About",
                          fluidRow(
                            box(width=11,solidHeader = TRUE,title="About this Dashboard",status="primary",
                                htmlOutput("about_text")
                            )
                          )
                 ),
                 # Tab for the Overall & Subpopulation Results
                 tabPanel(title="Overall & Subpopulation Results",
                          fluidRow(
                            column(width=2,
                                   box(width=15, title="Options", solidHeader = TRUE,status="primary",
                                       htmlOutput("overall_info_text"),
                                       radioButtons("subpop", tags$span(style = "font-weight: bold;", "Subpopulation:"),
                                                    c("All" = "All",
                                                      "Age" = "Age",
                                                      "Sex" = "Sex",
                                                      "Race" = "Race",
                                                      "Diabetes" = "Diabetes",
                                                      "Chronic Kidney Disease (CKD)" = "CKD"
                                                    )
                                       ),
                                       radioButtons("sub", tags$span(style = "font-weight: bold;", "Stratification:"),
                                                    c("Overall" = "Primary",
                                                      "Urban Heat Island Intensity (UHII)" = "UHII"
                                                    )
                                       ),
                                       radioButtons("type", tags$span(style = "font-weight: bold;", "Plot Type:"),
                                                    c("Cumulative (Lags 0-21)" = "pooled",
                                                      "Lags at Extreme Heat" = "lags")
                                       )
                                   )
                            ),
                            
                            box(width=5, title="Exposure-Lag-Response Curves",solidHeader = TRUE,status="primary",
                                plotOutput("pooled_curve")  %>% withSpinner(color="#a9a9a9"),
                                div(htmlOutput("pooled_curve_text"),style="font-size:90%")
                            ),
                            box(width=5,title="Primary Results",solidHeader=TRUE,status="primary",align='right',
                                radioButtons("bar.var",NULL,c("AN","AR"),inline=T),
                                plotlyOutput("pooled_plots")  %>% withSpinner(color="#a9a9a9"),
                                div(htmlOutput("pooled_plot_text"),style="font-size:90%",align='left')
                            )
                          ),
                          fluidRow(
                            box(width=2, title="Key Takeaways", solidHeader = TRUE,status="primary",
                                tags$style(HTML("ul { list-style-position: outside; padding-left: 1em;} ")),
                                htmlOutput("main_takeaway") %>% withSpinner(color="#a9a9a9")
                            ),
                            box(width=10,title="Table of Results",solidHeader=TRUE,status="primary",
                                div(DT::dataTableOutput("pooled_table")  %>% withSpinner(color="#a9a9a9"),style = "font-size:80%"),
                                tags$style(type="text/css", "#downloadAllData {background-color:#F8F8F8;border-color:#dbd9d9;color: black}"),
                                downloadButton("downloadAllData", "Download all results")
                            )
                          )
                          
                 ),
                 # Tab for the MSA-Specific Results
                 tabPanel(title="MSA-Specific Results",
                          fluidRow(
                            column(width=2,
                                   box(width=15, title="Options", solidHeader = TRUE,status="primary",
                                       htmlOutput("cbsa_info_text"),
                                       radioButtons("forest_variable", tags$span(style = "font-weight: bold;", "Variable:"),
                                                    c("RR (99th vs. MHP)" = "rr.99",
                                                      "Heat AN (Temp. >= MHP)" = "an.heat",
                                                      "Heat AR (per 100k)" = "ar.heat",
                                                      "MHT (\u00b0C)" = "mht"
                                                    )),
                                       radioButtons("forest_type", tags$span(style = "font-weight: bold;", "Stratification:"),
                                                    c("Overall" = "Primary",
                                                      "Urban Heat Island Intensity (UHII)" = "UHII"
                                                    )),
                                       radioButtons("forest_color",tags$span(style = "font-weight: bold;", "Color by MSA Characteristic:"),
                                                    c("None" = "none",
                                                      "Avg. Ambient Temperature" = "avg.temp",
                                                      "Avg. Temperature Range" = "avg.range",
                                                      "Region" = "region",
                                                      "Climate Type" = "climate")),
                                       checkboxInput("sort", "Sort by MSA Characteristic",
                                                     value=FALSE)
                                   ),
                                   box(width=15, title="Key Takeaways", solidHeader = TRUE,status="primary",
                                       tags$style(HTML("ul { list-style-position: outside; padding-left: 1em;} ")),
                                       htmlOutput("msa_takeaway")%>% withSpinner(color="#a9a9a9")
                                   )
                            ),
                            tabBox(width=6,title="",id="cbsa_tabs",
                                   tabPanel("Forest/Bar Plot",
                                            tags$style(tabbox.style),
                                            tags$style(type = "text/css", "#forest_plot {height: calc(100vh + 1000px) !important;}"),
                                            plotlyOutput('forest_plot') %>% withSpinner(color="#a9a9a9")
                                   ),
                                   tabPanel("Interactive Map",
                                            tags$style(type = "text/css", "#cbsa_map {height: calc(100vh - 180px) !important;}",
                                                       ".shiny-output-error { visibility: hidden; }",
                                                       ".shiny-output-error:before { visibility: hidden; }"),
                                            leafletOutput("cbsa_map")  %>% withSpinner(color="#a9a9a9")
                                   )
                                   
                            ),
                            
                            box(width=4,title="MSA-Specific Results",solidHeader = TRUE,status="primary",
                                selectInput(inputId="cbsa_choice",
                                            label="Select an MSA (below or in the forest/bar plot or map to the left):",
                                            choices = unique(cbsa_all$CBSA.Title)),
                                htmlOutput("cbsa_title"),
                                plotOutput("cbsa_curve",height=300)  %>% withSpinner(color="#a9a9a9"),
                                DT::dataTableOutput("cbsa_table")  %>% withSpinner(color="#a9a9a9"),
                                tags$style(type="text/css", "#downloadMSAData {background-color:#F8F8F8;border-color:#dbd9d9;color: black}"),
                                downloadButton("downloadMSAData", "Download results for all MSAs")
                            )
                          )
                 ),
                 # Tab for the Exposure & Outcome Maps
                 tabPanel(title="Exposure & Outcome Maps",
                          fluidRow(tags$style(HTML(box.style)),
                                   box(width=2, title="Options", solidHeader = TRUE,status="primary",
                                       htmlOutput("map_info_text"),
                                       radioButtons("variable", tags$span(style = "font-weight: bold;", "Variable:"),
                                                    c("Avg. Ambient Temperature" = "temp",
                                                      "Avg. Temperature Range" = "range",
                                                      "99th Temperature Percentile"="temp.99",
                                                      "# Hospitalizations" = "hosp",
                                                      "Urban Heat Island Intensity (UHII)" = "uhi",
                                                      "UHII Quartile" = "uhiq"
                                                    )),
                                       tags$head(tags$script(HTML("
                                       $(document).ready(function(e) {
                                          $('input[value=\"temp\"]').parent().parent().before('<i><u>Exposure</u><i>');
                                       })
                                       "))),
                                       tags$head(tags$script(HTML("
                                       $(document).ready(function(e) {
                                          $('input[value=\"temp.99\"]').parent().parent().after('<br><i><u>Outcome</u><i>');
                                       })
                                       "))),
                                       tags$head(tags$script(HTML("
                                       $(document).ready(function(e) {
                                          $('input[value=\"hosp\"]').parent().parent().after('<br><i><u>UHI</u><i>');
                                       })
                                       ")))
                                       
                                   ),
                                   box(width=10,title="Interactive Map", solidHeader = TRUE,status="primary",
                                       tags$style(type = "text/css", "#leaflet_map {height: calc(100vh - 180px) !important;}"),
                                       leafletOutput("leaflet_map")  %>% withSpinner(color="#a9a9a9"),
                                       htmlOutput("leaflet_text")
                                   )
                          )
                 ),
                 
                 inverse=F
)

# Server function
server <- function(input, output,session) { 
  
  # Observe event used to generate leaflet maps
  observe({
    
    # Maps for the Exposure & Outcome Maps tab
    if (input$tabs == "Exposure & Outcome Maps") { 
      if (input$variable=="uhi") {
        var = "UHI_summer_day.Wght"; title = "UHII (\u00b0C)"
        rev=F
      } else if (input$variable=="hosp") {
        var="n.HAs"; title = "# Hospitalizations"
        bins = c(0,1500,3000,4500,6000,7500,9000,10500,12000,70000)
        labs = c("0-1,500","1,500-3,000","3,000-4,500","4,500-6,000","6,000-7,500","7,500-9,000","9,000-10,500","10,500-12,000",">12,000")
        rev=T
      } else if (input$variable=="temp") {
        var = "m.temp"; title = "Mean Temp. (\u00b0C)"
        rev=T
      } else if (input$variable=="range") {
        var = "r.temp"; title = "Temp. Range (\u00b0C)"
        rev=T
      } else if (input$variable=="temp.99") {
        var = "temp.99";title = "99th Temp. %ile (\u00b0C)"
        rev=T
      } else if (input$variable == "uhiq") {
        var = "UHI_group"; title = "UHII Quartile"
      }
      
      if (input$variable=="uhiq") {
        pal <- colorFactor(
          palette = c("#66B7D6","#DCF2FA","#FAD0C8","#E34D34"),
          domain = eval(parse(text=paste0("zips_shape$",var))),
          reverse=F
        )
        popup_text <-  paste("<b>ZIP Code:</b>", zips_shape$ZIP, "<br>",
                             "<b>MSA:</b>",zips_shape$CBSA.Title,"<br>",
                             "<b>",title,":</b>", eval(parse(text=paste0("zips_shape$",var))))
        
        labeller_function <- labelFormat(prefix="")
      } else if (input$variable=="uhi") {
        pal <- colorNumeric(
          palette = c( "#5385BC", "#6C9ECE", "#85B7E0", "#9FD1F2", "#B5DBF2", "#C4C6CE", "#D4B1AA", "#E39C86", "#E7886D", "#E6745A", "#E46047", "#E34D34"),
          domain = eval(parse(text=paste0("zips_shape$",var))),
          reverse = rev
        )
        
        popup_text <-  paste("<b>ZIP Code:</b>", zips_shape$ZIP, "<br>",
                             "<b>MSA:</b>",zips_shape$CBSA.Title,"<br>",
                             "<b>",title,":</b>", round(eval(parse(text=paste0("zips_shape$",var))),1))
        
        labeller_function <- labelFormat(prefix = "")
      } else if (input$variable%in%c("temp","range","temp.99")) {
        pal <- colorNumeric(
          palette = hcl.colors(10,palettes[palettes$Variable==input$variable,2]),
          domain = eval(parse(text=paste0("zips_shape$",var))),
          reverse = rev
        )
        
        popup_text <-  paste("<b>ZIP Code:</b>", zips_shape$ZIP, "<br>",
                             "<b>MSA:</b>",zips_shape$CBSA.Title,"<br>",
                             "<b>",title,":</b>", round(eval(parse(text=paste0("zips_shape$",var))),1))
        
        labeller_function <- labelFormat(prefix = "")
      } else {
        pal <- colorBin(
          palette = hcl.colors(10,palettes[palettes$Variable==input$variable,2]),
          domain = eval(parse(text=paste0("zips_shape$",var))),
          bins = bins,
          reverse = rev
        )
        
        popup_text <-  paste("<b>ZIP Code:</b>", zips_shape$ZIP, "<br>",
                             "<b>MSA:</b>",zips_shape$CBSA.Title,"<br>",
                             "<b>",title,":</b>", round(eval(parse(text=paste0("zips_shape$",var))),1))
        
        
        labeller_function <- function(type, breaks) {
          return(labs)
        }
        
      }
      
      leafletProxy("leaflet_map") %>%
        clearShapes() %>% clearControls() %>%
        addPolygons(data=cbsas_shape_full,color="#d7d7d7",weight=1,opacity=0.7,smoothFactor = 0.5,
                    fillColor = "#e9e9e9",fillOpacity = 0.25) %>%
        addPolygons(data=zips_shape,color = ~pal(get(var)), weight=1,
                    fillColor=~pal(get(var)),
                    smoothFactor = 0.5,fillOpacity=0.7,
                    label=lapply(popup_text, htmltools::HTML),
                    highlight = highlightOptions(color = "lightyellow",weight = 2, bringToFront = T, opacity = 0.7)) %>%
        addLegend(data=zips_shape,"bottomright", pal = pal, values = ~get(var),
                  title = title,
                  opacity = 1,
                  labFormat = labeller_function)
    }
    
    # Maps for the MSA-Specific Results tabs
    if (input$tabs=="MSA-Specific Results" & input$cbsa_tabs == "Interactive Map") { 
      
      rev = F
      
      if (input$forest_variable == "rr.99") {
        x="RR.99";xmin="RR.99.low";xmax="RR.99.high";leg_lab="RR (99th vs. MHP)";hover.lab = "RR (95% CI)"
        bins <- c(0.5,0.97,1,1.03,1.06,1.09,1.3)
        labs <- c("< 0.97","0.97-1.00","1.00-1.03","1.03-1.06","1.06-1.09","> 1.09")
        
        rounding=3
      } else if (input$forest_variable == "an.heat") {
        x="heat_an";xmin="heat_an_lower";xmax="heat_an_upper";leg_lab="AN (Temp. \u2265 MHP)";hover.lab = "AN (95% CI)"
        if (input$forest_type == "Primary") {
          bins <- c(-10000,-500,0,500,1000,1500,12000)
          labs <- c("< -500","-500-0","0-500","500-1,000","1,000-1,500","> 1,500")
        } else {
          bins <- c(-10000,-250,0,250,500,750,12000)
          labs <- c("< -250","-250-0","0-250","250-500","500-750","> 750")
        }
        rounding=0
      } else if (input$forest_variable == "ar.heat") {
        x="ann_an_rate100k";xmin="ann_an_rate100k_lower";xmax="ann_an_rate100k_upper";leg_lab="AR (per 100k)";hover.lab = "AR (95% CI)"
        bins <- c(-100,-5,0,5,15,25,150)
        labs <- c("< -5","-5-0","0-5","0-15","15-25","> 25")
        rounding=2; rev=T
      } else if (input$forest_variable == "mht") {
        x="cen";xmin="cen.low";xmax="cen.high";leg_lab="MHT (\u00b0C)";hover.lab = "MHT (95% CI)"
        bins <- c(17,21,24,27,30,33,45)
        labs <- c("< 21","21-24","24-27","27-30","30-33","> 33")
        rounding=1
      }
      
      cbsas_shape <- cbsas_shape_full[cbsas_shape_full$group == input$forest_type,]
      
      pal <- colorBin(
        palette = hcl.colors(8,palettes_cbsa[palettes_cbsa$CBSAVariable==input$forest_variable,2]),
        bins = bins,
        reverse=rev
      )
      
      labeller_function <- function(type, breaks) {
        return(labs)
      }
      
      
      if (input$forest_type == "Primary") {
        popup_text <- paste0("<b>MSA:</b> ",cbsas_shape$CBSA.Title,
                             '<br><b>',hover.lab,":</b> ", trimws(format(round(eval(parse(text=paste0("cbsas_shape$",x))),rounding),rounding, big.mark = ',')),
                             " (",trimws(format(round(eval(parse(text=paste0("cbsas_shape$",xmin))),rounding),rounding,big.mark=',')),", ",
                             trimws(format(round(eval(parse(text=paste0("cbsas_shape$",xmax))),rounding),rounding,big.mark=',')),")")
        
        leafletProxy("cbsa_map") %>%
          clearShapes() %>% clearControls() %>%
          addPolygons(data=cbsas_shape,
                      layerId=cbsas_shape$CBSA.Title,
                      color = ~pal(get(x)), weight=1,
                      fillColor=~pal(get(x)),
                      smoothFactor = 0.5,fillOpacity=0.8,
                      label=lapply(popup_text, htmltools::HTML),
                      highlight = highlightOptions(color = "lightyellow",weight = 2, bringToFront = T, opacity = 0.7)) %>%
          addLegend(data=cbsas_shape,"bottomright",
                    pal = pal, values = ~get(x),
                    labFormat = labeller_function,
                    title = leg_lab,
                    opacity = 1)
      } else {
        cbsas_shape.1 <- cbsas_shape[cbsas_shape$var=="UHII-Q1",]
        popup_text.1 <- paste0("<b>MSA:</b> ",cbsas_shape.1$CBSA.Title,
                               "<br><b>UHII Level:</b> Low UHII",
                               '<br><b>',hover.lab,":</b> ", trimws(format(round(eval(parse(text=paste0("cbsas_shape.1$",x))),rounding),rounding,big.mark=',')),
                               " (",trimws(format(round(eval(parse(text=paste0("cbsas_shape.1$",xmin))),rounding),rounding,big.mark=',')),", ",
                               trimws(format(round(eval(parse(text=paste0("cbsas_shape.1$",xmax))),rounding),rounding,big.mark=',')),")")
        cbsas_shape.4 <- cbsas_shape[cbsas_shape$var=="UHII-Q4",]
        cbsas_shape.4$CBSA.Title <- paste0(cbsas_shape.4$CBSA.Title," ")
        popup_text.4 <- paste0("<b>MSA:</b> ",cbsas_shape.4$CBSA.Title,
                               "<br><b>UHII Level:</b> High UHII",
                               '<br><b>',hover.lab,":</b> ", trimws(format(round(eval(parse(text=paste0("cbsas_shape.4$",x))),rounding),rounding,big.mark = ',')),
                               " (",trimws(format(round(eval(parse(text=paste0("cbsas_shape.4$",xmin))),rounding),rounding,big.mark=',')),", ",
                               trimws(format(round(eval(parse(text=paste0("cbsas_shape.4$",xmax))),rounding),rounding,big.mark=',')),")")
        
        leafletProxy("cbsa_map") %>%
          clearShapes() %>% clearControls() %>%
          addPolygons(data=cbsas_shape.1,group="Low UHII",
                      layerId=cbsas_shape.1$CBSA.Title,
                      color = ~pal(get(x)), weight=1,
                      fillColor=~pal(get(x)),
                      smoothFactor = 0.5,fillOpacity=0.8,
                      label=lapply(popup_text.1, htmltools::HTML),
                      highlight = highlightOptions(color = "lightyellow",weight = 2, bringToFront = T, opacity = 0.7)) %>%
          addPolygons(data=cbsas_shape.4,group="High UHII",
                      layerId=cbsas_shape.4$CBSA.Title,
                      color = ~pal(get(x)), weight=1,
                      fillColor=~pal(get(x)),
                      smoothFactor = 0.5,fillOpacity=0.8,
                      label=lapply(popup_text.4, htmltools::HTML),
                      highlight = highlightOptions(color = "lightyellow",weight = 2, bringToFront = T, opacity = 0.7)) %>%
          addLegend(data=cbsas_shape.4,
                    "bottomright",
                    pal = pal, values = ~get(x),
                    title = leg_lab,
                    labFormat = labeller_function,
                    opacity = 1)%>%
          addLayersControl(
            position = 'topleft',
            baseGroups = c("Low UHII","High UHII"),
            options = layersControlOptions(collapsed = F)
          ) %>%
          htmlwidgets::onRender("
              function() {
                var map = this;
                var legends = map.controls._controlsById;
                function addActualLegend() {
                   var sel = $('.leaflet-control-layers-base').find('input[type=\"radio\"]:checked').siblings('span').text().trim();
                   $.each(map.controls._controlsById, (nm) => map.removeControl(map.controls.get(nm)));
                   map.addControl(legends[sel]);
                }
                $('.leaflet-control-layers-base').on('click', addActualLegend);
                addActualLegend();
             }"
          )
      }
    }
    
  })
  
  #############################################################################
  ######## Outputs for Overall & Subpopulation Results Tab ####################
  #############################################################################
  
  # Code to render the plots of the exposure-lag-response curves
  output$pooled_curve <- renderPlot({
    
    if (input$type == "pooled") {
      
      curves <- pooled[lapply(pooled, '[[',"type")==input$sub]
      curves <- curves[lapply(curves, '[[',"group")==input$subpop]
      
      colors  <- eval(parse(text=paste0('sub_palettes$',paste0(input$sub,input$subpop))))
      ltypes <- eval(parse(text=paste0('sub_linetypes$',paste0(input$sub,input$subpop))))
      
      inds <- c(50,60,70,80,90,99,100)
      
      limits <- c(round(min(pooled[["Primary"]]$tmeancbsa[names(pooled[["Primary"]]$tmeancbsa) %in% paste0(inds,".0%")]),1)-.1,round(max(pooled[["Primary"]]$tmeancbsa[names(pooled[["Primary"]]$tmeancbsa) %in% paste0(inds,".0%")]),1))
      
      if (!(input$sub == "Primary" && input$subpop == "All")) {
        if (input$sub != "Primary" && input$subpop != "All") {
          groups <- unique(unlist(lapply(curves, '[[',"subgroup")))
          if (input$subpop=="Age") {n.col = 3} else { n.col = 2}
          par(mfrow = c(1, n.col), xaxs = "i",mar=c(5.1+1, 4.1, 4.1, 2.1-1))
        } else {
          groups <- c("All")
          par(mfrow = c(1, 1),mar=c(5.1+1, 4.1, 2.1, 2.1-1))
        }
        
      } else {
        if (input$sub != "Primary" && input$subpop != "All") {
          groups <- unique(unlist(lapply(curves, '[[',"subgroup")))
          if (input$subpop=="Age") {n.col = 3} else { n.col = 2}
          par(mfrow = c(1, n.col), xaxs = "i",mar=c(5.1, 4.1, 4.1, 2.1-1))
        } else {
          groups <- c("All")
          par(mfrow = c(1, 1),mar=c(5.1, 4.1, 2.1, 2.1-1))
        }
      }
      curr_min=100;curr_max=0
      
      curves.orig <- curves
      
      plist <- list()
      
      for (j in 1:length(groups)) {
        
        if (length(groups) > 1) {
          curves <- curves.orig[lapply(curves.orig, '[[',"subgroup")==groups[j]]
        } else {
          curves <- curves.orig
        }
        
        names(curves)[names(curves) == "sexWomen"] <- "sexFemale"
        
        curves <- curves[order(names(curves))]
        
        for (i in 1:length(curves)) {
          pred <- curves[[i]]$pred.metareg
          cen <- curves[[i]]$cen.metareg
          
          if (input$sub == "Primary" & input$subpop == "All") {
            predvar <- curves[[i]]$tmeancbsa
            indlab <- names(predvar) %in% paste0(inds,".0%")
            
            plot(pred,"overall",type="l",lwd=2.5,col=colors[i],lty=ltypes[i],ylim=c(0.9,1.3),xlim=limits,axes=F,xlab="",
                 ylab="RR", ci.arg=list(density=20+((i-1)*5),lty=ltypes[i],col=adjustcolor(colors[i],alpha.f=0.2)))
            
            abline(v=cen,lty=2)
            axis(1,at=predvar[indlab],labels=inds)
            loc = 14.25
            mtext("%ile",1,line=1,at=loc,col="black",adj=1,cex=1)
            axis(1,at=seq(15,33,2),line=2.5,col="black",col.ticks="black",col.axis="black")
            axis(1,at=seq(15,33,1),labels=NA,line=2.5,col="black",col.ticks="black",col.axis="black",tck=-0.009)
            mtext(expression(paste(degree, "C")),1,line=3.5,at=loc,col="black",adj=1,cex=1)
            axis(2)
          } else {
            plot(pred,"overall",type="l",lwd=2.5,col=colors[i],lty=ltypes[i],ylim=c(0.9,1.3),xlim=limits,axes=F,xlab="",
                 ylab="RR", ci.arg=list(density=20+((i-1)*5),lty=ltypes[i],col=adjustcolor(colors[i],alpha.f=0.25)))
            abline(v=cen,lty=2,col=colors[i])
            predvar <- curves[[i]]$tmeancbsa
            indlab <- names(predvar) %in% paste0(inds,".0%")
            
            if (i != 1) {axis(1,at=predvar[indlab],labels=NA,col=NA,col.ticks=colors[i],tck=-0.03); tckcol = NA} else { tckcol = colors[i]}
            axis(1,at=predvar[indlab],labels=inds,col=NA,col.ticks=tckcol,
                 col.axis=colors[i],cex.axis=0.95,line=((i-1)/1.5))
            
            if (min(predvar[indlab]) < curr_min) {curr_min<-min(predvar[indlab])}
            if (max(predvar[indlab]) > curr_max) {curr_max<-max(predvar[indlab])}
            
            if (i == length(curves)) {
              loc = 14.25
              
              axis(1,at=c(curr_min,curr_max),labels=NA,col="black",col.ticks=NA,col.axis=NA)
              mtext("%ile",1,line=1,at=loc,col="black",adj=1)
              
              extra <- (1-length(curves))*(2/3)
              axis(1,at=seq(15,33,2),line=2.5-extra,col="black",col.ticks="black",col.axis="black")
              axis(1,at=seq(15,33,1),labels=NA,line=2.5-extra,col="black",col.ticks="black",col.axis="black",tck=-0.009)
              mtext(expression(paste(degree, "C")),1,line=3.5-extra,at=loc,col="black",adj=1)
              
              axis(2)
              
              par(new = F)
            } else {
              par(new=T)
            }
          }
        }
        
        if (!(input$sub == "Primary" && input$subpop == "All")) {
          
          if (input$sub != "Primary" && input$subpop != "All") {
            title(main = paste0(input$subpop, ", ", groups[j]))
            legend(x=par("usr")[1]+0.25,y=par("usr")[4], legend=c("Low UHII","High UHII"),
                   col=colors, lty=ltypes, lwd=1.5,cex=1,box.lty=0)
          } else if (input$sub=="Primary") {
            
            legend(x=par("usr")[1]+0.25,y=par("usr")[4], legend=paste0(unlist(lapply(curves, `[`, c('group'))),', ',unlist(lapply(curves, `[`, c('subgroup')))),
                   col=colors,  lty=ltypes, lwd=1.5,cex=1,box.lty=0)
          } else if (input$subpop == "All") {
            legend(x=par("usr")[1]+0.25,y=par("usr")[4], legend=c("Low UHII","High UHII"),
                   col=colors, lty=ltypes, lwd=1.5,cex=1,box.lty=0)
          }
          
        } 
        
      }
      
    } else if (input$type == "lags") {
      
      curves <- pooled.99[lapply(pooled.99, '[[',"type")==input$sub]
      curves <- curves[lapply(curves, '[[',"group")==input$subpop]
      
      colors  <- eval(parse(text=paste0('sub_palettes$',paste0(input$sub,input$subpop))))
      ltypes  <- eval(parse(text=paste0('sub_linetypes$',paste0(input$sub,input$subpop))))
      
      if (input$sub != "Primary" && input$subpop != "All") {
        groups <- unique(unlist(lapply(curves, '[[',"subgroup")))
        if (input$subpop=="Age") {n.col = 3} else { n.col = 2}
        par(mfrow = c(1, n.col),mar=c(5.1, 4.1, 4.1, 2.1-1))
      } else {
        groups <- c("All")
        par(mfrow = c(1, 1),mar=c(5.1, 4.1, 2.1, 2.1-1))
      }
      
      curves.orig <- curves
      
      plist <- list()
      
      for (j in 1:length(groups)) {
        
        if (length(groups) > 1) {
          curves <- curves.orig[lapply(curves.orig, '[[',"subgroup")==groups[j]]
        } else {
          curves <- curves.orig
        }
        
        names(curves)[names(curves) == "sexWomen"] <- "sexFemale"
        
        curves <- curves[order(names(curves))]
        
        for (i in 1:length(curves)) {
          pred <- curves[[i]]$pred.99
          
          if (input$sub == "Primary" & input$subpop=="All") {
            
            plot(pred,"overall",ylim=c(0.98,1.03),xlim=c(0,21),axes=T,xlab="Lag",
                 ylab="RR",lwd=2.5,xlab=c(0:21))
          } else {
            
            plot(pred,"overall",col=colors[i],lty=ltypes[i],ylim=c(0.98,1.03),xlim=c(0,21),axes=T,xlab="Lag",
                 ylab="RR",lwd=2.5,xlab=c(0:21),
                 ci.arg=list(density=20+((i-1)*5),lty=ltypes[i],col=adjustcolor(colors[i],alpha.f=0.25)))
            
            if (input$sub != "Primary" && input$subpop != "All") {
              title(main=paste0(input$subpop, ", ", groups[j]))
              legend(x=par("usr")[1]+0.25,y=par("usr")[4], legend=c("Low UHII","High UHII"),
                     col=colors, lty=ltypes, lwd=1.5,cex=1,box.lty=0)
            } else if (input$sub=="Primary") {
              legend(x=par("usr")[1]+0.25,y=par("usr")[4], legend=paste0(unlist(lapply(curves, `[`, c('group'))),', ',unlist(lapply(curves, `[`, c('subgroup')))),
                     col=colors, lty=ltypes, lwd=1.5,cex=1,box.lty=0)
            } else if (input$subpop == "All") {
              legend(x=par("usr")[1]+0.25,y=par("usr")[4], legend=c("Low UHII","High UHII"),
                     col=colors, lty=ltypes, lwd=1.5,cex=1,box.lty=0)
            }
            
            if (i == length(curves)) {
              par(new=F)
            } else {
              par(new=T)
            }
          }
        }
      }
    }
  })
  
  # Informational text for user selection
  output$overall_info_text <- renderText({
    paste("Select the subpopulation, stratification, and plot type to display:<br><br>")
  })
  
  # Figure caption for the exposure-lag-response curve
  output$pooled_curve_text <- renderText({
    
    if (input$type == "lags") {
      type = "Lag-response at extreme heat (99th temperature percentile) for the association(s) between daily mean ambient temperature and daily cardiovascular disease hospitalizations in the urban cores of 120 contiguous US metropolitan statistical areas, 2000-2017."
    } else {
      type = "The 21-day cumulative exposure-response association(s) between daily mean ambient temperature and daily cardiovascular disease hospitalizations in the urban cores of 120 contiguous US metropolitan statistical areas, 2000-2017. The vertical dashed line indicates the location of the minimum hospitalization percentile (MHP)." 
    }
    
    if (input$subpop == "All") {
      if (input$sub=="UHII") {
        caption = paste0(type," Results shown for the entire study population in low and high urban heat island intensity (UHII) areas.")
      }  else if (input$sub == "Primary") {
        caption = paste0(type, " Results shown for the entire study population.")
      }   
      
    } else {
      if (input$sub == "Primary") {
        if (input$subpop %in% c("Diabetes","CKD")) {
          label <- ifelse(input$subpop=="Diabetes",tolower(input$subpop),input$subpop)
          caption = paste0(type, " Results shown by ", label, " status.")
        } else {
          caption = paste0(type, " Results shown by ", tolower(input$subpop), ".")
        }
      } else if (input$sub == "UHII") {
        if (input$subpop %in% c("Diabetes","CKD")) {
          label <- ifelse(input$subpop=="Diabetes",tolower(input$subpop),input$subpop)
          
          caption = paste0(type, " Results shown by ", label, " status in low and high urban heat island intensity (UHII) areas.")
        } else {
          caption = paste0(type, " Results shown by ", tolower(input$subpop), " in low and high urban heat island intensity (UHII) areas.")
        }
      }
    }
  })
  
  # Code to render the primary results plots (forest plot + bar plot)
  output$pooled_plots <- renderPlotly({
    
    ### FOREST PLOT
    data.forest <- sub_rr_af_an[,c(1:11,15:17)];  names(data.forest) <- c(names(data.forest)[1:11],"RR","RR.low","RR.high")
    data.forest$variable_name <- rep("Relative Risk (99th vs. MHP)",nrow(data.forest))
    data.forest[,c("RR","RR.low","RR.high","cen.metareg")] <- sapply( data.forest[,c("RR","RR.low","RR.high","cen.metareg")],as.numeric)
    data.forest <- data.forest[data.forest$type == input$sub & data.forest$group == input$subpop,]
    data.forest$uhigroup <- ifelse(data.forest$subtype == "Primary","Overall",ifelse(data.forest$subtype=="Q1","Low UHII","High UHII"))
    data.forest$label <- ifelse(data.forest$type=="Primary"&data.forest$group=="All","",
                                ifelse(data.forest$group=="All",data.forest$uhigroup,paste0(data.forest$group,", ",data.forest$subgroup)))
    data.forest$label2 <- ifelse(data.forest$type=="Primary"&data.forest$group=="All","",
                                 ifelse(data.forest$group=="All",data.forest$uhigroup,
                                        ifelse(data.forest$type=="Primary",
                                               paste0(data.forest$group,", ",data.forest$subgroup),
                                               paste0(data.forest$group,", ",data.forest$subgroup," - ",data.forest$uhigroup)
                                        )))
    
    hover.lab = "RR (95% CI)"; rounding=3; xline=1; ylab = "RR"
    if (input$sub != "Primary" & input$subpop != "All") {
      pd <- position_dodge(width = 0.4)
      col.val <- "uhigroup"
    } else {
      pd <-  position_dodge(width = 0)
      col.val <- "label"
    }
    
    if (input$sub != "Primary") {
      pal <- rev(eval(parse(text=paste0('sub_palettes$',paste0(input$sub,input$subpop)))))
    } else {
      pal <- eval(parse(text=paste0('sub_palettes$',paste0(input$sub,input$subpop)))) 
    }
    
    facet.name <- c( "Relative Risk (99th vs. MHP)" =  "Relative Risk\n(99th vs. MHP)")
    forest.pooled <- ggplot(data=data.forest[data.forest$variable_name == "Relative Risk (99th vs. MHP)", ], 
                            aes(x=label,y=RR, ymin=RR.low, ymax=RR.high,color=.data[[col.val]],shape=.data[[col.val]],
                                text = paste0("<b>",label2,"</b>",
                                              '<br><b>',hover.lab,":</b> ", format(round(RR,rounding),rounding)," (",
                                              format(round(RR.low,rounding),rounding),", ",
                                              format(round(RR.high,rounding),rounding),")",'<br>',
                                              '<b>MHP (MHT):</b> ',cen.per.metareg,' (',round(cen.metareg,1),' \u00b0C)'))) +
      geom_point(position=pd,size=2) + 
      scale_color_manual(values=pal) + 
      geom_errorbar(width=0.1,position=pd) + theme_bw(base_size=10) + 
      geom_hline(yintercept=xline, color='black', linetype='dashed', alpha=.5) +
      ylab(ylab) + facet_wrap(~variable_name,labeller = as_labeller(facet.name)) +
      theme(axis.title.x = element_blank(),
            strip.background = element_rect(fill = "white",color="white"),
            strip.text = element_text(angle=0,face="bold"),
            axis.text.x = element_text(angle = 30, hjust=1)
      ) +
      ylim(0.96,1.06)
    
    p1 <- ggplotly(forest.pooled,tooltip="text",source="pooled_plots") %>%
      layout(hoverlabel=list(bgcolor="white",align='left'),
             yaxis = list(titlefont = list(size = 12),title = ylab), dragmode = F,
             legend = list(title = list(text = ""),orientation = "h", x = -0.5, y =-1))  %>% config(displayModeBar = F)
    
    ### BAR PLOT
    data <- sub_rr_af_an[sub_rr_af_an$type == input$sub&sub_rr_af_an$group == input$subpop,]
    
    if (input$bar.var == "AN") {
      lab = "Attributable Number (MHP+)"
      hover.lab = "AN (95% CI)";ylab="AN"
      facet.name <- c("Attributable Number (MHP+)"="Attributable Number\n(Temp. \u2265 MHP)")
      lims <- c(-362,38000); rounding=0
      y="ha_heat";ymin="ha_heat_lower";ymax="ha_heat_upper"
    } else {
      lab = "Attributable Rate (Annual, per 100k)"
      hover.lab = "AR (95% CI)";ylab="AR"
      facet.name <- c("Attributable Rate (Annual, per 100k)"="Attributable Rate\n(Annual, per 100k)")
      lims <- c(-2,50); rounding=2
      y="ann_an_rate100k";ymin="ann_an_rate100k_lower";ymax="ann_an_rate100k_upper"
    }
    
    data$exposure <- rep(lab,nrow(data)); data$exposure.f = factor(data$exposure, levels=c(lab))
    
    data$uhigroup <- ifelse(data$subtype == "Primary","Overall",ifelse(data$subtype=="Q1","Low UHII","High UHII"))
    data$label <- ifelse(data$type=="Primary"&data$group=="All","",
                         ifelse(data$group=="All",data$uhigroup,paste0(data$group,", ",data$subgroup)))
    data$label2 <- ifelse(data$type=="Primary"&data$group=="All","",
                          ifelse(data$group=="All",data$uhigroup,
                                 ifelse(data$type=="Primary",
                                        paste0(data$group,", ",data$subgroup),
                                        paste0(data$group,", ",data$subgroup," - ",data$uhigroup)
                                 )))
    
    
    xline=0
    
    sub_palettes.temp <- sub_palettes; sub_palettes.temp[1] <- "grey40"
    
    if (input$sub != "Primary") {
      pal <- rev(eval(parse(text=paste0('sub_palettes.temp$',paste0(input$sub,input$subpop)))))
    } else {
      pal <- eval(parse(text=paste0('sub_palettes.temp$',paste0(input$sub,input$subpop)))) 
    }
    
    if (input$sub != "Primary" & input$sub != "All") { pd <- position_dodge(width = 0.9) } else { pd <- position_dodge(width = 0.0)}
    bar.pooled <- ggplot(data=data[data$exposure == lab,], 
                         aes(x=label,y=.data[[y]], ymin=.data[[ymin]], ymax=.data[[ymax]],fill=.data[[col.val]],
                             text = paste0("<b>",label2,"</b>",
                                           '<br><b>',hover.lab,":</b> ", format(round(.data[[y]],rounding),big.mark =',',trim=T),
                                           " (",format(round(.data[[ymin]],rounding),big.mark=',',trim=T),", ",
                                           format(round(.data[[ymax]],rounding),big.mark=',',trim=T),")",
                                           '<br><b>','MHP (MHT):</b> ',cen.per.metareg,' (',round(cen.metareg,1),' \u00b0C)'))) + 
      geom_col(alpha=0.85,position=pd) +   geom_errorbar(width=.1,color='black',position=pd) + theme_bw(base_size=10) + 
      scale_fill_manual(values=pal) + 
      geom_hline(yintercept=xline, color='black', linetype='dashed', alpha=.5) +
      ylab(ylab) + facet_wrap(~exposure.f,labeller = as_labeller(facet.name)) + 
      theme(axis.title.x = element_blank(),
            strip.background = element_rect(fill = "white",color="white"),
            strip.text = element_text(angle=0,face="bold"),
            axis.text.x = element_text(angle = 30, hjust=1)
      ) + ylim(lims)
    
    p2 <- ggplotly(bar.pooled,tooltip="text",source="pooled_plots") %>%
      layout(hoverlabel=list(bgcolor="white",align='left'), yaxis = list(titlefont = list(size = 12),title = ylab), dragmode = F,
             legend = list(title = list(text = ""),orientation = "h", x = 0, y =0.125)) %>% 
      config(displayModeBar = F)
    
    if (input$sub=="UHII") {
      subplot(p1, style(p2,showlegend=F),margin = 0.075,titleY=T)
    } else {
      subplot(style(p1,showlegend=F), style(p2,showlegend=F),margin = 0.075,titleY=T)
    }
  })
  
  # Figure caption for the primary results forest + bar plot
  output$pooled_plot_text <- renderText({
    type = "Heat-related risk and burden"
    if (input$subpop == "All") {
      if (input$sub=="UHII") {
        caption = paste0(type," for the entire study population in low and high urban heat island intensity (UHII) areas, 2000-2017.")
      }  else if (input$sub == "Primary") {
        caption = paste0(type, " for the entire study population, 2000-2017.")
      }   
    } else {
      if (input$sub == "Primary") {
        if (input$subpop %in% c("Diabetes","CKD")) {
          label <- ifelse(input$subpop=="Diabetes",tolower(input$subpop),input$subpop)
          
          caption = paste0(type, " by ", label, " status.")
        } else {
          caption = paste0(type, " by ", tolower(input$subpop), ".")
        }
      } else if (input$sub == "UHII") {
        if (input$subpop %in% c("Diabetes","CKD")) {
          label <- ifelse(input$subpop=="Diabetes",tolower(input$subpop),input$subpop)
          
          caption = paste0(type, " by ", label, " status in low and high urban heat island intensity (UHII) areas.")
        } else {
          caption = paste0(type, " by ", tolower(input$subpop), " in low and high urban heat island intensity (UHII) areas.")
        }
      }
    }
    caption = paste0(caption," Metrics include the relative risk (RR) at extreme heat (99th temperature percentile compared to the minimum hospitalization percentile [MHP]) and the heat-attributable (temperatures above the MHP) number (AN) / annual rate (AR) of cardiovascular hospitalizations.")
  })
  
  # Code to render the table of results
  output$pooled_table <- DT::renderDataTable({
    
    sub_rr_af_an$Total_HAs <- as.numeric(sub_rr_af_an$Total_HAs)
    sub_rr_af_an$Total_Heat_HAs <- as.numeric(sub_rr_af_an$Total_Heat_HAs)
    
    
    sub_rr_af_an$subpop <- ifelse(sub_rr_af_an$group=="All","All",paste0(sub_rr_af_an$group,", ",sub_rr_af_an$subgroup))
    sub_rr_af_an$uhigroup <- ifelse(sub_rr_af_an$type=="Primary","Primary",
                                    ifelse(sub_rr_af_an$subtype == "Q1",
                                           "Low UHII","High UHII"))
    
    sub_rr_af_an$group_lab <- ifelse(sub_rr_af_an$type == "Primary", ifelse(sub_rr_af_an$group == "All","All",sub_rr_af_an$subpop),
                                     ifelse(sub_rr_af_an$group == "All",
                                            sub_rr_af_an$uhigroup,paste0(sub_rr_af_an$subpop,"<br>",sub_rr_af_an$uhigroup)))
    
    table <- sub_rr_af_an[sub_rr_af_an$type == input$sub&sub_rr_af_an$group == input$subpop,
                          c("group_lab","n.cbsas","Num_Zips","Total_HAs","temp.99",
                            "mhp","mht","RR.99.full","Attr_HAs_Heat","Ann_AN_Rate100k")]
    
    table[is.na(table)] <- "-"
    
    table$per_HAs <- (table$Total_HAs / sub_rr_af_an[sub_rr_af_an$type=="Primary" & sub_rr_af_an$group == "All",]$Total_HAs)*100
    
    table[,c("Total_HAs","Num_Zips")] <- sapply(table[,c("Total_HAs","Num_Zips")],
                                                function(x)paste0(format(round(x),scientific=F,big.mark=','),''))
    
    table[,c("per_HAs")] <- sapply(table[,c("per_HAs")],
                                   function(x)paste0(round(x,1),"%"))
    
    table$Total_HAs <- paste0(table$Total_HAs," (",table$per_HAs,")")
    
    table[,c("temp.99")] <- sapply(table[,c("temp.99")],function(x)paste0(round(x,1)))
    
    table <- table[,setdiff(colnames(table),c("per_HAs"))]
    
    names(table) <- c("Group","# MSAs","# ZIP Codes","# Hospitalizations (% Total)", "99th Temp. Percentile (\u00b0C)",
                      "MHP (95% CI)","MHT (\u00b0C) (95% CI)","RR (99th vs. MHP) (95% CI)",
                      "AN (Temp. \u2265 MHP) (95% CI)", "AR (Annual, per 100k) (95% CI)")
    
    type = "Exposure and outcome information and heat-related cardiovascular risk and burden results"
    if (input$subpop == "All") {
      if (input$sub=="UHII") {
        caption = paste0(type," for the entire study population in low and high urban heat island intensity (UHII) areas, 2000-2017.")
      }  else if (input$sub == "Primary") {
        caption = paste0(type, " for the entire study population, 2000-2017.")
      }   
    } else {
      if (input$sub == "Primary") {
        if (input$subpop %in% c("Diabetes","CKD")) {
          label <- ifelse(input$subpop=="Diabetes",tolower(input$subpop),input$subpop)
          
          caption = paste0(type, " by ", label, " status, 2000-2017.")
        } else {
          caption = paste0(type, " by ", tolower(input$subpop), ", 2000-2017.")
        }
      } else if (input$sub == "UHII") {
        if (input$subpop %in% c("Diabetes","CKD")) {
          label <- ifelse(input$subpop=="Diabetes",tolower(input$subpop),input$subpop)
          
          caption = paste0(type, " by ", label, " status in low and high urban heat island intensity (UHII) areas, 2000-2017.")
        } else {
          caption = paste0(type, " by ", tolower(input$subpop), " in low and high urban heat island intensity (UHII) areas, 2000-2017.")
        }
      }
    }
    
    caption = paste0(caption," MSA = Metropolitan Statistical Area; MHP = Minimum Hospitalization Percentile; MHT = Minimum Hospitalization Temperature; RR = Relative Risk; AN = Heat-Attributable Number; AR = Heat-Attributable Rate")
    
    DT::datatable(table,
                  options = list(paging = T,searching = T,dom='t',ordering=T,scrollX = TRUE,
                                 autoWidth = F,
                                 columnDefs = list(
                                   list(className = "nowrap", targets = "_all")
                                 )
                  ),
                  rownames=FALSE,escape=F,class = 'cell-border stripe',
                  selection = 'none',
                  caption = htmltools::tags$caption(
                    style = 'caption-side: bottom; text-align: left;',
                    caption
                  )) 
  })
  
  # Bulleted list of the main takeaways
  output$main_takeaway <- renderText({
    "<ul>
  <li>Notably larger heat-related burden in high UHII areas, attributed to a moderately higher risk</li>
  <li>Females, individuals aged 75-114, those with CKD, and those with diabetes had an elevated heat-related risk and burden</li>
  <li>High UHII exacerbated heat-related impacts among already heat-vulnerable subpopulations</li>
  <li>Heat posed a delayed, rather than immediate, threat</li>
  </ul>"
  })
  
  # Button to download all results for subpopulatuon analyses 
  output$downloadAllData <- downloadHandler(
    filename <- function() {
      paste("data/Overall-Subpop-Results.csv")
    },
    
    content <- function(file) {
      file.copy("data/Overall-Subpop-Results.csv", file)
    },
    contentType = "text/csv"
  )
  
  #############################################################################
  #### Outputs for Exposure & Outcome Maps Tab ################################
  #############################################################################
  
  # Code to render the leaflet map
  output$leaflet_map <- renderLeaflet({
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron,group="States") %>%
      fitBounds(-123.36119,25.13313,-70.02285,48.30850)
    
  })
  
  # Figure caption for the leaflet map
  output$leaflet_text <- renderText({
    if (input$variable=="uhi") {
      part1 = "population-weighted urban heat island intensity (UHII)"
    } else if (input$variable == "uhiq") {
      part1 = "population-weighted urban heat island intensity (UHII) quartile (Q1 = 'low' UHII areas, Q4 = 'high' UHII areas)"
    } else if (input$variable=="hosp") {
      part1 = "counts of daily cardiovascular-related hospitalizations among Medicare enrollees (age 65-114)"
    } else if (input$variable == "temp") {
      part1 = "population-weighted daily mean ambient temperature"
    } else if (input$variable == "range") {
      part1 = "range of population-weighted daily mean ambient temperature"
    } else if (input$variable == "temp.99") {
      part1 = "99th percentile of population-weighted daily mean ambient temperature"
    } 
    
    if (input$variable%in%c("uhi","uhiq")) {
      part2 = "in the urban cores of 120 contiguous US metropolitan statistical areas (MSAs). Additional information on the UHII metric and a link to download the data can be found <a href='https://www.sciencedirect.com/science/article/pii/S0924271620302082' target='_blank'>here</a>."
    } else {
      part2 = "in the urban cores of 120 contiguous US metropolitan statistical areas (MSAs), 2000-2017."
    }
    
    if (input$variable %in% c("temp","range","temp.99")) {
      part3 = "Temperature data was downloaded from NOAA's Global Surface Summary of the Day dataset, which can be accessed <a href = 'https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.ncdc:C00516' target='_blank'>here</a>."
    } else {
      part3 = ""
    }
    part4 = "The light grey shapes delineate the boundaries of the 120 MSAs. The blank areas within the MSA boundaries indicate areas that did overlap an urban core."
    paste("ZIP code-level",part1,part2,part3,part4)
  })
  
  # Informational text for user selection
  output$map_info_text <- renderText({
    paste("Select the exposure, outcome, or urban heat island (UHI) variable to display:<br><br>")
  })
  
  #############################################################################
  #### Output for MSA-Specific Results Tab ####################################
  #############################################################################
  
  # Code to render the forest/bar plot of MSA-specific results
  output$forest_plot <- renderPlotly({
    
    
    tck_size=0.5
    if (input$forest_variable == "rr.99") {
      x="RR.99";xmin="RR.99.low";xmax="RR.99.high";xline=1;alpha.val=1
      xlab="Relative Risk [RR] (99th vs. MHP)";hover.lab = "RR (95% CI)"
      xlim = c(0.7,1.4)
      rounding = 3
    } else if (input$forest_variable == "an.heat") {
      x="heat_an";xmin="heat_an_lower";xmax="heat_an_upper";xline=0;alpha.val=0.85
      xlab = "Heat-Attributable Number [AN] (Temp. \u2265 MHP)";hover.lab = "AN (95% CI)"
      if (input$forest_type == "UHII") {
        xlim = c(-2600,4100)
      } else {
        xlim = c(-4050,12005)
      }
      rounding = 0
    } else if (input$forest_variable == "ar.heat") {
      x="ann_an_rate100k";xmin="ann_an_rate100k_lower";xmax="ann_an_rate100k_upper";xline=0;alpha.val=0.85
      xlab = "Heat-Attributable Rate [AR] (per 100,000 beneficiaries)";hover.lab = "AR (95% CI)"
      xlim = c(-95,125)
      rounding = 2
    } else if (input$forest_variable == "mht") {
      x="cen";xmin="cen.low";xmax="cen.high";xline=0;alpha.val=1
      xlab = "Minimum Hospitalization Temperature [MHT] (\u00b0C)";hover.lab = "MHT (95% CI)"
      xlim = c(17,42)
      rounding = 1
    }
    
    if (input$forest_color=="avg.temp") {
      color.lab="Avg. Temp (\u00b0C)"; color="m.temp"
      pal = hcl.colors(10,palette="RdYlBu",rev=T)
      lims = c(8,24)
      brks = seq(8,24,2)
    } else if (input$forest_color=="avg.range") { 
      color.lab="Avg. Range (\u00b0C)"; color="r.temp"
      pal = hcl.colors(10,palette="Purple-Green",rev=T)
      lims = c(26,61)
      brks = seq(26,61,5)
    } else if (input$forest_color=="region") {
      color.lab="Region"; color="Region"
      pal=c(
        "Midwest" = "#82B446",
        "South" = "#BB5566",
        "Northeast" = "#4682B4",
        "West" = "#DDAA33"
      )
    } else if (input$forest_color=="climate") {
      color.lab="Climate Type"; color="Koppen.Simple"
      pal= c("Arid" = "#EF4444",
             "Cold" = "#394BA0",
             "Temperate" = "#009F75",
             "Tropical" = "#FAA31B")
    } 
    if (input$forest_type == "Primary") {
      if (input$sort == TRUE & input$forest_color != 'none') {
        forest.data.simp <- cbsa_all[cbsa_all$var == input$forest_type,] %>% mutate(CBSA.f = fct_reorder(CBSA.Title, .[[color]]))
        tck_size = 0.05
      } else {
        forest.data.simp <- cbsa_all[cbsa_all$var == input$forest_type,] %>% mutate(CBSA.f = fct_reorder(CBSA.Title, .[[x]]))
      }
    } else {
      forest.temp <- merge(cbsa_all[cbsa_all$subgroup =="Q1",c("CBSA","CBSA.Title",x)],cbsa_all[cbsa_all$subgroup =="Q4",c("CBSA","CBSA.Title",x)],
                           by=c("CBSA","CBSA.Title"),all.x=T)
      names(forest.temp) <- c("CBSA","CBSA.Title","var.q1",'var.q4')
      forest.temp$diff <- forest.temp$var.q4 - forest.temp$var.q1
      forest.temp$diff <- ifelse(is.na(forest.temp$diff),forest.temp$var.q1-10000,forest.temp$diff)
      forest.data.simp <- merge(cbsa_all[cbsa_all$var != "Primary",],forest.temp[,c("CBSA","diff")],by="CBSA")
      forest.data.simp <- forest.data.simp %>% mutate(CBSA.f = fct_reorder(CBSA.Title, diff))
    }
    
    if (input$forest_type != "Primary") {
      color = "var"
      pal = c("#66C0E4","#E34D34")
      color.lab = ""
      dodge <- position_dodge(0.6)
    } else {
      if (input$forest_color == 'none') {
        color.lab = ""
      }
    }
    
    
    if (input$forest_color %in% c("avg.range","avg.temp")) {
      forest.data.simp$r.temp <- round(forest.data.simp$r.temp,2)
      forest.data.simp$m.temp <- round(forest.data.simp$m.temp,2)
    }
    
    if (input$forest_color == "none" && input$forest_type == "Primary") {
      if (input$forest_variable %in% c("an.heat","ar.heat")) {
        forest.temp <- ggplot(data=forest.data.simp, 
                              aes(y=CBSA.f,x=.data[[x]], xmin=.data[[xmin]], xmax=.data[[xmax]],
                                  text = paste0("<b>MSA:</b> ",CBSA.f,
                                                '<br><b>',hover.lab,":</b> ", trimws(format(round(.data[[x]],rounding),big.mark=','))," (",
                                                trimws(format(round(.data[[xmin]],rounding),big.mark=',')),", ",
                                                trimws(format(round(.data[[xmax]],rounding),big.mark=',')),")")
                              )) 
      } else {
        forest.temp <- ggplot(data=forest.data.simp, 
                              aes(y=CBSA.f,x=.data[[x]], xmin=.data[[xmin]], xmax=.data[[xmax]],
                                  text = paste0("<b>MSA:</b> ",CBSA.f,
                                                '<br><b>',hover.lab,":</b> ", format(round(.data[[x]],rounding),rounding)," (",
                                                format(round(.data[[xmin]],rounding),rounding),", ",
                                                format(round(.data[[xmax]],rounding),rounding),")")
                              )) 
      }
    } else if (input$forest_color != "none" || input$forest_type == "UHII") {
      if (input$forest_variable %in% c("an.heat","ar.heat")) {
        if (input$forest_type == "UHII") {
          forest.temp <- ggplot(data=forest.data.simp, 
                                aes(y=CBSA.f,x=.data[[x]], xmin=.data[[xmin]], xmax=.data[[xmax]],fill=.data[[color]],
                                    text = paste0("<b>MSA: </b>",CBSA.f,"<br>",
                                                  "<b>UHII Level:</b> ",ifelse(.data[[color]]=="UHII-Q1","Low UHII","High UHII"),
                                                  '<br><b>',hover.lab,":</b> ", trimws(format(round(.data[[x]],rounding),big.mark=","))," (",
                                                  trimws(format(round(.data[[xmin]],rounding),big.mark=",")),", ",
                                                  trimws(format(round(.data[[xmax]],rounding),big.mark=",")),")")))
        } else {
          forest.temp <- ggplot(data=forest.data.simp, 
                                aes(y=CBSA.f,x=.data[[x]], xmin=.data[[xmin]], xmax=.data[[xmax]],fill=.data[[color]],
                                    text =  paste0("<b>MSA:</b> ",CBSA.f,
                                                   '<br><b>',hover.lab,":</b> ", trimws(format(round(.data[[x]],rounding),big.mark=","))," (",
                                                   trimws(format(round(.data[[xmin]],rounding),big.mark=",")),", ",
                                                   trimws(format(round(.data[[xmax]],rounding),big.mark=",")),")",
                                                   '<br><b>',color.lab,':</b> ',.data[[color]])))
        }
      } else {
        if (input$forest_type == "UHII") {
          forest.temp <- ggplot(data=forest.data.simp, 
                                aes(y=CBSA.f,x=.data[[x]], xmin=.data[[xmin]], xmax=.data[[xmax]],color=.data[[color]],shape=.data[[color]],
                                    text = paste0("<b>MSA: </b>",CBSA.f,"<br>",
                                                  "<b>UHII Level:</b> ",ifelse(.data[[color]]=="UHII-Q1","Low UHII","High UHII"),
                                                  '<br><b>',hover.lab,":</b> ", format(round(.data[[x]],rounding),rounding)," (",
                                                  format(round(.data[[xmin]],rounding),rounding),", ",
                                                  format(round(.data[[xmax]],rounding),rounding),")"
                                    )))
        } else {
          forest.temp <- ggplot(data=forest.data.simp, 
                                aes(y=CBSA.f,x=.data[[x]], xmin=.data[[xmin]], xmax=.data[[xmax]],color=.data[[color]],
                                    text =  paste0("<b>MSA:</b> ",CBSA.f,
                                                   '<br><b>',hover.lab,":</b> ", format(round(.data[[x]],rounding),rounding)," (",
                                                   format(round(.data[[xmin]],rounding),rounding),", ",
                                                   format(round(.data[[xmax]],rounding),rounding),")",
                                                   '<br><b>',color.lab,':</b> ',.data[[color]])))
        }
      }
    }
    
    if (input$forest_variable %in% c("an.heat","ar.heat")) {
      if (input$forest_type == "UHII") {
        forest.temp <- forest.temp + geom_col(position=dodge) + 
          geom_errorbarh(height=2,size=tck_size,color='black',position=dodge) + 
          theme_bw(base_size=10) + 
          geom_vline(xintercept=xline, color='black', linetype='dashed', alpha=.5) +
          theme(axis.title.y = element_blank()) +
          xlab(xlab)
      } else {
        if (input$forest_color == 'none') {
          forest.temp <- forest.temp + geom_col(fill='grey40') 
        } else {
          forest.temp <- forest.temp + geom_col()
        }
        forest.temp <- forest.temp +
          geom_errorbarh(height=2,size=tck_size,color='black') + theme_bw(base_size=10) + 
          geom_vline(xintercept=xline, color='black', linetype='dashed', alpha=.5) +
          theme(axis.title.y = element_blank()) +
          xlab(xlab)
      }
      
    } else {
      if (input$forest_type == "UHII") {
        forest.temp <- forest.temp + geom_point(position=dodge,size=2) + 
          geom_errorbarh(height=2,size=tck_size,position=dodge) + theme_bw(base_size=10) + 
          theme(axis.title.y = element_blank()) +
          xlab(xlab)
      } else {
        forest.temp <- forest.temp + geom_point(size=2) + 
          geom_errorbarh(height=2,size=tck_size) + theme_bw(base_size=10) + 
          theme(axis.title.y = element_blank()) +
          xlab(xlab)
      }
      if (input$forest_variable != "mht") {
        forest.temp <- forest.temp + geom_vline(xintercept=xline, color='black', linetype='dashed', alpha=.5)
      }
    }
    
    if (input$forest_color%in%c("avg.range","avg.temp") && input$forest_type == "Primary") {
      if (input$forest_variable %in% c("an.heat","ar.heat")) {
        forest.temp <- forest.temp + 
          scale_fill_gradientn(colours=alpha(pal,alpha.val),guide = 'colorbar',
                               oob = scales::squish,name=color.lab, limits = lims, breaks = brks)
      } else {
        forest.temp <- forest.temp + scale_color_gradientn(colours=pal,guide = 'colorbar',
                                                           oob = scales::squish,name=color.lab,
                                                           limits = lims, breaks = brks)
      }
      
      
    } else if ((input$forest_color%in%c("region","climate") && input$forest_type == "Primary" )|| input$forest_type == "UHII"){
      if (input$forest_variable %in% c("an.heat","ar.heat")) {
        forest.temp <-  forest.temp + scale_fill_manual(values = alpha(pal,alpha.val)) 
      } else {
        forest.temp <-  forest.temp + scale_colour_manual(values = pal) 
        
      }
      
    }
    
    forest.temp <- forest.temp + xlim(xlim)
    
    p1 <- suppressWarnings(ggplotly(forest.temp,tooltip="text",source="forest_plot") %>%
                             layout(hoverlabel=list(bgcolor="white",align='left'),legend = list(title = list(text = color.lab))))
    
    plot <- plotly_build(p1)
    
    if (input$forest_type == "UHII") {
      plot$x$data[[1]]$name <- "Low UHII"; plot$x$data[[2]]$name <- "High UHII"
    } 
    
    event_register(plot,'plotly_click') 
    
    if ((input$forest_color == "none" && input$forest_type == "UHII") ||
        input$forest_type == "Primary") {
      plot
    }
    
  })
  
  # Only generate forest/bar plot click data when on the "MSA-Specific Results" tab
  click_data <- reactive({
    if (input$tabs=="MSA-Specific Results") {
      event_data("plotly_click", source = "forest_plot")
    }
  })
  
  # Observe event to update curve and table to MSA selected in forest/bar plot
  observeEvent(click_data(),{  
    
    event_data <- click_data()
    
    if(!is.null(event_data$pointNumber)){
      
      if (input$forest_type == "Primary") {
        cbsa_temp <- cbsa_all[cbsa_all$var == input$forest_type,]
      } else {
        if (event_data$curveNumber %in% c(0,2)) { # Q1
          cbsa_temp <- cbsa_all[cbsa_all$var == "UHII-Q1",]
        } else { # Q4
          cbsa_temp <- cbsa_all[cbsa_all$var == "UHII-Q4",]
        }
      }
      updateSelectInput(session,"cbsa_choice",selected=cbsa_temp[event_data$pointNumber+1,]$CBSA.Title)
      
    }
  })
  
  # Code to render the leaflet map of MSA-specific results
  output$cbsa_map <- renderLeaflet({
    
    if (input$forest_type == "Primary") {
      cbsas_shape <- cbsas_shape_full[cbsas_shape_full$group == input$forest_type,]
      
      leaflet(cbsas_shape) %>% clearShapes() %>% clearControls() %>%
        addProviderTiles(providers$CartoDB.Positron, group="States") %>%
        fitBounds(-123.36119,25.13313,-70.02285,48.30850)
    } else {
      leaflet() %>% clearShapes() %>% clearControls() %>%
        addProviderTiles(providers$CartoDB.Positron, group="States") %>%
        fitBounds(-123.36119,25.13313,-70.02285,48.30850)
    }
    
  })
  
  # Observe event to update curve and table to MSA selected in map
  observeEvent(input$cbsa_map_shape_click, {
    
    click <- input$cbsa_map_shape_click
    
    if(!is.null(click$id)){
      updateSelectInput(session,"cbsa_choice",selected=trimws(click$id))
      
    }
  }) 
  
  # Observe event to update user selections based on the tab selected
  observeEvent (input$cbsa_tabs, {
    if(input$cbsa_tabs=="Interactive Map"){ 
      shinyjs::disable("forest_color")
      shinyjs::reset("forest_color")
      shinyjs::disable("sort")
      shinyjs::reset("sort")
    } else {
      if (input$forest_type == "Primary") {
        shinyjs::enable("forest_color")
        shinyjs::enable("sort")
      }
    }
  })
  
  # Observe event to allow/not allow sorting by MSA-level characteristic
  observeEvent (input$forest_color, {
    if(input$forest_color=="none"){ 
      shinyjs::disable("sort")
      shinyjs::reset("sort")
    } else {
      shinyjs::enable("sort")
    }
  })
  
  # Observe event to allow/not allow sorting by MSA-level characteristic
  observeEvent (input$forest_type, {
    if (input$cbsa_choice %in% unique(cbsa_all[cbsa_all$group == input$forest_type,]$CBSA.Title)) {
      updateSelectInput(session,"cbsa_choice",choices=unique(cbsa_all[cbsa_all$group == input$forest_type,]$CBSA.Title),
                        selected= input$cbsa_choice)
    } else {
      updateSelectInput(session,"cbsa_choice",choices=unique(cbsa_all[cbsa_all$group == input$forest_type,]$CBSA.Title),
                        selected= "Akron, OH")
    }
    
    if (input$forest_type == "Primary") {
      shinyjs::enable("forest_color")
      if(input$forest_color=="none"){ 
        shinyjs::disable("sort")
        shinyjs::reset("sort")
      } else {
        shinyjs::enable("sort")
      }      
    } else {
      shinyjs::disable("forest_color")
      shinyjs::disable("sort")
      shinyjs::reset("forest_color")
      shinyjs::reset("sort")
    }
  })
  
  # Code to render MSA-specific exposure-response curve
  output$cbsa_curve <- renderPlot({
    
    if (input$forest_type != "Primary") {
      par(mar=c(5.1+1, 4.1, 4.1, 2.1))
    } else {
      par(mar=c(5.1, 4.1, 4.1, 2.1))
    }
    
    if (input$cbsa_choice %in% unique(cbsa_all[cbsa_all$group == input$forest_type,]$CBSA.Title)) {
      idx = cbsa_all[cbsa_all$CBSA.Title==input$cbsa_choice & cbsa_all$group == input$forest_type,]$X
    } else {
      idx = cbsa_all[cbsa_all$CBSA.Title=="Akron, OH" & cbsa_all$group == input$forest_type,]$X
    }
    
    if (input$forest_type == "Primary") {
      preds <- list(CBSA.preds[[input$forest_type]][[idx]])
      predvars <- list(CBSA.tmeans[[input$forest_type]][[idx]])
    } else {
      if (length(idx) == 2) {
        preds <- list(CBSA.preds[["UHII-Q1"]][[idx[1]]],CBSA.preds[["UHII-Q4"]][[idx[2]]])
        predvars <- list(CBSA.tmeans[["UHII-Q1"]][[idx[1]]],CBSA.tmeans[["UHII-Q4"]][[idx[2]]])
        colors <- c("#66C0E4","#E34D34")
        ltypes <- c(2,6)
        curr_min=100;curr_max=0
      } else {
        preds <- list(CBSA.preds[["UHII-Q1"]][[idx]])
        predvars <- list(CBSA.tmeans[["UHII-Q1"]][[idx]])
        colors <- c("#66C0E4")
        ltypes <- c(2)
        curr_min=100;curr_max=0
        
      }
    }
    
    cens <- cbsa_all[cbsa_all$CBSA.Title==input$cbsa_choice & cbsa_all$group == input$forest_type,]$cen
    
    inds <- c(50,60,70,80,90,99,100)
    
    
    for (i in 1:length(preds)) {
      
      pred <- preds[[i]]
      predvar <- predvars[[i]]
      cen <- cens[i]
      
      
      indlab <- names(predvar) %in% paste0(inds,"%")
      
      limits <- c(round(min(predvar[indlab]),1)-.1,round(max(predvar[indlab]),1))
      
      
      if (input$forest_type == "Primary") {
        plot(pred,"overall",type="l",lwd=2.5,col='black',ylim=c(0.5,1.5),xlim=limits,axes=F,xlab="",
             ylab="RR", ci.arg=list(density=20+((i-1)*5),col=adjustcolor('black',alpha.f=0.25)))
        
        abline(v=cen,lty=2)
        axis(1,at=predvar[indlab],labels=inds)
        loc = par("usr")[1]-0.05*diff(par("usr")[1:2])
        mtext("%ile",1,line=1,at=loc,col="black",adj=1)
        min <- floor(min(predvar[indlab]))-1; max <- ceiling(max(predvar[indlab]))+1
        axis(1,at=seq(min,max,2),line=2.5,col="black",col.ticks="black",col.axis="black")
        axis(1,at=seq(min,max,1),labels=NA,line=2.5,col="black",col.ticks="black",col.axis="black",tck=-0.009)
        
        mtext(expression(paste(degree, "C")),1,line=3.5,at=loc,col="black",adj=1)
        axis(2)
      } else {
        
        plot(pred,"overall",type="l",lwd=2.5,col=colors[i],lty=ltypes[i],ylim=c(0.5,1.5),xlim=limits,axes=F,xlab="",
             ylab="RR", ci.arg=list(density=20+((i-1)*5),lty=ltypes[i],col=adjustcolor(colors[i],alpha.f=0.25)))
        abline(v=cen,lty=2,col=colors[i])
        
        if (i != 1) {axis(1,at=predvar[indlab],labels=NA,col=NA,col.ticks=colors[i],tck=-0.03); tckcol = NA} else { tckcol = colors[i]}
        axis(1,at=predvar[indlab],labels=inds,col=NA,col.ticks=tckcol,
             col.axis=colors[i],cex.axis=0.95,line=((i-1)/1.5))
        
        if (min(predvar[indlab]) < curr_min) {curr_min<-min(predvar[indlab])}
        if (max(predvar[indlab]) > curr_max) {curr_max<-max(predvar[indlab])}
        
        if (i == length(preds)) {
          loc = par("usr")[1]-0.05*diff(par("usr")[1:2])
          
          axis(1,at=c(curr_min,curr_max),labels=NA,col="black",col.ticks=NA,col.axis=NA)
          mtext("%ile",1,line=1,at=loc,col="black",adj=1)
          
          extra <- (1-length(preds))*(2/3)
          
          axis(1,at=seq(floor(curr_min),ceiling(curr_max),2),line=2.5-extra,col="black",col.ticks="black",col.axis="black")
          axis(1,at=seq(floor(curr_min),ceiling(curr_max),1),labels=NA,line=2.5-extra,col="black",col.ticks="black",col.axis="black",tck=-0.009)
          
          mtext(expression(paste(degree, "C")),1,line=3.5-extra,at=loc,col="black",adj=1)
          
          axis(2)
          
          if (length(preds) == 2) { leg = c("Low UHII","High UHII")} else { leg = c("Low UHII")}
          
          legend(x=par("usr")[1]+0.25,y=par("usr")[4], legend=leg,
                 col=colors, lty=ltypes,lwd=1.5,cex=1,box.lty=0)
          
          par(new = F)
        } else {
          par(new=T)
        }
        
      }
    }
    
    
  })
  
  # Informational text for user selection
  output$cbsa_info_text <- renderText({
    paste("Select the variable and stratification to display. For overall results, you may also choose to color and sort the variable by MSA-level characteristics:<br><br>")
  })
  
  # Download button for all MSA-level results
  output$downloadMSAData <- downloadHandler(
    filename <- function() {
      paste("data/MSA-Level-Results.csv")
    },
    
    content <- function(file) {
      file.copy("data/MSA-Level-Results.csv", file)
    },
    contentType = "text/csv"
  )
  
  # Code to render table of MSA-specific results
  output$cbsa_table <- DT::renderDataTable({
    
    if (input$cbsa_choice %in% unique(cbsa_all[cbsa_all$group == input$forest_type,]$CBSA.Title)) {
      table <- cbsa_all[cbsa_all$CBSA.Title==input$cbsa_choice & cbsa_all$group == input$forest_type,]
    } else {
      table <- cbsa_all[cbsa_all$CBSA.Title=="Akron, OH" & cbsa_all$group == input$forest_type,]
    }
    
    table$per_x_heat <- paste0(round((table$x_heat_an/table$heat_an)*100,1),"")
    table$RR.99 <- paste0(format(round(table$RR.99,3),3), " (",format(round(table$RR.99.low,3),3),", ",format(round(table$RR.99.high,3),3),")")
    table$heat_an <- paste0(format(round(table$heat_an,0),big.mark=',',trim=T), " (",
                            format(round(table$heat_an_lower,0),big.mark=',',trim=T),", ",
                            format(round(table$heat_an_upper,0),big.mark=',',trim=T),")")
    table$heat_ar <- paste0(format(round(table$ann_an_rate100k,2),big.mark=',',trim=T), " (",
                            format(round(table$ann_an_rate100k_lower,2),big.mark=',',trim=T),", ",
                            format(round(table$ann_an_rate100k_upper,2),big.mark=',',trim=T),")")
    table$temp.99 <- round(table$temp.99,1)
    
    table$mht <- paste0(format(round(table$cen,1),big.mark=',',trim=T), " (",
                        format(round(table$cen.low,1),big.mark=',',trim=T),", ",
                        format(round(table$cen.high,1),big.mark=',',trim=T),")")
    
    table$mhp <- paste0(format(round(table$cen.per,1),big.mark=',',trim=T), " (",
                        format(round(table$cen.per.low,1),big.mark=',',trim=T),", ",
                        format(round(table$cen.per.high,1),big.mark=',',trim=T),")")
    
    if (input$forest_type == "Primary") {
      all_hosp <- sum(cbsa_all[cbsa_all$var == input$forest_type,]$tot_hosp)
      all_heat_hosp <- sum(cbsa_all[cbsa_all$var == input$forest_type,]$tot_heat_hosp)
      table$tot_hosp_per <-round((table$tot_hosp/all_hosp)*100,1)
      table$tot_heat_hosp_per <- round((table$tot_heat_hosp/all_heat_hosp)*100,1)
    } else {
      all_hosp1 <- sum(cbsa_all[cbsa_all$var == "UHII-Q1",]$tot_hosp)
      all_heat_hosp1 <- sum(cbsa_all[cbsa_all$var == "UHII-Q1",]$tot_heat_hosp)
      all_hosp4 <- sum(cbsa_all[cbsa_all$var == "UHII-Q4",]$tot_hosp)
      all_heat_hosp4 <- sum(cbsa_all[cbsa_all$var == "UHII-Q4",]$tot_heat_hosp)
      table$tot_hosp_per <- c(round(( table[table$var == "UHII-Q1",]$tot_hosp/all_hosp1)*100,1), 
                              round(( table[table$var == "UHII-Q4",]$tot_hosp/all_hosp4)*100,1))
      table$tot_heat_hosp_per <- c(round(( table[table$var == "UHII-Q1",]$tot_heat_hosp/all_heat_hosp1)*100,1),
                                   round(( table[table$var == "UHII-Q4",]$tot_heat_hosp/all_heat_hosp4)*100,1))
      
    }
    
    table$tot_hosp <- format(table$tot_hosp,big.mark=',',trim=T)
    table$tot_heat_hosp <- format(table$tot_heat_hosp,big.mark=',',trim=T)
    table$tot_hosp_per_lab <- paste0(table$tot_hosp, " (",table$tot_hosp_per,"%)")
    table$tot_heat_hosp_per_lab <- paste0(table$tot_heat_hosp, " (",table$tot_heat_hosp_per,"%)")
    
    table <- data.frame(label=colnames(table),data=t(table))
    
    table <- table[c("n.zips","tot_hosp_per_lab","temp.99",
                     "mhp","mht","RR.99","heat_an","heat_ar"),]
    
    rownames(table) <- NULL
    
    table$label <- c("<b># ZIP Codes</b>","<b># Hospitalizations (% Total)</b>","<b>99th Temp. %ile (\u00b0C)</b>",
                     "<b>MHP (95% CI)</b>","<b>MHT (\u00b0C) (95% CI)</b>","<b>RR (99th vs. MHP) (95% CI)</b>",
                     "<b>AN (Temp. \u2265 MHP) (95% CI)</b>","<b> AR (Annual, per 100k) (95% CI)</b>")
    
    if (input$forest_type=="Primary") {
      caption = paste0('Exposure and outcome information and heat-related cardiovascular risk and burden results for ',input$cbsa_choice,', 2000-2017.')
      colnames = c("Overall")            
    } else {
      caption = paste0('Exposure and outcome information and heat-related cardiovascular risk and burden results for ',input$cbsa_choice,", in low and high urban heat island intensity (UHII) areas, 2000-2017.")
      if (ncol(table) == 3) {
        colnames = c("Low UHII","High UHII")
      } else {
        colnames = c("Low UHII")
      }
    }
    
    caption <- paste0(caption," ", input$cbsa_choice, " is located in the ", 
                      unique(cbsa_all[cbsa_all$CBSA.Title==input$cbsa_choice & cbsa_all$group == input$forest_type,]$Region),
                      " and has a climate type of: ", 
                      unique(cbsa_all[cbsa_all$CBSA.Title==input$cbsa_choice & cbsa_all$group == input$forest_type,]$Koppen.Description))
    
    caption <- paste0(caption, ". MSA = Metropolitan Statistical Area; MHP = Minimum Hospitalization Percentile; MHT = Minimum Hospitalization Temperature; RR = Relative Risk; AN = Heat-Attributable Number; AR = Heat-Attributable Rate.")
    DT::datatable(table, 
                  options = list(paging = FALSE,searching = FALSE,dom='t',ordering=F),
                  
                  selection = 'none',rownames=FALSE,escape=FALSE,class = 'cell-border stripe',
                  caption = htmltools::tags$caption(
                    style = 'caption-side: bottom; text-align: left;',
                    caption
                  ),
                  colnames = colnames) 
  })
  
  # Text to identify which MSA is currently selected
  output$cbsa_title <- renderText({
    paste0("Results for <b>",input$cbsa_choice,"</b>:")
  })
  
  # Bulleted list of key takeaways
  output$msa_takeaway <- renderText({
    "<ul>
  <li>Considerable variation in the MSA-level risk and burden, overall and by UHII level</li>
  <li>No clear geographic, regional, or climactic trend for which MSAs had the highest heat-related risk and burden</li>
  <li>Overall: 66% of MSAs had a RR (99th vs. MHP) > 1 </li>
  <li>Low UHII: 60% of MSAs had a RR (99th vs. MHP) > 1 </li>
  <li>High UHII: 78% of MSAs had a RR (99th vs. MHP) > 1 </li>
  </ul>"
  })
  
  
  #############################################################################
  #### Output for About Tab ###################################################
  #############################################################################  
  
  output$about_text <- renderText({
    paste0(
      "This dashboard is associated with the manuscript <b><i>Urban Heat Island Impacts on Heat-Related Cardiovascular Morbidity: A Time Series Analysis of Older Adults in US Metropolitan Areas</i></b>.",
      " It allows for interaction with the manuscript's primary results - the heat-related cardiovascular risk and burden, in areas experiencing urban heat islands (UHIs) compared to those not experiencing UHIs, in 120 contiguous US metropolitan statistical areas (MSAs), 2000-2017. The results are available for the entire study population, different subpopulations (by age, sex, race, and chronic condition status), and in low and high UHI intensity (UHII) areas ('Overall & Subpopulation Results' tab).",
      " It also allows for interaction with the MSA-level risk and burden, overall and in low and high UHII areas ('MSA-Specific Results' tab).", 
      " All results can be downloaded on their respective tabs.",
      " Additionally, users can explore the ZIP code-level temperature, UHII, and Medicare hospitalization data used in the analyses ('Exposure & Outcome Maps').",
      "<br><br>",
      "Details on the datasets and methods used as well as a discussion of all results and sensitivity analyses can be found in the associated manuscript.",
      "<br><br>",
      "For any questions, please email: <a href='mailto:rappold.ana@epa.gov'>rappold.ana@epa.gov</a>.",
      "<br><br>",
      "<b>Abstract</b>",
      "<br>Many United States (US) cities are experiencing urban heat islands (UHIs) and climate change-driven temperature increases. Extreme heat increases cardiovascular disease (CVD) risk, yet little is known about how this association varies with UHI intensity (UHII) within and between cities. We aimed to identify the urban populations most at-risk of and burdened by heat-related CVD morbidity in UHI-affected areas compared to unaffected areas. ZIP code-level daily counts of CVD hospitalizations among Medicare enrollees, aged 65-114, were obtained for 120 US metropolitan statistical areas (MSAs) between 2000-2017. Mean ambient temperature exposure was estimated by interpolating daily weather station observations. ZIP codes were classified as low and high UHII using the first and fourth quartiles of an existing surface UHII metric, weighted to each have 25% of all CVD hospitalizations. MSA-specific associations between ambient temperature and CVD hospitalization were estimated using quasi-Poisson regression with distributed lag non-linear models and pooled via multivariate meta-analyses. Across the US, extreme heat (MSA-specific 99th percentile, on average 28.6 \u00b0C) increased the risk of CVD hospitalization by 1.5% (95% CI: 0.4%, 2.6%), with considerable variation among MSAs. Extreme heat-related CVD hospitalization risk in high UHII areas (2.4% [95% CI: 0.4%, 4.3%]) exceeded that in low UHII areas (1.0% [95% CI: -0.8%, 2.8%]), with upwards of a 10% difference in some MSAs. During the 18-year study period, there were an estimated 37,028 (95% CI: 35,741, 37,988) heat-attributable CVD admissions. High UHII areas accounted for 35% of the total heat-related CVD burden, while low UHII areas accounted for 4%. High UHII disproportionately impacted already heat-vulnerable populations; females, individuals aged 75-114, and those with chronic conditions living in high UHII areas experienced the largest heat-related CVD impacts. Overall, extreme heat increased cardiovascular morbidity risk and burden in older urban populations, with UHIs exacerbating these impacts among those with existing vulnerabilities.",
      "<br><br>",
      "<b>Authors:</b> Stephanie E. Cleland, William Steinhardt, Lucas M. Neas, J. Jason West, Ana G. Rappold"
    )  
  })
  
}

shinyApp(ui, server)