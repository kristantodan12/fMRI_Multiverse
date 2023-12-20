library(shiny)
library(networkD3)
library(dplyr)
library(igraph)
library(visNetwork)
library(geomnet)
library(stringr)
library(png)
library(shinyjs)
library(DT)
library(rintrojs)
library(ggplot2)
library(qdapTools)
library(RColorBrewer)
library(shinyWidgets)

shinyWidgets::shinyWidgetsGallery()

source("helper_function.R")

ui <- shinyUI(navbarPage(title = div(img(src="metarep.jpg", height = "50px"), img(src="UOL.jpg", height = "50px") ), id = "navBar",
                         theme = "bootstrap.css",
                         collapsible = TRUE,
                         inverse = TRUE,
                         windowTitle = "Forking Path of fMRI and mobile EEG analyses",
                         position = "fixed-top",
                         header = tags$style(
                           ".navbar-right {
                       float: right !important;
                       }",
                           "body {padding-top: 150px;}"),
                         
                         tabPanel("Introduction", value = "home",
                                  
                                  shinyjs::useShinyjs(),
                                  
                                  tags$head(tags$script(HTML('
                                    var fakeClick = function(tabName) {
                                    var dropdownList = document.getElementsByTagName("a");
                                    for (var i = 0; i < dropdownList.length; i++) {
                                    var link = dropdownList[i];
                                    if(link.getAttribute("data-value") == tabName) {
                                    link.click();
                                    };
                                    }
                                    };
                                    '))),
                                  
                                  fluidRow(
                                    column(12,
                                           align = "center",
                                           shiny::tags$h1("METEOR", style = "color: #333;"),
                                           shiny::tags$h2("Mastering The Oppressive number of forking paths unfolded by noisy and complex neural data",
                                                          style = "color: #555; font-weight: normal;"),
                                           shiny::tags$h3("This interactive shiny app allows you to investigate the multiverse of functional Magnetic Resonance Imaging (fMRI) data pre-processing and analysis based on graph theory. You can explore the pre-processing choices of published articles. You can also construct your own preprocessing pipeline and compare it to the ones in the literature.",
                                                          style = "color: #777; font-weight: normal;"),
                                           shiny::HTML("<br>"),
                                           shiny::tags$h3("Database:", style = "color: #333;"),
                                           shiny::tags$p("The multiverse has been identified by a literature review. All information on the literature review, the included articles, and the coded pre-processing steps and their respective options can be found here."),
                                           shiny::HTML("<br>"),
                                           shiny::tags$h3("Steps:", style = "color: #333;"),
                                           shiny::tags$p("Explore which pre-processing steps have been used and which combinations and orders are common."),
                                           shiny::HTML("<br>"),
                                           shiny::tags$h3("Steps: Options:", style = "color: #333;"),
                                           shiny::tags$p("Explore which options for the respective pre-processing steps have been used by which articles."),
                                           shiny::HTML("<br>"),
                                           shiny::tags$h3("Individual Article:", style = "color: #333;"),
                                           shiny::tags$p("Check out pre-processing pipelines and their chosen options for individual article."),
                                           shiny::HTML("<br>"),
                                           shiny::tags$h3("Your Own Pipeline:", style = "color: #333;"),
                                           shiny::tags$p("Construct your own pipeline and compare it to the ones in the literature."),
                                           shiny::HTML("<br><br>")
                                    )
                                  ),
                                  
                                  fluidRow(shiny::HTML("<br><br><center> <h1>Ready to Get Started?</h1> </center> <br>")),
                                  
                                  fluidRow(
                                    column(4),
                                    column(4,
                                           tags$div(
                                             align = "center",
                                             tags$a("START",
                                                    onclick = "fakeClick('MA')",
                                                    class = "btn btn-primary btn-lg"
                                             )
                                           )
                                    ),
                                    column(4)
                                  ),
                                  
                                  fluidRow(style = "height:25px;")
                                  
                         ), # Closes the first tabPanel called "Home"
                         
                         
                         # tabPanel("Background", value = "background",
                         #          
                         #          #shinyjs::useShinyjs(),
                         #          
                         #          fluidRow(
                         #            column(3),
                         #            column(5,
                         #                   shiny::HTML("Summary of introduction of the paper")
                         #            ),
                         #            
                         #          ),
                         #          
                         # ), # Closes tab
                         
                         tabPanel("Database", value = "MA",
                                  mainPanel(
                                    tabsetPanel(type = "tabs",
                                    tabPanel(
                                      "PRISMA Diagram",
                                      shiny::HTML("<h1>PRISMA diagram</h1>"),
                                      fluidRow(
                                        column(4, 
                                               shiny::HTML("<h5><b>Literature review of articles related to graph fMRI studies.</b></h5> 
                                                  <b>Literature search:</b>Three databases were searched: Scopus, Web of Science, 
                                                  and PubMed for publications containing the terms related to fMRI, graph theory, and cognitive abilities <br><br> 
                                                  <b>Study inclusion:</b> We only included empirical studies which report
                                                  the preprocessing steps and deal with healthy subjects. Details can be found in the Preferred Reporting 
                                                  Items for Systematic Reviews and Meta-Analyses (PRISMA) flowchart on the right. <br><br>
                                                  <b>Data extraction:</b> All data was coded by one coder. Other two coders coded the data
                                                  independently for 25 articles each. The codes were then compared and the discrepancies were discussed.
                                                  Information on the pre-processing steps and 
                                                  their respective options were extracted. <br><br></h5>"),
                                        ),
                                        column(8, 
                                               shiny::HTML("<h3>Defining the space from general fMRI articles</h3>"),
                                               img(src='prisma1.jpg', align = "center", width = "600px", height = "700px"),
                                               shiny::HTML("<h3>Defining the forking paths of graph fMRI studies</h3>"),
                                               img(src='prisma2.jpg', align = "center", width = "600px", height = "850px")
                                        ),
                                        # column(4, # Display the second image and caption in a 4-column layout
                                        #        shiny::HTML("<h3>Defining the forking paths of graph fMRI studies</h3>"),
                                        #        img(src='prisma2.jpg', align = "center", width = "600px", height = "900px")
                                        # )
                                      )
                                    ),
                                    tabPanel(
                                      "List of Included Articles",
                                      shiny::HTML("<h5><b>List of included articles for graph fMRI studies</b></h5>"),
                                      DT::dataTableOutput("list_paper")
                                    ),
                                    tabPanel(
                                      "List of Steps",
                                      shiny::HTML("<h5><b>List of identified steps</b></h5>"),
                                      DT::dataTableOutput("list_steps")
                                    ),
                                    tabPanel(
                                      "List of Options",
                                      shiny::HTML("<h5><b>List of identified options</b></h5>"),
                                      DT::dataTableOutput("list_decisions")
                                    )
                                    )
                                  )  # Closes the mainPanel
                         ),  # Closes the second tabPanel called "Literature-based Analysis"
                         
                         
                         
                         tabPanel("Steps", value = "WH",
                                    tabsetPanel(type = "tabs",
                                                tabPanel(width = 12,
                                                  "Aggregated Steps",
                                                  sidebarLayout( 
                                                    
                                                    sidebarPanel( width = 3,
                                                                  shiny::HTML("<h5><b>Explore the aggregated preprocessing 
                                                                       pipelines of all articles in a network fashion.</b><br><br>
                                                                       Each node (circle) represents a pre-processing step. The 
                                                                       color of a node indicates the processing group the step 
                                                                       belongs to. <br><br>
                                                                       Steps performed in succession are connected by arrows, 
                                                                       these are edges. The wider the arrow, the higher the number
                                                                       of articles using this edge. The arrow points in the direction 
                                                                       of the step that is performed afterward. <br><br>
                                                                       By hovering over a node you can get its name. If you click 
                                                                       on it you get its definition and by how many articles it was 
                                                                       used. By hovering over an edge you can get how many
                                                                       articles used it. <br><br>
                                                                       To identify edges of a specific preprocessing step, please 
                                                                       select it in the dropdown below. If you choose all, you will 
                                                                       see edges of all preprocessing steps.<br><br>"
                                                                  ),
                                                                  selectInput("Node_WP",
                                                                              label   = "Explore edges of step:",
                                                                              choices =  list('All' = list('All'),
                                                                                              'Software',
                                                                                              'Structural Preprocessing' = (c(nodes$Names_vis[nodes$Groups=='Structural_preprocessing'])),
                                                                                              'Functional Preprocessing' = (c(nodes$Names_vis[nodes$Groups=='Functional_preprocessing'])),
                                                                                              'Noise Removal' = (c(nodes$Names_vis[nodes$Groups=='Noise_removal'])),
                                                                                              'FC Definition' = (c(nodes$Names_vis[nodes$Groups=='FC_def'])),
                                                                                              'Graph Analysis' = (c(nodes$Names_vis[nodes$Groups=='Graph_analysis']))),
                                                                              selected = "All"
                                                                  ),
                                                                  sliderInput("Thr", "Threshold article",
                                                                              min = 0, max = 99,
                                                                              value = 0
                                                                  ),
                                                                  shiny::HTML("<h5>Move the threshold to only see edges used by more artciles than the threshold.</h5>"),
                                                    ),  # Closes sidebarPanel
                                                    mainPanel( width = 9,
                                                               forceNetworkOutput(outputId = "WP", width = "100%", height = "700px")
                                                    )  # Closes the mainPanel
                                                  )  # Closes the sidebarLayout
                                                ),
                                                tabPanel(width = 12,
                                                         "Individual Step",
                                                         sidebarLayout( 
                                                           
                                                           sidebarPanel( width = 3,
                                                                         shiny::HTML("<h5><b>Explore the individual step.</b><br><br>
                                                                       Please select a specific step from drop-down menu. The list
                                                                                     of articles used the selected steps will be available
                                                                                     on the table in the main panel. <br><br>"
                                                                         ),
                                                                         selectInput("select_IS",
                                                                                     label   = "Select the step:",
                                                                                     choices =  list('Software',
                                                                                                     'Structural Preprocessing' = (c(nodes$Names_vis[nodes$Groups=='Structural_preprocessing'])),
                                                                                                     'Functional Preprocessing' = (c(nodes$Names_vis[nodes$Groups=='Functional_preprocessing'])),
                                                                                                     'Noise Removal' = (c(nodes$Names_vis[nodes$Groups=='Noise_removal'])),
                                                                                                     'FC Definition' = (c(nodes$Names_vis[nodes$Groups=='FC_def'])),
                                                                                                     'Graph Analysis' = (c(nodes$Names_vis[nodes$Groups=='Graph_analysis']))),
                                                                                     selected = "All"
                                                                         ),

                                                           ),  # Closes sidebarPanel
                                                           mainPanel( 
                                                             fluidRow(
                                                             column(12, textOutput("selected_IS")),
                                                             column(12, DT::dataTableOutput("table_IS"))
                                                           ),
                                                           )  # Closes the mainPanel
                                                         )  # Closes the sidebarLayout
                                                ),
                                                tabPanel(
                                                  "Combination",
                                                  sidebarPanel( width = 3,
                                                                shiny::HTML("<h5><b>Explore pairs of pre-processing steps used in 
                                                                combination. </b><br><br>
                                                                Select a step from the dropdown below to see which steps were are 
                                                                used in conjunction with this step. <br><br></h5>"),
                                                                selectInput("selectDecisionYN",
                                                                            label   = "Explore combinations of step:",
                                                                            choices =  list('Software',
                                                                                            'Structural Preprocessing' = (c(nodes$Names_vis[nodes$Groups=='Structural_preprocessing'])),
                                                                                            'Functional Preprocessing' = (c(nodes$Names_vis[nodes$Groups=='Functional_preprocessing'])),
                                                                                            'Noise Removal' = (c(nodes$Names_vis[nodes$Groups=='Noise_removal'])),
                                                                                            'FC Definition' = (c(nodes$Names_vis[nodes$Groups=='FC_def'])),
                                                                                            'Graph Analysis' = (c(nodes$Names_vis[nodes$Groups=='Graph_analysis']))),
                                                                            selected = "Software"
                                                                ),
                                                                shiny::HTML("<h5>Lollipop plot of the number of pre-processing steps use together with 
                                                              the step selected above. Selected step in red font. Color 
                                                                          indicates the pre-processing group.</h5>"),
                                                  ),
                                                  mainPanel(
                                                    fluidRow(
                                                      column(3),
                                                      column(6, plotOutput("plot_YN", height = 600, width = "100%")),
                                                      column(3)
                                                    )
                                                  )
                                               
                                                ),
                                                tabPanel(
                                                  "Order",
                                                  sidebarPanel( width = 3,
                                                                shiny::HTML("<h5><b>Investigate the order of pre-processing steps.</b><br><br>
                                                                Select a preprocessing step from the dropdown below to see which 
                                                                preprocessing steps were performed AFTER the selected one.<br><br></h5>"),
                                                                selectInput("selectDecisionOR",
                                                                            label   = "Explore steps performed after step:",
                                                                            choices =  list('Software',
                                                                                            'Structural Preprocessing' = (c(nodes$Names_vis[nodes$Groups=='Structural_preprocessing'])),
                                                                                            'Functional Preprocessing' = (c(nodes$Names_vis[nodes$Groups=='Functional_preprocessing'])),
                                                                                            'Noise Removal' = (c(nodes$Names_vis[nodes$Groups=='Noise_removal'])),
                                                                                            'FC Definition' = (c(nodes$Names_vis[nodes$Groups=='FC_def'])),
                                                                                            'Graph Analysis' = (c(nodes$Names_vis[nodes$Groups=='Graph_analysis']))),
                                                                            selected = "Software"
                                                                            
                                                                ),
                                                                shiny::HTML("<h5>Lollipop plot of the number of processing steps 
                                                                          used after the selected step. Selected step in red font. 
                                                                          Color indicates the preprocessing group.</h5>"),
                                                  ),
                                                  mainPanel(
                                                    fluidRow(
                                                      column(3),
                                                      column(6, plotOutput("plot_OR", height = 600, width = "100%")),
                                                      column(3)
                                                    )
                                                  )
                                                )
                                    )
                                  
                         ),  # Closes the second tabPanel called "Literature-based Analysis"
                         
                        
                         tabPanel("Steps: Options", value = "fa",
                                  sidebarPanel( width = 3,
                                                shiny::HTML("<h5><b>Explore the distribution of options chosen by
                                                                     different articles.</b><br></h5>"),
                                                selectInput("selectGroup",
                                                            label   = "Explore the chosen options of step:",
                                                            choices =  c(unique(nodes_op$Groups_vis)),
                                                            selected = "Software"
                                                ),
                                                shiny::HTML("<h5>Lollipop plot of the number of articles using
                                                         various options of the selected step.</h5>"),
                                                uiOutput("selectDecision"),
                                  ),
                                  mainPanel( 
                                    fluidRow(
                                      column(5, plotOutput("plot_group_decision", height = 600, width = "100%")),
                                      column(7, textOutput("selected_decision")),
                                      column(7, DT::dataTableOutput("table"))
                                    ),
                                  ),
                                             
                         ),
                                                  

                         tabPanel("Individual Article", value = "IP",
                                    tabsetPanel(type = "tabs",
                                                tabPanel(
                                                  "Step Visualization",sidebarLayout( 
                                                    
                                                    sidebarPanel( width = 3,
                                                                  shiny::HTML("<h5><b>Visualize the pre-processing steps taken by a 
                                                                  specific article.</b><br><br>
                                                                  Select the key of the article in the dropdown below to get 
                                                                  a visualization of the pre-processing steps and to generate a 
                                                                  table of the pre-procesing steps. You can 
                                                                  find the key of each study in the Database tab.<br></h5>"),
                                                                  selectInput("selectPapers",
                                                                              label   = "Select article:",
                                                                              choices =  c(dat$Key),
                                                                              selected = "1"
                                                                  ),
                                                                  
                                                    ),  # Closes sidebarPanel
                                                    mainPanel( width = 8,
                                                               textOutput("selected_paper"),
                                                               DT::dataTableOutput("table_step"),
                                                               plotOutput("plot", width = "100%")
                                                    )  # Closes the mainPanel
                                                  )
                                                ),
                                                tabPanel(
                                                  "Option Visualization",
                                                  sidebarLayout( 
                                                    
                                                    sidebarPanel( width = 3,
                                                                  shiny::HTML("<h5><b>Visualize the options of pre-processing steps taken by a 
                                                                  specific article.</b><br><br>
                                                                  Select the code of the article in the dropdown below to get 
                                                                  a visualization of the options of the pre-processing steps and to generate a 
                                                                  table of the options. You can 
                                                                  find the code of each study in the Database tab.<br></h5>"),
                                                                  selectInput("selectPapers_cv",
                                                                              label   = "Select article:",
                                                                              choices =  c(dat_op$Key),
                                                                              selected = "1"
                                                                  ),
                                                                  
                                                    ),  # Closes sidebarPanel
                                                    mainPanel( width = 8,
                                                               textOutput("selected_paper_cv"),
                                                               DT::dataTableOutput("table_option"),
                                                               plotOutput("plot_cv", width = "100%")
                                                    )  # Closes the mainPanel
                                                  )  # Closes the sidebarLayout
                                                )
                                    )
                         ),  # Closes the second
                         
                         
                         
                         tabPanel("Your Own Pipeline", value = "DIY",
                                  sidebarLayout(

                                    sidebarPanel( width = 3,
                                                  shiny::HTML("<h5><b>Construct your preferred pipeline for fMRI data 
                                                  pre-processing</b><br><br>
                                                  Select the step you want to include with the dropdown below. You may also 
                                                  select a specific option or any of the available ones. You can add and 
                                                  delete steps with the respective buttons. <br><br>
                                                  You can count the number of studies that have used the same pipeline and 
                                                  obtain a table by clicking count. You may specify whether the order of 
                                                  their pipeline should be considered. If this option is disabled, the a
                                                  lgorithm will identify all articles that have employed the selected steps 
                                                  regardless of the order. <br></h5>"),
                                                  selectInput("selectStep_DIY",
                                                              label   = "Select the step you want to include",
                                                              choices =  list('Software',
                                                                              'Structural Preprocessing' = (c(nodes$Names_vis[nodes$Groups=='Structural_preprocessing'])),
                                                                              'Functional Preprocessing' = (c(nodes$Names_vis[nodes$Groups=='Functional_preprocessing'])),
                                                                              'Noise Removal' = (c(nodes$Names_vis[nodes$Groups=='Noise_removal'])),
                                                                              'FC Definition' = (c(nodes$Names_vis[nodes$Groups=='FC_def'])),
                                                                              'Graph Analysis' = (c(nodes$Names_vis[nodes$Groups=='Graph_analysis']))),
                                                              selected = "Software"
                                                  ),
                                                  uiOutput("selectDecision_DIY"),
                                                  actionButton("add", 
                                                               label = "Add", 
                                                               icon = icon("arrow-circle-right", class = "fa-2x"),
                                                               width= "100px", height= "40px"
                                                  ),
                                                  actionButton("delete", 
                                                               label = "Delete", 
                                                               icon = icon("arrow-circle-left", class = "fa-2x"),
                                                               width= "100px", height= "40px"
                                                  ),
                                                  shiny::HTML("<h6>Click this Order button if you want to take the order 
                                                  of the steps into consideration.</h6>"),
                                                  materialSwitch(inputId = "order", 
                                                                 label = "order", 
                                                                 status = "success",
                                                                 right = T,
                                                                 value = T
                                                  ),
                                                  actionButton("count", 
                                                               label = "Count", 
                                                               icon = icon("arrow-circle-right", class = "fa-2x"),
                                                               width= "100px", height= "40px"
                                                  ),
                                                  downloadButton('download',"Download the table",
                                                                 width= "100px", height= "40px"
                                                  ),
                                                  shiny::HTML("<h5>Information here</h5>"),
                                                  
                                    ),  # Closes sidebarPanel
                                    mainPanel( 
                                      DT::DTOutput("table_DIY"),
                                      fluidRow(style = "height:100px;"),
                                      fluidRow(
                                        column(12, textOutput("counted_paper"))
                                        
                                      ),
                                      DT::DTOutput("table_DIY2"),
                                    )  # Closes the mainPanel
                                  )  # Closes the sidebarLayout
                         ),
                         
                         tabPanel("About", value = "about",
                                  
                                  column(1),
                                  column(10,
                                         shiny::HTML("<h3>The METEOR project is based at the University of Oldenburg funded 
                                         by priority program <a href='https://www.meta-rep.uni-muenchen.de'> META-REP</a> 
                                         (SPP 2317). Meta-REP involves 15 individual projects with 50+ scholars analyzing 
                                                     and optimizing replicability in the Behavioral, Social, and Cognitive Sciences.</h3><br>
                                                     <h2><center>Our team</center></h2><br>")
                                  ),
                                  
                                  # TEAM BIO
                                  fluidRow(
                                    
                                    style = "height:50px;"),
                                  
                                  fluidRow(
                                    column(2),
                                    
                                    # Andrea
                                    column(2,
                                           div(class="panel panel-default",
                                               div(class="panel-body",  width = "600px",
                                                   align = "center",
                                                   div(
                                                     tags$img(src = "andrea2.jpg",
                                                              width = "45px", height = "57px")
                                                   ),
                                                   div(
                                                     tags$h5("Hildebrandt, Andrea, Prof. Dr. rer. nat."),
                                                     tags$h6( tags$i("Project Investigator"))
                                                   ),
                                                   div(
                                                     "Professor for Psychological Methods and Statistics, Carl von Ossietzky Universitat Oldenburg."
                                                   )
                                               )
                                           )
                                    ),
                                    # Stefan
                                    column(2,
                                           div(class="panel panel-default",
                                               div(class="panel-body",  width = "600px",
                                                   align = "center",
                                                   div(
                                                     tags$img(src = "stefan.jpg",
                                                              width = "50px", height = "70px")
                                                   ),
                                                   div(
                                                     tags$h5("Debener, Stefan, Prof. Dr. rer. nat."),
                                                     tags$h6( tags$i("Project Investigator"))
                                                   ),
                                                   div(
                                                     "Professor for Neuropsychology, Carl von Ossietzky Universitat Oldenburg."
                                                   )
                                               )
                                           )
                                    ),
                                    # Carsten
                                    column(2,
                                           div(class="panel panel-default",
                                               div(class="panel-body",  width = "600px",
                                                   align = "center",
                                                   div(
                                                     tags$img(src = "carsten.jpg",
                                                              width = "50px", height = "50px")),
                                                   div(
                                                     tags$h5("Giessing, Carsten, Dr. rer. nat."),
                                                     tags$h6( tags$i("Project Investigator"))
                                                   ),
                                                   div(
                                                     "Senior Scientist in Biological Psychology, Carl von Ossietzky Universitat Oldenburg."
                                                   )
                                               )
                                           )
                                    ),
                                    
                                    # Christiane
                                    column(2,
                                           div(class="panel panel-default",
                                               div(class="panel-body",  width = "600px",
                                                   align = "center",
                                                   div(
                                                     tags$img(src = "christiane2.jpg",
                                                              width = "58px", height = "60px")),
                                                   div(
                                                     tags$h5("Thiel, Christiane, Prof. Dr. rer. nat."),
                                                     tags$h6( tags$i("Project Investigator"))
                                                   ),
                                                   div(
                                                     "Professor for Biological Psychology, Carl von Ossietzky Universitat Oldenburg."
                                                   )
                                               )
                                           )
                                    ),
                                    column(2)
                                    
                                  ),
                                  
                                  fluidRow(
                                    column(3),
                                    # Nadine
                                    column(2,
                                           div(class="panel panel-default",
                                               div(class="panel-body",  width = "600px",
                                                   align = "center",
                                                   div(
                                                     tags$img(src = "nadine.jpg",
                                                              width = "40px", height = "55px")),
                                                   div(
                                                     tags$h5("Jacobsen, Nadine, Dr. rer. nat."),
                                                     tags$h6( tags$i("Postdoctoral Fellow"))
                                                   ),
                                                   div(
                                                     "Carl von Ossietzky Universitat Oldenburg."
                                                   )
                                               )
                                           )
                                    ),
                                    # Daniel
                                    column(2,
                                           div(class="panel panel-default",
                                               div(class="panel-body",  width = "600px",
                                                   align = "center",
                                                   div(
                                                     tags$img(src = "Daniel.jpg",
                                                              width = "40px", height = "50px")),
                                                   div(
                                                     tags$h5("Kristanto, Daniel, PhD."),
                                                     tags$h6( tags$i("Postdoctoral Fellow"))
                                                   ),
                                                   div(
                                                     "Carl von Ossietzky Universitat Oldenburg."
                                                   )
                                               )
                                           )
                                    ),
                                    column(2,
                                           div(class="panel panel-default",
                                               div(class="panel-body",  width = "600px",
                                                   align = "center",
                                                   div(
                                                     tags$img(src = "cassie.jpg",
                                                              width = "50px", height = "68px")),
                                                   div(
                                                     tags$h5("Short, Cassie, PhD."),
                                                     tags$h6( tags$i("Postdoctoral Fellow"))
                                                   ),
                                                   div(
                                                     "Carl von Ossietzky Universitat Oldenburg."
                                                   )
                                               )
                                           )
                                    ),
                                    column(3)
                                    
                                  ),
                                  fluidRow(style = "height:150px;")
                         )  # Closes About tab
                         
)

)