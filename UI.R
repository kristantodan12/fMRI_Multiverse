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
                                    column(1),
                                    column(10,
                                           shiny::HTML("<br><br><center> <h1>METEOR</h1> </center><br>"),
                                           shiny::HTML("<h3>MastEring ThE OppRessive number of forking paths 
                                                        unfolded by noisy and complex neural data</h3>")
                                  ),
                                    column(1)
                                  ),
                                  
                                  
                                  # WHAT
                                  fluidRow(
                                    column(1),
                                    column(10,
                                           shiny::HTML("<br><br><center> <h1>What you'll find here</h1> </center><br>"),
                                           shiny::HTML("<h3>An interactive tool to help you explore the forking paths 
                                                       in the data preprocessing and analyses multiverse of graph fMRI study.</h3>")
                                    ),
                                    column(1)
                                  ),
                                  
                                  
                                  # WHERE
                                  fluidRow(
                                    column(1),
                                    column(10,
                                           shiny::HTML("<br><br><center> <h1>Where it came from</h1> </center><br>"),
                                           shiny::HTML("<h3>The data are the results of literature review performed in 
                                                       multiple databases.</h3>")
                                    ),
                                    column(1)
                                  ),
                                  
                                  
                                  # HOW TO START
                                  fluidRow(
                                    column(1),
                                    column(10,
                                           shiny::HTML("<br><br><center> <h1>How to get started</h1> </center><br>"),
                                           shiny::HTML("<h3>The app consists of several tabs, each serving a distinct function:<br>
                                           1.	The initial tab, labeled Introduction, provides an introduction to the app, an overview of its available features, and information on how to use them.<br>
                                           2.	The following tab, Database, presents all the gathered data, organised into various subsections. These include the PRISMA diagrams, a list of papers included in the Review 2, and lists of steps and options identified during the analysis.<br>
                                           3.	Next, users can explore the aggregated pipelines across studies in Steps tab. The Steps tab contains multiple subsections: Aggregated steps, Combination, and Order. Aggregated steps visualizes the aggregated pipelines in network fashion, where the nodes represent pre-processing or analysis steps, and the edges are the connection between two steps. By hovering and clicking the nodes/edges, users can get information about the steps and edges, including how many papers used them and their definitions. There are also some settings on the left dashboard to specify the displayed step and the threhshold on the number of papers used the edges. Combination subsection allows users to explore pairs of functional pre-processing steps used in combination. By selecting a specific step, such as Global Signal Regression, a lollipop plot is generated to indicate the frequency at which other steps were employed in conjunction with Global Signal Regression. Lastly, Order subsection enables users to investigate the ordering of specific steps relative to others. For instance, if a user selects the step of Motion Regression, a bar plot is generated to illustrate how many studies implemented other steps following Motion Regression.<br>
                                           4.	The next tab, Options, enables users to visualise the distribution of options chosen by different studies. For example, users can examine the distribution of software used for fMRI data pre-processing. Within this tab, users can also specify a particular option, such as SPM (Statistical Parametric Mapping), and obtain a list of studies that employed this option.<br>
                                           5.	Moreover, users can explore and visualise the steps and options taken by individual study in “Individual Paper”. It contains two subsection STEP VISUALISATION and OPTION VISUALISATION, enabling users to interactively explore the pre-processing and analysis pipeline and the specific options chosen by each study.<br>
                                           6.	We have also incorporated an important feature called the YOUR OWN PIPELINE tab, where users can input their preferred pipeline for fMRI data pre-processing and the associated options. The app then provides a count of the number of studies that have used the same pipeline and a list of these studies. Importantly, users can specify whether the order of their pipeline should be taken into account. If this option is disabled, the algorithm will count the number of papers that have employed the user-inputted pre-processing pipeline, regardless of the order.<br>
                                           7.	Finally, the last tab, labelled ABOUT, provides information about the project and the research team involved in this endeavour.</h3>")
                                    ),
                                    column(1)
                                  ),
                                  
                                  # BUTTONS TO START
                                  
                                  fluidRow(
                                    
                                    style = "height:50px;"),
                                  
                                  # PAGE BREAK
                                  tags$hr(),
                                  
                                  fluidRow(shiny::HTML("<br><br><center> <h1>Ready to Get Started?</h1> </center>
                                                 <br>")
                                  ),
                                  fluidRow(
                                    column(3),
                                    column(6,
                                           tags$div(align = "center", 
                                                    tags$a("START", 
                                                           onclick="fakeClick('MA')", 
                                                           class="btn btn-primary btn-lg")
                                           )
                                           
                                    ),
                                    column(3)
                                  ),
                                  fluidRow(style = "height:25px;"
                                  )
                                  
                         ), # Closes the first tabPanel called "Home"
                         
                         tabPanel("Database", value = "MA",
                                  mainPanel(
                                    tabsetPanel(type = "tabs",
                                    tabPanel(
                                      "PRISMA diagram",
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
                                        column(8, # Display the first image and caption in a 4-column layout
                                               shiny::HTML("<h3>Defining the space from general fMRI papers</h3>"),
                                               img(src='prisma1.jpg', align = "center", width = "600px", height = "750px"),
                                               shiny::HTML("<h3>Defining the forking paths of graph fMRI studies</h3>"),
                                               img(src='prisma2.jpg', align = "center", width = "600px", height = "900px")
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
                                                  "Aggregated steps",
                                                  sidebarLayout( 
                                                    
                                                    sidebarPanel( width = 3,
                                                                  shiny::HTML("<h5><b>Explore the aggregated preprocessing 
                                                                       pipelines of all articles in a network fashion.</b><br><br>
                                                                       Each node (circle) represents a pre-processing step. The 
                                                                       color of a node indicates the processing group the step 
                                                                       belongs to. <br><br>
                                                                       Steps performed in succession are connected by arrows, 
                                                                       these are edges. The wider the arrow, the higher the number
                                                                       of papers using this edge. The arrow points in the direction 
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
                                                                                              'Structural Preprocessing' = (c(nodes$Names_vis[nodes$Groups=='Structural_preprocessing'])),
                                                                                              'Functional Preprocessing' = (c(nodes$Names_vis[nodes$Groups=='Functional_preprocessing'])),
                                                                                              'Noise Removal' = (c(nodes$Names_vis[nodes$Groups=='Noise_removal'])),
                                                                                              'FC Definition' = (c(nodes$Names_vis[nodes$Groups=='FC_def'])),
                                                                                              'Graph Analysis' = (c(nodes$Names_vis[nodes$Groups=='Graph_analysis']))),
                                                                              selected = "All"
                                                                  ),
                                                                  sliderInput("Thr", "Threshold paper",
                                                                              min = 0, max = 99,
                                                                              value = 0
                                                                  ),
                                                                  shiny::HTML("<h5>Move the threshold to only see edges used by more papers than the threshold.</h5>"),
                                                    ),  # Closes sidebarPanel
                                                    mainPanel( width = 9,
                                                               forceNetworkOutput(outputId = "WP", width = "100%", height = "700px")
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
                                                                            choices =  list('Structural Preprocessing' = (c(nodes$Names_vis[nodes$Groups=='Structural_preprocessing'])),
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
                                                  "Orders",
                                                  sidebarPanel( width = 3,
                                                                shiny::HTML("<h5><b>Investigate the order of pre-processing steps.</b><br><br>
                                                                Select a preprocessing step from the dropdown below to see which 
                                                                preprocessing steps were performed AFTER the selected one.<br><br></h5>"),
                                                                selectInput("selectDecisionOR",
                                                                            label   = "Explore steps performed after step:",
                                                                            choices =  list(
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
                                                  

                         tabPanel("Individual Paper", value = "IP",
                                    tabsetPanel(type = "tabs",
                                                tabPanel(
                                                  "Step Visualisation",sidebarLayout( 
                                                    
                                                    sidebarPanel( width = 3,
                                                                  shiny::HTML("<h5><b>Visualize the pre-processing steps taken by a 
                                                                  specific article.</b><br><br>
                                                                  Select the code of the article in the dropdown below to get 
                                                                  a visualization of the pre-processing steps and to generate a 
                                                                  table of the pre-procesing steps. You can 
                                                                  find the code of each study in the Database tab.<br></h5>"),
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
                                                  "Option visualisation",
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
                                                              choices =  list(
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