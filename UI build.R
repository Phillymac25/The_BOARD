if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
if (!requireNamespace("shinyWidgets", quietly = TRUE)) install.packages("shinyWidgets")
if (!requireNamespace("DT", quietly = TRUE)) install.packages("DT")
if (!requireNamespace("shinyjs", quietly = TRUE)) install.packages("shinyjs")

library(shiny)
library(shinyWidgets)
library(DT)
library(shinyjs)

# Define UI
ui <- fluidPage(
  useShinyjs(),  # Enable shinyjs
  tags$head(
    tags$style(HTML(paste0(
      "body {\n",
      "    background-color: #fff4b5;\n",  # pastel yellow background
      "    color: #ffb6c1;\n",  # Softer pastel pink text
      "    font-family: 'Courier New', monospace;\n",
      "    transition: background-color 0.5s, color 0.5s;\n",  # Smooth transition effect
      "    margin: 0;\n",
      "    padding: 0;\n",
      "    overflow-x: hidden;\n",  # Prevent horizontal scrolling
      "}\n",
      "html, body {\n",
      "    width: 100%;\n",
      "    height: 100%;\n",
      "}\n",
      ".container {\n",
      "    width: 100vw;\n",
      "    max-width: 100%;\n",
      "    margin: 0 auto;\n",
      "    padding: 20px;\n",
      "    background-color: #fff4b5;\n",  # Matching pastel yellow
      "    border-radius: 10px;\n",
      "    box-shadow: 0px 4px 10px rgba(0, 0, 0, 0.1);\n",
      "    transition: background-color 0.5s, color 0.5s;\n",  # Smooth transition effect
      "    display: flex;\n",
      "    flex-direction: column;\n",
      "    align-items: center;\n",
      "    justify-content: center;\n",
      "}\n",
      ".title {\n",
      "    font-size: 80px;\n",  # Increased title size significantly
      "    font-weight: bold;\n",
      "    text-align: center;\n",
      "    margin-bottom: 20px;\n",
      "}\n",
      "[data-theme='dark'] body {\n",
      "    background-color: #000000;\n",  # Black background for dark mode
      "    color: #39ff14;\n",  # Green text for retro terminal style
      "}\n",
      "[data-theme='dark'] .container {\n",
      "    background-color: #121212;\n",
      "    color: #ffffff;\n",  # Updated to white text for better visibility
      "}\n",
      "[data-theme='dark'] .title {\n",
      "    color: #39ff14 !important;\n",  # Ensuring title is green in dark mode
      "}\n",
      "[data-theme='dark'] .dataTables_wrapper, [data-theme='dark'] .dataTable {\n",
      "    color: #39ff14 !important;\n",  # Ensuring table text is green for visibility
      "    background-color: #000000 !important;\n",  # Black background for contrast
      "}\n"
    )))
  ),
  div(class="container",
      div(class="title", "The BOARD"),
      br(),
      switchInput(inputId = "darkModeSwitch", label = "Dark Mode", value = FALSE),
      br(),
      selectInput("filterStatus", "Filter by Status", choices = c("All", "Open", "In Progress", "Completed")),
      dataTableOutput("jobTable"),
      br(),
      textInput("jobTitle", "Job Title", ""),
      textAreaInput("jobDesc", "Job Description", ""),
      textInput("jobContact", "Job Contact", ""),
      textAreaInput("skillsRequired", "Skills Required", ""),
      textAreaInput("skillsGained", "Skills Gained", ""),
      textInput("estimatedTime", "Estimated Time Commitment", ""),
      selectInput("jobStatus", "Status", choices = c("Open", "In Progress", "Completed")),
      actionButton("postJob", "Post Job", class="btn-success"),
      br(), br(),
      h4("Update Job Status"),
      selectInput("updateJob", "Select a job", choices = NULL),
      selectInput("updateStatus", "New Status", choices = c("Open", "In Progress", "Completed")),
      actionButton("update", "Update Status", class="btn-warning"),
      br(), br(),
      h4("Remove a Job"),
      selectInput("removeJob", "Select a job", choices = NULL),
      actionButton("remove", "Remove Job", class="btn-danger"),
      br(), br(),
      h4("Apply for a Job"),
      selectInput("applyJob", "Select a job", choices = NULL),
      actionButton("apply", "Apply", class="btn-primary")
  )
)

# Define server logic
server <- function(input, output, session) {
  jobList <- reactiveVal(data.frame(JobTitle=character(), JobDesc=character(), JobContact=character(), SkillsRequired=character(), SkillsGained=character(), EstimatedTime=character(), Status=character(), stringsAsFactors=FALSE))
  
  observeEvent(input$postJob, {
    newJob <- data.frame(
      JobTitle = input$jobTitle,
      JobDesc = input$jobDesc,
      JobContact = input$jobContact,
      SkillsRequired = input$skillsRequired,
      SkillsGained = input$skillsGained,
      EstimatedTime = input$estimatedTime,
      Status = input$jobStatus,
      stringsAsFactors = FALSE
    )
    jobList(rbind(jobList(), newJob))
    updateSelectInput(session, "updateJob", choices = jobList()$JobTitle)
    updateSelectInput(session, "removeJob", choices = jobList()$JobTitle)
    updateSelectInput(session, "applyJob", choices = jobList()$JobTitle)
  })
  
  output$jobTable <- renderDataTable({
    jobs <- jobList()
    if (input$filterStatus != "All") {
      jobs <- jobs[jobs$Status == input$filterStatus, ]
    }
    datatable(jobs, options = list(dom = 't', pageLength = 5))
  })
  
  observeEvent(input$darkModeSwitch, {
    if (input$darkModeSwitch) {
      runjs("document.body.setAttribute('data-theme', 'dark');")
    } else {
      runjs("document.body.removeAttribute('data-theme');")
    }
  })
}

shinyApp(ui, server)
