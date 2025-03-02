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
      "body {",
      "background-color: #fff4b5;",  # pastel yellow background
      "color: #ffb6c1;",  # Softer pastel pink text
      "font-family: 'Courier New', monospace;",
      "transition: background-color 0.5s, color 0.5s;",  # Smooth transition effect
      "margin: 0; padding: 0; overflow-x: hidden;",  # Prevent horizontal scrolling
      "}",
      "html, body { width: 100%; height: 100%; }",
      ".container { width: 100vw; max-width: 100%; margin: 0 auto; padding: 20px;",
      "background-color: #fff4b5; border-radius: 10px; box-shadow: 0px 4px 10px rgba(0, 0, 0, 0.1); transition: background-color 0.5s, color 0.5s;",
      "display: flex; flex-direction: column; align-items: center; justify-content: center; }",
      ".title { font-size: 80px; font-weight: bold; text-align: center; margin-bottom: 20px; }",
      "[data-theme='dark'] body {",
      "background-color: #000000;",  # Black background for dark mode
      "color: #39ff14;",  # Green text for retro terminal style
      "}",
      "[data-theme='dark'] .container {",
      "background-color: #121212;",
      "color: #ffffff;",  # Updated to white text for better visibility
      "}",
      "[data-theme='dark'] .title {",
      "color: #39ff14 !important;",  # Ensuring title is green in dark mode
      "}",
      "[data-theme='dark'] .dataTables_wrapper, [data-theme='dark'] .dataTable {",
      "color: #39ff14 !important;",  # Ensuring table text is green for visibility
      "background-color: #000000 !important;",  # Black background for contrast
      "}")))
  ),
  div(class="container",
      div(class="title", "The BOARD"),
      switchInput("darkMode", label = "Dark Mode", value = FALSE),
      br(),
      selectInput("filterStatus", "Filter by Status", choices = c("All", "Open", "In Progress", "Completed")),
      dataTableOutput("jobTable"),
      br(),
      textInput("jobTitle", "Job Title", ""),
      textAreaInput("jobDesc", "Job Description", ""),
      textInput("jobContact", "Job Contact", ""),
      textAreaInput("skillsneeded", "Skills Needed", ""),
      textAreaInput("skillsgained", "Skills Gained", ""),
      textInput("timecommit", "Time Commitment", ""),
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
      actionButton("remove", "Remove Job", class="btn-danger")
  )
)

# Define server logic
server <- function(input, output, session) {
  observeEvent(input$darkMode, {
    if (input$darkMode) {
      runjs("document.documentElement.setAttribute('data-theme', 'dark');")
    } else {
      runjs("document.documentElement.removeAttribute('data-theme');")
    }
  })
  
  jobs <- reactiveVal(data.frame(Title = character(), Description = character(), Contact = character(), SkillsNeeded = character(), SkillsGained = character(), TimeCommitment = character(), Status = character(), stringsAsFactors = FALSE))
  
  confirmAction <- function(message, action) {
    showModal(modalDialog(
      title = "Confirmation",
      div(message),
      footer = tagList(
        actionButton("confirmYes", "Yes"),
        modalButton("No")
      )
    ))
    observeEvent(input$confirmYes, {
      removeModal()
      isolate(action())
    }, ignoreInit = TRUE, once = TRUE)
  }
  
  observeEvent(input$postJob, {
    confirmAction("Are you sure you want to add this job?", function() {
      newJob <- data.frame(Title = input$jobTitle, Description = input$jobDesc, Contact = input$jobContact, SkillsNeeded = input$skillsneeded, SkillsGained = input$skillsgained, TimeCommitment = input$timecommit, Status = input$jobStatus, stringsAsFactors = FALSE)
      jobs(rbind(jobs(), newJob))
      updateSelectInput(session, "updateJob", choices = jobs()$Title)
      updateSelectInput(session, "removeJob", choices = jobs()$Title)
    })
  })
  
  observeEvent(input$update, {
    confirmAction("Are you sure you want to update this job status?", function() {
      updatedJobs <- jobs()
      updatedJobs$Status[updatedJobs$Title == input$updateJob] <- input$updateStatus
      jobs(updatedJobs)
    })
  })
  
  observeEvent(input$remove, {
    confirmAction("Are you sure you want to remove this job?", function() {
      updatedJobs <- jobs()
      jobs(updatedJobs[updatedJobs$Title != input$removeJob, ])
      updateSelectInput(session, "updateJob", choices = jobs()$Title)
      updateSelectInput(session, "removeJob", choices = jobs()$Title)
    })
  })
  
  output$jobTable <- renderDataTable({
    if (input$filterStatus == "All") {
      datatable(jobs(), options = list(dom = 't', pageLength = 5))
    } else {
      datatable(jobs()[jobs()$Status == input$filterStatus, ], options = list(dom = 't', pageLength = 5))
    }
  })
}

shinyApp(ui, server)
