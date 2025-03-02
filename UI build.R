if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
if (!requireNamespace("shinyWidgets", quietly = TRUE)) install.packages("shinyWidgets")
if (!requireNamespace("DT", quietly = TRUE)) install.packages("DT")

library(shiny)
library(shinyWidgets)
library(DT)

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML(paste0(
      "body {\n",
      "    background-color: #0d1117;\n",
      "    color: #39ff14;\n",
      "    font-family: 'Courier New', monospace;\n",
      "}\n",
      ".container {\n",
      "    max-width: 800px;\n",
      "    margin: auto;\n",
      "    padding: 20px;\n",
      "}\n",
      ".title {\n",
      "    text-align: center;\n",
      "    font-size: 30px;\n",
      "    font-weight: bold;\n",
      "    text-shadow: 2px 2px 5px #00ff00;\n",
      "    animation: blink 1s step-start infinite;\n",
      "}\n",
      "@keyframes blink {\n",
      "    50% { opacity: 0; }\n",
      "}\n",
      ".loading-text {\n",
      "    font-family: 'Courier New', monospace;\n",
      "    color: #39ff14;\n",
      "    text-align: center;\n",
      "    font-size: 20px;\n",
      "    animation: typing 3s steps(30, end) infinite alternate;\n",
      "}\n",
      "@keyframes typing {\n",
      "    from { width: 0 }\n",
      "    to { width: 100% }\n",
      "}\n",
      ".cursor {\n",
      "    display: inline-block;\n",
      "    width: 10px;\n",
      "    height: 20px;\n",
      "    background-color: #39ff14;\n",
      "    animation: blink 0.6s step-end infinite alternate;\n",
      "}\n",
      ".beep-sound {\n",
      "    display: none;\n",
      "}"
    ))),
    tags$audio(id="beep", src="https://www.fesliyanstudios.com/play-mp3/4386", type="audio/mpeg", autoplay=NA)
  ),
  div(class="container",
      div(class="title", "The BOARD"),
      div(class="loading-text", "Loading... Please Wait<span class='cursor'></span>"),
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
  jobCounter <- reactiveVal(1)  # Auto-increment Job ID
  jobs <- reactiveVal(data.frame(JobID = integer(), Title = character(), Description = character(), Contact = character(), SkillsRequired = character(), SkillsGained = character(), EstimatedTime = character(), Status = character(), stringsAsFactors = FALSE))
  previousJobs <- reactiveVal(NULL)  # Store previous state for undo
  
  observeEvent(input$postJob, {
    if (input$jobDesc == "" || input$jobContact == "" || input$estimatedTime == "") {
      showModal(modalDialog(
        title = "Missing Information",
        "Please fill in Job Description, Job Contact, and Estimated Time Commitment before posting.",
        easyClose = TRUE,
        footer = NULL
      ))
    } else {
      previousJobs(jobs())  # Save previous state for undo
      newJob <- data.frame(JobID = jobCounter(), Title = input$jobTitle, Description = input$jobDesc, Contact = input$jobContact, SkillsRequired = input$skillsRequired, SkillsGained = input$skillsGained, EstimatedTime = input$estimatedTime, Status = input$jobStatus, stringsAsFactors = FALSE)
      jobs(rbind(jobs(), newJob))
      jobCounter(jobCounter() + 1)  # Increment Job ID
      updateSelectInput(session, "applyJob", choices = jobs()$Title)
      updateSelectInput(session, "updateJob", choices = jobs()$Title)
      updateSelectInput(session, "removeJob", choices = jobs()$Title)
      
      runjs("document.getElementById('beep').play();")
      showModal(modalDialog(
        title = "Job Posted Successfully",
        "Your job has been posted successfully!\nProceed? (Y/N)",
        footer = tagList(
          modalButton("No"),
          actionButton("confirmPost", "Yes")
        )
      ))
    }
  })
}

shinyApp(ui, server)

