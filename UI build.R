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
      ".modal-content {",
      "  background-color: #fff4b5 !important;",  # Ensuring modal background is pastel yellow
      "  color: #ffb6c1;",  # Text color matching the pastel pink
      "}",
      ".modal-header {",
      "  background-color: #fff4b5 !important;",  # Ensuring modal header is the same pastel yellow
      "  border-bottom: none;",  # Removing border from header for consistency
      "}",
      ".modal-footer {",
      "  background-color: #fff4b5 !important;",  # Ensuring footer is pastel yellow
      "}",
      ".modal-body {",
      "  font-size: 16px;",  # Set font size
      "  color: #ffb6c1;",  # Set text color to pastel pink
      "}",
      ".modal-title {",
      "  font-weight: bold;",  # Title boldness
      "}",
      "[data-theme='dark'] .modal-content {",
      "  background-color: #121212 !important;",  # Dark background for modal
      "  color: #39ff14;",  # Green text in dark mode
      "}",
      "[data-theme='dark'] .modal-header {",
      "  background-color: #121212 !important;",  # Dark background for header
      "}",
      "[data-theme='dark'] .modal-footer {",
      "  background-color: #121212 !important;",  # Dark footer
      "}",
      "[data-theme='dark'] .modal-body {",
      "  color: #39ff14 !important;",  # Green text in the body in dark mode
      "}",
      "[data-theme='dark'] .modal-title {",
      "  color: #39ff14 !important;",  # Title in green in dark mode
      "}",
      "[data-theme='dark'] body {",
      "  background-color: #000000;",  # Black background for dark mode
      "  color: #39ff14;",  # Green text for retro terminal style
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
      div(class="title", "The B.O.A.R.D"),
      div(class="subtitle", "Best.OCE.Analytical.Resource.Deployment"),
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
  
  # Display Welcome Modal on App Load
  showModal(modalDialog(
    title = "Welcome to The B.O.A.R.D",
    div(style = "font-size: 20px;", "Best.OCE.Analytical.Resource.Deployment"),
    tags$hr(),
    h4("Purpose"),
    HTML("<p>In OCE we care deeply about your development . The B.O.A.R.D is designed to help everyone in OCE hit their development goals whilst also making sure resources are allocated in the very most optimal way <br>
         The B.O.A.R.D lets people with a job that needs doing match up with those who have some spare time for some doing, but need a job.<br>
         Users can post a job alongside some headline information about the job, then others can mosey along and see if they think they'd be suited to the task.</p>"),
    tags$hr(),
    h4("How to Use"),
    HTML("<p>Thank you for choosing to spend some of your time with The B.O.A.R.D today.<br>
         To ensure there are no unproductive mishaps please take a brief moment to read, and understand, these instructions. <br>
         <br>
         <strong> Posting a Job </strong>
         If you have a job that you want posted to The B.O.A.R.D please first liase with your line manager. If you both deem it appropriate to add to the board then you may continue. <br>
         <br>
         Enter the B.O.A.R.D and provide us with a job title and a few pieces of information to help people see if the job is right for them. Remember to fil in all the fields!
         <br>
         Then just click the Post job button and you're off to the races!<br>
         <br>
         Wait for someone to contact you about the Job and discuss with them in detail to make sure it works for them. If everything is hunky dory then head back to The B.O.A.R.D and update the job status<br>
         <br>
         When the job is done make sure to come back one more time and change the job status to closed. You're now releived of all obligation.
         <br>
         <strong>Taking a Job</strong><br>
         Before looking for a job on The B.O.A.R.D it is essential that you check with your line manager and agree you have some spare time to pick up jobs from The B.O.A.R.D <br>
         <br>
         Failure to check with your line manager could lead to negative consequences and exclude you from future engagement with the B.O.A.R.D<br>
         <br>
         After agreeing with your line manager enter the B.O.A.R.D browse from the array of jobs avaialble and choose one appropriate you. <br>
         <br>
         Make sure that you don't just look at the job description and skills needed. Also check that the skills gained matches your personal development needs and the time commitment matches your time available.<br>
         <br>
         You can filter to see which jobs are OPen, In Progress or Closed. Some in progress jobs might need extra people, there may be no need for despair. <br>
         <br>
         Once you find a job that is appropriate it's time to take it offline. Contact the users through electronic mail or use an instant messaging function to contact them and find out more.</p>"),
    tags$hr(),
    h4("Contact"),
    HTML("<p>In the unlikely event that your experience with The B.O.A.R.D is less than optimal please contact example.mdkd@jnfkd.co.uk. We can assure you there will be no reperocussions for this cause of action.</p>"),
    easyClose = TRUE,
    footer = modalButton("Close")
  ))
  
  # Function to confirm actions
  confirmAction <- function(id, message) {
    showModal(modalDialog(
      title = "Confirmation",
      div(message),
      footer = tagList(
        actionButton(paste0("confirmYes_", id), "Yes"),
        modalButton("No")
      )
    ))
  }
  
  # Reactive value for jobs
  jobs <- reactiveVal(data.frame(Title = character(), Description = character(), Contact = character(), 
                                 SkillsNeeded = character(), SkillsGained = character(), 
                                 TimeCommitment = character(), Status = character(), stringsAsFactors = FALSE))
  
  # Function to check if all fields are filled
  validateFields <- function() {
    if (input$jobTitle == "" || input$jobDesc == "" || input$jobContact == "" ||
        input$skillsneeded == "" || input$skillsgained == "" || input$timecommit == "" || input$jobStatus == "") {
      return(FALSE)
    }
    return(TRUE)
  }
  
  # Post Job with Confirmation
  observeEvent(input$postJob, {
    if (validateFields()) {
      confirmAction("post", HTML("Are you sure you want to add this job?<br> 
                             Failure to properly consider this message may have consequences"))
    } else {
      showModal(modalDialog(
        title = "Error",
        HTML("<strong>Oh no silly billy!</strong><br>
            It looks like you've forgotten to fill out all of the fields.<br>
            Have another go and this time try to do everything that has been asked of you."),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    }
  })
  
  observeEvent(input$confirmYes_post, {
    removeModal()
    newJob <- data.frame(Title = input$jobTitle, Description = input$jobDesc, Contact = input$jobContact, 
                         SkillsNeeded = input$skillsneeded, SkillsGained = input$skillsgained, 
                         TimeCommitment = input$timecommit, Status = input$jobStatus, stringsAsFactors = FALSE)
    jobs(rbind(jobs(), newJob))
    updateSelectInput(session, "updateJob", choices = jobs()$Title)
    updateSelectInput(session, "removeJob", choices = jobs()$Title)
  })
  
  # Update Job Status with Confirmation
  observeEvent(input$update, {
    jobToUpdate <- input$updateJob
    newStatus <- input$updateStatus
    
    if (!is.null(jobToUpdate)) {
      confirmAction("update", HTML("Are you sure you want to update the status of<strong?>", jobToUpdate, "</strong>to<strong>", newStatus, "</strong>?<br> 
                                     Unauthorised updates may be investigated"))
    }
  })
  
  observeEvent(input$confirmYes_update, {
    removeModal()
    jobToUpdate <- input$updateJob
    newStatus <- input$updateStatus
    
    jobsData <- jobs()
    jobsData[jobsData$Title == jobToUpdate, "Status"] <- newStatus
    jobs(jobsData)
  })
  
  # Remove Job with Confirmation
  observeEvent(input$remove, {
    jobToRemove <- input$removeJob
    
    if (!is.null(jobToRemove)) {
      confirmAction("remove", HTML("Are you sure you want to remove <strong>", jobToRemove, "</strong>? <br>
                                     Please remember to think before acting"))
    }
  })
  
  observeEvent(input$confirmYes_remove, {
    removeModal()
    jobToRemove <- input$removeJob
    
    jobsData <- jobs()
    jobsData <- jobsData[jobsData$Title != jobToRemove, ]
    jobs(jobsData)
    updateSelectInput(session, "updateJob", choices = jobs()$Title)
    updateSelectInput(session, "removeJob", choices = jobs()$Title)
  })
  
  # Observe and update the select inputs dynamically
  observe({
    updateSelectInput(session, "updateJob", choices = jobs()$Title)
    updateSelectInput(session, "removeJob", choices = jobs()$Title)
  })
  
  # Display the jobs table
  output$jobTable <- renderDataTable({
    if (nrow(jobs()) == 0) {
      return(NULL)  # Handle the case where there are no jobs
    }
    
    if (input$filterStatus == "All") {
      datatable(jobs(), options = list(dom = 't', pageLength = 5))
    } else {
      datatable(jobs()[jobs()$Status == input$filterStatus, ], options = list(dom = 't', pageLength = 5))
    }
  })
  
  # Dark Mode Toggle
  observeEvent(input$darkMode, {
    if (input$darkMode) {
      runjs("document.documentElement.setAttribute('data-theme', 'dark');")  # Apply dark theme
    } else {
      runjs("document.documentElement.removeAttribute('data-theme');")  # Remove dark theme
    }
  })
}

shinyApp(ui, server)