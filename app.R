library(shiny)
library(httr)
library(sass)
library(markdown)
library(waiter)
library(shinyjs)
library(shinyCopy2clipboard)
library(shinyauthr)
library(bslib)

# JavaScript code to ensure the latest message is visible in the chat container
jscode <- 'var container = document.getElementById("chat-container");
if (container) {
  var elements = container.getElementsByClassName("user-message");
  if (elements.length > 1) {
    var lastElement = elements[elements.length - 1];
    lastElement.scrollIntoView({
      behavior: "smooth"
    });
  }
}'

# Set environment variable for the OpenAI API key
Sys.setenv(OPENAI_API_KEY = "sk-YOURKEY")

# Simple user database 
users <- data.frame(
  username = c("user1", "user2","user3","user4"),
  password = c("pw1", "pw2","pw3","pw4"), 
  stringsAsFactors = FALSE
)

# Function to interact with OpenAI's GPT model via API
chatGPT_R <- function(apiKey, prompt, history_formatted, model) {
  messages <- if (length(history_formatted) > 0) {
    history_formatted
  } else {
    list(list(role = "user", content = prompt))
  }
  
  messages <- c(messages, list(list(role = "user", content = prompt)))
  
  body <- list(
    model = model,
    messages = messages
  )
  
  response <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(Authorization = paste("Bearer", apiKey)),
    content_type_json(),
    body = body,
    encode = "json"
  )
  
  if (status_code(response) > 200) {
    result <- paste("Error:", content(response)$error$message)
  } else {
    result <- content(response)$choices[[1]]$message$content
  }
  
  return(result)
}

execute_at_next_input <- function(expr, session = getDefaultReactiveDomain()) {
  observeEvent(once = TRUE, reactiveValuesToList(session$input), {
    force(expr)
  }, ignoreInit = TRUE)
}

ui <- fluidPage(
  #theme = bs_theme(),
  useShinyjs(), 
  useWaiter(),
  use_copy(), 
  tags$head(
    tags$style(HTML("
      #chat-container {
        max-height: 1200px;
        max-width: 2200px;
        min-height: 800px;  /* Minimum height */
        min-width: 800px;   /* Minimum width */
        overflow-y: auto;
        overflow-x: auto;
        border: 1px solid #ccc;
        padding: 10px;
        width: 100%; /* Ensure container takes full width */
        box-sizing: border-box; /* Include padding and border in element's total width and height */
      }
      .user-message {
        text-align: right;
        margin: 10px;
        padding: 10px;
        background-color: #d1e7dd;
        border-radius: 10px;
        display: inline-block; /* Make sure messages don't push container width */
      }
      .bot-message {
        text-align: left;
        margin: 10px;
        padding: 10px;
        background-color: #f8d7da;
        border-radius: 10px;
        display: inline-block; /* Make sure messages don't push container width */
      }
      #chat-input {
        margin-top: 20px;
        width: 100%; /* Ensure input field takes full width */
      }
    "))
  ),
  tags$script(HTML('
    var chatContainer = document.getElementById("chat-container");
    if (chatContainer) {
      chatContainer.scrollTop = chatContainer.scrollHeight;
    }
  ')),
  div(id = "login_page",
      wellPanel(
        textInput("username", "Username"),
        passwordInput("password", "Password"),
        actionButton("login", "Log in")
      )),
  div(id = "main_app", style = "display: none;",
      tags$div(
        id = "model-selection",
        selectInput("model", "Model", choices = c("gpt-3.5-turbo", "gpt-4", "gpt-4-turbo"), selected = "gpt-4-turbo"),
        style = "background-color: #fff; color: #333; border: 1px solid #ccc; padding: 10px; margin-bottom: 20px;"
      ),
      tags$div(
        id = "chat-container",
        tags$div(
          id = "chat-header",
          tags$h3("Atlas Chat: Solutions at Your Fingertips")
        ),
        tags$div(
          id = "chat-header",
          textOutput("modelDisplay")
        ),
        tags$div(
          id = "chat-history",
          uiOutput("chatThread")
        )
      ),
      tags$div(
        id = "chat-input",
        tags$form(
          textAreaInput(inputId = "prompt", label = "", placeholder = "Type your prompt here...", width = "100%"),
          fluidRow(
            actionButton(inputId = "submit", label = "Send", icon = icon("paper-plane")),
            actionButton(inputId = "remove_chatThread", label = "Clear History", icon = icon("trash-can")),
            CopyButton("clipbtn", label = "Copy", icon = icon("clipboard"), text = "")
          )
        )
      )
  )
)


server <- function(input, output, session) {
  userSession <- reactiveValues(username = NULL)
  
  observeEvent(input$login, {
    valid_user <- any(input$username == users$username & input$password == users$password)
    
    if (valid_user) {
      shinyjs::hide("login_page")
      shinyjs::show("main_app")
      userSession$username <- input$username
    } else {
      shinyjs::alert("Login failed: Incorrect username or password.")
    }
  })
  
  output$modelDisplay <- renderText({
    paste("Current model:", input$model)
  })
  
  historyALL <- reactiveValues(df = data.frame(role = character(), content = character()), val = character(0))
  
  observeEvent(input$submit, {
    if (nchar(trimws(input$prompt)) > 0) {
      historyALL$df <- rbind(historyALL$df, data.frame(role = userSession$username, content = input$prompt, stringsAsFactors = FALSE))
      
      history_formatted <- lapply(1:nrow(historyALL$df), function(i) {
        list(role = ifelse(historyALL$df[i, "role"] == userSession$username, "user", "assistant"),
             content = historyALL$df[i, "content"])
      })
      
      if (length(history_formatted) == 0) {
        history_formatted <- list(list(role = "system", content = "Start of conversation"))
      }
      
      w <- Waiter$new(id = "chat-history",
                      html = spin_3(),
                      color = transparent(.5))
      w$show()
      
      chatGPT_response <- if (input$model %in% c("gpt-3.5-turbo", "gpt-4", "gpt-4-turbo")) {
        chatGPT_R(Sys.getenv("OPENAI_API_KEY"), input$prompt, history_formatted, input$model)
      } else {
        "Invalid model selected."
      }
      
      # Clean up the response to remove any redundancy (e.g., duplicate steps or phrases)
      cleaned_response <- cleanResponse(chatGPT_response)
      
      historyALL$df <- rbind(historyALL$df, data.frame(role = "assistant", content = cleaned_response, stringsAsFactors = FALSE))
      historyALL$val <- cleaned_response
      
      updateTextInput(session, "prompt", value = "")
      output$chatThread <- renderUI({
        conversations <- lapply(seq_len(nrow(historyALL$df)), function(x) {
          tags$div(class = ifelse(historyALL$df[x, "role"] == userSession$username,
                                  "user-message",
                                  "bot-message"),
                   
                   HTML(markdownToHTML(text = paste0(ifelse(historyALL$df[x, "role"] == userSession$username,
                                                            paste0("***", userSession$username, "***: "),
                                                            "***ATLAS***: "),
                                                     historyALL$df[x, "content"]))))
        })
        do.call(tagList, conversations)
      })
      
      w$hide()
      execute_at_next_input(runjs(jscode))
    }
  })
  
  observeEvent(input$remove_chatThread, {
    output$chatThread <- renderUI({return(NULL)})
    historyALL$df <- NULL
    updateTextInput(session, "prompt", value = "")
  })
  
  observe({
    req(input$clipbtn)
    CopyButtonUpdate(session,
                     id = "clipbtn",
                     label = "Copy",
                     icon = icon("clipboard"),
                     text = as.character(historyALL$val))
  })
  
  # Helper function to clean response
  cleanResponse <- function(response) {
    # Split the response into sentences
    sentences <- unlist(strsplit(response, "\\.\\s+"))
    
    # Initialize an empty vector to store unique sentences
    unique_sentences <- c()
    
    # Loop over sentences and keep only the first occurrence of each
    for (sentence in sentences) {
      if (!sentence %in% unique_sentences) {
        unique_sentences <- c(unique_sentences, sentence)
      }
    }
    
    # Recombine unique sentences into a single string
    cleaned_response <- paste(unique_sentences, collapse = ". ")
    
    return(cleaned_response)
  }
}



shinyApp(ui = ui, server = server)
