# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(shinydashboard)
library(plotly)
library(DT)
library(scales)
library(shinyjs)
library(waiter) # for loading animations
library(RSQLite)
library(sodium)
library(shinyjs)
library(googleAuthR)
library(httr)
library(jsonlite)
library(conflicted)

conflicts_prefer(
  dplyr::filter,
  dplyr::lag,
  dplyr::intersect,
  dplyr::setdiff,
  dplyr::setequal,
  dplyr::union,
  shinydashboard::box,
  plotly::layout,
  shiny::observe,
  httr::config
)

options(shiny.port = 6740)

# ENVs
readRenviron("./.env")
google_client_id <- Sys.getenv("GOOGLE_CLIENT_ID")
google_client_secret <- Sys.getenv("GOOGLE_CLIENT_SECRET")
google_redirect_uri <- Sys.getenv("GOOGLE_REDIRECT_URI")

# Initialize SQLite database for users
init_db <- function() {
  con <- dbConnect(RSQLite::SQLite(), "users.db")
  
  # Create users table if it doesn't exist
  if (!dbExistsTable(con, "users")) {
    dbExecute(con, "CREATE TABLE users (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      username TEXT UNIQUE,
      password TEXT,
      email TEXT UNIQUE NOT NULL,
      google_id TEXT UNIQUE,
      google_email TEXT,
      profile_picture TEXT,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )")
  }
  
  dbDisconnect(con)
}

# Initialize database
init_db()

# Add these UI components for login and signup pages
loginPageUI <- function() {
  div(
    class = "login-container",
    style = "max-width: 400px; margin: 100px auto; padding: 20px; background: white; border-radius: 8px; box-shadow: 0 0 10px rgba(0,0,0,0.1);",
    h2("Login", style = "text-align: center; margin-bottom: 20px;"),
    textInput("login_username", "Username"),
    passwordInput("login_password", "Password"),
    div(
      style = "text-align: center; margin-top: 20px;",
      actionButton("login_btn", "Login", 
                   class = "btn-primary",
                   style = "width: 100%; margin-bottom: 10px;"),
      actionButton("go_to_signup", "Don't have an account? Sign up", 
                   class = "btn-link")
    ),
    tags$hr(style = "margin: 20px 0;"),
    div(class = "auth-switch",
        "Or sign in with:",
        actionButton("google_signin", "Sign in with Google",
                     class = "auth-btn",
                     style = "background-color: #4285f4; margin-top: 10px;",
                     icon = icon("google")
        )
    ),
    textOutput("login_message")
  )
}

signupPageUI <- function() {
  div(
    class = "signup-container",
    style = "max-width: 400px; margin: 100px auto; padding: 20px; background: white; border-radius: 8px; box-shadow: 0 0 10px rgba(0,0,0,0.1);",
    h2("Sign Up", style = "text-align: center; margin-bottom: 20px;"),
    textInput("signup_username", "Username"),
    textInput("signup_email", "Email"),
    passwordInput("signup_password", "Password"),
    passwordInput("signup_confirm_password", "Confirm Password"),
    div(
      style = "text-align: center; margin-top: 20px;",
      actionButton("signup_btn", "Sign Up", 
                   class = "btn-primary",
                   style = "width: 100%; margin-bottom: 10px;"),
      actionButton("go_to_login", "Already have an account? Login", 
                   class = "btn-link")
    ),
    textOutput("signup_message")
  )
}

# Load data
data <- read.csv("C:\\Users\\DELL\\OneDrive\\Documents\\R project\\data.csv")

# Clean and preprocess the data
filtered_df <- data %>%
  # First remove any rows with missing values
  dplyr::filter(!is.na(location), !is.na(price), !is.na(total_sqft), 
                !is.na(bath), !is.na(balcony)) %>%
  # Clean the total_sqft column
  mutate(
    # Remove any non-numeric characters and convert ranges to averages
    total_sqft = sapply(as.character(total_sqft), function(x) {
      # If it contains a range (e.g., "1000 - 1200"), take the average
      if(grepl("-", x)) {
        parts <- as.numeric(strsplit(gsub("[^0-9.-]", "", x), "-")[[1]])
        return(mean(parts, na.rm = TRUE))
      }
      # Remove any non-numeric characters except decimal point
      return(as.numeric(gsub("[^0-9.]", "", x)))
    }),
    # Convert other columns to numeric
    price = as.numeric(as.character(price)),
    bath = as.numeric(as.character(bath)),
    balcony = as.numeric(as.character(balcony))
  ) %>%
  # Remove any rows where conversion failed
  dplyr::filter(!is.na(total_sqft), !is.na(price), 
                total_sqft > 0, price > 0) %>%
  # Remove outliers (optional - adjust these thresholds as needed)
  dplyr::filter(total_sqft <= quantile(total_sqft, 0.99) & 
                  total_sqft >= quantile(total_sqft, 0.01))

dashboardUI <- function() {
  # Your existing UI code here (everything that's currently in your ui variable)
  tagList(
    useShinyjs(),
    use_waiter(),
    # ... rest of your existing UI code ...
    # Custom CSS
    tags$head(
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/animate.css/4.1.1/animate.min.css"),
      tags$style(HTML('
      .home-page {
        text-align: center;
        padding: 20px;
        background: linear-gradient(135deg, #1a2a6c, #b21f1f, #fdbb2d);
        min-height: 100vh;
        font-family: "Helvetica Neue", Arial, sans-serif;
      }
      .welcome-title {
        font-size: 3.5em;
        color: white;
        margin-bottom: 30px;
        text-shadow: 2px 2px 4px rgba(0,0,0,0.3);
        animation: fadeInDown 1s ease-out;
      }
      .description {
        font-size: 1.3em;
        color: white;
        max-width: 800px;
        margin: 0 auto 40px auto;
        line-height: 1.8;
        animation: fadeIn 1.5s ease-out;
      }
      .feature-box {
        background: rgba(255, 255, 255, 0.9);
        padding: 25px;
        margin: 15px;
        border-radius: 15px;
        box-shadow: 0 4px 15px rgba(0,0,0,0.2);
        transition: transform 0.3s ease, box-shadow 0.3s ease;
        width: 250px;
        cursor: pointer;
        animation: zoomIn 1s ease-out;
      }
      .feature-box:hover {
        transform: translateY(-10px);
        box-shadow: 0 6px 20px rgba(0,0,0,0.3);
      }
      .enter-button {
        background-color: #e74c3c;
        color: white;
        padding: 15px 40px;
        border: none;
        border-radius: 30px;
        font-size: 1.4em;
        cursor: pointer;
        transition: all 0.3s ease;
        margin-top: 30px;
        animation: pulse 2s infinite;
        box-shadow: 0 4px 15px rgba(0,0,0,0.2);
      }
      .enter-button:hover {
        background-color: #c0392b;
        transform: scale(1.05);
      }
      .feature-grid {
        display: flex;
        justify-content: center;
        flex-wrap: wrap;
        max-width: 1200px;
        margin: 0 auto 40px auto;
      }
      .feature-icon {
        font-size: 2.5em;
        color: #2c3e50;
        margin-bottom: 15px;
        transition: transform 0.3s ease;
      }
      .feature-box:hover .feature-icon {
        transform: scale(1.2);
      }
      .feature-title {
        color: #2c3e50;
        font-size: 1.4em;
        margin: 10px 0;
        font-weight: bold;
      }
      .feature-text {
        color: #34495e;
        font-size: 1.1em;
        line-height: 1.4;
      }
      .stats-container {
        display: flex;
        justify-content: center;
        margin: 40px auto;
        max-width: 1000px;
        animation: fadeInUp 1s ease-out;
      }
      .stat-box {
        background: rgba(255, 255, 255, 0.9);
        padding: 20px;
        margin: 0 15px;
        border-radius: 10px;
        text-align: center;
        min-width: 200px;
      }
      .stat-number {
        font-size: 2em;
        color: #2c3e50;
        font-weight: bold;
      }
      .stat-label {
        color: #34495e;
        margin-top: 5px;
        font-size: 1.1em;
      }
      @keyframes pulse {
        0% { transform: scale(1); }
        50% { transform: scale(1.05); }
        100% { transform: scale(1); }
      }
      .scroll-down {
        position: absolute;
        bottom: 20px;
        left: 50%;
        transform: translateX(-50%);
        color: white;
        font-size: 2em;
        animation: bounce 2s infinite;
        cursor: pointer;
      }
      @keyframes bounce {
        0%, 20%, 50%, 80%, 100% { transform: translateY(0); }
        40% { transform: translateY(-30px); }
        60% { transform: translateY(-15px); }
      }
    '))
    ),
    
    # Navigation state
    div(id = "home-page",
        class = "home-page",
        h1(class = "welcome-title", "Real Estate Analytics"),
        p(class = "description",
          "Discover the power of data-driven real estate decisions with our comprehensive analytics platform. 
        Explore property trends, price patterns, and market insights to make informed investments in Bangalore's 
        dynamic real estate market."),
        
        # Quick Stats
        div(class = "stats-container",
            div(class = "stat-box",
                div(class = "stat-number", textOutput("totalLocations")),
                div(class = "stat-label", "Locations")
            ),
            div(class = "stat-box",
                div(class = "stat-number", textOutput("avgPropertyPrice")),
                div(class = "stat-label", "Avg. Price")
            ),
            div(class = "stat-box",
                div(class = "stat-number", textOutput("totalProperties")),
                div(class = "stat-label", "Properties")
            )
        ),
        
        # Interactive Feature boxes
        div(class = "feature-grid",
            div(class = "feature-box", id = "analysis-box",
                div(class = "feature-icon", icon("chart-line")),
                h3(class = "feature-title", "Market Analysis"),
                p(class = "feature-text", "Interactive visualizations of price trends and property distributions")
            ),
            div(class = "feature-box", id = "location-box",
                div(class = "feature-icon", icon("map-marker-alt")),
                h3(class = "feature-title", "Location Insights"),
                p(class = "feature-text", "Detailed analysis by location and property type")
            ),
            div(class = "feature-box", id = "data-box",
                div(class = "feature-icon", icon("table")),
                h3(class = "feature-title", "Data Explorer"),
                p(class = "feature-text", "Advanced filtering and sorting capabilities")
            ),
            div(class = "feature-box", id = "price-box",
                div(class = "feature-icon", icon("calculator")),
                h3(class = "feature-title", "Price Analytics"),
                p(class = "feature-text", "Comprehensive price per square foot analysis")
            )
        ),
        
        actionButton("start_login", "Get Started", class = "enter-button"),
        
        # Scroll down indicator
        div(class = "scroll-down",
            icon("chevron-down"))
    ),
    
    # Main dashboard UI (initially hidden)
    hidden(
      div(id = "main-dashboard",
          style = "display: none;",
          dashboardPage(
            dashboardHeader(
              title = "Real Estate Dashboard", 
              tags$li(
                class = "dropdown",
                actionButton(
                  "logout_btn", 
                  "Logout", 
                  icon = icon("sign-out-alt"),
                  style = "margin-top: 8px; margin-right: 10px; background-color: #e74c3c; color: white;"
                )
              )
            ),
            
            dashboardSidebar(
              sidebarMenu(
                menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
                menuItem("Detailed Analysis", tabName = "analysis", icon = icon("chart-line")),
                menuItem("Data Explorer", tabName = "data", icon = icon("table")),
                
                
                selectInput("location", "Select Location:", 
                            choices = sort(unique(filtered_df$location)), 
                            selected = unique(filtered_df$location)[1]),
                
                sliderInput("priceRange", "Price Range (Lakhs):", 
                            min = min(filtered_df$price, na.rm = TRUE),
                            max = max(filtered_df$price, na.rm = TRUE),
                            value = c(min(filtered_df$price, na.rm = TRUE), 
                                      max(filtered_df$price, na.rm = TRUE))),
                
                selectInput("area_type", "Area Type:", 
                            choices = unique(filtered_df$area_type),
                            selected = unique(filtered_df$area_type)[1])
              )
            ),
            
            dashboardBody(
              tags$head(
                tags$style(HTML('
                .skin-blue .main-header .logo { background-color: #2c3e50; }
                .skin-blue .main-header .navbar { background-color: #2c3e50; }
                .skin-blue .main-sidebar { background-color: #34495e; }
                .content-wrapper { background-color: #ecf0f1; }
                .box { border-top-color: #2c3e50; }
              '))
              ),
              
              tabItems(
                # Overview Tab
                tabItem(tabName = "overview",
                        fluidRow(
                          valueBoxOutput("avgPrice", width = 4),
                          valueBoxOutput("totalProperties", width = 4),
                          valueBoxOutput("pricePerSqft", width = 4)
                        ),
                        fluidRow(
                          box(plotlyOutput("priceDist"), width = 6, title = "Price Distribution"),
                          box(plotlyOutput("priceVsSqft"), width = 6, title = "Price vs Total Sqft")
                        ),
                        fluidRow(
                          box(plotlyOutput("bathBalconyDist"), width = 12, title = "Bath and Balcony Distribution")
                        )
                ),
                
                # Analysis Tab
                tabItem(tabName = "analysis",
                        fluidRow(
                          box(plotlyOutput("areaTypeBox"), width = 12, title = "Price Distribution by Area Type")
                        ),
                        fluidRow(
                          box(plotlyOutput("societyPrice"), width = 6, title = "Average Price by Society"),
                          box(plotlyOutput("sizePrice"), width = 6, title = "Price vs Size")
                        )
                ),
                
                # Data Tab
                tabItem(tabName = "data",
                        box(
                          title = "Property Details",
                          width = 12,
                          DTOutput("propertyTable")
                        )
                )
              )
            )
          )
      )
    )
  )
}
# New combined UI
ui <- function() {
  fluidPage(
    useShinyjs(),
    
    # Add custom CSS for login/signup pages
    tags$head(
      tags$script("
        Shiny.addCustomMessageHandler('redirectToGoogle', function(authUrl) {
          window.location.href = authUrl;
        });
      "),
      tags$style(HTML('
        .btn-link {
          background: none;
          border: none;
          color: #007bff;
          text-decoration: underline;
          padding: 0;
        }
        .btn-link:hover {
          color: #0056b3;
          text-decoration: underline;
        }
      '))
    ),
    uiOutput("currentPage")
  )
}

## Google OAuth
# Add these Google authentication functions
setup_google_auth <- function() {
  options(googleAuthR.scopes.selected = c(
    "https://www.googleapis.com/auth/userinfo.email",
    "https://www.googleapis.com/auth/userinfo.profile"
  ))
  
  # Replace with your OAuth credentials
  options(googleAuthR.webapp.client_id =google_client_id,
          googleAuthR.webapp.client_secret =google_client_secret,
          googleAuthR.webapp.redirect_uri =google_redirect_uri)
  # Configure for same window authentication
  # options(googleAuthR.webapp.use_basic_auth = TRUE,
  #         googleAuthR.webapp.redirect_on_signout = TRUE,
  #         googleAuthR.webapp.popup = FALSE)
}

get_google_user_info <- function(access_token) {
  # Get user info from Google using the access token directly in the header
  response <- GET(
    "https://www.googleapis.com/oauth2/v1/userinfo",
    add_headers(Authorization = paste("Bearer", access_token))
  )
  
  if (status_code(response) == 200) {
    user_info <- fromJSON(rawToChar(response$content))
    return(user_info)
  }
  return(NULL)
}

# Handle the OAuth code exchange and user authentication
handle_google_auth <- function(code, session) {
  # Exchange the authorization code for an access token
  token_response <- POST(
    "https://oauth2.googleapis.com/token",
    body = list(
      code = code,
      client_id = google_client_id,
      client_secret = google_client_secret,
      redirect_uri = google_redirect_uri,
      grant_type = "authorization_code"
    ),
    encode = "form"
  )
  
  if (status_code(token_response) == 200) {
    token_data <- fromJSON(rawToChar(token_response$content))
    access_token <- token_data$access_token
    
    # Get user info using the access token
    user_info <- get_google_user_info(access_token)
    
    if (!is.null(user_info)) {
      # Connect to database
      con <- dbConnect(RSQLite::SQLite(), "users.db")
      
      # Check if user exists
      existing_user <- dbGetQuery(con, sprintf(
        "SELECT * FROM users WHERE google_id = '%s' OR email = '%s'",
        user_info$id, user_info$email
      ))
      
      if (nrow(existing_user) == 0) {
        # Create new user
        dbExecute(con, 
                  "INSERT INTO users (username, email, google_id, google_email, profile_picture) 
           VALUES (?, ?, ?, ?, ?)",
                  params = list(
                    user_info$email,
                    user_info$email,
                    user_info$id,
                    user_info$email, 
                    user_info$picture
                  )
        )
        
        # Get the newly created user
        user <- dbGetQuery(con, sprintf(
          "SELECT * FROM users WHERE google_id = '%s'",
          user_info$id
        ))
      } else {
        # Update existing user's Google info
        dbExecute(con,
                  "UPDATE users SET 
           google_id = ?, 
           google_email = ?, 
           profile_picture = ? 
           WHERE email = ?",
                  params = list(
                    user_info$id,
                    user_info$email,
                    user_info$picture,
                    user_info$email
                  )
        )
        user <- existing_user
      }
      
      dbDisconnect(con)
      
      # Return user data
      return(list(
        username = user_info$email,
        email = user_info$email,
        profile_picture = user_info$picture
      ))
    }
  }
  return(NULL)
}


# Server logic
server <- function(input, output, session) {
  
  # Initialize page state and user data
  appState <- reactiveVal("home")
  user_data <- reactiveVal(NULL)
  
  # Setup Google Authentication
  setup_google_auth()
  
  # Then in the server function, add a logout observer
  observeEvent(input$logout_btn, {
    # Clear user data
    user_data(NULL)
    
    # Reset app state to landing page
    appState("login")
    
    # Optional: Show a logout notification
    showNotification("You have been logged out.", type = "message")
  })
  
  # Handle Google Sign-In button click
  observeEvent(input$google_signin, {
    # Create OAuth URL
    auth_url <- paste0(
      "https://accounts.google.com/o/oauth2/v2/auth?",
      "client_id=", google_client_id,
      "&redirect_uri=", URLencode("http://localhost:6740", reserved = TRUE),
      "&response_type=code",
      "&scope=", URLencode("email profile openid https://www.googleapis.com/auth/userinfo.profile https://www.googleapis.com/auth/userinfo.email", reserved = TRUE)
    )
    
    # Redirect to Google login
    session$sendCustomMessage("redirectToGoogle", auth_url)
  })
  
  # Handle the OAuth callback observer
  observe({
    query <- parseQueryString(session$clientData$url_search)
    
    if (!is.null(query$code)) {
      # Handle the authorization code
      user <- handle_google_auth(query$code, session)
      
      if (!is.null(user)) {
        user_data(user)
        appState("main-dashboard")
      } else {
        output$login_message <- renderText("Failed to authenticate with Google")
        appState("login")
      }
    }
  })
  
  
  # Handle page transitions
  observeEvent(input$go_to_signup, {
    appState("signup")
  })
  
  observeEvent(input$go_to_login, {
    appState("login")
  })
  
  # Handle signup
  observeEvent(input$signup_btn, {
    req(input$signup_username, input$signup_password, 
        input$signup_confirm_password, input$signup_email)
    
    if (input$signup_password != input$signup_confirm_password) {
      output$signup_message <- renderText("Passwords do not match!")
      return()
    }
    
    tryCatch({
      con <- dbConnect(RSQLite::SQLite(), "users.db")
      
      # Check if username or email already exists
      existing_user <- dbGetQuery(con, sprintf(
        "SELECT * FROM users WHERE username = '%s' OR email = '%s'",
        input$signup_username, input$signup_email))
      
      if (nrow(existing_user) > 0) {
        output$signup_message <- renderText(
          "Username or email already exists!")
        dbDisconnect(con)
        return()
      }
      
      # Insert new user
      password_hash <- sodium::password_store(input$signup_password)
      dbExecute(con, 
                "INSERT INTO users (username, password, email) VALUES (?, ?, ?)",
                list(input$signup_username, password_hash, input$signup_email))
      
      dbDisconnect(con)
      output$signup_message <- renderText("Sign up successful! Please login.")
      appState("login")
      
    }, error = function(e) {
      output$signup_message <- renderText("An error occurred during signup.")
    })
  })
  
  # Handle login
  observeEvent(input$login_btn, {
    req(input$login_username, input$login_password)
    
    tryCatch({
      con <- dbConnect(RSQLite::SQLite(), "users.db")
      user <- dbGetQuery(con, sprintf(
        "SELECT * FROM users WHERE username = '%s'", 
        input$login_username))
      dbDisconnect(con)
      
      if (nrow(user) == 1 && 
          sodium::password_verify(user$password, input$login_password)) {
        user_data(list(username = user$username, email = user$email))
        appState("main-dashboard")
      } else {
        output$login_message <- renderText("Invalid username or password!")
      }
      
    }, error = function(e) {
      output$login_message <- renderText("An error occurred during login.")
    })
  })
  
  # Render the appropriate page based on state
  output$currentPage <- renderUI({
    switch(appState(),
           "home" = dashboardUI(),
           "login" = loginPageUI(),
           "signup" = signupPageUI(),
           "main-dashboard" = {
             if (!is.null(user_data())) {
               # Here's where you'd load your actual main dashboard
               div(id = "main-dashboard",
                   dashboardPage(
                     dashboardHeader(
                       title = "Real Estate Dashboard", 
                       tags$li(
                         class = "dropdown",
                         actionButton(
                           "logout_btn", 
                           "Logout", 
                           icon = icon("sign-out-alt"),
                           style = "margin-top: 8px; margin-right: 10px; background-color: #e74c3c; color: white;"
                         )
                       )
                     ),
                     
                     dashboardSidebar(
                       sidebarMenu(
                         menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
                         menuItem("Detailed Analysis", tabName = "analysis", icon = icon("chart-line")),
                         menuItem("Data Explorer", tabName = "data", icon = icon("table")),
                         
                         
                         selectInput("location", "Select Location:", 
                                     choices = sort(unique(filtered_df$location)), 
                                     selected = unique(filtered_df$location)[1]),
                         
                         sliderInput("priceRange", "Price Range (Lakhs):", 
                                     min = min(filtered_df$price, na.rm = TRUE),
                                     max = max(filtered_df$price, na.rm = TRUE),
                                     value = c(min(filtered_df$price, na.rm = TRUE), 
                                               max(filtered_df$price, na.rm = TRUE))),
                         
                         selectInput("area_type", "Area Type:", 
                                     choices = unique(filtered_df$area_type),
                                     selected = unique(filtered_df$area_type)[1])
                       )
                     ),
                     
                     dashboardBody(
                       tags$head(
                         tags$style(HTML('
                .skin-blue .main-header .logo { background-color: #2c3e50; }
                .skin-blue .main-header .navbar { background-color: #2c3e50; }
                .skin-blue .main-sidebar { background-color: #34495e; }
                .content-wrapper { background-color: #ecf0f1; }
                .box { border-top-color: #2c3e50; }
              '))
                       ),
                       
                       tabItems(
                         # Overview Tab
                         tabItem(tabName = "overview",
                                 fluidRow(
                                   valueBoxOutput("avgPrice", width = 4),
                                   valueBoxOutput("totalProperties", width = 4),
                                   valueBoxOutput("pricePerSqft", width = 4)
                                 ),
                                 fluidRow(
                                   box(plotlyOutput("priceDist"), width = 6, title = "Price Distribution"),
                                   box(plotlyOutput("priceVsSqft"), width = 6, title = "Price vs Total Sqft")
                                 ),
                                 fluidRow(
                                   box(plotlyOutput("bathBalconyDist"), width = 12, title = "Bath and Balcony Distribution")
                                 )
                         ),
                         
                         # Analysis Tab
                         tabItem(tabName = "analysis",
                                 fluidRow(
                                   box(plotlyOutput("areaTypeBox"), width = 12, title = "Price Distribution by Area Type")
                                 ),
                                 fluidRow(
                                   box(plotlyOutput("societyPrice"), width = 6, title = "Average Price by Society"),
                                   box(plotlyOutput("sizePrice"), width = 6, title = "Price vs Size")
                                 )
                         ),
                         
                         # Data Tab
                         tabItem(tabName = "data",
                                 box(
                                   title = "Property Details",
                                   width = 12,
                                   DTOutput("propertyTable")
                                 )
                         )
                       )
                     )
                   )
               )
             } else {
               loginPageUI()
             }
           }
    )
  })
  
  # Add this helper function at the start of your server function
  format_currency <- function(x) {
    ifelse(is.na(x) || !is.finite(x), 
           "N/A", 
           paste0("₹ ", format(round(as.numeric(x), 2), big.mark = ","), "L"))
  }
  
  # Loading screen
  w <- Waiter$new(id = "main-dashboard")
  
  # Add an observer to handle navigation from home to login
  observeEvent(input$start_login, {
    appState("login")
  })
  
  # Navigation handling
  
  
  observeEvent(input$home, {
    # Return to home page
    appState("home")
  })
  
  # Quick stats for home page
  output$totalLocations <- renderText({
    length(unique(filtered_df$location))
  })
  
  output$avgPropertyPrice <- renderText({
    paste0("₹", round(mean(filtered_df$price, na.rm = TRUE), 1), "L")
  })
  
  output$totalProperties <- renderText({
    format(nrow(filtered_df), big.mark = ",")
  })
  
  # Update the filteredData reactive in your server function
  filteredData <- reactive({
    req(input$location, input$priceRange, input$area_type)
    
    tryCatch({
      filtered_df %>%
        dplyr::filter(
          location == input$location,
          price >= as.numeric(input$priceRange[1]),
          price <= as.numeric(input$priceRange[2]),
          area_type == input$area_type
        )
    }, error = function(e) {
      print(paste("Error in filteredData:", e$message))
      return(data.frame())  # Return empty dataframe on error
    })
  })
  
  # Value Boxes
  output$avgPrice <- renderValueBox({
    req(filteredData())
    avg_price <- tryCatch({
      mean(as.numeric(filteredData()$price), na.rm = TRUE)
    }, error = function(e) NA)
    
    valueBox(
      format_currency(avg_price),
      "Average Price",
      icon = icon("indian-rupee-sign"),
      color = "blue"
    )
  })
  
  output$totalProperties <- renderValueBox({
    req(filteredData())
    total_props <- nrow(filteredData())
    
    valueBox(
      format(total_props, big.mark = ","),
      "Total Properties",
      icon = icon("building"),
      color = "green"
    )
  })
  
  # Replace your entire output$pricePerSqft in the server function with this:
  output$pricePerSqft <- renderValueBox({
    req(filteredData())
    
    # Safe calculation of price per sqft
    price_per_sqft <- tryCatch({
      df <- filteredData()
      prices <- as.numeric(as.character(df$price))
      sqft <- as.numeric(as.character(df$total_sqft))
      mean(prices / sqft, na.rm = TRUE)
    }, error = function(e) {
      return(NA)
    })
    
    # Handle NA or invalid results
    if(is.na(price_per_sqft) || !is.finite(price_per_sqft)) {
      display_value <- "N/A"
    } else {
      display_value <- paste0("₹ ", round(price_per_sqft, 2), "/sq.ft")
    }
    
    valueBox(
      display_value,
      "Avg Price per Sqft",
      icon = icon("calculator"),
      color = "purple"
    )
  })
  
  # Price Distribution Plot
  output$priceDist <- renderPlotly({
    req(filteredData())
    p <- ggplot(filteredData(), aes(x = price)) +
      geom_histogram(bins = 30, fill = "#3498db", color = "#2980b9") +
      labs(x = "Price (Lakhs)", y = "Count") +
      theme_minimal()
    
    ggplotly(p) %>%
      layout(hovermode = "closest") %>%
      event_register("plotly_hover")
  })
  
  # Price vs Total Sqft Plot
  output$priceVsSqft <- renderPlotly({
    req(filteredData())
    p <- ggplot(filteredData(), aes(x = total_sqft, y = price)) +
      geom_point(aes(color = area_type), alpha = 0.6) +
      geom_smooth(method = "lm", color = "#c0392b", se = FALSE) +
      labs(x = "Total Square Footage", y = "Price (Lakhs)") +
      theme_minimal()
    
    ggplotly(p) %>%
      layout(hovermode = "closest") %>%
      event_register("plotly_hover")
  })
  
  # Bath and Balcony Distribution
  output$bathBalconyDist <- renderPlotly({
    req(filteredData())
    p <- ggplot(filteredData()) +
      geom_bar(aes(x = factor(bath), fill = "Bathrooms"), 
               position = "dodge", alpha = 0.7) +
      geom_bar(aes(x = factor(balcony), fill = "Balconies"), 
               position = "dodge", alpha = 0.5) +
      scale_fill_manual(values = c("Bathrooms" = "#e67e22", "Balconies" = "#9b59b6")) +
      labs(x = "Count", y = "Frequency", fill = "Type") +
      theme_minimal()
    
    ggplotly(p) %>%
      layout(hovermode = "closest") %>%
      event_register("plotly_hover")
  })
  
  # Area Type Box Plot
  output$areaTypeBox <- renderPlotly({
    req(filteredData())
    p <- ggplot(filteredData(), aes(x = area_type, y = price, fill = area_type)) +
      geom_boxplot() +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = "Area Type", y = "Price (Lakhs)") +
      scale_fill_brewer(palette = "Set3")
    
    ggplotly(p) %>%
      layout(hovermode = "closest") %>%
      event_register("plotly_hover")
  })
  
  # Society Price Plot
  output$societyPrice <- renderPlotly({
    req(filteredData())
    avg_price_society <- filteredData() %>%
      group_by(society) %>%
      summarise(avg_price = mean(price, na.rm = TRUE)) %>%
      arrange(desc(avg_price)) %>%
      head(10)
    
    p <- ggplot(avg_price_society, aes(x = reorder(society, avg_price), y = avg_price)) +
      geom_bar(stat = "identity", fill = "#2ecc71") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = "Society", y = "Average Price (Lakhs)")
    
    ggplotly(p) %>%
      layout(hovermode = "closest") %>%
      event_register("plotly_hover")
  })
  
  # Size vs Price Plot
  output$sizePrice <- renderPlotly({
    req(filteredData())
    p <- ggplot(filteredData(), aes(x = factor(size), y = price)) +
      geom_boxplot(fill = "#3498db") +
      theme_minimal() +
      labs(x = "Size (BHK)", y = "Price (Lakhs)")
    
    ggplotly(p) %>%
      layout(hovermode = "closest") %>%
      event_register("plotly_hover")
  })
  
  # Data Table
  output$propertyTable <- renderDT({
    req(filteredData())
    datatable(
      filteredData(),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      ),
      rownames = FALSE
    )
  })
}


# Run the Shiny app
shinyApp(ui = ui, server = server)