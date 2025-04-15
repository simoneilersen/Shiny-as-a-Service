library(shiny)
library(auth0)
library(RStripe)
library(shinyjs)
library(httr)
library(jsonlite)

# UI
ui <- tagList(
  
  fluidPage(
    
    title = "My Shiny SaaS",
    
    useShinyjs(),
    
    titlePanel(
      "My Shiny SaaS" # App title
    ),
    
    sidebarPanel(
      width = 3,
      tabsetPanel(
        id = "sidebar_tabs",
        tabPanel(
          "Your SaaS", 
          
          br(),
          
          helpText("Button for subscription purchase:"),
          
          fluidRow(align = "center",
                   uiOutput("check_sub")),
          
          hr(),
          
          helpText("Button for product purchase"),
          
          fluidRow(align = "center",
                   uiOutput("check_prod")),
          
        ),
        tabPanel(
          "Your account", 
          br(),
          uiOutput("display_sub_status"),
          
          hr(), 
          
          div(class = "centered-content", uiOutput("manage_sub")),
          tags$div(
            style = "margin-bottom: 10px; margin-top: 20px; text-align: center;",
            actionButton("logout", "Log out", icon = icon("right-from-bracket"), style = "font-size: 12px; background-color: red; border-color: red; color: white; size: 5px;
                                         box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);")
          )
        )
      )
    ),
    mainPanel(
      tabsetPanel(tabPanel("Subscription", uiOutput("subscription_status_panel")),
                  tabPanel("Product", uiOutput("product_status_panel"))))
  ))


# Server

server <- function(input, output, session) {
  
  # Check if a product is purchased. We need the invoices to check for that.
  check_product_payment <- function(customer_id, product_id) {
    # Get all invoices directly within the function
    invoices <- stripe_list_invoices(api_key)
    
    # Filter invoices for this customer
    customer_rows <- which(invoices$data$customer == customer_id)
    
    if (length(customer_rows) == 0) {
      return(list(
        has_paid = FALSE,
        message = "No invoices found for this customer"
      ))
    }
    
    # For each customer invoice, check its status
    for (row in customer_rows) {
      # Simply check the status
      if (invoices$data$status[row] == "paid") {
        return(list(
          has_paid = TRUE,
          invoice_id = invoices$data$id[row],
          message = "Customer has paid"
        ))
      } 
      else if (invoices$data$status[row] == "draft") {
        return(list(
          has_paid = FALSE,
          invoice_id = invoices$data$id[row],
          message = "Customer has not paid (draft invoice)"
        ))
      }
    }
    
    # If we get here, no paid invoices were found
    return(list(
      has_paid = FALSE,
      message = "No paid invoices found"
    ))
  }
  
  # Check for subcription status of the logged in customer
  sub <- reactive({
    
    stripeid <- session$userData$auth0_info$`stripeid/app_metadata`$stripe_customer_id
    
    subscriptions <- stripe_list_subscriptions(api_key = stripe_api_key,
                                               customer_id = stripeid)
    
    # Check if the subscription data is NULL or empty
    if (is.null(subscriptions$data) || length(subscriptions$data) == 0) {
      return("non-subscriber")
    }
    
    # Extract subscription status
    sub <- subscriptions$data$status
    
    # For testing purposes:
    # Uncomment to override subscription status
    #sub <- "non-subscriber"
    #sub <- "active"
    
    return(sub)
    
  })
  
  # Checks for purchase of the product from the customer
  prod <- reactive({
    
    # Get customer ID from session
    stripeid <- session$userData$auth0_info$`stripeid/app_metadata`$stripe_customer_id
    
    # Get product ID from environment
    product_id <- product_id_from_stripe
    
    # Check if customer has paid for the product
    payment_status <- check_product_payment(stripeid, product_id)
    
    return(payment_status)
    
  })
  
  output$display_sub_status <- renderUI({
    
    stripeid <- session$userData$auth0_info$`stripeid/app_metadata`$stripe_customer_id
    
    # Initialize subscriptions and trial_message variables
    subscriptions <- NULL
    trial_message <- "No subscription information available"
    
    # Only proceed with Stripe API calls if we have a valid stripeid
    if (!is.null(stripeid) && nchar(stripeid) > 0) {
      subscriptions <- stripe_list_subscriptions(api_key = stripe_api_key,
                                                 customer_id = stripeid)
      
      # Check if there are subscriptions and extract trial_end
      if (!is.null(subscriptions$data) && length(subscriptions$data$trial_end) > 0) {
        trial_end <- as.POSIXct(subscriptions$data$trial_end, origin = "1970-01-01", tz = "UTC")
        trial_end <- format(trial_end, "%d-%m-%Y")
        trial_message <- paste0("Your trial ends: ", trial_end)
      } else {
        # If trial_end is NULL, check if stripeid is in invoices$data$customer
        invoices <- stripe_list_invoices(api_key = stripe_api_key)
        
        if (!is.null(invoices$data) && stripeid %in% invoices$data$customer) {
          trial_message <- "Customer has already had a trial period."
        } else {
          trial_message <- "Customer has not yet had a trial period."
        }
      }
    }
    
    # Get subscription status, with safer fallback
    subscription_status <- tryCatch({
      status <- sub()
      if (is.null(status) || is.na(status)) "unknown" else status
    }, error = function(e) {
      "unknown"
    })
    
    # For your own sake to debug
    print(trial_message)
    
    fluidRow(
      align = "center",
      div(
        style = "display: inline-block; text-align: center; position: relative;",
        span(
          paste0("Your subscription: ", subscription_status),
          style = "position: relative; 
                    color: black; 
                    font-family: 'Demica Mono Pro', monospace; 
                    padding: 10px; 
                    font-size: 12px; 
                    background: transparent; 
                    display: inline-block; 
                    text-align: center;
                    line-height: 1.2;
                    z-index: 1;",
          div(
            style = "position: absolute; 
                        bottom: 0; 
                        left: 50%; 
                        transform: translateX(-50%);
                        width: 90%; 
                        height: 30%;
                        margin-bottom: 7px;
                        background-color: rgba(245, 110, 15, 0.3); 
                        z-index: -1;"
          )
        )
      ),
      # Only show trial message if we have a valid subscription status that isn't "non-subscriber"
      if (!is.null(subscription_status) && 
          !is.na(subscription_status) && 
          subscription_status != "non-subscriber" && 
          subscription_status != "unknown") {
        div(
          trial_message,
          style = "font-size: 11px; color: grey; margin-top: 7px; margin-bottom: 2px;"
        )
      }
    )
  })
  
  # Activate the customer portal so they can update billing details, fetch invoices and cancel subscriptions
  output$manage_sub <- renderUI({
    
    div(
      style = "text-align: center; margin-top: 10px; margin-bottom: 10px;",
      actionButton(
        "manage_sub", 
        label = tagList(
          icon("arrow-right"),
          HTML("Go to Customer Portal"),
          div(
            paste0("(Manage or cancel subscription. Update payment & billing)"),
            style = "font-size: 11px; color: white; margin-top: 7px; margin-bottom: 2px; color: lightgrey;"
          )
        ),
        class = "btn btn-success",
        style = "background-color: #004000; border-color: #004000;
                              font-size: 12px;
                              box-shadow: 0 4px 8px rgba(0, 60, 0, 1); padding: 6px 14px;
                              margin-bottom: 5px;"
      )
    )
  })
  
  # Renders the purchase product button depending on the status of the customer's purchase
  output$check_prod <- renderUI({
    # Check if the user has paid
    if (!is.null(prod()) && prod()$has_paid == TRUE) {
      # If user has paid, show a thank you message
      div(
        style = "text-align: center; margin-top: 20px; margin-bottom: 5px;",
        div(
          icon("check-circle", class = "fa-2x", style = "color: #004000;"),
          style = "margin-bottom: 10px;"
        ),
        div(
          "Thank you for your purchase!",
          style = "font-size: 16px; font-weight: bold; color: #004000; margin-bottom: 8px;"
        ),
        div(
          "You have full access to all premium features",
          style = "font-size: 12px; color: #666;"
        )
      )
    } else {
      # If user has NOT paid, show the "Get instant access" button
      div(
        style = "text-align: center; margin-top: 20px; margin-bottom: 5px;",
        actionButton(
          "buy_prod", 
          label = tagList(
            icon("bolt"),
            HTML("Get instant access")
          ),
          class = "btn btn-success",
          style = "background-color: #004000; border-color: #004000;
                  font-size: 12px;
                  box-shadow: 0 4px 8px rgba(0, 60, 0, 1); padding: 6px 14px;
                  margin-bottom: 5px;"
        )
      )
    }
  })
  
  # Renders the purchase product button depending on the status of the customer's subcscription. Looks a bit long, but there are different status for trial and active. Customers turn into active after a trial, so check the invoices as well.  
  output$check_sub <- renderUI({
    
    # Get the subscription status
    subscription_status <- sub()
    
    # Print for debugging
    print(subscription_status)
    
    # Check if subscription_status exists and is one of the expected values
    if (!is.null(subscription_status) && 
        length(subscription_status) > 0 && 
        subscription_status %in% c("trialing", "active")) {
      # User is in trial or has an active subscription
      div(
        style = "display: inline-block; text-align: center; position: relative; margin-bottom: -10px;",
        span(
          icon("bolt"),
          paste0("Welcome to PRO version!"),
          style = "position: relative; 
                       color: black; 
                       font-family: 'Demica Mono Pro', monospace; 
                       padding: 10px; 
                       font-size: 12px; 
                       margin-top: 10px;
                       background: transparent; 
                       display: inline-block; 
                       text-align: center;
                       line-height: 1.2;
                       z-index: 1;",
          div(
            style = "position: absolute; 
                           bottom: 0; 
                           left: 50%; 
                           transform: translateX(-50%);
                           width: 90%; 
                           height: 30%;
                           margin-bottom: 7px;
                           background-color: rgba(245, 110, 15, 0.3); 
                           z-index: -1;"
          )
        )
      )
    } else {
      
      # Safely check for stripe_customer_id
      stripeid <- NULL
      if (!is.null(session$userData$auth0_info) && 
          !is.null(session$userData$auth0_info$`stripeid/app_metadata`) &&
          !is.null(session$userData$auth0_info$`stripeid/app_metadata`$stripe_customer_id)) {
        stripeid <- session$userData$auth0_info$`stripeid/app_metadata`$stripe_customer_id
      }
      
      # Safely get invoices
      has_invoice <- FALSE
      tryCatch({
        if (!is.null(stripeid)) {
          invoices <- stripe_list_invoices(api_key = stripe_api_key)
          has_invoice <- !is.null(invoices$data) && stripeid %in% invoices$data$customer
        }
      }, error = function(e) {
        print(paste("Error checking invoices:", e$message))
      })
      
      action_label <- if (has_invoice) {
        "Upgrade to PRO"
      } else {
        "Try PRO version 30 days for free"
      }
      
      div(
        style = "text-align: center; margin-top: 20px; margin-bottom: 5px;",
        actionButton(
          "buy_sub", 
          label = tagList(
            icon("bolt"),
            HTML(action_label)
          ),
          class = "btn btn-success",
          style = "background-color: #004000; border-color: #004000;
                      font-size: 12px;
                      box-shadow: 0 4px 8px rgba(0, 60, 0, 1); padding: 6px 14px;
                      margin-bottom: 5px;"
        ),
        div(
          HTML('(Cancel any time. <span style="background-color: #f56e2d; color: white; padding: 2px 4px; border-radius: 3px;">Save €60</span> with annual billing.)'),
          style = "font-size: 11px; color: grey; margin-top: 7px; margin-bottom: 2px;"
        )
      )
    }
  })
  
  # Display the content depending on subsciption status
  output$subscription_status_panel <- renderUI({
    
    status <- sub()
    
    if (status %in% c("trialing", "active")) {
      # User has active subscription or trial
      div(
        style = "background-color: #e8f5e9; border-left: 4px solid #4caf50; padding: 15px; margin-bottom: 20px; border-radius: 3px;",
        h4(
          icon("check-circle"), 
          "Premium Access Confirmed", 
          style = "color: #2e7d32; margin-top: 0; margin-bottom: 10px;"
        ),
        verbatimTextOutput("subscription_details"),
        style = "width: 100%;"
      )
    } else {
      # User does not have active subscription
      div(
        style = "background-color: #ffebee; border-left: 4px solid #f44336; padding: 15px; margin-bottom: 20px; border-radius: 3px;",
        h4(
          icon("exclamation-triangle"), 
          "Premium Access Required", 
          style = "color: #c62828; margin-top: 0; margin-bottom: 10px;"
        ),
        verbatimTextOutput("subscription_details"),
        style = "width: 100%;"
      )
    }
  })
  
  # Display the content depending on purchase status
  output$product_status_panel <- renderUI({
    
    status <- prod()
    
    if (!is.null(status) && status$has_paid == TRUE) {
      # User has paid for premium access
      div(
        style = "background-color: #e8f5e9; border-left: 4px solid #4caf50; padding: 15px; margin-bottom: 20px; border-radius: 3px;",
        h4(
          icon("check-circle"), 
          "Thank you for your purchase!", 
          style = "color: #2e7d32; margin-top: 0; margin-bottom: 10px;"
        ),
        p(
          "You have access to all premium content and features in this product.",
          style = "margin-bottom: 15px; font-size: 16px;"
        ),
        div(
          style = "background-color: white; border-radius: 5px; padding: 15px; border: 1px solid #e0e0e0;",
          h5(
            "Premium Content",
            style = "color: #2e7d32; margin-top: 0; border-bottom: 1px solid #e0e0e0; padding-bottom: 8px;"
          ),
          p("Here you can see all the premium content that is included with your purchase. Enjoy all the advanced features and benefits of our product.")
        ),
        style = "width: 100%;"
      )
    } else {
      # User has not paid for premium access
      div(
        style = "background-color: #ffebee; border-left: 4px solid #f44336; padding: 15px; margin-bottom: 20px; border-radius: 3px;",
        h4(
          icon("exclamation-triangle"), 
          "Premium Access Required", 
          style = "color: #c62828; margin-top: 0; margin-bottom: 10px;"
        ),
        p(
          "You have not yet purchased this product and do not have access to this content.",
          style = "margin-bottom: 15px; font-size: 16px;"
        ),
        div(
          style = "background-color: white; border-radius: 5px; padding: 15px; border: 1px solid #e0e0e0;",
          h5(
            "Premium Content Preview",
            style = "color: #c62828; margin-top: 0; border-bottom: 1px solid #e0e0e0; padding-bottom: 8px;"
          ),
          p("This section contains exclusive features and content that will be available after purchase. Get instant access to unlock all these premium benefits.")
        ),
        style = "width: 100%;"
      )
    }
  })
  
  # Show the customer what his status in terms of subscription is - and when the trial might end.
  output$subscription_details <- renderText({
    
    status <- sub()
    
    # Get trial end date information
    trialEndInfo <- NULL
    if (status == "trialing") {
      stripeid <- session$userData$auth0_info$`stripeid/app_metadata`$stripe_customer_id
      subscriptions <- stripe_list_subscriptions(api_key = stripe_api_key,
                                                 customer_id = stripeid)
      
      if (!is.null(subscriptions$data) && length(subscriptions$data$trial_end) > 0) {
        trial_end <- as.POSIXct(subscriptions$data$trial_end, origin = "1970-01-01", tz = "UTC")
        trialEndInfo <- format(trial_end, "%d-%m-%Y")
      }
    }
    
    if (status %in% c("trialing", "active")) {
      # Details for active subscribers
      if (status == "trialing") {
        trialMessage <- if (!is.null(trialEndInfo)) {
          paste0("✓ Your trial is active until: ", trialEndInfo)
        } else {
          "✓ Your trial is active"
        }
        
        return(paste0(
          trialMessage, 
          "\n✓ All premium features are enabled",
          "\n✓ Full access to all data and analytics"
        ))
      } else {
        return("✓ Your subscription is active\n✓ All premium features are enabled\n✓ Full access to all data and analytics\n✓ Priority customer support")
      }
    } else {
      # Message for non-subscribers
      return("✗ You do not have permission to access premium features\n✗ Limited functionality available\n✗ Upgrade to PRO to unlock all features")
    }
  })
  
  # Look for customer's clicking on the customer portal button
  observeEvent(input$manage_sub, {
    # Get the customer portal URL from environment variables
    portal_url <- url_to_customer_portal
    
    # Create JavaScript code with the environment variable
    js_code <- sprintf("window.open('%s', '_blank');", portal_url)
    
    # Run the JavaScript
    runjs(js_code)
  })
  
  # Observer for one-time product purchase
  observeEvent(input$buy_prod, {
    
    # Function to update customer name in Stripe
    update_customer_name <- function(api_key, customer_id, name) {
      url <- paste0("https://api.stripe.com/v1/customers/", customer_id)
      
      response <- PATCH(
        url = url,
        add_headers(
          Authorization = paste("Bearer", api_key),
          "Content-Type" = "application/x-www-form-urlencoded"
        ),
        body = list(name = name),
        encode = "form"
      )
      
      content <- content(response, "text")
      parsed <- fromJSON(content)
      
      return(parsed)
    }
    
    create_checkout_session <- function(api_key, price_id, customer_id, customer_email) {
      url <- "https://api.stripe.com/v1/checkout/sessions"
      
      # Properly format the line_items array for Stripe's API
      body <- list(
        `line_items[0][price]` = price_id,
        `line_items[0][quantity]` = 1,
        customer = customer_id,
        receipt_email = customer_email,
        mode = "payment",
        success_url = url_to_your_success_page,
        cancel_url = url_to_your_cancel_page,
        `payment_method_types[0]` = "card",
        allow_promotion_codes = "true",
        billing_address_collection = "auto",
        
        # Enable invoice creation
        `invoice_creation[enabled]` = "true",
        `invoice_creation[invoice_data][description]` = "Shiny as a Service",
        `invoice_creation[invoice_data][footer]` = "Thank you for your business!"
      )
      
      response <- POST(
        url = url,
        add_headers(
          Authorization = paste("Bearer", api_key),
          "Content-Type" = "application/x-www-form-urlencoded"
        ),
        body = body,
        encode = "form"
      )
      
      content <- content(response, "text")
      parsed <- fromJSON(content)
      
      return(parsed)
    }
    
    # Get configuration values
    api_key <- stripe_api_key
    price_id <- pricing_id # Price ID for one-time purchase
    customer_id <- session$userData$auth0_info$`stripeid/app_metadata`$stripe_customer_id
    customer_email <- session$userData$auth0_info$email # Get email from session data
    
    # Get user name from input field (if it exists)
    if(!is.null(input$billing_name) && input$billing_name != "") {
      user_name <- input$billing_name
      # Update customer name in Stripe
      update_customer_name(api_key, customer_id, user_name)
    }
    
    # Create checkout session and get response
    checkout_response <- create_checkout_session(api_key, price_id, customer_id, customer_email)
    
    # Extract the URL from the response
    checkout_url <- checkout_response$url
    
    # Open Stripe checkout link in a new tab
    shinyjs::runjs(sprintf("window.open('%s', '_blank');", checkout_url))
  })
  
  # Observer for subscription purchase with free trial
  observeEvent(input$buy_sub, {
    
    # Function to update customer name in Stripe
    update_customer_name <- function(api_key, customer_id, name) {
      url <- paste0("https://api.stripe.com/v1/customers/", customer_id)
      
      response <- PATCH(
        url = url,
        add_headers(
          Authorization = paste("Bearer", api_key),
          "Content-Type" = "application/x-www-form-urlencoded"
        ),
        body = list(name = name),
        encode = "form"
      )
      
      content <- content(response, "text")
      parsed <- fromJSON(content)
      
      return(parsed)
    }
    
    create_checkout_session <- function(api_key, price_id, customer_id) {
      url <- "https://api.stripe.com/v1/checkout/sessions"
      
      # Body parameters with trial period
      body <- list(
        `line_items[0][price]` = price_id,
        `line_items[0][quantity]` = 1,
        customer = customer_id,
        mode = "subscription",
        success_url = url_to_your_success_page,
        cancel_url = url_to_your_cancel_page,
        `payment_method_types[0]` = "card",
        allow_promotion_codes = "true",
        billing_address_collection = "auto",
        
        # Set the trial period duration (30 days)
        `subscription_data[trial_period_days]` = 30
      )
      
      response <- POST(
        url = url,
        add_headers(
          Authorization = paste("Bearer", api_key),
          "Content-Type" = "application/x-www-form-urlencoded"
        ),
        body = body,
        encode = "form"
      )
      
      content <- content(response, "text")
      parsed <- fromJSON(content)
      
      return(parsed)
    }
    
    # Get configuration values
    api_key <- stripe_api_key
    price_id <- pricing_id_for_subscription
    customer_id <- session$userData$auth0_info$`stripeid/app_metadata`$stripe_customer_id
    
    # Create checkout session with trial
    checkout_response <- create_checkout_session(api_key, price_id, customer_id)
    
    # Extract the URL from the response
    if (!is.null(checkout_response$url)) {
      checkout_url <- checkout_response$url
      
      # Open Stripe checkout link in a new tab
      shinyjs::runjs(sprintf("window.open('%s', '_blank');", checkout_url))
    } else {
      # Handle error case
      print("Error creating checkout session:")
      print(checkout_response)
    }
  })
  
  # Log out your customer. 
  observeEvent(input$logout, {
    auth0::logout()
  })
  
}


# Run the application with Auth0 authentication. Rememeber to "Run External" before firing up your app!
auth0::shinyAppAuth0(ui, server)