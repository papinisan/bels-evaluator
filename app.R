library(shiny)

# Define the UI
ui <- fluidPage(
  
  titlePanel("Brief Exposure Learning Studies (BELS) Design Evaluator"),
  
  sidebarLayout(
    sidebarPanel(
      width = 7,
      
      # Brought back the Objective question, limiting choices to only those that impact recommendations
      checkboxGroupInput("q1", 
                         "What are the primary objectives of this study? (Select all that apply, or leave blank if none apply)",
                         choices = c("Exploring a new area of study or testing novel procedures." = "A",
                                     "Forward translation (building on prior analogue studies toward clinical trials)." = "B",
                                     "Directly investigating under what circumstances analogue vs. clinical samples are most informative." = "C")),
      
      checkboxGroupInput("q2", 
                         "What type of sample do you plan to recruit? (Select all that apply)",
                         choices = c("Analogue sample (nonclinical or subclinical symptoms)" = "A",
                                     "Clinical sample (non-treatment-seeking)" = "B",
                                     "Clinical sample (treatment-seeking)" = "C")),
      
      # Conditional: Only show if Analogue (A) is selected in Q2
      conditionalPanel(
        condition = "input.q2 && input.q2.indexOf('A') > -1",
        checkboxGroupInput("q3", 
                           "How will participants in your analogue sample be selected? (Select all that apply)",
                           choices = c("Systematically selected based on elevated symptoms related to the fear of interest." = "A",
                                       "Screened using an established measure with an empirically derived, meaningful cut score." = "B",
                                       "Convenience / general population without specific symptom cut-off criteria." = "C"))
      ),
      
      # Conditional: Only show if Clinical (B) or Treatment-seeking (C) is selected in Q2
      conditionalPanel(
        condition = "input.q2 && (input.q2.indexOf('B') > -1 || input.q2.indexOf('C') > -1)",
        radioButtons("q4", 
                     "How will clinical or treatment-seeking participants be recruited?",
                     choices = c("Recruited specifically and solely for this BELS study." = "A",
                                 "Co-recruited / embedded within another ongoing study (e.g., a larger psychopathology study or full clinical treatment trial)." = "B"),
                     selected = character(0))
      ),
      
      radioButtons("q5", 
                   "How will financial compensation be structured for participants?",
                   choices = c("Tied directly to assessment sessions (e.g., completing baseline/follow-up measures)." = "A",
                               "Tied directly to completion of the exposure interventions." = "B",
                               "A single flat fee paid only upon complete study finish." = "C"),
                   selected = character(0)),
      
      checkboxGroupInput("q6", 
                         "What are the primary statistical tests/hypotheses for your study? (Select all that apply)",
                         choices = c("Group differences / Main effects only." = "A",
                                     "Moderation analyses (e.g., interaction terms, baseline moderators)." = "B",
                                     "Mediation analyses (e.g., indirect mechanisms, change pathways)." = "C")),
      
      radioButtons("q7", 
                   "How was your target sample size determined?",
                   choices = c("A priori power analysis specifically calculated for moderation or mediation effects." = "A",
                               "A priori power analysis calculated for main effects only." = "B",
                               "Rule of thumb / resource availability / feasibility constraints." = "C"),
                   selected = character(0))
    ),
    
    # Main panel to display the split recommendations
    mainPanel(
      width = 5,
      h3("Study Design Evaluation"),
      hr(),
      uiOutput("recommendations")
    )
  )
)

# Define the Server logic
server <- function(input, output) {
  
  output$recommendations <- renderUI({
    
    # 1. Fetch Inputs
    q1 <- input$q1 # Objectives (Optional, can be null)
    q2 <- input$q2 # Sample type
    q3 <- input$q3 # Analogue selection
    q4 <- input$q4 # Clinical recruitment
    q5 <- input$q5 # Compensation
    q6 <- input$q6 # Stats tests
    q7 <- input$q7 # Power analysis
    
    # 2. Check if the form is fully complete
    # Base questions must be answered (q1 is optional)
    base_complete <- !is.null(q2) && !is.null(q5) && !is.null(q6) && !is.null(q7)
    
    # Conditional questions must be answered IF they are triggered
    q3_complete <- !("A" %in% q2) || !is.null(q3)
    q4_complete <- !(any(c("B", "C") %in% q2)) || !is.null(q4)
    
    is_complete <- base_complete && q3_complete && q4_complete
    
    # If not complete, stop here and ask them to finish
    if (!is_complete) {
      return(p(em("Please complete all the required questions to generate your evaluation.")))
    }
    
    # 3. Generate Recommendations into two separate lists
    in_line <- list()
    needs_update <- list()
    
    # ---------------------------------------------------------
    # Logic for R1: Systematically-selected analogue samples
    # Applicable if: Objective is 'New area' OR they are using an Analogue sample
    if ("A" %in% q1 || "A" %in% q2) {
      r1_text <- p(tags$b("(R1)"), " Strongly consider the use of systematically-selected analogue samples for BELS, especially when exploring new areas of study.", style = "margin-bottom: 15px;")
      
      # In line if: they are using an analogue sample AND selecting systematically/screened (not convenience)
      if ("A" %in% q2 && !("C" %in% q3)) {
        in_line[[length(in_line) + 1]] <- r1_text
      } else {
        needs_update[[length(needs_update) + 1]] <- r1_text
      }
    }
    
    # ---------------------------------------------------------
    # Logic for R2: Clinical/treatment-seeking samples
    # Applicable if: Objective is 'Forward translation' OR they are using a Clinical sample
    if ("B" %in% q1 || any(c("B", "C") %in% q2)) {
      r2_text <- p(tags$b("(R2)"), " Use clinical, and, if possible, treatment-seeking samples for BELS, particularly when the intent of the study is to build on studies with analogue samples for forward translation toward clinical trials. Such studies may only be practical when these participants are recruited for another study...", style = "margin-bottom: 15px;")
      
      # In line if: they are using a clinical sample AND they are co-recruiting them (not solely for BELS)
      if (any(c("B", "C") %in% q2) && !is.null(q4) && q4 == "B") {
        in_line[[length(in_line) + 1]] <- r2_text
      } else {
        needs_update[[length(needs_update) + 1]] <- r2_text
      }
    }
    
    # ---------------------------------------------------------
    # Logic for R3: Financial incentives
    # Always applicable
    r3_text <- p(tags$b("(R3)"), " Tie financial incentives for BELS to assessments rather than intervention completion such that participant report of symptoms is unlikely to be contaminated by desire to be in the study.", style = "margin-bottom: 15px;")
    if (q5 == "A") {
      in_line[[length(in_line) + 1]] <- r3_text
    } else {
      needs_update[[length(needs_update) + 1]] <- r3_text
    }
    
    # ---------------------------------------------------------
    # Logic for R4: Comparing samples
    # Applicable if: Objective is 'Compare samples' OR they selected both Analogue and Clinical
    if ("C" %in% q1 || ("A" %in% q2 && any(c("B", "C") %in% q2))) {
      r4_text <- p(tags$b("(R4)"), " A high priority target of investigation is under what circumstances analogue samples and clinical samples are most useful in BELS.", style = "margin-bottom: 15px;")
      
      # In line if: they actually have both samples to compare
      if ("A" %in% q2 && any(c("B", "C") %in% q2)) {
        in_line[[length(in_line) + 1]] <- r4_text
      } else {
        needs_update[[length(needs_update) + 1]] <- r4_text
      }
    }
    
    # ---------------------------------------------------------
    # Logic for R5: Powering for moderation/mediation
    # Always applicable
    r5_text <- p(tags$b("(R5)"), " BELS should be powered to detect plausible effects based on a priori power analyses. If hypotheses include mediation or moderation, sample size should be based on powering those tests and not merely group differences.", style = "margin-bottom: 15px;")
    plans_mod_med <- any(c("B", "C") %in% q6)
    
    if (q7 == "C" || (plans_mod_med && q7 != "A")) {
      # Needs update if they used a rule of thumb, OR they plan mod/med but didn't explicitly power for it
      needs_update[[length(needs_update) + 1]] <- r5_text
    } else {
      in_line[[length(in_line) + 1]] <- r5_text
    }
    
    # 4. Format Output UI
    output_ui <- tagList()
    
    # Add "In Line" section if there are matches
    if (length(in_line) > 0) {
      output_ui <- tagAppendChildren(output_ui, 
                                     h4("✅ Your study is in line with these recommendations:", style = "color: #2e7d32; margin-bottom: 15px;"),
                                     tagList(in_line),
                                     hr())
    }
    
    # Add "Needs Update" section if there are mismatches
    if (length(needs_update) > 0) {
      output_ui <- tagAppendChildren(output_ui, 
                                     h4("⚠️ Consider updating your design to be in line with these recommendations:", style = "color: #c62828; margin-bottom: 15px;"),
                                     tagList(needs_update))
    } else {
      output_ui <- tagAppendChildren(output_ui, 
                                     p(tags$b("Fantastic!"), " Your study design perfectly aligns with all triggered BELS methodological recommendations."))
    }
    
    return(output_ui)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)