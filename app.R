library(shiny)

# Define the UI
ui <- fluidPage(
  tags$head(
    tags$script(HTML("
      window.goatcounter = {
        endpoint: 'https://bels-evaluator.goatcounter.com/count',
        allow_local: true
      };
    ")),
    tags$script(async = TRUE, src = "https://gc.zgo.at/count.js")
  ),
  
  titlePanel("Brief Exposure Learning Studies (BELS) Design Evaluator"),
  
  sidebarLayout(
    sidebarPanel(
      width = 7,
      
      h4("1. Study Objectives & Sample"),
      checkboxGroupInput("q1", 
                         "What are the primary objectives of this study? (Select all that apply)",
                         choices = c("Exploring a new area of study or testing novel procedures." = "A",
                                     "Forward translation (building on prior analogue studies toward clinical trials)." = "B",
                                     "Directly investigating under what circumstances analogue vs. clinical samples are most informative." = "C")),
      
      checkboxGroupInput("q2", 
                         "What type of sample do you plan to recruit? (Select all that apply)",
                         choices = c("Analogue sample (nonclinical or subclinical symptoms)" = "A",
                                     "Clinical sample (non-treatment-seeking)" = "B",
                                     "Clinical sample (treatment-seeking)" = "C")),
      
      conditionalPanel(
        condition = "input.q2 && input.q2.indexOf('A') > -1",
        checkboxGroupInput("q3", 
                           "How will participants in your analogue sample be selected? (Select all that apply)",
                           choices = c("Systematically selected based on elevated symptoms related to the fear of interest." = "A",
                                       "Screened using an established measure with an empirically derived, meaningful cut score." = "B",
                                       "Convenience / general population without specific symptom cut-off criteria." = "C"))
      ),
      
      conditionalPanel(
        condition = "input.q2 && (input.q2.indexOf('B') > -1 || input.q2.indexOf('C') > -1)",
        radioButtons("q4", 
                     "How will clinical or treatment-seeking participants be recruited?",
                     choices = c("Recruited specifically and solely for this BELS study." = "A",
                                 "Co-recruited / embedded within another ongoing study (e.g., a larger psychopathology study or full clinical treatment trial)." = "B"),
                     selected = character(0))
      ),
      
      hr(),
      h4("2. Design & Statistics"),
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
                   selected = character(0)),
      
      hr(),
      h4("3. Exposure Procedures"),
      checkboxGroupInput("q8", 
                         "Which of the following are true about your planned exposure procedures? (Select all that apply)",
                         choices = c("Study includes at least one exposure session." = "r5",
                                     "Uses standard exposure formats for the condition." = "r6",
                                     "Uses multiple exposure formats." = "r7",
                                     "Delivery system offers gradations, varied contexts, and adequate immersion." = "r8",
                                     "Standardized tracking of exposure type, intensity, AND context." = "r9_10",
                                     "Includes a brief rationale / psychoeducation." = "r13",
                                     "Includes non-exposure elements (e.g., cognitive restructuring) NOT being manipulated." = "r14_fail")),
      
      radioButtons("q9", 
                   "Will you include between-session exposure practice (homework)?",
                   choices = c("Assigned and tracked in real-time and/or via weekly metrics." = "A",
                               "Assigned, but not systematically tracked." = "B",
                               "No between-session practice is planned." = "C"),
                   selected = character(0)),
      
      hr(),
      h4("4. Measurement Modality & Validity"),
      checkboxGroupInput("q10", 
                         "How are you structuring your measurements? (Select all that apply)",
                         choices = c("Measures use multiple items when possible." = "r15",
                                     "Single-item measures are obtained frequently." = "r16",
                                     "Includes common symptom measures at baseline and over time." = "r20",
                                     "Outcomes are assessed using multiple modalities." = "r21",
                                     "Measurement tasks might interfere with the intended dose of exposure." = "r22_fail")),
      
      checkboxGroupInput("q11", 
                         "How are you addressing reliability and construct validity? (Select all that apply)",
                         choices = c("Reliability of meaningful units will be calculated ahead of testing hypotheses." = "r17_18",
                                     "Includes distinct/divergent constructs to test validity." = "r23_24",
                                     "Constructs drawn explicitly from theory, including competing mediators." = "r25_26")),
      
      hr(),
      h4("5. Translation & Open Science"),
      checkboxGroupInput("q12", 
                         "Which of the following apply to your study's translation and open science practices? (Select all that apply)",
                         choices = c("Study incorporates thorough understanding of relevant animal research." = "r27",
                                     "Includes a replication of previous BELS results." = "r28",
                                     "Uses open science practices (preregistration, open data, open methods, reporting all measures)." = "r29"))
    ),
    
    # Main panel to display the split recommendations
    mainPanel(
      width = 5,
      h3("Study Design Evaluation"),
      hr(),
      uiOutput("recommendations"),
      
      # Custom Visitor Counter Display
      tags$div(
        style = "margin-top: 30px; padding-top: 15px; border-top: 1px solid #ddd; font-weight: bold; color: #444;",
        tags$p(id = "visitor-count", "BELS Evaluator Users: ...")
      ),
      
      # Script to fetch count and prepend your custom label
      tags$script(HTML("
        fetch('https://bels-evaluator.goatcounter.com/counter/TOTAL.json')
          .then(response => {
            if (!response.ok) throw new Error('Counter disabled or restricted');
            return response.json();
          })
          .then(data => {
            document.getElementById('visitor-count').innerText = 'BELS Evaluator Users: ' + data.count;
          })
          .catch(err => {
            console.log('GoatCounter error:', err);
          });
      "))
    )
  )
)

# Define the Server logic
server <- function(input, output) {
  
  # Helper function to generate section UI
  build_section_ui <- function(title, in_line, needs_update) {
    if (length(in_line) == 0 && length(needs_update) == 0) return(NULL)
    
    res <- tagList(h4(title, style = "color: #333; margin-top: 20px; border-bottom: 1px solid #ccc; padding-bottom: 5px;"))
    
    if (length(in_line) > 0) {
      res <- tagAppendChildren(res, 
                               h5("✅ Your study is in line with these recommendations:", style = "color: #2e7d32; margin-top: 15px; font-weight: bold;"),
                               tagList(in_line))
    }
    
    if (length(needs_update) > 0) {
      res <- tagAppendChildren(res, 
                               h5("⚠️ Consider updating your design to be in line with these recommendations:", style = "color: #c62828; margin-top: 15px; font-weight: bold;"),
                               tagList(needs_update))
    }
    return(res)
  }
  
  output$recommendations <- renderUI({
    
    # Fetch Inputs
    q1 <- input$q1 
    q2 <- input$q2 
    q3 <- input$q3 
    q4 <- input$q4 
    q5 <- input$q5 
    q6 <- input$q6 
    q7 <- input$q7 
    q8 <- input$q8
    q9 <- input$q9
    q10 <- input$q10
    q11 <- input$q11
    q12 <- input$q12
    
    # Section Completion Rules
    s1_complete <- !is.null(q2) && (!("A" %in% q2) || !is.null(q3)) && (!(any(c("B", "C") %in% q2)) || !is.null(q4))
    s2_complete <- !is.null(q5) && !is.null(q6) && !is.null(q7)
    s3_complete <- !is.null(q9)
    s4_complete <- s3_complete
    s5_complete <- s3_complete
    
    if (!s1_complete) {
      return(p(em("Please complete Section 1 (Study Objectives & Sample) to begin generating your evaluation.")))
    }
    
    # Section Lists
    s1_in <- list(); s1_up <- list()
    s2_in <- list(); s2_up <- list()
    s3_in <- list(); s3_up <- list()
    s4_in <- list(); s4_up <- list()
    s5_in <- list(); s5_up <- list()
    
    # ==========================
    # 1. Study Objectives & Sample
    # ==========================
    if ("A" %in% q1 || "A" %in% q2) {
      r1_text <- p(tags$b("(R1)"), " Strongly consider the use of systematically-selected analogue samples for BELS, especially when exploring new areas of study.", style = "margin-bottom: 10px;")
      if ("A" %in% q2 && !("C" %in% q3)) { s1_in[[length(s1_in) + 1]] <- r1_text } else { s1_up[[length(s1_up) + 1]] <- r1_text }
    }
    
    if ("B" %in% q1 || any(c("B", "C") %in% q2)) {
      r2_text <- p(tags$b("(R2)"), " Use clinical, and, if possible, treatment-seeking samples for BELS, particularly when the intent of the study is to build on studies with analogue samples for forward translation toward clinical trials. Such studies may only be practical when these participants are recruited for another study.", style = "margin-bottom: 10px;")
      if (any(c("B", "C") %in% q2) && !is.null(q4) && q4 == "B") { s1_in[[length(s1_in) + 1]] <- r2_text } else { s1_up[[length(s1_up) + 1]] <- r2_text }
    }
    
    if ("C" %in% q1 || ("A" %in% q2 && any(c("B", "C") %in% q2))) {
      r3_text <- p(tags$b("(R3)"), " A high priority target of investigation is under what circumstances analogue samples and clinical samples are most useful in BELS.", style = "margin-bottom: 10px;")
      if ("A" %in% q2 && any(c("B", "C") %in% q2)) { s1_in[[length(s1_in) + 1]] <- r3_text } else { s1_up[[length(s1_up) + 1]] <- r3_text }
    }
    
    # ==========================
    # 2. Design & Statistics
    # ==========================
    if (s2_complete) {
      r4_text <- p(tags$b("(R4)"), " BELS should be powered to detect plausible effects based on a priori power analyses. If hypotheses include mediation or moderation, sample size should be based on powering those tests and not merely group differences.", style = "margin-bottom: 10px;")
      plans_mod_med <- any(c("B", "C") %in% q6)
      if (q7 == "C" || (plans_mod_med && q7 != "A")) { s2_up[[length(s2_up) + 1]] <- r4_text } else { s2_in[[length(s2_in) + 1]] <- r4_text }
      
      inc_text <- p(tags$b("(Incentives)"), " Tie financial incentives for BELS to assessments rather than intervention completion such that participant report of symptoms is unlikely to be contaminated by desire to be in the study.", style = "margin-bottom: 10px;")
      if (q5 == "A") { s2_in[[length(s2_in) + 1]] <- inc_text } else { s2_up[[length(s2_up) + 1]] <- inc_text }
    }
    
    # ==========================
    # 3. Exposure Procedures
    # ==========================
    if (s3_complete) {
      r5_text <- p(tags$b("(R5)"), " BELS must contain at least one exposure, and two or more exposures are typically ideal. The number of exposures should not be so high as to risk ceiling effects.", style = "margin-bottom: 10px;")
      if (!is.null(q8) && "r5" %in% q8) { s3_in[[length(s3_in) + 1]] <- r5_text } else { s3_up[[length(s3_up) + 1]] <- r5_text }
      
      r6_text <- p(tags$b("(R6)"), " BELS should incorporate the standard exposure format for the condition under study, unless variation on exposure format is a topic of study.", style = "margin-bottom: 10px;")
      if (!is.null(q8) && "r6" %in% q8) { s3_in[[length(s3_in) + 1]] <- r6_text } else { s3_up[[length(s3_up) + 1]] <- r6_text }
      
      r7_text <- p(tags$b("(R7)"), " When the condition under study is commonly treated using multiple exposure formats, BELS should incorporate those formats if this comports with the proposed mechanism of action.", style = "margin-bottom: 10px;")
      if (!is.null(q8) && "r7" %in% q8) { s3_in[[length(s3_in) + 1]] <- r7_text } else { s3_up[[length(s3_up) + 1]] <- r7_text }
      
      r8_text <- p(tags$b("(R8)"), " The exposure delivery system used should offer gradations of intensity, varied contexts and stimuli, adequate immersion, and other features typical of in vivo exposure.", style = "margin-bottom: 10px;")
      if (!is.null(q8) && "r8" %in% q8) { s3_in[[length(s3_in) + 1]] <- r8_text } else { s3_up[[length(s3_up) + 1]] <- r8_text }
      
      r9_10_text <- p(tags$b("(R9 & R10)"), " A standardized way of selecting exposures and tracking what type, intensity, and context of exposures are employed should be used.", style = "margin-bottom: 10px;")
      if (!is.null(q8) && "r9_10" %in% q8) { s3_in[[length(s3_in) + 1]] <- r9_10_text } else { s3_up[[length(s3_up) + 1]] <- r9_10_text }
      
      r11_12_text <- p(tags$b("(R11 & R12)"), " Consider assigning between-session practice (i.e., homework), especially when maximizing external validity is a focus. If included, it would ideally be tracked in real time as well as via weekly summary metrics.", style = "margin-bottom: 10px;")
      if (q9 == "A") { s3_in[[length(s3_in) + 1]] <- r11_12_text } else { s3_up[[length(s3_up) + 1]] <- r11_12_text }
      
      r13_text <- p(tags$b("(R13)"), " BELS should include provision of a brief rationale and psychoeducation, because these are universal across exposure-based therapies.", style = "margin-bottom: 10px;")
      if (!is.null(q8) && "r13" %in% q8) { s3_in[[length(s3_in) + 1]] <- r13_text } else { s3_up[[length(s3_up) + 1]] <- r13_text }
      
      r14_text <- p(tags$b("(R14)"), " BELS should typically minimize inclusion of non-exposure elements that are not part of the components being manipulated.", style = "margin-bottom: 10px;")
      if (!is.null(q8) && "r14_fail" %in% q8) { s3_up[[length(s3_up) + 1]] <- r14_text } else { s3_in[[length(s3_in) + 1]] <- r14_text }
    }
    
    # ==========================
    # 4. Measurement Modality & Validity
    # ==========================
    if (s4_complete) {
      r15_text <- p(tags$b("(R15)"), " When possible, measures in BELS should include multiple items.", style = "margin-bottom: 10px;")
      if (!is.null(q10) && "r15" %in% q10) { s4_in[[length(s4_in) + 1]] <- r15_text } else { s4_up[[length(s4_up) + 1]] <- r15_text }
      
      r16_text <- p(tags$b("(R16)"), " When single-item measures are preferable... these should be obtained frequently enough that multiple ratings can be obtained for each eventual measure to be used.", style = "margin-bottom: 10px;")
      if (!is.null(q10) && "r16" %in% q10) { s4_in[[length(s4_in) + 1]] <- r16_text } else { s4_up[[length(s4_up) + 1]] <- r16_text }
      
      r17_18_text <- p(tags$b("(R17 & R18)"), " Reliability of all meaningful units in BELS should be calculated ahead of testing hypotheses. When good reliability has not been obtained, BELS should focus on how to obtain it.", style = "margin-bottom: 10px;")
      if (!is.null(q11) && "r17_18" %in% q11) { s4_in[[length(s4_in) + 1]] <- r17_18_text } else { s4_up[[length(s4_up) + 1]] <- r17_18_text }
      
      r20_text <- p(tags$b("(R20)"), " Measurement of outcomes in BELS should include common symptom measures both at baseline and over time.", style = "margin-bottom: 10px;")
      if (!is.null(q10) && "r20" %in% q10) { s4_in[[length(s4_in) + 1]] <- r20_text } else { s4_up[[length(s4_up) + 1]] <- r20_text }
      
      r21_text <- p(tags$b("(R21)"), " In most cases, measurement in BELS, especially of outcome, should be multimodal.", style = "margin-bottom: 10px;")
      if (!is.null(q10) && "r21" %in% q10) { s4_in[[length(s4_in) + 1]] <- r21_text } else { s4_up[[length(s4_up) + 1]] <- r21_text }
      
      r22_text <- p(tags$b("(R22)"), " The possibility that a measure might interfere with the intended dose of exposure (or other active ingredients) deserves careful attention in BELS.", style = "margin-bottom: 10px;")
      if (!is.null(q10) && "r22_fail" %in% q10) { s4_up[[length(s4_up) + 1]] <- r22_text } else { s4_in[[length(s4_in) + 1]] <- r22_text }
      
      r23_24_text <- p(tags$b("(R23 & R24)"), " Tests of validity should include whether changes move together with convergent measures and distinguish from divergent measures. Include measures expected to be distinct to avoid jingle/jangle fallacies.", style = "margin-bottom: 10px;")
      if (!is.null(q11) && "r23_24" %in% q11) { s4_in[[length(s4_in) + 1]] <- r23_24_text } else { s4_up[[length(s4_up) + 1]] <- r23_24_text }
      
      r25_26_text <- p(tags$b("(R19, R25, & R26)"), " Modality and constructs assessed in BELS should comport with and be drawn from the theory tested. Include all competing constructs (e.g., multiple mediators) if possible.", style = "margin-bottom: 10px;")
      if (!is.null(q11) && "r25_26" %in% q11) { s4_in[[length(s4_in) + 1]] <- r25_26_text } else { s4_up[[length(s4_up) + 1]] <- r25_26_text }
    }
    
    # ==========================
    # 5. Translation & Open Science
    # ==========================
    if (s5_complete) {
      r27_text <- p(tags$b("(R27)"), " When BELS draw in part from animal research, thorough understanding of this research will benefit design and interpretation of BELS.", style = "margin-bottom: 10px;")
      if (!is.null(q12) && "r27" %in% q12) { s5_in[[length(s5_in) + 1]] <- r27_text } else { s5_up[[length(s5_up) + 1]] <- r27_text }
      
      r28_text <- p(tags$b("(R28)"), " Replication of BELS results is essential.", style = "margin-bottom: 10px;")
      if (!is.null(q12) && "r28" %in% q12) { s5_in[[length(s5_in) + 1]] <- r28_text } else { s5_up[[length(s5_up) + 1]] <- r28_text }
      
      r29_text <- p(tags$b("(R29)"), " Open science practices should be used in conducting and reporting BELS, including pre-registration, open methods, reporting all measures used and all tests conducted, and open data.", style = "margin-bottom: 10px;")
      if (!is.null(q12) && "r29" %in% q12) { s5_in[[length(s5_in) + 1]] <- r29_text } else { s5_up[[length(s5_up) + 1]] <- r29_text }
    }
    
    # Compile Progressive UI
    output_ui <- tagList(
      build_section_ui("1. Study Objectives & Sample", s1_in, s1_up),
      if (s2_complete) build_section_ui("2. Design & Statistics", s2_in, s2_up) else p(em("Complete Section 2 to see evaluation for Design & Statistics...")),
      if (s3_complete) build_section_ui("3. Exposure Procedures", s3_in, s3_up) else p(em("Complete Section 3 to see evaluation for Exposure Procedures...")),
      if (s4_complete) build_section_ui("4. Measurement Modality & Validity", s4_in, s4_up),
      if (s5_complete) build_section_ui("5. Translation & Open Science", s5_in, s5_up)
    )
    
    return(output_ui)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)