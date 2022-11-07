# ---------- #
# --- UI --- #
# ---------- #

table_ui <- function(id){
  
  fluidPage(
    title = "",
    br(),
    br(),
    tags$script(
      'document.getElementById("table").addEventListener("click", function(event){
                             setTimeout(function(){
                             console.log("rerender")
                                        HTMLWidgets.staticRender()
                             }, 10);
                          })
                           '
    ),
    strong("NBA 2020-21 Regular Season Coach's Challenge Database", style = "font-size:30px;font-family:Georgia; color:black"),
    br(),
    strong("Version: 2022-11-05", style = "font-size:15px;font-family:Georgia; color:red"),
    p(
      "Table: @mattabolanos | Data: nbastatR, inpredicatable, nba.com |",
      a("Spinner", href = "https://media.tenor.com/_u-gDFZQuIQAAAAM/basketball-sports.gif", target =
          "_blank"),
      style = "font-size:16px;font-family:Karla; color:black;"
    ),
    fluidRow(
      column(
        2, 
        downloadButton(
          NS(id, 'down'),
          'Download Data',
          class = "down_btn"
        )
      ),
      column(
        3, 
        offset = 0,
        uiOutput(NS(id, "garbage"))
      )
    ),
    br(),
    fluidRow(
      column(
        12,
        reactableOutput(NS(id, "table")) %>% 
          withSpinner(image = "loading.gif", image.width = "150px", image.height = "150px")
      )
    )
  )
  
}

# -------------- #
# --- server --- #
# -------------- #

table_server <- function(id){
  
  moduleServer(id, function(input, output, session){
    
    # Garbage switch
    output$garbage <- renderUI({
      
      materialSwitch(
        session$ns("garb_sel"),
        label = "Filter Out Garbage Time",
        value = TRUE,
        inline = TRUE
      )
      
    })
    
    # download button
    output$down <- downloadHandler(
      
      filename = "nba_coach_challenge.csv",
      content = function(file) {
        
        write.csv(csv_export_dat, file, row.names = FALSE)
        
      }
      
    )
    
    
    observeEvent(input$garb_sel, {
      
      if (input$garb_sel) {
        
        table_data <- challs %>%
          filter(
            !(garb == T & quarter == 4)
          )
        
        # Render table
        output$table <- renderReactable({
          
          create_chall_table(table_data)
          
        })
        
      }else {
        
        table_data <- challs
        
        # Render table
        output$table <- renderReactable({
          
          create_chall_table(table_data)
          
        })
        
      }
      
      
      
    })
    
    
    
  })
  
}
