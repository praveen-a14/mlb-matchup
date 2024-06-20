library(shiny)
library(baseballr)
library(ggplot2)
library(dplyr)
library(viridis)

pl <- baseballr::chadwick_player_lu()
pl_sel <- dplyr::select(pl, name_last, name_first, mlb_played_first, mlb_played_last)
pl_fil <- dplyr::filter(pl_sel, mlb_played_last >= 2015)
pl_mut <- dplyr::mutate(pl_fil, formatted_name = paste(name_last, ", ", name_first, " (", mlb_played_first, "-", mlb_played_last, ")", sep = ""))

# Define UI
ui <- fluidPage(
  titlePanel("Batter vs Pitcher Matchup Heatmaps"),
  sidebarLayout(
    sidebarPanel(
      selectInput("batter_name", "Select Batter", choices = c("", pl_mut$formatted_name)),
      selectInput("pitcher_name", "Select Pitcher", choices = c("", pl_mut$formatted_name)),
      dateInput("start_date", "Start Date"),
      dateInput("end_date", "End Date"),
      actionButton("plot_button", "Generate Heatmaps"),
      br(),
      textOutput("a_swings_note")
    ),
    mainPanel(
      uiOutput("tabs_ui")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  output$a_swings_note <- renderText({
    "Note: 'A' Swings are a hitter's top 20th percentile bat speed swings. 2024+ only"
  })
  
  matchup <- function(batter_first_name, batter_last_name, pitcher_first_name, pitcher_last_name, start, end){
    b_id <- baseballr::playerid_lookup(last_name = batter_last_name, first_name = batter_first_name)
    batter_id <- b_id$mlbam_id[!is.na(b_id$mlbam_id)][1]
    p_id <- baseballr::playerid_lookup(last_name = pitcher_last_name, first_name = pitcher_first_name)
    pitcher_id <- p_id$mlbam_id[!is.na(p_id$mlbam_id)][1]
    
    batter_year_list <- list()
    batter_start_year <- substr(start, 1, 4)
    batter_end_year <- substr(end, 1, 4)
    batter_start_year_int <- as.integer(batter_start_year)
    batter_end_year_int <- as.integer(batter_end_year)
    
    for(i in 1:((batter_end_year_int - batter_start_year_int) + 1)){
      begin <- batter_start_year_int + i - 1
      start_string <- paste(begin, "03-27", sep = "-")
      end_string <- paste(begin, "10-03", sep = "-")
      batter_data_year <- statcast_search(start_date = start_string, end_date = end_string, playerid = batter_id, player_type = "batter")
      batter_year_list[[i]] <- batter_data_year
    }
    
    batter_data <- NULL
    
    for(i in 1:length(batter_year_list)){
      if(is.null(batter_data)){
        batter_data <- batter_year_list[[i]]
      } else {
        batter_data <- rbind(batter_data, batter_year_list[[i]])
      }
    }
    
    pitcher_data <- baseballr::statcast_search(start_date = start, end_date = end, playerid = pitcher_id, player_type = "pitcher")
    pitcher_grouped <- dplyr::group_by(pitcher_data, pitch_type, p_throws)
    pitcher <- dplyr::summarize(pitcher_grouped, velo = mean(release_speed), rel_height = mean(release_pos_z),  iVB = mean(pfx_z), HB = mean(pfx_x))
    
    batter_selected <- dplyr::select(.data = batter_data, description, estimated_woba_using_speedangle, pitch_type, release_pos_z, release_speed, p_throws, plate_x, plate_z, pfx_z, pfx_x, launch_speed, bat_speed)
    batter_mutated <- dplyr::mutate(.data = batter_selected, estimated_woba_using_speedangle = ifelse(description %in% c("swinging_strike", "swinging_strike_blocked"), 0, estimated_woba_using_speedangle))
    batter <- dplyr::mutate(.data = batter_mutated, estimated_woba_using_speedangle = ifelse(description == "ball", NA, estimated_woba_using_speedangle))
    
    top80th <- quantile(batter$bat_speed, 0.80, na.rm = TRUE)
    
    fil_dfs <- list()
    plots <- list("Swing Rate" = list(),"Whiff Rate" = list(),  "Hard Hit" = list(), "A Swings" = list())
    
    for(pitch in pitcher$pitch_type){
      fil_df <- dplyr::filter(.data = batter, p_throws == pitcher$p_throws[[1]], description == "swinging_strike" | description == "foul" | description == "foul_tip" | description == "hit_into_play" | description == "swinging_strike_blocked", pitch_type == pitch)
      fil_dfs[[pitch]] <- fil_df
      
      plots[["Swing Rate"]][[pitch]] <- ggplot(fil_df, aes(x = plate_x, y = plate_z)) +
        stat_density_2d(aes(fill = ..density..), geom = 'raster', contour = 'FALSE') +
        scale_fill_viridis_c() +
        geom_rect(aes(xmin = -0.71, xmax = 0.71, ymin = 1.6, ymax = 3.5), fill = NA, color = "white") +
        xlim(-2, 2) + 
        ylim(1, 4) +
        theme_classic() +
        labs(x = "H Location", y = "V Location", 
             title = sprintf("%s %s's Swing rate vs %s %s type %ss (%d pitches)", 
                             batter_first_name, batter_last_name, 
                             pitcher_first_name, pitcher_last_name, 
                             pitch, nrow(fil_df)))
      
      filter_df <- dplyr::filter(.data = batter, p_throws == pitcher$p_throws[[1]], description == "swinging_strike"|description == "swinging_strike_blocked", pitch_type == pitch)
      
      plots[["Whiff Rate"]][[pitch]] <- ggplot(filter_df, aes(x = plate_x, y = plate_z)) +
        stat_density_2d(aes(fill = ..density..), geom = 'raster', contour = 'FALSE') +
        scale_fill_viridis_c() +
        geom_rect(aes(xmin = -0.71, xmax = 0.71, ymin = 1.6, ymax = 3.5), fill = NA, color = "white") +
        xlim(-2, 2) + 
        ylim(1, 4) +
        theme_classic() +
        labs(x = "H Location", y = "V Location", 
             title = sprintf("%s %s's Whiff rate vs %s %s type %ss (%d pitches)", 
                             batter_first_name, batter_last_name, 
                             pitcher_first_name, pitcher_last_name, 
                             pitch, nrow(filter_df)))
      
      filter_df <- dplyr::filter(.data = batter, p_throws == pitcher$p_throws[[1]], description == "hit_into_play", launch_speed >= 95, pitch_type == pitch)
      
      plots[["Hard Hit"]][[pitch]] <- ggplot(filter_df, aes(x = plate_x, y = plate_z)) +
        stat_density_2d(aes(fill = ..density..), geom = 'raster', contour = 'FALSE') +
        scale_fill_viridis_c() +
        geom_rect(aes(xmin = -0.71, xmax = 0.71, ymin = 1.6, ymax = 3.5), fill = NA, color = "white") +
        xlim(-2, 2) + 
        ylim(1, 4) +
        theme_classic() +
        labs(x = "H Location", y = "V Location", 
             title = sprintf("%s %s's HardHit rate vs %s %s type %ss (%d pitches)", 
                             batter_first_name, batter_last_name, 
                             pitcher_first_name, pitcher_last_name, 
                             pitch, nrow(filter_df)))
      
      filter_df <- dplyr::filter(.data = batter, p_throws == pitcher$p_throws[[1]], description == "swinging_strike" | description == "foul" | description == "foul_tip" | description == "hit_into_play" | description == "swinging_strike_blocked", is.na(bat_speed) == FALSE, bat_speed >= top80th, pitch_type == pitch)
      
      plots[["A Swings"]][[pitch]] <- ggplot(filter_df, aes(x = plate_x, y = plate_z)) +
        stat_density_2d(aes(fill = ..density..), geom = 'raster', contour = 'FALSE') +
        scale_fill_viridis_c() +
        geom_rect(aes(xmin = -0.71, xmax = 0.71, ymin = 1.6, ymax = 3.5), fill = NA, color = "white") +
        xlim(-2, 2) + 
        ylim(1, 4) +
        theme_classic() +
        labs(x = "H Location", y = "V Location", 
             title = sprintf("%s %s's A-Swings vs %s %s type %ss (%d pitches)", 
                             batter_first_name, batter_last_name, 
                             pitcher_first_name, pitcher_last_name, 
                             pitch, nrow(filter_df)))
    }
    
    return(plots)
  }
  
  observeEvent(input$plot_button, {
    batter_name <- input$batter_name
    pitcher_name <- input$pitcher_name
    start_date <- as.character(input$start_date)
    end_date <- as.character(input$end_date)
    
    extract_names <- function(full_name) {
      names <- strsplit(full_name, ", ")[[1]]
      first_name <- strsplit(names[2], " ")[[1]][1]
      last_name <- names[1]
      return(c(first_name, last_name))
    }
    
    batter_names <- extract_names(batter_name)
    pitcher_names <- extract_names(pitcher_name)
    
    batter_first_name <- batter_names[1]
    batter_last_name <- batter_names[2]
    
    pitcher_first_name <- pitcher_names[1]
    pitcher_last_name <- pitcher_names[2]
    
    plots <- matchup(batter_first_name, batter_last_name, pitcher_first_name, pitcher_last_name, start_date, end_date)
    
    output$tabs_ui <- renderUI({
      tabsetPanel(
        tabPanel("Swing Rate", 
                 uiOutput("swing_rate_ui")),
        tabPanel("Whiff Rate", 
                 uiOutput("whiff_rate_ui")),
        tabPanel("Hard Hit", 
                 uiOutput("hard_hit_ui")),
        tabPanel("A Swings", 
                 uiOutput("a_swings_ui"))
      )
    })
    
    output$swing_rate_ui <- renderUI({
      plot_output_list <- lapply(names(plots[["Swing Rate"]]), function(pitch) {
        plotname <- paste("swing_rate_plot", pitch, sep="_")
        plotOutput(plotname)
      })
      do.call(tagList, plot_output_list)
    })
    
    output$whiff_rate_ui <- renderUI({
      plot_output_list <- lapply(names(plots[["Whiff Rate"]]), function(pitch) {
        plotname <- paste("whiff_rate_plot", pitch, sep="_")
        plotOutput(plotname)
      })
      do.call(tagList, plot_output_list)
    })
    
    output$hard_hit_ui <- renderUI({
      plot_output_list <- lapply(names(plots[["Hard Hit"]]), function(pitch) {
        plotname <- paste("hard_hit_plot", pitch, sep="_")
        plotOutput(plotname)
      })
      do.call(tagList, plot_output_list)
    })
    
    output$a_swings_ui <- renderUI({
      plot_output_list <- lapply(names(plots[["A Swings"]]), function(pitch) {
        plotname <- paste("a_swings_plot", pitch, sep="_")
        plotOutput(plotname)
      })
      do.call(tagList, plot_output_list)
    })
    
    lapply(names(plots[["Swing Rate"]]), function(pitch) {
      output[[paste("swing_rate_plot", pitch, sep="_")]] <- renderPlot({ plots[["Swing Rate"]][[pitch]] })
    })
    
    lapply(names(plots[["Whiff Rate"]]), function(pitch) {
      output[[paste("whiff_rate_plot", pitch, sep="_")]] <- renderPlot({ plots[["Whiff Rate"]][[pitch]] })
    })
    
    lapply(names(plots[["Hard Hit"]]), function(pitch) {
      output[[paste("hard_hit_plot", pitch, sep="_")]] <- renderPlot({ plots[["Hard Hit"]][[pitch]] })
    })
    
    lapply(names(plots[["A Swings"]]), function(pitch) {
      output[[paste("a_swings_plot", pitch, sep="_")]] <- renderPlot({ plots[["A Swings"]][[pitch]] })
    })
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
