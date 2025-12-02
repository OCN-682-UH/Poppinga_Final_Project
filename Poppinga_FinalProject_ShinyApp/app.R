# This is a Shiny dashboard to visualize data collected by Malama Maunalua (https://www.malamamaunalua.org/) 

# LOAD LIBRARIES
library(shiny)
library(shinydashboard)
library(tidyverse)
library(here)
library(stringr)

# READ DATA
MMdata_3years_clean <-read_csv("Data/MMdata_3years_clean.csv")


# CLEAN THE DATA 
MMdata_3years_clean <- MMdata_3years_clean %>%
  select(-starts_with("...")) %>% # remove the first 2 columns that got messed up
  mutate(species_names = species %>% 
         str_replace_all("_", " ") %>%  # remove underscore and add space
         str_to_sentence()) # capitalize first letter only using stringr
#write.csv(MMdata_3years_clean, here("Poppinga_FinalProject_ShinyApp", "Data", "MMdata_3years_clean.csv"))
# names(MMdata_3years_clean)
#glimpse(MMdata_3years_clean)


# Plot 1 (Tab 2): Relative Percent Cover of Invasive vs Native Algae Communities
relative_cover_data<-MMdata_3years_clean %>% 
  mutate(hits = percent_cover) # species level data with category column and % cover == count
# Calculate relative algal percent cover b/c other way was giving us extremely high percentages
relative_pcover<-relative_cover_data %>% # total algal hits per survey
  group_by(plot_id, year) %>% 
  mutate(total_hits = sum(hits[community %in% c("Native", "Invasive")], # calculate relative percent cover
                          na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(community %in% c("Native", "Invasive")) %>% 
  group_by(plot_id, year, community) %>% 
  summarise(hits_community = sum(hits, na.rm = TRUE),
            total_hits = first(total_hits)) %>% 
  mutate(relative_cover = 100* hits_community / total_hits) # calculate 0-100%, native and invasive sums to 100



# Plot 2 (Tab 3): Species Mean Percent Cover Over Time
spp_pcover<-MMdata_3years_clean %>% 
  group_by(year, species, category) %>% # summmarise to one mean per year
  summarise(spp_mean_cover = mean(count, na.rm = TRUE))

species_choices_tbl <- MMdata_3years_clean %>% # easier for selection
  distinct(species, species_names) %>% # one row per species
  arrange(species)

species_choices <- setNames(species_choices_tbl$species,
                           species_choices_tbl$species_names)


# Plot 3 (Tab 4): Relative Abundance by Category 
MMdata_3years_clean$category<-factor(MMdata_3years_clean$category, # change order of categories
                                     levels=c("Green Algae", "Red Algae", "Brown Algae", "Cyanobacteria","Substrate", "Other" ))
relative_c_abundance<-MMdata_3years_clean %>% 
  filter(!is.na(category), # filter categories
         assessment_type != "cleared") %>% # drop these rows
  mutate(assessment = case_when(assessment_type %in% # combine assessment types (they are the same method)
                                  c("assessed", "huki", "quadrat_survey") ~
                                  "monthly_assessment", TRUE ~ assessment_type),
         assessment = factor(assessment, levels = c("before_huki", "after_huki", "monthly_assessment"))) # reorder

# Plot 4 (Tab 5): Regression Between Native and Invasive Species
regression_stuff<-MMdata_3years_clean %>% 
  group_by(plot_id, year, species) %>%  
  summarise(percent_cover = mean(percent_cover, na.rm = TRUE)) %>% # get means
  pivot_wider(names_from = species, # pivot wider
              values_from = percent_cover) %>% 
  drop_na()

invasives_tbl <-MMdata_3years_clean %>% # selecting only the invasives
  filter(community == "Invasive") %>% # only invasive rows
  distinct(species, species_names) %>%  # one row per species
  arrange(species)

invasive_choices <- setNames(invasives_tbl$species, # values returned to server
                             invasives_tbl$species_names) # labels shown in dropdown

natives_tbl <- MMdata_3years_clean %>% # selecting only the natives
  filter(community == "Native") %>% # only native rows
  distinct(species, species_names) %>% # one row per species
  arrange(species)

native_choices <- setNames(natives_tbl$species,
                        natives_tbl$species_names)



# MAKE THE SHINYAPP
# Notes:
# everything in server can be written just like R 
# everything in ui fluid page needs to have commas between functions


## UI for dashboard
ui <- #fluidPage(
  dashboardPage( # entire dashboard page
    skin = "blue", # color of the  dashboard
    
    dashboardHeader(  # dashboard header title
      title = "Mālama Maunalua Data", # title name
      titleWidth = 260), # width of the header title section
    
    dashboardSidebar( # side bar menu
      width = 260, # width to match title
      sidebarMenu( # menu
        menuItem("Home", tabName = "home", icon = icon("water")), # Home tab
        #menuItem("Sites", tabName = "sites", icon = icon("map")), # Sites tab (with map) *make this when have more time
        menuItem("Data Visualization", icon = icon("chart-line"), startExpanded = TRUE, # Data visualization, branches out
                 menuSubItem("Percent Cover of Algae Communities", tabName = "relative_cover_data"), # tab for relative percent cover by algae community plot
                 menuSubItem("Percent Cover by Species", tabName = "spp_mean_cover"), # tab for percent cover by species over time plot
                 menuSubItem("Relative Abundance", tabName = "relative_abundance"), # tab for relative abundance plot
                 menuSubItem("Regression", tabName = "regression") # tab for regression between native and invasive species
        )
      )
    ),
    
    dashboardBody( 
      tabItems( # content of each tab
        
        # Home Tab (Tab 1)
        # Description of Malama Maunalua + Photo
        tabItem(tabName = "home", # tab name, matches name given above in sidebar menu
                h2("Mālama Maunalua"), # title
                img(src="IMG_1474.jpeg", height="100%", width="100%", align = "center"), # image loaded in from ShinyApp folder
                h6("Image Source: Haley Poppinga"), # smaller text for image source
                br(), # line break
                tags$div( # paragraphs with information about MM and ShinyApp
                  tags$p("Mālama Maunalua (MM) is a non-profit organization that utilizes citizen science and community initiatives to restore and conserve the Maunalua Region of Oʻahu, Hawaiʻi from ridge to reef."),
                  tags$p("This webpage visualizes data collected in 2014, 2019, and 2024 in Maunalua Bay. 
                         This time-series dataset comes from over 12 years of monthtly algae quadrat surveys taken at our habitat restoration events (referred to as a 'huki').
                         A special mahalo to Ralph Dykes for handling this data since 2013 and to Alex Awo for making all of our conservation work happen.")
                ),
                br(), # line break
                img(src="mm_pic.jpg", height="100%", width="100%", align = "center"),
                h6("Image Source: Mālama Munalua Website"), # smaller text for image source
                br(), # line break
                tags$a(href="https://malamamaunalua.org/", "More information about Mālama Maunalua here.") # link to MM website
        ),
        
        # Sites tab content
        # Site map
       # tabItem(tabName = "sites", # tab name, matches name in sidebar menu
        #        h2("Maunalua Bay"), # title
        #        h4("Maunalua Bay surveys from 2014 to 2024."), # subtitle
        #        plotOutput("site_map", height = 600, width = 800) # map plot
       # ),
        
        
        # Percent Cover of Algae Communities for each year (Tab 2)
        # no user input here
        tabItem(tabName = "relative_cover_data", # tab name
                box(width = 11,
                  plotOutput("relative_cover_plot", height = 600) # community percent cover plot
                )
                ), 
        
        # Percent Cover by Species for 2014, 2019, 2024 (Tab 3)
        # Pick a species and plot mean cover over time (does facet by category just to show what type it is)
        tabItem(tabName = "spp_mean_cover", # tab name
                box( # each box of content on the page
                  width = 4, # box width: specify width of box (total page width = 12)
                  selectInput("select_spp1", # input for picking a species (matching data)
                              label = h3("Select a Species"), # title of the dropdown box
                            #  choices = setNames(names(species_names), species_names) # species options to choose from, matches dataset
                              choices = species_choices) # labels are the species names
                ),
                box(width = 8,
                  plotOutput("spp_mean_cover_plot", height = 600) # species mean percent cover plot
                )
        ),
        
        # Relative Abundance plot (Tab 4)
        # Pick a year and plot relative abundance of categories
        tabItem(tabName = "relative_abundance", # tab name
                box(width = 4,
                    sliderInput("slider_year", # input for picking a year
                                label = h3("Select Year"), 
                                min = 2014, max = 2024, value = 2014, step = 5, # slider inputs
                                sep = "") # removes commas
                ),
                box(width = 8,
                  plotOutput("relative_abundance_plot", height = 600) # relative abundance of each category plot
                )
        ),
      # Regression plot (Tab 5)
      # Pick a native and invasive species on each axis and plot regression
      tabItem(tabName = "regression", # tab name
              box(width = 4, 
                  h3("Select Species"),
                  selectInput("select_invasive", # input for picking a site (matching data)
                              label = "Invasive Species",
                              choices = invasive_choices # only the 3 invasives
                              ),
                  selectInput("select_native", 
                              label = "Native Species",
                              choices = native_choices) # only natives
                              ),
              box(width = 8,
                  plotOutput("regression_plot", height = 600) # regression of invasive (x-axis) vs native (y-axis) plot
              )
              )
      )
    )
  )
#)
  


# SERVER  
server <- function(input, output) {
  # Site Map
 # output$site_map <- renderPlot({ # making map plot
    # read in map data here another day when have more time
    

# Species Percent Cover Plot (2nd Tab, Non-Interactive)
  cover_data<-reactive({ # make reactive data frame for species data
    # read in data
    cover_data<-relative_pcover
    # non-interactive tab
  })
  
  output$relative_cover_plot <- renderPlot({
    # Make the Violin Plot
    ggplot(cover_data(), # reactive dataframe
                         aes(x = community, y = relative_cover)) + 
      geom_violin(trim = FALSE, alpha = 0.6) + # violin plot over a jitter plot
      geom_jitter(width = 0.15, alpha = 0.4, size =1, color = "grey20") +
      facet_wrap(~year) + # facet the plots of each year next to each other
      labs(x = "Community Type",
           y = "Relative Percent % Cover",
           title = "Native vs. Invasive Algae Percent Cover by Year") +
      theme_minimal(base_size = 11) + # labels/theme for a pub visualization
      theme(#panel.grid = element_blank(),
            plot.title = element_text(face = "bold", size = 18, hjust = 0.5), # bold title
            axis.title = element_text(size = 14, face = "bold"), # make axis titles and text bigger
            axis.text.x = element_text(size = 12),
            panel.spacing = unit(1.2, "lines"),  # increase space between facets
            panel.border = element_rect(fill = NA, linewidth = 0.7), # border around each plot
            strip.text = element_text(size = 14, face = "bold")) # making labels bigger
  })
  

# Percent Cover by Species Over Time (3rd Tab)
  spp_cover<-reactive({ # reactive dataframe
    spp_cover<-spp_pcover %>% # data cleaned above
    # filter by species chosen by user input
      filter(species == input$select_spp1)
    
  })
  
  output$spp_mean_cover_plot <- renderPlot({
    # make all of the species names nice
    species_title<-species_choices_tbl$species_names[species_choices_tbl$species == input$select_spp1]
    
    # Make scatter plot with line
    ggplot(spp_cover(),
           aes(x = year, y = spp_mean_cover)) +
      geom_point(size = 3, alpha = 0.8, color = "grey10") +
      geom_line(data = spp_cover(),
                aes(x = year, y = spp_mean_cover, group = 1)) +
      facet_wrap(~category) +
      labs(x = "Year", y = "Mean Percent % Cover", # labels
           title = bquote(italic(.(species_title)) ~ "Percent Cover Over Time")) + # plot tile using species chosen
      theme_minimal(base_size = 11) + # labels/theme for a pub visualization
      theme(panel.grid = element_blank(),
            plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 14, face = "bold"), # make axis titles and text bigger
            panel.border = element_rect(fill = NA, linewidth = 0.7), # border around plot
            strip.text = element_text(size = 14, face = "bold")) # making labels bigger
  })

 # Relative Abundance Plot (4th Tab)
  relative_abundance_data <- reactive({ # new reactive dataframe
    relative_abundance_data<-relative_c_abundance %>%
      filter(year %in% input$slider_year)  # pick which year to plot from user input
  })
  output$relative_abundance_plot <- renderPlot({ # make plot
    # Make the Relative Abundance Plot
    ggplot(relative_abundance_data(),
           aes(x= assessment, y= count, fill= category)) + 
      geom_bar(stat="identity", position= "fill") + # stacked bar plot
      scale_x_discrete(labels = c(before_huki = "Before Huki", after_huki = "After Huki",
                                  monthly_assessment = "Monthly Assessment")) +
      labs(x = "Assessment Type", # labels
           y="Relative Abundance",
           fill = "Category",
           title = paste("Relative Abundance of Categories in", input$slider_year)) + # title using year chosen by user
      guides(color = "none") + # keep only legend for fill since fill and color are the same
      scale_fill_manual(values = c("Red Algae" = "indianred2", # specify fill colors for each category
                                   "Green Algae" = "#096700", # match the color to what the category looks like
                                   "Brown Algae" = "#5C4F25",
                                   "Cyanobacteria" = "#3a9085", "Substrate" = "#CBBD93", 
                                   "Other" = "#333311")) +
      scale_color_manual(values = c("Red Algae" = "indianred2", # specify line colors for each category (same as fill)
                                    "Green Algae" = "#096700", "Brown Algae" = "#5C4F25",
                                    "Cyanobacteria" = "#3a9085", "Substrate" = "#CBBD93", 
                                    "Other" = "#333311")) +
      theme_minimal() + 
      theme(panel.grid = element_blank(),
            plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
            axis.text.x = element_text(size = 12, vjust = 1, hjust = 0.5), # x axis label
            axis.title = element_text(size = 14, face = "bold"),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 14),
            strip.text = element_text(size = 14)) # making labels bigger
  })
  
# Regression Plot (5th Tab)
  regression_data<-reactive({ # new reactive dataframe
    regression_data<-regression_stuff %>% 
      select(plot_id, year, 
             invasive = all_of(input$select_invasive),
             native = all_of(input$select_native))
      #filter(species %in% input$select_invasive,
      #       species %in% input$select_native)  # pick which species to plot on x-axis from user input
  })
  
  # Make the Regression Plot
  output$regression_plot <- renderPlot({ 
    # labels for axes names based on ui
    invasive_label<-invasives_tbl$species_names[invasives_tbl$species == input$select_invasive]
    native_label<-natives_tbl$species_names[natives_tbl$species == input$select_native] # use brackets for selected columns
    
    # calculate axis ranges
    x_range <- range(regression_data()$invasive, na.rm = TRUE)
    y_range <- range(regression_data()$native, na.rm = TRUE)
    # add small padding for ranges
    x_pad <- diff(x_range) * 0.05
    y_pad <- diff(y_range) * 0.05
    
    # Make the Regression Plot
    ggplot(regression_data(), 
           aes(x = invasive, # plot all data for one invasive and one native species
               y = native)) + 
      geom_point(alpha = 0.6) + # scatter plot
      geom_jitter(alpha = 0.6, height = 0.5, width = 0.4) + # add jitter so that spreads out a little from 0
      geom_smooth(method = "lm", se = TRUE, color = "#0077BB", fill = "#99CCEE") + # linear regression line
      scale_x_continuous(limits = c(x_range[1] - x_pad, x_range[2] + x_pad)) + # scale axes for better vis
      scale_y_continuous(limits = c(y_range[1] - y_pad, y_range[2] + y_pad)) +
      labs(x = bquote("Invasive" ~ italic(.(invasive_label)) ~ "Percent Cover"),
           y = bquote("Native" ~ italic(.(native_label)) ~ "Percent Cover"),
           title = bquote("Invasive" ~ italic(.(invasive_label)) ~ "and Native" ~ italic(.(native_label)) ~ "Species Percent Cover")) +
      theme_minimal(base_size = 11) + # labels/theme for a pub visualization
      theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
            panel.grid = element_blank(),
            strip.text = element_text(size = 12), # making labels bigger
            axis.title  = element_text(size = 14),
            axis.text   = element_text(size = 12))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
