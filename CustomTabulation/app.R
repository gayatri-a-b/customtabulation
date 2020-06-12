library(gt)
library(shiny)
library(janitor)
library(tidyverse)
library(formattable)

# options(shiny.maxRequestSize=100*1024^2)

# read in all the files
# set colnames to second row

raw_data_nation_1R <- read.csv("1R/nation__EEO_10_5YR_EEOALL1R_Data.csv", colClasses = 'character', skip=1, check.names=FALSE)
raw_data_state_1R <- read.csv("1R/state__EEO_10_5YR_EEOALL1R_Data.csv", colClasses = 'character', skip=1, check.names=FALSE)
#raw_data_place_1R <- read.csv("1R/place__EEO_10_5YR_EEOALL1R_Data.csv", colClasses = 'character', skip=1, check.names=FALSE)
#raw_data_metro_1R <- read.csv("1R/metro__EEO_10_5YR_EEOALL1R_Data.csv", colClasses = 'character', skip=1, check.names=FALSE)

raw_data_nation_1W <- read.csv("1W/nation__EEO_10_5YR_EEOALL1W_Data.csv", colClasses = 'character', skip=1, check.names=FALSE)
raw_data_state_1W <- read.csv("1W/state__EEO_10_5YR_EEOALL1W_Data.csv", colClasses = 'character', skip=1, check.names=FALSE)
#raw_data_place_1W <- read.csv("1W/place__EEO_10_5YR_EEOALL1W_Data.csv", colClasses = 'character', skip=1, check.names=FALSE)
#raw_data_metro_1W <- read.csv("1W/metro__EEO_10_5YR_EEOALL1W_Data.csv", colClasses = 'character', skip=1, check.names=FALSE)


# Define UI for data upload app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("Uploading Files"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # select table name
            selectInput(
                "tab_name", 
                "Table Name",
                c("", "1R", "1W")
            ),
            
            
            # select overall geography
            selectInput(
                "geo", 
                "Overal Geography",
                c("nation", "state", "place", "metro", "micro")
            ),
            
            # select occupation
            selectInput(
                "occupation", 
                "Occupation (empty until table name selected)",
                c()
            ),
            
            # select specific geography
            selectInput(
                "geo_spec", 
                "Specific Geography (empty until table name selected",
                c()
            ),
            
            # select estimate or margin of error
            radioButtons(
                "est_mar", 
                "Table type",
                c("Estimate", "Margin of Error")
            ),
            
            # # select number or percentage
            # radioButtons(
            #     "num_per", 
            #     "Data type",
            #     c("Number", "Percent")
            # )
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            h4("Output Data"),
            
            tableOutput("final_output")
            
        )
        
    )
)


# Define server logic to read selected file

server <- function(input, output, session) {
    
    # returns out data table
    dt_extract <- function(){
    
        # get the correct file
        
        table_name <- input$tab_name
        geography <- input$geo
        
        # build up the file name
        # make sure micro uses same filename as metro
        
        temp <- geography
        
        if (geography == "micro") {
            temp <- "metro"
        }
        
        data_name <- paste("raw_data", temp, table_name, sep = "_")
        
        # get the dataset
        
        x <- eval(parse(text = data_name))
        
        
        ################################################# CLEAN DATA
        
        # set the columns to pivot on based on table name
        
        if (table_name == "1R"){
            pivot_cols <- c("Id", 
                            "Id2", 
                            "Geography", 
                            "Occupation Code"
            )
        } else if (table_name == "1W"){
            pivot_cols <- c("Id", 
                            "Id2", 
                            "Geography", 
                            "Occupation Code",
                            "Residence to Work Place Flows"
            )
        } else{
            pivot_cols <- c()
        }
        
        # pivot columns from wide to narrow
        
        x <- x %>%
            pivot_longer(
                # don't include these columns
                -pivot_cols,
                
                # set the new pivoted cols to statistic_name
                names_to = "statistic_name",
                
                # set their pivoted value to count
                values_to = "count"
            )
        
        # clean up the names for x
        
        x <- x %>% 
            clean_names()
        
        return (x)
    }
    
    
    # Populate dropdowns based on input file
    
    observe({
        
        # input$tab_name will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        req(input$tab_name)
        
        
        # get our data table
        
        x <- dt_extract()
        
        
        # get the list of occupations and geographies
        
        occupations <- x %>% distinct(occupation_code) %>% pull(occupation_code) %>%  sort()
        geographies <- x %>% distinct(geography)
        
        updateSelectInput(session, "occupation",
                          label = "Occupation",
                          choices = occupations,
                          selected = tail(occupations, 1)
        )
        
        updateSelectInput(session, "geo_spec",
                          label = "Specific geography",
                          choices = geographies,
                          selected = head(geographies, 1)
        )
        
    })
    
    # Create output table
    
    output$final_output <- renderTable({
        req(input$tab_name)
        
        # get our data table
        
        x <- dt_extract()
        
        ################################################# vARIABLES
        
        occupation <- input$occupation
        type <- input$est_mar
        # num_type <- input$num_per
        geo <- input$geo_spec
        
        ################################################# FILTER DATA BY INTAKE PARAMETERS
        
        # treat count as number
        # filter by occupation
        
        x_occupation <- x %>% 
            mutate(count = as.numeric(count)) %>%
            filter(
                occupation_code == occupation
            )
        
        # filter by geography
        
        x_geography <- x_occupation %>%
            filter(
                grepl(geo, geography, fixed = TRUE)
            )
        
        # filter by estimate (or margin of error)
        
        x_type <- x_geography %>% 
            filter(
                grepl(type, statistic_name, fixed = TRUE)
            )
        
        
        ################################################################### DATAFRAMES FOR SPANNER VERTICAL COLS
        
        # Total, race and ethnicity column
        
        total_race_ethnicity <- x_type %>% 
            filter(
                grepl('Total, race and ethnicity', statistic_name, fixed = TRUE)
            )
        
        # Hispanic or Latino column
        
        hispanic_or_latino <- x_type %>% 
            filter(
                grepl('Hispanic or Latino', statistic_name, fixed = TRUE) &
                    !(grepl('Not Hispanic or Latino', statistic_name, fixed = TRUE)) &
                    !(grepl('Balance of not Hispanic or Latino', statistic_name, fixed = TRUE))
            )
        
        # Not Hispanic or Latino, one or more races column
        
        not_hispanic_or_latino_one_race <- x_type %>% 
            filter(
                grepl('Not Hispanic or Latino, one race', statistic_name, fixed = TRUE)
            )
        
        # Not Hispanic or Latino, two or more races column
        
        not_hispanic_or_latino_two_ore_more_races <- x_type %>% 
            filter(
                grepl('Not Hispanic or Latino, two or more races', statistic_name, fixed = TRUE)
            )
        
        
        ################################################################### DATAFRAMES FOR INDIVIDUAL VERT COLS (RACES)
        
        #### Hispanic or Latino
        
        # White alone Hispanic or Latino
        
        white_alone_hispanic_or_latino <- hispanic_or_latino %>% 
            filter(
                grepl('White alone Hispanic or Latino', statistic_name, fixed = TRUE)
            )
        
        # All other Hispanic or Latino
        
        all_other_hispanic_or_latino <- hispanic_or_latino %>% 
            filter(
                grepl('All other Hispanic or Latino', statistic_name, fixed = TRUE)
            )
        
        
        #### Not Hispanic or Latino, one or more races
        
        # White alone
        
        white_alone <- not_hispanic_or_latino_one_race %>% 
            filter(
                grepl('White alone', statistic_name, fixed = TRUE)
            )
        
        # Black or African American alone
        
        black_or_african_american_alone <- not_hispanic_or_latino_one_race %>% 
            filter(
                grepl('Black or African American alone', statistic_name, fixed = TRUE)
            )
        
        # American Indian and Alaska Native alone
        
        american_indian_and_alask_native_alone <- not_hispanic_or_latino_one_race %>% 
            filter(
                grepl('American Indian and Alaska Native alone', statistic_name, fixed = TRUE)
            )
        
        # Asian alone
        
        asian_alone <- not_hispanic_or_latino_one_race %>% 
            filter(
                grepl('Asian alone', statistic_name, fixed = TRUE)
            )
        
        # Native Hawaiian and Other Pacific Islander alone
        
        native_hawaiian_and_other_pacific_islander_alone <- not_hispanic_or_latino_one_race %>% 
            filter(
                grepl('Native Hawaiian and Other Pacific Islander alone', statistic_name, fixed = TRUE)
            )
        
        
        #### Not Hispanic or Latino, two or more races
        
        # White and Black
        
        white_and_black <- not_hispanic_or_latino_two_ore_more_races %>% 
            filter(
                grepl('White and Black', statistic_name, fixed = TRUE)
            )
        
        # White and AIAN
        
        white_and_aian <- not_hispanic_or_latino_two_ore_more_races %>% 
            filter(
                grepl('White and AIAN', statistic_name, fixed = TRUE)
            )
        
        # White and Asian
        
        white_and_asian <- not_hispanic_or_latino_two_ore_more_races %>% 
            filter(
                grepl('White and Asian', statistic_name, fixed = TRUE)
            )
        
        # Black and Black
        
        black_and_aian <- not_hispanic_or_latino_two_ore_more_races %>% 
            filter(
                grepl('Black and AIAN', statistic_name, fixed = TRUE)
            )
        
        # NHPI and White (Hawaii only)
        
        nhpi_and_white_Hawaii_only <- not_hispanic_or_latino_two_ore_more_races %>% 
            filter(
                grepl('NHPI and White (Hawaii only)', statistic_name, fixed = TRUE)
            )
        
        # NHPI and Asian (Hawaii only)
        
        nhpi_and_asian_Hawaii_only <- not_hispanic_or_latino_two_ore_more_races %>% 
            filter(
                grepl('NHPI and Asian (Hawaii only)', statistic_name, fixed = TRUE)
            )
        
        # NHPI and Asian and White (Hawaii only)
        
        nhpi_and_asian_and_white_Hawaii_only <- not_hispanic_or_latino_two_ore_more_races %>% 
            filter(
                grepl('NHPI and Asian and White (Hawaii only)', statistic_name, fixed = TRUE)
            )
        
        
        ################################################################### FUNCTIONS FOR GENDER HORIZONTAL ROWS

        
        # output Total, both sexes total
        # only obtain number
        
        getTotalNum <- function(dataframe) {
            return(
                dataframe %>% 
                    filter(
                        grepl('Total, both sexes', statistic_name, fixed = TRUE),
                        grepl("Number", statistic_name, fixed = TRUE)
                    ) %>% 
                    pull()       
            )
        }
        
        # output Female total
        # only obtain number
        
        getFemaleNum <- function(dataframe) {
            return(
                dataframe %>% 
                    filter(
                        grepl("Female", statistic_name, fixed = TRUE),
                        grepl("Number", statistic_name, fixed = TRUE)
                    ) %>% 
                    pull()
            )
        }
        
        # output Male total
        # only obtain number
        
        getMaleNum <- function(dataframe) {
            return(
                dataframe %>% 
                    filter(
                        grepl('Male', statistic_name, fixed = TRUE),
                        grepl("Number", statistic_name, fixed = TRUE)
                    ) %>% 
                    pull()
            )
        }
        
        
        # output Total, both sexes total
        # only obtain percentage
        
        getTotalPer <- function(dataframe) {
            return(
                dataframe %>% 
                    filter(
                        grepl('Total, both sexes', statistic_name, fixed = TRUE),
                        grepl("Percent", statistic_name, fixed = TRUE)
                    ) %>% 
                    pull()       
            )
        }
        
        # output Female total
        # only obtain percentage
        
        getFemalePer <- function(dataframe) {
            return(
                dataframe %>% 
                    filter(
                        grepl("Female", statistic_name, fixed = TRUE),
                        grepl("Percent", statistic_name, fixed = TRUE)
                    ) %>% 
                    pull()
            )
        }
        
        # output Male total
        # only obtain percentage
        
        getMalePer <- function(dataframe) {
            return(
                dataframe %>% 
                    filter(
                        grepl('Male', statistic_name, fixed = TRUE),
                        grepl("Percent", statistic_name, fixed = TRUE)
                    ) %>% 
                    pull()
            )
        }
        
        
        ################################################################### VALUES FOR EACH CELL
        
        #### Total, both sexes row number
        
        total_both_sexes_total_num <- getTotalNum(total_race_ethnicity)
        total_both_sexes_hisp_white_num <- getTotalNum(white_alone_hispanic_or_latino)
        total_both_sexes_hisp_all_other_num <- getTotalNum(all_other_hispanic_or_latino)
        total_both_sexes_not_hisp_one_race_white_num <- getTotalNum(white_alone)
        total_both_sexes_not_hisp_one_race_black_num <- getTotalNum(black_or_african_american_alone)
        total_both_sexes_not_hisp_one_race_american_indian_num <- getTotalNum(american_indian_and_alask_native_alone)
        total_both_sexes_not_hisp_one_race_asian_num <- getTotalNum(asian_alone)
        total_both_sexes_not_hisp_one_race_nhpi_num <- getTotalNum(native_hawaiian_and_other_pacific_islander_alone)
        total_both_sexes_not_hisp_two_races_white_black_num <- getTotalNum(white_and_black)
        total_both_sexes_not_hisp_two_races_white_aian_num <- getTotalNum(white_and_aian)
        total_both_sexes_not_hisp_two_races_white_asian_num <- getTotalNum(white_and_asian)
        total_both_sexes_not_hisp_two_races_black_aian_num <- getTotalNum(black_and_aian)
        total_both_sexes_not_hisp_two_races_nhpi_white_num <- getTotalNum(nhpi_and_white_Hawaii_only)
        total_both_sexes_not_hisp_two_races_nhpi_asian_num <- getTotalNum(nhpi_and_asian_Hawaii_only)
        total_both_sexes_not_hisp_two_races_nhpi_asian_white_num <- getTotalNum(nhpi_and_asian_and_white_Hawaii_only)
        
        
        #### Male row number
        
        total_male_total_num <- getMaleNum(total_race_ethnicity)
        total_male_hisp_white_num <- getMaleNum(white_alone_hispanic_or_latino)
        total_male_hisp_all_other_num <- getMaleNum(all_other_hispanic_or_latino)
        total_male_not_hisp_one_race_white_num <- getMaleNum(white_alone)
        total_male_not_hisp_one_race_black_num <- getMaleNum(black_or_african_american_alone)
        total_male_not_hisp_one_race_american_indian_num <- getMaleNum(american_indian_and_alask_native_alone)
        total_male_not_hisp_one_race_asian_num <- getMaleNum(asian_alone)
        total_male_not_hisp_one_race_nhpi_num <- getMaleNum(native_hawaiian_and_other_pacific_islander_alone)
        total_male_not_hisp_two_races_white_black_num <- getMaleNum(white_and_black)
        total_male_not_hisp_two_races_white_aian_num <- getMaleNum(white_and_aian)
        total_male_not_hisp_two_races_white_asian_num <- getMaleNum(white_and_asian)
        total_male_not_hisp_two_races_black_aian_num <- getMaleNum(black_and_aian)
        total_male_not_hisp_two_races_nhpi_white_num <- getMaleNum(nhpi_and_white_Hawaii_only)
        total_male_not_hisp_two_races_nhpi_asian_num <- getMaleNum(nhpi_and_asian_Hawaii_only)
        total_male_not_hisp_two_races_nhpi_asian_white_num <- getMaleNum(nhpi_and_asian_and_white_Hawaii_only)
        
        
        #### Female row number
        
        total_female_total_num <- getFemaleNum(total_race_ethnicity)
        total_female_hisp_white_num <- getFemaleNum(white_alone_hispanic_or_latino)
        total_female_hisp_all_other_num <- getFemaleNum(all_other_hispanic_or_latino)
        total_female_not_hisp_one_race_white_num <- getFemaleNum(white_alone)
        total_female_not_hisp_one_race_black_num <- getFemaleNum(black_or_african_american_alone)
        total_female_not_hisp_one_race_american_indian_num <- getFemaleNum(american_indian_and_alask_native_alone)
        total_female_not_hisp_one_race_asian_num <- getFemaleNum(asian_alone)
        total_female_not_hisp_one_race_nhpi_num <- getFemaleNum(native_hawaiian_and_other_pacific_islander_alone)
        total_female_not_hisp_two_races_white_black_num <- getFemaleNum(white_and_black)
        total_female_not_hisp_two_races_white_aian_num <- getFemaleNum(white_and_aian)
        total_female_not_hisp_two_races_white_asian_num <- getFemaleNum(white_and_asian)
        total_female_not_hisp_two_races_black_aian_num <- getFemaleNum(black_and_aian)
        total_female_not_hisp_two_races_nhpi_white_num <- getFemaleNum(nhpi_and_white_Hawaii_only)
        total_female_not_hisp_two_races_nhpi_asian_num <- getFemaleNum(nhpi_and_asian_Hawaii_only)
        total_female_not_hisp_two_races_nhpi_asian_white_num <- getFemaleNum(nhpi_and_asian_and_white_Hawaii_only)
        
        
        #### Total, both sexes row percent
        
        total_both_sexes_total_per <- getTotalPer(total_race_ethnicity)
        total_both_sexes_hisp_white_per <- getTotalPer(white_alone_hispanic_or_latino)
        total_both_sexes_hisp_all_other_per <- getTotalPer(all_other_hispanic_or_latino)
        total_both_sexes_not_hisp_one_race_white_per <- getTotalPer(white_alone)
        total_both_sexes_not_hisp_one_race_black_per <- getTotalPer(black_or_african_american_alone)
        total_both_sexes_not_hisp_one_race_american_indian_per <- getTotalPer(american_indian_and_alask_native_alone)
        total_both_sexes_not_hisp_one_race_asian_per <- getTotalPer(asian_alone)
        total_both_sexes_not_hisp_one_race_nhpi_per <- getTotalPer(native_hawaiian_and_other_pacific_islander_alone)
        total_both_sexes_not_hisp_two_races_white_black_per <- getTotalPer(white_and_black)
        total_both_sexes_not_hisp_two_races_white_aian_per <- getTotalPer(white_and_aian)
        total_both_sexes_not_hisp_two_races_white_asian_per <- getTotalPer(white_and_asian)
        total_both_sexes_not_hisp_two_races_black_aian_per <- getTotalPer(black_and_aian)
        total_both_sexes_not_hisp_two_races_nhpi_white_per <- getTotalPer(nhpi_and_white_Hawaii_only)
        total_both_sexes_not_hisp_two_races_nhpi_asian_per <- getTotalPer(nhpi_and_asian_Hawaii_only)
        total_both_sexes_not_hisp_two_races_nhpi_asian_white_per <- getTotalPer(nhpi_and_asian_and_white_Hawaii_only)
        
        
        #### Male row number
        
        total_male_total_per <- getMalePer(total_race_ethnicity)
        total_male_hisp_white_per <- getMalePer(white_alone_hispanic_or_latino)
        total_male_hisp_all_other_per <- getMalePer(all_other_hispanic_or_latino)
        total_male_not_hisp_one_race_white_per <- getMalePer(white_alone)
        total_male_not_hisp_one_race_black_per <- getMalePer(black_or_african_american_alone)
        total_male_not_hisp_one_race_american_indian_per <- getMalePer(american_indian_and_alask_native_alone)
        total_male_not_hisp_one_race_asian_per <- getMalePer(asian_alone)
        total_male_not_hisp_one_race_nhpi_per <- getMalePer(native_hawaiian_and_other_pacific_islander_alone)
        total_male_not_hisp_two_races_white_black_per <- getMalePer(white_and_black)
        total_male_not_hisp_two_races_white_aian_per <- getMalePer(white_and_aian)
        total_male_not_hisp_two_races_white_asian_per <- getMalePer(white_and_asian)
        total_male_not_hisp_two_races_black_aian_per <- getMalePer(black_and_aian)
        total_male_not_hisp_two_races_nhpi_white_per <- getMalePer(nhpi_and_white_Hawaii_only)
        total_male_not_hisp_two_races_nhpi_asian_per <- getMalePer(nhpi_and_asian_Hawaii_only)
        total_male_not_hisp_two_races_nhpi_asian_white_per <- getMalePer(nhpi_and_asian_and_white_Hawaii_only)
        
        
        #### Female row percent
        
        total_female_total_per <- getFemalePer(total_race_ethnicity)
        total_female_hisp_white_per <- getFemalePer(white_alone_hispanic_or_latino)
        total_female_hisp_all_other_per <- getFemalePer(all_other_hispanic_or_latino)
        total_female_not_hisp_one_race_white_per <- getFemalePer(white_alone)
        total_female_not_hisp_one_race_black_per <- getFemalePer(black_or_african_american_alone)
        total_female_not_hisp_one_race_american_indian_per <- getFemalePer(american_indian_and_alask_native_alone)
        total_female_not_hisp_one_race_asian_per <- getFemalePer(asian_alone)
        total_female_not_hisp_one_race_nhpi_per <- getFemalePer(native_hawaiian_and_other_pacific_islander_alone)
        total_female_not_hisp_two_races_white_black_per <- getFemalePer(white_and_black)
        total_female_not_hisp_two_races_white_aian_per <- getFemalePer(white_and_aian)
        total_female_not_hisp_two_races_white_asian_per <- getFemalePer(white_and_asian)
        total_female_not_hisp_two_races_black_aian_per <- getFemalePer(black_and_aian)
        total_female_not_hisp_two_races_nhpi_white_per <- getFemalePer(nhpi_and_white_Hawaii_only)
        total_female_not_hisp_two_races_nhpi_asian_per <- getFemalePer(nhpi_and_asian_Hawaii_only)
        total_female_not_hisp_two_races_nhpi_asian_white_per <- getFemalePer(nhpi_and_asian_and_white_Hawaii_only)
        
        
        ################################################################### CREATE MATRIX USING VALUES
        
        table_x <- matrix(
            # column headers
            
            c(
                "Geography, Occupation, Subject",
                " ",
                "White alone\nHispanic or Latino",
                "All other\nHispanic or Latino",
                "White alone",
                "Black or African American alone",
                "American Indian and\nAlaska Native alone",
                "Asian alone",
                "Native Hawaiian\nand Other Pacific Islander alone",
                "White and\nBlack",
                "White and\nAIAN",
                "White and\nAsian",
                "Black and\nAIAN",
                "NHPI and White\n(Hawaii only)",
                "NHPI and Asian\n(Hawaii only)",
                "NHPI and Asian and\nWhite (Hawaii only)",
                
                geo,'','','','','','','','','','','','','','','',
                occupation,'','','','','','','','','','','','','','','',
                
                'Total, both sexes','','','','','','','','','','','','','','','',
                
                
                # Total, both sexes Number values
                
                'Number',
                total_both_sexes_total_num,
                total_both_sexes_hisp_white_num,
                total_both_sexes_hisp_all_other_num,
                total_both_sexes_not_hisp_one_race_white_num,
                total_both_sexes_not_hisp_one_race_black_num,
                total_both_sexes_not_hisp_one_race_american_indian_num,
                total_both_sexes_not_hisp_one_race_asian_num,
                total_both_sexes_not_hisp_one_race_nhpi_num,
                total_both_sexes_not_hisp_two_races_white_black_num,
                total_both_sexes_not_hisp_two_races_white_aian_num,
                total_both_sexes_not_hisp_two_races_white_asian_num,
                total_both_sexes_not_hisp_two_races_black_aian_num,
                total_both_sexes_not_hisp_two_races_nhpi_white_num,
                total_both_sexes_not_hisp_two_races_nhpi_asian_num,
                total_both_sexes_not_hisp_two_races_nhpi_asian_white_num,
                
                # Total, both sexes Percent values
                
                'Percent',
                total_both_sexes_total_per,
                total_both_sexes_hisp_white_per,
                total_both_sexes_hisp_all_other_per,
                total_both_sexes_not_hisp_one_race_white_per,
                total_both_sexes_not_hisp_one_race_black_per,
                total_both_sexes_not_hisp_one_race_american_indian_per,
                total_both_sexes_not_hisp_one_race_asian_per,
                total_both_sexes_not_hisp_one_race_nhpi_per,
                total_both_sexes_not_hisp_two_races_white_black_per,
                total_both_sexes_not_hisp_two_races_white_aian_per,
                total_both_sexes_not_hisp_two_races_white_asian_per,
                total_both_sexes_not_hisp_two_races_black_aian_per,
                total_both_sexes_not_hisp_two_races_nhpi_white_per,
                total_both_sexes_not_hisp_two_races_nhpi_asian_per,
                total_both_sexes_not_hisp_two_races_nhpi_asian_white_per,
                
                
                'Male','','','','','','','','','','','','','','','',
                
                # Male Number values
                
                'Number',
                total_male_total_num,
                total_male_hisp_white_num,
                total_male_hisp_all_other_num,
                total_male_not_hisp_one_race_white_num,
                total_male_not_hisp_one_race_black_num,
                total_male_not_hisp_one_race_american_indian_num,
                total_male_not_hisp_one_race_asian_num,
                total_male_not_hisp_one_race_nhpi_num,
                total_male_not_hisp_two_races_white_black_num,
                total_male_not_hisp_two_races_white_aian_num,
                total_male_not_hisp_two_races_white_asian_num,
                total_male_not_hisp_two_races_black_aian_num,
                total_male_not_hisp_two_races_nhpi_white_num,
                total_male_not_hisp_two_races_nhpi_asian_num,
                total_male_not_hisp_two_races_nhpi_asian_white_num,
                
                # Male Percent Values
                
                'Percent',
                total_male_total_per,
                total_male_hisp_white_per,
                total_male_hisp_all_other_per,
                total_male_not_hisp_one_race_white_per,
                total_male_not_hisp_one_race_black_per,
                total_male_not_hisp_one_race_american_indian_per,
                total_male_not_hisp_one_race_asian_per,
                total_male_not_hisp_one_race_nhpi_per,
                total_male_not_hisp_two_races_white_black_per,
                total_male_not_hisp_two_races_white_aian_per,
                total_male_not_hisp_two_races_white_asian_per,
                total_male_not_hisp_two_races_black_aian_per,
                total_male_not_hisp_two_races_nhpi_white_per,
                total_male_not_hisp_two_races_nhpi_asian_per,
                total_male_not_hisp_two_races_nhpi_asian_white_per,
                
                
                
                'Female','','','','','','','','','','','','','','','',
                
                # Female Number values
                
                'Number',
                total_female_total_num,
                total_female_hisp_white_num,
                total_female_hisp_all_other_num,
                total_female_not_hisp_one_race_white_num,
                total_female_not_hisp_one_race_black_num,
                total_female_not_hisp_one_race_american_indian_num,
                total_female_not_hisp_one_race_asian_num,
                total_female_not_hisp_one_race_nhpi_num,
                total_female_not_hisp_two_races_white_black_num,
                total_female_not_hisp_two_races_white_aian_num,
                total_female_not_hisp_two_races_white_asian_num,
                total_female_not_hisp_two_races_black_aian_num,
                total_female_not_hisp_two_races_nhpi_white_num,
                total_female_not_hisp_two_races_nhpi_asian_num,
                total_female_not_hisp_two_races_nhpi_asian_white_num,
                
                # Female Percent Values
                
                'Percent',
                total_female_total_per,
                total_female_hisp_white_per,
                total_female_hisp_all_other_per,
                total_female_not_hisp_one_race_white_per,
                total_female_not_hisp_one_race_black_per,
                total_female_not_hisp_one_race_american_indian_per,
                total_female_not_hisp_one_race_asian_per,
                total_female_not_hisp_one_race_nhpi_per,
                total_female_not_hisp_two_races_white_black_per,
                total_female_not_hisp_two_races_white_aian_per,
                total_female_not_hisp_two_races_white_asian_per,
                total_female_not_hisp_two_races_black_aian_per,
                total_female_not_hisp_two_races_nhpi_white_per,
                total_female_not_hisp_two_races_nhpi_asian_per,
                total_female_not_hisp_two_races_nhpi_asian_white_per
            ),
            # conditions so matrix is parsed correctly
            ncol=16,byrow=TRUE
        )
        
        
        ################################################################### SET ROWS/COLS OF MATRIX
        
        colnames(table_x) = table_x[1,]
        table_x <- table_x[-1,]
        rownames(table_x) = table_x[,1]
        
        
        ################################################################### FORMAT MATRIX INTO PRETTY TABLE
        
        dt <- table_x %>%
            gt() %>%
            
            # spanner labels
            tab_spanner(
                label = "Hispanic or Latino",
                columns = vars("White alone\nHispanic or Latino", "All other\nHispanic or Latino")
            ) %>%
            tab_spanner(
                label = "Not Hispanic or Latino, one race",
                columns = vars(
                    "White alone",
                    "Black or African American alone",
                    "American Indian and\nAlaska Native alone",
                    "Asian alone",
                    "Native Hawaiian\nand Other Pacific Islander alone",
                    "White and\nBlack",
                    "White and\nAIAN",
                    "White and\nAsian",
                    "Black and\nAIAN"
                )
            ) %>%
            tab_spanner(
                label = "Not Hispanic or Latino, two or more races",
                columns = vars(
                    "NHPI and White\n(Hawaii only)",
                    "NHPI and Asian\n(Hawaii only)",
                    "NHPI and Asian and\nWhite (Hawaii only)"
                )
            ) %>%
            tab_spanner(
                label = "Race/Ethnicity",
                columns = vars(
                    "Geography, Occupation, Subject"
                )
            ) %>%
            tab_spanner(
                label = "Total, race\nand Ethnicity",
                columns = vars(
                    " "
                )
            ) %>%
            
            # format missing values to zero
            
            fmt_missing(
                columns = 3:16,
                missing_text = 0
            )
        
        dt
        
    })
    
}
# Run the app ----
shinyApp(ui, server)