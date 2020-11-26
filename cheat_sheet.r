library(ggplot2)
library(maps)
library(openintro)
library(dplyr)
library(scales)

# Configurando o working directory
setwd("C:/Users/eduar/Downloads/EACH EAD/20202/MQA/trabalho")

US_POPULATION <- 328239523
BLACK_PERCENTAGE <- 0.134
WHITE_PERCENTAGE <- 0.763
HISPANIC_PERCENTAGE <- 0.185
ASIAN_PERCENTAGE <- 0.059
NATIVE_PERCENTAGE <- 0.013


data = read.csv("shootings.csv", header = TRUE)
gender <- data$gender
race <- data$race
age <- floor(data$age)
data$age_as_factor <- as.factor(age)
data$date <- as.Date(data$date)
date <- data$date
weapons <- data$arms_category
flee <- data$flee
body_camera <- data$body_camera
mental_ilness <- data$signs_of_mental_ilness



get_mode <- function(column) {
  uniqv <- unique(column)
  uniqv[which.max(tabulate(match(column, uniqv)))]
}


create_subset_based_on_gender <- function(full_data, desired_gender){
  return(new_subset <- subset(full_data, gender == desired_gender))
}

create_subset_based_on_race <- function(full_data, desired_race){
  return(new_subset <- subset(full_data, race == desired_race))
}


normalize_states <- function(states) {
  states_abbreviation <- data$state
  return(tolower(abbr2state(states_abbreviation)))
}

generate_us_heatmap <- function(data) {
  normalized_states <- normalize_states(data$state)
  data$region <- normalized_states
  data <- transform(data, count = ave(state, region, FUN = length))
  states <- map_data("state")
  merged_states <- inner_join(states, data, by = "region")
  merged_states <- merged_states[order(merged_states$order),]
  p <- ggplot()
  p <- p + geom_polygon( data=merged_states, 
                         aes(x=long, y=lat, group=group, fill=as.numeric(count)), 
                         color="white", size = 0.2) 
  p <- p + scale_fill_continuous(name="N° de casos", 
                        low = "lightblue", high = "darkblue",limits = c(0,1000), 
                        breaks=c(0, 500, 1000), na.value = "grey90") +
                        theme(panel.background = element_blank()) +
                        xlab("") +
                        ylab("") 
  p
}

generate_gender_barplot <- function(gender) {
  barplot <- ggplot(data, aes(gender)) +
    geom_bar(fill = "#0073C2FF") +
    theme_minimal() +
    xlab("Gênero") +
    ylab("Ocorrências") 
  
  barplot
}

generate_gender_pie_chart <- function(gender) {
  data_percentages <- data %>% 
    group_by(gender) %>% 
    count() %>% 
    ungroup() %>% 
    mutate(per=`n`/sum(`n`)) %>% 
    arrange(desc(gender))
  data_percentages$label <- scales::percent(data_percentages$per)
  
  pie_chart <- ggplot(data=data_percentages)+
    geom_bar(aes(x="", y=per, fill=gender), stat="identity", width = 1)+
    coord_polar("y", start=0)+
    theme_void()+
    geom_text(aes(x=1, y = cumsum(per) - per/2, label=label)) +
    scale_fill_manual(values = c("red", 
                                 "steelblue"))
  pie_chart

}

generate_race_pie_chart <- function(race) {
  data_percentages <- data %>% 
    group_by(race) %>% 
    count() %>% 
    ungroup() %>% 
    mutate(per=`n`/sum(`n`)) %>% 
    arrange(desc(race))
  data_percentages$label <- scales::percent(data_percentages$per)
  
  pie_chart <- ggplot(data=data_percentages)+
    geom_bar(aes(x="", y=per, fill=race), stat="identity", width = 1)+
    coord_polar("y", start=0)+
    theme_void()
    #geom_text(aes(x=1, y = cumsum(per) - per/2, label=label)) 
  pie_chart
  
}


generate_race_barplot <- function(race) {
  barplot <- ggplot(data, aes(x=reorder(race, race, function(x)-length(x)))) +
    geom_bar(fill = "#0073C2FF") +
    theme_minimal() +
    xlab("Raça/Etnia") +
    ylab("Ocorrências")
  
  barplot
}

set_normalized_numbers_column <- function(df){
  data_percentages$normalized_numbers[data_percentages$race == "White"] <- 
    data_percentages$n[data_percentages$race == "White"] / 
    (US_POPULATION * WHITE_PERCENTAGE)
  
  data_percentages$normalized_numbers[data_percentages$race == "Black"] <- 
    data_percentages$n[data_percentages$race == "Black"] / 
    (US_POPULATION * BLACK_PERCENTAGE)
  
  data_percentages$normalized_numbers[data_percentages$race == "Hispanic"] <- 
    data_percentages$n[data_percentages$race == "Hispanic"] / 
    (US_POPULATION * HISPANIC_PERCENTAGE)
  
  data_percentages$normalized_numbers[data_percentages$race == "Asian"] <- 
    data_percentages$n[data_percentages$race == "Asian"] / 
    (US_POPULATION * ASIAN_PERCENTAGE)
  
  data_percentages$normalized_numbers[data_percentages$race == "Native"] <- 
    data_percentages$n[data_percentages$race == "Native"] / 
    (US_POPULATION * NATIVE_PERCENTAGE)
  
  data_percentages<-data_percentages[!(data_percentages$race=="Other"),]
}

generate_normalized_race_barplot <- function(race) {
  data_percentages <- data %>% 
    group_by(race) %>% 
    count() %>% 
    ungroup() %>% 
    mutate(per=`n`/sum(`n`)) %>% 
    arrange(desc(race))
  data_percentages <- set_normalized_numbers_column(data_percentages)
  barplot <- ggplot(data_percentages, aes(x=reorder(race, -normalized_numbers), y= normalized_numbers)) +
    geom_bar(stat="identity", fill = "#0073C2FF") +
    theme_minimal() +
    xlab("Raça/Etnia") +
    ylab("Ocorrências")
  
  barplot

}



generate_age_barplot <- function(age) {
  histogram <- ggplot(data , aes(x=age)) +
    geom_histogram(color="darkblue", fill="lightblue") +
    theme_minimal() +
    ylab("Ocorrências") + 
    xlab("Idade") +
    geom_density(alpha=.2, fill="#FF6666") +
    geom_vline(aes(xintercept=mean(age)),
                color="blue", linetype="solid", size=1) 

    
  histogram
}

generate_race_weapons_barplot <- function(race, weapons){
  
  barplot <- ggplot(data, aes(fill=weapons, x=race)) + 
    geom_bar(position="dodge", stat="count") +
    scale_y_continuous(trans='log2') +
    theme_minimal() +
    xlab("Raça/Etnia") +
    ylab("Ocorrências")
  
  barplot
}

generate_unarmed_and_not_fleeing_by_race_barplot <- function(race, weapons){
  data_unarmed <- subset(data, weapons=="Unarmed" & flee=="Not fleeing")
  barplot <- ggplot(data_unarmed, aes(fill=race, x=reorder(race, race, function(x)-length(x)))) + 
    geom_bar(position="dodge", stat="count") +
    theme_minimal() +
    xlab("Raça/Etnia") +
    ylab("Ocorrências")
  
  barplot
  
}

generate_unarmed_and_not_fleeing_body_camera_pie_chart <- function(race, weapons){
  data_unarmed <- subset(data, armed=="Unarmed" & flee=="Not fleeing")
  generate_body_camera_pie_chart(data_unarmed$body_camera, data_unarmed)
  
}

generate_unarmed_by_race_barplot <- function(race, weapons){
  data_unarmed <- subset(data, weapons=="Unarmed")
  barplot <- ggplot(data_unarmed, aes(fill=race, x=reorder(race, race, function(x)-length(x)))) + 
    geom_bar(position="dodge", stat="count") +
    theme_minimal() +
    xlab("Raça/Etnia") +
    ylab("Ocorrências")
  
  barplot
  
}

generate_unarmed_and_not_fleeing_proportional_to_cases_by_race_barplot <- function(race, weapons){
  cases_data_by_race <- data %>% 
    group_by(race) %>% 
    count() %>% 
    ungroup() %>% 
    mutate(per=`n`/sum(`n`)) %>% 
    arrange(desc(race))
  data_unarmed <- subset(data, weapons=="Unarmed" & flee=="Not fleeing")
  unarmed_cases_data_by_race <- data_unarmed %>% 
    group_by(race) %>% 
    count() %>% 
    ungroup() %>% 
    mutate(per=`n`/sum(`n`)) %>% 
    arrange(desc(race))
  cases_data_by_race$unarmed_proportion <- 
    (unarmed_cases_data_by_race$n/cases_data_by_race$n)
  barplot <- ggplot(cases_data_by_race, aes(
    fill=race,
    x=reorder(race, -unarmed_proportion),
    y=unarmed_proportion)) + 
    geom_bar(position="dodge", stat="identity") +
    theme_minimal() +
    xlab("Raça/Etnia") +
    ylab("Ocorrências")
  
  barplot
}

generate_unarmed_proportional_to_cases_by_race_barplot <- function(race, weapons){
  cases_data_by_race <- data %>% 
    group_by(race) %>% 
    count() %>% 
    ungroup() %>% 
    mutate(per=`n`/sum(`n`)) %>% 
    arrange(desc(race))
  data_unarmed <- subset(data, weapons=="Unarmed")
  unarmed_cases_data_by_race <- data_unarmed %>% 
    group_by(race) %>% 
    count() %>% 
    ungroup() %>% 
    mutate(per=`n`/sum(`n`)) %>% 
    arrange(desc(race))
  cases_data_by_race$unarmed_proportion <- 
    (unarmed_cases_data_by_race$n/cases_data_by_race$n)
  barplot <- ggplot(cases_data_by_race, aes(
    fill=race,
    x=reorder(race, -unarmed_proportion),
    y=unarmed_proportion)) + 
    geom_bar(position="dodge", stat="identity") +
    theme_minimal() +
    xlab("Raça/Etnia") +
    ylab("Ocorrências")
  
  barplot
}

generate_race_flee_barplot <- function(race, flee){
  
  barplot <- ggplot(data, aes(fill=flee, x=race)) + 
    geom_bar(position="dodge", stat="count") +
    theme_minimal() +
    xlab("Raça/Etnia") +
    ylab("Ocorrências") 
  
  barplot
}

generate_body_camera_barplot <- function(body_camera){
  barplot <- ggplot(data, aes(body_camera)) +
    geom_bar(fill = "#0073C2FF") +
    theme_minimal() +
    xlab("Câmera estava ligada?") +
    ylab("Ocorrências") 
  
  barplot
}

generate_body_camera_pie_chart <- function(body_camera, cases_data) {
  data_percentages <- data_unarmed %>% 
    group_by(body_camera) %>% 
    count() %>% 
    ungroup() %>% 
    mutate(per=`n`/sum(`n`)) %>% 
    arrange(desc(body_camera))
  data_percentages$label <- scales::percent(data_percentages$per)
  View(data_percentages)
  pie_chart <- ggplot(data=data_percentages)+
    geom_bar(aes(x="", y=per, fill=body_camera), stat="identity", width = 1)+
    coord_polar("y", start=0)+
    theme_void()+
    geom_text(aes(x=1, y = cumsum(per) - per/2, label=label)) +
    scale_fill_manual(values = c("red", 
                                 "steelblue"))
  pie_chart
  
}

get_boxplot_info_from_genders <- function(){
  male_subset <- create_subset_based_on_gender(data, "M")
  mean(male_subset$age)
  median(male_subset$age)
  get_mode(male_subset$age)
  
  female_subset <- create_subset_based_on_gender(data, "F")
  mean(female_subset$age)
  median(female_subset$age)
  get_mode(female_subset$age) 
}

get_boxplot_info_from_races <- function(){
  asian_subset <- create_subset_based_on_race(data, "Asian")
  mean(asian_subset$age)
  median(asian_subset$age)
  get_mode(asian_subset$age)
  
  black_subset <- create_subset_based_on_race(data, "Black")
  mean(black_subset$age)
  median(black_subset$age)
  get_mode(black_subset$age)
  
  hispanic_subset <- create_subset_based_on_race(data, "Hispanic")
  mean(hispanic_subset$age)
  median(hispanic_subset$age)
  get_mode(hispanic_subset$age)
  
  native_subset <- create_subset_based_on_race(data, "Native")
  mean(native_subset$age)
  median(native_subset$age)
  get_mode(native_subset$age)
  
  other_subset <- create_subset_based_on_race(data, "Other")
  mean(other_subset$age)
  median(other_subset$age)
  get_mode(other_subset$age)
  
  white_subset <- create_subset_based_on_race(data, "White")
  mean(white_subset$age)
  median(white_subset$age)
  get_mode(white_subset$age)
  
}


generate_age_boxplot <- function(age){
  p <- ggplot(data, aes(x=age)) + 
    geom_boxplot(outlier.colour="black", outlier.shape=16,
                 outlier.size=2, notch=FALSE, fill='steelblue', color="black") +
    coord_flip() +
    theme_minimal() +
    xlab("Idade")
  ggplot_build(p)$data
  p
}

generate_age_boxplot_based_on_gender <- function(age, gender){
  p <- ggplot(data, aes(x=age, y=gender, fill=gender)) + 
    geom_boxplot(outlier.colour="black", outlier.shape=16,
                 outlier.size=2, notch=FALSE, color="black") +
    coord_flip() +
    theme_minimal() +
    xlab("Idade") +
    ylab("Gênero") +
    theme(legend.position="none")
  ggplot_build(p)$data
  p
}

generate_age_scatter_plot <- function(){
  data_age <- data %>% 
    group_by(age) %>% 
    count() %>% 
    ungroup() %>% 
    mutate(per=`n`/sum(`n`)) %>% 
    arrange(desc(age))
  scatter_plot <- ggplot(data_age, aes(x=age, n)) +
    geom_point() +
    geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
  scatter_plot
}

generate_age_boxplot_based_on_race <- function(race){
  p <- ggplot(data, aes(x=age, y=race, fill=race)) + 
    geom_boxplot(outlier.colour="black", outlier.shape=16,
                 outlier.size=2, notch=FALSE, color="black") +
    coord_flip() +
    theme_minimal() +
    theme(legend.position="none") +
    xlab("Idade") +
    ylab("Raça/Etnia")
  ggplot_build(p)$data
  p
}

generate_dates_histogram <- function(date){
  
  date_data <- data %>% 
    mutate(year = format(date, "%m-%Y")) %>%
    group_by(year) %>% 
    ungroup() %>%
    arrange(desc(year))
  
  freqs <- aggregate(data$date, by=list(data$date), FUN=length)
  freqs$names <- as.Date(freqs$Group.1, format="%Y-%m-%d")
  
  histogram <- ggplot(date_data, aes(year, ..count..)) + geom_bar(stat="count") +
    ylab("Frequency") + xlab("Year and Month") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=6))
  histogram

}


# Gênero

generate_gender_barplot(gender)

generate_gender_pie_chart(gender)

# Raça/Etnia

generate_race_flee_barplot(race, flee)

generate_race_weapons_barplot(race, weapons)

generate_race_barplot(race)

generate_normalized_race_barplot(race)

generate_race_pie_chart(race)

generate_race_weapons_barplot(race, weapons)

# Body camera

generate_body_camera_barplot(body_camera)

generate_body_camera_pie_chart(body_camera, data)


# Age

generate_age_scatter_plot()

generate_age_boxplot(data$age_as_factor)

generate_age_boxplot_based_on_gender(data$age_as_factor, gender)

generate_age_histogram(age)

generate_age_boxplot_based_on_race(race)

# Armed/Flee

generate_unarmed_and_not_fleeing_by_race_barplot(race, weapons)

generate_unarmed_and_not_fleeing_proportional_to_cases_by_race_barplot(race, weapons)

generate_unarmed_by_race_barplot(race, weapons)

generate_unarmed_proportional_to_cases_by_race_barplot(race, weapons)

generate_unarmed_and_not_fleeing_body_camera_pie_chart(body_camera)

# Heatmap

generate_us_heatmap(data)

# Date

generate_dates_histogram(date)
