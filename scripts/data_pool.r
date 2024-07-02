library(dplyr)
library(reshape2)
library(readxl)
library(stringdist)
library(ggplot2)

green_area_data <- read_excel("./green-area-per_capita-square-meters-per-capita_clean.xls",
    na = "..")
na_rows <- apply(green_area_data, 1,
    function(cur_row) any(is.na(cur_row)))
countries <- green_area_data$"Metropolitan areas"[na_rows]
length(countries)
country_reps <- diff(which(diff(c(na_rows, 1)) != 0))
country_reps <- country_reps[seq(1, length(country_reps), 2)]
length(countries) == length(country_reps)
countries_col_vec <- rep(countries, times = country_reps)
green_area_data <- na.omit(green_area_data)
green_area_data$country <- countries_col_vec

temp_data <- read.csv("./daily_temperature_1000_cities_1980_2020.csv")
temp_cities <- as.character(temp_data[2,])
temp_countries <- as.character(temp_data[5,])
dup_cities <- names(table(temp_cities))[table(temp_cities) > 1]
cities_to_remove <- sapply(dup_cities,
    function(cur_city) which(temp_cities == cur_city)[2])
temp_data <- temp_data[, -cities_to_remove]
temp_cities <- as.character(temp_data[2,])
temp_countries <- as.character(temp_data[5,])
temp_df <- temp_data[-c(1:12),]
temp_df$year <- regmatches(temp_df[,1], regexpr("\\d{4}", temp_df[,1]))
temp_df <- sapply(temp_df[, 2:ncol(temp_df)], as.numeric)
temp_df <- data.frame(temp_df)
mean_temp_df <- temp_df %>%
    group_by(year) %>%
    summarise(across(everything(), list(mean)))
colnames(mean_temp_df)[2:ncol(mean_temp_df)] <- temp_cities[2:length(temp_countries)]
test <- melt(mean_temp_df, id = "year")

countries <- rep(temp_countries[2:length(temp_countries)], 
    each = length(unique(test$year)))
test$countries <- countries

length(unique(green_area_data$country))
length(unique(countries))
intersect(green_area_data$"Metropolitan areas", test$variable)
country_dict <- setNames(countries, countries)
country_dict_b <- c("Czechia" = "Czech Republic",
    "Korea, South" = "Korea",
    "Netherlands" = "The Netherlands",
    "Slovakia" = "Slovak Republic")
country_dict[names(country_dict_b)] <- country_dict_b
test$countries <- country_dict[test$countries]
setdiff(green_area_data$country, test$countries)

city_options <- sapply(setdiff(green_area_data$"Metropolitan areas", test$variable),
    function(cur_city) {
        cur_country <- green_area_data$country[
            green_area_data$"Metropolitan areas" == cur_city
            ][1]
        as.character(unique(test$variable[
                test$countries == cur_country
            ]))})

best_city_matches <- sapply(seq_along(city_options),
    function(cur_city_idx) {
        temp_city <- names(city_options)[cur_city_idx]
        green_cities <- city_options[[cur_city_idx]]
        scores <- stringdist(tolower(temp_city),
            tolower(green_cities), method = "jaccard", q = 2)
        c(temp_city, green_cities[which.min(scores)], min(scores))
        }
)

best_city_matches <- best_city_matches[,as.numeric(best_city_matches[3,]) < 0.75]
n_match <- c(6, 10, 11, 14:23, 25, 35, 39)
best_city_matches <- best_city_matches[, -n_match]
city_dict <- setNames(unique(test$variable), unique(test$variable))
city_dict[best_city_matches[1, ]] <- best_city_matches[2, ]
any(names(city_dict)[1] %in% green_area_data$"Metropolitan areas")
test$variable <- city_dict[test$variable]
colnames(test)[2:4] <- c("city", "temperature","country")
colnames(green_area_data)[1] <- "city"
length(setdiff(test$city, green_area_data$city))
green_area_data_m <- melt(green_area_data, id = c("city", "country"))
colnames(green_area_data_m)[3:4] <- c("year", "green_area_pc")
green_area_data_m$year <- as.numeric(as.character(green_area_data_m$year))
green_area_years <- green_area_data_m$year
joined_df <- green_area_data_m %>%
    inner_join(test[test$year %in% green_area_years, ], by = c("country", "city", "year"))

joined_df_norm <- joined_df %>%
    group_by(city) %>%
    mutate(temp_norm = (temperature - mean(temperature))/sd(temperature)) %>%
    ungroup()
joined_df_norm %>%
    group_by(city) %>%
    summarise(temp_mean= mean(temp_norm), temp_sd = sd(temp_norm))

ggplot(joined_df_norm) +
    geom_point(aes(x = green_area_pc, y = temp_norm, fill = city))

write.csv(joined_df_norm, "joined_temperature_green_aerea.csv", row.names = FALSE)
