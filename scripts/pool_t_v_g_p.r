list.files()
library(stringdist)
library(dplyr)
library(reshape2)
setwd("D:\\Nextcloud\\Documents\\Uni\\Master\\Vorlesungen\\Semester 4\\Computational Ecology\\Group Project\\comp_ecology\\data\\particulates")
df_list <- lapply(list.files(),
        function(cur_file) {
            cur_df <- read.csv(cur_file,
                sep = ";")
            cur_year <- regmatches(cur_file, regexpr("\\d{4}", cur_file))
            cur_df$year <- as.integer(cur_year)
            return(cur_df)
        }
    )
other_df <- read.csv("../joined_temperature_green_aerea.csv")

shared_columns <- Reduce(intersect, 
    lapply(df_list, function(cur_df) colnames(cur_df)))

shared_cities <- Reduce(intersect, 
    lapply(df_list, function(cur_df) unique(cur_df$Station.name)))

all_df <- Reduce(rbind, 
    lapply(df_list, function(cur_df) cur_df[cur_df$Station.name %in% shared_cities,
        shared_columns]))

urban_df <- all_df #%>%
    filter(Station.setting == "urban area")

df_shared_years <- intersect(urban_df$year, other_df$year)
year_urban_df  <- urban_df %>%
    filter(year %in% df_shared_years)
year_other_df <- other_df %>%
    filter(year %in% df_shared_years,
        country == "Germany")

table(year_other_df$city)
city_dict <- setNames(unique(year_other_df$city), unique(year_other_df$city))
city_dict[c("Munich", "Nuremberg", "Duisburg", "Cologne")] <- c("München", "Nürnberg",
    "Düsburg", "Köln")
year_other_df$city <- city_dict[year_other_df$city]

df_shared_cities <- intersect(year_other_df$city, year_urban_df$Station.name)

city_matches <- lapply(unique(year_other_df$city),
    FUN = function(cur_city) {
        other_cities <- unique(year_urban_df$Station.name)
        match_scores <- stringsim(cur_city, other_cities,
            method = "lcs", q=3)
        other_cities <- other_cities[match_scores > 0.5]
        match_scores <- match_scores[match_scores > 0.5]
        return(c(other_cities[order(match_scores, decreasing = TRUE)],
            match_scores[order(match_scores, decreasing = TRUE)]))
    })
names(city_matches) <- unique(year_other_df$city)
View(city_matches)

city_dict2 <- list(
    "Berlin" = c("Berlin Buch",
        "Berlin Wedding",
        "Berlin Neukölln"),
    "München" = c(
        "München/Stachus",
        "München/Lothstraße"
    ),
    "Frankfurt" = c(
        "Frankfurt Ost",
        "Frankfurt-Höchst"
    ),
    "Stuttgart" = c(
        "Stuttgart-Bad Cannstatt"
    ),
    "Leipzig" = c(
        "Leipzig-West",
        "Leipzig-Mitte",
        "Leipzig Lützner Str."
    ),
    "Dresden" = c("Dresden-Nord"),
    "Dortmund" = c("Dortmund-Eving"),
    "Bremen" = c("Bremen-Nord",
        "Bremen-Dobben"),
    "Mannheim" = c("Mannheim-Nord")
)
View(city_dict2)

test <- lapply(names(city_dict2),
    function(cur_name) {
        setNames(rep(cur_name, length(city_dict2[[cur_name]])),
            city_dict2[[cur_name]])
    })
unlist(test)

city_dict3 <- setNames(unique(year_urban_df$Station.name), 
    unique(year_urban_df$Station.name))
city_dict3[names(unlist(test))] <- unlist(test)

year_urban_df$Station.name <- city_dict3[year_urban_df$Station.name]

intersect(year_urban_df$Station.name, year_other_df$city)

year_urban_df <- year_urban_df %>%
    filter(Station.name %in% intersect(year_urban_df$Station.name, year_other_df$city))

year_other_df <- year_other_df %>%
    filter(city %in% intersect(year_urban_df$Station.name, year_other_df$city))
year_other_df <- year_other_df[, -ncol(year_other_df)]
year_urban_df_sub <- year_urban_df[, c("Station.name", "year", "Annual.mean.value.in.µg.m.")]
colnames(year_urban_df_sub) <- c("city", "year", "particles")
colnames(year_other_df)

year_urban_df_sub <- year_urban_df_sub %>%
    group_by(city, year) %>%
    summarise(particles = mean(particles))

year_urban_df_sub$country <- "Germany"
long_year_other_df <- melt(year_other_df, measure.vars = c("green_area_pc", "temperature"))
long_year_urban_df_sub <- melt(year_urban_df_sub, measure.vars = "particles")
colnames(long_year_urban_df_sub)[1] <- "city"
long_year_urban_df_sub$variable <- "Particulates"
full_df <- rbind(long_year_urban_df_sub, long_year_other_df)
write.csv(full_df, "yearly_p_t_v_g_data.csv")
