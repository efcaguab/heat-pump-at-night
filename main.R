library(magrittr)

analysis_hours <- c(0, 9)

# PROCESS TEMPERATURE -----------------------------------------------------

temperature <- readr::read_csv("data/temperature.csv")

temp_night <- temperature %>%
  dplyr::mutate(date_time = lubridate::ymd_hm(`Date(NZST)`), 
                date = lubridate::as_date(date_time),
                hour = lubridate::hour(date_time),
                t_min = `Tmin(C)`) %>%
  dplyr::filter(hour >= analysis_hours[1], 
                hour < analysis_hours[2]) %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(temperature = mean(t_min)) 


# PROCESS POWER -----------------------------------------------------------

power_raw <- xml2::read_xml("data/power.xml") %>%
  xml2::as_list() %>%
  extract2("power")

extract_row <- function(x){
  tidyr::tibble(date = x[[1]][[1]], 
                usage = x[[2]][[1]], 
                total = x[[3]][[1]])
}

extract_week <- function(x){
  x[[2]] %>%
    purrr::map_dfr(extract_row)
}

power_df <- power_raw %>%
  purrr::map_dfr(extract_week)

power <- power_df %>%
  dplyr::mutate(date = lubridate::mdy(date)) %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(time = (1:dplyr::n() - 1) / 2, 
                usage = as.numeric(usage)) %>%
  dplyr::filter(dplyr::n() == 48, 
                time >= analysis_hours[1],
                time < analysis_hours[2]) %>%
  dplyr::summarise(power = sum(usage)) 

# MERGE DATASETS ----------------------------------------------------------

model_df <- power %>%
  dplyr::inner_join(temp_night, by = "date") %>%
  dplyr::mutate(weekday = lubridate::wday(date) %in% 2:6, 
                keerthy = date > lubridate::ymd("2020-05-16"), 
                heatpump_on = date > lubridate::ymd("2020-05-29") & 
                  date < lubridate::ymd("2020-07-13"))

# MODEL -------------------------------------------------------------------

library(brms)

m <- brm(power ~ s(temperature) + heatpump_on  + weekday + keerthy, 
         data = model_df, 
         control = list(adapt_delta = 0.99), 
         iter = 4000,
         cores = 2)

conditional_effects(
  m, 
  effects = c("temperature", "heatpump_on"),
  method = "posterior_epred", 
  int_conditions = list(keerthy = TRUE, weekday = TRUE))

