inputfile <- system.file("extdata/signif.txt.tsv", package = "FinalAssignmentR")
rawdata <- eq_get_data(inputfile)
data <- eq_clean_data(rawdata)

#Test cleaning  
test_that("field LONGITUDE is numeric", {
	expect_is(data$LONGITUDE, "numeric")
})

test_that("field LATITUDE is numeric", {
	expect_is(data$LATITUDE, "numeric")
})

test_that("field DATE is a date", {
	expect_is(data$DATE, "Date")
})

#Test geoms
test_that("geom_timeline is ggplot object", {
	geomTimeline <- data %>% 
	  dplyr::filter(COUNTRY %in% c("CHINA", "USA") & lubridate::year(DATE) >= 2000) %>%
	  ggplot2::ggplot(aes(x = DATE, y = COUNTRY, colour = TOTAL_DEATHS, size = EQ_PRIMARY)) +
	  geom_timeline() 

	expect_is(geomTimeline, "ggplot")
})
  
test_that("geom_timeline_label is ggplot object", {
	geomTimelineLabel <- data %>% 
	  dplyr::filter(COUNTRY %in% c("CHINA", "USA") & lubridate::year(DATE) >= 2000) %>%
	  ggplot2::ggplot(aes(x = DATE, y = COUNTRY, colour = TOTAL_DEATHS, size = EQ_PRIMARY)) +
	  geom_timeline_label(aes(label = LOCATION_NAME), n_max = 5)

	expect_is(geomTimelineLabel, "ggplot")
})

test_that("theme_timeline is ggplot object", {
	geomThemeTimeline <- data %>% 
	  dplyr::filter(COUNTRY %in% c("CHINA", "USA") & lubridate::year(DATE) >= 2000) %>%
	  ggplot2::ggplot(aes(x = DATE, y = COUNTRY, colour = TOTAL_DEATHS, size = EQ_PRIMARY)) +
	  theme_timeline()

	expect_is(geomThemeTimeline, "ggplot")
})
  
#Test leafleat
test_that("eq_map is leaflet object", {
	eqMap <- data %>%
	  dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
	  eq_map(annot_col = "DATE")

	expect_is(eqMap, "leaflet")
})
  
test_that("eq_map_label is character", {
	eqMapLabel <- eq_create_label(data)
	expect_is(eqMapLabel, "character")
})
