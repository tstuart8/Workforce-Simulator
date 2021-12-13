library(forecast)
library(ggplot2)
library(lubridate)
points = read.csv("points.csv", header=T)
points$Week = as.character(points$Week)
points$Week = as.Date(points$Week, "%m/%d/%Y")

headcount = read.csv("headcount.csv", header=T)
headcount2 = read.csv("techs.csv", header=T)
headcount2$RecordWeek = as.character(headcount2$RecordWeek)
headcount2$RecordWeek = as.Date(headcount2$RecordWeek, "%m/%d/%Y")
headcount2[is.na(headcount2)] <- 0

points_pro = read.csv("Points_Pro.csv", header=T)
points_pro$Week = as.character(points_pro$Week)
points_pro$Week = as.Date(points_pro$Week, "%m/%d/%Y")
