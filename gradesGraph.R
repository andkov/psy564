grades <- data.frame(c(1:100))
names(grades) <- c("percent")

head(grades)

attach(grades)
grades$letter[percent >= 90] <- "A+"
grades$letter[percent >= 85 & percent <= 89] <- "A"
grades$letter[percent >= 80 & percent <= 84] <- "A-"
grades$letter[percent >= 77 & percent <= 79] <- "B+"
grades$letter[percent >= 73 & percent <= 76] <- "B"
grades$letter[percent >= 70 & percent <= 72] <- "B-"
grades$letter[percent >= 65 & percent <= 69] <- "C+"
grades$letter[percent >= 60 & percent <= 64] <- "C"
grades$letter[percent >= 50 & percent <= 59] <- "D"
grades$letter[percent < 50 ] <- "F"
detach(grades)

head(grades)
table(grades$letter)