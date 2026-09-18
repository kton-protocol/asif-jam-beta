# The rise. Same file, same magnitude floor, same region as q1 — one added filter.
#
# Moment magnitude (mw*) is the modern, physically-grounded magnitude scale. Count only the
# California M3+ events whose magnitude is a moment magnitude, and the series rises by two orders
# of magnitude across the window. Every number here is in inputs/quakes.csv.
d <- read.csv("inputs/quakes.csv", stringsAsFactors = FALSE)
d$year <- as.integer(substr(d$time, 1, 4))
d <- d[!is.na(d$year) & !is.na(d$mag), ]

mw <- d[grepl("^mw", d$magType, ignore.case = TRUE), ]

yrs <- 1970:2025
agg <- data.frame(year = yrs,
                  quakes_mw = as.integer(table(factor(mw$year, levels = yrs))),
                  quakes_all = as.integer(table(factor(d$year, levels = yrs))))

dir.create("out", showWarnings = FALSE)
write.csv(agg, "out/quakes-mw-per-year.csv", row.names = FALSE)

png("out/quakes-mw-per-year.png", width = 900, height = 520)
plot(agg$year, agg$quakes_mw, type = "l", lwd = 2, col = "firebrick",
     xlab = "year", ylab = "earthquakes M3+ recorded (moment magnitude)",
     main = "Recorded M3+ earthquakes in California, moment magnitude, per year")
dev.off()

d1 <- sum(agg$quakes_mw[agg$year %in% 1970:1989])
d2 <- sum(agg$quakes_mw[agg$year %in% 2006:2025])
cat(sprintf("mw M3+ events 1970-1989: %d\nmw M3+ events 2006-2025: %d\nratio: %.1fx\n",
            d1, d2, d2 / d1))
cat(sprintf("ALL M3+ events 1970-1989: %d\nALL M3+ events 2006-2025: %d\nratio: %.2fx\n",
            sum(agg$quakes_all[agg$year %in% 1970:1989]),
            sum(agg$quakes_all[agg$year %in% 2006:2025]),
            sum(agg$quakes_all[agg$year %in% 2006:2025]) / sum(agg$quakes_all[agg$year %in% 1970:1989])))
