# f4 — the horizon. The same balanced panel as f2, with the fitted trend carried forward to the
# year it reaches zero.
#
# This is the figure the claim's second clause ("on track to disappear") rests on, and it is an
# EXTRAPOLATION: nothing in the data speaks about any year after the window. The script prints the
# crossing year together with the 95% interval on the slope, and carries the same interval into
# the forward line, because a crossing year quoted without one is a number with no error bar
# pretending to be a date.

d <- read.csv("inputs/fog-nebel-gew.csv", stringsAsFactors = FALSE)
d$year <- as.integer(substr(d$time, 1, 4))

cov <- tapply(!is.na(d$nebel), list(d$year, d$station), sum); cov[is.na(cov)] <- 0
ok  <- cov >= 364; yrs <- as.integer(rownames(cov))
best <- NULL
for (y0 in seq_along(yrs)) for (y1 in seq(y0, length(yrs))) {
  keep <- colnames(cov)[apply(ok[y0:y1, , drop = FALSE], 2, all)]
  if (length(keep) >= 5) {
    cand <- list(y = yrs[y0:y1], s = keep, score = (y1 - y0 + 1) * 1000 + length(keep))
    if (is.null(best) || cand$score > best$score) best <- cand
  }
}
stns <- as.integer(best$s); Y <- best$y

b   <- d[d$station %in% stns & d$year %in% Y & !is.na(d$nebel), ]
per <- data.frame(year = Y, fog_days = as.integer(tapply(b$nebel > 0, b$year, sum)))

fit <- lm(fog_days ~ year, per)
ci  <- confint(fit, level = 0.95)
# A least-squares line pivots about the centroid, so the slope interval is carried forward as a
# rotation about (mean year, mean fog days) — taking the intercept and slope limits independently
# would treat two strongly correlated estimates as if they were free, and gives nonsense.
xbar <- mean(per$year); ybar <- mean(per$fog_days)
line  <- function(bb, yr) ybar + bb * (yr - xbar)
cross <- function(bb) xbar - ybar / bb
zero      <- cross(coef(fit)[2])
zero_fast <- cross(ci[2, 1])                 # steepest credible decline -> earliest zero
zero_slow <- cross(ci[2, 2])                 # shallowest credible decline -> latest zero

fwd <- data.frame(year = seq(Y[1], ceiling(zero_slow)))
fwd$fit  <- line(coef(fit)[2], fwd$year)
fwd$fast <- line(ci[2, 1],     fwd$year)
fwd$slow <- line(ci[2, 2],     fwd$year)

dir.create("out", showWarnings = FALSE)
write.csv(data.frame(quantity = c("slope_per_year", "slope_lo95", "slope_hi95",
                                  "zero_crossing", "zero_crossing_fast", "zero_crossing_slow"),
                     value = round(c(coef(fit)[2], ci[2, 1], ci[2, 2],
                                     zero, zero_fast, zero_slow), 2)),
          "out/horizon.csv", row.names = FALSE)

SURF <- "#fcfcfb"; INK <- "#0b0b0b"; INK2 <- "#52514e"; GRID <- "#e6e5e1"
FOG  <- "#2a78d6"; BAND <- "#cde2fb"

png("out/fog-horizon.png", width = 1000, height = 580, res = 110, bg = SURF)
par(mar = c(4.2, 5.4, 5.0, 2.2), family = "sans", col.axis = INK2, las = 1)
xr <- range(fwd$year); ymax <- max(per$fog_days) * 1.12
plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "", xlim = xr, ylim = c(0, ymax))
abline(h = pretty(c(0, ymax)), col = GRID, lwd = 1)
polygon(c(fwd$year, rev(fwd$year)), pmax(0, c(fwd$fast, rev(fwd$slow))),
        col = BAND, border = NA)
lines(per$year, per$fog_days, lwd = 2, col = FOG)
points(per$year, per$fog_days, pch = 19, cex = 0.7, col = FOG)
ff <- fwd[fwd$fit >= 0, ]
lines(c(ff$year, zero), c(ff$fit, 0), lwd = 2, lty = 3, col = INK2)
abline(v = tail(Y, 1), col = "#b8b7b2", lwd = 1, lty = 2)
axis(1, col = GRID, col.ticks = GRID, cex.axis = 0.85)
axis(2, col = GRID, col.ticks = GRID, cex.axis = 0.85)
text(tail(Y, 1), ymax * 0.97, "  last observed year", adj = 0, col = INK2, cex = 0.78)
points(zero, 0, pch = 19, cex = 1.1, col = INK)
text(zero, 0, sprintf("%.0f  ", zero), adj = c(1, -0.7), col = INK, font = 2, cex = 0.95)
mtext("Fog days per year, and the year the fitted trend reaches zero",
      side = 3, line = 3.2, adj = 0, font = 2, cex = 1.15, col = INK)
mtext(sprintf("%d stations, %d–%d, %s station-days per year. Solid: observed. Dotted: linear trend, extrapolated.",
              length(stns), Y[1], tail(Y, 1),
              paste(range(as.integer(table(b$year))), collapse = "\u2013")),
      side = 3, line = 2.0, adj = 0, cex = 0.82, col = INK2)
mtext(sprintf("Shaded: the same extrapolation at the 95%% limits of the fitted slope — zero between %.0f and %.0f.",
              zero_fast, zero_slow), side = 3, line = 0.8, adj = 0, cex = 0.82, col = INK2)
par(las = 0); mtext("fog days observed", side = 2, line = 3.4, cex = 0.9, col = INK2); par(las = 1)
dev.off()

cat(sprintf("slope %.3f fog days/year  (95%% CI %.3f to %.3f)\n", coef(fit)[2], ci[2,1], ci[2,2]))
cat(sprintf("fitted zero crossing: %.1f   (95%% slope limits: %.1f to %.1f)\n", zero, zero_fast, zero_slow))
cat("This is an extrapolation. The data say nothing about any year after", tail(Y,1), "\n")
