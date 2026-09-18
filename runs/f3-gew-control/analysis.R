# f3 — the control. The test the other side is entitled to run, run by us first.
#
# `gew` (thunderstorm day) is the second manual eyeball observation in the same daily form, made
# by the same observer at the same station on the same day. If the fall in `nebel` were an
# artefact of who is watching and how hard, `gew` would fall with it, by the same amount, on the
# same station-days. So we plot them together on the SAME station-days of the SAME balanced panel
# as f2 — a day enters only if both indicators were reported.
#
# Second figure: fog days per 100 thunderstorm days. That index divides out anything that scales
# both observations equally — observer effort, station automation, form redesign — and leaves
# whatever is specific to fog.

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

b <- d[d$station %in% stns & d$year %in% Y & !is.na(d$nebel) & !is.na(d$gew), ]
per <- data.frame(year = Y,
                  fog_days   = as.integer(tapply(b$nebel > 0, b$year, sum)),
                  storm_days = as.integer(tapply(b$gew   > 0, b$year, sum)),
                  station_days = as.integer(table(b$year)))
per$fog_per_100_storm <- round(100 * per$fog_days / per$storm_days, 1)

n5 <- function(v) mean(head(v, 5)); l5 <- function(v) mean(tail(v, 5))
pc <- function(v) 100 * (l5(v) / n5(v) - 1)

dir.create("out", showWarnings = FALSE)
write.csv(per, "out/fog-vs-storm.csv", row.names = FALSE)

SURF <- "#fcfcfb"; INK <- "#0b0b0b"; INK2 <- "#52514e"; GRID <- "#e6e5e1"
FOG  <- "#2a78d6"; STORM <- "#eb6834"

png("out/fog-vs-storm.png", width = 1000, height = 560, res = 110, bg = SURF)
par(mar = c(4.2, 5.4, 4.8, 6.0), family = "sans", col.axis = INK2, las = 1)
ymax <- max(per$fog_days, per$storm_days) * 1.10
plot(per$year, per$fog_days, type = "n", axes = FALSE, xlab = "", ylab = "", ylim = c(0, ymax))
abline(h = pretty(c(0, ymax)), col = GRID, lwd = 1)
lines(per$year, per$storm_days, lwd = 2, col = STORM)
points(per$year, per$storm_days, pch = 19, cex = 0.7, col = STORM)
lines(per$year, per$fog_days, lwd = 2, col = FOG)
points(per$year, per$fog_days, pch = 19, cex = 0.7, col = FOG)
axis(1, col = GRID, col.ticks = GRID, cex.axis = 0.85)
axis(2, col = GRID, col.ticks = GRID, cex.axis = 0.85)
# direct labels at the right-hand end, outside the plot box
par(xpd = NA)
text(tail(Y, 1), tail(per$storm_days, 1), "  thunderstorm days", adj = 0, col = STORM, font = 2, cex = 0.8)
text(tail(Y, 1), tail(per$fog_days, 1),   "  fog days",          adj = 0, col = FOG,   font = 2, cex = 0.8)
par(xpd = FALSE)
legend("bottomleft", legend = c("fog days (nebel)", "thunderstorm days (gew)"),
       col = c(FOG, STORM), lwd = 2, bty = "n", cex = 0.8, text.col = INK2, horiz = TRUE)
mtext("Fog and thunderstorms, observed by the same people on the same days",
      side = 3, line = 3.0, adj = 0, font = 2, cex = 1.15, col = INK)
mtext(sprintf("%d stations, %d–%d, days on which BOTH indicators were reported",
              length(stns), Y[1], tail(Y, 1)), side = 3, line = 1.8, adj = 0, cex = 0.85, col = INK2)
mtext(sprintf("first five years → last five years:  fog %+.0f%%,  thunderstorms %+.0f%%",
              pc(per$fog_days), pc(per$storm_days)), side = 3, line = 0.6, adj = 0, cex = 0.8, col = INK2)
par(las = 0); mtext("days per year", side = 2, line = 3.4, cex = 0.9, col = INK2); par(las = 1)
dev.off()

png("out/fog-per-storm.png", width = 1000, height = 560, res = 110, bg = SURF)
par(mar = c(4.2, 5.4, 4.8, 2.2), family = "sans", col.axis = INK2, las = 1)
ymax <- max(per$fog_per_100_storm) * 1.12
plot(per$year, per$fog_per_100_storm, type = "n", axes = FALSE, xlab = "", ylab = "", ylim = c(0, ymax))
abline(h = pretty(c(0, ymax)), col = GRID, lwd = 1)
fit <- lm(fog_per_100_storm ~ year, per)
lines(per$year, per$fog_per_100_storm, lwd = 2, col = FOG)
points(per$year, per$fog_per_100_storm, pch = 19, cex = 0.7, col = FOG)
lines(per$year, fitted(fit), lwd = 2, lty = 3, col = INK2)
axis(1, col = GRID, col.ticks = GRID, cex.axis = 0.85)
axis(2, col = GRID, col.ticks = GRID, cex.axis = 0.85)
mtext("Fog days per 100 thunderstorm days", side = 3, line = 3.0, adj = 0, font = 2, cex = 1.15, col = INK)
mtext("Anything that scales both manual observations equally divides out of this ratio",
      side = 3, line = 1.8, adj = 0, cex = 0.85, col = INK2)
mtext(sprintf("first five years → last five years: %+.0f%%   (dotted: least-squares trend, p = %.3f)",
              pc(per$fog_per_100_storm), summary(fit)$coef[2, 4]),
      side = 3, line = 0.6, adj = 0, cex = 0.8, col = INK2)
par(las = 0); mtext("fog days per 100 thunderstorm days", side = 2, line = 3.4, cex = 0.9, col = INK2); par(las = 1)
dev.off()

cat(sprintf("panel: stations %s, %d-%d\n", paste(stns, collapse = ","), Y[1], tail(Y, 1)))
for (v in c("fog_days", "storm_days", "fog_per_100_storm")) {
  f <- lm(per[[v]] ~ per$year)
  cat(sprintf("%-18s first5 %7.1f  last5 %7.1f  %+6.1f%%   slope %+7.3f/yr  p=%.2g\n",
              v, n5(per[[v]]), l5(per[[v]]), pc(per[[v]]), coef(f)[2], summary(f)$coef[2, 4]))
}
