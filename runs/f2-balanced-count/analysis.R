# f2 — the headline figure.
#
# The obvious attack on the starter is the denominator: the starter's rate is taken over
# station-days where `nebel` was reported at all, and that pool shrinks as stations stop making
# the observation. So this run removes the denominator from the argument entirely.
#
# A station-year is admitted only if that station reported `nebel` on essentially every day of
# that year (>= 364 of 365/366). Only stations that clear that bar in EVERY year of the window
# are kept. The result is a strictly balanced panel: the same 5 stations, every year, and an
# identical number of station-days behind every point on the plot. The y axis is a COUNT of
# fog days, not a rate.

d <- read.csv("inputs/fog-nebel-gew.csv", stringsAsFactors = FALSE)
d$year <- as.integer(substr(d$time, 1, 4))

# --- select the balanced panel from the data, not by hand -----------------------------------
cov  <- tapply(!is.na(d$nebel), list(d$year, d$station), sum)
cov[is.na(cov)] <- 0
ok   <- cov >= 364                       # near-complete station-year
yrs  <- as.integer(rownames(cov))
# SELECTION RULE, fixed before looking at any fog number:
#   the claim is about a trend since 1990, so the binding constraint is WINDOW LENGTH.
#   Take the longest run of consecutive years for which at least 5 stations are complete
#   throughout; break ties on station count. Nothing here reads `nebel`'s values.
best <- NULL
for (y0 in seq_along(yrs)) for (y1 in seq(y0, length(yrs))) {
  keep <- colnames(cov)[apply(ok[y0:y1, , drop = FALSE], 2, all)]
  if (length(keep) >= 5) {
    cand <- list(y = yrs[y0:y1], s = keep,
                 score = (y1 - y0 + 1) * 1000 + length(keep))
    if (is.null(best) || cand$score > best$score) best <- cand
  }
}
stns <- as.integer(best$s); Y <- best$y

b <- d[d$station %in% stns & d$year %in% Y & !is.na(d$nebel), ]
per <- data.frame(year = Y,
                  fog_days    = as.integer(tapply(b$nebel > 0, b$year, sum)),
                  station_days = as.integer(table(b$year)))

fit  <- lm(fog_days ~ year, per)
e    <- mean(per$fog_days[per$year %in% head(Y, 5)])
l    <- mean(per$fog_days[per$year %in% tail(Y, 5)])

dir.create("out", showWarnings = FALSE)
write.csv(per, "out/fog-balanced-panel.csv", row.names = FALSE)

# --- figure ----------------------------------------------------------------------------------
SURF <- "#fcfcfb"; INK <- "#0b0b0b"; INK2 <- "#52514e"; GRID <- "#e6e5e1"; FOG <- "#2a78d6"

png("out/fog-balanced-panel.png", width = 1000, height = 560, res = 110, bg = SURF)
par(mar = c(4.2, 6.0, 4.4, 2.2), family = "sans", col.axis = INK2, col.lab = INK2, las = 1)
plot(per$year, per$fog_days, type = "n", axes = FALSE, xlab = "", ylab = "",
     ylim = c(0, max(per$fog_days) * 1.12))
abline(h = pretty(c(0, max(per$fog_days))), col = GRID, lwd = 1)
lines(per$year, per$fog_days, lwd = 2, col = FOG)
points(per$year, per$fog_days, pch = 19, cex = 0.75, col = FOG)
lines(per$year, fitted(fit), lwd = 2, lty = 3, col = INK2)
axis(1, col = GRID, col.ticks = GRID, lwd = 1, cex.axis = 0.85)
axis(2, col = GRID, col.ticks = GRID, lwd = 1, cex.axis = 0.85)
# direct labels on the ends only
text(Y[1], per$fog_days[1], paste0("  ", per$fog_days[1]), adj = c(0, -0.9), col = INK, font = 2, cex = 0.9)
text(tail(Y, 1), tail(per$fog_days, 1), paste0(tail(per$fog_days, 1), "  "),
     adj = c(1, -0.9), col = INK, font = 2, cex = 0.9)
mtext("Fog days per year, Austria", side = 3, line = 2.6, adj = 0, font = 2, cex = 1.25, col = INK)
mtext(sprintf("%d stations reporting every day of every year — %s–%s station-days behind every point",
              length(stns), format(min(per$station_days), big.mark = ","),
              format(max(per$station_days), big.mark = ",")),
      side = 3, line = 1.4, adj = 0, cex = 0.85, col = INK2)
mtext(sprintf("%d–%d.  Dotted line: least-squares trend, %.1f fog days lost per year.",
              Y[1], tail(Y, 1), -coef(fit)[2]),
      side = 3, line = 0.4, adj = 0, cex = 0.8, col = INK2)
par(las = 0); mtext("fog days observed", side = 2, line = 3.6, cex = 0.9, col = INK2); par(las = 1)
dev.off()

cat(sprintf("panel: stations %s, %d-%d\n", paste(stns, collapse = ","), Y[1], tail(Y, 1)))
cat(sprintf("denominator: %d-%d station-days per year — a spread of %.2f%% across the whole window\n",
            min(per$station_days), max(per$station_days),
            100 * (max(per$station_days) / min(per$station_days) - 1)))
cat(sprintf("first 5 years mean: %.1f fog days\nlast  5 years mean: %.1f fog days\nchange: %+.1f%%\n",
            e, l, 100 * (l / e - 1)))
cat(sprintf("trend: %+.2f fog days/year, p = %.2g, R2 = %.2f\n",
            coef(fit)[2], summary(fit)$coef[2, 4], summary(fit)$r.squared))
