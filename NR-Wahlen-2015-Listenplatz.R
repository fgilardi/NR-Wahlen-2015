library(ggplot2)

d <- read.csv("NR-Wahlen-2015-Proporz-Kantone.csv")

kt <- unique(d$Kanton)

tab <- data.frame(kt = NA, listenplatz = NA, coef = NA, se = NA)

for(i in 1:length(kt)){

	lp <- sort(unique(d$Listenplatz[d$Kanton == kt[i]]))
	mlp <- max(lp) - 1
	tab2 <- data.frame(kt = rep(kt[i], mlp), listenplatz = lp[-1], coef = NA, se = NA)

	out <- lm(Gewaehlt ~ as.factor(Listenplatz) + Frau + as.factor(Liste), data = subset(d, Kanton == kt[i]))
	tab2[1:mlp,3] <- summary(out)$coef[2:(mlp + 1),1]
	tab2[1:mlp,4] <- summary(out)$coef[2:(mlp + 1),2]

	tab <- rbind(tab, tab2)

}

tab$cii <- tab$coef - 1.96*tab$se
tab$cis <- tab$coef + 1.96*tab$se

tab <- subset(tab, is.na(kt) == FALSE)
tab[,3:6] <- tab[,3:6] * 100

ggplot(data = tab) +
	aes(y = coef, x = listenplatz, ymin = cii, ymax = cis) +
	geom_hline(yintercept = 0) +
	facet_wrap(~ kt, ncol = 5) +
	geom_pointrange(size = 0.2) +
	labs(title = "Wahlchancen: Unterschiede im Vergleich mit Listenplatz 1, nach Listenplatz und Kanton", subtitle = "(NR-Wahlen 2015, Proporz-Kantone, unter Berücksichtigung von Liste und Geschlecht)", x = "Listenplatz", y = "Prozentpunkte") +
	theme_light() +
	theme(strip.background = element_blank(), strip.text = element_text(color = "black"))

ggsave("wahlchance-listenplatz-nr2015.png", width = 8, height = 6)


