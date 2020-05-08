library(ggplot2)

## Did Revenues decrease?
## Data manually drawn from https://www.ksrevenue.org/prannualreport.html
Krev <- data.frame(year = c(2004:2019),
                   revenue = c(4230820531, 4592296690, 5208190254, 5679536893, 5787650186, 5286354279, 4863319786, 5549205598, 6014616578, 6173763811, 5456042771, 5526841954, 5585492934, 5640971391, 6856525504, 7305556920)
)

Revplot <- ggplot(data = Krev, aes(x = year, y = revenue)) +
    geom_line(color = "deepskyblue1") +
    geom_point(color = "deepskyblue1") +
    ggtitle("Kansas Annual Revenue") +
    ylab("Revenue (in Billions)") +
    xlab("Year") +
    theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.title.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold")
        ) +
    geom_rect(aes(xmin = 2012, xmax = 2017, ymin = -Inf, ymax = Inf),
              fill = "red",
              alpha = 0.01,
              inherit.aes = FALSE
    ) +
    scale_y_continuous(labels = paste(4:8, "B"),
                        breaks = 10^9 * 4:8)

ggsave("Revplot.png", Revplot, device = "png")
## Plotting this shows a drop in 2012 when the cuts were implemented, and a sharp rise after 2016 when they were rolled back.

## Did the economy grow slower than neighboring states and the US?
## Data table generated at https://www.bea.gov/data/gdp/gdp-state
GDPgrowth <- read.csv("GDPGrowth.csv")
KGDP <- t(GDPgrowth[GDPgrowth$GeoName == "Kansas", 3:ncol(GDPgrowth)])
PGDP <- t(GDPgrowth[GDPgrowth$GeoName == "Plains", 3:ncol(GDPgrowth)])
USGDP <- t(GDPgrowth[GDPgrowth$GeoName == "United States", 3:ncol(GDPgrowth)])
GDP <- cbind(USGDP, PGDP, KGDP, c(1998:2019))
colnames(GDP) <- c("US", "Plains", "Kansas", "Year")
rownames(GDP) <- NULL
GDP <- as.data.frame(GDP)
GDP <- GDP[GDP$Year >= 2010, ]


GDPplot <- ggplot(data = GDP, aes(x = Year)) +
    geom_line(aes(y = Kansas, color = "Kansas")) +
    geom_line(aes(y = Plains, color = "Plains")) +
    geom_line(aes(y = US, color = "US")) +
    geom_point(aes(y = Kansas, color = "Kansas")) +
    geom_point(aes(y = Plains, color = "Plains")) +
    geom_point(aes(y = US, color = "US")) +
    ggtitle("Growth in Effective GDP as a % of Previous Year") +
    ylab("Growth (in percent)") +
    xlab("Year") +
    theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.title.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold")
    ) +
    geom_rect(aes(xmin = 2012, xmax = 2017, ymin = -Inf, ymax = Inf),
              fill = "red",
              alpha = 0.01,
              inherit.aes = FALSE
    ) +
    scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2017, 2018)) +
    scale_colour_manual("", 
                        breaks = c("Kansas", "Plains", "US"),
                        values = c("deepskyblue1", "mediumblue", "blueviolet"))
ggsave("GDPplot.png", GDPplot, device = "png")
## This plot does not suggest that GDP Growth was substantially affected by the Kansas Experiment


## Did bond rating decrease?
## S&P Rating changed in 2014 https://www.spglobal.com/ratings/en/research/articles/190319-history-of-u-s-state-ratings-2185306 and has not gone back up yet
## This article explains that it was because of the imbalanced budget :https://www.kansas.com/news/politics-government/article91961917.html#adnrb=900000

## Did they cut education spending?
## Data manually drawn from https://www.nasbo.org/reports-data/state-expenditure-report/state-expenditure-archives
Keduc <- data.frame(year = (2004:2019),
                    K12 = c(2627, 2784, 3082, 3315, 3576, 3682, 3584, 3824, 3824, 3714, 3742, 3809, 4553, 4449, 4600, 4962),
                    Higher = c(1711, 1843, 1958, 2050, 2222, 2316, 2258, 2424, 2424, 2428, 2546, 2590, 2674, 2750, 2804, 2829)
)
Keduc <- cbind(Keduc, total = rowSums(Keduc[ ,c("Higher", "K12")]))


Educplot <- ggplot(data = Keduc, aes(x = year)) +
    geom_line(aes(y = K12, color = "K-12 Education")) +
    geom_line(aes(y = Higher, color = "Higher Education")) +
    geom_point(aes(y = K12, color = "K-12 Education")) +
    geom_point(aes(y = Higher, color = "Higher Education")) +
    ggtitle("Kansas Annual Expenditure on Education") +
    ylab("Expenditure (in Billions") +
    xlab("Year") +
    theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.title.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold")
    ) +
    geom_rect(aes(xmin = 2012, xmax = 2017, ymin = -Inf, ymax = Inf),
              fill = "red",
              alpha = 0.01,
              inherit.aes = FALSE
    ) +
    scale_y_continuous(labels = paste(1:8, "B"),
                       breaks = 10^3 * 1:8) +
    scale_colour_manual("", 
                        breaks = c("K-12 Education", "Higher Education"),
                        values = c("deepskyblue1", "mediumblue"))
ggsave("Educplot.png", Educplot, device = "png")
## These plots do not suggest that there was a substantial effect of the tax cuts on education expenditures

## Did they cut infrastructure spending?
## 