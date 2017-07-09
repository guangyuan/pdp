# Load required packages
library(ggplot2)
library(pdp)
library(randomForest)

# Fit a random forest to the Boston housing data
data (boston)  # load the boston housing data
set.seed(101)  # for reproducibility
boston.rf <- randomForest(cmedv ~ ., data = boston)

# Partial dependence of cmedv on lstat and rm
pd <- partial(boston.rf, pred.var = c("lstat", "rm"), chull = FALSE,
              progress = "text")

# Create image for logo (i.e., remove axis labels, etc.)
pdp <- autoplot(pd, contour = TRUE, contour.color = "black") +
  annotate("text", label = "pdp",
           x = mean(range(boston$lstat)),
           y = mean(range(boston$rm)),
           size = 15,
           color = "white") +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())
pdp  # text will look smaller

# Save logo image
ggsave("/home/w108bmg/Desktop/Dropbox/devel/pdp/tools/pdp-logo-img.png",
       plot = pdp,
       device = NULL,
       path = NULL,
       scale = 1,
       width = 1.7321,
       height = 2,
       units = "in",
       dpi = 300,
       limitsize = TRUE)
