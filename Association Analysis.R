# Amber Garner
# 10/13/2016

# Setup 
install.packages("arules")
install.packages("arulesViz")
library("arules", lib.loc="~/R/win-library/3.3")
library("arulesViz", lib.loc="~/R/win-library/3.3")
library("discretization", lib.loc="~/R/win-library/3.3")

# Load dataset
cancer <- read.csv("cancer.csv", header = T, sep = ",")

# Get descriptive stats on dataset
head(cancer)
str(cancer)
summary(cancer)

# remove id
# include only mean columns
cancer <- cancer[,2:12]

# verify subset
head(cancer)

# rename columns
names(cancer)<-c("diagnosis", "radius", "texture", "perimeter", "area", "smoothness", "compactness","concavity", "concave.points", "symmetry", "fractal_dimension")

# verify names change
names(cancer)

# convert numeric to factor
cancer$radius <- discretize(cancer$radius, "frequency", categories = 6)
cancer$perimeter<- discretize(cancer$perimeter, "frequency", categories = 6)
cancer$texture <- discretize(cancer$texture, "frequency", categories=6)
cancer$area <- discretize(cancer$area, "frequency", categories = 6)
cancer$smoothness <- discretize(cancer$smoothness, "frequency", categories = 6)
cancer$compactness <- discretize(cancer$compactness, "frequency", categories = 6)
cancer$concavity <- discretize(cancer$concavity, "frequency", categories = 6)
cancer$concave.points <- discretize(cancer$concave.points, "frequency", categories = 6)
cancer$symmetry <- discretize(cancer$symmetry, "frequency", categories = 6)
cancer$fractal_dimension <- discretize(cancer$fractal_dimension, "frequency", categories = 6)
summary(cancer)

# apriori with confidence 0.7, support 0.15, minlen 2
# only include where right hand side is diagnosis = M or B
rules <- apriori(cancer, parameter = list(conf=0.9, supp=0.15, minlen=2), appearance=list(rhs=c("diagnosis=M", "diagnosis=B"), default="lhs"))
inspect(rules[1:10])

# Find redudant rules
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)

# Remove redudant rules
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)

# Visualization 
plot(rules.pruned, control=list(main="Breast Cancer Diagnosis Rules"))
plot(rules.pruned[1:10,], method="graph", control=list(type="items", main="Breast Cancer Diagnosis Top 10 Rules"))

# *************THE END*********************
