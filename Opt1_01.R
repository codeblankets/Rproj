require(readxl)
df=read_excel("Opt01_GraceEdit.xlsx")

df$`Primary Diet`= as.factor(df$`Primary Diet`)

plot(df$`Primary Diet`) #Seeing the spread of Primary Diet across the species in our dataset. #700+ in invertebrate.

mod1 = lm (`Elevation Range` ~ `Primary Diet`, data = df)

par(mfrow = c(2,2))
plot(mod1)
shapiro.test(df$`Elevation Range`) #W = 0.96483, p-value < 2.2e-16 # NON-NORMAL

#Heteroscedasticity violated. So running Kruskal-Wallis + Dunn's posthoc test.

# Basic Kruskal-Wallis rank sum test
kruskal.test(`Elevation Range` ~ `Primary Diet`, data = df)

#Kruskal-Wallis chi-squared = 30.271, df = 10, p-value = 0.0007734
#If p<0.05, it means at least one diet category differs significantly in median elevational range from the others.

install.packages("FSA")
require(FSA)

# Run Dunn's test with Benjamini-Hochberg adjustment for multiple testing
dunnTest(`Elevation Range` ~ `Primary Diet`, 
         data = df, 
         method = "bh")


#55 pairwise cmoparisons due to the 6 diff levels. Looking at P-values....

#Only two pairs cross the significance threshold:
#Fish vs Omnivore
#Fish vs Plant

#In all of these near-significant cases below also, Fish (and to an extent, Fruit) tends to drive the differences, likely reflecting unique ecological constraints on the elevational distribution of aquatic/piscivorous or frugivorous birds compared to broader generalists.

# Fish vs Herbivore
# Fish vs Invertebrate
# Fruit vs Invertebrate

#Every other pairwise combination (e.g., Nectar vs. Seed, Carnivore vs. Insectivore, etc.) yielded adjusted p-values well above 0.05. This means that aside from the distinctions involving fish-eating birds, the other primary diet categories do not have statistically distinguishable differences in the sizes of their elevational range in this dataset.
