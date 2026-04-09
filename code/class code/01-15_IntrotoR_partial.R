## 2026 CRSS 8030 - Jan 15 Agenda

# Housekeeping:
## Create a folder named 01_intro at the main folder for this course, and move this script into it
## Website (bookmark it!): https://leombastos.github.io/bastoslab/teaching/2026-dsa/2026-dsa.html
## GitHub: material, constantly being updated 
## YouTube: recordings  

# Setting up RStudio:
## Tools > Global Options > General > Workspace > uncheck "Restore ...", select "Never" on dropdown menu  
## Tools > Global Options > R markdown > Show in document outline > Sections and Named Chunks


# Learning objectives ----
# - Become familiarized with using R and RStudio
# - Learn about R terminology and syntax
# - Understand different object types
# - Create a simple data, explore it with numbers and graphics
# - Learn about RStudio projects, create your own, set up proper sub-directories  


# 1) R/Rstudio ----

## Why R? 
### free, 
### runs on multiple platforms, 
### online community and support, 
### continuous development, 
### reproducible research!

## Why RStudio?
### Integrates various components of an analysis
### Colored syntax
### Syntax suggestions

## RStudio panels
### Script  
### Console
### Environment
### Files/Plots/Help

# 2) R terminology ----
# Object
37
45 / 13
a <- 45 / 32
a

b <- c(10,15,5)
b

c <- "data science"
c
## Object classes
### Data frame
d <- data.frame(number = b,
                id = c
                )
d

### Matrices
e <- matrix(c(b, b), ncol = 2)
e

### Lists 
f <- list(
  "number" = a,
  "numbers" = b,
  "word" = c,
  "data" = d
  )
f
f$data

d$number
f$data$number

class(f)
class(d)
class(d$number)
class(d$id)
## Function
mean(x = b)

## Argument
help("mean")
b2 <- c(b, NA)
b2
mean(b2)

mean(b2, TRUE)

mean(x = b2,
     na.rm = TRUE
     )

mean(na.rm=T,
     x=b2)

## Package
## Install vs. load a package
#install.packages("tibble")
library(tibble)

## Let's install package tibble, then load it

# 3) Creating a data set, exploring it ----
intro <- tribble(~name, ~height, ~dpt,
                 "Juan",168, "Food Science",
                 "Bishal",178, "Applied Econ",
                 "Emma",167,"Froestry",
                 "Taiwo",168,"Horticulture",
                 "Kriti",158,"Crop and Soils",
                 "Susmitha",165,"Ag Econ")

intro

## as.factor to change class of an object

# Check class, summary, and structure
class(intro)
summary(intro)

head(intro, n = 3)
tail(intro, n=3)
# Sampling the dataset
# First row only
intro[1,]

# First column only
intro[,1]
# Rows 1 to 3 and columns 1 to 3
intro[1:3,1:3]
# Rows 1 and 3 and columns 1 and 3
intro[c(1,3),c(1,3)]
# 4) ggplot2 philosophy and plots ---- 
#install.packages("ggplot2")
library("ggplot2")

# Point
intro

color = dpt
x = name 
y = height

ggplot(data = intro,
       mapping = aes(x = name,
                     y = height,
                     color = dpt
                     )
       ) + 
  geom_point(color = "red")

# Customizing
ggplot(data = intro,
       mapping = aes(x = name,
                     y = height,
                     color = dpt
       )
) + 
  geom_point(aes(color = dpt)) + 
  scale_color_viridis_d() +
  theme_light()
# Exporting
ggsave("plot1.png")
# 5) RStudio projects ----
## Inside of your course main folder, create a sub-folder called 02_datawrangling
## Create sub-folders data, code, output
## Create an RStudio project at the level of main folder 02_datawrangling
## Create an Rmarkdwon file (just to explore, not saving it) 

# 6) Assignment #1 - Play with ggplot
# Play with scale_color_ , explore the available options, choose one different from the one in class
# play with theme_ , explore the available options, choose one different from the one in class
# Export your new version saving it as "A1_firstname_lastname.png"
# Upload your new plot to eLC under "Assignment #1"
ggplot(data = intro,
       mapping = aes(x = name,
                     y = height,
                     color = dpt
       )
) + 
  geom_point(aes(color = dpt)) + 
  scale_color_ordinal()+
  theme_minimal()
ggsave("A1_Liming_Zhou.png",width = 6,height = 4)





