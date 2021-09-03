library(sf)
library(sp)
library(rgdal)
library(ggplot2)
library(stringr)
library(dplyr)

points <- readOGR("/Users/rragankonywa/OneDrive/UniWurzburg/EAGLES/Semester2/field Mesurements/Vector data/Survey_points_first_route.shp")
points$notes
# rename the plot ids to have all capital F and capital P
xx <- str_replace(string = plotNames,pattern = "F1p1",replacement ="F1P1" )

x <- grep(x = plotNames2, pattern = "(([F])([1-2]{1}))", value = T)
points$notes <- xx

# Remove unecessary points
points_clean <- points[-c(1,28), ]
points_clean$notes

comments <- read.csv("/Users/rragankonywa/OneDrive/UniWurzburg/EAGLES/Semester2/field_Mesurements/Comments.csv")


# change the names of tyhe plot id to be similar in both datasets
# important to make a spatial joint
names(points_clean)[3] <- names(comments)[1]





Percentage <- list()

  
# compute dominant species percentage for each plot  
for(i in 1:nrow(comments)){
  Percentage[[i]] <- ((max(comments[i, ][-c(1, ncol(comments))]))/(sum(comments[i, ][-c(1, ncol(comments))])))*100
}  


  # find the dominant species for each plot
Dominant_species <- colnames(comments)[apply(comments,1, which.max)]

# group the data into forest type based on the dominant tree spss
forest_type <- list()
for(i in 1:length(Dominant_species)){
  if(Dominant_species[i] %in% c("Oak","Beech","Esche","Eiche","Marple")){
    forest_type[[i]] <- "Broad_leaf"
  } else {
    if(Dominant_species[i] %in% c("Dead","Logged")){
      forest_type[[i]] <- "Dead"
    } else{
      forest_type[[i]] <- "Needle_leaf"
    }
  }
}



# Add the data to the bigger comments dataframe.

comments_complete <- comments %>% mutate(Dominant_Species=Dominant_species, 
                    Dominant_Species_Percentage=unlist(Percentage),
                    Forest_type=unlist(forest_type),.before="Notes")

merged <- merge(points_clean, comments_complete, by = "plot_ID")


writeOGR(merged,dsn ="/Users/rragankonywa/OneDrive/UniWurzburg/EAGLES/Semester2/field Mesurements",layer = "Merged3",driver = "ESRI Shapefile")


