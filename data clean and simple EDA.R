## midterm project

library(tidyverse)
library(magrittr)
library(readxl)

strawb <- read_xlsx("/Users/brenda/Downloads/strawberries-2022oct30-a.xlsx")
#strawb

## Get the column names and index them
cnames <- colnames(strawb)
x <- 1:dim(strawb)[2]

#delete useless columns
T <- NULL
for(i in x){T <- c(T, dim(unique(strawb[i]))[1])}
drop_cols <- cnames[which(T == 1)]
strawb %<>% dplyr::select(!all_of(drop_cols))
strawb %<>% arrange(Year, State)

#sepereate many information in a unit
strawb %>% separate(col=`Data Item`,
                    into = c("Strawberries", "type", "items", "units"),
                    sep = ",",
                    fill = "right")

## Plot-Chemicals used in different states
plot2.data<-strawb %>% 
    filter(
        str_detect(`Domain Category`,'(CHEMICAL|FERTILIZER)'),
        #State=='CALIFORNIA',
        str_detect(Value,'^\\d+$')) %>%
    filter(Value != "(NA)" & Value != "(D)" ) %>%
    mutate(Value=as.numeric(Value),
           State= as.character(State),
           Domain=ordered(Domain,c('CHEMICAL, HERBICIDE','CHEMICAL, INSECTICIDE','CHEMICAL, FUNGICIDE','CHEMICAL, OTHER','FERTILIZER')))


ggplot(plot2.data,aes(x=State,y=Value,fill=Domain))+
    geom_bar(stat='identity',width = 0.6,position= 'dodge')+
    theme_bw()+ 
    scale_y_continuous(trans='log10')+
    scale_fill_hue()+labs(title='CHEMICAL usage in different states')

plot2.data %>% group_by(State,Domain) %>% summarise(sum(as.numeric(Value)))

## Plot-STRAWBERRIES - PRICE RECEIVED
plot3.data<-strawb %>% 
    filter(type=='STRAWBERRIES - PRICE RECEIVED') %>%
    mutate(Value=as.numeric(Value),
           Year=as.character(Year))



ggplot(plot3.data,aes(y=State,x=Year,size=Value,fill=State))+
    geom_point(shape=21)+
    theme_bw()+
    geom_text(aes(label=Value),size=4,nudge_y=0.2)+
    scale_size_continuous(range=c(2,16))+
    labs(size='PRICE RECEIVED',title='STRAWBERRIES - PRICE RECEIVED')


## seperate form into three subsets: oranic, non organic -- and commercial vs chemicals in each
# 1. chemicals used in strawberry cultivation (pesticides, insecticides, fertilizers, fungicides, herbicides, and others)
# 2. sales of organic strawberries
# 3. sales of non-organic strawberries
type_organic <- grep("organic", 
                     strawb$type, 
                     ignore.case = T)

items_organic <- grep("organic", 
                      strawb$items, 
                      ignore.case = T)  ## nothing here

Domain_organic <- grep("organic", 
                       strawb$Domain, 
                       ignore.case = T)

Domain_Category_organic <- grep("organic", 
                                strawb$`Domain Category`, 
                                ignore.case = T)
org_rows <- intersect(type_organic, Domain_organic)
# organic and non-organic strawberries
strawb_organic <- strawb %>% slice(org_rows, preserve = FALSE)
strawb_non_organic <- strawb %>% filter(!row_number() %in% org_rows)

chem_rows <- grep("BEARING - APPLICATIONS", 
                  strawb_non_organic$type, 
                  ignore.case = T)
chem_rows_1 <- grep("chemical", 
                    strawb_non_organic$Domain, 
                    ignore.case = T)
ins <- intersect(chem_rows, chem_rows_1)
chem_rows_2 <- grep("chemical", 
                    strawb_non_organic$`Domain Category`, 
                    ignore.case = T)
ins_2 <- intersect(chem_rows, chem_rows_2)
# chemical used in strawberry cultivation
strawb_chem <- strawb_non_organic %>% slice(chem_rows, preserve = FALSE)
## drop useless columns
before_cols = colnames(strawb_chem)
T = NULL
x = length(before_cols)

for(i in 1:x){
  b <- length(unlist(strawb_chem[,i] %>% unique()) )
  T <- c(T,b)
}

drop_cols <- before_cols[which(T == 1)]
strawb_chem %<>% dplyr::select(!all_of(drop_cols))
after_cols = colnames(strawb_chem)
temp1 <- strawb_chem %>% dplyr::select(units) %>% distinct()
strawb_chem %<>% separate(col=`Domain Category`, 
                          into = c("dc1", "chem_name"),
                          sep = ":", 
                          fill = "right")
aa  <- grep("measured in", 
            strawb_chem$items, 
            ignore.case = T)
length(aa)

temp1 <- strawb_chem %>% dplyr::select(chem_name) %>% unique()
length(unlist(temp1))

sum(strawb_chem$Domain == strawb_chem$dc1) == dim(strawb_chem)[1]

strawb_chem %<>% dplyr::select(Year, State, items, units, dc1, chem_name, Value)
strawb_chem %<>% rename(category = units)
strawb_chem$items <- str_remove_all(strawb_chem$items, "MEASURED IN ")

strawb_chem %<>% rename(units = items)

## Do all the dc1 entries begen with "Chemical"?

bb  <- grep("CHEMICAL, ", 
            strawb_chem$dc1, 
            ignore.case = T)
length(bb)
chem <- 1:2112

non_chem_rows <- setdiff(chem, bb)
length(non_chem_rows)

## on let's look at these rows in a tibble

temp1 <- strawb_chem %>% slice(non_chem_rows)

### !! fertilizers  

## keep them -- probably won't use them as a lone tibble

fertilizers <- temp1


## now remove "CHEMICAL, " from the entries in the dc1
## and rename the column chem_types


strawb_chem$dc1 <- str_remove_all(strawb_chem$dc1, "CHEMICAL, ")

strawb_chem$dc1 %>% unique()

strawb_chem %<>% rename(chem_types = dc1)


## Now let's get the units and categories sorted out
## we can see that the chemicals appear many times each
## to investigate, pick one

bb  <- grep("BIFENTHRIN", 
            strawb_chem$chem_name, 
            ignore.case = T)

bifen <- strawb_chem %>% slice(bb)

## now look at the befen tibble you just made

## now fix the chem_name column

## remove the parens

strawb_chem$chem_name <- str_remove_all(strawb_chem$chem_name, "\\(")

strawb_chem$chem_name <- str_remove_all(strawb_chem$chem_name, "\\)")

## separate chem_name and chem_code

strawb_chem %<>% separate(col = chem_name,
                          into = c("chem_name","chem_code"),
                          sep = "=",
                          fill = "right"
) 


## now fill in a label fot NA in the category column

## first check that "lb" in the units column corresponds 
## to NA in the category column


aa <- which(strawb_chem$units == " LB")

bb <- which(is.na(strawb_chem$category))

sum(aa==bb)==length(aa)

##-----------
#(below are some data cleaning and thinking by myself)

rm(aa,after_cols,b,bb,before_cols,chem,chem_rows,chem_rows_1,chem_rows_2,cnames,
   i,drop_cols,x,T,type_organic,org_rows,ins,ins_2,items_organic,non_chem_rows,
   Domain_Category_organic,Domain_organic,temp1)

## fumigant always be mentioned by articles are 
## Carbendazim, Bifenthrin, methyl bromide, 1,3-dichloropropene, chloropicrin, Telone
## good ways also need to write(not finish)

##state in data
unique(strawb$State)
## According to articles,
##"CALIFORNIA" and "FLORIDA" are majority two places to produce strawberry;
## "NEW JERSEY", "NEW YORK" and "PENNSYLVANIA" can put togather as Northeast and Mid-Atlantic United States (Northeast region);
## "OREGON"

##according to articles, because of the improvement of people's health awareness,
## organic strawberries will be more and more important in the future.
## So, we explore organic strawberry subset first.
unique(strawb_organic$State)
## all states mentation in strawberry dataset plant organic strawberry, that means all of
# states realize people want to keep healthy and plant relevant things to cater costumers.

##begin to clean organic data
temp1 <- strawb_organic %>% dplyr::select(Year,State) %>% distinct()
temp1
unique(strawb_organic$`Domain`)
unique(strawb_organic$`Domain Category`)
unique(strawb_organic$units)
strawb_organic$units <- str_remove_all(strawb_organic$units, "MEASURED IN ")

#clean organic strawberry data
strawb_organic <- strawb_organic[!names(strawb_organic) %in% c("Period","State ANSI","Strawberries", "Domain", "Domain Category")]
#type, items and units need to be cleaned

##begin to clean non_organic data
unique(strawb_non_organic$State) # "CALIFORNIA" "FLORIDA"    "NEW YORK"   "OREGON"

unique(strawb_non_organic$Strawberries) 
#"STRAWBERRIES - PRICE RECEIVED" "STRAWBERRIES"
strawb_non_organic[strawb_non_organic$Strawberries=="STRAWBERRIES - PRICE RECEIVED",]
#all in marketing year

unique(strawb_non_organic$type) 
# " MEASURED IN $ / CWT"; " FRESH MARKET - PRICE RECEIVED";
#" PROCESSING - PRICE RECEIVED"; " BEARING - APPLICATIONS" 
strawb_non_organic %>% dplyr::select(type, items) %>% unique()
strawb_non_organic[strawb_non_organic$Strawberries=="STRAWBERRIES - PRICE RECEIVED",]
strawb_non_organic[strawb_non_organic$type==" MEASURED IN $ / CWT",]
length(which(is.na(strawb_non_organic$items)))
#when Strawberries==STRAWBERRIES - PRICE RECEIVED, tyoe==MEASURED IN $ / CWT, items==NA
#at this time, we will see all Period=="MARKETING YEAR". So, is there relationship between them?
x5 <- strawb_non_organic[strawb_non_organic$Period=="MARKETING YEAR",]
#No. About all Period=="MARKETING YEAR", Domain=="TOTAL", types==	"NOT SPECIFIED"
#if we do not have "MARKETING YEAR" is there some pattern about value?
x6 <- setdiff(strawb_non_organic,x5)
#only marketing year have " MEASURED IN $ / CWT", " FRESH MARKET - PRICE RECEIVED" and " PROCESSING - PRICE RECEIVED" type;
# year only have " BEARING - APPLICATIONS" type
#So, we can divide subset into marketing year and year

unique(strawb_non_organic$items) 
strawb_non_organic$items <- str_remove_all(strawb_non_organic$items, "MEASURED IN ")
#do not know how to clean

unique(strawb_non_organic$units) # same operation just like chem 
#delete directly

unique(strawb_non_organic$Domain)
#delete

unique(strawb_non_organic$`Domain Category`)
strawb_non_organic %<>% separate(col=`Domain Category`, 
                          into = c("types", "name"),
                          sep = ":", 
                          fill = "right")
strawb_non_organic %>% dplyr::select(name) %>% unique()
sum(strawb_non_organic$Domain == strawb_non_organic$types) == dim(strawb_non_organic)[1]
#No. Now, know the difference between them
x1 <- strawb_non_organic[(strawb_non_organic$Domain == strawb_non_organic$types) == FALSE,]
#All the Domain==TOTAL and types == NOT SPECIFIED
#delete Domain, keep types.
## Do all the types entries begen with "Chemical"?
x2 <- grep("CHEMICAL, ", 
           strawb_non_organic$types, 
            ignore.case = T)
length(x2)
##if they are not entries begen with "Chemical", what kind of thing they begin with?
x3 <- strawb_non_organic[grepl("CHEMICAL, ",strawb_non_organic$types),]
nrow(x3)
x4 <- setdiff(strawb_non_organic, x3)
View(x4)
unique(x4$types)
#"NOT SPECIFIED" "FERTILIZER" 
#at this time, we can divide subset into chemical and FERTILIZER

#deal with names
strawb_non_organic %>% dplyr::select(name) %>% unique()
#remove the parens
strawb_non_organic$name <- str_remove_all(strawb_non_organic$name, "\\(")
strawb_non_organic$name <- str_remove_all(strawb_non_organic$name, "\\)")
## separate name and code
strawb_non_organic %<>% separate(col = name,
                          into = c("name","code"),
                          sep = "=",
                          fill = "right"
) 

#delete useless columns
strawb_non_organic <- strawb_non_organic[!names(strawb_non_organic) %in% c("State ANSI","types")]

#final slice non-organic strawberries dataset into 3 dataset:
#first, slice data according to Period (marketing year and year);
#Next, slice year data according to Domain or chemical
strawb_non_organic_my <- strawb_non_organic[strawb_non_organic$Period=="MARKETING YEAR",]
strawb_non_organic_y <- setdiff(strawb_non_organic, strawb_non_organic_my)
strawb_non_organic_chemical <- strawb_non_organic[grepl("CHEMICAL, ",strawb_non_organic$Domain),]
strawb_non_organic_fertilizers <- setdiff(strawb_non_organic_y, strawb_non_organic_chemical)


#now, we have all datasets: 
#strawb_organic
#strawb_non_organic_my
#strawb_non_organic_chemical
#strawb_non_organic_fertilizers



##However, there is still something wrong with data: 
#1. type, type, items and units need to be cleaned
#2. how to analysis CV and value

##during analysis:
#We also need to discuss about advantage and disadvantage chemicals in data.

#=========
#EDA:
#organic strawberry[use e1 plot a picture]
e1 <- strawb_organic[strawb_organic$type==" ORGANIC - SALES", ]
#View(e1)
#NEW JERSEY have no data about Value in 2019. But we can see its CWT decrease. So, we give this
##missing data a value according to proportion.
e1$Value <- as.numeric(e1$Value)
e1[17, "Value"] = 73017.4
e1m <- e1[e1$items == " MEASURED IN $", ]
e1w <- e1[e1$items == " MEASURED IN CWT", ]
datao <- data.frame(Year = e1$Year,State = e1$State, Value = e1$Value)
#first
data1 <- e1m %>% dplyr::select("Year", "State", "Value")
t1 <- e1m %>% pivot_wider(names_from = State, values_from = Value)
library(fmsb)
e1 %>% pivot_longer(c("CALIFORNIA","FLORIDA","NEW JERSEY","NEW YORK","OREGON","PENNSYLVANIA"))

#radarchart(e1, title = "123", pfcol = scales::alpha("gray", 0.3), plwd = 2)
colors_fill <- c(scales::alpha("gray", 0.2), scales::alpha("gold", 0.3))
colors_line <- c(scales::alpha("darkgray", 0.9), scales::alpha("gold", 0.9))
radarchart(e1, title = "123", pcol = colors_line, pfcol = colors_fill, plwd = 2)
legend(x = 0.6, y = 1.35, legend = rownames(c(2016,2019)), bty = "n", pch = 20, col = colors_line,
       cex = 1.2, pt.cex = 3)

# Reduce plot margin using par()
op <- par(mar = c(1, 2))
# Create the radar charts
create_beautiful_radarchart(
  data = df, caxislabels = c(0, 5, 10, 15, 20),
  color = c("#00AFBB", "#E7B800", "#FC4E07")
)
# Add an horizontal legend
legend(
  x = "bottom", legend = rownames(df[-c(1,2),]), horiz = TRUE,
  bty = "n", pch = 20 , col = c("#00AFBB", "#E7B800", "#FC4E07"),
  text.col = "black", cex = 1, pt.cex = 1.5
)
par(op)
ggplot(data = e1)+
  geom_line(aes(x=Year,y=Value,color=State))+
  scale_x_continuous(breaks = c(2016,2019))+
  scale_y_log10()

###non_organic:
##marketing year:
e2 <- strawb_non_organic_my[strawb_non_organic_my$type==" MEASURED IN $ / CWT", ]
#View(e2)
#plot value about year state 
##second
###yzhou arrange
ggplot(data = e2, aes(x = Year, y = Value))+
  geom_line(aes(group = State, color = State))+
  labs(title = NULL,x = "STRAWBERRIES - PRICE" , y = "Year")

##fertilizers
unique(strawb_non_organic_fertilizers$State) #2
unique(strawb_non_organic_fertilizers$name) #4
#strawb_non_organic_fertilizers %>% dplyr::select(items, units) %>% distinct()
#AVG only shows in " LB / ACRE / APPLICATION" and " LB / ACRE / YEAR"
#choose one
# e3 <- strawb_non_organic_fertilizers[strawb_non_organic_fertilizers$items==" LB", ]
# e3 <- strawb_non_organic_fertilizers[strawb_non_organic_fertilizers$items==" LB / ACRE / APPLICATION", ]
e3 <- strawb_non_organic_fertilizers[strawb_non_organic_fertilizers$items==" LB / ACRE / YEAR", ]
#plot items any type plot a picture 
#third
#dark and light

##chemical
unique(strawb_non_organic_chemical$State) #"CALIFORNIA" "FLORIDA"    "OREGON"  
unique(strawb_non_organic_chemical$items) #" LB" " LB / ACRE / APPLICATION" " LB / ACRE / YEAR"  
#strawb_non_organic_chemical %>% dplyr::select(items, units) %>% distinct()
#AVG only shows in " LB / ACRE / APPLICATION" and " LB / ACRE / YEAR"
e4 <- strawb_non_organic_chemical %>% separate(col=`Domain`,
                    into = c("category", "detail"),
                    sep = ",",
                    fill = "right")
#View(e4)
unique(e4$category) #1
unique(e4$items) ##" LB" " LB / ACRE / APPLICATION" " LB / ACRE / YEAR"  
unique(e4$detail)  #" FUNGICIDE"   " HERBICIDE"   " INSECTICIDE" " OTHER"   
nrow(e4 %>% dplyr::select(detail, name) %>% distinct()) #171
e4 %>%
  group_by(detail) %>%
  count(detail)
length(unique(e4$name)) #168
## Carbendazim, Bifenthrin, methyl bromide, 1,3-dichloropropene, chloropicrin, Telone
df_carbendazim <- grep("carbendazim", 
                       e4$`name`, ignore.case = T)
length(df_carbendazim) #0
df_Bifenthrin <- grep("Bifenthrin", 
                       e4$`name`, ignore.case = T)
length(df_Bifenthrin) #27 #INSECTICIDE
df_methyl_bromide <- grep("methyl bromide", 
                      e4$`name`, ignore.case = T)
length(df_methyl_bromide) #3 #other
df_1_3_dichloropropene <- grep("1,3-dichloropropene", 
                               e4$`name`, 
                               ignore.case = T)
length(df_1_3_dichloropropene) #0
df_chloropicrin <- grep("chloropicrin", 
                        e4$`name`, 
                        ignore.case = T)
length(df_chloropicrin) #18 #other
df_Telone <- grep("Telone", 
                  e4$`name`, 
                  ignore.case = T)
length(df_Telone) #0
e5 <- e4 %>%
  group_by(detail) %>%
  count(name) %>%
  arrange(detail,desc(n)) %>%
  slice_max(n, n = 5)
view(e5)
e6 <- e4 %>%
  group_by(detail) %>%
  count(name) %>%
  arrange(detail,desc(n)) %>%
  filter(n>=10) 
e7 <- e6 %>%
  group_by(detail) %>%
  count(detail)
##bar chart, show the form of top 5 chemicals people usually use.(e5)
##fourth
e5 %>%
  ggplot(aes(n,fct_reorder(name, n),fill = detail)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~detail, ncol = 2, scales = "free") +
  labs(x = "count", y = NULL)


## Edited by Tianjian Xie
## Part of plotting circular barplot

## Dataset
## Drop NAs and Ds
NA_rows <- grep("(NA)",
                strawb_non_organic_chemical$Value,
                ignore.case = T)
D_rows <- grep("(D)",
               strawb_non_organic_chemical$Value,
               ignore.case = T)
California_non_Organic <- grep("CALIFORNIA",
                               strawb_non_organic_chemical$State,
                               ignore.case = T)
Year_non_Organic <- grep("2016",
                         strawb_non_organic_chemical$Year,
                         ignore.case = T)
ins <- intersect(California_non_Organic, Year_non_Organic)
used_NA_rows <- intersect(ins, NA_rows)
used_D_rows <- intersect(ins, D_rows)
uncleaned_rows <- sort(c(used_NA_rows, used_D_rows), decreasing = FALSE)
#Clean Dataset
cleaned_rows <- setdiff(ins, uncleaned_rows)
cleaned_strawb_non_organic <- strawb_non_organic_chemical %>% slice(cleaned_rows)
LB_Year_App <- grep("LB / ACRE / APPLICATION",
                    cleaned_strawb_non_organic$items,
                    ignore.case = T)
LB_Year_App_data <- cleaned_strawb_non_organic %>% slice(LB_Year_App)

strawb_non_organic_chemical1 <- LB_Year_App_data %>% arrange(Domain)
# Create dataset
data <- data.frame(
  individual=c(LB_Year_App_data$name),
  group=c( rep('FUNGICIDE', 24), rep('HERBICIDE', 3), rep('INSECTICIDE', 24), rep('OTHER', 4)),
  value=round(as.numeric(LB_Year_App_data$Value),4)
)

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 3
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$group <- rep(levels(data$group), each=empty_bar)
data <- rbind(data, to_add)
data <- data %>% arrange(group)
data$id <- seq(1, nrow(data))

# Get the name and the y position of each label
label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- data %>% 
  group_by(group) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]


# Plotting 
# Make the plot
p <- ggplot(data, aes(x=as.factor(id), y=value, fill=group)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=1, size=0.03 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=1, size=0.03 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.03 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.03 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(data$id),4), y = c(20, 40, 60, 80), label = c("20", "40", "60", "80") , color="grey", size=1 , angle=0, fontface="bold", hjust=1) +
  
  geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
  ylim(-100,120) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=1.25, angle= label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -18, label=group), hjust=c(1,1,0,0), colour = "black", alpha=0.8, size=2, fontface="bold", inherit.aes = FALSE)

p


