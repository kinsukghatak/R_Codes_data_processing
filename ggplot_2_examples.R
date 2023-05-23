
## GGPlot2 :
library("skimr")
library("here")
library("janitor")
library("dplyr")
library("palmerpenguins")
library("tidyverse")

ggplot(data=penguins)+geom_point(mapping=aes(x=flipper_length_mm,y=body_mass_g))

##ggplot() --> layer1 : every ggplot2 function starts with this. Arg is the dataset
#geom_point()--> layer2: how to represent the data (point) [geometric obj]
#mapping=aex()--> 3rd layer to define aesthetics 

ggplot(data=penguins)+geom_point(mapping=aes(x=bill_length_mm,y=bill_depth_mm))

?geom_point()

ggplot(data=penguins)+geom_point(mapping=aes(x=flipper_length_mm,y=body_mass_g))


# aes() stands for aestetics
# Other aesthtetics like colour size etc can also be mapped.
ggplot(data=penguins)+geom_point(mapping=aes(x=flipper_length_mm,
                                             y=body_mass_g,color=species))

# let's add shape as an astehteic now:
ggplot(data=penguins)+geom_point(mapping=aes(x=flipper_length_mm,
                                             y=body_mass_g,shape=species))

#Colour and shape both:
ggplot(data=penguins)+geom_point(mapping=aes(x=flipper_length_mm,
                                             y=body_mass_g,
                                             color=species,shape=species, size=bill_depth_mm))

#use alphas to make some species transparent:
ggplot(data=penguins)+geom_point(mapping=aes(x=flipper_length_mm,
                                             y=body_mass_g,
                                             alpha=species))

## Writing outside the aeS() to impact the overall code
ggplot(data=penguins)+geom_point(mapping=aes(x=flipper_length_mm,
                                             y=body_mass_g,
                                             alpha=species),color='purple')



#### Let's focus on geom() now :
# geom()--> geometrical object to rep data: points,line,bar etc

## Instead of points let's have a smooth line:
ggplot(data=penguins)+geom_smooth(mapping=aes(x=flipper_length_mm,y=body_mass_g))

## Now, let's have booth the lines and points:
ggplot(data=penguins)+
  geom_smooth(mapping=aes(x=flipper_length_mm,y=body_mass_g))+
  geom_point(mapping=aes(x=flipper_length_mm,y=body_mass_g))

# Add different line type for different species:
ggplot(data=penguins)+
  geom_smooth(mapping=aes(x=flipper_length_mm,y=body_mass_g,linetype=species))

# geom_jitter () adds noise and makes finding easier:
ggplot(data=penguins)+
  geom_jitter(mapping=aes(x=flipper_length_mm,y=body_mass_g))



## Let's now play with the diamonds dataset:
ggplot(data=diamonds)+
  geom_bar(mapping=aes(x=cut))  ## If you don't give y R automatically counts frequency for each category in X

## add new aesthetics:
ggplot(data=diamonds)+
  geom_bar(mapping=aes(x=cut,fill=clarity))


## Add Facets : the 4th layer for ggplot plotting. Facets can create separate plots for categories
ggplot(data=penguins)+
  geom_point(mapping=aes(x=flipper_length_mm,
              y=body_mass_g,color=species))+
  facet_wrap(~species)

## Facet_grid: With two variables (Organise complex data into several sub categories)

ggplot(data=penguins)+
  geom_point(mapping=aes(x=flipper_length_mm,
                         y=body_mass_g,color=species))+
  facet_grid(sex~species)


##Faceting for diamonds dataset:
ggplot(data=diamonds)+
  geom_bar(mapping=aes(x=cut,fill=cut))+
  facet_wrap(~cut)


##Finally the annotation layer (Titles, captions etc) :

ggplot(data=penguins)+
  geom_point(mapping=aes(x=flipper_length_mm,
                         y=body_mass_g,color=species))+
  labs(title="Palmer Penguins: Body mass vs Flipper length",
       subtitle = "Done on a sample",
       caption="Data collected by Dr XYZ for time period ")+
  annotate("text",x=220,y=3500,label="The Gentoos are the largest",color='purple',
           fontface='bold',size=4.5,angle=25)


## To shorten the code : some part can be stored under a variable:
p<-ggplot(data=penguins)+
  geom_point(mapping=aes(x=flipper_length_mm,
                         y=body_mass_g,color=species))+
  labs(title="Palmer Penguins: Body mass vs Flipper length",
       subtitle = "Done on a sample",
       caption="Data collected by Dr XYZ for time period ")

## Now just call p and + annotate
p+  annotate("text",x=220,y=3500,label="The Gentoos are the largest",color='purple',
             fontface='bold',size=4.5,angle=25)



### LEt us now save our plots :

ggplot(data=penguins)+
  geom_point(mapping=aes(x=flipper_length_mm,
                         y=body_mass_g,color=species))
# You can save using export option as image or pdf file

## Save using ggsave()
ggsave("three penguin specieis.png")


