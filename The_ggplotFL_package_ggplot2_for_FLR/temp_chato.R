
# Adding stuff to exisitng GGPLOTS

fqs <- FLQuants(F = rlnorm(200, fbar(ple4), 0.15), 
     SSB = ssb(ple4), Rec = rec(ple4), Catch = catch(ple4))

plot(fqs) + stat_smooth()

plot(fqs) + stat_smooth(method = "lm")

plot(fqs) + geom_smooth(span = 0.3)

data("nsher")


# Create a plot for FLindex

str(ple4)

pi <- as.data.frame(ple4.indices)


# Plot time series by survey

# Plot time series of Index, aggr over ages, scale
a <- ggplot(
  ddply(pi[pi$slot == "index",],.(year,  cname), summarize, 
        data = sum(data/max(data))),
       aes(year, data, col = cname))+
        geom_line()+ylab("index")

# Plot index by Age
b <- ggplot(
  pi[pi$slot == "catch.n",],  aes(year, data, col = age))+geom_line()+ylab("index")+ geom_point()+
  facet_grid(age ~ cname, scales = "free")


# Plot timeline of each survey
# it's not in the data frame, need to take it from the FLIndices

#ggplot(as.data.frame(c(0,12))) + geom_vline(xintercept = as.data.frame(range(ple4.indices[[1]])[6:7])*12)


val <- lapply(ple4.indices, function(x) (range(x)))
val <- as.data.frame(do.call('rbind', val))

val <- cbind(name = rownames(val), val)
rownames(val) <- 1:nrow(val)

# Plot Time of the survey
#ggplot(val, aes(col = name))+ geom_vline(xintercept = val$startf*12, col = "green") + geom_vline(xintercept = val$endf*12, col = "red") + facet_grid(~ name) +  xlim(0,13) + xlab ("Month")

# Intersect Time of survey and Max age

c <- ggplot(val, aes(fill = name)) + 
  geom_hline(aes(yintercept = min), val)+
  geom_hline(aes(yintercept = max), val)+
  geom_hline(aes(yintercept = plusgroup), val)+
  geom_vline(aes(xintercept = startf*12, col = "green"),val) +
geom_vline(aes(xintercept = endf*12, col = "blue"),val)+  
facet_grid(~ name)+
  xlim(0,13) + xlab ("Month")

c <- ggplot(val) + 
  geom_point(aes (x = min, y = name, col = "green"), val,position = "dodge")+
  geom_point(aes( x = max, y = name, col = "blue"), val,position = "dodge")+
  geom_point(aes(x = plusgroup, y = name, col = "red"), val,position = "dodge")+
  #coord_flip()+ 
  xlim(0,13) + xlab ("Month")+ ylab("") 
  #facet_grid( name ~ .)
  

# Plot ACF for survey index

acf(pi[pi$slot == "catch.n",], plot = TRUE)



# Combine

grid.arrange(a,b,c)
