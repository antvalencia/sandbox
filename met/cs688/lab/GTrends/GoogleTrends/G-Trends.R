# Anthony Valencia G-Trends Lab Project
setwd("/Users/anthony.valencia/misc/code/met/cs688/lab/GTrends/GoogleTrends")

df <- read.table(
  "Data/geoMap.csv", 
  header = TRUE,
  sep = ",",
  stringsAsFactors = FALSE
)
names(df) <- c("region", "GB", "GG")

# – Q1: Which are the states where GG is smaller than 1? Find those and replace them with zero.
df$region[df$GG == '<1']
# South Dakota, Maine, Idaho
df$GG[df$GG == '<1'] <- 0
df$GG <- as.numeric(df$GG)

# – Q2: For How Many States GB > GG?
length(which(df$GB>df$GG))
# all 46

# – Q3: Find any states where GG+10 > GB
df$region[df$GG+10 > df$GB]
# Washington

# – Q4: What is the % of states for which GG+10 > GB?
100*length(which(df$GG+10 > df$GB))/nrow(df)
# 2.173913%

# – Q5: What is the ratio GG/GB for the state of New Hampshire? 
df$GG[df$region == "New Hampshire"]/df$GB[df$region == "New Hampshire"]
# 0.5

# – Q6: Create a Bar Plot of GG & GB values for each state.
cols <- c('red','blue');
ylim <- c(0,max(df[c('GG','GB')])*1.8);
par(lwd=6);
barplot(
  t(df[c('GG','GB')]),
  beside=T,
  ylim=ylim,
  border=cols,
  col='white',
  names.arg=df$region,
  xlab='region',
  ylab='gift',
  legend.text=c('GG','GB'),
  args.legend=list(text.col=cols,col=cols,border=cols,bty='n')
);
box();