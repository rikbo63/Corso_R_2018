#tell where the data come from
datafilename="http://personality-project.org/R/datasets/R.appendix1.data"
data.ex1=read.table(datafilename,header=T)   #read the data into a table

# One Way Analysis of Variance
aov.ex1 = aov(Alertness~Dosage,data=data.ex1)  #do the analysis of variance
summary(aov.ex1)                                    #show the summary table
print(model.tables(aov.ex1,"means"),digits=3)       #report the means and the number of subjects/cell
boxplot(Alertness~Dosage,data=data.ex1)        #graphical summary


# Two way - between subject analysis of variance
datafilename="http://personality-project.org/r/datasets/R.appendix2.data"
data.ex2=read.table(datafilename,header=T)   #read the data into a table
data.ex2                                      #show the data
aov.ex2 = aov(Alertness~Gender*Dosage,data=data.ex2)         #do the analysis of variance
summary(aov.ex2)                                    #show the summary table
print(model.tables(aov.ex2,"means"),digits=3)       #report the means and the number of subjects/cell
boxplot(Alertness~Dosage*Gender,data=data.ex2) #graphical summary of means of the 4 cells


# 1 way ANOVA - within subjects

datafilename="http://personality-project.org/r/datasets/R.appendix3.data"
data.ex3=read.table(datafilename,header=T)   #read the data into a table
data.ex3                                      #show the data
aov.ex3 = aov(Recall~Valence+Error(Subject/Valence),data.ex3)
summary(aov.ex3)
print(model.tables(aov.ex3,"means"),digits=3)       #report the means and the number of subjects/cell
boxplot(Recall~Valence,data=data.ex3)          #graphical output


# Two-way Within Subjects ANOVA
datafilename="http://personality-project.org/r/datasets/R.appendix4.data"
data.ex4=read.table(datafilename,header=T)   #read the data into a table
data.ex4                                      #show the data
aov.ex4=aov(Recall~(Task*Valence)+Error(Subject/(Task*Valence)),data.ex4 )

summary(aov.ex4)
print(model.tables(aov.ex4,"means"),digits=3)       #report the means and the number of subjects/cell
boxplot(Recall~Task*Valence,data=data.ex4) #graphical summary of means of the 6 cells


# Mixed (between and Within) designs

datafilename="http://personality-project.org/r/datasets/R.appendix5.data"
data.ex5=read.table(datafilename,header=T)   #read the data into a table
data.ex5                                      #show the data
aov.ex5 = aov(Recall~(Task*Valence*Gender*Dosage)+Error(Subject/(Task*Valence))+(Gender*Dosage),data.ex5   )

summary(aov.ex5)
print(model.tables(aov.ex5,"means"),digits=3)       #report the means and the number of subjects/cell
boxplot(Recall~Task*Valence*Gender*Dosage,data=data.ex5) #graphical summary of means of the 36 cells
boxplot(Recall~Task*Valence*Dosage,data=data.ex5) #graphical summary of means of  18 cells


# Reorganizing the data for within subject analyses

datafilename="http://personality-project.org/r/datasets/recall1.data"
data=read.table(datafilename,header=TRUE)
data    #show the data

sums=data[,9:12]   #get the summary numbers
stackeds=stack(sums)   #convert to a column vector to do anova with repeated measures
#stackeds            #show the data as they are now reorganized

numcases=27                  #How many subjects are there?
numvariables=4               #How many repeated measures are there?

recall.df=data.frame(recall=stackeds,
                     subj=factor(rep(paste("subj", 1:numcases, sep=""), numvariables)),
                     time=factor(rep(rep(c("short", "long"), c(numcases, numcases)), 2)),
                     study=factor(rep(c("d45", "d90"), c(numcases*2, numcases*2)))) 

recall.df     #show the results of stacking and forming the data.frame
#now, do the within subjects ANOVA and show the results  
recall.aov= aov(recall.values ~ time * study + Error(subj/(time * study)), data=recall.df)
summary(recall.aov)             
print(model.tables(recall.aov,"means"),digits=3) 


# The next three analyses compare the effects of including the subject replication as part of the design

raw=data[,1:8]              #just trial data 
#First set some specific paremeters for the analysis -- this allows
numcases=27                  #How many subjects are there?
numvariables=8               #How many repeated measures are there?
numreplications=2            #How many replications/subject?
numlevels1=2                 #specify the number of levels for within subject variable 1
numlevels2=2                 #specify the number of levels for within subject variable 2

stackedraw=stack(raw)        #convert the data array into a vector 
#add the various coding variables for the conditions
#make sure to check that this coding is correct
recall.raw.df=data.frame(recall=stackedraw,
                         subj=factor(rep(paste("subj", 1:numcases, sep=""), numvariables)),
                         replication=factor(rep(rep(c("1","2"), c(numcases, numcases)), numvariables/numreplications)),
                         time=factor(rep(rep(c("short", "long"), c(numcases*numreplications, numcases*numreplications)),numlevels1)),
                         study=rep(c("d45", "d90") ,c(numcases*numlevels1*numreplications,numcases*numlevels1*numreplications)))


recall.aov= aov(recall.values ~ time * study + Error(subj/(time * study)), data=recall.raw.df)   #do the ANOVA
summary(recall.aov)                                #show the output
print(model.tables(recall.aov,"means"),digits=3)   #show the cell means for the anova table

#compare with the complete analysis 
recall.aov= aov(recall.values ~ time * study*replication + Error(subj/(time * study * replication)), data=recall.raw.df)   #do the ANOVA
summary(recall.aov)                                #show the output
print(model.tables(recall.aov,"means"),digits=3)   #show the cell means for the anova table


