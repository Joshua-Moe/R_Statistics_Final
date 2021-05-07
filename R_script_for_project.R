setwd("C:/UNC_Fall_2019/BINF_6200/Project")
Infant_cord_data = read.csv("Song_DCC_PlosOne_datadryad9.16.15.csv")

#Hypothesis 1: Infants born with a DCC delay of 60-75s have a higher rate of death than those born with 
# a DDC 30-45s delay.

# Ho: p2 - p1 =< 0     Ha: P2 - p1 > 0    alpha = 0.05

DCC_30_to_45_sec = Infant_cord_data[1:187,]
DCC_60_to_75_sec = Infant_cord_data[187:353,]

p1_hat = sum(DCC_30_to_45_sec$died)/length(DCC_30_to_45_sec$died) 
p1_hat*length(DCC_30_to_45_sec$died)
(1 - p1_hat)*length(DCC_30_to_45_sec$died)

p2_hat = sum(DCC_60_to_75_sec$died)/length(DCC_60_to_75_sec$died)
p2_hat*length(DCC_60_to_75_sec$died)
(1 - p2_hat)*length(DCC_60_to_75_sec$died)

z = (p1_hat-p2_hat)/(sqrt((p1_hat*(1 - p1_hat)/length(DCC_30_to_45_sec)) + (p2_hat*(1 - p2_hat)/length(DCC_60_to_75_sec))))
z   #0.004448026
qnorm(0.05)  #-1.644854
pnorm(z,lower.tail = T) #0.5017745
                  "Because z > crit value, I fail to reject the Ho." 
""

#Hypothesis 2: Infants born above 900 weight have a higher rate of death than those born below 900 weight.
# Ho: P2 - P1 =< 0     Ha: P2 - P1 > 0    alpha = 0.05

bw_ranks = order(Infant_cord_data$bw)
bw_sorted = Infant_cord_data[bw_ranks,]
bw_sorted$bw[67] #900

below = bw_sorted$died[1:67]
above = bw_sorted$died[68:353]


P1_hat = sum(below)/length(below)
P2_hat = sum(above)/length(above)

P1_hat*length(below)
(1 - P1_hat)*length(below)

P2_hat*length(above)
(1 - P2_hat)*length(above)


Z = (P1_hat - P2_hat)/(sqrt((P1_hat*(1 - P1_hat)/length(below)) + (P2_hat*(1 - P2_hat)/length(above))))         
Z   #3.403905
qnorm(0.95)   #1.644854
pnorm(Z,lower.tail = F)   #0.0003321497
                  "Because z > crit value, I reject the Ho." 
                  
                  
                  
""          
#Hypothesis 3: Gestrational age is a statistical predictor for admission temperature.

#Hypothesis 4: The weight of male neonates is greater than that of female neonates.
#Ho: Xf >= Xm   Ha: Xm > Xf   alpha = 0.05

sex_ranks = order(Infant_cord_data$sex)
sex_sorted = Infant_cord_data[sex_ranks,]
female = sex_sorted[1:137,]
male = sex_sorted[138:353,]
var.test(female$bw,male$bw)   #The two samples do not have significantly different variances.
shapiro.test(male$bw)   #Not normally distributed   p-value = 0.02496
shapiro.test(female$bw) #Is normally distributed  p-value = 0.1196

#My data is non-parametric

wilcox.test(female$bw,male$bw,alternative = "less")   #p-value = 0.04029, significant. I reject the null.

                  