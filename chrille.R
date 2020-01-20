setwd('C:/Users/a01686/OneDrive - RSA Group')
library(stats)
library(readxl)
dataset <- read_excel("Data.xlsx")
delar <- split(dataset, dataset$control_group)
grupp1 <- delar[['K1']]
grupp2 <- delar[['K2']]
grupp2 <- grupp2[complete.cases(grupp2$ttc_ped_right),]
grupp3 <- delar[['3']]

# Nedan skapas histogram samt varians och medelvärde för ttc för respektive kontrollgrupp med funktionerna
# hist(), var() respektive mean(). Därefter tas en modell fram för att förklara resultatet i ttc med hjälp
# utav de ingående faktorerna (ålder, synfel osv) med hjälp av funktionen lm(). Vissa av dessa faktorer 
# kommer enbart vara brus och inte ha någon effekt, varför man vill rensa bort dem ur modellen, vilket
# görs med funktionen step(). För att se en summering av modellen används funktionen summary(). 


### pedestrian right

# grupp 1
hist(grupp1$ttc_ped_right)
var(grupp1$ttc_ped_right)
mean(grupp1$ttc_ped_right)
fit1_1 <- lm(ttc_ped_right ~ gender + age + vis_right + vis_left + vis_bi + low_contrast_right + low_contrast_left
           + low_contrast_bi + bino + uvof_1 + uvof_2 + uvof_3 + uvof_risk + passed + coll_vehicle + coll_ped + speeding
           , data=grupp1)
step1_1 <- step(fit1_1, direction="both")
# grupp 2
hist(grupp2$ttc_ped_right)
var(grupp2$ttc_ped_right)
mean(grupp2$ttc_ped_right)
fit1_2 <- lm(ttc_ped_right ~ gender + age + vis_right + vis_left + vis_bi + low_contrast_right + low_contrast_left
             + low_contrast_bi + uvof_2 + uvof_3 + passed + coll_vehicle + coll_ped + speeding
             , data=grupp2)
step1_2 <- step(fit1_2, direction="both")
# grupp 3
hist(grupp3$ttc_ped_right)
var(grupp3$ttc_ped_right)
mean(grupp3$ttc_ped_right)
fit1_3 <- lm(ttc_ped_right ~ gender + age + vis_right + low_contrast_left + low_contrast_bi + coll_ped + speeding, data=grupp3)
step1_3 <- step(fit1_3, direction="both")

### Green car1

#grupp1
hist(grupp1$ttc_green_1)
var(grupp1$ttc_green_1)
mean(grupp1$ttc_green_1)
fit2_1 <- lm(ttc_green_1 ~ gender + age + vis_right + vis_left + vis_bi + low_contrast_right + low_contrast_left
           + low_contrast_bi + bino + uvof_1 + uvof_2 + uvof_3 + uvof_risk + passed + coll_vehicle + coll_ped + speeding
           , data=grupp1)
step2_1 <- step(fit2_1, direction="both")
#grupp2
hist(grupp2$ttc_green_1)
var(grupp2$ttc_green_1)
mean(grupp2$ttc_green_1)
fit2_2 <- lm(ttc_green_1 ~ gender + age + vis_right + vis_left + vis_bi + low_contrast_right + low_contrast_left
             + low_contrast_bi + uvof_2 + uvof_3 + passed + coll_vehicle + coll_ped + speeding
             , data=grupp2)
step2_2 <- step(fit2_2, direction="both")
#grupp 3
hist(grupp3$ttc_green_1)
var(grupp3$ttc_green_1)
mean(grupp3$ttc_green_1)
fit2_3 <- lm(ttc_green_1 ~ gender + age + vis_right + low_contrast_left + low_contrast_bi + coll_ped + speeding, data=grupp3)
step2_3 <- step(fit2_3, direction="both")

### Pedestrian light

# grupp1
hist(grupp1$ttc_ped_light)
var(grupp1$ttc_ped_light)
mean(grupp1$ttc_ped_light)
fit3_1 <- lm(as.numeric(ttc_ped_light) ~ gender + age + vis_right + vis_left + vis_bi + low_contrast_right + low_contrast_left
           + low_contrast_bi + bino + uvof_1 + uvof_2 + uvof_3 + uvof_risk + passed + coll_vehicle + coll_ped + speeding
           , data=grupp1)
step3_1 <- step(fit3_1, direction="both")
# grupp2
hist(grupp2$ttc_ped_light)
var(grupp2$ttc_ped_light)
mean(grupp2$ttc_ped_light)
fit3_2 <- lm(as.numeric(ttc_ped_light) ~ gender + age + vis_right + vis_left + vis_bi + low_contrast_right + low_contrast_left
             + low_contrast_bi + uvof_2 + uvof_3 + passed + coll_vehicle + coll_ped + speeding
             , data=grupp2)
step3_2 <- step(fit3_2, direction="both")
# grupp 3
hist(grupp3$ttc_ped_light)
var(grupp3$ttc_ped_light)
mean(grupp3$ttc_ped_light)
fit3_3 <- lm(as.numeric(ttc_ped_light) ~ gender + age + vis_right + low_contrast_left + low_contrast_bi + coll_ped + speeding, data=grupp3)
step3_3 <- step(fit3_3, direction="both")


### Pedestrian at crossing from right 2

# Grupp 1
hist(grupp1$ttc_ped_cross_right2)
var(grupp1$ttc_ped_cross_right2)
mean(grupp1$ttc_ped_cross_right2)
fit4_1 <- lm(ttc_ped_cross_right2 ~ gender + age + vis_right + vis_left + vis_bi + low_contrast_right + low_contrast_left
           + low_contrast_bi + bino + uvof_1 + uvof_2 + uvof_3 + uvof_risk + passed + coll_vehicle + coll_ped + speeding
           , data=grupp1)
step4_1 <- step(fit4_1, direction="both")
# Grupp 2
hist(grupp2$ttc_ped_cross_right2)
var(grupp2$ttc_ped_cross_right2)
mean(grupp2$ttc_ped_cross_right2)
fit4_2 <- lm(ttc_ped_cross_right2 ~ gender + age + vis_right + vis_left + vis_bi + low_contrast_right + low_contrast_left
             + low_contrast_bi + uvof_2 + uvof_3 + passed + coll_vehicle + coll_ped + speeding
             , data=grupp2)
step4_2 <- step(fit4_2, direction="both")
# Grupp 3
hist(grupp3$ttc_ped_cross_right2)
var(grupp3$ttc_ped_cross_right2)
mean(grupp3$ttc_ped_cross_right2)
fit4_3 <- lm(ttc_ped_cross_right2 ~ gender + age + vis_right + low_contrast_left + low_contrast_bi + coll_ped + speeding, data=grupp3)
step4_3 <- step(fit4_3, direction="both")

### bus

# Grupp 1
hist(grupp1$ttc_buss)
var(grupp1$ttc_buss)
mean(grupp1$ttc_buss)
fit5_1<- lm(as.numeric(ttc_buss) ~ gender + age + vis_right + vis_left + vis_bi + low_contrast_right + low_contrast_left
           + low_contrast_bi + bino + uvof_1 + uvof_2 + uvof_3 + uvof_risk + passed + coll_vehicle + coll_ped + speeding
           , data=grupp1)
step5_1 <- step(fit5_1, direction="both")
# Grupp 2
hist(grupp2$ttc_buss)
var(grupp2$ttc_buss)
mean(grupp2$ttc_buss)
fit5_2 <- lm(as.numeric(ttc_buss) ~ gender + age + vis_right + vis_left + vis_bi + low_contrast_right + low_contrast_left
             + low_contrast_bi + uvof_2 + uvof_3 + passed + coll_vehicle + coll_ped + speeding
             , data=grupp2)
step5_2 <- step(fit5_2, direction="both")
# Grupp 3
hist(grupp3$ttc_buss)
var(grupp3$ttc_buss)
mean(grupp3$ttc_buss)
fit5_3 <- lm(as.numeric(ttc_buss) ~ gender + age + vis_right + low_contrast_left + low_contrast_bi + coll_ped + speeding, data=grupp3)
step5_3 <- step(fit5_3, direction="both")

### Green car 2

# Grupp 1
hist(grupp1$ttc_green_2)
var(grupp1$ttc_green_2)
mean(grupp1$ttc_green_2)
fit6_1 <- lm(as.numeric(ttc_green_2) ~ gender + age + vis_right + vis_left + vis_bi + low_contrast_right + low_contrast_left
           + low_contrast_bi + bino + uvof_1 + uvof_2 + uvof_3 + uvof_risk + passed + coll_vehicle + coll_ped + speeding
           , data=grupp1)
step6_1 <- step(fit6_1, direction="both")
# Grupp 2
hist(grupp2$ttc_green_2)
var(grupp2$ttc_green_2)
mean(grupp2$ttc_green_2)
fit6_2 <- lm(as.numeric(ttc_green_2) ~ gender + age + vis_right + vis_left + vis_bi + low_contrast_right + low_contrast_left
             + low_contrast_bi + uvof_2 + uvof_3 + passed + coll_vehicle + coll_ped + speeding
             , data=grupp2)
step6_2 <- step(fit6_2, direction="both")
# Grupp 3
hist(grupp3$ttc_green_2)
var(grupp3$ttc_green_2)
mean(grupp3$ttc_green_2)
fit6_3 <- lm(as.numeric(ttc_green_2) ~ gender + age + vis_right + low_contrast_left + low_contrast_bi + coll_ped + speeding, data=grupp3)
step6_3 <- step(fit6_3, direction="both")

### Pedestrians left and right

# Grupp 1
hist(grupp1$ttc_ped_lr)
var(grupp1$ttc_ped_lr)
mean(grupp1$ttc_ped_lr)
fit7_1 <- lm(as.numeric(ttc_ped_lr) ~ gender + age + vis_right + vis_left + vis_bi + low_contrast_right + low_contrast_left
           + low_contrast_bi + bino + uvof_1 + uvof_2 + uvof_3 + uvof_risk + passed + coll_vehicle + coll_ped + speeding
           , data=grupp1)
step7_1 <- step(fit7_1, direction="both")
# Grupp 2
hist(grupp2$ttc_ped_lr)
var(grupp2$ttc_ped_lr)
mean(grupp2$ttc_ped_lr)
fit7_2 <- lm(as.numeric(ttc_ped_lr) ~ gender + age + vis_right + vis_left + vis_bi + low_contrast_right + low_contrast_left
             + low_contrast_bi + uvof_2 + uvof_3 + passed + coll_vehicle + coll_ped + speeding
             , data=grupp2)
step7_2 <- step(fit7_2, direction="both")
# Grupp 3
hist(grupp3$ttc_ped_lr)
var(grupp3$ttc_ped_lr)
mean(grupp3$ttc_ped_lr)
fit7_3 <- lm(as.numeric(ttc_ped_lr) ~ gender + age + vis_right + low_contrast_left + low_contrast_bi + coll_ped + speeding, data=grupp3)
step7_3 <- step(fit7_3, direction="both")

### Pedestrians crossing right 3

# Grupp 1
hist(grupp1$ttc_ped_cross_right3)
var(grupp1$ttc_ped_cross_right3)
mean(grupp1$ttc_ped_cross_right3)
fit8_1 <- lm(as.numeric(ttc_ped_cross_right3) ~ gender + age + vis_right + vis_left + vis_bi + low_contrast_right + low_contrast_left
           + low_contrast_bi + bino + uvof_1 + uvof_2 + uvof_3 + uvof_risk + passed + coll_vehicle + coll_ped + speeding
           , data=grupp1)
# Stepwise Regression
step8_1 <- step(fit8_1, direction="both")
# Grupp 2
hist(grupp2$ttc_ped_cross_right3)
var(grupp2$ttc_ped_cross_right3)
mean(grupp2$ttc_ped_cross_right3)
fit8_2 <- lm(as.numeric(ttc_ped_cross_right3) ~ gender + age + vis_right + vis_left + vis_bi + low_contrast_right + low_contrast_left
             + low_contrast_bi + uvof_2 + uvof_3 + passed + coll_vehicle + coll_ped + speeding
             , data=grupp2)
step8_2 <- step(fit8_2, direction="both")
# Grupp 3
hist(grupp3$ttc_ped_cross_right3)
var(grupp3$ttc_ped_cross_right3)
mean(grupp3$ttc_ped_cross_right3)
fit8_3 <- lm(as.numeric(ttc_ped_cross_right3) ~ gender + age + vis_right + low_contrast_left + low_contrast_bi + coll_ped + speeding, data=grupp3)
step8_3 <- step(fit8_3, direction="both")


### Girl bus

# Grupp 1
hist(grupp1$ttc_girl_bus)
var(grupp1$ttc_girl_bus)
mean(grupp1$ttc_girl_bus)
fit9_1 <- lm(as.numeric(ttc_girl_bus) ~ gender + age + vis_right + vis_left + vis_bi + low_contrast_right + low_contrast_left
           + low_contrast_bi + bino + uvof_1 + uvof_2 + uvof_3 + uvof_risk + passed + coll_vehicle + coll_ped + speeding
           , data=grupp1)
step9_1 <- step(fit9_1, direction="both")
# Grupp 2
hist(grupp2$ttc_girl_bus)
var(grupp2$ttc_girl_bus)
mean(grupp2$ttc_girl_bus)
fit9_2 <- lm(as.numeric(ttc_girl_bus) ~ gender + age + vis_right + vis_left + vis_bi + low_contrast_right + low_contrast_left
             + low_contrast_bi + uvof_2 + uvof_3 + passed + coll_vehicle + coll_ped + speeding
             , data=grupp2)
step9_2 <- step(fit9_2, direction="both")
# GRupp 3
hist(grupp3$ttc_girl_bus)
var(grupp3$ttc_girl_bus)
mean(grupp3$ttc_girl_bus)
fit9_3 <- lm(as.numeric(ttc_girl_bus) ~ gender + age + vis_right + low_contrast_left + low_contrast_bi + coll_ped + speeding, data=grupp3)
step9_3 <- step(fit9_3, direction="both")

### Reversing car

# Grupp 1
hist(grupp1$ttc_rev_car)
var(grupp1$ttc_rev_car)
mean(grupp1$ttc_rev_car)
fit10_1 <- lm(as.numeric(ttc_rev_car) ~ gender + age + vis_right + vis_left + vis_bi + low_contrast_right + low_contrast_left
            + low_contrast_bi + bino + uvof_1 + uvof_2 + uvof_3 + uvof_risk + passed + coll_vehicle + coll_ped + speeding
            , data=grupp1)
step10_1 <- step(fit10_1, direction="both")
# Grupp 2
hist(grupp2$ttc_rev_car)
var(grupp2$ttc_rev_car)
mean(grupp2$ttc_rev_car)
fit10_2 <- lm(as.numeric(ttc_rev_car) ~ gender + age + vis_right + vis_left + vis_bi + low_contrast_right + low_contrast_left
              + low_contrast_bi + uvof_2 + uvof_3 + passed + coll_vehicle + coll_ped + speeding
              , data=grupp2)
step10_2 <- step(fit10_2, direction="both")
# Grupp 3
hist(grupp3$ttc_rev_car)
var(grupp3$ttc_rev_car)
mean(grupp3$ttc_rev_car)
fit10_3 <- lm(as.numeric(ttc_rev_car) ~ gender + age + vis_right + low_contrast_left + low_contrast_bi + coll_ped + speeding, data=grupp3)
step10_3 <- step(fit10_3, direction="both")

### Crossing from right villa

# Grupp 1
hist(grupp1$ttc_cross_right_villa)
var(grupp1$ttc_cross_right_villa)
mean(grupp1$ttc_cross_right_villa)
fit11_1 <- lm(as.numeric(ttc_cross_right_villa) ~ gender + age + vis_right + vis_left + vis_bi + low_contrast_right + low_contrast_left
            + low_contrast_bi + bino + uvof_1 + uvof_2 + uvof_3 + uvof_risk + passed + coll_vehicle + coll_ped + speeding
            , data=grupp1)
step11_1 <- step(fit11, direction="both")
# Grupp 2
hist(grupp2$ttc_cross_right_villa)
var(grupp2$ttc_cross_right_villa)
mean(grupp2$ttc_cross_right_villa)
fit11_2 <- lm(as.numeric(ttc_cross_right_villa) ~ gender + age + vis_right + vis_left + vis_bi + low_contrast_right + low_contrast_left
              + low_contrast_bi + uvof_2 + uvof_3 + passed + coll_vehicle + coll_ped + speeding
              , data=grupp2)
step11_2 <- step(fit11_2, direction="both")
# Grupp 3
hist(grupp3$ttc_cross_right_villa)
var(grupp3$ttc_cross_right_villa)
mean(grupp3$ttc_cross_right_villa)
fit11_3 <- lm(as.numeric(ttc_cross_right_villa) ~ gender + age + vis_right + low_contrast_left + low_contrast_bi + coll_ped + speeding, data=grupp3)
step11_3 <- step(fit11_3, direction="both")

