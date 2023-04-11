#Baca data yang sudah diformat ke txt
hpv <- read.table(file="HPV.txt", header = TRUE)
str(hpv)
cols <- c("L6", "L11", "H16", "H18", "H31", "H33", "H35", "H39", "L40.61", 
          "L42", "L43.44", "H45", "H51", "H52", "H53", "L54.55", "H56", 
          "L57.71", "H58", "H59", "H66.68", "L70", "L72", "L73", "L81", 
          "L82", "M84.26","Prov", "Stiker_Kes")
hpv[cols] <- lapply(hpv[cols], factor)
sapply(hpv, class)
hpv$Umur <- as.numeric(hpv$Umur)
sapply(hpv, class)


#Uji Regresi Logistik dgn Var Dependen L6
library(brglm2)

lr.L6 <- glm(L6 ~ L11 + H16 + H18 + H31 + H33 + H35 + H39 + 
                L40.61 + L42 + L43.44 + H45 + H51 + H52 + H53 + L54.55 + H56 + 
                L57.71 + H58 + H59 + H66.68 + L70 + L72 + L73 + L81 + L82 + M84.26 +
                Umur + Prov, family = binomial(logit), data = hpv, method = "brglmFit", type = "AS_median")
summary(lr.L6)

#Ekstrak nilai p value-nya saja utk var dep L6
pvL6 <- coef(summary(lr.L6))[,'Pr(>|z|)']

#Hitung Odds Ratio atau Exp(B) & 95% CI untuk OR var dep L6
OR.L6 <- exp(lr.L6$coefficients)
beta.L6 <- coef(lr.L6)
se.L6 <- coef(summary(lr.L6))[,2]
CIOR.L6 <- cbind(exp(beta.L6 + (1.96 * se.L6 * -1)), exp(beta.L6 + (1.96 * se.L6 * 1)))
colnames(CIOR.L6) <- c("lower 95%CI","upper 95%CI")

#Buat Tabel Hasil OR, 95%CI untuk OR dan p-value var dep L6
L6CIOR <- cbind(OR.L6,CIOR.L6,pvL6)
L6CIOR

#Uji Regresi Logistik dgn Var Dependen L11

lr.L11 <- glm(L11 ~ L6 + H16 + H18 + H31 + H33 + H35 + H39 + 
            L40.61 + L42 + L43.44 + H45 + H51 + H52 + H53 + L54.55 + H56 + 
              L57.71 + H58 + H59 + H66.68 + L70 + L72 + L73 + L81 + L82 +
            M84.26 + Umur + Prov, family = binomial(logit), data = hpv, method = "brglmFit", type = "AS_median")
summary(lr.L11)

#Ekstrak nilai p value-nya saja utk var dep L11
pvL11 <- coef(summary(lr.L11))[,'Pr(>|z|)']

#Hitung Odds Ratio atau Exp(B) & 95% CI untuk OR var dep L11
OR.L11 <- exp(lr.L11$coefficients)
beta.L11 <- coef(lr.L11)
se.L11 <- coef(summary(lr.L11))[,2]
CIOR.L11 <- cbind(exp(beta.L11 + 1.96 * se.L11 * -1), exp(beta.L11 + 1.96 * se.L11 * 1))
colnames(CIOR.L11) <- c("lower 95%CI","upper 95%CI")

#Buat Tabel Hasil OR, 95%CI untuk OR dan p-value var dep L11
L11CIOR <- cbind(OR.L11,CIOR.L11,pvL11)
L11CIOR

#Uji Regresi Logistik dgn Var Dependen H16

lr.H16 <- glm(H16 ~ L6 + L11 + H18 + H31 + H33 + H35 + H39 + 
            L40.61 + L42 + L43.44 + H45 + H51 + H52 + H53 + L54.55 + H56 + 
            L57.71 + H58 + H59 + H66.68 + L70 + L72 + L73 + L81 + L82 + 
            M84.26 + Umur + Prov, family = binomial(logit), data = hpv, method = "brglmFit", type = "AS_median")
summary(lr.H16)

#Ekstrak nilai p value-nya saja utk var dep H16
pvH16 <- coef(summary(lr.H16))[,'Pr(>|z|)']

#Hitung Odds Ratio atau Exp(B) & 95% CI untuk OR var dep H16
OR.H16 <- exp(lr.H16$coefficients)
beta.H16 <- coef(lr.H16)
se.H16 <- coef(summary(lr.H16))[,2]
CIOR.H16 <- cbind(exp(beta.H16 + (1.96 * se.H16 * -1)), exp(beta.H16 + (1.96 * se.H16 * 1)))
colnames(CIOR.H16) <- c("lower 95%CI","upper 95%CI")

#Buat Tabel Hasil OR, 95%CI untuk OR dan p-value var dep H16
H16CIOR <- cbind(OR.H16,CIOR.H16,pvH16)
H16CIOR

#Uji Regresi Logistik dgn Var Dependen H18

lr.H18 <- glm(H18 ~ L6 + L11 + H16 + H31 + H33 + H35 + H39 + 
            L40.61 + L42 + L43.44 + H45 + H51 + H52 + H53 + L54.55 + H56 + 
            L57.71 + H58 + H59 + H66.68 + L70 + L72 + L73 + L81 + L82 + 
            M84.26 + Umur + Prov, family = binomial(logit), data = hpv, method = "brglmFit", type = "AS_median")
summary(lr.H18)

#Ekstrak nilai p value-nya saja utk var dep H18
pvH18 <- coef(summary(lr.H18))[,'Pr(>|z|)']

#Hitung Odds Ratio atau Exp(B) & 95% CI untuk OR var dep H18
OR.H18 <- exp(lr.H18$coefficients)
beta.H18 <- coef(lr.H18)
se.H18 <- coef(summary(lr.H18))[,2]
CIOR.H18 <- cbind(exp(beta.H18 + 1.96 * se.H18 * -1), exp(beta.H18 + 1.96 * se.H18 * 1))
colnames(CIOR.H18) <- c("lower 95%CI","upper 95%CI")

#Buat Tabel Hasil OR, 95%CI untuk OR dan p-value var dep H18
H18CIOR <- cbind(OR.H18,CIOR.H18,pvH18)
H18CIOR

#Uji Regresi Logistik dgn Var Dependen H31

lr.H31 <- glm(H31 ~ L6 + L11 + H16 + H18 + H33 + H35 + H39 + 
            L40.61 + L42 + L43.44 + H45 + H51 + H52 + H53 + L54.55 + H56 + 
            L57.71 + H58 + H59 + H66.68 + L70 + L72 + L73 + L81 + L82 + 
            M84.26 + Umur + Prov, family = binomial(logit), data = hpv, method = "brglmFit", type = "AS_median")
summary(lr.H31)

#Ekstrak nilai p value-nya saja utk var dep H31
pvH31 <- coef(summary(lr.H31))[,'Pr(>|z|)']

#Hitung Odds Ratio atau Exp(B) & 95% CI untuk OR var dep H31
OR.H31 <- exp(lr.H31$coefficients)
beta.H31 <- coef(lr.H31)
se.H31 <- coef(summary(lr.H31))[,2]
CIOR.H31 <- cbind(exp(beta.H31 + 1.96 * se.H31 * -1), exp(beta.H31 + 1.96 * se.H31 * 1))
colnames(CIOR.H31) <- c("lower 95%CI","upper 95%CI")

#Buat Tabel Hasil OR, 95%CI untuk OR dan p-value var dep H31
H31CIOR <- cbind(OR.H31,CIOR.H31,pvH31)
H31CIOR

#Uji Regresi Logistik dgn Var Dependen H33

lr.H33 <- glm(H33 ~ L6 + L11 + H16 + H18 + H31 + H35 + H39 + 
            L40.61 + L42 + L43.44 + H45 + H51 + H52 + H53 + L54.55 + H56 + 
            L57.71 + H58 + H59 + H66.68 + L70 + L72 + L73 + L81 + L82 + 
            M84.26 + Umur + Prov, family = binomial(logit), data = hpv, method = "brglmFit", type = "AS_median")
summary(lr.H33)

#Ekstrak nilai p value-nya saja utk var dep H33
pvH33 <- coef(summary(lr.H33))[,'Pr(>|z|)']

#Hitung Odds Ratio atau Exp(B) & 95% CI untuk OR var dep H33
OR.H33 <- exp(lr.H33$coefficients)
beta.H33 <- coef(lr.H33)
se.H33 <- coef(summary(lr.H33))[,2]
CIOR.H33 <- cbind(exp(beta.H33 + 1.96 * se.H33 * -1), exp(beta.H33 + 1.96 * se.H33 * 1))
colnames(CIOR.H33) <- c("lower 95%CI","upper 95%CI")

#Buat Tabel Hasil OR, 95%CI untuk OR dan p-value var dep H33
H33CIOR <- cbind(OR.H33,CIOR.H33,pvH33)
H33CIOR

#Uji Regresi Logistik dgn Var Dependen H35

lr.H35 <- glm(H35 ~ L6 + L11 + H16 + H18 + H31 + H33 + H39 + 
            L40.61 + L42 + L43.44 + H45 + H51 + H52 + H53 + L54.55 + H56 + 
            L57.71 + H58 + H59 + H66.68 + L70 + L72 + L73 + L81 + L82 + 
            M84.26 + Umur + Prov, family = binomial(logit), data = hpv, method = "brglmFit", type = "AS_median")
summary(lr.H35)

#Ekstrak nilai p value-nya saja utk var dep H35
pvH35 <- coef(summary(lr.H35))[,'Pr(>|z|)']

#Hitung Odds Ratio atau Exp(B) & 95% CI untuk OR var dep H35
OR.H35 <- exp(lr.H35$coefficients)
beta.H35 <- coef(lr.H35)
se.H35 <- coef(summary(lr.H35))[,2]
CIOR.H35 <- cbind(exp(beta.H35 + 1.96 * se.H35 * -1), exp(beta.H35 + 1.96 * se.H35 * 1))
colnames(CIOR.H35) <- c("lower 95%CI","upper 95%CI")

#Buat Tabel Hasil OR, 95%CI untuk OR dan p-value var dep H35
H35CIOR <- cbind(OR.H35,CIOR.H35,pvH35)
H35CIOR

#Uji Regresi Logistik dgn Var Dependen H39

lr.H39 <- glm(H39 ~ L6 + L11 + H16 + H18 + H31 + H33 + H35 + 
            L40.61 + L42 + L43.44 + H45 + H51 + H52 + H53 + L54.55 + H56 + 
            L57.71 + H58 + H59 + H66.68 + L70 + L72 + L73 + L81 + L82 + 
            M84.26 + Umur + Prov, family = binomial(logit), data = hpv, method = "brglmFit", type = "AS_median")
summary(lr.H39)

#Ekstrak nilai p value-nya saja utk var dep H39
pvH39 <- coef(summary(lr.H39))[,'Pr(>|z|)']

#Hitung Odds Ratio atau Exp(B) & 95% CI untuk OR var dep H39
OR.H39 <- exp(lr.H39$coefficients)
beta.H39 <- coef(lr.H39)
se.H39 <- coef(summary(lr.H39))[,2]
CIOR.H39 <- cbind(exp(beta.H39 + 1.96 * se.H39 * -1), exp(beta.H39 + 1.96 * se.H39 * 1))
colnames(CIOR.H39) <- c("lower 95%CI","upper 95%CI")


#Buat Tabel Hasil OR, 95%CI untuk OR dan p-value var dep H39
H39CIOR <- cbind(OR.H39,CIOR.H39,pvH39)
H39CIOR


#Uji Regresi Logistik dgn Var Dependen L40.61

lr.L40.61 <- glm(L40.61 ~ L6 + L11 + H16 + H18 + H31 + H33 + 
              H35 + H39 + L42 + L43.44 + H45 + H51 + H52 + H53 + L54.55 + 
              H56 + L57.71 + H58 + H59 + H66.68 + L70 + L72 + L73 + L81 + 
              L82 + M84.26 + Umur + Prov, family = binomial(logit), data = hpv, method = "brglmFit", type = "AS_median")
summary(lr.L40.61)

#Ekstrak nilai p value-nya saja utk var dep L40_61
pvL40.61 <- coef(summary(lr.L40.61))[,'Pr(>|z|)']

#Hitung Odds Ratio atau Exp(B) & 95% CI untuk OR var dep L40_61
OR.L40.61 <- exp(lr.L40.61$coefficients)
beta.L40.61 <- coef(lr.L40.61)
se.L40.61 <- coef(summary(lr.L40.61))[,2]
CIOR.L40.61 <- cbind(exp(beta.L40.61 + 1.96 * se.L40.61 * -1), exp(beta.L40.61 + 1.96 * se.L40.61 * 1))
colnames(CIOR.L40.61) <- c("lower 95%CI","upper 95%CI")


#Buat Tabel Hasil OR, 95%CI untuk OR dan p-value var dep L40_61
L40.61CIOR <- cbind(OR.L40.61,CIOR.L40.61,pvL40.61)
L40.61CIOR

#Uji Regresi Logistik dgn Var Dependen L42

lr.L42 <- glm(L42 ~ L6 + L11 + H16 + H18 + H31 + H33 + H35 + H39 
            + L40.61 + L43.44 + H45 + H51 + H52 + H53 + L54.55 + H56 + 
            L57.71 + H58 + H59 + H66.68 + L70 + L72 + L73 + L81 + L82 + 
            M84.26 + Umur + Prov, family = binomial(logit), data = hpv, method = "brglmFit", type = "AS_median")
summary(lr.L42)

#Ekstrak nilai p value-nya saja utk var dep L42
pvL42 <- coef(summary(lr.L42))[,'Pr(>|z|)']

#Hitung Odds Ratio atau Exp(B) & 95% CI untuk OR var dep L42
OR.L42 <- exp(lr.L42$coefficients)
beta.L42 <- coef(lr.L42)
se.L42 <- coef(summary(lr.L42))[,2]
CIOR.L42 <- cbind(exp(beta.L42 + 1.96 * se.L42 * -1), exp(beta.L42 + 1.96 * se.L42 * 1))
colnames(CIOR.L42) <- c("lower 95%CI","upper 95%CI")


#Buat Tabel Hasil OR, 95%CI untuk OR dan p-value var dep L42
L42CIOR <- cbind(OR.L42,CIOR.L42,pvL42)
L42CIOR

#Uji Regresi Logistik dgn Var Dependen L43.44

lr.L43.44 <- glm(L43.44 ~ L6 + L11 + H16 + H18 + H31 + H33 + H35 
              + H39 + L40.61 + L42 + H45 + H51 + H52 + H53 + L54.55 + H56 + 
              L57.71 + H58 + H59 + H66.68 + L70 + L72 + L73 + L81 + L82 + 
              M84.26 + Umur + Prov, family = binomial(logit), data = hpv, method = "brglmFit", type = "AS_median")
summary(lr.L43.44)

#Ekstrak nilai p value-nya saja utk var dep L43.44
pvL43.44 <- coef(summary(lr.L43.44))[,'Pr(>|z|)']

#Hitung Odds Ratio atau Exp(B) & 95% CI untuk OR var dep L43.44
OR.L43.44 <- exp(lr.L43.44$coefficients)
beta.L43.44 <- coef(lr.L43.44)
se.L43.44 <- coef(summary(lr.L43.44))[,2]
CIOR.L43.44 <- cbind(exp(beta.L43.44 + 1.96 * se.L43.44 * -1), exp(beta.L43.44 + 1.96 * se.L43.44 * 1))
colnames(CIOR.L43.44) <- c("lower 95%CI","upper 95%CI")

#Buat Tabel Hasil OR, 95%CI untuk OR dan p-value var dep L43.44
L43.44CIOR <- cbind(OR.L43.44,CIOR.L43.44,pvL43.44)
L43.44CIOR

#Uji Regresi Logistik dgn Var Dependen H45

lr.H45 <- glm(H45 ~ L6 + L11 + H16 + H18 + H31 + H33 + H35 + 
            H39 + L40.61 + L42 + L43.44 + H51 + H52 + H53 + L54.55 + H56 
            + L57.71 + H58 + H59 + H66.68 + L70 + L72 + L73 + L81 + L82 + 
            M84.26 + Umur + Prov, family = binomial(logit), data = hpv, method = "brglmFit", type = "AS_median")
summary(lr.H45)

#Ekstrak nilai p value-nya saja utk var dep H45
pvH45 <- coef(summary(lr.H45))[,'Pr(>|z|)']

#Hitung Odds Ratio atau Exp(B) & 95% CI untuk OR var dep H45
OR.H45 <- exp(lr.H45$coefficients)
beta.H45 <- coef(lr.H45)
se.H45 <- coef(summary(lr.H45))[,2]
CIOR.H45 <- cbind(exp(beta.H45 + 1.96 * se.H45 * -1), exp(beta.H45 + 1.96 * se.H45 * 1))
colnames(CIOR.H45) <- c("lower 95%CI","upper 95%CI")

#Buat Tabel Hasil OR, 95%CI untuk OR dan p-value var dep H45
H45CIOR <- cbind(OR.H45,CIOR.H45,pvH45)
H45CIOR

#Uji Regresi Logistik dgn Var Dependen H51

lr.H51 <- glm(H51 ~ L6 + L11 + H16 + H18 + H31 + H33 + H35 + 
                H39 + L40.61 + L42 + L43.44 + H45 + H52 + H53 + L54.55 + H56 
                + L57.71 + H58 + H59 + H66.68 + L70 + L72 + L73 + L81 + L82 
                + M84.26 + Umur + Prov, family = binomial(logit), data = hpv, method = "brglmFit", type = "AS_median")
summary(lr.H51)

#Ekstrak nilai p value-nya saja utk var dep H51
pvH51 <- coef(summary(lr.H51))[,'Pr(>|z|)']

#Hitung Odds Ratio atau Exp(B) & 95% CI untuk OR var dep H51
OR.H51 <- exp(lr.H51$coefficients)
beta.H51 <- coef(lr.H51)
se.H51 <- coef(summary(lr.H51))[,2]
CIOR.H51 <- cbind(exp(beta.H51 + 1.96 * se.H51 * -1), exp(beta.H51 + 1.96 * se.H51 * 1))
colnames(CIOR.H51) <- c("lower 95%CI","upper 95%CI")

#Buat Tabel Hasil OR, 95%CI untuk OR dan p-value var dep H51
H51CIOR <- cbind(OR.H51,CIOR.H51,pvH51)
H51CIOR

#Uji Regresi Logistik dgn Var Dependen H52

lr.H52 <- glm(H52 ~ L6 + L11 + H16 + H18 + H31 + H33 + H35 + 
              H39 + L40.61 + L42 + L43.44 + H45 + H51 + H53 + L54.55 + H56 
              + L57.71 + H58 + H59 + H66.68 + L70 + L72 + L73 + L81 + L82 
              + M84.26 + Umur + Prov, family = binomial(logit), data = hpv, method = "brglmFit", type = "AS_median")
summary(lr.H52)

#Ekstrak nilai p value-nya saja utk var dep H52
pvH52 <- coef(summary(lr.H52))[,'Pr(>|z|)']

#Hitung Odds Ratio atau Exp(B) & 95% CI untuk OR var dep H52
OR.H52 <- exp(lr.H52$coefficients)
beta.H52 <- coef(lr.H52)
se.H52 <- coef(summary(lr.H52))[,2]
CIOR.H52 <- cbind(exp(beta.H52 + 1.96 * se.H52 * -1), exp(beta.H52 + 1.96 * se.H52 * 1))
colnames(CIOR.H52) <- c("lower 95%CI","upper 95%CI")

#Buat Tabel Hasil OR, 95%CI untuk OR dan p-value var dep H52
H52CIOR <- cbind(OR.H52,CIOR.H52,pvH52)
H52CIOR

#Uji Regresi Logistik dgn Var Dependen H53

lr.H53 <- glm(H53 ~ L6 + L11 + H16 + H18 + H31 + H33 + H35 + H39
          + L40.61 + L42 + L43.44 + H45 + H51 + H52 + L54.55 + H56 + L57.71 
          + H58 + H59 + H66.68 + L70 + L72 + L73 + L81 + L82 + M84.26 + 
          Umur + Prov, family = binomial(logit), data = hpv, method = "brglmFit", type = "AS_median")
summary(lr.H53)

#Ekstrak nilai p value-nya saja utk var dep H53
pvH53 <- coef(summary(lr.H53))[,'Pr(>|z|)']

#Hitung Odds Ratio atau Exp(B) & 95% CI untuk OR var dep H53
OR.H53 <- exp(lr.H53$coefficients)
beta.H53 <- coef(lr.H53)
se.H53 <- coef(summary(lr.H53))[,2]
CIOR.H53 <- cbind(exp(beta.H53 + 1.96 * se.H53 * -1), exp(beta.H53 + 1.96 * se.H53 * 1))
colnames(CIOR.H53) <- c("lower 95%CI","upper 95%CI")

#Buat Tabel Hasil OR, 95%CI untuk OR dan p-value var dep H53
H53CIOR <- cbind(OR.H53,CIOR.H53,pvH53)
H53CIOR

#Uji Regresi Logistik dgn Var Dependen L54.55

lr.L54.55 <- glm(L54.55 ~ L6 + L11 + H16 + H18 + H31 + H33 + H35
              + H39 + L42 + L43.44 + H45 + H51 + H52 + H53 + H56 + 
              L57.71 + H58 + H59 + H66.68 + L70 + L73 + L81 + 
              M84.26 + Umur + Prov, family = binomial(logit), data = hpv, method = "brglmFit", type = "AS_median")
summary(lr.L54.55)

#Ekstrak nilai p value-nya saja utk var dep L54.55
pvL54.55 <- coef(summary(lr.L54.55))[,'Pr(>|z|)']

#Hitung Odds Ratio atau Exp(B) & 95% CI untuk OR var dep L54.55
OR.L54.55 <- exp(lr.L54.55$coefficients)
beta.L54.55 <- coef(lr.L54.55)
se.L54.55 <- coef(summary(lr.L54.55))[,2]
CIOR.L54.55 <- cbind(exp(beta.L54.55 + 1.96 * se.L54.55 * -1), exp(beta.L54.55 + 1.96 * se.L54.55 * 1))
colnames(CIOR.L54.55) <- c("lower 95%CI","upper 95%CI")

#Buat Tabel Hasil OR, 95%CI untuk OR dan p-value var dep L54.55
L54.55CIOR <- cbind(OR.L54.55,CIOR.L54.55,pvL54.55)
L54.55CIOR

#Uji Regresi Logistik dgn Var Dependen H56

lr.H56 <- glm(H56 ~ L6 + L11 + H16 + H18 + H31 + H33 + H35 + H39
          + L40.61 + L42 + L43.44 + H45 + H51 + H52 + H53 + L54.55 + L57.71 
          + H58 + H59 + H66.68 + L70 + L72 + L73 + L81 + L82 + M84.26 + 
          Umur + Prov, family = binomial(logit), data = hpv, method = "brglmFit", type = "AS_median")
summary(lr.H56)

#Ekstrak nilai p value-nya saja utk var dep H56
pvH56 <- coef(summary(lr.H56))[,'Pr(>|z|)']

#Hitung Odds Ratio atau Exp(B) & 95% CI untuk OR var dep H56
OR.H56 <- exp(lr.H56$coefficients)
beta.H56 <- coef(lr.H56)
se.H56 <- coef(summary(lr.H56))[,2]
CIOR.H56 <- cbind(exp(beta.H56 + 1.96 * se.H56 * -1), exp(beta.H56 + 1.96 * se.H56 * 1))
colnames(CIOR.H56) <- c("lower 95%CI","upper 95%CI")

#Buat Tabel Hasil OR, 95%CI untuk OR dan p-value var dep H56
H56CIOR <- cbind(OR.H56,CIOR.H56,pvH56)
H56CIOR

#Uji Regresi Logistik dgn Var Dependen L57.71

lr.L57.71 <- glm(L57.71 ~ L6 + L11 + H16 + H18 + H31 + H33 + H35
              + H39 + L40.61 + L42 + L43.44 + H45 + H51 + H52 + H53 + L54.55 + 
              H56 + H58 + H59 + H66.68 + L70 + L72 + L73 + L81 + L82 + 
              M84.26 + Umur + Prov, family = binomial(logit), data = hpv, method = "brglmFit", type = "AS_median")
summary(lr.L57.71)

#Ekstrak nilai p value-nya saja utk var dep L57.71
pvL57.71 <- coef(summary(lr.L57.71))[,'Pr(>|z|)']

#Hitung Odds Ratio atau Exp(B) & 95% CI untuk OR var dep L57.71
OR.L57.71 <- exp(lr.L57.71$coefficients)
beta.L57.71 <- coef(lr.L57.71)
se.L57.71 <- coef(summary(lr.L57.71))[,2]
CIOR.L57.71 <- cbind(exp(beta.L57.71 + 1.96 * se.L57.71 * -1), exp(beta.L57.71 + 1.96 * se.L57.71 * 1))
colnames(CIOR.L57.71) <- c("lower 95%CI","upper 95%CI")

#Buat Tabel Hasil OR, 95%CI untuk OR dan p-value var dep L57.71
L57.71CIOR <- cbind(OR.L57.71,CIOR.L57.71,pvL57.71)
L57.71CIOR

#Uji Regresi Logistik dgn Var Dependen H58

lr.H58 <- glm(H58 ~ L6 + L11 + H16 + H18 + H31 + H33 + H35 + H39
          + L40.61 + L42 + L43.44 + H45 + H51 + H52 + H53 + L54.55 + H56 + 
          L57.71 + H59 + H66.68 + L70 + L72 + L73 + L81 + L82 + M84.26 + 
          Umur + Prov, family = binomial(logit), data = hpv, method = "brglmFit", type = "AS_median")
summary(lr.H58)

#Ekstrak nilai p value-nya saja utk var dep H58
pvH58 <- coef(summary(lr.H58))[,'Pr(>|z|)']

#Hitung Odds Ratio atau Exp(B) & 95% CI untuk OR var dep H58
OR.H58 <- exp(lr.H58$coefficients)
beta.H58 <- coef(lr.H58)
se.H58 <- coef(summary(lr.H58))[,2]
CIOR.H58 <- cbind(exp(beta.H58 + 1.96 * se.H58 * -1), exp(beta.H58 + 1.96 * se.H58 * 1))
colnames(CIOR.H58) <- c("lower 95%CI","upper 95%CI")


#Buat Tabel Hasil OR, 95%CI untuk OR dan p-value var dep H58
H58CIOR <- cbind(OR.H58,CIOR.H58,pvH58)
H58CIOR

#Uji Regresi Logistik dgn Var Dependen H59

lr.H59 <- glm(H59 ~ L6 + L11 + H16 + H18 + H31 + H33 + H35 + H39
          + L40.61 + L42 + L43.44 + H45 + H51 + H52 + H53 + L54.55 + H56 + 
          L57.71 + H58 + H66.68 + L70 + L72 + L73 + L81 + L82 + M84.26 + 
          Umur + Prov, family = binomial(logit), data = hpv, method = "brglmFit", type = "AS_median")
summary(lr.H59)

#Ekstrak nilai p value-nya saja utk var dep H59
pvH59 <- coef(summary(lr.H59))[,'Pr(>|z|)']

#Hitung Odds Ratio atau Exp(B) & 95% CI untuk OR var dep H59
OR.H59 <- exp(lr.H59$coefficients)
beta.H59 <- coef(lr.H59)
se.H59 <- coef(summary(lr.H59))[,2]
CIOR.H59 <- cbind(exp(beta.H59 + 1.96 * se.H59 * -1), exp(beta.H59 + 1.96 * se.H59 * 1))
colnames(CIOR.H59) <- c("lower 95%CI","upper 95%CI")


#Buat Tabel Hasil OR, 95%CI untuk OR dan p-value var dep H59
H59CIOR <- cbind(OR.H59,CIOR.H59,pvH59)
H59CIOR

#Uji Regresi Logistik dgn Var Dependen H66.68

lr.H66.68 <- glm(H66.68 ~ L6 + L11 + H16 + H18 + H31 + H33 + 
              H35 + H39 + L40.61 + L42 + L43.44 + H45 + H51 + H52 + H53 + 
              L54.55 + H56 + L57.71 + H58 + H59 + L70 + L72 + L73 + 
              L81 + L82 + M84.26 + Umur + Prov, family = binomial(logit), data = hpv, method = "brglmFit", type = "AS_median")
summary(lr.H66.68)

#Ekstrak nilai p value-nya saja utk var dep H66.68
pvH66.68 <- coef(summary(lr.H66.68))[,'Pr(>|z|)']

#Hitung Odds Ratio atau Exp(B) & 95% CI untuk OR var dep H66.68
OR.H66.68 <- exp(lr.H66.68$coefficients)
beta.H66.68 <- coef(lr.H66.68)
se.H66.68 <- coef(summary(lr.H66.68))[,2]
CIOR.H66.68 <- cbind(exp(beta.H66.68 + 1.96 * se.H66.68 * -1), exp(beta.H66.68 + 1.96 * se.H66.68 * 1))
colnames(CIOR.H66.68) <- c("lower 95%CI","upper 95%CI")


#Buat Tabel Hasil OR, 95%CI untuk OR dan p-value var dep H66.68
H66.68CIOR <- cbind(OR.H66.68,CIOR.H66.68,pvH66.68)
H66.68CIOR

#Uji Regresi Logistik dgn Var Dependen L70

lr.L70 <- glm(L70 ~ L6 + L11 + H16 + H18 + H31 + H33 + H35 + H39
          + L40.61 + L42 + L43.44 + H45 + H51 + H52 + H53 + L54.55 + H56 + 
          L57.71 + H58 + H59 + H66.68 + L73 + L81 + M84.26 + 
          Umur + Prov, family = binomial(logit), data = hpv, method = "brglmFit", type = "AS_median")
summary(lr.L70)

#Ekstrak nilai p value-nya saja utk var dep L70
pvL70 <- coef(summary(lr.L70))[,'Pr(>|z|)']

#Hitung Odds Ratio atau Exp(B) & 95% CI untuk OR var dep L70
OR.L70 <- exp(lr.L70$coefficients)
beta.L70 <- coef(lr.L70)
se.L70 <- coef(summary(lr.L70))[,2]
CIOR.L70 <- cbind(exp(beta.L70 + 1.96 * se.L70 * -1), exp(beta.L70 + 1.96 * se.L70 * 1))
colnames(CIOR.L70) <- c("lower 95%CI","upper 95%CI")


#Buat Tabel Hasil OR, 95%CI untuk OR dan p-value var dep L70
L70CIOR <- cbind(OR.L70,CIOR.L70,pvL70)
L70CIOR

#Uji Regresi Logistik dgn Var Dependen L72

lr.L72 <- glm(L72 ~ L6 + L11 + H16 + H18 + H31 + H33 + H35 + H39
          + L40.61 + L42 + L43.44 + H45 + H51 + H52 + H53 + L54.55 + H56 + 
          L57.71 + H58 + H59 + H66.68 + L70 + L73 + L81 + L82 + M84.26 + 
          Umur + Prov, family = binomial(logit), data = hpv, method = "brglmFit", type = "AS_median")
summary(lr.L72)

#Ekstrak nilai p value-nya saja utk var dep L72
pvL72 <- coef(summary(lr.L72))[,'Pr(>|z|)']

#Hitung Odds Ratio atau Exp(B) & 95% CI untuk OR var dep L72
OR.L72 <- exp(lr.L72$coefficients)
beta.L72 <- coef(lr.L72)
se.L72 <- coef(summary(lr.L72))[,2]
CIOR.L72 <- cbind(exp(beta.L72 + 1.96 * se.L72 * -1), exp(beta.L72 + 1.96 * se.L72 * 1))
colnames(CIOR.L72) <- c("lower 95%CI","upper 95%CI")

#Buat Tabel Hasil OR, 95%CI untuk OR dan p-value var dep L72
L72CIOR <- cbind(OR.L72,CIOR.L72,pvL72)
L72CIOR

#Uji Regresi Logistik dgn Var Dependen L73

lr.L73 <- glm(L73 ~ L6 + L11 + H16 + H18 + H31 + H33 + H35 + 
            H39 + L40.61 + L42 + L43.44 + H45 + H51 + H52 + H53 + L54.55 + 
            H56 + L57.71 + H58 + H59 + H66.68 + L70 + L72 + L81 + L82 
            + M84.26 + Umur + Prov, family = binomial(logit), data = hpv, method = "brglmFit", type = "AS_median")
summary(lr.L73)

#Ekstrak nilai p value-nya saja utk var dep L73
pvL73 <- coef(summary(lr.L73))[,'Pr(>|z|)']

#Hitung Odds Ratio atau Exp(B) & 95% CI untuk OR var dep L73
OR.L73 <- exp(lr.L73$coefficients)
beta.L73 <- coef(lr.L73)
se.L73 <- coef(summary(lr.L73))[,2]
CIOR.L73 <- cbind(exp(beta.L73 + 1.96 * se.L73 * -1), exp(beta.L73 + 1.96 * se.L73 * 1))
colnames(CIOR.L73) <- c("lower 95%CI","upper 95%CI")

#Buat Tabel Hasil OR, 95%CI untuk OR dan p-value var dep L73
L73CIOR <- cbind(OR.L73,CIOR.L73,pvL73)
L73CIOR

#Uji Regresi Logistik dgn Var Dependen L81

lr.L81 <- glm(L81 ~ L6 + L11 + H16 + H18 + H31 + H33 + H35 + 
            H39 + L40.61 + L42 + L43.44 + H45 + H51 + H52 + H53 + L54.55 + 
            H56 + L57.71 + H58 + H59 + H66.68 + L70 + L72 + L73 + L82 + 
            M84.26 + Umur + Prov, family = binomial(logit), data = hpv, method = "brglmFit", type = "AS_median")
summary(lr.L81)

#Ekstrak nilai p value-nya saja utk var dep L81
pvL81 <- coef(summary(lr.L81))[,'Pr(>|z|)']

#Hitung Odds Ratio atau Exp(B) & 95% CI untuk OR var dep L81
OR.L81 <- exp(lr.L81$coefficients)
beta.L81 <- coef(lr.L81)
se.L81 <- coef(summary(lr.L81))[,2]
CIOR.L81 <- cbind(exp(beta.L81 + 1.96 * se.L81 * -1), exp(beta.L81 + 1.96 * se.L81 * 1))
colnames(CIOR.L81) <- c("lower 95%CI","upper 95%CI")

#Buat Tabel Hasil OR, 95%CI untuk OR dan p-value var dep L81
L81CIOR <- cbind(OR.L81,CIOR.L81,pvL81)
L81CIOR

#Uji Regresi Logistik dgn Var Dependen L82

lr.L82 <- glm(L82 ~ L6 + L11 + H16 + H18 + H31 + H33 + H35 + 
            H39 + L40.61 + L42 + L43.44 + H45 + H51 + H52 + H53 + L54.55 + 
            H56 + L57.71 + H58 + H59 + H66.68 + L70 + L72 + L73 + L81 + 
            M84.26 + Umur + Prov, family = binomial(logit), data = hpv, method = "brglmFit", type = "AS_median")
summary(lr.L82)

#Ekstrak nilai p value-nya saja utk var dep L82
pvL82 <- coef(summary(lr.L82))[,'Pr(>|z|)']

#Hitung Odds Ratio atau Exp(B) & 95% CI untuk OR var dep L82
OR.L82 <- exp(lr.L82$coefficients)
beta.L82 <- coef(lr.L82)
se.L82 <- coef(summary(lr.L82))[,2]
CIOR.L82 <- cbind(exp(beta.L82 + 1.96 * se.L82 * -1), exp(beta.L82 + 1.96 * se.L82 * 1))
colnames(CIOR.L82) <- c("lower 95%CI","upper 95%CI")

#Buat Tabel Hasil OR, 95%CI untuk OR dan p-value var dep L82
L82CIOR <- cbind(OR.L82,CIOR.L82,pvL82)
L82CIOR

#Uji Regresi Logistik dgn Var Dependen M84.26

lr.M84.26 <- glm(M84.26 ~ L6 + L11 + H16 + H18 + H31 + H33 +
                   H35 + H39 + L40.61 + L42 + L43.44 + H45 + H51 + H52 + H53 + L54.55 +
                   H56 + L57.71 + H58 + H59 + H66.68 + L70 + L73 + L81 + L82 + Umur + Prov, family = binomial(logit), data = hpv, method = "brglmFit", type = "AS_median")
summary(lr.M84.26)

#Ekstrak nilai p value-nya saja utk var dep M84.26
pvM84.26 <- coef(summary(lr.M84.26))[,'Pr(>|z|)']

#Hitung Odds Ratio atau Exp(B) & 95% CI untuk OR var dep M84.26
OR.M84.26 <- exp(lr.M84.26$coefficients)
beta.M84.26 <- coef(lr.M84.26)
se.M84.26 <- coef(summary(lr.M84.26))[,2]
CIOR.M84.26 <- cbind(exp(beta.M84.26 + 1.96 * se.M84.26 * -1), exp(beta.M84.26 + 1.96 * se.M84.26 * 1))
colnames(CIOR.M84.26) <- c("lower 95%CI","upper 95%CI")

#Buat Tabel Hasil OR, 95%CI untuk OR dan p-value var dep M84.26
M84.26CIOR <- cbind(OR.M84.26,CIOR.M84.26,pvM84.26)
M84.26CIOR

#Buat Tabel Gabungan Hasil Analisis Regresi Logistik dan Ekspor ke Excel
ORgab <-  rbind(L6CIOR,L11CIOR,H16CIOR,H18CIOR,H31CIOR,H33CIOR,
           H35CIOR,H39CIOR,L40.61CIOR,L42CIOR,L43.44CIOR,
           H45CIOR,H51CIOR,H52CIOR,H53CIOR,L54.55CIOR,
           H56CIOR,L57.71CIOR,H58CIOR,H59CIOR,H66.68CIOR,
           L70CIOR,L72CIOR,L73CIOR,L81CIOR,L82CIOR,M84.26CIOR)
ORgab

library(openxlsx)

write.xlsx(
  ORgab,
  file = "Reg Log Gab HPV(brglm2).xlsx",
  sheetName = "Sheet1",
  col.names = TRUE,
  row.names = TRUE,
  append = FALSE,
  showNA = TRUE,
  password = NULL
)

