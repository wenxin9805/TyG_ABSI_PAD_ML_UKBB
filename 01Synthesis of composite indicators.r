df <- read.csv("baseline_imputed_final.csv")
#Composite variable generation
colnames(df)

# 1.convert TG and FBG from mmol/L to mg/dL
df$TG_mgdl <- df$Triglycerides * 88.57
df$FBG_mgdl <- df$Glucose * 18

# 2.Calculate TyG index
df$tyg <- log((df$TG_mgdl * df$FBG_mgdl) / 2)

# Unit conversion
df$TC_mgdl   <- df$Cholesterol * 38.67
df$HDL_mgdl  <- df$HDL_cholesterol * 38.67

# Calculate CHG index
df$CHG <- log((df$TC_mgdl * df$FBG_mgdl) / (2 * df$HDL_mgdl))

# METS-IR formula calculation
df$mets_ir <- log(2 * df$FBG_mgdl + df$TG_mgdl) * df$BMI / log(df$HDL_mgdl)

# 1. Unit conversion
df$WC_m <- df$WC / 100                # WC: cm → m
df$Height_m <- sqrt(df$Weight / df$BMI)    # 身高公式

#2. Calculate ABSI
df$ABSI <- df$WC_m / (df$BMI^(2/3) * df$Height_m^(1/2))

# 3.  Calculate TyG-ABSI
df$TyG_ABSI <- df$tyg * df$ABSI

table(df$Sex)
# VAI calculation
# VAI for males
VAI_male <- (df$WC / (39.68 + 1.88 * df$BMI)) * (df$Triglycerides / 1.03) * (1.31 / df$HDL_cholesterol)

# VAI for females
VAI_female <- (df$WC / (36.58 + 1.89 * df$BMI)) * (df$Triglycerides / 0.81) * (1.52 / df$HDL_cholesterol)

# Assign VAI by sex
df$VAI <- ifelse(df$Sex == 1, VAI_male,
                 ifelse(df$Sex == 0, VAI_female, NA))

# Calculate TyG-VAI
df$TyG_VAI <- df$tyg * df$VAI

# LAP calculation
# LAP for males
LAP_male <- (df$WC - 65) * df$Triglycerides

# LAP for females
LAP_female <- (df$WC - 58) * df$Triglycerides

# Assign LAP values by sex
df$LAP <- ifelse(df$Sex == 1, LAP_male,
                 ifelse(df$Sex == 0, LAP_female, NA))
# Calculate TyG-LAP
df$TyG_LAP <- df$tyg * df$LAP


# Calculate TG/HDL-C ratio
df$TG_HDL_ratio <- df$TG_mgdl / df$HDL_mgdl

#  TyG-BMI
df$TyG_BMI <- df$tyg * df$BMI

# TyG-WC
df$TyG_WC <- df$tyg * df$WC

# TyG-WHtR
df$TyG_WHtR <- df$tyg* (df$WC / (df$Height_m * 100))   # WC and height are both in cm needs to be multiplied by 100

# Calculate CTI
df$CTI <- 0.412 * log(df$C_reactive_protein) + log( (df$TG_mgdl * df$FBG_mgdl) / 2 )

# Remove specific columns
df <- df[,-c(17,46:51,53,54,57,58)]

