df <- read.csv("baseline_imputed_final.csv")
#复合变量生成
colnames(df)
# 首先将TG和FBG从mmol/L转换为mg/dL
df$TG_mgdl <- df$Triglycerides * 88.57
df$FBG_mgdl <- df$Glucose * 18

# 计算TyG指数
df$tyg <- log((df$TG_mgdl * df$FBG_mgdl) / 2)
# 单位换算
df$TC_mgdl   <- df$Cholesterol * 38.67
df$HDL_mgdl  <- df$HDL_cholesterol * 38.67

# 计算 CHG index
df$CHG <- log((df$TC_mgdl * df$FBG_mgdl) / (2 * df$HDL_mgdl))
# METS-IR 计算公式
df$mets_ir <- log(2 * df$FBG_mgdl + df$TG_mgdl) * df$BMI / log(df$HDL_mgdl)
# 1. 换算单位
df$WC_m <- df$WC / 100                # WC: cm → m
df$Height_m <- sqrt(df$Weight / df$BMI)    # 身高公式

# 2. 计算 ABSI
df$ABSI <- df$WC_m / (df$BMI^(2/3) * df$Height_m^(1/2))
# 3. 计算 TyG-ABSI
df$TyG_ABSI <- df$tyg * df$ABSI
table(df$Sex)
#VAI
# 男性VAI
VAI_male <- (df$WC / (39.68 + 1.88 * df$BMI)) * (df$Triglycerides / 1.03) * (1.31 / df$HDL_cholesterol)

# 女性VAI
VAI_female <- (df$WC / (36.58 + 1.89 * df$BMI)) * (df$Triglycerides / 0.81) * (1.52 / df$HDL_cholesterol)

# 综合，按性别赋值
df$VAI <- ifelse(df$Sex == 1, VAI_male,
                 ifelse(df$Sex == 0, VAI_female, NA))
#计算 TyG-VAI
df$TyG_VAI <- df$tyg * df$VAI
# 计算男性LAP
LAP_male <- (df$WC - 65) * df$Triglycerides

# 计算女性LAP
LAP_female <- (df$WC - 58) * df$Triglycerides

# 根据性别选择赋值
df$LAP <- ifelse(df$Sex == 1, LAP_male,
                 ifelse(df$Sex == 0, LAP_female, NA))
#计算 TyG-LAP
df$TyG_LAP <- df$tyg * df$LAP


# 计算 TG/HDL-C
df$TG_HDL_ratio <- df$TG_mgdl / df$HDL_mgdl
# 2. TyG-BMI
df$TyG_BMI <- df$tyg * df$BMI

# 3. TyG-WC
df$TyG_WC <- df$tyg * df$WC

# 4. TyG-WHtR
df$TyG_WHtR <- df$tyg* (df$WC / (df$Height_m * 100))   # 因为WC、height都是cm，Height_m需乘100
df$CTI <- 0.412 * log(df$C_reactive_protein) + log( (df$TG_mgdl * df$FBG_mgdl) / 2 )
df <- df[,-c(17,46:51,53,54,57,58)]

