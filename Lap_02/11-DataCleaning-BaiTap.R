# ============================================================================
# BAI TAP THUC HANH - PHAN TICH DU LIEU KHACH HANG
# Xu ly du lieu clients.csv
# ============================================================================

# 1. Doc du lieu
clients <- read.csv("C:/Users/Binh An/OneDrive/Máy tính/CODER/.vscode/Y3/Phân tích và trực quan dữ liệu/RStudio/Baitap/Data/clients.csv", 
                    stringsAsFactors = FALSE)

# Xem 6 dong dau tien
head(clients)

# Kiem tra cau truc du lieu
str(clients)

# Tom tat thong ke
summary(clients)

# ============================================================================
# PHAT HIEN VA DIEU CHINH DU LIEU BI LECH
# ============================================================================

# 1) DIEU CHINH NHOM DU LIEU BI SAI "2n Cycle"
# Dac diem nhan dang: cot Education co gia tri "2n" va cot Marital_Status co gia tri "Cycle"
# ============================================================================

# Xac dinh cac dong du lieu bi sai
index_cycle <- clients$Marital_Status == "Cycle"
cat("So luong dong bi sai 2n Cycle:", sum(index_cycle), "\n")

# Danh sach cac cot can dich chuyen sang trai
cot_dich_trai <- c("Marital_Status", "Income", "Kidhome", "Teenhome", "Dt_Customer",
                   "Recency", "MntWines", "MntFruits", "MntMeatProducts", "MntFishProducts",
                   "MntSweetProducts", "MntGoldProds", "NumDealsPurchases", "NumWebPurchases",
                   "NumCatalogPurchases", "NumStorePurchases", "NumWebVisitsMonth",
                   "AcceptedCmp3", "AcceptedCmp4", "AcceptedCmp5", "AcceptedCmp1",
                   "AcceptedCmp2", "Complain", "Z_CostContact", "Z_Revenue", "Response")

# Luu giu gia tri truoc khi thuc hien dich chuyen
du_lieu_tam <- clients[index_cycle, cot_dich_trai]

# Hop nhat gia tri cho cot Education
clients$Education[index_cycle] <- "2n Cycle"

# Thuc hien dich chuyen cac cot sang trai mot vi tri
clients[index_cycle, cot_dich_trai[-length(cot_dich_trai)]] <- du_lieu_tam[, cot_dich_trai[-1]]

# Cot cuoi cung se bi trong sau khi dich chuyen
clients$Response[index_cycle] <- NA

# Xac nhan ket qua sau dieu chinh
cat("So dong con bi loi Marital_Status = Cycle:", 
    sum(clients$Marital_Status == "Cycle", na.rm = TRUE), "\n")


# 2) DIEU CHINH NHOM DU LIEU BI SAI DINH DANG NGAY THANG
# Dac diem nhan dang: cot Teenhome co dinh dang ngay thang (dd-mm-yyyy)
# ============================================================================

# Xac dinh cac dong co dinh dang ngay thang
index_date <- grepl("^[0-9]{2}-[0-9]{2}-[0-9]{4}$", clients$Teenhome)
cat("So luong dong bi sai dinh dang ngay thang:", sum(index_date), "\n")

# Danh sach cac cot can dich chuyen sang phai
cot_dich_phai <- c("Income", "Kidhome", "Teenhome", "Dt_Customer", "Recency", "MntWines",
                   "MntFruits", "MntMeatProducts", "MntFishProducts", "MntSweetProducts",
                   "MntGoldProds", "NumDealsPurchases", "NumWebPurchases",
                   "NumCatalogPurchases", "NumStorePurchases", "NumWebVisitsMonth",
                   "AcceptedCmp3", "AcceptedCmp4", "AcceptedCmp5", "AcceptedCmp1",
                   "AcceptedCmp2", "Complain", "Z_CostContact", "Z_Revenue", "Response")

# Luu giu gia tri truoc khi thuc hien dich chuyen
du_lieu_tam_phai <- clients[index_date, cot_dich_phai]

# Thuc hien dich chuyen cac cot sang phai mot vi tri
clients[index_date, cot_dich_phai[-1]] <- du_lieu_tam_phai[, cot_dich_phai[-length(cot_dich_phai)]]

# Cot Income cua cac dong nay bi thieu du lieu
clients$Income[index_date] <- NA

# Xac nhan ket qua sau dieu chinh
cat("So dong con bi sai dinh dang ngay thang:", 
    sum(grepl("^[0-9]{2}-[0-9]{2}-[0-9]{4}$", clients$Teenhome)), "\n\n")


# ============================================================================
# CHUYEN DOI KIEU DU LIEU SANG SO
# ============================================================================

# Danh sach cac bien can chuyen sang kieu so
bien_so <- c("X", "ID", "Year_Birth", "Income", "Kidhome", "Teenhome", "Recency",
             "MntWines", "MntFruits", "MntMeatProducts", "MntFishProducts",
             "MntSweetProducts", "MntGoldProds", "NumDealsPurchases",
             "NumWebPurchases", "NumCatalogPurchases", "NumStorePurchases",
             "NumWebVisitsMonth", "AcceptedCmp3", "AcceptedCmp4", "AcceptedCmp5",
             "AcceptedCmp1", "AcceptedCmp2", "Complain", "Z_CostContact",
             "Z_Revenue", "Response")

# Ham chuyen doi an toan
chuyen_so <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("NA", "N/A", "")] <- NA
  suppressWarnings(as.numeric(x))
}

# Thuc hien chuyen doi
clients[bien_so] <- lapply(clients[bien_so], chuyen_so)

# Kiem tra ket qua chuyen doi
cat("=== KET QUA SAU CHUYEN DOI KIEU SO ===\n")
cat("So luong dong co Kidhome lon hon 2:", sum(clients$Kidhome > 2, na.rm = TRUE), "\n")
cat("So luong dong co Teenhome lon hon 2:", sum(clients$Teenhome > 2, na.rm = TRUE), "\n\n")


# ============================================================================
# XU LY GIA TRI KHUYET THIEU (NA)
# ============================================================================

cat("=== THONG KE GIA TRI KHUYET THIEU TRUOC KHI XU LY ===\n")
thong_ke_na <- colSums(is.na(clients))
print(thong_ke_na[thong_ke_na > 0])
cat("Tong so gia tri khuyet thieu:", sum(is.na(clients)), "\n\n")

# Dieu chinh gia tri cho Year_Birth (su dung trung vi va lam tron)
clients$Year_Birth[is.na(clients$Year_Birth)] <- round(median(clients$Year_Birth, na.rm = TRUE))
cat("Hoan tat dieu chinh Year_Birth\n")

# Dieu chinh gia tri cho Income (su dung trung vi)
clients$Income[is.na(clients$Income)] <- median(clients$Income, na.rm = TRUE)
cat("Hoan tat dieu chinh Income\n")

# Dieu chinh gia tri cho MntWines (su dung trung vi)
clients$MntWines[is.na(clients$MntWines)] <- median(clients$MntWines, na.rm = TRUE)
cat("Hoan tat dieu chinh MntWines\n")

# Dieu chinh gia tri cho Response (su dung gia tri xuat hien nhieu nhat)
gia_tri_dac_trung <- as.numeric(names(which.max(table(clients$Response))))
clients$Response[is.na(clients$Response)] <- gia_tri_dac_trung
cat("Hoan tat dieu chinh Response voi gia tri dac trung =", gia_tri_dac_trung, "\n\n")

# Kiem tra lai gia tri khuyet thieu
cat("=== THONG KE GIA TRI KHUYET THIEU SAU KHI XU LY ===\n")
thong_ke_na_sau <- colSums(is.na(clients))
print(thong_ke_na_sau[thong_ke_na_sau > 0])
cat("Tong so gia tri khuyet thieu con lai:", sum(is.na(clients)), "\n\n")

# Xac nhan ket qua xu ly
if(sum(is.na(clients)) == 0) {
  cat("Hoan tat xu ly tat ca gia tri khuyet thieu\n\n")
} else {
  cat("Con", sum(is.na(clients)), "gia tri khuyet thieu chua duoc xu ly\n\n")
}

# Hien thi cac dong van con gia tri thieu (neu co)
if(sum(is.na(clients)) > 0) {
  cat("Cac dong du lieu con chua gia tri khuyet thieu:\n")
  print(clients[!complete.cases(clients), ])
}


# ============================================================================
# CHUYEN DOI KIEU DU LIEU THANH FACTOR
# ============================================================================

cat("=== CHUYEN DOI KIEU DU LIEU THANH FACTOR ===\n")

# Chuyen doi Marital_Status sang kieu factor
clients$Marital_Status <- factor(clients$Marital_Status)
cat("Da chuyen doi Marital_Status sang kieu factor\n")

# Chuyen doi Education sang kieu ordered factor
clients$Education <- ordered(clients$Education,
                             levels = c("Basic", "2n Cycle", "Graduation", "Master", "PhD"))
cat("Da chuyen doi Education sang kieu ordered factor\n")

# Chuyen doi cac bien nhi phan sang kieu factor
bien_nhi_phan <- c("AcceptedCmp1", "AcceptedCmp2", "AcceptedCmp3", 
                   "AcceptedCmp4", "AcceptedCmp5", "Complain", "Response")

for(var in bien_nhi_phan) {
  clients[[var]] <- factor(clients[[var]])
}
cat("Da chuyen doi", length(bien_nhi_phan), "bien nhi phan sang kieu factor\n\n")


# ============================================================================
# KIEM TRA VA DANH GIA KET QUA
# ============================================================================

cat("=== TONG QUAN KET QUA XU LY ===\n")
cat("So luong quan sat:", nrow(clients), "\n")
cat("So luong bien:", ncol(clients), "\n")
cat("Tong so gia tri khuyet thieu:", sum(is.na(clients)), "\n\n")

cat("=== CAU TRUC DU LIEU ===\n")
str(clients)

cat("\n=== THONG KE MO TA ===\n")
summary(clients)

cat("\n=== PHAN BO TRINH DO HOC VAN ===\n")
print(table(clients$Education))

cat("\n=== PHAN BO TINH TRANG HON NHAN ===\n")
print(table(clients$Marital_Status))

cat("\n=== PHAN BO PHAN HOI ===\n")
print(table(clients$Response))


# ============================================================================
# PHAT HIEN CAC GIA TRI BAT THUONG
# ============================================================================

cat("\n=== PHAT HIEN GIA TRI BAT THUONG ===\n")
cat("So dong co thu nhap nho hon hoac bang 0:", sum(clients$Income <= 0, na.rm = TRUE), "\n")
cat("So dong co nam sinh nho hon 1900:", sum(clients$Year_Birth < 1900, na.rm = TRUE), "\n")
cat("So dong co nam sinh lon hon 2020:", sum(clients$Year_Birth > 2020, na.rm = TRUE), "\n")
cat("So dong co so tre nho hon 2 tuoi:", sum(clients$Kidhome > 2, na.rm = TRUE), "\n")
cat("So dong co so thanh thieu nien:", sum(clients$Teenhome > 2, na.rm = TRUE), "\n")


# ============================================================================
# LUU TRU KET QUA
# ============================================================================

# Luu du lieu da xu ly dang RData
save(clients, file = "C:/Users/Binh An/OneDrive/Máy tính/CODER/.vscode/Y3/Phân tích và trực quan dữ liệu/RStudio/Baitap/Data/clientsInR.RData")
cat("\nDa luu du lieu vao tep clientsInR.RData\n")

# Luu du lieu da xu ly dang CSV
write.csv(clients, 
          "C:/Users/Binh An/OneDrive/Máy tính/CODER/.vscode/Y3/Phân tích và trực quan dữ liệu/RStudio/Baitap/Data/clients_cleaned.csv", 
          row.names = FALSE)
cat("Da luu du lieu da xu ly vao tep clients_cleaned.csv\n")

# Hien thi ket qua cuoi cung
View(clients)

