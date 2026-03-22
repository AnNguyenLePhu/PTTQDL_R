# ============================================================================
# PHÂN TÍCH DỮ LIỆU MARKETING CAMPAIGN
# PHÂN TÍCH HÀNH VI KHÁCH HÀNG VÀ HIỆU QUẢ CHIẾN DỊCH MARKETING
# ============================================================================

# PHẦN 1: ĐỌC VÀ KIỂM TRA DỮ LIỆU
# ============================================================================

# Đọc file dữ liệu, các cột được phân cách bằng dấu tab
marketing <- read.csv("C:/Users/Binh An/OneDrive/Máy tính/CODER/.vscode/Y3/Phân tích và trực quan dữ liệu/RStudio/Baitap/Data/marketing_campaign.csv", sep = "\t", stringsAsFactors = FALSE, na.strings = c("", "NA", "NULL"))

# Hiển thị thông tin tổng quan
cat("=== THÔNG TIN CƠ BẢN ===\n")
cat("Số lượng khách hàng:", nrow(marketing), "\n")
cat("Số lượng biến quan sát:", ncol(marketing), "\n\n")

# Xem cấu trúc dữ liệu
str(marketing)

# Xem thống kê tóm tắt
summary(marketing)

# Xem 10 dòng đầu tiên
head(marketing, 10)


# PHẦN 2: XỬ LÝ DỮ LIỆU BỊ THIẾU (NA)
# ============================================================================

cat("\n=== XỬ LÝ DỮ LIỆU BỊ THIẾU ===\n")

# Đếm số lượng NA trong từng cột
na_count <- colSums(is.na(marketing))
na_columns <- na_count[na_count > 0]

if(length(na_columns) > 0) {
  cat("Các cột có dữ liệu bị thiếu:\n")
  print(na_columns)
} else {
  cat("Không phát hiện dữ liệu bị thiếu\n")
}

# Xử lý NA cho cột Income (Thu nhập) - dùng median vì có outlier
if(sum(is.na(marketing$Income)) > 0) {
  marketing$Income <- as.numeric(as.character(marketing$Income))
  marketing$Income[is.na(marketing$Income)] <- median(marketing$Income, na.rm = TRUE)
  cat("Đã xử lý NA cho Income\n")
}

# Xử lý NA cho cột Year_Birth (Năm sinh) - dùng median và làm tròn
if(sum(is.na(marketing$Year_Birth)) > 0) {
  marketing$Year_Birth[is.na(marketing$Year_Birth)] <- round(median(marketing$Year_Birth, na.rm = TRUE))
  cat("Đã xử lý NA cho Year_Birth\n")
}

# Xử lý NA cho cột MntWines (Chi tiêu rượu) - dùng median
if(sum(is.na(marketing$MntWines)) > 0) {
  marketing$MntWines[is.na(marketing$MntWines)] <- median(marketing$MntWines, na.rm = TRUE)
  cat("Đã xử lý NA cho MntWines\n")
}

# Xử lý NA cho cột Response (Phản hồi) - dùng mode (giá trị xuất hiện nhiều nhất)
if(sum(is.na(marketing$Response)) > 0) {
  mode_response <- as.numeric(names(which.max(table(marketing$Response))))
  marketing$Response[is.na(marketing$Response)] <- mode_response
  cat("Đã xử lý NA cho Response\n")
}

# Kiểm tra lại
cat("Tổng số NA còn lại:", sum(is.na(marketing)), "\n\n")


# PHẦN 3: TẠO CÁC BIẾN MỚI PHỤC VỤ PHÂN TÍCH
# ============================================================================

cat("=== TẠO BIẾN MỚI ===\n")

# Tính tuổi khách hàng (năm hiện tại 2024)
marketing$Age <- 2024 - marketing$Year_Birth
cat("- Đã tạo biến Age (tuổi)\n")

# Tính tổng chi tiêu của khách hàng
marketing$TotalSpend <- marketing$MntWines + marketing$MntFruits + marketing$MntMeatProducts + marketing$MntFishProducts + marketing$MntSweetProducts + marketing$MntGoldProds
cat("- Đã tạo biến TotalSpend (tổng chi tiêu)\n")

# Tính tổng số con trong gia đình
marketing$TotalKids <- marketing$Kidhome + marketing$Teenhome
cat("- Đã tạo biến TotalKids (tổng số con)\n")

# Chuyển đổi ngày tháng
marketing$Dt_Customer <- as.Date(marketing$Dt_Customer, format = "%d-%m-%Y")
cat("- Đã chuyển đổi Dt_Customer sang định dạng ngày\n")

# Tính số ngày làm khách hàng
marketing$CustomerDays <- as.numeric(difftime(Sys.Date(), marketing$Dt_Customer, units = "days"))
cat("- Đã tạo biến CustomerDays (số ngày làm khách)\n\n")


# PHẦN 4: THỐNG KÊ MÔ TẢ CƠ BẢN
# ============================================================================

cat("=== THỐNG KÊ MÔ TẢ ===\n")

# Thống kê về độ tuổi
cat("\n--- ĐỘ TUỔI ---\n")
cat("Trung bình:", round(mean(marketing$Age), 1), "tuổi\n")
cat("Trung vị:", median(marketing$Age), "tuổi\n")
cat("Nhỏ nhất:", min(marketing$Age), "tuổi\n")
cat("Lớn nhất:", max(marketing$Age), "tuổi\n")

# Thống kê về thu nhập
cat("\n--- THU NHẬP (Euro) ---\n")
cat("Trung bình:", round(mean(marketing$Income), 0), "\n")
cat("Trung vị:", median(marketing$Income), "\n")
cat("Nhỏ nhất:", min(marketing$Income), "\n")
cat("Lớn nhất:", max(marketing$Income), "\n")

# Thống kê về chi tiêu
cat("\n--- CHI TIÊU (Euro) ---\n")
cat("Tổng chi tiêu:", round(mean(marketing$TotalSpend), 0), "\n")
cat("Rượu vang:", round(mean(marketing$MntWines), 0), "\n")
cat("Thịt:", round(mean(marketing$MntMeatProducts), 0), "\n")
cat("Cá:", round(mean(marketing$MntFishProducts), 0), "\n")
cat("Trái cây:", round(mean(marketing$MntFruits), 0), "\n")
cat("Bánh kẹo:", round(mean(marketing$MntSweetProducts), 0), "\n")
cat("Vàng:", round(mean(marketing$MntGoldProds), 0), "\n")

# Thống kê về hành vi mua sắm
cat("\n--- HÀNH VI MUA SẮM ---\n")
cat("Mua qua web:", round(mean(marketing$NumWebPurchases), 2), "lần\n")
cat("Mua qua catalog:", round(mean(marketing$NumCatalogPurchases), 2), "lần\n")
cat("Mua tại store:", round(mean(marketing$NumStorePurchases), 2), "lần\n")
cat("Mua giảm giá:", round(mean(marketing$NumDealsPurchases), 2), "lần\n")
cat("Truy cập web/tháng:", round(mean(marketing$NumWebVisitsMonth), 2), "lần\n")

# Tỷ lệ phản hồi chung
cat("\n--- TỶ LỆ PHẢN HỒI ---\n")
cat("Tỷ lệ phản hồi chung:", round(mean(marketing$Response) * 100, 2), "%\n\n")


# PHẦN 5: PHÂN TÍCH THEO TRÌNH ĐỘ HỌC VẤN
# ============================================================================

cat("=== PHÂN TÍCH THEO TRÌNH ĐỘ HỌC VẤN ===\n")

# Số lượng theo từng trình độ
edu_count <- table(marketing$Education)
cat("\n--- SỐ LƯỢNG ---\n")
print(sort(edu_count, decreasing = TRUE))

# Thu nhập trung bình theo trình độ
edu_income <- aggregate(Income ~ Education, marketing, mean)
edu_income <- edu_income[order(edu_income$Income, decreasing = TRUE),]
cat("\n--- THU NHẬP TRUNG BÌNH (Euro) ---\n")
print(edu_income)

# Chi tiêu trung bình theo trình độ
edu_spend <- aggregate(TotalSpend ~ Education, marketing, mean)
edu_spend <- edu_spend[order(edu_spend$TotalSpend, decreasing = TRUE),]
cat("\n--- CHI TIÊU TRUNG BÌNH (Euro) ---\n")
print(edu_spend)

# Tỷ lệ phản hồi theo trình độ
edu_resp <- aggregate(Response ~ Education, marketing, function(x) mean(x) * 100)
edu_resp <- edu_resp[order(edu_resp$Response, decreasing = TRUE),]
cat("\n--- TỶ LỆ PHẢN HỒI (%) ---\n")
print(edu_resp)


# PHẦN 6: PHÂN TÍCH THEO TÌNH TRẠNG HÔN NHÂN
# ============================================================================

cat("\n=== PHÂN TÍCH THEO TÌNH TRẠNG HÔN NHÂN ===\n")

# Số lượng theo từng tình trạng
marital_count <- table(marketing$Marital_Status)
cat("\n--- SỐ LƯỢNG ---\n")
print(sort(marital_count, decreasing = TRUE))

# Thu nhập trung bình theo tình trạng
marital_income <- aggregate(Income ~ Marital_Status, marketing, mean)
marital_income <- marital_income[order(marital_income$Income, decreasing = TRUE),]
cat("\n--- THU NHẬP TRUNG BÌNH (Euro) ---\n")
print(marital_income)

# Tỷ lệ phản hồi theo tình trạng
marital_resp <- aggregate(Response ~ Marital_Status, marketing, function(x) mean(x) * 100)
marital_resp <- marital_resp[order(marital_resp$Response, decreasing = TRUE),]
cat("\n--- TỶ LỆ PHẢN HỒI (%) ---\n")
print(marital_resp)


# PHẦN 7: PHÂN TÍCH THEO SỐ CON
# ============================================================================

cat("\n=== PHÂN TÍCH THEO SỐ CON ===\n")

# Phân bố số con
kid_dist <- table(marketing$TotalKids)
cat("\n--- PHÂN BỐ SỐ CON ---\n")
print(kid_dist)

# Thu nhập theo số con
kid_income <- aggregate(Income ~ TotalKids, marketing, mean)
cat("\n--- THU NHẬP TRUNG BÌNH THEO SỐ CON (Euro) ---\n")
print(kid_income)

# Tỷ lệ phản hồi theo số con
kid_resp <- aggregate(Response ~ TotalKids, marketing, function(x) mean(x) * 100)
cat("\n--- TỶ LỆ PHẢN HỒI THEO SỐ CON (%) ---\n")
print(kid_resp)


# PHẦN 8: PHÂN TÍCH THEO NHÓM TUỔI
# ============================================================================

cat("\n=== PHÂN TÍCH THEO NHÓM TUỔI ===\n")

# Phân nhóm tuổi
age_group <- cut(marketing$Age, breaks = c(0, 30, 40, 50, 60, 100), 
                 labels = c("Dưới 30", "30-40", "40-50", "50-60", "Trên 60"))

# Thống kê theo nhóm tuổi
age_stats <- aggregate(cbind(Income, TotalSpend, Response) ~ age_group, marketing, mean)
age_stats$Response <- age_stats$Response * 100
age_stats <- age_stats[order(age_stats$age_group),]
cat("\n--- THỐNG KÊ THEO NHÓM TUỔI ---\n")
print(age_stats)


# PHẦN 9: PHÂN TÍCH HIỆU QUẢ CHIẾN DỊCH
# ============================================================================

cat("\n=== PHÂN TÍCH HIỆU QUẢ CHIẾN DỊCH ===\n")

# Danh sách các chiến dịch
campaigns <- c("AcceptedCmp1", "AcceptedCmp2", "AcceptedCmp3", "AcceptedCmp4", "AcceptedCmp5")

# Tính tỷ lệ chấp nhận từng chiến dịch
camp_rates <- sapply(campaigns, function(x) mean(marketing[[x]], na.rm = TRUE) * 100)

cat("\n--- TỶ LỆ CHẤP NHẬN TỪNG CHIẾN DỊCH ---\n")
for(i in 1:5) {
  cat(campaigns[i], ":", round(camp_rates[i], 2), "%\n")
}

# Chiến dịch hiệu quả nhất
best_camp <- campaigns[which.max(camp_rates)]
cat("\nChiến dịch hiệu quả nhất:", best_camp, "với tỷ lệ", round(max(camp_rates), 2), "%\n")


# PHẦN 10: PHÂN TÍCH MỐI QUAN HỆ TƯƠNG QUAN
# ============================================================================

cat("\n=== PHÂN TÍCH MỐI QUAN HỆ ===\n")

# Chọn các biến để phân tích tương quan
cor_vars <- c("Age", "Income", "TotalSpend", "Recency", "NumWebVisitsMonth", "NumDealsPurchases")

# Tính ma trận tương quan
cor_matrix <- cor(marketing[, cor_vars], use = "complete.obs")

cat("\n--- MA TRẬN TƯƠNG QUAN ---\n")
print(round(cor_matrix, 3))

# Giải thích các mối quan hệ đáng chú ý
cat("\n--- GIẢI THÍCH ---\n")
cat("- TotalSpend và Income:", round(cor_matrix["TotalSpend", "Income"], 3), 
    "- Thu nhập cao thì chi tiêu nhiều\n")
cat("- TotalSpend và Recency:", round(cor_matrix["Recency", "TotalSpend"], 3), 
    "- Khách hàng mua gần đây chi tiêu nhiều hơn\n")
cat("- WebVisits và TotalSpend:", round(cor_matrix["NumWebVisitsMonth", "TotalSpend"], 3), 
    "- Truy cập web nhiều thì chi tiêu nhiều\n")


# PHẦN 11: PHÁT HIỆN KHÁCH HÀNG TIỀM NĂNG
# ============================================================================

cat("\n=== KHÁCH HÀNG TIỀM NĂNG ===\n")

# Khách hàng thu nhập cao nhưng chưa phản hồi
high_income <- quantile(marketing$Income, 0.75)
potential_income <- marketing[marketing$Income > high_income & marketing$Response == 0, ]
cat("\nKhách hàng thu nhập cao chưa phản hồi:", nrow(potential_income), "người\n")

# Khách hàng chi tiêu cao nhưng chưa phản hồi
high_spend <- quantile(marketing$TotalSpend, 0.75)
potential_spend <- marketing[marketing$TotalSpend > high_spend & marketing$Response == 0, ]
cat("Khách hàng chi tiêu cao chưa phản hồi:", nrow(potential_spend), "người\n")

# Hiển thị 10 khách hàng tiềm năng đầu tiên
if(nrow(potential_income) > 0) {
  cat("\nTop 10 khách hàng tiềm năng (thu nhập cao):\n")
  print(head(potential_income[, c("ID", "Income", "TotalSpend", "Education")], 10))
}


# PHẦN 12: PHÁT HIỆN GIÁ TRỊ BẤT THƯỜNG
# ============================================================================

cat("\n=== PHÁT HIỆN GIÁ TRỊ BẤT THƯỜNG ===\n")

# Phát hiện outlier cho thu nhập
q1_inc <- quantile(marketing$Income, 0.25)
q3_inc <- quantile(marketing$Income, 0.75)
iqr_inc <- q3_inc - q1_inc
outliers_inc <- marketing[marketing$Income < (q1_inc - 1.5 * iqr_inc) | marketing$Income > (q3_inc + 1.5 * iqr_inc), ]
cat("\nOutlier về thu nhập:", nrow(outliers_inc), "người\n")

# Phát hiện outlier cho chi tiêu
q1_spend <- quantile(marketing$TotalSpend, 0.25)
q3_spend <- quantile(marketing$TotalSpend, 0.75)
iqr_spend <- q3_spend - q1_spend
outliers_spend <- marketing[marketing$TotalSpend < (q1_spend - 1.5 * iqr_spend) | marketing$TotalSpend > (q3_spend + 1.5 * iqr_spend), ]
cat("Outlier về chi tiêu:", nrow(outliers_spend), "người\n")


# PHẦN 13: PHÂN TÍCH THEO KÊNH MUA
# ============================================================================

cat("\n=== PHÂN TÍCH THEO KÊNH MUA ===\n")

# So sánh các kênh mua hàng
channels <- c("Web", "Catalog", "Store", "Deals")
channel_means <- c(
  mean(marketing$NumWebPurchases),
  mean(marketing$NumCatalogPurchases),
  mean(marketing$NumStorePurchases),
  mean(marketing$NumDealsPurchases)
)

cat("\n--- SỐ LẦN MUA TRUNG BÌNH ---\n")
for(i in 1:4) {
  cat(channels[i], ":", round(channel_means[i], 2), "lần\n")
}

# Kênh được ưa chuộng nhất
best_channel <- channels[which.max(channel_means)]
cat("\nKênh mua phổ biến nhất:", best_channel, "với", round(max(channel_means), 2), "lần\n")


# PHẦN 14: LƯU DỮ LIỆU VÀ KẾT THÚC
# ============================================================================
# Đường dẫn lưu file
save_path <- "C:/Users/Binh An/OneDrive/Máy tính/CODER/.vscode/Y3/Phân tích và trực quan dữ liệu/RStudio/Baitap/Data/"

# Lưu dữ liệu đã xử lý dạng RData
save(marketing, file = paste0(save_path, "marketing_processed.RData"))
cat("\n=== ĐÃ LƯU DỮ LIỆU ===\n")
cat("- File RData:", save_path, "marketing_processed.RData\n")

# Lưu dữ liệu đã xử lý dạng CSV
write.csv(marketing, paste0(save_path, "marketing_cleaned.csv"), row.names = FALSE)
cat("- File CSV:", save_path, "marketing_cleaned.csv\n")

# Lưu báo cáo phân tích
sink(paste0(save_path, "marketing_analysis_report.txt"))
cat("=== BÁO CÁO PHÂN TÍCH DỮ LIỆU MARKETING ===\n")
cat("Ngày phân tích:", Sys.Date(), "\n")
cat("Số lượng khách hàng:", nrow(marketing), "\n")
cat("Số lượng biến:", ncol(marketing), "\n\n")

cat("--- THỐNG KÊ CHÍNH ---\n")
cat("Tuổi trung bình:", round(mean(marketing$Age), 1), "tuổi\n")
cat("Thu nhập trung bình:", round(mean(marketing$Income), 0), "Euro\n")
cat("Chi tiêu trung bình:", round(mean(marketing$TotalSpend), 0), "Euro\n")
cat("Tỷ lệ phản hồi:", round(mean(marketing$Response) * 100, 2), "%\n")
cat("Kênh mua phổ biến nhất:", best_channel, "\n")
cat("Chiến dịch hiệu quả nhất:", best_camp, "\n")
sink()

cat("- File báo cáo:", save_path, "marketing_analysis_report.txt\n")

# Xem dữ liệu cuối cùng
View(marketing)

