getwd()
#install.packages("reshape2")
#library(reshape2)

# read in cleaned data
veg<-read_csv("processed_data/Seep_2025.csv")
# format dates
veg$Date<-as.Date(veg$Date,format<-"%m/%d/%Y")
veg$Year<-format(veg$Date,format<-"%Y")
print(veg)
colnames(veg)
# monitored the Phase 2 transects late, both times in February, but we will
# refer to the data as applying to the previous year
start_2024 <- as.Date("2025-01-01")
end_2024 <- as.Date("2025-02-24")
start_2025 <- as.Date("2026-01-01")
end_2025 <- as.Date("2026-02-10")
veg <- veg %>%
  mutate(Year = case_when(
          between(Date, start_2024, end_2024) ~ "2024",
          between(Date, start_2025, end_2025) ~ "2025",
          TRUE ~ Year),
         # fix spelling for species names
         PSOC = case_when(
           grepl(pattern = "Aparagus asparagoides", x = PSOC) ~ "Asparagus_asparagoides",
           grepl(pattern = "Asparagus asparagoides", x = PSOC) ~ "Asparagus_asparagoides",
           TRUE ~ PSOC)
         ) %>%
  # remove data from U2W; not relevant to Phase 1 or 2
  filter(Transect_Name != "U2W")
unique(veg$Year)

## filter out sums and NAs
veg <- veg %>%
  filter(PSOC!="Sum of Native Cover" & PSOC!= "Sum of Non-Native Cover" & 
           PSOC!="NA")


# find the sum percent cover of each species in each transect
veg_avg<-aggregate(Percent_Cover ~ PSOC + Year + Habitat + Transect_Name +
                     Cover_Category, veg, FUN=sum) %>%
  # divide by number of quadrats in each transect (16) to find the average
  mutate(Percent_Cover = Percent_Cover / 16)

# average by habitat
veg_avg_sum<-aggregate(Percent_Cover ~ PSOC + Year + Habitat + Cover_Category,
                       veg_avg, FUN=mean) %>%
  mutate(Percent_Cover = round(Percent_Cover, 1))

# rename species to remove underscore
veg_avg_sum$PSOC<-gsub("_"," ",veg_avg_sum$PSOC)
unique(veg_avg_sum$PSOC)


#Natives -----
# select the native species using Cover_Category
veg.nat<-veg_avg_sum[veg_avg_sum$`Cover_Category`=="NATIVE COVER",]
unique(veg.nat$`PSOC`)
print(veg.nat)

#Native Uplands ----
nat.ul<-veg.nat[veg.nat$Habitat=="Upland",]
nat.ul<-nat.ul[,c(1,2,5)]
nat.ul<-nat.ul %>%
  pivot_wider(names_from = Year, values_from = `Percent_Cover`)
nat.ul[is.na(nat.ul)] <- 0
nat.ul

nat.ul.l<-nat.ul %>% 
  pivot_longer(cols =! PSOC, names_to = "Year", values_to = "Percent_Cover")


nat.ul.l$Presence<-as.logical(nat.ul.l$Percent_Cover)
nat_ul_list <-ggplot(nat.ul.l, aes(x=Year, y=`PSOC`, fill=Percent_Cover)) +
  theme_minimal() +
  geom_tile(color = "black", linewidth = 0.2) +
  scale_fill_gradientn(colours = c("white", "darkseagreen1", "chartreuse4"),
                       values = scales::rescale(c(0,1,19)),
                       limits=c(0, 20)) +
  geom_text(data = nat.ul.l %>% filter(Percent_Cover > 0),
    aes(label = label_percent(accuracy = 1, scale = 1)(Percent_Cover)),
        color = "black", size = 3) +
  scale_y_discrete(limits = rev) +
  ggtitle("Native species recorded in upland habitat each year") + 
  labs(fill = "Average absolute \npercent cover") +
  ylab("Species")
nat_ul_list
ggsave("figures/Native_Plant_Presence_By_Year_Uplands.png", nat_ul_list, width=8 , 
       height=6 , units="in" , dpi=300)


#Native Wetlands ----
nat.wl<-veg.nat[veg.nat$Habitat=="Wetland",]
nat.wl<-nat.wl[,c(1,2,5)]
nat.wl<-nat.wl %>%
  pivot_wider(names_from = Year, values_from = `Percent_Cover`)
nat.wl[is.na(nat.wl)] <- 0
nat.wl

nat.wl.l<-nat.wl %>% 
  pivot_longer(cols =! PSOC, names_to = "Year", values_to = "Percent_Cover")


nat.wl.l$Presence<-as.logical(nat.wl.l$Percent_Cover)
nat_wl_list <-ggplot(nat.wl.l, aes(x=Year, y=`PSOC`, fill=Percent_Cover)) +
  theme_minimal() +
  geom_tile(color = "black", linewidth = 0.2) +
  scale_fill_gradientn(colours = c("white", "darkseagreen1", "chartreuse4"),
                       values = scales::rescale(c(0,1,35)),
                       limits=c(0, 40)) +
  geom_text(data = nat.wl.l %>% filter(Percent_Cover > 0),
            aes(label = label_percent(accuracy = 1, scale = 1)(Percent_Cover)),
            color = "black", size = 3) +
  scale_y_discrete(limits = rev) +
  ggtitle("Native species recorded in wetland habitat each year") + 
  labs(fill = "Average absolute \npercent cover") +
  ylab("Species")
nat_wl_list
ggsave("figures/Native_Plant_Presence_By_Year_Wetlands.png", nat_wl_list, width=8 , 
       height=6.5 , units="in" , dpi=300)


################################################################################################
################################################################################################
################################################################################################
#Nonnative Species ----

veg.NON<-veg_avg_sum[veg_avg_sum$`Cover_Category`=="NON-NATIVE COVER",]
unique(veg.NON$`PSOC`)
print(veg.NON)


#Non-native Uplands
NON.ul<-veg.NON[veg.NON$Habitat=="Upland",]
NON.ul<-NON.ul[,c(1,2,5)]
NON.ul<-NON.ul %>%
  pivot_wider(names_from = Year, values_from = `Percent_Cover`)
NON.ul[is.na(NON.ul)] <- 0
NON.ul

NON.ul.l<-NON.ul %>% 
  pivot_longer(cols =! PSOC, names_to = "Year", values_to = "Percent_Cover")


NON.ul.l$Presence<-as.logical(NON.ul.l$Percent_Cover)
NON_ul_list <-ggplot(NON.ul.l, aes(x=Year, y=`PSOC`, fill=Percent_Cover)) +
  theme_minimal() +
  geom_tile(color = "black", linewidth = 0.2) +
  scale_fill_gradientn(colours = c("white", "pink", "red"),
                       values = scales::rescale(c(0,1,95)),
                       limits=c(0, 100)) +
  geom_text(data = NON.ul.l %>% filter(Percent_Cover > 0),
            aes(label = label_percent(accuracy = 1, scale = 1)(Percent_Cover)),
            color = "black", size = 3) +
  scale_y_discrete(limits = rev) +
  ggtitle("Non-native species recorded in upland habitat each year") + 
  labs(fill = "Average absolute \npercent cover") +
  ylab("Species")
NON_ul_list
ggsave("figures/Nonnative_Plant_Presence_By_Year_Uplands.png", NON_ul_list, width=8 , 
       height=6 , units="in" , dpi=300)

#Non-native Wetlands
NON.wl<-veg.NON[veg.NON$Habitat=="Wetland",]
NON.wl<-NON.wl[,c(1,2,5)]
NON.wl<-NON.wl %>%
  pivot_wider(names_from = Year, values_from = `Percent_Cover`)
NON.wl[is.na(NON.wl)] <- 0
NON.wl

NON.wl.l<-NON.wl %>% 
  pivot_longer(cols =! PSOC, names_to = "Year", values_to = "Percent_Cover")


NON.wl.l$Presence<-as.logical(NON.wl.l$Percent_Cover)
NON_wl_list <-ggplot(NON.wl.l, aes(x=Year, y=`PSOC`, fill=Percent_Cover)) +
  theme_minimal() +
  geom_tile(color = "black", linewidth = 0.2) +
  scale_fill_gradientn(colours = c("white", "pink", "red"),
                       values = scales::rescale(c(0,1,99)),
                       limits=c(0, 100)) +
  geom_text(data = NON.wl.l %>% filter(Percent_Cover > 0),
            aes(label = label_percent(accuracy = 1, scale = 1)(Percent_Cover)),
            color = "black", size = 3) +
  scale_y_discrete(limits = rev) +
  ggtitle("Non-native species recorded in wetland habitat each year") + 
  labs(fill = "Average absolute \npercent cover") +
  ylab("Species")
NON_wl_list
ggsave("figures/Nonnative_Plant_Presence_By_Year_Wetlands.png", NON_wl_list, width=8 , 
       height=6.5 , units="in" , dpi=300)

