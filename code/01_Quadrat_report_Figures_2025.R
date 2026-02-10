library(ggfittext)
library(scales)

getwd()
veg<-read_csv("processed_data/Seep_2025.csv")
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
    TRUE ~ Year
  )) %>%
  # remove U2W data, this might be restored later down the line, but is not a
  # part of phase 1 or phase 2
  filter(Transect_Name != "U2W")

# select just native and non-native veg cover
vegNNN<-veg[veg$Cover_Category=="NATIVE COVER" | 
              veg$Cover_Category=="NON-NATIVE COVER",]
unique(vegNNN$Cover_Category)

# remove the rows the sum rows
vegNNN<-vegNNN[vegNNN$PSOC!="Sum of Native Cover" & 
                 vegNNN$PSOC!="Sum of Non-Native Cover",]
# sum the percent cover in each phase/habitat type
vegNNN<-aggregate(Percent_Cover~Phase+Habitat+Year+Cover_Category+Transect_Distance+
                  Transect_Name,vegNNN,FUN=sum)

# average percent cover in each phase/habitat type
vegNNNagg<-aggregate(Percent_Cover~Phase+Habitat+Year+Cover_Category,vegNNN,FUN=mean)
unique(vegNNNagg$Habitat)

# round percent_cover to the nearest decimal place and check the results
vegNNNagg <- vegNNNagg %>%
  mutate(Percent_Cover = round(Percent_Cover, 1))
unique(vegNNNagg)

#Bar Charts: Absolute Percent Cover ----
unique_phase <- unique(vegNNNagg$Phase)
unique_habitat <- unique(vegNNNagg$Habitat)

for (i in unique_phase) {
  for (j in unique_habitat) {
    q <- ggplot(data = subset(vegNNNagg, Phase == i & Habitat == j),
               aes(Year, Percent_Cover, fill=Cover_Category, 
                   label=Percent_Cover)) +
      geom_bar(position="dodge", stat="identity", color="black") +
      geom_bar_text(aes(label = label_percent(accuracy = 0.1, scale = 1)
                        (Percent_Cover)),
                    position = "dodge", outside = TRUE,
                    place = 'top', min.size = 6,
                    fontface='bold', contrast = TRUE) +
      ggtitle("Average Absolute Percent Cover", paste(i, j)) +
      theme_bw() + 
      theme(axis.text.x = element_text(angle = 90))+
      theme(axis.text.x = element_text(angle = 0, size = 12),
            axis.text.y = element_text(size=12),
            axis.title.y = element_text(
              size=14, margin = margin(t = 0, r = 10, b = 0, l = 20)),
            axis.title.x=element_text(
              size=14,margin = margin(t = 0, r = 10, b = 0, l = 20)),
            title = element_text(size=15, face='bold'),
            plot.subtitle = element_text(size=12, face='bold')) +
      scale_fill_manual(
        "Cover Type", values = c("NATIVE COVER" = "darkgreen",
                                 "NON-NATIVE COVER" = "red"))
    print(q)
    
    filename <- file.path("figures", paste0("Absolute_NNN_", i,
                                            "_", j, ".png"))
    ggsave(filename=filename, plot=q , width=8 , height=6 , units="in" , dpi=300)
  }
}


#Bar Charts: Relative Percent Cover ----
for (i in unique_phase) {
  for (j in unique_habitat) {
    p <- subset(vegNNNagg, Phase == i & Habitat == j) %>%
      group_by(Year) %>%
      mutate(
        rel_percent_cover = round((Percent_Cover / sum(Percent_Cover)*100), 1)
        ) %>%
      ungroup()
    
    q <- ggplot(p, aes(Year, Percent_Cover, fill=Cover_Category,
                       label = rel_percent_cover)) +
      geom_bar(position="fill", stat="identity", color="black") +
      geom_bar_text(aes(label = label_percent(accuracy = 0.1, scale = 1)
                        (rel_percent_cover)), 
                    outside = TRUE, place = 'center', 
                    min.size = 6, 
                    position = "fill",
                    fontface='bold',
                    contrast = TRUE) +
      ggtitle("Average Relative Percent Cover", paste(i, j)) +
      theme_bw() + 
      theme(axis.text.x = element_text(angle = 90))+
      theme(axis.text.x = element_text(angle = 0, size = 12),
            axis.text.y = element_text(size=12),
            axis.title.y = element_text(
              size=14, margin = margin(t = 0, r = 10, b = 0, l = 20)),
            axis.title.x=element_text(
              size=14,margin = margin(t = 0, r = 10, b = 0, l = 20)),
            title = element_text(size=15, face='bold'),
            plot.subtitle = element_text(size=12, face='bold')) +
      scale_fill_manual(
        "Cover Type", values = c("NATIVE COVER" = "darkgreen",
                                 "NON-NATIVE COVER" = "red"))
    print(q)
    
    filename <- file.path("figures", paste0("Relative_NNN_", i,
                                            "_", j, ".png"))
    ggsave(filename=filename, plot=q , width=8 , height=6 , units="in" , dpi=300)
  }
}


#Bar Charts: Relative including bare ground and thatch ----
unique(veg$Habitat)
vegsums<-veg[veg$PSOC=="Bare Ground"|veg$PSOC=="Sum of Thatch Cover"|
               veg$PSOC=="Sum of Non-Native Cover"|veg$PSOC=="Sum of Native Cover"|
               veg$PSOC=="Sum of Other Cover",]
unique(vegsums$PSOC)
vegsumsagg<-aggregate(Percent_Cover~Cover_Category+Phase+Habitat+Year,vegsums,FUN=mean)

unq_phase_sum <- unique(vegsumsagg$Phase)
unq_hab_sum <- unique(vegsumsagg$Habitat)


for (i in unq_phase_sum) {
  for (j in unq_hab_sum) {
    p <- subset(vegsumsagg, Phase == i & Habitat == j) %>%
      group_by(Year) %>%
      mutate(
        rel_percent_cover = round((Percent_Cover / sum(Percent_Cover)*100), 1)
      ) %>%
      ungroup()
    
    q <- ggplot(p, aes(Year, Percent_Cover, fill=Cover_Category)) +
      geom_bar(position="fill", stat="identity", color="black") +
      geom_bar_text(aes(label = label_percent(accuracy = 0.1, scale = 1)
                        (rel_percent_cover)), 
                    outside = TRUE, place = 'center', 
                    min.size = 6, 
                    position = "fill",
                    fontface='bold',
                    contrast = TRUE) +
      ggtitle("Average Relative Percent Cover", paste(i, j)) +
      theme_bw() + 
      theme(axis.text.x = element_text(angle = 90))+
      theme(axis.text.x = element_text(angle = 0, size = 12),
            axis.text.y = element_text(size=12),
            axis.title.y = element_text(
              size=14, margin = margin(t = 0, r = 10, b = 0, l = 20)),
            axis.title.x=element_text(
              size=14,margin = margin(t = 0, r = 10, b = 0, l = 20)),
            title = element_text(size=15, face='bold'),
            plot.subtitle = element_text(size=12, face='bold')) +
      scale_fill_manual("Cover Type", 
                        values = c("NATIVE COVER" = "darkgreen", 
                                   "NON-NATIVE COVER" = "red", 
                                   "BARE GROUND" = "tan", 
                                   "OTHER COVER" = "burlywood4", 
                                   "THATCH" = "grey"))
    print(q)
    
    filename <- file.path("figures", paste0("Relative_AllCover_", i,
                                            "_", j, ".png"))
    ggsave(filename=filename, plot=q , width=8 , height=6 , units="in" , dpi=300)
  }
}


### for vegetation table

veg$PSOC <- sub(" ", "_", veg$PSOC)
vegNative<-veg[veg$Cover_Category=="NATIVE COVER"&veg$PSOC!="Sum_of Native Cover",]

#NATIVE ONLY
vegNative<- within(vegNative, PSOC[PSOC == "Cardamine_olidosperna"] <- 'Cardamine_oligosperma')
vegNative<- within(vegNative, PSOC[PSOC == "Sparganium_Eurycarpum"] <- 'Sparganium_eurycarpum')
vegNative<- within(vegNative, PSOC[PSOC == "Spergula_marina"] <- 'Spergularia_marina')
vegNative

# check for misspellings
unique_veg <- vegNative[!duplicated(vegNative$PSOC), ]
sorted_unique_customers <- unique_veg[order(unique_veg$PSOC), ]
print(sorted_unique_customers)

vegNativespecies<-aggregate(Percent_Cover~PSOC+Year+Habitat,vegNative,FUN=mean)
vegNativespecies$count<-1
vegNativespecies
vegNativeDiversity<-aggregate(count~Habitat+Year,vegNativespecies,FUN=sum)
vegNativeDiversity

nat25 = vegNativespecies[vegNativespecies$Year=="2025",]
nat25

#ALL VEG
# check for misspellings of sum of native and nonnative
unique_veg <- veg[!duplicated(veg$PSOC), ]
sorted_unique_customers <- unique_veg[order(unique_veg$PSOC), ]
sorted_unique_customers[220:250,]

vegonly <- veg[veg$PSOC=="Sum_of_Native_Cover"|veg$PSOC=="Sum_of_Non-Native_Cover",]
vegonlyall <- aggregate(Percent_Cover~Habitat+Transect_Name+Transect_Distance+Year,vegonly,FUN=sum)
vegonlyallmean <- aggregate(Percent_Cover~Habitat+Year,vegonlyall,FUN=mean)


#2025 Native Species Cover

