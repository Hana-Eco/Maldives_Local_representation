# =============================================================================== ~
#           ~ Mapping Maldivian peer-reviewed literature: 1985-2025 ~
# =============================================================================== ~
#
# ~> Load packages ####
  library(tidyverse)
  library(janitor)
  library(patchwork)
  # only load if mapping data
  library(sf)
  library(scatterpie)
  library(ggspatial)
#
# ===============================================================================
#                         #### ~ Load quantification data ~ ####
# =============================================================================== ~
#
# ~ Note: 534 articles of 662 candidate papers were assessed 
# ~ Note: Some of the papers accessed beyond the time frame of the analysis and need to be removed
# ~ Note: 534 articles to 524 articles -> 524/652 = 80.4%
#

#### ===> Load raw data <=== ####
para <- read.csv("Country-Analysis_v1_534_Anotate_clean_07.08.2026.csv",stringsAsFactors = TRUE)%>%
  clean_names()%>%
  # filter to time frame: 1985 to 2025
  filter(year<2026 & year>1984)

#### ===> Spatial data for the country <=== ####
  ### ==> atoll mid points
  atmid<-read.csv("Atoll.csv",stringsAsFactors = TRUE)%>%
    clean_names()
  ### ==> Reef
  reef<-st_read("mv_shp/Reef.shp")
  ### ==> Lagoon 
  lagoon<-st_read("mv_shp/Lagoon.shp")
  ### ==> Island
  island<-st_read("mv_shp/Island.shp")
  

#
# =============================================================================== 
#                         #### ~ Overall study counts ~ ####
# =============================================================================== ~
#
#
#### ===> Overall 1. All studies over the years <=== ####
ov_total<-para%>%
    count(year)%>%
    rename(total = n)
  # check count
  sum(ov_total$total) #524
    
#### ===> Overall 2. Studies affliated with the Maldives <=== ####
ov_mvaff<-para%>%
    filter(maldives == "YES")%>%
    count(year)%>%
    rename(affiliation = n)
  # check count
  sum(ov_mvaff$affiliation) #201
  
#### ===> Overall 3. Studies with Maldivian affiliations but not local authors <=== ####
ov_mvaff.no<-para%>%
    filter(maldives == "YES" & mdv_auth == "NO")%>%
    count(year)%>%
    rename(no_local = n)
  # check count
  sum(ov_mvaff.no$no_local) #101 
  
#### ===> Overall 4. Studies with local authors <=== ####
ov_auth<-para%>%
    filter(mdv_auth == "YES")%>%
    count(year)%>%
    rename(with_local = n)
  # check count
  sum(ov_auth$with_local) #119
  
#### ===> Overall 5. Studies with local authors but no Maldives affiliation <=== ####
ov_auth.noaff<-para%>%
    filter(maldives == "NO" & mdv_auth == "YES")%>%
    count(year)%>%
    rename(nomvaff_local=n)
  # check count
  sum(ov_auth.noaff$nomvaff_local) #20
  # identify which countries
  ov_auth.noaff_ct<-para%>%
    filter(maldives == "NO" & mdv_auth == "YES")%>%
    select(no, citation, authors, australia:qatar)%>%
    pivot_longer(cols = australia:qatar,
                 names_to = "country",
                 values_to = "value")%>%
    filter(value=="YES")%>%
    count(country)
    
  
#### ===> Join data frames to total data frame and calculation proportions <=== ####
ov_total.all<-ov_total%>%
    # join data frames
    left_join(ov_mvaff, by="year")%>%
    left_join(ov_mvaff.no, by="year")%>%
    left_join(ov_auth, by="year")%>%
    left_join(ov_auth.noaff, by="year")%>%
    # replace with NA with zero
    mutate(across(everything(), ~ replace_na(., 0)))%>%
    # calculate proportions
    mutate(pct_aff=affiliation/total*100,
           pct_no_local=no_local/total*100,
           pct_with_local=with_local/total*100,
           pct_nomvaff_local=nomvaff_local/total*100)
  
  
#
# =============================================================================== 
#                        #### ~ Authorship details ~ ####
# =============================================================================== ~
#
#  
#### ===> Authorship 1: Local primary authorship over time <=== ####
auth_pa<-para%>%
    filter(local_pa == "YES")%>%
    count(year)
  
#### ===> Authorship 2: Type of local authored studies <=== ####
auth_pa.brd<-para%>%
    filter(local_pa == "YES")%>%
    count(year, broad)%>%
    group_by(broad)%>%
    summarise(sum=sum(n))
  
#### ===> Authorship 3: authorship, broad category, h index and quartile <=== ####
auth_pa_h<-para%>%
    filter(local_pa == "YES")%>%
    select(year, broad, secondary, h_index, journal_quartile_ii, journal_country)%>%
    mutate(h_index=replace_na(as.numeric(h_index), 0))
  
#### ===> Authorship 4: CRediT specified roles <=== #####
  ### check how many papers have proper crediting
  auth_crd_ct<-para%>%
    filter(mdv_auth == "YES" & mdv_credit == "YES") # 53 observations -> 10.1%
  
  ### check roles
  auth_crd<-para%>%
      filter(mdv_auth == "YES" & mdv_credit == "YES")%>%
      select(no,id, year, conceptualisation:w_re)%>%
      pivot_longer(cols = conceptualisation:w_re, 
                   names_to = "type",
                   values_to = "value")%>%
      group_by(value)%>%
      count(year, type, value) 
#
# =============================================================================== 
#                        #### ~ Acknowledgement counts ~ ####
# =============================================================================== ~
#
#  
#### ===> Ack 1: Studies with acknowledgements <=== ####
ack_total<-para%>%
    filter(mdv_ack == "YES")%>%
    count(year)
  # check count
  sum(ack_total$n) #194
  
#### ===> Ack 2: Proportion of acknowledgement over time <=== ####
ack_prop<-ov_total%>%
    left_join(ack_total, by = "year")%>%
    mutate(
      # replace the NA in acknowledgement with 0
      n = replace_na(n, 0),
      # calculate proportion of studies with acknowledgements
      prop = n/total*100
    )
  
#### ===> Ack 3: Types of acknowledgements <=== ####
ack_type<-para%>%
    filter(mdv_ack == "YES")%>%
    select(no, id, year, logistics:misc)%>%
    pivot_longer(cols = logistics:misc,
                 names_to = "type",
                 values_to = "value")%>%
    group_by(value)%>%
    count(year, type, value)%>%
    mutate(type = factor(as.factor(type), 
                         levels = c("logistics","data_collection","stakeholding","permitting","misc","data_provision","expertise","funds"), 
                         labels = c("Logistics","Data collection","Stakeholding","Permitting","Miscellaneous","Data provision","Expertise","Funds")))%>%as.data.frame()

  
#
# =============================================================================== 
#                       #### ~ Country affiliations ~ ####
# =============================================================================== ~
#
# 
#### ===> Country 1: total count authors from each affiliated country <=== ####  
country<-para%>%
    mutate(
      # Maldives affiliations but no local authors
      MDV.yes_no = if_else(maldives == "YES" & mdv_auth == "NO", "YES", "NO"),
      # No Maldivian affilication but with local authors
      MDV.no_yes = if_else(maldives == "NO" & mdv_auth == "YES", "YES", "NO"),
      # Maldives affiliation with local authors
      MDV.yes_yes = if_else(maldives == "YES" & mdv_auth == "YES", "YES", "NO"),
      # No maldivian affiliation and no local local authors
      MDV.no_no   = if_else(maldives == "NO" & mdv_auth == "NO", "YES", "NO"))%>%
    select(no, id, year, MDV.yes_no,MDV.yes_yes,australia:qatar)%>%
    pivot_longer(cols = MDV.yes_no:qatar,
                 names_to = "country",
                 values_to = "value")%>%
    count(year, country,value) 
  
    ## check count
    countr_count<-country%>%
      filter(value == "YES")%>%
      group_by(country)%>%
      summarise(sum = sum(n),.groups = "drop") %>%
      arrange(desc(sum))
    
#### ===> Country 2: Affiliation vs broad categories <=== ####
country_br<-para%>%
  mutate(
    # Maldives affiliations but no local authors
    MDV.yes_no = if_else(maldives == "YES" & mdv_auth == "NO", "YES", "NO"),
    # No Maldivian affilication but with local authors
    MDV.no_yes = if_else(maldives == "NO" & mdv_auth == "YES", "YES", "NO"),
    # Maldives affiliation with local authors
    MDV.yes_yes = if_else(maldives == "YES" & mdv_auth == "YES", "YES", "NO"),
    # No maldivian affiliation and no local local authors
    MDV.no_no   = if_else(maldives == "NO" & mdv_auth == "NO", "YES", "NO"))%>%
  select(no, id, year,broad, MDV.yes_no,MDV.yes_yes,australia:qatar)%>%
  pivot_longer(cols = MDV.yes_no:qatar,
               names_to = "country",
               values_to = "value")%>%
  count(country, broad, value)%>%
      drop_na()
    
#### ===> Country 3: Which countries without local but Maldives? <=== ####
country_cor<-para%>%
      filter(maldives == "YES" & mdv_auth == "NO")%>%
      select(australia:qatar)%>%
      pivot_longer(cols = australia:qatar, 
                   names_to = "country",
                   values_to = "value")%>%
      filter(value == "YES")%>%
      count(country)
country_cor
#
# =============================================================================== 
#                       #### ~ Locality of study ~ ####
# =============================================================================== ~
#
# 
#### ===> Locality 1: Studies with local authors across the atolls <=== ####
locality_lcl<-para%>%
      filter(mdv_auth == "YES")%>%
      select(no, id, year, ha:s)%>%
      pivot_longer(cols = ha:s, 
                   names_to = "atoll",
                   values_to = "presence")%>%
      filter(presence=="YES")%>%
      group_by(atoll)%>%
      count(presence)%>%
      rename(local=n)
    
#### ===> Locality 2: Studies without local authors across the atolls <=== ####
locality_nolcl<-para%>%
      filter(mdv_auth != "YES")%>%
      select(no, id, year, ha:s)%>%
      pivot_longer(cols = ha:s, 
                   names_to = "atoll",
                   values_to = "presence")%>%
      filter(presence=="YES")%>%
      group_by(atoll)%>%
      count(presence)%>%
      rename(foreign=n)
    
#### ===> Locality 3: Proportion of locals to non local authors across the atolls <=== ####
locality_prop<-locality_nolcl%>%
      left_join(locality_lcl, by="atoll")%>%
      mutate(
        # replace the NA's in local counts with zero for calculations
        local = replace_na(local, 0),
        # calculate the total studies per atoll
        total = local + foreign,
        # calculate percentage for each author type
        pct_l = local/total*100,
        pct_f = foreign/total*100
      )%>%
      # join with atoll mid point coordinates
      left_join(atmid, by="atoll")%>%as.data.frame()%>%
      # set atolls in N-S order
      mutate(atoll = factor(as.factor(atoll), levels = c("ha","h_dh","sh","n","r","b","lh",                                                         "nm","sm","aa","a_dh","v","m","dh",
                                                         "f","th","l","ga","g_dh","gn","s"),
                            labels = c("HA","HDh","Sh","N","R","B","Lh",                                                                           "NM","SM","AA","ADh","V","M","Dh",
                                       "F","Th","L","GA","GDh","Gn","S")))
    
#
# =============================================================================== 
#                     #### ~ Accessibility & transparency ~ ####
# =============================================================================== ~
#
#
#### ===> Access 1: Is the paper open access? <=== ####
access_oap<-para%>%
      count(oap)%>%
      group_by(oap)%>%
      mutate(pct = n/524*100)%>%as.data.frame()
    
#### ===> Access 1: Has paper accessibility changed over the years? <=== ####
access_oap.yr<-para%>%
      count(year,oap)%>%
      group_by(year, oap)%>%
      mutate(pct = n/524*100)%>%as.data.frame()
      
#### ===> Access 3: Is the data openly available? <=== ####
access_data<-para%>%
      count(data_availability)%>%
      group_by(data_availability)%>%
      mutate(pct = n/524*100)%>%as.data.frame()
      
#### ===> Access 4: Does the paper provide explicit permitting details <=== ####
access_perm<-para%>%
      count(permited)%>%
      group_by(permited)%>%
      mutate(pct = n/524*100)%>%as.data.frame()
      
#
# =============================================================================== 
#                        #### ~ Study categorization ~ ####
# =============================================================================== ~
#
#
#### ===> Broad 1: broad categories of studies over the years <=== ####
brd_total<-para%>%
  count(year, broad)
  
#### ===> Broad 2: broad categories of studies with locals present <=== ####
brd_total.lcl<-para%>%
  filter(mdv_auth == "YES")%>%
  count(year, broad)
  
#### ===> Broad 3: proportion between locals to non-local <=== ####
  ### ==> Join data sets
  brd_prop<-brd_total%>% 
      rename(total=n)%>%
      left_join(brd_total.lcl)%>%
      rename(local=n)%>%
      mutate(local = replace_na(local, 0), 
             foreign = total-local,
             pct_l = local/total*100,
             pct_f = foreign/total*100)
    
  ### ===> Calculate differences and proportions
  brd_prop_sum<-brd_prop%>%
    group_by(broad)%>%
    summarise(total=sum(total),
              local=sum(local),
              foreign=sum(foreign))%>%
    mutate(pct_l=local/total*100,
           pct_f=foreign/total*100)
   
#### ===> Secondary 1: secondary categories of studies over the years <=== #### 
sec_total<-para%>%
  count(year, secondary)

#### ===> Secondary 2: secondary categories of studies with locals present <=== ####
sec_total.lcl<-para%>%
  filter(mdv_auth == "YES")%>%
  count(year, secondary)

#### ===> Secondary 3: proportion between locals to non-local <=== ####
  ### ==> Join data sets
  sec_prop<-sec_total%>% 
    rename(total=n)%>%
    left_join(sec_total.lcl)%>%
    rename(local=n)%>%
    mutate(local = replace_na(local, 0), 
           foreign = total-local,
           pct_l = local/total*100,
           pct_f = foreign/total*100)
  
  ### ===> Calculate differences and proportions
  sec_prop_sum<-sec_prop%>%
    group_by(secondary)%>%
    summarise(total=sum(total),
              local=sum(local),
              foreign=sum(foreign))%>%
    mutate(pct_l=local/total*100,
           pct_f=foreign/total*100)
  
#
# =============================================================================== 
#                           #### ~ Detailed counts ~ ####
# =============================================================================== ~
#
#### ===> Detail 1: how involved are Maldivians in discovery studies <=== ####
detail_spp_disc<- para%>%
  select(no, id, year, secondary, taxa, mdv_auth)%>%
  filter(secondary == "Discovery")%>%
  count(year, taxa, mdv_auth)
  # check total number of discovery studies
  para%>%
    filter(secondary == "Discovery")%>%
    count() # 68 studies -> 10 with locals (14.7%)
  
#
# =============================================================================== 
#                                 #### ~ Plots ~ ####
# =============================================================================== ~
#
#
#### ===> plot 1: Total study counts over time 1985-2025 <=== ####
plot.overall<-ggplot(data=ov_total.all)+
    # total studies over time
      geom_line(aes(x=year, y=total), linewidth=0.6, color="grey20")+
      geom_point(aes(x=year, y=total, color=NA), size=2, shape=21, fill="grey20", show.legend = FALSE)+
    # studies with authors affiliated with the Maldives
      geom_line(aes(x=year, y=affiliation), linewidth=0.6, color="#e69f00")+
      geom_point(aes(x=year, y=affiliation, color=NA),size=1.5, shape=24, fill="#e69f00", show.legend = FALSE)+
    # studies with local authors
      geom_line(aes(x=year, y=with_local), linewidth=0.6, color="#009E73")+
      geom_point(aes(x=year, y=with_local, color=NA),size=1.5, shape=22, fill="#009E73", show.legend = FALSE)+
    labs(x="Year", y="Study count")+
    theme_classic(base_size = 10)
plot.overall

#### ===> plot 2: Proportion of Maldives affiliated studies and studies with locals <=== ####
plot.ov_prop<-ggplot(data = ov_total.all)+
  # proportion of studies affiliated with the Maldives
  geom_line(aes(x=year, y=pct_aff), linewidth=0.4, color="#e69f00")+
  geom_point(aes(x=year, y=pct_aff, color=NA),size=2.5, shape=24, fill="#e69f00", show.legend = FALSE)+
  # proportion of studies with locals 
  geom_line(aes(x=year, y=pct_with_local), linewidth=0.8, color="#009E73")+
  geom_point(aes(x=year, y=pct_with_local, color=NA),size=2.5, shape=22, fill="#009E73", show.legend = FALSE)+
  labs(x="Year", y="Percentage of total studies (%)")+
  theme_classic(base_size = 10)
plot.ov_prop

#### ===> plot 3 total and proportion of acknowledgements over time <=== ####
  ### ==> calculate scaling factor for second axis
  sf.ack<-max(ack_total$n, na.rm = TRUE)/100

  ### ==> plot
  plot.ack<-ggplot(data=ack_prop)+
    # proportion of studies with acknowledgements
    geom_line(aes(x=year, y=prop*sf.ack), linewidth=0.8, color="grey30")+
    geom_point(aes(x=year, y=prop*sf.ack, color=NA), shape=22, size=1.5, fill="grey30", show.legend = FALSE)+
    # total acknowledgements over time
    geom_line(aes(x=year, y=n), linewidth=0.6, color="#009E73")+
    geom_point(aes(x=year, y=n, color=NA), size=2, shape=21, fill="#009E73", show.legend = FALSE)+
    # adjust axes
    scale_y_continuous(name = "Study count with acknowledgments",
                       sec.axis = sec_axis(~ . / sf.ack, "Proportion (%)"))+
    xlab("Year")+
    theme_classic(base_size = 10)
  plot.ack
  
#### ===> plot 4: types of acknowledgements given over time <=== ####
plot.ack_type<-ggplot(data = ack_type%>%filter(value == "YES"))+
  geom_line(aes(x=year, y=n, group=type), col ="#009E73",linewidth=1)+
  geom_point(aes(x=year, y=n), fill="#009E73",color="#009E73",shape=21, size=1.5)+
  facet_wrap(~type, ncol=4)+
  labs(x="Year", y="Study count")+
  theme_classic(base_size = 10)
plot.ack_type

  ### ==> save plot
  ggsave(last_plot(), 
         filename = "pub_outputs/S8.png",
         dpi = 600, width =183 , height = 164, units = "mm", scale = 1)
  
#### ===> plot 5: types of acknowledgements given summarised <=== ####
plot.ack_type2<-ggplot(data = ack_type%>%filter(value == "YES")%>%group_by(type)%>%summarise(sum=sum(n)))+
  geom_col(aes(x=reorder(type,sum), y=sum), fill="grey30")+
  geom_text(aes(x=reorder(type,sum), y=sum, label = sum), hjust = 1.2)+
  scale_y_reverse(expand = expansion(mult = c(0.15, 0)))+
  scale_x_discrete(position = "top")+
  coord_flip()+
  labs(x="Type of acknowledgment",y="Study count")+
  theme_classic(base_size = 10)
plot.ack_type2
  
#### ===> plot 6: countries affiliated with Maldivian studies <=== ####
  ### ==> adjust the dataframe to help with plotting data
  country_bar<-country %>%
    filter(value == "YES") %>%
    group_by(country) %>%
    summarise(sum = sum(n), .groups = "drop") %>%
    mutate(country_group = ifelse(sum < 20, "Other", country))%>%
    group_by(country_group)%>%
    summarise(sum = sum(sum), .groups = "drop")%>%
    mutate(fill_group = case_when(country_group == "MDV.yes_yes" ~ "Maldives (with locals)",
                                  country_group == "MDV.yes_no" ~ "Maldives (without locals)",
                                  TRUE ~ "Other"))

  ### ==> Plot affiliations
  plot.country<-ggplot(data = country_bar, 
                       aes(x=reorder(country_group, sum), y = sum))+
    geom_col(aes(fill=fill_group), show.legend = FALSE)+
    geom_text(aes(label = sum), hjust = 1.2)+
    #annotate("text", x = 14.1, y = 160, hjust = 0,label = "(47 countries)", col="white")+ 
    coord_flip()+
    scale_fill_manual(values = c("#009E73","#0072B2","grey30"))+
    scale_y_reverse(expand = expansion(mult = c (0.15,0)))+
    scale_x_discrete(position = "top",
                     labels = c(
      "MDV.yes_yes" = "Maldives", # (with locals)
      "MDV.yes_no" = "Maldives", #  (without locals)
      "italy" = "Italy",
      "uk" = "United Kingdom",
      "usa" = "United States",
      "australia" = "Australia",
      "germany" = "Germany",
      "new_zealand" = "New Zealand",
      "france" = "France",
      "japan" = "Japan",
      "netherlands" = "Netherlands",
      "india" = "India",
      "spain" = "Spain"
    ))+
    labs(x="Affiliation", y="Study count")+
    theme_classic(base_size = 11)
  plot.country
  
#### ===> plot 7: distribution of studies over atolls <=== ####
plot.atoll<-ggplot(data = locality_prop)+
    geom_col(aes(x=fct_rev(atoll), y=total), fill="grey30")+
    geom_text(aes(x=fct_rev(atoll), y=total, label=total), hjust=-0.3)+
    coord_flip()+
    labs(x="Atoll", y="Study count")+
    theme_classic(base_size = 10)
plot.atoll

  ### ==> save plot
  #ggsave(last_plot(), 
  #       filename = "pub_outputs/S1.png",
  #       dpi = 600, width = 89, height = 82, units = "mm", scale = 1)

#### ===> plot 8: proportion locals:foreign in studies in each atoll <=== ####
  ### ==> Convert to long format for easier plotting
  locality_prop.l<-locality_prop%>%
    select(atoll, total, pct_f, pct_l)%>%
    pivot_longer(cols = pct_f:pct_l,
                 names_to = "Rep",
                 values_to = "Percent")%>%
    mutate(Rep = factor(as.factor(Rep), levels = c("pct_l","pct_f"), 
                        labels = c("Local","Foreign")))

  ### ==> Plot
  plot.atoll_dist<-ggplot(data=locality_prop.l)+
    # Split of author
    geom_col(aes(x=fct_rev(atoll), y=Percent, fill=Rep))+
    # mark 50% mark
    geom_hline(yintercept = 50, linetype="dashed", linewidth=0.8, alpha =0.5)+
    # Add labels for total study counts for each atoll
    geom_text(aes(x=fct_rev(atoll), y=100,label = total), hjust = -0.1, color="grey70")+
    # Add labels for split for each authorship type
    geom_text(aes(x=fct_rev(atoll), y=Percent, label = ifelse(Percent > 1, round(Percent, 1), "")),
              position = position_stack(vjust = 0.5), size = 3.5, colour = "white")+
    coord_flip()+
    scale_fill_manual(values = c("#009E73","#0072B2"))+
    labs(y="Represenation (%)", x="Atoll", fill="Authors")+
    theme_classic(base_size = 10)+
    theme(legend.position = "none")
  plot.atoll_dist

#### ===> plot 9: local primary authors over time <=== ####
plot.prim_year<-ggplot(data=auth_pa,
                       aes(x=year, y=n))+
    geom_line(linewidth=0.6, color="#009E73")+
    geom_point(aes(color=NA),shape=21, size=1.5, fill="#009E73", show.legend=FALSE)+
    labs(x="Year", y="Local primary authored studies")+
    theme_classic(base_size = 10)
plot.prim_year

#### ===> plot 10: local primary authors summarised <=== ####
plot.prim_sum<-ggplot(data=auth_pa.brd, 
                      aes(x=reorder(broad, sum), y=sum))+
  geom_col(fill="grey30")+
  geom_text(aes(label = sum), hjust = -0.2)+
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))+
  labs(x="Broad field", y="Local primary authored studies")+
  coord_flip()+
  theme_classic(base_size = 10)
plot.prim_sum
  ### ==> save plot
  ggsave(last_plot(), 
         filename = "pub_outputs/S2.png",
         dpi = 600, width = 89, height = 82, units = "mm", scale = 1)

  
#### ===> plot 11: authorship roles by locals <=== ####
  ### ==> Summarise to groups
  auth_crd.sum<-auth_crd%>%
  filter(value=="YES")%>%
  group_by(type)%>%
  summarise(sum=sum(n))%>%as.data.frame()

  ### ==> plot 
  plot.auth_type<-ggplot(data = auth_crd.sum, 
                         aes(x = reorder(type, sum), y = sum))+
    geom_col(show.legend = FALSE, fill="grey30")+
    geom_text(aes(label = sum), hjust = 1.1)+
    coord_flip()+
    scale_y_reverse(expand = expansion(mult = c(0.15, 0))) + 
    scale_x_discrete(position = "top",
      labels = c(
      "w_re" = "Review and edits",
      "investigation" = "Investigation",
      "conceptualisation" = "Conceptualisation",
      "data_curation" = "Data curation",
      "w_od" = "Orginal draft",
      "methodologoy" = "Methodology",
      "project_administration" = "Project admin",
      "validation" = "Validation",
      "resources" = "Resources",
      "formal_analysis" = "Formal analysis",
      "funding" = "Funding",
      "supervision" = "Supervision",
      "visualisation" = "Visualisation",
      "software" = "Software"
    ))+
    labs(x="CRediT authorship role",y="Authors credited")+
  theme_classic(base_size = 10)
  plot.auth_type
  
#### ===> plot 12: accessibility to papers <=== ####
  ### ==> bar chat version
  plot.oap1<-ggplot(data=access_oap)+
      geom_col(aes(x=oap, y=pct, fill=oap), show.legend = FALSE)+
      geom_text(aes(x=oap, y=pct,
                    label = ifelse(pct > 0.5, round(pct, 1),"")), hjust = -0.1)+
      coord_flip()+
      labs(x="Open access paper?", y="Proportion of studies (%)")+
      theme_classic(base_size = 10)
  plot.oap1

  ### ==> Pie chart version
  plot.oap2<-ggplot(data=access_oap%>%mutate(oap=factor(oap, levels = c("YES","NO"), labels = c("Open access","Paywalled"))),
                    aes(x="",y=pct))+
    geom_bar(aes(fill=oap),stat="identity", width=1, color="white")+
    coord_polar("y", start=0) +
    scale_fill_manual(values = c("#56B4E9","#D55E00"))+
    geom_text(aes(label = oap), position = position_stack(vjust = 0.5), color="white")+
    geom_text(aes(label = round(pct,1)), position = position_stack(vjust = 0.4), color="white")+
    #labs(title = "Paper accessibility")+
    theme_void()+
    #xlim(0.5, 2.5) + 
    theme(legend.position = "none")
  plot.oap2
  
#### ===> plot 13: accessibility to data <=== ####
  ### ==> Bar plot
  plot.data<-ggplot(data=access_data)+
    geom_col(aes(x=reorder(data_availability, -pct), y=pct), fill="grey30")+
    geom_text(aes(x=reorder(data_availability, -pct), y=pct,
                  label = ifelse(pct > 0.5, round(pct, 1),"")), hjust = -0.3)+
    scale_x_discrete(labels = c(
      "No information provided" = "Not stated",
      "Yes - open source" = "Open source",
      "Yes - conditional" = "Conditional",
      "No - not available" = "Not available",
      "Yes - stated but inaccesible" = "Inaccesible"
    ))+
    coord_flip()+
    labs(x="Data availability", y="Proportion of studies (%)")+
    theme_classic(base_size = 8)
  plot.data
  
  ### ==> save plot
  #ggsave(last_plot(), 
  #       filename = "pub_outputs/S3.png",
  #       dpi = 600, width = 89, height = 82, units = "mm", scale = 2)
  
  
  ### ==> pie chart
  plot.data2<-ggplot(data = access_data%>%mutate(data_availability=factor(data_availability,
                      levels = c("No information provided","Yes - open source","Yes - conditional","No - not available","Yes - stated but inaccesible"),
                      labels = c("Not stated","Open source","Conditional","Not available","Inaccesible"))),
                     aes(x="",y=pct))+#x=2
    geom_bar(aes(fill=data_availability),stat="identity", width=1, color="white")+
    #geom_text(aes(label = round(pct,1)), color="white")+
    scale_fill_manual(values=c("#D55E00","#009E73","#56B4E9","#E69F00","#F0E442"))+
    coord_polar("y", start=0)+
    #xlim(0.5, 2.5) + 
    #labs(title="access")+
    theme_void()+
    theme(legend.position = "none")
  plot.data2
  
#### ===> plot 14: transparency of study permission <=== ####
  ### ==> bar plot
  plot.permission<-ggplot(data=access_perm)+
    geom_col(aes(x=reorder(permited, pct), y=pct), fill="grey30")+
    geom_text(aes(x=reorder(permited, pct), y=pct,
                  label = ifelse(pct > 0.5, round(pct, 1),"")), hjust = 1.2)+
    scale_y_reverse(expand = expansion(mult = c(0.15, 0)))+
    scale_x_discrete(position = "top",
                     labels = c(
      "No information provided" = "No information",
      "Unspecified permit" = "Unspecified",
      "Marine research permit" = "Marine research"
    ))+
    coord_flip()+
    labs(x="Research permission", y="Proportion of studies (%)")+
    theme_classic(base_size = 10)
  plot.permission
  
  ### ==> pie chart
  plot.permission2<-ggplot(data = access_perm %>%
                           mutate(permited = factor(permited, 
                           levels = c("Government collab", "Marine research permit", "No information provided", "Unspecified permit"),
                           labels = c("Government collab", "Marine research", "No information", "Unspecified"))),
                           aes(x="", y=pct))+
    geom_bar(aes(fill=permited),stat="identity", width=1, color="white")+
    coord_polar("y", start=0)+
    scale_fill_manual(values=c("#D55E00","#009E73","#E69F00","#56B4E9"))+
    #xlim(0.5, 2.5) + 
    theme_void()+
    #labs(title = "permission")+
    theme(legend.position = "none")
  plot.permission2

#### ===> plot 15: changes to broad study categories over time <=== ####
plot.broad<-ggplot(data = brd_prop)+
  # total from each broad group over time
  geom_line(aes(x=year, y=total), col ="grey30",linewidth=1)+
  geom_point(aes(x=year, y=total), fill="grey30",shape=21, size=2.5, alpha = 0.5)+
  # local from each broad group over time
  geom_line(aes(x=year, y=local), col ="#009E73",linewidth=1)+
  geom_point(aes(x=year, y=local), fill="#009E73",shape=22, size=2.5, alpha = 0.5)+
  facet_wrap(.~broad, ncol=4)+
  labs(x="Year", y="Study count")+
  theme_classic(base_size = 10)
plot.broad

  ### ==> save plot
  #ggsave(last_plot(), 
  #       filename = "pub_outputs/S4.png",
  #       dpi = 600, width =183 , height = 164, units = "mm", scale = 1)


#### ===> plot 16: changes to broad study categories summarise <=== ####
plot.broad_sum<-ggplot(data=brd_prop_sum%>%
                     pivot_longer(cols = pct_l:pct_f,
                                  names_to = "rep",
                                  values_to = "percent")%>%
                     mutate(rep=factor(as.factor(rep), levels = c("pct_f","pct_l"), 
                                                   labels = c("Foreign","Local"))))+
  geom_col(aes(x=broad, y=percent, fill=rep))+
  geom_text(aes(x=broad, y=percent, label = ifelse(percent>1, round(percent, 1),"")),
            position = position_stack(vjust = 0.5), size = 3.5, colour = "white")+
  geom_text(aes(x=broad, y=100,label = total), hjust = -0.1, color="grey70")+
  scale_fill_manual(values = c("#0072B2","#009E73"))+
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))+
  labs(x="Broad field", y="Percent (%)", fill="Authors")+
  scale_x_discrete(labels=c(
    "Mesophotic and deep sea" = "Mesophitc/deep",
    "Marine megafauna" = "Megafauna",
    "Coastal ecosystems" = "Coastal"
  ))+
  coord_flip()+
  theme_classic(base_size = 10)+
  theme(legend.position = "none")
plot.broad_sum

#### ===> plot 17: changes to secondary categories over time <=== ####
plot.sec<-ggplot(data = sec_prop)+
  # total from each broad group over time
  geom_line(aes(x=year, y=total), col ="grey30",linewidth=1)+
  geom_point(aes(x=year, y=total), fill="grey30",shape=21, size=2.5, alpha = 0.5)+
  # local from each broad group over time
  geom_line(aes(x=year, y=local), col ="#009E73",linewidth=1)+
  geom_point(aes(x=year, y=local), fill="#009E73",shape=22, size=2.5, alpha = 0.5)+
  facet_wrap(.~secondary)+
  labs(x="Year", y="Study count")+
  theme_classic(base_size = 10)
plot.sec

  ### ==> save plot
  #ggsave(last_plot(), 
  #       filename = "pub_outputs/S5.png",
  #       dpi = 600, width =183 , height = 247, units = "mm", scale = 1)

#### ===> plot 18: changes to secondary study categories summarise <=== ####
plot.sec_sum<-ggplot(data=sec_prop_sum%>%
                         pivot_longer(cols = pct_l:pct_f,
                                      names_to = "rep",
                                      values_to = "percent")%>%
                         mutate(rep=factor(as.factor(rep), levels = c("pct_f","pct_l"), 
                                           labels = c("Foreign","Local"))))+
  geom_col(aes(x=secondary, y=percent, fill=rep))+
  geom_text(aes(x=secondary, y=percent, label = ifelse(percent>1, round(percent, 1),"")),
            position = position_stack(vjust = 0.5), size = 3.5, colour = "white")+
  geom_text(aes(x=secondary, y=100,label = total), hjust = -0.1, color="grey70")+
  scale_fill_manual(values = c("#0072B2","#009E73"))+
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))+
  scale_x_discrete(labels = c(
    "Rehab and Restoration" = "R & R",
    "Pharmaceuticals and biotech" = "Biopharma",
    "Engineering and technology" = "E & T",
    "CCAR" = "Climate change"
  ))+
  labs(x="Secondary field", y="Percent (%)", fill="Authors")+
  coord_flip()+
  theme_classic(base_size = 10)+
  theme(legend.position = "none")
plot.sec_sum

#### ===> plot 19: discovery studies summarised <=== ####
  ### ==> summarise the studies by taxa
  detail_spp_disc.sum<-detail_spp_disc%>%
    group_by(taxa, mdv_auth)%>%
    summarise(sum=sum(n))%>% as.data.frame()%>%
    mutate(mdv_auth=factor(mdv_auth, levels = c("YES","NO"), labels = c("Local", "Foreign")))
  
  ### ===> plot
  plot.sppdisc<-ggplot(data = detail_spp_disc.sum)+
    geom_col(aes(x=reorder(taxa, sum), y=sum, fill=mdv_auth))+
    labs(y = "Study count", 
         x = "Discovery group",
         fill = "Authors")+
    geom_text(aes(x=reorder(taxa, sum), y=sum, label = sum, colour = mdv_auth),
              position = position_stack(vjust = 0.5), size = 3.5, show.legend = FALSE)+
    scale_fill_manual(values = c("#009E73","#0072B2"))+
    scale_color_manual(values = c("white", "white"))+
    coord_flip()+
    theme_classic(base_size = 10)+
    theme(legend.position = "bottom")
  plot.sppdisc
  
#### ===> plot 20: relationship between broad and secondary categories <=== ####
  ### ==> confirm relationship 
  para %>% 
    count(secondary, broad) %>% 
    count(secondary) %>% 
    filter(n > 1) # crossed relationship - not hierarchical ->heatmap or bubble
  
  ### ==> plot figure
  plot.bs<-para%>%
    count(broad, secondary, .drop = FALSE)%>%
    mutate(broad = fct_reorder(broad, n, sum),
           secondary = fct_reorder(secondary, n, sum))%>%
    filter(n > 0)%>%
    ggplot(aes(x = broad, y = secondary, size = n)) +
    geom_point(colour = "#56B4E9", alpha = 0.8) +
    scale_size_area(max_size = 8, name = "Studies") +
    scale_x_discrete(labels=c(
      "Mesophotic and deep sea" = "Mesophitc/deep sea",
      "Marine megafauna" = "Megafauna",
      "Coastal ecosystems" = "Coastal"
    ))+
    scale_y_discrete(limits = rev,
                     labels = c(
                       "Rehab and Restoration" = "R & R",
                       "Pharmaceuticals and biotech" = "Biopharma",
                       "Engineering and technology" = "E & T",
                       "CCAR" = "Climate change"
                     )) +
    labs(x = "Broad field", y = "Secondary field") +
    theme_classic(base_size = 10)+
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.minor = element_blank())
  plot.bs
  
  ### ==> save plot
  ggsave(last_plot(), 
         filename = "pub_outputs/S6.png",
         dpi = 600, width =183 , height = 183, units = "mm", scale = 1)
  
#### ===> plot 21: local authorship vs publication h-index <=== ####
plot.auth_h<-ggplot(data=auth_pa_h)+
    geom_point(aes(x=secondary, y=h_index, fill=broad), size=4, shape=21)+
    scale_x_discrete(limits = rev,
                     labels = c(
                       "Rehab and Restoration" = "R & R",
                       "Pharmaceuticals and biotech" = "Biopharma",
                       "Engineering and technology" = "E & T",
                       "CCAR" = "Climate change"
                     )) +
    scale_fill_discrete(labels=c(
      "Mesophotic and deep sea" = "Mesophitc/deep sea",
      "Marine megafauna" = "Megafauna",
      "Coastal ecosystems" = "Coastal"))+
    labs(fill = "Broad field", x = "Secondary field", y="H Index") +
    coord_flip()+
    theme_classic(base_rect_size = 10)
plot.auth_h

  ### ==> save plot
  ggsave(last_plot(), 
         filename = "pub_outputs/S7.png",
         dpi = 600, width =183 , height = 183, units = "mm", scale = 1)
  
#### ===> Plot 22: country affiliation vs broad categories <=== ####
  ### ==> adjust the data frame to help with plotting data
  country_bar.br<-country_br %>%
    filter(value == "YES") %>%
    group_by(country, broad) %>%
    summarise(sum = sum(n), .groups = "drop") %>%
    #mutate(country_group = ifelse(sum < 8, "Other", country))%>%
    mutate(country_group = case_when(
      country %in% c("MDV.yes_yes","MDV.yes_no") ~ country,
      sum < 8 ~ "Other",
      TRUE ~ country
    ))%>%
    group_by(country_group, broad)%>%
    summarise(sum = sum(sum), .groups = "drop")%>%
    mutate(fill_group = case_when(country_group == "MDV.yes_yes" ~ "Maldives (with locals)",
                                  country_group == "MDV.yes_no" ~ "Maldives (without locals)",
                                  TRUE ~ "Other"))
  
    ### ==> Plot affiliations
    plot.country_br<-ggplot(data = country_bar.br, 
                         aes(x=reorder(country_group, sum), y = sum))+
      geom_col(aes(fill=fill_group), show.legend = FALSE)+
      geom_text(aes(label = sum), hjust = 1.2)+
      #annotate("text", x = 14.1, y = 160, hjust = 0,label = "(47 countries)", col="white")+ 
      coord_flip()+
      scale_fill_manual(values = c("#009E73","#0072B2","grey30"))+
      scale_y_reverse(expand = expansion(mult = c (0.15,0)))+
      scale_x_discrete(position = "top",
                       labels = c(
                         "MDV.yes_yes" = "MDV/locals", # (with locals)
                         "MDV.yes_no" = "MDV/no locals", #  (without locals)
                         "italy" = "Italy",
                         "uk" = "United Kingdom",
                         "usa" = "United States",
                         "australia" = "Australia",
                         "germany" = "Germany",
                         "new_zealand" = "New Zealand",
                         "russia" = "Russia",
                         "switzerland" = "Switzerland",
                         "france" = "France",
                         "japan" = "Japan",
                         "austria" = "Austria",
                         "netherlands" = "Netherlands",
                         "saudi_arabia" = "Saudi Arabia"
                       ))+
      facet_wrap(.~broad, ncol=4)+
      labs(x="Affiliation", y="Affiliate count")+
      theme_classic(base_size = 11)
    plot.country_br
  
#### ===> plot 23: Maldives affiliation, locals with countries <=== ####
plot.country_cor<-ggplot(data = country_cor,
                         aes(x=reorder(country,n), y=n))+
      geom_col(fill="grey30")+
      geom_text(aes(label = n), hjust = 1.2)+
      scale_y_reverse(expand = expansion(mult = c (0.15,0)))+
      scale_x_discrete(position = "top",
                       labels = c(
                         "italy" = "Italy",
                         "uk" = "United Kingdom",
                         "usa" = "United States",
                         "australia" = "Australia",
                         "saudi_arabia" = "Saudi Arabia",
                         "france" = "France",
                         "germany" = "Germany",
                         "uae" = "United Arab Emirates",
                         "switzerland" = "Switzerland",
                         "portugal" = "Portugal",
                         "new_zealand" = "New Zealand",
                         "japan" = "Japan",
                         "india" = "India",
                         "austria" = "Austria",
                         "taiwan" = "Taiwan",
                         "sri_lanka" = "Sri Lanka",
                         "spain" = "Spain",
                         "south_africa" = "South Africa",
                         "seychelles" = "Seychelles",
                         "russia" = "Russia",
                         "qatar" = "Qatar",
                         "phillipines"="Phillipines",
                         "oman" = "Oman",
                         "netherlands" = "Netherlands",
                         "kenya" = "Kenya",
                         "honduras" = "Honduras",
                         "chile" = "Chile",
                         "bahamas" = "Bahamas"
                       ))+
      coord_flip()+
      labs(y="Affiliated authors", x="Countries with Maldivian affiliates without local authors")+
      theme_classic(base_size = 11)
plot.country_cor
  ### ==> save plot
  ggsave(last_plot(), 
         filename = "pub_outputs/S9.png",
         dpi = 600, width =183 , height = 183, units = "mm", scale = 1)
#
# =============================================================================== 
#                                 #### ~ Plot stitching ~ ####
# =============================================================================== ~
#
# ~ Note: Saving at Nature's column widths (89 mm single, 183 mm double, 247 mm max height)
#
#### ===> Main: Figure 1 <=== ####
  ### ==> plot layout
  layout.f1<-"
              AB
              CD
              EF
              "
  
  ### ==> Stitch plots
  fig1<-(
          plot.overall+plot.prim_year+
          plot.ack+plot.country+
          plot.oap2+plot.permission12
        )+
    plot_layout(design = layout.f1)+
    plot_annotation(tag_levels = "a")
  fig1
  
  ### ==> save plot
  #ggsave(fig1, 
  #       filename = "pub_outputs/Main_Figure_1.pdf",
  #       dpi = 600, width = 183, height = 247, units = "mm", scale = 1)
  

#### ===> Main: Figure 2 <=== #### 
  ### ==> plot layout
  layout.f2<-"
             AA
             BC
             "
  
  ### ==> stitch plots
  fig2<-(     plot.country_br+
         plot.auth_type+plot.ack_type2
        )+
    plot_layout(design = layout.f2)+
    plot_annotation(tag_levels = "a")
  fig2
  
  ### ==> save plot
  ggsave(fig2, 
         filename = "pub_outputs/Main_Figure_2.png",
         dpi = 600, width = 183, height = 247, units = "mm", scale = 1)
  
  
#### ===> Main: Figure 3 <=== #### 
  ### ==> plot layout
  layout.f3<-"
             AB
             CD
             "
  
  ### ==> stitch plots
  fig3<-(plot.broad_sum+plot.atoll_dist+
           plot.sec_sum+plot.sppdisc
        )+
    plot_layout(design = layout.f3)+
    plot_annotation(tag_levels = "a")
  fig3
  
  ### ==> save plot
  #ggsave(fig3, 
  #       filename = "pub_outputs/Main_Figure_3.pdf",
  #       dpi = 600, width = 183, height = 247, units = "mm", scale = 1)
  

  
