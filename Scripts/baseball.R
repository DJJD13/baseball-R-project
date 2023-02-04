library(tidyverse)
library(readxl)

xlFile <- file.path("C:", "Users", "jdegg", "Documents", "Dev_Stuff", "R_Projects", "Baseball_Stats", "baseball_project", "Data", "Baseball_Season_2022.xlsx", fsep="\\")
excel_sheets(xlFile)

t_bats <- read_excel(xlFile, sheet = "2022_Batting")
t_pitch <- read_excel(xlFile, sheet = "2022_Pitching")
batters <- read_excel(xlFile, sheet = "2022_Batters")
starters <- read_excel(xlFile, sheet = "2022_Starters")
relief <- read_excel(xlFile, sheet = "2022_Relief")

# TEAM STATS

  # BATTING

    # Team WL%: Plots WL% for each team
    wins <- t_bats %>% 
      ggplot(mapping = aes(x = fct_rev(fct_reorder(Team, W)), y = W, fill = Playoffs)) + 
        geom_col() + 
        geom_text(aes(label = Team), vjust = -0.2) + 
        scale_fill_manual(values = c("#f8766d", "#00bfc4")) + 
        geom_abline(slope = 0, intercept = 81, linewidth = .8) + 
        labs(x = "Teams", y = "Team Wins", title = "Team Total Wins - 2022 Season", caption = "Data from baseball-reference.com") +
        theme_bw()
      
    wins
    
    # Team WL-.500: First, we transmute the Batting Sheet to get just Wins and Loses
    win_los <- t_bats %>%
      transmute(
        Team,
        W,
        L,
        Playoffs
      ) %>%
      mutate (
        wl_diff = (W - 81)
      )
    # Then, plot the win differential
    wl_pl <- win_los %>%
      ggplot(mapping = aes(x = fct_rev(fct_reorder(Team, wl_diff)), y = wl_diff, fill = Playoffs)) + 
      geom_col() + 
      geom_text(aes(label = Team), vjust = -0.3) + 
      labs(x = "Team", y = "Wins above/below .500", title = "Wins Above/Below .500 - 2022 Season", caption = "Data from baseball-reference.com")
    
    wl_pl
    
    # Team Run Differential: First, we transmute the Batting Sheet to get runs scored for each team
    rdf1 <- t_bats %>%
      transmute(
        Team,
        R,
        Playoffs
      )
    # Then transmute the pitching sheet to get runs allowed
    rdf2 <- t_pitch %>%
      transmute(
        Team,
        R
      ) %>%
      rename(RA = R)
    # Now join the tables to get the combination
    rdf <- inner_join(rdf1, rdf2, by = "Team") %>%
      mutate(
        run_diff = (R - RA)
      )
    # Finally, plot the run differential
    rdf_pl <- rdf %>%
      ggplot(mapping = aes(x = fct_rev(fct_reorder(Team, run_diff)), y = run_diff, fill = Playoffs)) + 
      geom_col() + 
      geom_text(aes(label = Team), vjust = -0.3) + 
      labs(x = "Team", y = "Run Differential", title = "Run Differential - 2022 Season", caption = "Data from baseball-reference.com")
    
    rdf_pl
    
    # Team Hits: Gets hits and transforms the table into a long version
    hits <- t_bats %>%
      transmute(
        Team,
        H,
        `1B`,
        `2B`,
        `3B`,
        HR
      ) %>%
      pivot_longer(c(`1B`, `2B`, `3B`, HR), names_to = "hit_type", values_to = "count")
      
    
    # Plots the hits, splitting each bar into hit type
    hits_plot <- hits %>%
      ggplot(mapping = aes(x = fct_rev(fct_reorder(Team, H)), y = count, fill = hit_type)) + 
      geom_col() +
      scale_fill_manual(values = c("#fefefe", "#adc8ca", "#669ab4", "#317588")) +
      labs(x = "Teams", y = "Total Hits", fill = "Hit Type", title = "Total Hits - 2022 Season", caption = "Data from baseball-reference.com") + 
      theme_dark()
    
    hits_plot
    
    # Team RBIs: Calculates the total RBIs scored by each team
    rbis <- t_bats %>%
      ggplot(mapping = aes(x = fct_rev(fct_reorder(Team, RBI)), y = RBI)) + 
      geom_col(fill = "#00bfc4") +
      geom_text(aes(label = Team), vjust = -0.3) + 
      geom_abline(slope = 0, intercept = 663) + 
      labs(x = "Teams", y = "RBIs", title = "Total RBIs - 2022 Season", caption = "Data from baseball-reference.com")
    
    rbis
    
    # Team Stolen Bases: Graphs the total Stolen bases by each team
    sbs <- t_bats %>%
      ggplot(mapping = aes(x = fct_rev(fct_reorder(Team, SB)), y = SB)) + 
      geom_col(fill = "#00bfc4") +
      geom_text(aes(label = Team), vjust = -0.3) + 
      labs(x = "Teams", y = "Total Stolen Bases - 2022 Season", caption = "Data from baseball-reference.com")
    
    sbs
    
    # Team Stolen-base %: Graphs the percentage of successful steals
    sbs_per <- t_bats %>% transmute(
      Team,
      SB,
      CS,
      SB_Per = SB / (SB + CS)
      )
    # Pivot the SB table into a long version for the bar chart
    sbs_long <- sbs_per %>%
      pivot_longer(c(SB, CS), names_to = "steal_res", values_to = "count")
    
    sbs_pl <- sbs_long %>%
      ggplot(mapping = aes(x = fct_rev(fct_reorder(Team, count)), y = count, fill = steal_res)) + 
      geom_col(color = "black") +
      scale_fill_manual(values = c("#f8766d", "#00bfc4")) +
      labs(x = "Teams", y = "Stolen Bases", fill = "Caught Stealing", title = "Total SB Attempts - 2022 Season", caption = "Data from baseball-reference.com")
    
    sbs_pl
    
    sbs_per_pl <- sbs_per %>%
      ggplot(mapping = aes(x = fct_rev(fct_reorder(Team, SB_Per)), y = SB_Per)) + 
      geom_col(fill = "#00bfc4") +
      labs(x = "Teams", y = "Stolen Base %", title = "Stolen Base % - 2022 Season", caption = "Data from baseball-reference.com")
    
    sbs_per_pl
    
    # Team Strikeouts: Graphs the total Strikeouts by each team
    sos <- t_bats %>%
      ggplot(mapping = aes(x = fct_rev(fct_reorder(Team, SO)), y = SO)) + 
      geom_col(fill = "#00bfc4") +
      geom_text(aes(label = Team), vjust = -0.3) + 
      labs(x = "Teams", y = "RBIs", title = "Total Strikeouts for Batters - 2022 Season", caption = "Data from baseball-reference.com")
    
    sos
    
    # Team LOB: Graphs batters left-on-base
    lob <- t_bats %>%
      ggplot(mapping = aes(x = fct_rev(fct_reorder(Team, LOB)), y = LOB)) + 
      geom_col(fill = "#00bfc4") +
      geom_text(aes(label = Team), vjust = -0.3) + 
      labs(x = "Teams", y = "Total Batters LOB", title = "Total Runners Left on Base - 2022 Season", caption = "Data from baseball-reference.com")
    
    lob
    
    # Team OPS+: Calculates the OPS+ above/below 100 for each team (Should be 15 above and below)
    ops_plus <- t_bats %>%
      transmute(
        Team,
        `OPS+`,
        OPS_diff = `OPS+` - 100,
        Playoffs
      )
    
    # Then graphs the OPS+ with regards to above/below 100 (league average)
    ops_plus_pl <- ops_plus %>%
      ggplot(mapping = aes(x = fct_rev(fct_reorder(Team, OPS_diff)), y = OPS_diff, fill = Playoffs)) + 
      geom_col() +
      geom_text(aes(label = Team), vjust = -0.3) + 
      labs(x = "Teams", y = "OPS+ to 100", title = "OPS+ Above/Below 100 - 2022 Season")
    
    ops_plus_pl
    
    # HBP Leaders
    hbp_b <- t_bats %>%
      ggplot(mapping = aes(x = fct_rev(fct_reorder(Team, HBP)), y = HBP)) + 
      geom_col() +
      labs(x = "Team", y = "HBP", title = "Total HBPs (Team) - 2022 Season") + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
    hbp_b
    
  # PITCHING
    
    # Team ERA: Get Team ERA
    era <- t_pitch %>%
      ggplot(mapping = aes(x = fct_reorder(Team, ERA), y = ERA, fill = Playoffs)) + 
      geom_col() +
      geom_text(aes(label = Team), vjust = -0.3) + 
      labs(x = "Teams", y = "Team ERA", title = "Team ERA - 2022 Season")
    
    era
    
    # Team ERA vs WL%
    era_wl <- t_pitch %>%
      ggplot(mapping = aes(x = `WL%`, y = ERA, color = Playoffs)) + 
      geom_point() + 
      geom_text(aes(label = Team), vjust = -0.3) + 
      labs(x = "WL%", y = "ERA", title = "ERA vs. Win-Lose % - 2022 Season")
    
    era_wl

# PLAYERS
  
  # BATTING
    
    # WAR Leaders - Batting
    war_b <- batters %>%
      ggplot(mapping = aes(x = fct_rev(fct_reorder(Player, WAR)), y = WAR)) + 
      geom_col() +
      labs(x = "Players", y = "WAR", title = "WAR (Batters) - 2022 Season") + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
    war_b
    
    # Individual OPS+
    ops_plus_i <- batters %>% 
      ggplot(mapping = aes(x = fct_rev(fct_reorder(Player, `OPS+`)), y = `OPS+`)) + 
      geom_col() +
      labs(x = "Teams", y = "OPS+", title = "OPS+ - 2022 Season") + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
    ops_plus_i
    
    # OBP vs At-Bats
    slg_ab <- batters %>%
      ggplot(mapping = aes(x = AB, y = OBP)) + 
      geom_point() +
      labs(x = "At-Bats", y = "OBP", title = "At-Bats vs OBP - 2022 Season")
    
    slg_ab
    
    # Slugging vs At-Bats
    slg_ab <- batters %>%
      ggplot(mapping = aes(x = AB, y = SLG)) + 
      geom_point() +
      labs(x = "At-Bats", y = "SLG", title = "At-Bats vs Slugging - 2022 Season")
    
    slg_ab
    
    # Individual RBIs
    rbis_i <- batters %>%
      ggplot(mapping = aes(x = fct_rev(fct_reorder(Player, RBI)), y = RBI)) + 
      geom_col() +
      labs(x = "Player", y = "RBIs", title = "Total RBIs - 2022 Season") + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
    rbis_i
    
    # Stolen base + attempts
    sbs_per_i <- batters %>% transmute(
      Player,
      SB,
      CS,
      SB_Per = SB / (SB + CS)
    )
    # Pivot the SB table into a long version for the bar chart
    sbs_long_i <- sbs_per_i %>%
      filter(SB + CS >= 10) %>%
      pivot_longer(c(SB, CS), names_to = "steal_res", values_to = "count")
    
    sbs_pl_i <- sbs_long_i %>%
      ggplot(mapping = aes(x = fct_rev(fct_reorder(Player, count)), y = count, fill = steal_res)) + 
      geom_col(color = "black") +
      scale_fill_manual(values = c("#f8766d", "#00bfc4")) +
      labs(x = "Teams", y = "Stolen Bases", fill = "Caught Stealing", title = "Total SB Attempts - 2022 Season") + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
    sbs_pl_i
    
    # HBP Leaders
    hbp_b <- batters %>%
      filter(HBP >= 5) %>%
      ggplot(mapping = aes(x = fct_rev(fct_reorder(Player, HBP)), y = HBP)) + 
      geom_col() +
      labs(x = "Player", y = "HBP", title = "Total HBPs - 2022 Season") + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
    hbp_b
  # PITCHING
    
    # WAR Leaders - Pitching (Starters)
    war_p <- starters %>%
      ggplot(mapping = aes(x = fct_rev(fct_reorder(Player, WAR)), y = WAR)) + 
      geom_col() +
      labs(x = "Players", y = "WAR", title = "WAR (Pitchers) - 2022 Season") + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
    war_p
    
    # Individual ERA
    era_i <- starters %>%
      ggplot(mapping = aes(x = fct_rev(fct_reorder(Player, ERA)), y = ERA)) + 
      geom_col() +
      labs(x = "Teams", y = "ERA", title = "ERA - 2022 Season") + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
    era_i
    
    # Individual Strikeouts
    so_i <- starters %>%
      ggplot(mapping = aes(x = fct_rev(fct_reorder(Player, SO)), y = SO)) + 
      geom_col() +
      labs(x = "Teams", y = "SO", title = "Total Strikeouts - 2022 Season") + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
    so_i
    
    # Innings pitched vs ERA
    ip_era <- starters %>%
      ggplot(mapping = aes(x = IP, y = ERA)) + 
      geom_point() + 
      labs(x = "Innings Pitched", y = "ERA", title = "Innings Pitched vs ERA - 2022 Season")
    
    ip_era
    
    # Innings pitched vs SO/BB
    ip_sobb <- starters %>%
      ggplot(mapping = aes(x = IP, y = `SO/BB`)) + 
      geom_point() + 
      labs(x = "Innings Pitched", y = "SO/BB", title = "Innings Pitched vs SO/BB - 2022 Season")
    
    ip_sobb
    
    # WHIP vs ERA
    whip_era <- starters %>%
      ggplot(mapping = aes(x = ERA, y = WHIP)) + 
      geom_point() + 
      scale_x_reverse() + 
      labs(x = "ERA", y = "WHIP", title = "ERA vs WHIP - 2022 Season")
    
    whip_era
    
    