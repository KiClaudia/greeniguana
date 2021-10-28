# Graphs of progression over the weeks after first LPS challenge for phys

#----------bka------------
pdf('GLProgression1LPSBKA.pdf',
    width = 11, height = 7)
boxplot(GL$X0423bka, GL$X0428bka, GL$X0430bka, GL$X0504bka, GL$X0511bka, GL$X0525bka,
        main = "Progression of BKA after first LPS challenge for Glucose-LPS group",
        ylab = "Bacterial Killing",
        xlab = "Time Progression",
        names = c("Pre-LPS", "24hr Post-LPS", "72hr", "1 week", "2 week", "4week"))
dev.off()

pdf('GCProgression1LPSBKA.pdf',
    width = 11, height = 7)
boxplot(GC$X0423bka, GC$X0428bka, GC$X0430bka, GC$X0504bka, GC$X0511bka, GC$X0525bka,
        main = "Progression of BKA after first LPS challenge for Glucose-Control group",
        ylab = "Bacterial Killing",
        xlab = "Time Progression",
        names = c("Pre-LPS", "24hr Post-LPS", "72hr", "1 week", "2 week", "4week"))
dev.off()

pdf('WCProgression1LPSBKA.pdf',
    width = 11, height = 7)
boxplot(WC$X0423bka, WC$X0428bka, WC$X0430bka, WC$X0504bka, WC$X0511bka, WC$X0525bka,
        main = "Progression of BKA after first LPS challenge for Water-Control group",
        ylab = "Bacterial Killing",
        xlab = "Time Progression",
        names = c("Pre-LPS", "24hr Post-LPS", "72hr", "1 week", "2 week", "4week"))
dev.off()

pdf('WLProgression1LPSBKA.pdf',
    width = 11, height = 7)
boxplot(WL$X0423bka, WL$X0428bka, WL$X0430bka, WL$X0504bka, WL$X0511bka, WL$X0525bka,
        main = "Progression of BKA after first LPS challenge for Water-LPS group",
        ylab = "Bacterial Killing",
        xlab = "Time Progression",
        names = c("Pre-LPS", "24hr Post-LPS", "72hr", "1 week", "2 week", "4week"))
dev.off()
