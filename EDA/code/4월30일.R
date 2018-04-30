rgb(10, 4, 23, maxColorValue = 255, alpha = 10) # 끝에.. 투명도를 지정할 수 있음.
col2rgb('lightblue')


# hcl(h, c, l, alpha)
# h: colors, c: saturation, l: brightness, alpha: opacity
# h∈[0,360]: 0~120(red), 120~240(green), 240~360(blue)
# l ∈[0,100], alpha ∈[0,1]
# range of c depends on the value of h and l.
hcl(h = 0, c = 35, l = 85, alpha = 0.1) 

hsv(0.3, 0.5, 0.1, alpha = 0.4)
# hsv(h, s, v, alpha)
# h: color, s: satulation, v: brightness, alpha: opacity
# h, s, v, alpha ∈[0,1]



a <- matrix(1:4, 4,1)
mypal <- heat.colors(4, alpha = 1)
image(a, col =mypal)
a <- matrix(1:100, 100,1)
mypal <- heat.colors(100, alpha = 1)
image(a, col =mypal)
a <- matrix(runif(300),300,1)
mypal <- heat.colors(300,alpha = 1)
image(a, col = mypal)


x <- 10*(1:nrow(volcano))
y <- 10*(1:ncol(volcano))
str(volcano) #행이 87 열이 61
min(volcano) #94..즉 이게 빨간색(히트컬러안의 제일 작은값에 대응되는 색깔이 빨강임)
max(volcano) #195에 해당하는 색이 흰색.
image(x, y, volcano, col = heat.colors(20, alpha = 1), axes = FALSE) #heat.colors(20,~~)는 20개 색상~
contour(x, y, volcano, levels = seq(90, 200, by = 5), # 등고선을 90~200까지 5의 간격으로
        add = TRUE, col = 'blue')


x <- 10*(1:nrow(volcano))
y <- 10*(1:ncol(volcano))
image(x, y, volcano, col = topo.colors(20, alpha = 1), axes = FALSE)
contour(x, y, volcano, levels = seq(90, 200, by = 5),
        add = TRUE, col = 'white')


rainbow(5, s = 0.4, v = 0.3, start = 0, end = 0.05, alpha = 1)
library(RColorBrewer)
brewer.pal(4,'Blues')
x <- 10*(1:nrow(volcano))
y <- 10*(1:ncol(volcano))
image(x, y, volcano, col = brewer.pal(9, 'Blues'), axes = FALSE)
install.packages('colorspace')
library(colorspace)
diverge_hcl(7, h = c(246, 40), c = 96, l = c(65, 90))
pal = choose_palette() #오 이렇게하면, GUI가 떠서 팔레트 만들 수 있음.
mypal = pal(30)
pal(n)
args(pal)
