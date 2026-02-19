# 📿 chulangnghiem

Tools for chanting and memorizing the **Chu Lăng Nghiêm**.

## ✨ Install

```r
install.packages("remotes")
remotes::install_github("henrydoth/chulangnghiem")
```

## 📖 Usage

```
library(chulangnghiem)

cln(13)              # chant block of line 13
cln("0*")            # block 0 (1–12)
cln("0*:2*")         # blocks 0–2 (1–36)

cln(13, auto=TRUE)   # auto mode

clnk("tát đát")      # search by keyword
```

🙏