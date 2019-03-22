# sinx: R fortunes in Chinese

### What is the project about?

One of the funniest things I found when I (as a Windows user) leant Ubuntu  was that there is a command `fortune`, which prints a random/pseudorandom message from a database of quotations. It is said that this old feature has been available since 1970s. It was a pity that this feature was unavailable in boring Windows OS, until the R community developed in 2012 a package called 'fortunes,'  which displays funny messages taken from the talks or communications in the R community. It supports external database as well. Unfortunately, it does not support Chinese texts, probably because (1) the 'fortunes' package cannot read multibyte characters such as Chinese, and (2) no one contributes Chinese database of fortunes.

It is a great loss, as fortunes actually come from Chinese restaurants.

![](https://img.taste.com.au/LJmHu6_E/w643-h428-cfill-q90/taste/2016/11/fortune-cookies-16447-1.jpeg)

R has been developing very rapidly in the Chinese community in recent years. There are online Chinese forums such as the [Capital of Statistics](https://d.cosx.org), annually organized R conference in China, and numerous books on R. Many interesting 'fortunes' are hidden there. What if bring them into a 'fortunes'-like package? Or bring some more fun into it?

It is a pity that the 'fortunes' package is read-only. Therefore, I develop a brand new package, called 'sinx'. It displays fortunes in any language, only if a database is available.

The name originated from the name cosx, which is the domain of the Capital of Statistics. `sin(x)` is the derivative of `-cos(x)`, or I would like to explain 'sinx' as 'Sino Xmen Said'.

I designed a logo for 'sinx', which can be printed on a T-shirt.

![](https://github.com/pzhaonet/sinx/raw/master/img/logo-sinx1.png)

## What are the current features?

Briefly speaking, the current features of 'rosr' are as follows.

- Like the 'fortunes' package, the main feature of 'sinx' prints randomly a Chinese message taken from the R community by default. See `?sinx()`.
- External data bases can be imported into sinx. Users can customize their own fortunes in their favorite languages. See `?cscx()`.
- One advantage of a 'sinx' database is that it can be organized either in markdown format besides the table format. Users can easily contribute good fortunes to 'sinx' via github. See the [source database](https://github.com/pzhaonet/sinx/blob/master/inst/sinxs/sinxs.md).
- Another funny thing is 'cowsay'. The 'sinx' package can print messages in a cowsay way, which means that a colored pet can be displayed behind the sinx message. See `?tanx()`.
- A useful feature is that users can easily convert a database into an E-book ([demo](https://www.pzhao.org/book/cosx/)) or slides ([demo](https://www.pzhao.org/slides/cosx/)). See `?secx()`.
- Users can easily choose whether to display a sinx message at the startup of R. See `?ctanx()`
- I have prepared six databases including famous poems in ancient China and famous sayings worldwide besides the default one.

Here is an example, which prints randomly a message in the cowsay mode:

```
libs <- read.sinxs(lib = c("tangshi", "songshi", "chinese", "yangsheng", "english","jinyong"))
tanx(sinxs.data = libs)
#> 
#> 
#>  ----- 
#> 风吹柳花满店香，吴姬压酒唤客尝。
#> 金陵子弟来相送，欲行不行各尽觞。
#> 请君试问东流水，别意与之谁短长。
#> 
#> --- 李白 (金陵酒肆留别) 
#>  ------ 
#>     \   
#>      \
#>         (.)_(.)
#>      _ (   _   ) _
#>     / \/`-----'\/ \
#>   __\ ( (     ) ) /__
#>   )   /\ \._./ /\   (
#>    )_/ /|\   /|\ \_(  [nosig]
```

More details can be found on the online [documentation.](https://www.pzhao.org/pkg/sinx/reference/index.html) 

### Roadmap

In the future, more features will be brought into 'rosr' as follows. 

1. Create more funny database.
2. Develop an RStudio addin for users to easily contribute new fortunes.
3. Create a new feature for quizzes. Let users fill blanks in the message.

# sinx: R 语言中文社区火花集锦

## 简介

sinx 包是个语录集，默认素材主要来自[统计之都](http://d.cosx.org)，附送唐诗、宋诗、金庸语录、中文名言、英文名言、养生语录……未来也不排除采集其他来源的语录。欢迎大家贡献精彩语录。直接 PR 到[项目主页 ](https://github.com/pzhaonet/sinx/blob/master/inst/sinxs) 即可。

相关链接：[缘起](https://d.cosx.org/d/7673/156)，[讨论](https://d.cosx.org/d/420467)。

### 安装

```
if(!require('remotes')) install.packages('remotes')
remotes::install_github('pzhaonet/sinx', build = TRUE, 
                        build_opts = c("--no-resave-data", "--no-manual"))
```

### 使用

主要函数：

```
sinx() # 打印一条语录
tanx()  # 小猫小狗说语录
secx() # 把语录做成一本电子书
ctanx() #  往 ~/.Rprofile 里添加一条代码，在 R 启动时随机显示一条语录
read.sinxs() # 读取语录文件
vignette('sinx') # 语录合集
```
详见各函数的帮助信息，或[在线文档](https://www.pzhao.org/pkg/sinx/)。。

例如，我经常需要用下面这一条语录来回答论坛上的提问：

```
sinx::sinx(1)

## 
## 这个问题我很感兴趣，然而遗憾的是，这个问题我无法重现。
## 
## 能否提供一个可以重现问题的数据和代码示例，让我直接拷贝粘贴就能运行？最好能提供 `session_info()` 信息。
## 
## 提问题的方式详见[新手须知 1.2 正确的发帖姿势](https://d.cosx.org/d/1553-1553)。
##    -- dapengde (耐心温馨欢欣地回答一个不规范的提问)
##       d.cosx.org (2018 年 12 月)
```

支持字符串搜索，也支持正则表达式：

```
sinx('不知道')

## 
## 那我就不知道为什么了……
## 
## 
##    -- yihui (<https://d.cosx.org/d/420428/4>)
##       d.cosx.org (2019-01)
```


值得一提的是，语录文件是可以随意扩展的，只需用 `read.sinxs()` 一次读入多个文件即可。下面这个例子，将所有自带语录合并进来，随机显示：

```
libs <- read.sinxs(lib = c("tangshi", "songshi", "chinese", "yangsheng", "english","jinyong"))
tanx(sinxs.data = libs)
```

# To do

- quiz

# License

Copyright [Peng Zhao](http://pzhao.org).

Released under the GPL-3 license.


![](https://github.com/pzhaonet/sinx/raw/master/img/logo-sinx1.png)
