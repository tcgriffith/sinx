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
