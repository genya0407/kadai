# 前提

- ruby-2.3.0
- bundler
- gnuplot

# 実行手順

## 依存しているライブラリをインストールする

```shell-session
    $ bundle install --path vendor/bin
```

## プログラムを実行する

```shell-session
    $ bundle exec ruby main.rb 2> /dev/null
```

`2> /dev/null`とあるのは、ライブラリの警告を消すためである。
