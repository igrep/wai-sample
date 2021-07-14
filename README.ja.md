# wai-sample

作成中の、新しい[WAI](https://www.stackage.org/package/wai)ベースのウェブアプリケーションフレームワーク（名前未定）と、そのサンプルアプリを含んだリポジトリーです。

## なぜ新しいフレームワークを？

### 「[Servant](https://docs.servant.dev/en/stable/)でやっていることの大半は、型レベルプログラミングに頼らなくてもできるのでは？」という仮説から

Haskellの世界では現在[Servant](https://docs.servant.dev/en/stable/)と[Yesod](https://www.yesodweb.com/)の二つのウェブアプリケーションフレームワークが有名ですが、この二つはいずれも型レベルプログラミングか独自のDSLという、ちょっと敷居の高い技術の学習を要求します。とりあえず使うだけであればサンプルコードに従って雰囲気で書けば十分なのですが、仕組みが分かりづらく、トラブルシューティングや凝った応用がしにくいでしょう。そこで、型レベルプログラミングに頼らず、**できるだけ単純なHaskell**によって、ユーザーが理解しやすく、なおかつServantに負けないくらい高度な機能を持ったウェブアプリケーションフレームワークを作ることを目指します。

### [makeMistakesToLearnHaskell](https://github.com/haskell-jp/makeMistakesToLearnHaskell)の配信用サーバーとして、誰でもハックできるフレームワークにしたいから

[makeMistakesToLearnHaskell](https://github.com/haskell-jp/makeMistakesToLearnHaskell)は、私がのんびり開発・執筆しているHaskell学習用アプリケーションです<small>（詳細は以前書いた「[Haskell社内勉強会とHaskell学習ツールの紹介](https://eng-blog.iij.ad.jp/archives/3467)」という記事をご覧ください）</small>。makeMistakesToLearnHaskellは、現在はコマンドラインアプリケーションとしてインストールして実行するものですが、インストールするためにはGHCとかをインストールしないといけないので、そこでハマるひとが多数いました。

- ➡️インストール不要なものを作ろう
    - ➡️ ユーザーが入力したコードをサーバーで実行して判定するサーバー
- ➡️ フレームワークから作ろう
    - ➡️ Haskell学習したて人でも修正できるOSSにしたいという理想により、簡単に理解できるフレームワークから作ることに
        - ➡️ 型レベルプログラミングなど、GHCの高度な機能なしで使えるフレームワーク


## 2021年7月現在までの開発

### 2020年10月～2020年12月

これより前は、makeMistakesToLearnHaskellの開発を配信しながら学習する、という進め方をしていましたが、ネタ切れ  
➡️サーバーをフレームワークから作ろう！

💡makeMistakesToLearnHaskell自身も「習いたてのHaskellerでも開発できる」ことを目指しているので、勉強会で配信するのにも向いていた

[プログラミングHaskell](https://www.lambdanote.com/products/haskell)の第13章を参考にパスをパースして、マッチしたパスに登録したアクションを起動

<https://github.com/igrep/wai-sample/blob/3408f55f140dca1078348c0b1fa1b8608a4b36b9/src/WaiSample.hs#L28>

### 2020年12月～2021年1月

ルーティングテーブルを構成する型を defunctionalize して、複数の解釈ができるように。

<https://github.com/igrep/wai-sample/blob/e527a98c28d2d63d335541f8f3b5016a27780c8f/src/WaiSample.hs#L35>

- ⚠️当時は defunctionalization と呼んでいたけど、合っているかな？
- ➡️ルーターだけでなく、ルーティングテーブルの生成もできるようになった！

### 2021年2月～2021年6月

クライアントの自動生成にもチャレンジ

<https://github.com/igrep/wai-sample/blob/c567086a7079225052e59f2ee15782c557047b07/src/WaiSample.hs#L57>

- handlerの名前を指定するようになったのがポイント

<https://github.com/igrep/wai-sample/blob/c567086a7079225052e59f2ee15782c557047b07/src/WaiSample/Client.hs#L30>

<https://github.com/igrep/wai-sample/blob/c567086a7079225052e59f2ee15782c557047b07/app/WaiSample/Client/Sample.hs>

- 😓なるべく使いたくなかったけど、さすがにTemplate Haskellを使わなければダメだった
- その他、大本の設計にもミスが見つかったので修正

### 2021年6月～

- (WIP) パスのパースを、`pathInfo`関数が返した`[Text]`に対して行うのではなく、`[Text]`を`/`でくっつけた`Text`に対してパースすることで、ユーザーがパースをより柔軟にできるように
    - 🤔Servantなどではどうやって実現してるんだろう

<https://github.com/igrep/wai-sample/commit/b8b8c002ba9e23146020f49cad04f3d9074a2e17>
