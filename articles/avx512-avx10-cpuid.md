---
title: "AVX-512とAVX10とCPUID雑感"
emoji: "🗂"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: [avx512, avx10]
published: true
---

## AVX-512雑感

AVX-512はIntelが2013年に発表した命令セットアーキテクチャーで、以下のような特徴を持ちます：

* 512ビット幅のZMMレジスター
* x86-64ではSIMDレジスターが32本に倍増（AVX2までは16本だった）
* 条件分岐っぽいことをするのに便利なマスク
* 埋め込み丸めと浮動小数点例外の抑制
* 命令に埋め込めるブロードキャスト

AVX-512は命令セット拡張の総称で、個々の拡張は以下になります：

* F (Foundation)
* CD (Conflict Detection)
* ER (Exponential and Reciprocal / Xeon Phi only)
* PF (Prefetch / Xeon Phi only)
* VL (Vector Length)
* DQ (Doubleword and Quadword)
* BW (Byte and Word)
* IFMA
* VBMI
* VBMI2
* VNNI
* BITALG
* VPOPCNTDQ
* 4VNNIW (Xeon Phi only)
* 4FMAPS (Xeon Phi only)
* VP2INTERSECT
* FP16
* BF16

多いですね。

このセクションではAVX-512に対する素人的な雑感を書いていきます。

### AVX-512は512ビット幅が基本

AVX-512は名前の通り512ビットが基本です。

AVX-512を構成する拡張では、512ビット（ZMM）のサポートが基本であり、128ビット（XMM）/256ビット（YMM）のサポートはオプションです。具体的には、CPUがVL拡張を実装していれば128ビット版・256ビット版の命令（例えば、XMM16-31やYMM16-31の使用）も使えますが、一般には128ビット版・256ビット版は仮定できないということです。

例えば、AVX-512 FP16に含まれる半精度加算命令 `vaddsh`/`vaddph` のバリエーションを見ると、

* `vaddph xmm, xmm, xmm`: AVX-512 FP16だけで使える
* `vaddph xmm, xmm, xmm`: AVX-512 FP16に加えてAVX-512 VLが必要
* `vaddph ymm, ymm, ymm`: AVX-512 FP16に加えてAVX-512 VLが必要
* `vaddph zmm, zmm, zmm`: AVX-512 FP16だけで使える

となります。

x86のSIMDレジスターの幅がSSEの128ビット→AVXの256ビット→AVX-512の512ビットと拡張されてきた経緯を考えると、AVX-512で追加された命令は常に128ビット、256ビットでも使えて欲しいものですが、AVX-512的にはそれはVLという「拡張」扱いなのです。

とはいえ、VL拡張を実装していないCPUはすでにディスコンとなったXeon Phiだけのようなので、今日のAVX-512をターゲットとするアプリケーションでは常にVL拡張が使えると仮定して良いでしょう（コンパイル時に `-mavx512f -mavx512vl` という風にして、VL拡張が使えないAVX-512対応CPUは考慮しない）。

「512ビットが基本」という方針の弊害として、何らかの事情で512ビット幅のレジスターと演算器を用意できないCPUではAVX-512が一切使えないということになります。例えばAVX-512 FP16で追加された半精度の命令が便利そうだと思っても、512ビット幅を提供できないCPUではそれは絶対に実装できないのです。

### 黒歴史となった拡張

AVX-512の一部の拡張は黒歴史となっています。

例えば、ER, PF, 4VNNIW, 4FMAPSはXeon Phiにしか搭載されませんでした。

そして、VP2INTERSECTは「他の命令でエミュレーションした方が速い」という論文が出てしまいました：[\[2112.06342\] Faster-Than-Native Alternatives for x86 VP2INTERSECT Instructions](https://arxiv.org/abs/2112.06342)

それが理由かは知りませんが、VP2INTERSECTはIntel的には「Tiger Lakeのみ実装」となっており、SDMでは「削除された機能」扱いとなっています。AMDはZen 5で実装したようですが。

### 私の感想

AVX-512は一部の人からボロクソ言われています：

[Linus Torvalds: "I Hope AVX512 Dies A Painful Death" - Phoronix](https://www.phoronix.com/news/Linus-Torvalds-On-AVX-512)

が、浮動小数点数オタクの私としては、嫌いになりきれない面があります。

AVX-512からはIEEE 754-2008を意識している側面がところどころに感じられます。それは静的な丸め指定だったり、`vrange` 系の命令でIEEE 754-2008 minNum/maxNum演算ができたりすることだったりします。

なので、浮動小数点数オタクとしては、AVX-512が動くマシンを一台手元に置いておきたいです。そういうわけで私はIce Lake搭載のIntel MacBookを買ってみたり、AMD Zen 4搭載のミニPCを買ったりしています。

まあこれは趣味として触っている人の感想なので、真面目に高性能計算をやったりする人は別の感想を持つでしょう。

## AVX10

AVX-512の欠点の一つは、「512ビットが基本」なことでした。なので、512ビットのサポートをオプションにしたものがAVX10となります。あと乱立するファミリーを統合して「バージョン」で扱えるようにします。

[Intel® Advanced Vector Extensions 10.1 (Intel® AVX10.1) Architecture Specification](https://www.intel.com/content/www/us/en/content-details/828964/intel-advanced-vector-extensions-10-1-intel-avx10-1-architecture-specification.html)

AVX10.1は、これまでに登場したAVX-512ファミリーから「黒歴史」つまりER, FP, 4VNNIW, 4FMAPS, VP2INTERSECTを除いたものと大体等価です。もちろん、512ビット（ZMM）のサポートはオプションとなります。

【2025年10月10日 追記】2025年春頃の計画変更でAVX10も512ビットを必須とすることとなりました。【追記終わり】

## AVX-512とAVX10とCPUID

これまでの（廃止されたものを除く）AVX-512で導入された命令のCPUID欄は、「AVX-512またはAVX10.1を要求する」という風になります。例えば、半精度加算命令 `vaddsh`/`vaddph` のCPUID欄は

* `vaddsh xmm, xmm, xmm`: AVX-512 FP16 OR AVX10.1/256
* `vaddph xmm, xmm, xmm`: (AVX-512 FP16 AND AVX-512 VL) OR AVX10.1/256
* `vaddph ymm, ymm, ymm`: (AVX-512 FP16 AND AVX-512 VL) OR AVX10.1/256
* `vaddph zmm, zmm, zmm`: AVX-512 FP16 OR AVX10.1/512

となります。

CPUIDの条件に「または」が登場するのはこれまではあまりなかったと思います。例えば、コンパイラーの `-m` オプションで使える命令セットを複数指定するときは、プログラムがCPUに対して要求する命令セットは「かつ」（CPUに対する条件が厳しくなる）で組み合わされました。普通に `-mavx512fp16 -mavx10.1` と指定してしまうと、「AVX-512 FP16」と「AVX10.1」の両方を要求するプログラムができてしまいます。

AVX10を前提にするプログラムをビルドする際はそれでもいいのですが、AVX-512のみに対応するCPUは今の時点ではそれなりに存在するので、移行期には「AVX-512またはAVX10に対応するCPUで動くようなプログラム」をビルドできる必要があります。コンパイラーにはどのように指示したら良いでしょうか？

GCCやClangでは、AVX-512のコンパイルオプションに加えて `-mno-evex512` を指定すると、「AVX-512の機能を使うが、幅は256ビットに制限する」という風になるようです。否定を使う必要があるのがややイレギュラー感があります。

* [x86 Options (Using the GNU Compiler Collection (GCC))](https://gcc.gnu.org/onlinedocs/gcc-14.2.0/gcc/x86-Options.html)
* [Clang Compiler User’s Manual — Clang 20.0.0git documentation](https://clang.llvm.org/docs/UsersManual.html#x86)

では、何らかの事情で「肯定的なフラグ」「ANDによる組み合わせ」しか使えない場合はどうなるでしょうか？

そういう場合は、仮想的なCPUIDフラグを導入することが考えられます。例えば、

```
AVX512_FP16_SCALAR := AVX512_FP16 OR AVX10.1/256
AVX512_FP16_VL256 := (AVX512_FP16 AND AVX512VL) OR AVX10.1/256
AVX512_FP16_VL512 := AVX512_FP16 OR AVX10.1/512
```

と定義すれば、

* `vaddsh xmm, xmm, xmm`: AVX512_FP16_SCALAR
* `vaddph xmm, xmm, xmm`: AVX512_FP16_VL256
* `vaddph ymm, ymm, ymm`: AVX512_FP16_VL256
* `vaddph zmm, zmm, zmm`: AVX512_FP16_VL512

となります。

このように導入できる仮想的なCPUIDフラグは以下のようになります：

```
AVX512_BF16_VL256
AVX512_BF16_VL512
AVX512_BITALG_VL256
AVX512_BITALG_VL512
AVX512_FP16_SCALAR
AVX512_FP16_VL256
AVX512_FP16_VL512
AVX512_IFMA_VL256
AVX512_IFMA_VL512
AVX512_VBMI_VL256
AVX512_VBMI_VL512
AVX512_VBMI2_VL256
AVX512_VBMI2_VL512
AVX512_VNNI_VL256
AVX512_VNNI_VL512
AVX512_VPOPCNTDQ_VL256
AVX512_VPOPCNTDQ_VL512
AVX512BW_MASK
AVX512BW_VL256
AVX512BW_VL512
AVX512CD_VL256
AVX512CD_VL512
AVX512DQ_SCALAR
AVX512DQ_VL256
AVX512DQ_VL512
AVX512F_SCALAR
AVX512F_VL256
AVX512F_VL512
```

---

AVX10（特に、新命令が追加される10.2）が楽しみですね。SDEを触りながら待ちましょう。
