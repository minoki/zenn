---
title: "x86-64機械語入門"
emoji: "🧮"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: ["x86", "asm", "assembly", "機械語"]
published: true
---

この記事はx86-64の機械語を書けるようになるためのガイドとなることを目指します。読者はアセンブリー言語について既にある程度知っていることを想定します。

## 情報源

x86-64の機械語のオフィシャルなガイドはIntelのSoftware Developer ManualまたはAMDのAMD64 Architecture Programmer's Manualです。

* Intel SDM: [Intel® 64 and IA-32 Architectures Software Developer Manuals](https://www.intel.com/content/www/us/en/developer/articles/technical/intel-sdm.html)
* [AMD64 Architecture Programmer's Manual, Volumes 1-5](https://www.amd.com/content/dam/amd/en/documents/processor-tech-docs/programmer-references/40332.pdf)

このほか、Cから呼び出される関数を定義したり、Cの関数を呼び出すためには、呼び出し規約の知識も必要です。使用される呼び出し規約はOSに依存し、Unix系では主にSystem V ABIが、WindowsではWindowsのABIが使用されます。また、プログラミング言語によっては独自の呼び出し規約を採用している場合があります。

* Unixの呼び出し規約：[x86 psABIs / x86-64 psABI · GitLab](https://gitlab.com/x86-psABIs/x86-64-ABI)
* Windowsの呼び出し規約：[x64 ABI conventions | Microsoft Learn](https://learn.microsoft.com/en-us/cpp/build/x64-software-conventions?view=msvc-170)

機械語を触る際は、アセンブラーと逆アセンブラーを気軽に使えると便利です。ググるとWebで触れるアセンブラー等が色々出てくるので、適当に使いましょう。

## レジスター

この記事では汎用レジスターのみを扱います。

x86-64は64ビット幅の汎用レジスターを16本持ち、順番にrax, rcx, rdx, rbx, rsp, rbp, rsi, rdi, r8, r9, r10, r11, r12, r13, r14, r15と呼ばれます。これらはビット列で書くと0000から1111に対応します。最初の4つはA, B, C, DではなくA, C, D, Bの順番なので注意してください。

汎用レジスターの下位32ビットには、32ビット幅としての名前もついており、それぞれeax, ecx, edx, ebx, esp, ebp, esi, edi, r8d, r9d, r10d, r11d, r12d, r13d, r14d, r15dと呼ばれます。

同様に、汎用レジスターの下位16ビット、下位8ビットを表す名前もあります。まとめると、以下の表のようになります。

| 番号 | 64ビット幅 | 32ビット幅 | 16ビット幅 | 8ビット幅 |
|-|-|-|-|-|
| 0 | rax | eax | ax | al |
| 1 | rcx | ecx | cx | cl |
| 2 | rdx | edx | dx | dl |
| 3 | rbx | ebx | bx | bl |
| 4 | rsp | esp | sp | spl |
| 5 | rbp | ebp | bp | bpl |
| 6 | rsi | esi | si | sil |
| 7 | rdi | edi | di | dil |
| 8 | r8 | r8d | r8w | r8b |
| 9 | r9 | r9d | r9w | r9b |
| 10 | r10 | r10d | r10w | r10b |
| 11 | r11 | r11d | r11w | r11b |
| 12 | r12 | r12d | r12w | r12b |
| 13 | r13 | r13d | r13w | r13b |
| 14 | r14 | r14d | r14w | r14b |
| 15 | r15 | r15d | r15w | r15b |

（ah, ch, dh, bhについてはここでは割愛します。）

## 命令の構成

x86-64の命令は大まかには以下の部分から構成されます：

1. レガシーなプリフィックス（あれば）
2. REXプリフィックス（必要に応じて）
3. オペコード
4. レジスターやアドレスの指定（あれば）
5. 即値（あれば）

このうち一番重要なのがオペコードで、命令の種類を表します。オペコードのビット数は8の整数倍とは限らず、8ビット未満で指定される場合もあれば、ModR/Mと呼ばれる後続のバイトの一部も使用して指定される場合もあります。

## REXプリフィックス

プリフィックスの中で特に重要なのがREXプリフィックスです。REXプリフィックスについて詳しくはSDMのVolume 2の2.2.1を参照してください。ここでは簡単な説明に留めます。

REXプリフィックスはr8以降のレジスターにアクセスするのに使ったり、オペランドの幅を指定するのに使います。

その実態は上位4ビットが4=0b0100であるような1バイト、つまり十六進で40から4Fまでの16通りで、4ビットの情報を持ちます。4ビットの内訳は次のようになります：

```
0100 WRXB
     ^^^^
     |||+- ModR/Mのr/mフィールド、SIBのbaseフィールド、opcode regフィールドの拡張に使われる
     ||+-- SIB indexフィールドの拡張に使われる
     |+--- ModR/Mのregフィールドの拡張に使われる
     +---- オペランドの幅を指定する
```

REXのWビットは、オペコードの一部と考えることもできます。SDMでは、Wビットを立てるか否かで命令の欄を分けています。Wビットを立てたREXプリフィックスが必要な場合は、オペコードのところにREX.Wと書かれます。

C言語でREXプリフィックスの値を生成するマクロを書くならば、次のようになるでしょう：

```c
// r, x, b はそれぞれ0または1
#define REX(r,x,b) (0x40 | (((r) & 1) << 2) | (((x) & 1) << 1) | ((b) & 1))
#define REX_W(r,x,b) (0x48 | (((r) & 1) << 2) | (((x) & 1) << 1) | ((b) & 1))
```

## ModR/MバイトとSIBバイト

x86では命令のオペランドとしてレジスターの他にメモリーアドレスも指定できます。アドレスを取れる命令は、説明にr/mと書かれます。

指定できるアドレスは、レジスターに格納されたベースアドレス `base` と、そこからの相対位置 `displacement` （0でも良い）の和 `base + displacement` あるいは、さらにレジスターに格納されたインデックスの1,2,4,8倍を足したもの `base + scale * index + displacement` です。

r/mと指定されたオペランドにレジスターを指定するかアドレスを指定するかは、オペコードに続くModR/Mバイトによって指示します。アドレスを指定する場合は、ModR/Mバイトの後にさらにSIBバイトが続きます。

ModR/Mバイトの構造は次のようになります：

```
aabb bccc
\/\__/\_/
|   |  +-- r/m; レジスター番号の下位3ビットまたはアドレスモードの選択
|   +----- reg/opcode; レジスター番号の下位3ビットまたはオペコードの追加3ビット
+--------- mod; 種別
```

SIBバイトの構造は次のようになります。Scale, Index, Baseです。

```
ssii ibbb
\/\__/\_/
|   |  +-- base
|   +----- index
+--------- scale
```

C言語でModR/MバイトとSIBバイトの値を生成するマクロを書くならば、次のようになるでしょう：

```c
// 0 <= mod <= 3, 0 <= reg <= 7, 0 <= rm <= 7
#define ModRM(mod,reg,rm) ((((mod) & 3) << 6) | (((reg) & 7) << 3) | ((rm) & 7))

// 0 <= ss <= 3, 0 <= index <= 7, 0 <= base <= 7
#define SIB(ss,index,base) ((((ss) & 3) << 6) | (((index) & 7) << 3) | ((base) & 7))
```

以下でModR/Mの詳細に踏み込みますが、複雑なので、一旦後回しにして先に個別の命令の解説を読んで必要になったら戻ってくるのでも良いかもしれません。ModR/MバイトとSIBバイトの公式な説明は、SDM Volume 2の2.1.3あたりを参照してください。

まず、r/mにレジスターを指定する場合は、modビットに0b11を指定します。レジスターの番号の下位3ビットはModR/Mのr/mビットで指定し、上位1ビットはREXプリフィックスのBビットで指定します。SIBバイトは使用しません。

次に、r/mに `base + displacement` の形のアドレスを指定する場合です。

* displacementが0の場合：
    * baseがrsp, rbp, r12, r13以外の場合、mod=0b00, r/m=レジスター番号下位3ビットとします。レジスター番号の上位1ビットはREXプリフィックスのBビットで指定します。SIBバイトは使用しません。
    * baseがrspまたはr12の場合、mod=0b00, r/m=0b100として、SIBバイトを続けます。SIBバイトはindex=0b100, base=0b100とします（ssは任意）。r12の場合はREXプリフィックスのBビットを立てます。
    * baseがrbpまたはr13の場合は、「displacementが符号つき8ビットで表現可能な場合」を使ってエンコードします。
* displacementが符号つき8ビットで表現可能（-128以上127以下）な場合：
    * baseがrsp, r12以外の場合は、mod=0b01, r/m=レジスター番号下位3ビットとします。レジスター番号の上位1ビットはREXプリフィックスのBビットで指定します。SIBバイトは使用しません。displacementはModR/Mバイトの後の1バイトで指定します。
    * baseがrspまたはr12の場合、mod=0b01, r/m=0b100として、SIBバイトを続けます。SIBバイトはindex=0b100, base=0b100とします（ssは任意）。r12の場合はREXプリフィックスのBビットを立てます。displacementはSIBバイトの後の1バイトで指定します。
* displacementが符号つき32ビットで表現可能な場合：
    * baseがrsp, r12以外の場合は、mod=0b10, r/m=レジスター番号下位3ビットとします。レジスター番号の上位1ビットはREXプリフィックスのBビットで指定します。SIBバイトは使用しません。displacementはModR/Mバイトの後の4バイトで指定します。
    * baseがrspまたはr12の場合、mod=0b10, r/m=0b100として、SIBバイトを続けます。SIBバイトはindex=0b100, base=0b100とします（ssは任意）。r12の場合はREXプリフィックスのBビットを立てます。displacementはSIBバイトの後の4バイトで指定します。

最後に、r/mに `base + scale * index + displacement` の形のアドレスを指定する場合です。この場合は必ずSIBバイトが指定されます。indexとしてはrsp以外のレジスターを指定できます。

* SIBバイトのssはscaleに応じて、scale=1→ss=0b00, scale=2→ss=0b01, scale=4→ss=0b10, scale=8→ss=0b11とします。
* indexがrsp以外の場合は、SIBバイトのindexにレジスター番号の下位3ビットを格納します。レジスター番号の上位1ビットはREXプリフィックスのXビットを使って指定します。
* displacementが0の場合：
    * ModR/Mバイトはmod=0b00, r/m=0b100とします。
* displacementが符号つき8ビットで表現可能な場合：
    * ModR/Mバイトはmod=0b01, r/m=0b100とします。
    * displacementはSIBバイトの後の1バイトで指定します。
* displacementが符号つき32ビットで表現可能な場合：
    * ModR/Mバイトはmod=0b10, r/m=0b100とします。
    * displacementはSIBバイトの後の4バイトで指定します。

アドレスの形式として、rip相対アドレスを指定することもできます。基準となるripの値は現在の命令の次の命令で、それに符号つき32ビットのdisplacementを加えます。rip相対アドレスを使うには、ModR/Mバイトはmod=0b00, r/m=0b101とします。

## 命令の例

### RET命令

簡単な命令として、呼び出し元に制御を返すRET命令を見てみましょう。Intel SDMのVolume 2→Chapter 4 Instruction Set Reference, M-U→4.3 Instructions (M-U)の中から探してください。ここでも引用しておきます。

> RET -- Return From Procedure
> | Opcode\* | Instruction | Op/En | 64-Bit Mode | Compat/Leg Mode | Description |
> |-|-|-|-|-|-|
> | C3 | RET | ZO | Valid | Valid | Near return to calling procedure. |
> | CB | RET | ZO | Valid | Valid | Far return to calling procedure. |
> | C2 iw | RET imm16 | I | Valid | Valid | Near return to calling procedure and pop imm16 bytes from stack. |
> | CA iw | RET imm16 | I | Valid | Valid | Far return to calling procedure and pop imm16 bytes from stack. |

まず言えるのは、アセンブリー言語でRETという一つのニーモニックに、複数のオペコードが割り当てられている場合があるということです。

ここでは即値なしの素のRET命令を使いたいとしましょう。即値なしのRET命令にも2種類あり、nearとfarがあります。筆者も正直よくわかっていませんが、64ビット時代は全部nearだと思って良さそうです。

というわけで、RET命令の機械語は（十六進で）C3の1バイトだとわかりました。

### PUSH命令

PUSH命令を見てみます。PUSH命令は値をスタックに積むやつです。

> PUSH -- Push Word, Doubleword, or Quadword Onto the Stack
> | Opcode | Instruction | Op/En | 64-Bit Mode | Compat/Leg Mode | Description |
> |-|-|-|-|-|-|
> | FF /6 | PUSH r/m16 | M | Valid | Valid | Push r/m16. |
> | FF /6 | PUSH r/m32 | M | N.E. | Valid | Push r/m32. |
> | FF /6 | PUSH r/m64 | M | Valid | N.E. | Push r/m64. |
> | 50+rw | PUSH r16 | O | Valid | Valid | Push r16. |
> | 50+rd | PUSH r32 | O | N.E. | Valid | Push r32. |
> | 50+rd | PUSH r64 | O | Valid | N.E. | Push r64. |

PUSHというニーモニックを持つ命令は他にも種類がありますが、ここでは省略します。

x86の32ビットモードと64ビットモードで使用可能な命令の種類が異なります。N.E.はnot encodableの略で、PUSH r/m32とPUSH r32は64ビットモードでは使用できません。逆に、PUSH r/m64とPUSH r64は32ビットモードでは使用できません。

#### PUSH r64

まずは、PUSH r64のエンコード方法を確認してみましょう。Opcodeの欄には50+rdと書かれています。実はこの命令はオペコードが8ビット未満のケースに該当し、50（十六進）にレジスターの番号の下位3ビットを加えたものが命令の8ビットを構成します。

SDMのVolume 2→Appendix A Opcode Mapを見ると、確かに50から57までがPUSH命令に割り当てられていることがわかります。

レジスターの番号の上位1ビットはREXプリフィックスを使ってエンコードします。ここが0の場合はREXプリフィックスは省略可能です。

まとめると、PUSH r64は一般には次の2バイトを使ってエンコードされます：

```
REX:
0100 000*
        ^
        +- レジスターの番号の最上位ビット

opcode:
0101 0***
      ^^^
      ||+- レジスターの番号の最下位ビット
      |+-- レジスターの番号の下から2番目のビット
      +--- レジスターの番号の下から3番目のビット
```

エンコード例をいくつか挙げます：

* `push rax`: `40 50` または `50`
* `push rbp`: `40 55` または `55`
* `push r13`: `41 55`

#### PUSH r/m64

次に、PUSH r/m64のエンコード方法を確認してみましょう。Opcodeの欄にはFF /6と書かれています。FFはわかりやすくて、REXプリフィックスの後に（十六進）FFを続けるという意味です。ただ、FFだけだと命令が一種類には特定されない（CALL命令もFFから始まるものがあります）ので、ModR/Mのreg/opcodeフィールドも使って命令を識別します。そのreg/opcodeフィールドに入るのが6（二進表記で0b110）です。

まとめると、PUSH r/m64は次の3バイト以上を使ってエンコードされます：

```
REX:
0100 0*XB
      ^^^
      ||+- ModR/Mのr/mフィールド、SIBのbaseフィールドの拡張に使われる
      |+-- SIB indexフィールドの拡張に使われる
      +--- 任意

primary opcode byte:
1111 1111

ModR/M byte:
aa11 0ccc
\/    \_/
|      +-- r/m; レジスター番号の下位3ビットまたはアドレスモードの選択
+--------- mod; 種別
```

#### オペランドサイズ

表をよく見ると、PUSH r64の他にPUSH r16もオペコードが50+rwで、PUSH r/m64の他にPUSH r/m16もオペコードがFF /6となっています。これらはどうやって区別するのかというと、operand-size prefixと呼ばれる（十六進）66を命令の前につけることによって区別します。REX.Wが存在しない状況下で66がついているとオペランドサイズが16ビットになります。

### ADD命令

足し算を行うADD命令を見てみましょう。実物はバリエーションが多すぎるので、適当に抜粋してきます。

> ADD -- Add
> | Opcode | Instruction | Op/En | 64-Bit Mode | Compat/Leg Mode | Description |
> |-|-|-|-|-|-|
> | 81 /0 id | ADD r/m32, imm32 | MI | Valid | Valid | Add imm32 to r/m32. |
> | REX.W + 81 /0 id | ADD r/m64, imm32 | MI | Valid | N.E. | Add imm32 sign-extended to 64-bits to r/m64. |
> | 01 /r | ADD r/m32, r32 | MR | Valid | Valid | Add r32 to r/m32. |
> | REX.W + 01 /r | ADD r/m64, r64 | MR | Valid | N.E. | Add r64 to r/m64. |
> | 03 /r | ADD r32, r/m32 | RM | Valid | Valid | Add r/m32 to r32. |
> | REX.W + 03 /r | ADD r64, r/m64 | RM | Valid | N.E. | Add r/m64 to r64. |

表のOp/Enは下にあるInstruction Operand Encodingの項目に対応して、オペランドのエンコード方法を詳しく規定します。これも引用します。

> Instruction Operand Encoding
> | Op/En | Operand 1 | Operand 2 | Operand 3 | Operand 4 |
> |-|-|-|-|-|
> | RM | ModRM:reg (r, w) | ModRM:r/m (r) | N/A | N/A |
> | MR | ModRM:r/m (r, w) | ModRM:reg (r) | N/A | N/A |
> | MI | ModRM:r/m (r, w) | imm8/16/32 | N/A | N/A |
> | I | AL/AX/EAX/RAX | imm8/16/32 | N/A | N/A |

まあModR/Mのregとr/mのどっちにどっちを使うかという話ですね。オペランドに読み取り・書き込みをするかという情報も含まれるようです。Operand 1がr, wとなっているのでOperand 1とOperand 2を足した結果をOperand 1に書き込む、という動作がわかります。

r/mにはレジスターも指定することができるので、ADD r32, r32の形の命令はオペコードの異なる2通りのエンコード方法があるということになります。好きな方を選びましょう。

Opcodeの欄の/0はすでに説明しました。/rはModR/Mバイトのreg/opcodeにレジスター番号の下位3ビットを格納するという意味です（上位1ビットはREXプリフィックスのRビットを使います）。REX.WはWビットの立ったREXプリフィックスを使うという意味です。idは4バイト (dword) の即値 (immediate) が続くという意味です。

例を挙げると、`add <r64>, <imm32>` は次の7バイトでエンコードされます：

```
REX.W:
0100 100*
        ^
        +- レジスター番号の上位1ビット

primary opcode byte:
1000 0001

ModR/M byte:
1100 0***
      \_/
       +-- r/m; レジスター番号の下位3ビット

immediate (little-endian):
xxxx xxxx
xxxx xxxx
xxxx xxxx
xxxx xxxx
```

`add r13, 0xc0ffee` なら `49 81 c5 ee ff c0 00` という具合です。

### その他

表のOpcodeのところで使われている記法はVolume 2の3.1.1.1に説明があります。

## アセンブラーとIntel記法

もっと複雑な命令を扱う場合は、既存のアセンブラーを使って「答え合わせ」がしたいかもしれません。残念なことにx86のアセンブリー言語には流派があり、Intel SDMではIntel記法が、GNU系ツールではAT\&T記法が使われます。オペランドの順番とレジスター名につく `%` などの違いがあります。

GNUのアセンブラーでIntel記法を使いたい場合は、`.s` ファイルで `.intel_syntax noprefix` ディレクティブを使用します。

```
$ cat test.s
    .intel_syntax noprefix
    push rax
    push rbp
    push r13
    add r13, 0xc0ffee
    ret
$ as -o test.o test.s
```

オブジェクトファイルを逆アセンブルするには、`objdump -d` を使うと良いでしょう。これもデフォルトはAT\&T記法ですが、`-Mintel` オプションを使うとオーバーライドできます。

```
$ objdump -d -Mintel test.o

test.o:     file format elf64-x86-64


Disassembly of section .text:

0000000000000000 <.text>:
   0:   50                      push   rax
   1:   55                      push   rbp
   2:   41 55                   push   r13
   4:   49 81 c5 ee ff c0 00    add    r13,0xc0ffee
   b:   c3                      ret
```

GCC/ClangにIntel記法を使わせるには `-masm=intel` オプション、またはClangの場合は `--x86-asm-syntax=intel` オプションを使います。

GDBにIntel記法を使わせるには、`-ex 'set disassembly-flavor intel'` オプションを使うか、起動後に `set disassembly-flavor intel` コマンドを使います。

LLDBにIntel記法を使わせるには、`-O 'settings set target.x86-disassembly-flavor intel'` オプションを使うか、起動後に `settings set target.x86-disassembly-flavor intel` コマンドを打ち込みます。

## 実行時の機械語生成

人はなぜ機械語を勉強するのでしょうか。普通にプログラミングをしたいならアセンブラーを経由すれば機械語に触れなくて済みます。世の中には多くのコンパイラーがありますが、多くは機械語の生成をアセンブラーに委ねています。

答えの一つとしてありうるのは「実行時に機械語を生成したい・書き換えたいから」でしょう。実行時にちょっと機械語を生成したいときに、既存のアセンブラーに頼るのは大掛かりすぎると思うかもしれません。その場合は自前で機械語を勉強して書き込むことになります。

実行時に機械語を生成するような用途は、JITコンパイルが代表的です。そこまで本格的じゃなくても、GCCの関数内関数はアドレスを取れる関数を作るために少量の機械語を書いています。類似の例として、Windowsプログラミングでは第一引数に特定のポインターを代入するような関数を実行時に作るようなことを行う界隈が存在します（atlthunk）。

ここでは簡単な例として、与えられた整数に定数を加える関数を実行時に生成してみたいと思います。生成する関数は次のような感じです：

```c
int foo(int x)
{
    return x + <定数>;
}
```

ここではUnix系を仮定します。System V ABIによると、第一引数はediに割り当てられ、返り値はeaxで返却します。今回はレジスターで完結するのでスタックは触らなくてよくて、生成する命令列は次のようになります：

```
add edi, <定数>
mov eax, edi
ret
```

retは最初に説明した通り、C3でエンコードされます。movは説明していませんが、アセンブラーに食わせると89 f8が出てきました。

最近の環境では、通常の `malloc` で確保できるメモリーは実行可能になっていないと思います。実行時に機械語を生成するには、書き込み可能かつ実行可能なメモリー領域を用意する必要があります。ここでは `mmap` を使います。

なお、最近の厳しい実行環境ではW^X (write xor execute)と呼ばれる制限がかかっており、同一のメモリー領域を書き込み可能と同時に実行可能な状態にできない場合があります。そういう場合は最初に「書き込み可能、実行不可」で領域を確保して書き込みが終わったら `mprotect` で「書き込み不能、実行可能」に変えてやる必要があるかもしれません。ここでは厳しくない実行環境を想定して、RWX権限のついたメモリー領域を確保します。

書き込みが終わったら、GCCの組み込み関数 `__builtin___clear_cache` を呼び出します。アーキテクチャーによっては実行時にコード生成をする際にこういう特殊な関数を呼び出す必要があったりするのですが、実のところx86ではこういう操作は必須ではありません。しかし、コンパイラーの最適化を抑止する意味もあって、一応呼び出しておいた方が良いかもしれません。Windowsには同様の目的の関数として `FlushInstructionCache` があります。

```c
#include <assert.h>
#include <string.h>
#include <sys/mman.h> // mmap, mprotect, munmap
#include <unistd.h>
// typeofはC23の新機能で、関数ポインターを返す関数をわかりやすく書くことを可能にする
typeof(int (*)(int)) adder(int y)
{
    void *mem = mmap(NULL, getpagesize(), PROT_READ | PROT_WRITE | PROT_EXEC, MAP_ANON | MAP_PRIVATE, -1, 0);
    assert(mem != NULL);
    unsigned char *instr = mem;
    *instr++ = 0x81; *instr++ = 0xc7; // add edi, ...
    memcpy(instr, &y, 4); instr += 4; // <imm32>
    *instr++ = 0x89; *instr++ = 0xf8; // mov eax, edi
    *instr++ = 0xc3; // ret
    __builtin___clear_cache(mem, (void *)instr);
    return (int (*)(int))mem;
}
// 解放処理は省略
```

呼び出す例は次のようになります：

```c
#include <stdio.h>
int main(void)
{
    int (*f)(int) = adder(3);
    int (*g)(int) = adder(-7);
    int (*h)(int) = adder(42);
    printf("f(0) = %d\n", f(0));
    printf("f(-5) = %d\n", f(-5));
    printf("f(2) = %d\n", f(2));
    printf("g(0) = %d\n", g(0));
    printf("g(-5) = %d\n", g(-5));
    printf("g(2) = %d\n", g(2));
    printf("h(0) = %d\n", h(0));
    printf("h(-5) = %d\n", h(-5));
    printf("h(2) = %d\n", h(2));
}
```

暇な人は、`mmap` の呼び出しを `malloc` で置き換えるとどうなるか試してみましょう。

## 他のチュートリアル

ググって出てきた他のチュートリアルにもリンクを貼っておきます。この記事が分かりにくかった場合に読むと良いでしょう。

* [X86-64 Instruction Encoding - OSDev Wiki](https://wiki.osdev.org/X86-64_Instruction_Encoding)
* [x86_64 機械語入門](https://tanakamura.github.io/pllp/docs/x8664_language.html)

## 最後に

RISCなら、どこの馬の骨が書いたかもわからんこんな非公式解説を読まなくても公式のマニュアルを見ただけで機械語がわかります。RISC最高！この記事はM1 Macで書いています。

【追記】AArch64で機械語を触る話は以前ブログに書きました：

* [AArch64でJITしてみる](https://blog.miz-ar.info/2021/10/jit-on-aarch64/)
