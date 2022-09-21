---
title: "自動ベクトル化とC言語のrestrict"
emoji: "🌟"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: [c言語]
published: true
---

# 単純なループ

与えられた2つの `float` の配列の要素ごとの和を計算する関数を考えます。

```c
#include <stddef.h>
void add(size_t n, float *result, const float *a, const float *b) {
    for (size_t i = 0; i < n; ++i) {
        result[i] = a[i] + b[i];
    }
}
```

C99の可変長配列 (VLA) を使うと、同等の関数を次のように書くこともできます。

```c
#include <stddef.h>
void add_vla(size_t n, float result[n], const float a[n], const float b[n]) {
    for (size_t i = 0; i < n; ++i) {
        result[i] = a[i] + b[i];
    }
}
```

# SIMD intrinsicsの使用

最近のCPUにはSIMD命令が載っています。これらのSIMD命令を使って先ほどのコードを書き直してみましょう。

Intel SSEの場合：

```c
#include <stddef.h>
#include <immintrin.h>
void add_sse(size_t n, float *result, const float *a, const float *b) {
    size_t i = 0;
    for (; i + 4 <= n; i += 4) {
        __m128 va = _mm_loadu_ps(&a[i]);
        __m128 vb = _mm_loadu_ps(&b[i]);
        __m128 vr = _mm_add_ps(va, vb);
        _mm_storeu_ps(&result[i], vr);
    }
#pragma clang loop vectorize(disable)
    for (; i < n; ++i) {
        result[i] = a[i] + b[i];
    }
}
```

Arm NEON/ASIMDの場合：

```c
#include <stddef.h>
#include <arm_neon.h>
void add_neon(size_t n, float *result, const float *a, const float *b) {
    size_t i = 0;
    for (; i + 4 <= n; i += 4) {
        float32x4_t va = vld1q_f32(&a[i]);
        float32x4_t vb = vld1q_f32(&b[i]);
        float32x4_t vr = vaddq_f32(va, vb);
        vst1q_f32(&result[i], vr);
    }
#pragma clang loop vectorize(disable)
    for (; i < n; ++i) {
        result[i] = a[i] + b[i];
    }
}
```

# コンパイラーの自動ベクトル化

最近のコンパイラーには自動ベクトル化という機能が備わっていて、最初のコードなら余裕でベクトル化してくれます（自動ベクトル化）。実際に生成されたコードを見てみましょう（生成コードを簡単にするためにloop unrollingは無効にしています）。

* [コード](https://godbolt.org/#g:!((g:!((g:!((h:codeEditor,i:(filename:'1',fontScale:14,fontUsePx:'0',j:1,lang:___c,selection:(endColumn:1,endLineNumber:21,positionColumn:1,positionLineNumber:21,selectionStartColumn:1,selectionStartLineNumber:21,startColumn:1,startLineNumber:21),source:'%23include+%3Cstddef.h%3E%0Avoid+add(size_t+n,+float+*result,+const+float+*a,+const+float+*b)+%7B%0A++++for+(size_t+i+%3D+0%3B+i+%3C+n%3B+%2B%2Bi)+%7B%0A++++++++result%5Bi%5D+%3D+a%5Bi%5D+%2B+b%5Bi%5D%3B%0A++++%7D%0A%7D%0A%23include+%3Cimmintrin.h%3E%0Avoid+add_sse(size_t+n,+float+*result,+const+float+*a,+const+float+*b)+%7B%0A++++size_t+i+%3D+0%3B%0A++++for+(%3B+i+%2B+4+%3C%3D+n%3B+i+%2B%3D+4)+%7B%0A++++++++__m128+va+%3D+_mm_loadu_ps(%26a%5Bi%5D)%3B%0A++++++++__m128+vb+%3D+_mm_loadu_ps(%26b%5Bi%5D)%3B%0A++++++++__m128+vr+%3D+_mm_add_ps(va,+vb)%3B%0A++++++++_mm_storeu_ps(%26result%5Bi%5D,+vr)%3B%0A++++%7D%0A%23pragma+clang+loop+vectorize(disable)%0A++++for+(%3B+i+%3C+n%3B+%2B%2Bi)+%7B%0A++++++++result%5Bi%5D+%3D+a%5Bi%5D+%2B+b%5Bi%5D%3B%0A++++%7D%0A%7D%0A'),l:'5',n:'0',o:'C+source+%231',t:'0')),k:50,l:'4',n:'0',o:'',s:0,t:'0'),(g:!((h:compiler,i:(compiler:cclang1500,filters:(b:'0',binary:'1',commentOnly:'0',demangle:'0',directives:'0',execute:'1',intel:'0',libraryCode:'0',trim:'1'),flagsViewOpen:'1',fontScale:14,fontUsePx:'0',j:1,lang:___c,libs:!(),options:'-O3+-fno-unroll-loops',selection:(endColumn:1,endLineNumber:1,positionColumn:1,positionLineNumber:1,selectionStartColumn:1,selectionStartLineNumber:1,startColumn:1,startLineNumber:1),source:1,tree:'1'),l:'5',n:'0',o:'x86-64+clang+15.0.0+(C,+Editor+%231,+Compiler+%231)',t:'0')),k:50,l:'4',n:'0',o:'',s:0,t:'0')),l:'2',n:'0',o:'',t:'0')),version:4)

同じようなコードが生成されるかと思いきや、自動ベクトル化させた方が生成コードが長くなっています。なぜでしょうか？

実は元々のコードと手動ベクトル化したものでは、関数の意味が微妙に変わっています。具体的には、一部がオーバーラップした配列を与えた際の挙動が異なります。

```c
#include <stdio.h>
int main(int argc, char *argv[]) {
    {
        float a[4] = {0.0f, 2.0f, 4.0f, 6.0f};
        float b[5] = {0.0f, -1.0f, -2.0f, -3.0f, -4.0f};
        add_p(4, b + 1, a, b);
        for (int i = 0; i < 5; ++i) {
            printf("%g\n", b[i]);
        }
    }
    puts("---");
    {
        float a[4] = {0.0f, 2.0f, 4.0f, 6.0f};
        float b[5] = {0.0f, -1.0f, -2.0f, -3.0f, -4.0f};
        add_neon(4, b + 1, a, b); // or add_sse
        for (int i = 0; i < 5; ++i) {
            printf("%g\n", b[i]);
        }
    }
}
```

出力：

```
0
0
2
6
12
---
0
0
1
2
3
```

コンパイラーは最適化の際にコードの意味を変えてしまうといけないため、自動ベクトル化させた方は配列のオーバーラップの検査が入っているようです。

# restrictキーワードの使用

入出力の配列がオーバーラップすることはないとコンパイラーに伝えることができれば、自動ベクトル化の際により簡潔なコードを出力できるようになります。C言語にはそのためのキーワード、 `restrict` があります。ポインターの場合は `restrict` は `*` の後に書きます。

```c
#include <stddef.h>
void add_r(size_t n, float * restrict result, const float *a, const float *b) {
    for (size_t i = 0; i < n; ++i) {
        result[i] = a[i] + b[i];
    }
}
```

配列の場合は、やや奇妙な文法ですが、 `[]` の中、要素数の前に書きます。

```c
#include <stddef.h>
void add_r_vla(size_t n, float result[restrict n], const float a[n], const float b[n]) {
    for (size_t i = 0; i < n; ++i) {
        result[i] = a[i] + b[i];
    }
}
```

生成されたコードを見ると、 `restrict` によって生成コードが短くなっていることがわかります：

* [コード](https://godbolt.org/#z:OYLghAFBqd5QCxAYwPYBMCmBRdBLAF1QCcAaPECAMzwBtMA7AQwFtMQByARg9KtQYEAysib0QXACx8BBAKoBnTAAUAHpwAMvAFYTStJg1AB9U8lJL6yAngGVG6AMKpaAVxYMQANlIOAMngMmABy7gBGmMQSGqQADqgKhLYMzm4e3nEJSQIBQaEsEVFcMZaY1slCBEzEBKnunj6l5QKV1QS5IeGR0RZVNXXpjX3tgZ0F3cUAlBaorsTI7BwApABMAMyByG5YANRLa44KBOhYVAB0CPvYSxoAggBuqHjoO0wnEIkAXpjGBDsMpB2VFoqCYfwAVDtiJgFK5aARAWgGEcgSCwTtwUxEQIUcDQRCwpM9gB2ABCN1uOypQJIOw%2BeG%2Bvx2eD2awAIjsNPtSczWY5/ty9itycK8ESlmSKdTpVCYXCCEsAKykvBKjn7DlMJUqtVCnlhbWqxVs7lS6kSk13C0Ux7PV4nYzEemMv4A1H4jGyo7EPDWL3y7HIv549GYwO4tEE8WSu7S/jEOlfH5/FkazmC1MHAVrHmrEUq6Pk2MyqnQ2Hww26tNa5VG9XCnYG2tq03FqnWq3Ey23VYbBhbVy7faOPAsFiBAg%2BhgXK42p4vN7oYwKJTO5P/QEhiFlgM7JERj1hvc44ORjGEklFynUpNMzMcrk5s1U%2BN0jN6naSPlphjvvNpyRC2fGVTBYLgVgADh2e4mFZDljDHYw0XQVxjFiBQIFWLwax1Y1Jlba8S1A8CoPuMI4J2BCWCQ0EULQjCsKbXC2Xwp82xAhCSOghM0yo4xF3oiAYMBMjWKvEsqT4o4SEwVD0MwlYvB3Ctm2NETiDE4COx7dZYmIJhgBYWCtkMYAdhBVBYmgsoiB9b4IHwBQmDCeh8PY19MJzXlh2zXNhTzMVL2A6VlIVVT1XZV5K2ND8mLrAjpW07SOGmWhOEVXhPA4LRSFQTh%2BQUWZ5kwIU1h4UgCE0FLpgAaxARUYjSjhJEyqrcs4XgFBAGJKuylLSDgWAkDQFhYjoSJyEoEaxvoKJkBMowuAamIaHhSIuogMI2rCQJqgAT04cqRrYQQAHkGFoA6%2BtILAjKMcRrvwaFynuGE2swVQylcAhFnKidMCanLaDwMJ9OIPbnCwNrJ1HQ7%2BuBAyFAANTwTAAHdTtiRg4ZkQQRDEdgpFx%2BQlDUNrdC4fRTJAUxjHMYGwi6yBpksmwcU4ABaU61h2DmqAYVAOdcBhiBcWgOYs9DOoBmzknsBgnBceo9H8UZ8kKPR4kSNmUiV9JKa17IGA6dWJgsGXmgYVp%2Bj1zxKaaHXrZGPIuiKXo2gGO33ZqE3XYkaZCrmBY9EnTBfv6pqMtILKcryjhVAgrwOa8L8FrMpazg0TO6UcQFcEIWle0pnZnFG8aePWLhJl4XqtEmWr6sazgWujtq4867qKqq%2Bv9E4FZWuu9uu76nvXuIRI7EkIA%3D%3D%3D)

`restrict` キーワードにはC++にはありませんが、各種コンパイラーが独自拡張として似たようなキーワードを提供している場合があるようです。

# 余談：`__builtin_assume_aligned` の使用

IntelのSSE/AVX系には、ポインターが16バイト（SSEの場合）／32バイト（AVXの場合）／64バイト（AVX-512の場合）にアラインされている場合に使えるロード・ストア命令（movaps）があります。元々のコードはポインターのアラインメントに仮定を置いていないためコンパイラーはベクトル化の際にそれらを使うことができませんが、コンパイラーにアラインメントの仮定を教えてやるとベクトル化の際にmovapsが使われるようになります。

GCC/Clangの場合は `__builtin_assume_aligned` を使うことでポインターのアラインメントをコンパイラーに教えてやることができます。

```c
#include <stddef.h>
void add_aligned(size_t n, float * restrict result, const float *a, const float *b) {
    result = __builtin_assume_aligned(result, 128);
    a = __builtin_assume_aligned(a, 128);
    b = __builtin_assume_aligned(b, 128);
    for (size_t i = 0; i < n; ++i) {
        result[i] = a[i] + b[i];
    }
}
```

配列に対しても使えます。

```c
#include <stddef.h>
void add_aligned_vla(size_t n, float result[restrict n], const float a[n], const float b[n]) {
    result = __builtin_assume_aligned(result, 128);
    a = __builtin_assume_aligned(a, 128);
    b = __builtin_assume_aligned(b, 128);
    for (size_t i = 0; i < n; ++i) {
        result[i] = a[i] + b[i];
    }
}
```

C++20には標準化された機能として `std::assume_aligned` があるようです。
