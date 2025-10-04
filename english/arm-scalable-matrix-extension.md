---
title: Trying Out Arm's Scalable Matrix Extension with Apple M4 or QEMU
published: false
description: 
tags: arm, ABotWroteThis
# cover_image: https://direct_url_to_image.jpg
# Use a ratio of 100:42 for best results.
# published_at: 2025-10-04 08:11 +0000
---

*This is the English version of my Japanese article, [ArmのScalable Matrix Extension (SME)を試す](https://zenn.dev/mod_poppo/articles/arm-scalable-matrix-extension), translated with help of ChatGPT.*

Some of modern CPUs include instructions designed to accelerate matrix multiplication. Examples include Intel’s Advanced Matrix Extensions (AMX), Apple’s AMX, IBM Power’s Matrix-Multiply Assist (MMA), and the focus of this article—Arm’s **Scalable Matrix Extension (SME)**.

I first heard about SME around 2021, but my real interest began when I learned that it’s implemented in the Apple M4, announced in 2024. Naturally, I got my hands on a Mac powered by the M4. Even if you don’t have an actual Apple M4 device, don’t worry—you can still experiment with SME using QEMU. Let’s give it a try.

## Environment Setup and Vector Length

If you’re testing on an **M4 Mac**, simply install the latest Xcode. My setup is macOS 15.7.1, Xcode 26.0.1, and Apple Clang 17.0.0.

If you want to experiment using **QEMU**, the easiest way is to use *user-mode emulation on Linux*. The examples shown here were tested on Ubuntu 24.04 with GCC 14, Clang 20, and QEMU 8.2. Ubuntu itself is running on an x86_64 host.

```console
$ sudo apt install gcc-14-aarch64-linux-gnu clang-20 qemu-user
```

SME can be seen as an extension of **SVE**, so its vector length depends on the CPU. Programs using SME operate in either *streaming mode* or *non-streaming mode*, and the vector length may differ between the two.

We’ll denote the vector length (in bits) in streaming mode as {% katex inline %}\mathrm{SVL}{% endkatex %}, and define:

* {% katex inline %}\mathrm{SVL}_{\mathrm{B}}=\mathrm{SVL}/8{% endkatex %} (vector length in bytes)
* {% katex inline %}\mathrm{SVL}_{\mathrm{H}}=\mathrm{SVL}/16{% endkatex %} (vector length in halfwords, 16-bit)
* {% katex inline %}\mathrm{SVL}_{\mathrm{S}}=\mathrm{SVL}/32{% endkatex %} (vector length in 32-bit words)
* {% katex inline %}\mathrm{SVL}_{\mathrm{D}}=\mathrm{SVL}/64{% endkatex %} (vector length in 64-bit words)
* {% katex inline %}\mathrm{SVL}_{\mathrm{Q}}=\mathrm{SVL}/128{% endkatex %} (vector length in 128-bit quads)

As a “Hello world” example, let’s write a small program to check the vector length in each mode.

```c
#include <stdio.h>
#include <arm_sme.h>

__arm_locally_streaming
void streaming_fn(void)
{
    printf("Streaming mode: svcntw() = %u, svcntsw() = %u\n", (unsigned)svcntw(), (unsigned)svcntsw());
}

int main(void)
{
    printf("Has SME? %s\n", __arm_has_sme() ? "Yes" : "No");
#if defined(__ARM_FEATURE_SVE)
    printf("Non-streaming mode: svcntw() = %u, svcntsw() = %u\n", (unsigned)svcntw(), (unsigned)svcntsw());
#endif
    streaming_fn();
}
```

Functions marked with the `__arm_locally_streaming` attribute execute with streaming mode enabled. The `svcnt*` intrinsics are from SVE, and their return values differ depending on whether streaming mode is active. The `svcnts*` intrinsics are new in SME—they return the vector length specifically in streaming mode.

Note that the Apple M4 does *not* support non-streaming SVE, so calling `svcnt*` functions outside streaming mode will cause a SIGILL (illegal instruction).

Enough talk—let’s just compile and run it.

On Clang/macOS:

```console
$ clang -O2 -march=armv8-a+sme -o veclen veclen.c
$ ./veclen
Has SME? Yes
Streaming mode: svcntw() = 16, svcntsw() = 16
```

On GCC/Linux:

```console
$ aarch64-linux-gnu-gcc-14 -O2 -march=armv9-a+sme -static -o veclen veclen.c
$ qemu-aarch64 ./veclen
Has SME? Yes
Non-streaming mode: svcntw() = 16, svcntsw() = 8
Streaming mode: svcntw() = 8, svcntsw() = 8
```

On Clang/Linux:

```console
$ clang-20 -O2 --target=aarch64-linux-gnu -march=armv9-a+sme -static -o veclen veclen.c
$ qemu-aarch64 ./veclen
Has SME? Yes
Non-streaming mode: svcntw() = 16, svcntsw() = 8
Streaming mode: svcntw() = 8, svcntsw() = 8
```

Now let’s interpret the results. On the Apple M4, `svcntw()` returned 16, which means the streaming SVE vector appears to be 512 bits wide — so {% katex inline %}\mathrm{SVL}=512{% endkatex %}.

In QEMU, `svcntw()` in non-streaming mode also returned 16, indicating that the regular SVE vector length is 16 words (32-bit each), i.e., 512 bits. Meanwhile, `svcntsw()` returned 8, suggesting that the streaming SVE vector is 256 bits wide.

When statically linking with Clang while GCC 13 cross-compilers are installed, you may encounter a linker error like ``undefined reference to `__arm_sme_state'``. To fix this, either use dynamic linking and run the program with `qemu-aarch64 -L /usr/aarch64-linux-gnu/` or simply install GCC 14, as shown in the earlier steps.

On older versions of macOS or Xcode, a similar linker error about `___arm_sme_state` may appear. If that happens, try updating Xcode (and macOS if necessary).

In QEMU, you can change the vector length using the `-cpu` option. The non-streaming vector length is controlled by `sve-default-vector-length`, and the streaming vector length by `sme-default-vector-length` (For details, see the [Arm CPU Features — QEMU documentation](https://www.qemu.org/docs/master/system/arm/cpu-features.html)). The unit is bytes. Let’s experiment with these settings:

```console
$ qemu-aarch64 -cpu max,sve-default-vector-length=256,sme-default-vector-length=128 ./veclen
Has SME? Yes
Non-streaming mode: svcntw() = 64, svcntsw() = 32
Streaming mode: svcntw() = 32, svcntsw() = 32
```

Here, we specified a 256-byte (2048-bit) vector length for non-streaming mode and a 128-byte (1024-bit) vector length for streaming mode. As you can see, the output of the `svcnt*` functions changes accordingly.

## Instructions and Registers for Matrix Multiplication

At the core of SME are the **MOP (sum of outer products)** instructions, which compute and accumulate the *outer product* of vectors. The floating-point variant, which performs fused multiply–add operations, is called **FMOPA** (floating-point sum of outer products and accumulate).

The outer product of two vectors can be written as:

{% katex %}
\begin{pmatrix}
a_0\\\\a_1\\\\a_2
\end{pmatrix}
\begin{pmatrix}
b_0\\\\b_1\\\\b_2
\end{pmatrix}^T
=\begin{pmatrix}
a_0b_0 & a_0b_1 & a_0b_2 \\\\
a_1b_0 & a_1b_1 & a_1b_2 \\\\
a_2b_0 & a_2b_1 & a_2b_2
\end{pmatrix}
{% endkatex %}

By repeating and accumulating this operation, you effectively perform matrix multiplication. Each multiply–add operation is *fused*—the multiplication and addition are performed with a single rounding (known as FMA).

A large register area called **ZA** is introduced for this purpose. Its total size is {% katex inline %}\mathrm{SVL}\_{\mathrm{B}}\times\mathrm{SVL}\_{\mathrm{B}}{% endkatex %}. For example:

* if {% katex inline %}\mathrm{SVL}=128{% endkatex %}, then the ZA size is 256 bytes (2048 bits),
* if {% katex inline %}\mathrm{SVL}=512{% endkatex %}, then it’s 4096 bytes (32,768 bits).

The ZA area can be viewed as {% katex inline %}\mathrm{SVL}\_{\mathrm{B}}{% endkatex %} vectors, each {% katex inline %}\mathrm{SVL}\_{\mathrm{B}}{% endkatex %} bytes long. Each vector is named `ZA[N]` where {% katex inline %}0\leq N\leq \mathrm{SVL}\_{\mathrm{B}}-1{% endkatex %}. When referring to a vector with an element type, the notation is:

* `ZA.B[N]` for 8-bit elements,
* `ZA.H[N]` for 16-bit elements,
* `ZA.S[N]` for 32-bit elements, and so on.

The ZA region can also be partitioned into **tiles**. For 8-bit elements, you get a single {% katex inline %}\mathrm{SVL}\_{\mathrm{B}}\times\mathrm{SVL}\_{\mathrm{B}}{% endkatex %} tile called `ZA0.B`. For 32-bit elements, you get four {% katex inline %}\mathrm{SVL}\_{\mathrm{S}}\times\mathrm{SVL}\_{\mathrm{S}}{% endkatex %} tiles, named `ZA0.S`, `ZA1.S`, `ZA2.S`, and `ZA3.S`.

Elements of a tile can be accessed as *row vectors* or *column vectors*, called **slices**. Row vectors (horizontal slices) are prefixed with `H`, and column vectors (vertical slices) with `V`. For example, you can refer to `ZA0H.S[i]` for a row vector or `ZA2V.S[j]` for a column vector.

## Implementing Matrix Multiplication

Let’s implement matrix multiplication {% katex inline %}A\times B=C{% endkatex %}. We’ll use 32-bit floating-point numbers as elements, and assume that the matrices are stored in memory using C-style multidimensional array order (row-major).

First, here’s a basic reference implementation. We won’t worry about performance for now — while reordering the loops can improve speed, that’s beyond the scope of this section.

We’ll compute the product of moderately sized matrices and print the top-left 10×10 submatrix of the result.

```c
#include <stdio.h>

void matmul_naive(size_t l, size_t m, size_t n, const float A[l][m], const float B[m][n], float C[restrict l][n])
{
    for (size_t i = 0; i < l; ++i) {
        for (size_t k = 0; k < n; ++k) {
            C[i][k] = 0.0f;
            for (size_t j = 0; j < m; ++j) {
                C[i][k] += A[i][j] * B[j][k];
            }
        }
    }
}

int main(void)
{
    float A[100][200];
    float B[200][150];
    float C[100][150];
    for (size_t i = 0; i < 100; ++i) {
        for (size_t j = 0; j < 200; ++j) {
            A[i][j] = (float)i + (float)j;
        }
    }
    for (size_t i = 0; i < 200; ++i) {
        for (size_t j = 0; j < 150; ++j) {
            B[i][j] = (float)i - (float)j;
        }
    }
    matmul_naive(100, 200, 150, A, B, C);
    for (size_t i = 0; i < 10; ++i) {
        for (size_t j = 0; j < 10; ++j) {
            printf("%g ", C[i][j]);
        }
        puts("");
    }
}
```

Example of compiling and running with Clang:

```console
$ clang -O2 -o matmul_naive matmul_naive.c
$ ./matmul_naive 
2.6467e+06 2.6268e+06 2.6069e+06 2.587e+06 2.5671e+06 2.5472e+06 2.5273e+06 2.5074e+06 2.4875e+06 2.4676e+06 
2.6666e+06 2.6465e+06 2.6264e+06 2.6063e+06 2.5862e+06 2.5661e+06 2.546e+06 2.5259e+06 2.5058e+06 2.4857e+06 
2.6865e+06 2.6662e+06 2.6459e+06 2.6256e+06 2.6053e+06 2.585e+06 2.5647e+06 2.5444e+06 2.5241e+06 2.5038e+06 
2.7064e+06 2.6859e+06 2.6654e+06 2.6449e+06 2.6244e+06 2.6039e+06 2.5834e+06 2.5629e+06 2.5424e+06 2.5219e+06 
2.7263e+06 2.7056e+06 2.6849e+06 2.6642e+06 2.6435e+06 2.6228e+06 2.6021e+06 2.5814e+06 2.5607e+06 2.54e+06 
2.7462e+06 2.7253e+06 2.7044e+06 2.6835e+06 2.6626e+06 2.6417e+06 2.6208e+06 2.5999e+06 2.579e+06 2.5581e+06 
2.7661e+06 2.745e+06 2.7239e+06 2.7028e+06 2.6817e+06 2.6606e+06 2.6395e+06 2.6184e+06 2.5973e+06 2.5762e+06 
2.786e+06 2.7647e+06 2.7434e+06 2.7221e+06 2.7008e+06 2.6795e+06 2.6582e+06 2.6369e+06 2.6156e+06 2.5943e+06 
2.8059e+06 2.7844e+06 2.7629e+06 2.7414e+06 2.7199e+06 2.6984e+06 2.6769e+06 2.6554e+06 2.6339e+06 2.6124e+06 
2.8258e+06 2.8041e+06 2.7824e+06 2.7607e+06 2.739e+06 2.7173e+06 2.6956e+06 2.6739e+06 2.6522e+06 2.6305e+06 
```

Example of compiling and running with GCC:

```console
$ aarch64-linux-gnu-gcc-14 -O2 -march=armv9-a+sme -static -o matmul_naive matmul_naive.c
$ qemu-aarch64 ./matmul_naive
2.6467e+06 2.6268e+06 2.6069e+06 2.587e+06 2.5671e+06 2.5472e+06 2.5273e+06 2.5074e+06 2.4875e+06 2.4676e+06
2.6666e+06 2.6465e+06 2.6264e+06 2.6063e+06 2.5862e+06 2.5661e+06 2.546e+06 2.5259e+06 2.5058e+06 2.4857e+06
2.6865e+06 2.6662e+06 2.6459e+06 2.6256e+06 2.6053e+06 2.585e+06 2.5647e+06 2.5444e+06 2.5241e+06 2.5038e+06
2.7064e+06 2.6859e+06 2.6654e+06 2.6449e+06 2.6244e+06 2.6039e+06 2.5834e+06 2.5629e+06 2.5424e+06 2.5219e+06
2.7263e+06 2.7056e+06 2.6849e+06 2.6642e+06 2.6435e+06 2.6228e+06 2.6021e+06 2.5814e+06 2.5607e+06 2.54e+06
2.7462e+06 2.7253e+06 2.7044e+06 2.6835e+06 2.6626e+06 2.6417e+06 2.6208e+06 2.5999e+06 2.579e+06 2.5581e+06
2.7661e+06 2.745e+06 2.7239e+06 2.7028e+06 2.6817e+06 2.6606e+06 2.6395e+06 2.6184e+06 2.5973e+06 2.5762e+06
2.786e+06 2.7647e+06 2.7434e+06 2.7221e+06 2.7008e+06 2.6795e+06 2.6582e+06 2.6369e+06 2.6156e+06 2.5943e+06
2.8059e+06 2.7844e+06 2.7629e+06 2.7414e+06 2.7199e+06 2.6984e+06 2.6769e+06 2.6554e+06 2.6339e+06 2.6124e+06
2.8258e+06 2.8041e+06 2.7824e+06 2.7607e+06 2.739e+06 2.7173e+06 2.6956e+06 2.6739e+06 2.6522e+06 2.6305e+06
```

Using SME for matrix multiplication, it makes sense to multiply several rows of {% katex inline %}A{% endkatex %} by several columns of {% katex inline %}B{% endkatex %} at once and store the results into a block of {% katex inline %}C{% endkatex %}. Let the number of rows and columns processed at once be up to {% katex inline %}\mathrm{SVL}\_{\mathrm{S}}{% endkatex %} each.

![](/images/arm-sme-1.jpg)

How should we implement the inner loop (the loop over {% katex inline %}j{% endkatex %} that computes {% katex inline %}\sum_{j} a_{ij}\cdot b_{jk}{% endkatex %})? One possible approach is to process one column / one row at a time.

![](/images/arm-sme-2.jpg)

Reading rows of {% katex inline %}B{% endkatex %} is fine because they are contiguous in memory and can be loaded easily, but reading columns of {% katex inline %}A{% endkatex %} requires loading from non-contiguous memory locations and therefore needs gathers. SVE provides gather support, but it isn’t necessarily available in Streaming SVE Mode (it depends on whether the `FEAT_SME_FA64` extension is implemented).

In this write-up we instead block the {% katex inline %}j{% endkatex %} loop as well, so we can load multiple columns of {% katex inline %}A{% endkatex %} in a single, bulk operation. You don’t actually need to transpose the matrices in memory — you can “load rows into tiles” and then extract column vectors from those tiles, which works nicely.

![](/images/arm-sme-3.jpg)

First, we declare the function. Since we’re using SME, we specify `__arm_locally_streaming`, and we also add an attribute indicating that the function uses ZA. Because the ZA state is not inherited from the caller, we specify `__arm_new("za")`.

```c
__arm_locally_streaming
__arm_new("za")
void matmul_sme(size_t l, size_t m, size_t n, const float A[l][m], const float B[m][n], float C[restrict l][n])
{
    // ...
}
```

Next, we write the loops over {% katex inline %}i{% endkatex %} and {% katex inline %}k{% endkatex %}, and prepare the masks. This part is similar to standard SVE programming.

```c
    for (size_t i = 0; i < l; i += svcntw()) {
        svbool_t maskA = svwhilelt_b32_s64(i, l);
        for (size_t k = 0; k < n; k += svcntw()) {
            svbool_t maskB = svwhilelt_b32_s64(k, n);
            // ...
        }
    }
```

With 32-bit elements, we can use four tiles. Here, we use tile 0 as the accumulator and tile 1 to hold the submatrix of {% katex inline %}A{% endkatex %}. In SME, tiles are not regular variables; instead, they are specified by passing an *integer constant* to the intrinsic functions.

First, we implement clearing the tiles and loading the submatrix of {% katex inline %}A{% endkatex %}.

```c
            constexpr uint64_t TILE_ACC = 0;
            constexpr uint64_t TILE_A = 1;
            svzero_za(); // Clear all tiles to zero
            size_t limit_ii = min(l, i + svcntw());
            for (size_t j = 0; j < m; j += svcntw()) {
                svbool_t maskTA = svwhilelt_b32_s64(j, m);
                svzero_mask_za(17 << TILE_A); // Clear TILE_A (note the unusual way of specifying it)
                for (size_t ii = i; ii < limit_ii; ++ii) {
                    svld1_hor_za32(TILE_A, ii, maskTA, &A[ii][j]);
                }
                // ... Perform outer product computation and accumulation here ...
            }
            // ... Write the computed block back to the C submatrix ...
```

`svld1_hor_za32` is an intrinsic that loads column vectors from memory into a ZA tile. The slice indices are automatically calculated modulo the tile size, so you don’t need expressions like `ii - i`.

Next, we compute the outer products and accumulate:

```c
                size_t limit_jj = min(m, j + svcntw());
                for (size_t jj = j; jj < limit_jj; ++jj) {
                    svfloat32_t a;
                    a = svread_ver_za32_f32_m(a, maskA, TILE_A, jj); // Load a column vector from the tile
                    svfloat32_t b = svld1_f32(maskB, &B[jj][k]);
                    svmopa_za32_f32_m(TILE_ACC, maskA, maskB, a, b); // Compute outer product and accumulate into the tile
                }
```

`svread_ver_za32_f32_m` reads a vector from a ZA tile. Strangely, it requires a vector as an argument—probably to supply values for elements masked out by the predicate. Again, slice indices are automatically modded, so `jj - j` is unnecessary.

Finally, write the accumulated tile back to the corresponding submatrix of {% katex inline %}C{% endkatex %}:

```c
for (size_t ii = i; ii < limit_ii; ++ii) {
    svst1_hor_za32(TILE_ACC, ii, maskB, &C[ii][k]);
}
```

Slice indices are automatically modded, so there’s no need for `ii - i`.

The complete source code looks like this:

```c
#include <stdio.h>
#include <stdint.h>
#include <arm_sme.h>

void matmul_naive(size_t l, size_t m, size_t n, const float A[l][m], const float B[m][n], float C[restrict l][n])
{
    for (size_t i = 0; i < l; ++i) {
        for (size_t k = 0; k < n; ++k) {
            C[i][k] = 0.0f;
            for (size_t j = 0; j < m; ++j) {
                C[i][k] += A[i][j] * B[j][k];
            }
        }
    }
}

// To ensure that functions called from Streaming Mode are inlined, we need to specify __arm_streaming_compatible
__attribute__((always_inline))
static inline size_t min(size_t x, size_t y) __arm_streaming_compatible
{
    return x > y ? y : x;
}

__arm_locally_streaming
__arm_new("za")
void matmul_sme(size_t l, size_t m, size_t n, const float A[l][m], const float B[m][n], float C[restrict l][n])
{
    for (size_t i = 0; i < l; i += svcntw()) {
        svbool_t maskA = svwhilelt_b32_s64(i, l);
        for (size_t k = 0; k < n; k += svcntw()) {
            svbool_t maskB = svwhilelt_b32_s64(k, n);
            constexpr uint64_t TILE_ACC = 0;
            constexpr uint64_t TILE_A = 1;
            svzero_za();
            size_t limit_ii = min(l, i + svcntw());
            for (size_t j = 0; j < m; j += svcntw()) {
                svbool_t maskTA = svwhilelt_b32_s64(j, m);
                svzero_mask_za(17 << TILE_A);
                for (size_t ii = i; ii < limit_ii; ++ii) {
                    svld1_hor_za32(TILE_A, ii, maskTA, &A[ii][j]);
                }
                size_t limit_jj = min(m, j + svcntw());
                for (size_t jj = j; jj < limit_jj; ++jj) {
                    svfloat32_t a;
                    a = svread_ver_za32_f32_m(a, maskA, TILE_A, jj);
                    svfloat32_t b = svld1_f32(maskB, &B[jj][k]);
                    svmopa_za32_f32_m(TILE_ACC, maskA, maskB, a, b);
                }
            }
            for (size_t ii = i; ii < limit_ii; ++ii) {
                svst1_hor_za32(TILE_ACC, ii, maskB, &C[ii][k]);
            }
        }
    }
}

int main()
{
    float A[100][200];
    float B[200][150];
    float C_naive[100][150], C_sme[100][150];
    for (size_t i = 0; i < 100; ++i) {
        for (size_t j = 0; j < 200; ++j) {
            A[i][j] = (float)i + (float)j;
        }
    }
    for (size_t i = 0; i < 200; ++i) {
        for (size_t j = 0; j < 150; ++j) {
            B[i][j] = (float)i - (float)j;
        }
    }
    matmul_naive(100, 200, 150, A, B, C_naive);
    for (size_t i = 0; i < 10; ++i) {
        for (size_t j = 0; j < 10; ++j) {
            printf("%g ", C_naive[i][j]);
        }
        puts("");
    }
    puts("---");
    matmul_sme(100, 200, 150, A, B, C_sme);
    for (size_t i = 0; i < 10; ++i) {
        for (size_t j = 0; j < 10; ++j) {
            printf("%g ", C_sme[i][j]);
        }
        puts("");
    }
    bool equal = true;
    for (size_t i = 0; i < 10; ++i) {
        for (size_t j = 0; j < 10; ++j) {
            equal = equal && C_naive[i][j] == C_sme[i][j];
            if (!equal) {
                break;
            }
        }
        if (!equal) {
            break;
        }
    }
    puts(equal ? "Equal" : "Not equal");
}
```

Let’s compile and run it on an M4 Mac:

```console
$ clang -O2 -march=armv8-a+sme -std=c23 -o matmul_sme matmul_sme.c
$ ./matmul_sme
2.6467e+06 2.6268e+06 2.6069e+06 2.587e+06 2.5671e+06 2.5472e+06 2.5273e+06 2.5074e+06 2.4875e+06 2.4676e+06 
2.6666e+06 2.6465e+06 2.6264e+06 2.6063e+06 2.5862e+06 2.5661e+06 2.546e+06 2.5259e+06 2.5058e+06 2.4857e+06 
2.6865e+06 2.6662e+06 2.6459e+06 2.6256e+06 2.6053e+06 2.585e+06 2.5647e+06 2.5444e+06 2.5241e+06 2.5038e+06 
2.7064e+06 2.6859e+06 2.6654e+06 2.6449e+06 2.6244e+06 2.6039e+06 2.5834e+06 2.5629e+06 2.5424e+06 2.5219e+06 
2.7263e+06 2.7056e+06 2.6849e+06 2.6642e+06 2.6435e+06 2.6228e+06 2.6021e+06 2.5814e+06 2.5607e+06 2.54e+06 
2.7462e+06 2.7253e+06 2.7044e+06 2.6835e+06 2.6626e+06 2.6417e+06 2.6208e+06 2.5999e+06 2.579e+06 2.5581e+06 
2.7661e+06 2.745e+06 2.7239e+06 2.7028e+06 2.6817e+06 2.6606e+06 2.6395e+06 2.6184e+06 2.5973e+06 2.5762e+06 
2.786e+06 2.7647e+06 2.7434e+06 2.7221e+06 2.7008e+06 2.6795e+06 2.6582e+06 2.6369e+06 2.6156e+06 2.5943e+06 
2.8059e+06 2.7844e+06 2.7629e+06 2.7414e+06 2.7199e+06 2.6984e+06 2.6769e+06 2.6554e+06 2.6339e+06 2.6124e+06 
2.8258e+06 2.8041e+06 2.7824e+06 2.7607e+06 2.739e+06 2.7173e+06 2.6956e+06 2.6739e+06 2.6522e+06 2.6305e+06 
---
2.6467e+06 2.6268e+06 2.6069e+06 2.587e+06 2.5671e+06 2.5472e+06 2.5273e+06 2.5074e+06 2.4875e+06 2.4676e+06 
2.6666e+06 2.6465e+06 2.6264e+06 2.6063e+06 2.5862e+06 2.5661e+06 2.546e+06 2.5259e+06 2.5058e+06 2.4857e+06 
2.6865e+06 2.6662e+06 2.6459e+06 2.6256e+06 2.6053e+06 2.585e+06 2.5647e+06 2.5444e+06 2.5241e+06 2.5038e+06 
2.7064e+06 2.6859e+06 2.6654e+06 2.6449e+06 2.6244e+06 2.6039e+06 2.5834e+06 2.5629e+06 2.5424e+06 2.5219e+06 
2.7263e+06 2.7056e+06 2.6849e+06 2.6642e+06 2.6435e+06 2.6228e+06 2.6021e+06 2.5814e+06 2.5607e+06 2.54e+06 
2.7462e+06 2.7253e+06 2.7044e+06 2.6835e+06 2.6626e+06 2.6417e+06 2.6208e+06 2.5999e+06 2.579e+06 2.5581e+06 
2.7661e+06 2.745e+06 2.7239e+06 2.7028e+06 2.6817e+06 2.6606e+06 2.6395e+06 2.6184e+06 2.5973e+06 2.5762e+06 
2.786e+06 2.7647e+06 2.7434e+06 2.7221e+06 2.7008e+06 2.6795e+06 2.6582e+06 2.6369e+06 2.6156e+06 2.5943e+06 
2.8059e+06 2.7844e+06 2.7629e+06 2.7414e+06 2.7199e+06 2.6984e+06 2.6769e+06 2.6554e+06 2.6339e+06 2.6124e+06 
2.8258e+06 2.8041e+06 2.7824e+06 2.7607e+06 2.739e+06 2.7173e+06 2.6956e+06 2.6739e+06 2.6522e+06 2.6305e+06 
Equal
```

Let’s compile it with GCC on Linux and run it using QEMU:

```console
$ aarch64-linux-gnu-gcc-14 -O2 -march=armv9-a+sme -std=c23 -static -o matmul_sme matmul_sme.c
$ qemu-aarch64 ./matmul_sme
2.6467e+06 2.6268e+06 2.6069e+06 2.587e+06 2.5671e+06 2.5472e+06 2.5273e+06 2.5074e+06 2.4875e+06 2.4676e+06 
2.6666e+06 2.6465e+06 2.6264e+06 2.6063e+06 2.5862e+06 2.5661e+06 2.546e+06 2.5259e+06 2.5058e+06 2.4857e+06 
2.6865e+06 2.6662e+06 2.6459e+06 2.6256e+06 2.6053e+06 2.585e+06 2.5647e+06 2.5444e+06 2.5241e+06 2.5038e+06 
2.7064e+06 2.6859e+06 2.6654e+06 2.6449e+06 2.6244e+06 2.6039e+06 2.5834e+06 2.5629e+06 2.5424e+06 2.5219e+06 
2.7263e+06 2.7056e+06 2.6849e+06 2.6642e+06 2.6435e+06 2.6228e+06 2.6021e+06 2.5814e+06 2.5607e+06 2.54e+06 
2.7462e+06 2.7253e+06 2.7044e+06 2.6835e+06 2.6626e+06 2.6417e+06 2.6208e+06 2.5999e+06 2.579e+06 2.5581e+06 
2.7661e+06 2.745e+06 2.7239e+06 2.7028e+06 2.6817e+06 2.6606e+06 2.6395e+06 2.6184e+06 2.5973e+06 2.5762e+06 
2.786e+06 2.7647e+06 2.7434e+06 2.7221e+06 2.7008e+06 2.6795e+06 2.6582e+06 2.6369e+06 2.6156e+06 2.5943e+06 
2.8059e+06 2.7844e+06 2.7629e+06 2.7414e+06 2.7199e+06 2.6984e+06 2.6769e+06 2.6554e+06 2.6339e+06 2.6124e+06 
2.8258e+06 2.8041e+06 2.7824e+06 2.7607e+06 2.739e+06 2.7173e+06 2.6956e+06 2.6739e+06 2.6522e+06 2.6305e+06 
---
2.6467e+06 2.6268e+06 2.6069e+06 2.587e+06 2.5671e+06 2.5472e+06 2.5273e+06 2.5074e+06 2.4875e+06 2.4676e+06 
2.6666e+06 2.6465e+06 2.6264e+06 2.6063e+06 2.5862e+06 2.5661e+06 2.546e+06 2.5259e+06 2.5058e+06 2.4857e+06 
2.6865e+06 2.6662e+06 2.6459e+06 2.6256e+06 2.6053e+06 2.585e+06 2.5647e+06 2.5444e+06 2.5241e+06 2.5038e+06 
2.7064e+06 2.6859e+06 2.6654e+06 2.6449e+06 2.6244e+06 2.6039e+06 2.5834e+06 2.5629e+06 2.5424e+06 2.5219e+06 
2.7263e+06 2.7056e+06 2.6849e+06 2.6642e+06 2.6435e+06 2.6228e+06 2.6021e+06 2.5814e+06 2.5607e+06 2.54e+06 
2.7462e+06 2.7253e+06 2.7044e+06 2.6835e+06 2.6626e+06 2.6417e+06 2.6208e+06 2.5999e+06 2.579e+06 2.5581e+06 
2.7661e+06 2.745e+06 2.7239e+06 2.7028e+06 2.6817e+06 2.6606e+06 2.6395e+06 2.6184e+06 2.5973e+06 2.5762e+06 
2.786e+06 2.7647e+06 2.7434e+06 2.7221e+06 2.7008e+06 2.6795e+06 2.6582e+06 2.6369e+06 2.6156e+06 2.5943e+06 
2.8059e+06 2.7844e+06 2.7629e+06 2.7414e+06 2.7199e+06 2.6984e+06 2.6769e+06 2.6554e+06 2.6339e+06 2.6124e+06 
2.8258e+06 2.8041e+06 2.7824e+06 2.7607e+06 2.739e+06 2.7173e+06 2.6956e+06 2.6739e+06 2.6522e+06 2.6305e+06 
Equal
```

It looks like the results match the loop-based reference implementation — great!

Of course, you can also compile with Clang:

```console
$ clang-20 -O2 --target=aarch64-linux-gnu -march=armv9-a+sme -std=c23 -static -o matmul_sme matmul_sme.c
$ qemu-aarch64 ./matmul_sme
... output omitted ...
```

## Impressions

I tried implementing matrix multiplication using SME.

It was already known that Apple CPUs had dedicated matrix multiplication instructions (AMX), but it’s nice to see them implemented in the SME form, which makes them easier to use.

Compared to other platforms, Intel and IBM server CPUs are hard for individuals to access (though some households might have them). Apple CPUs, on the other hand, are readily available to anyone. In other words, with a Mac equipped with an *Apple M4*, individual users can program SME on a personal computer. As an *instruction-set enthusiast*—someone who cares more about instruction sets than overall CPU performance—this is very exciting.

As a disclaimer: I am not an expert on matrix multiplication, and there may be mistakes in the code presented here. Please keep that in mind if you use this article as a reference.

## References

* [Arm Architecture Reference Manual for A-profile architecture](https://developer.arm.com/documentation/ddi0487/ka/?lang=en)
* [The Scalable Matrix Extension (SME), for Armv9-A](https://developer.arm.com/documentation/ddi0616/ba/?lang=en)
    * Note: This has now been merged into the Architecture Reference Manual and is considered RETIRED.
* ACLE: [Arm C Language Extensions](https://arm-software.github.io/acle/main/)
* LLVM: [Support for AArch64 Scalable Matrix Extension in LLVM — LLVM 22.0.0git documentation](https://llvm.org/docs/AArch64SME.html)
* Clang: [Attributes in Clang — Clang 22.0.0git documentation](https://clang.llvm.org/docs/AttributeReference.html#aarch64-sme-attributes)
* QEMU: [Arm CPU Features — QEMU documentation](https://www.qemu.org/docs/master/system/arm/cpu-features.html)
