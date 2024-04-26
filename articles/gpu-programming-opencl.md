---
title: "GPUプログラミングを試す：OpenCL編"
emoji: "👌"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: []
published: false
---

GPUプログラミングを行う技術（API）にはさまざまなものがあります。この記事を含めて数回に分けて、簡単な利用例を通してGPUを使うAPIを比較してみます。

題材とするのは「ベクトル（配列）の加算」です。C言語で書けば次のようなコードです。

```c
#include <stdio.h>

void vector_add(size_t n, const float a[n], const float b[n], float result[restrict n])
{
    for (size_t i = 0; i < n; ++i) {
        result[i] = a[i] + b[i];
    }
}

int main(void)
{
    const size_t n = 6;
    float a[6] = {1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 0x1p-126f};
    float b[6] = {2.0f, 7.0f, -4.0f, 7.2f, 9.5f, -0x0.c0ffeep-126f};
    float result[6];
    vector_add(n, a, b, result);
    for (size_t i = 0; i < n; ++i) {
        printf("result[%zu] = %g (%a)\n", i, result[i], result[i]);
    }
}
```

出力例：

```
result[0] = 3 (0x1.8p+1)
result[1] = 9 (0x1.2p+3)
result[2] = -1 (-0x1p+0)
result[3] = 11.2 (0x1.666666p+3)
result[4] = 14.5 (0x1.dp+3)
result[5] = 2.89283e-39 (0x1.f8009p-129)
```

題材としては少し簡単すぎるかもしれませんが、Hello world代わりならこんなもんだろうということで勘弁してください。

まずはOpenCLです。

## OpenCLの情報源

OpenCLの仕様は以下で参照できます：

* [Khronos OpenCL Registry - The Khronos Group Inc](https://registry.khronos.org/OpenCL/)

OpenCLには色々バージョンがあり、Appleの実装なんかは1.2で止まっていたりします。ただ、「OpenCL 3.0 Unified Specifications」には新しめのAPIに注釈が付いていたりするので、仕様を読む上ではUnified Specificationsを読んでおけば良さそうです。

## インストール

C言語でOpenCLの開発をするには、OpenCLのヘッダーとライブラリーが必要です。

Windowsの場合、

* [KhronosGroup/OpenCL-SDK: OpenCL SDK](https://github.com/KhronosGroup/OpenCL-SDK)
* [Intel® SDK for OpenCL™ Applications](https://www.intel.com/content/www/us/en/developer/articles/guide/sdk-for-opencl-2020-gsg.html)
* CUDA Toolkit

のいずれかを入れるとどこかに `<CL/cl.h>` と `OpenCL.lib` が入ります。ヘッダーとライブラリーは基本的に共通（対応バージョンの違いはあるかもしれませんが）なので、Intel SDKでビルドしてNVIDIAのGPUを使用することも可能なはずです。

Intel SDKを入れた場合やCUDAを入れた場合のプログラムのコンパイル手順は、MSVCの場合は次のようになるでしょう：

```
cl /nologo /I"%INTELOCLSDKROOT%\include" program.c /link /LIBPATH:"%INTELOCLSDKROOT%\lib\x64" OpenCL.lib
cl /nologo /I"%CUDA_PATH%\include" program.c /link /LIBPATH:"%CUDA_PATH%\lib\x64" OpenCL.lib
```

MSYS2の場合は `*-opencl-headers` と `*-opencl-icd` みたいなパッケージを入れて

```
cc -Wall -o program.exe program.c -lOpenCL
```

みたいな感じでコンパイルすれば良いでしょう。

Ubuntuの場合、`opencl-c-headers` と `ocl-icd-opencl-dev` パッケージを入れます。他のLinuxの場合は自分で調べてください。コンパイルするコマンドは

```
cc -Wall -o program program.c -lOpenCL
```

みたいになると思います。

macOSの場合、ヘッダーの名前が他と違って `<OpenCL/cl.h>` です。macOSを想定するプログラムの場合は `#include` 文を

```c
#if defined(__APPLE__)
#include <OpenCL/cl.h>
#else
#include <CL/cl.h>
#endif
```

という風に書くと良いでしょう。コンパイルに使うコマンドは

```
cc -Wall -o program program.c -framework OpenCL
```

となります。

## デバイスの列挙

OpenCLはCPU, GPUを含むさまざまなデバイスを想定しており、OpenCLの実装も一つのシステム上に複数存在する可能性があります。OpenCLの実装とそれに対応したデバイスを合わせてプラットフォームと呼びます。したがって、OpenCLのデバイスを列挙する際はプラットフォームを列挙→そのプラットフォームに対応しているデバイスを列挙、という形になります。

プラットフォームの列挙を行う関数は `clGetPlatformIDs` で、デバイスの列挙を行う関数は `clGetDeviceIDs` です。プラットフォームの情報は `clGetPlatformInfo` で、デバイスの情報は `clGetDeviceInfo` で取れます。

* [4.1. Querying Platform Info](https://registry.khronos.org/OpenCL/specs/3.0-unified/html/OpenCL_API.html#_querying_platform_info)
* [4.2. Querying Devices](https://registry.khronos.org/OpenCL/specs/3.0-unified/html/OpenCL_API.html#platform-querying-devices)

プラットフォームとデバイスを列挙するプログラムの例は次のようになります：

https://github.com/minoki/gpu-programming-test/blob/master/opencl/list.c

筆者の環境でのこのプログラムの実行結果をいくつか載せておきます。

Apple M1 Mac (macOS 13) での実行結果：

```
Platform #0:
  Profile: FULL_PROFILE
  Version: OpenCL 1.2 (Oct 28 2023 11:19:36)
  Name: Apple
  Vendor: Apple
  Extensions: cl_APPLE_SetMemObjectDestructor cl_APPLE_ContextLoggingFunctions cl_APPLE_clut cl_APPLE_query_kernel_names cl_APPLE_gl_sharing cl_khr_gl_event
  Device 0 (GPU):
    Name: Apple M1
    Vendor: Apple
    Driver version: 1.2 1.0
    Profile: FULL_PROFILE
    Device version: OpenCL 1.2 
    OpenCL C Version: OpenCL C 1.2 
    Extensions: cl_APPLE_SetMemObjectDestructor cl_APPLE_ContextLoggingFunctions cl_APPLE_clut cl_APPLE_query_kernel_names cl_APPLE_gl_sharing cl_khr_gl_event cl_khr_byte_addressable_store cl_khr_global_int32_base_atomics cl_khr_global_int32_extended_atomics cl_khr_local_int32_base_atomics cl_khr_local_int32_extended_atomics cl_khr_3d_image_writes cl_khr_image2d_from_buffer cl_khr_depth_images 
    Built-in kernels: 
    Max compute units: 8
    Preferred vector width (half): 0
    Preferred vector width (float): 1
    Preferred vector width (double): 1
```

Intel Mac (macOS 13) での実行結果：

```
Platform #0:
  Profile: FULL_PROFILE
  Version: OpenCL 1.2 (Oct 28 2023 11:19:20)
  Name: Apple
  Vendor: Apple
  Extensions: cl_APPLE_SetMemObjectDestructor cl_APPLE_ContextLoggingFunctions cl_APPLE_clut cl_APPLE_query_kernel_names cl_APPLE_gl_sharing cl_khr_gl_event
  Device 0 (CPU):
    Name: Intel(R) Core(TM) i5-1038NG7 CPU @ 2.00GHz
    Vendor: Intel
    Driver version: 1.1
    Profile: FULL_PROFILE
    Device version: OpenCL 1.2 
    OpenCL C Version: OpenCL C 1.2 
    Extensions: cl_APPLE_SetMemObjectDestructor cl_APPLE_ContextLoggingFunctions cl_APPLE_clut cl_APPLE_query_kernel_names cl_APPLE_gl_sharing cl_khr_gl_event cl_khr_fp64 cl_khr_global_int32_base_atomics cl_khr_global_int32_extended_atomics cl_khr_local_int32_base_atomics cl_khr_local_int32_extended_atomics cl_khr_byte_addressable_store cl_khr_int64_base_atomics cl_khr_int64_extended_atomics cl_khr_3d_image_writes cl_khr_image2d_from_buffer cl_APPLE_fp64_basic_ops cl_APPLE_fixed_alpha_channel_orders cl_APPLE_biased_fixed_point_image_formats cl_APPLE_command_queue_priority
    Built-in kernels: 
    Max compute units: 8
    Preferred vector width (half): 0
    Preferred vector width (float): 4
    Preferred vector width (double): 2
  Device 1 (GPU):
    Name: Intel(R) Iris(TM) Plus Graphics
    Vendor: Intel Inc.
    Driver version: 1.2(Feb 19 2024 20:09:20)
    Profile: FULL_PROFILE
    Device version: OpenCL 1.2 
    OpenCL C Version: OpenCL C 1.2 
    Extensions: cl_APPLE_SetMemObjectDestructor cl_APPLE_ContextLoggingFunctions cl_APPLE_clut cl_APPLE_query_kernel_names cl_APPLE_gl_sharing cl_khr_gl_event cl_khr_global_int32_base_atomics cl_khr_global_int32_extended_atomics cl_khr_local_int32_base_atomics cl_khr_local_int32_extended_atomics cl_khr_byte_addressable_store cl_khr_image2d_from_buffer cl_khr_gl_depth_images cl_khr_depth_images cl_khr_3d_image_writes 
    Built-in kernels: 
    Max compute units: 64
    Preferred vector width (half): 0
    Preferred vector width (float): 1
    Preferred vector width (double): 0
```

AMD Ryzen（GPU内蔵のやつ、Windows 11）での実行結果：

```
Platform #0:
  Profile: FULL_PROFILE
  Version: OpenCL 2.1 AMD-APP (3516.0)
  Name: AMD Accelerated Parallel Processing
  Vendor: Advanced Micro Devices, Inc.
  Extensions: cl_khr_icd cl_khr_d3d10_sharing cl_khr_d3d11_sharing cl_khr_dx9_media_sharing cl_amd_event_callback cl_amd_offline_devices
  Device 0 (GPU):
    Name: gfx1103
    Vendor: Advanced Micro Devices, Inc.
    Driver version: 3516.0 (PAL,LC)
    Profile: FULL_PROFILE
    Device version: OpenCL 2.0 AMD-APP (3516.0)
    OpenCL C Version: OpenCL C 2.0
    Extensions: cl_khr_fp64 cl_khr_global_int32_base_atomics cl_khr_global_int32_extended_atomics cl_khr_local_int32_base_atomics cl_khr_local_int32_extended_atomics cl_khr_int64_base_atomics cl_khr_int64_extended_atomics cl_khr_3d_image_writes cl_khr_byte_addressable_store cl_khr_fp16 cl_khr_gl_sharing cl_amd_device_attribute_query cl_amd_media_ops cl_amd_media_ops2 cl_khr_d3d10_sharing cl_khr_d3d11_sharing cl_khr_dx9_media_sharing cl_khr_image2d_from_buffer cl_khr_subgroups cl_khr_gl_event cl_khr_depth_images cl_khr_mipmap_image cl_khr_mipmap_image_writes cl_amd_liquid_flash cl_amd_copy_buffer_p2p cl_amd_planar_yuv
    Built-in kernels:
    Max compute units: 6
    Preferred vector width (half): 1
    Preferred vector width (float): 1
    Preferred vector width (double): 1
```

## 加算

OpenCLではデバイスで動かす関数のことをカーネル (kernel) と呼びます。カーネルはOpenCL Cという言語で書きます。足し算の場合はこんな感じになるでしょう：

```c
__kernel void add(int n, __global const float *a, __global const float *b, __global float *c)
{
    int i = get_global_id(0);
    if (i < n) {
        c[i] = a[i] + b[i];
    }
}
```

OpenCL Cで書いたプログラムは文字列で与えて実行時にコンパイルします。新しめのOpenCLではSPIR-Vという中間言語を与えることもできるようになっています。

* コンテキスト `cl_context`
* コマンドキュー `cl_command_queue`
* バッファー `cl_mem`
* プログラム `cl_program`
* カーネル `cl_kernel`
