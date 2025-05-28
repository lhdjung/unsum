# CLOSURE: complete listing of original samples of underlying raw evidence

Implements the novel CLOSURE technique for efficiently reconstructing all possible distributions of raw data from summary statistics. It is not about the Rust feature called closure.

You will likely only need `dfs_parallel()`.

Most of the code was written by Claude 3.5, translating Python code by Nathanael Larigaldie.

## Example

Enter summary data reported in a paper and call `dfs_parallel()`.

```
let mean = 5.0;
let sd = 2.78;
let n = 30;
let scale_min = 1;
let scale_max = 7;
let rounding_error_mean = 0.01;
let rounding_error_sd = 0.01;

dfs_parallel(
    mean,
    sd,
    n,
    scale_min,
    scale_max,
    rounding_error_mean,
    rounding_error_sd,
)
```

#### License

<sup>
Licensed under either of <a href="LICENSE-APACHE">Apache License, Version
2.0</a> or <a href="LICENSE-MIT">MIT license</a> at your option.
</sup>

<br>

<sub>
Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in this crate by you, as defined in the Apache-2.0 license, shall
be dual licensed as above, without any additional terms or conditions.
</sub>