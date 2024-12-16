# `kdl` Release Changelog

<a name="4.7.1"></a>
## 4.7.1 (2024-12-16)

The `miette` bump turned out to be a breaking change, so this release undoes it.
With the imminent release of `6.0.0` and the upcoming support for v1 parsing in
that version of the crate, it is unlikely this `kdl-rs` release line will
receive a `miette` update.

### Bug Fixes

* **deps:** Back out "feat(deps): bump miette to 7.4.0" ([337e6ec3](https://github.com/kdl-org/kdl-rs/commit/337e6ec339d1661abb7e91d82f0ac3d03fe6f061))

<a name="v4.7.0"></a>
## 4.7.0 (2024-12-03)

### Features

* **deps:** bump miette to 7.4.0 ([b5fa2e1c](https://github.com/kdl-org/kdl-rs/commit/b5fa2e1c70e522b6929559b9382b34368d2f41da))

<a name="4.7.0"></a>
## 4.7.0 (2024-12-03)

This is a quick maintenance release to bump the miette version for folks still
using kdl v1.

### Features

* **deps:** bump miette to 7.4.0 ([b5fa2e1c](https://github.com/kdl-org/kdl-rs/commit/b5fa2e1c70e522b6929559b9382b34368d2f41da))

<a name="4.6.0"></a>
## 4.6.0 (2022-10-09)

### Features

* **errors:** Add better diagnostics for errant plain identifiers in nodes (#59) ([3ddbfec8](https://github.com/kdl-org/kdl-rs/commit/3ddbfec80ec18bc97d9df4004ad262dcdcf79e9b))

<a name="4.5.0"></a>
## 4.5.0 (2022-08-31)

### Features

* **spans:** add spans to most elements (#57) ([b17ef8e2](https://github.com/kdl-org/kdl-rs/commit/b17ef8e2c61b67cdc632f1772e18f6c7521dcfd8))

<a name="4.4.0"></a>
## 4.4.0 (2022-08-18)

### Features

* **deps:** bump miette ([8d0f36ce](https://github.com/kdl-org/kdl-rs/commit/8d0f36ceb1c5c1243bae3247b6c86bfa45083f19))

### Bug Fixes

* **formatting:** Fix formatting when decoration is not present (#56) ([2e9c0447](https://github.com/kdl-org/kdl-rs/commit/2e9c0447f9420e37d5fe46d2a42ec7b9f0646d90))

<a name="4.3.0"></a>
## 4.3.0 (2022-06-11)

### Features

* **fmt:** Add clear_fmt_recursive method (#45) ([cd2d6e42](https://github.com/kdl-org/kdl-rs/commit/cd2d6e42b19b801a43e78256dca1d856367349f4))

<a name="4.2.0"></a>
## 4.2.0 (2022-05-11)

### Features

* **entry:** Add accessors to entry type. (#43) ([afccf012](https://github.com/kdl-org/kdl-rs/commit/afccf012168dcab1de89f3737014ee8ee037785b))

<a name="4.1.1"></a>
## 4.1.1 (2022-04-28)

### Bug Fixes

* **compliance:** pull in spec test suite and fix issues (#40) ([58a40fdf](https://github.com/kdl-org/kdl-rs/commit/58a40fdf487b303f7466c93d84a4cd8a5665aa24))

<a name="v4.1.0"></a>
## 4.1.0 (2022-04-24)

### Features

* **fmt:** shiny new comment-preserving formatter! (#38) ([12d373a1](https://github.com/kdl-org/kdl-rs/commit/12d373a1e0de6533e7722e3ecc69e7ddc0e59db9))

<a name="v4.0.0"></a>
## 4.0.0 (2022-04-23)

Hello again!

kdl-rs 4.0.0 is a _complete_ rewrite, featuring a full-fledged
"document-oriented" parser: that is, formatting, whitespace, comments, etc,
are all preserved and can be programmatically manipulated. KDL documents are
fully round-trippable, without losing any of that human-written content!

This crate will, for the time being, not include a serde-based parser, but
there's also crates like [`knuffel`](https://crates.io/crates/knuffel) and
[`kaydle`](https://crates.io/crates/kaydle) now that do probide serde (or
serde-like) functionality. You should definitely check those out if you're
looking for that kind of workflow!

Please give this version a whirl if you've been curious about using KDL for
your own projects, and let me know what can be improved, or even what you love
about it!

### Features

* **api:** complete rewrite into document-oriented parser (#29) ([364ea617](https://github.com/kdl-org/kdl-rs/commit/364ea6173c0bcfc2f5e4b21e19120179f6a5c5ed))
    * **BREAKING CHANGE**: Completely new API and bumped MSRV to 1.56.0.
* **tests:** add test for kdl-schema.kdl (#30) ([ad34cfd9](https://github.com/kdl-org/kdl-rs/commit/ad34cfd93a9e6d8018b8086821a3463b764fb363))
* **types:** add type annotation support (#31) ([16c82f1e](https://github.com/kdl-org/kdl-rs/commit/16c82f1ec18c221b0d98dfcfb805ed3642354f5b))
* **errors:** improve parsing errors and fix some bugs (#33) ([8ed6a5cd](https://github.com/kdl-org/kdl-rs/commit/8ed6a5cd068e60de03a0e14493383f2515b98f81))
* **clear_fmt:** add methods to clear formatting and reset it to default ([892bf06e](https://github.com/kdl-org/kdl-rs/commit/892bf06e69c746ea9711fe33979f28f937329672))
* **errors:** overhauled error reporting a ton ([d63f336d](https://github.com/kdl-org/kdl-rs/commit/d63f336d188eb15a4bd8c870e7ee37617923270a))
* **len:** add APIs to calculate component lengths (#36) ([177c42ca](https://github.com/kdl-org/kdl-rs/commit/177c42cae75d8a0d9985c26ea28cb4f1cf7077de))

### Bug Fixes

* **parse:** small parser tweaks + more tests ([1a8eb351](https://github.com/kdl-org/kdl-rs/commit/1a8eb351685dc368c55d992d719e6bad34398df2))
* **api:** remove obsolete type ([40b04418](https://github.com/kdl-org/kdl-rs/commit/40b04418c9dc9a8363c000e19bc22e54c0dae7e9))

<a name="3.0.0"></a>
## 3.0.0 (2021-09-16)

### Features

* **spec:** update parser to handle KDL 1.0.0 ([f811c5c8](https://github.com/kdl-org/kdl-rs/commit/f811c5c89c18cb02cc3e7bdd8c872ea42308ae3e))
    * **BREAKING CHANGE**: Various things have changed in the process of moving to KDL 1.0.0. Please test your stuff

<a name="2.0.0"></a>
## 2.0.0 (2021-09-16)

### Features

* **license:** change license to Apache-2.0 ([0dbf75c7](https://github.com/kdl-org/kdl-rs/commit/0dbf75c78eb918b6966aae27fb1d7591791f15de))
    * **BREAKING CHANGE**: This is a significant licensing change. Please review.

<a name="1.1.0"></a>
## 1.1.0 (2021-05-08)

It's been a while! This release brings kdl-rs much closer in sync with the
actual spec.

#### Bug Fixes

* **deps:**  Remove nom dependency on `bitvec` and `lexical` (#14) ([9bc5363b](https://github.com/kdl/kdl-rs/commit/9bc5363bb5b8e4ae39e250f2facbfcdf4557f11b))
* **numbers:**  Fix parsing of non-integer and non-decimal numbers (#13) ([c1b7c25c](https://github.com/kdl/kdl-rs/commit/c1b7c25c0095ac2bd8acf06f6834c734a42b4470))

#### Features

* **display:**  implemented Display for KdlNode (#6) ([b8c8b527](https://github.com/kdl/kdl-rs/commit/b8c8b52748747d80215ee0c3dea73e260e133af2))
* **docs:**  Add documentation for the entire crate (#16) ([94190697](https://github.com/kdl/kdl-rs/commit/94190697d8ad676f9b879dcc90f8eb03266c3ef8))
* **identifier:**  much larger character set for identifiers (not just alphanumeric), to match spec more closely (#7) ([95a1ee3e](https://github.com/kdl/kdl-rs/commit/95a1ee3e57156507c3bf8a8035017d4836e49a01))



<a name="1.0.0"></a>
## 1.0.0 (2020-12-19)

Initial Release! �
