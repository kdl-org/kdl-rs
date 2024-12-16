# `kdl` Release Changelog

<a name="6.0.0-alpha.5"></a>
## 6.0.0-alpha.5 (2024-12-16)

This version can now parse the "final" draft of 2.0.0, which is not quite
released, but should represent what the final version will look like. Still,
there might be last-minute changes based on major issues discovered in the next
few days.

### Features

* **fmt:** Configurable autoformat with `FormatConfig` (#95) ([014c7c57](https://github.com/kdl-org/kdl-rs/commit/014c7c57a2089dae78cd9136ab9bbdf8aa3f049f))
* **errors:** error collection/recovery and reporting improvements (#94) ([f31750a0](https://github.com/kdl-org/kdl-rs/commit/f31750a048e95c6ea6fe6df1820c463bae8fae81))
* **parser:** Update parser to 2.0.0 (#100) ([a1919a0e](https://github.com/kdl-org/kdl-rs/commit/a1919a0e36fcf056ff0415561b8bac7c62ac3037))

<a name="6.0.0-alpha.4"></a>
## 6.0.0-alpha.4 (2024-12-04)

### Features

* **draft:** 2.0.0-draft.6 support (#92) ([4d4ef759](https://github.com/kdl-org/kdl-rs/commit/4d4ef7595479b1dd75a59b52f31f8e5417daaf67))

<a name="v6.0.0-alpha.3"></a>
## 6.0.0-alpha.3 (2024-12-03)

### Features

* **parser:** update to 2.0.0-draft.5 (#90) ([2802578b](https://github.com/kdl-org/kdl-rs/commit/2802578ba07e669236e5d346433f191f090d22d0))

### Refactor

* **parser:** refactor number parsing to support generic number types ([fb7c92b5](https://github.com/kdl-org/kdl-rs/commit/fb7c92b59f9570f460ae3008ec43975739fb35db))

<a name="v6.0.0-alpha.2"></a>
## 6.0.0-alpha.2 (2024-11-30)

### Bug Fixes

* **deps:** remove nom ([d013ece7](https://github.com/kdl-org/kdl-rs/commit/d013ece76a7053828d325a592e6a7ee8ff2218b1))
* **fmt:** fix formatting error in CI (#88) ([11e11926](https://github.com/kdl-org/kdl-rs/commit/11e119263d78260f090d6f5c0223e465be6692d2))

### Documentation

* **license:** fix format of license file (#87) ([cc2995d7](https://github.com/kdl-org/kdl-rs/commit/cc2995d72be85f2b96be9e958fda7f969ebbe7ee))

### Features

* **value,entry:** i128 for integers, autoformat safe KdlEntryFormat (#91) ([05959558](https://github.com/kdl-org/kdl-rs/commit/059595585079ed4b9f9f47042b7b2238ecb29151))

### Refactor

* **parser:** refactor try_parse to be lift more weight ([a6419952](https://github.com/kdl-org/kdl-rs/commit/a641995293ffa93c7958f4f5462bb2aa5c8a2c45))

<a name="v6.0.0-alpha.1"></a>
## 6.0.0-alpha.1 (2024-10-06)

### Bug Fixes

* **api:** Fix `insert` and `remove` behavior for `KdlNode` ([be0dc170](https://github.com/kdl-org/kdl-rs/commit/be0dc1708d93b496815f8850e211f697fcb7244c))

### Features

* **parser:** v2 parser (#70) ([c9898cdb](https://github.com/kdl-org/kdl-rs/commit/c9898cdbd28f61eef482fb2703fe85552dc58e4c))

<a name="v5.0.0-alpha.1"></a>
## 5.0.0-alpha.1 (2023-04-01)

### Features

* **deps:** bump miette and thiserror ([db0a395f](https://github.com/kdl-org/kdl-rs/commit/db0a395fb4ecb4b35d67d281903a4ed89b9e9161))

<a name="v5.0.0-alpha.0"></a>
## 5.0.0-alpha.0 (2022-12-05)

### Features

* **api:** Improve .get()/.entry() APIs to be more consistent with Rust conventions ([3d8778a6](https://github.com/kdl-org/kdl-rs/commit/3d8778a610e65720ed5cf25bc612aada93349119))
    * **BREAKING CHANGE**: KdlNode::get* are now KdlNode::entry*. Additionally, KdlDocument::get_dash_vals has been renamed to KdlDocument::get_dash_args.
* **spans:** rework the span API to be strictly by-val (#60) ([04471a53](https://github.com/kdl-org/kdl-rs/commit/04471a537ecf97867e7dc8ee987cce0caba61982))
    * **BREAKING CHANGE**: this removes all the `.span_mut()` methods and changes the signature for `.span()`.
* **kql:** implement KQL query engine (#61) ([6d1a516e](https://github.com/kdl-org/kdl-rs/commit/6d1a516eb92415f99f7a5170ac61ce3252d6a4b5))

<a name="6.0.0-alpha.3"></a>
## 6.0.0-alpha.3 (2024-12-03)

### Features

* **parser:** update to 2.0.0-draft.5 (#90) ([2802578b](https://github.com/kdl-org/kdl-rs/commit/2802578ba07e669236e5d346433f191f090d22d0))

### Refactor

* **parser:** refactor number parsing to support generic number types ([fb7c92b5](https://github.com/kdl-org/kdl-rs/commit/fb7c92b59f9570f460ae3008ec43975739fb35db))

<a name="6.0.0-alpha.2"></a>
## 6.0.0-alpha.2 (2024-11-30)

### Features

* **value,entry:** i128 for integers, autoformat safe KdlEntryFormat (#91) ([05959558](https://github.com/kdl-org/kdl-rs/commit/059595585079ed4b9f9f47042b7b2238ecb29151))

### Bug Fixes

* **deps:** remove nom ([d013ece7](https://github.com/kdl-org/kdl-rs/commit/d013ece76a7053828d325a592e6a7ee8ff2218b1))
* **fmt:** fix formatting error in CI (#88) ([11e11926](https://github.com/kdl-org/kdl-rs/commit/11e119263d78260f090d6f5c0223e465be6692d2))

### Documentation

* **license:** fix format of license file (#87) ([cc2995d7](https://github.com/kdl-org/kdl-rs/commit/cc2995d72be85f2b96be9e958fda7f969ebbe7ee))

### Refactor

* **parser:** refactor try_parse to be lift more weight ([a6419952](https://github.com/kdl-org/kdl-rs/commit/a641995293ffa93c7958f4f5462bb2aa5c8a2c45))

<a name="6.0.0-alpha.1"></a>
## 6.0.0-alpha.1 (2024-10-06)

The long-awaited KDL v2 parser is upon us! This is a major rewrite of kdl-rs to make it comply with the [KDL v2 spec](https://github.com/kdl-org/kdl/pull/286)).

It also comes with some shiny new features and some significant API changes:

* The parser can now handle multiple errors per parse operation, continuing the parse and moving on to the next error when something doesn't go quite right.
* Value types have been simplified. You'll need to rely on `KdlEntry.value_repr` to change how a value is actually rendered.

There's a big caveat with this release: There's some known bugs around autoformatting when comments or `;` terminators are present, and the error reporting on the parser is _pretty bad_ right now (but will be fixed by release time).

This alpha is meant as an initial preview for folks who want to get an actual feel for KDL v2, and to make additional comments on the language before it's finalized.

### Features

* **parser:** v2 parser (#70) ([c9898cdb](https://github.com/kdl-org/kdl-rs/commit/c9898cdbd28f61eef482fb2703fe85552dc58e4c))
### Bug Fixes

* **api:** Fix `insert` and `remove` behavior for `KdlNode` ([be0dc170](https://github.com/kdl-org/kdl-rs/commit/be0dc1708d93b496815f8850e211f697fcb7244c))

<a name="5.0.0-alpha.1"></a>
## 5.0.0-alpha.1 (2023-04-01)

This release upgrades miette and thiserror in lockstep, bringing in syn2, in
order to avoid [potential issues with the
upgrade](https://github.com/yaahc/eyre/pull/92).

### Features

* **deps:** bump miette and thiserror ([db0a395f](https://github.com/kdl-org/kdl-rs/commit/db0a395fb4ecb4b35d67d281903a4ed89b9e9161))

<a name="5.0.0-alpha.0"></a>
## 5.0.0-alpha.0 (2022-12-05)

This release makes some breaking API changes, but most notably, introduces
support for the [KDL Query
Language](https://github.com/kdl-org/kdl/blob/main/QUERY-SPEC.md).

### Features

* **kql:** implement KQL query engine (#61) ([6d1a516e](https://github.com/kdl-org/kdl-rs/commit/6d1a516eb92415f99f7a5170ac61ce3252d6a4b5))
* **api:** Improve .get()/.entry() APIs to be more consistent with Rust conventions ([3d8778a6](https://github.com/kdl-org/kdl-rs/commit/3d8778a610e65720ed5cf25bc612aada93349119))
    * **BREAKING CHANGE**:
* **spans:** rework the span API to be strictly by-val (#60) ([04471a53](https://github.com/kdl-org/kdl-rs/commit/04471a537ecf97867e7dc8ee987cce0caba61982))
    * **BREAKING CHANGE**: this removes all the `.span_mut()` methods and changes the signature for `.span()`.

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
