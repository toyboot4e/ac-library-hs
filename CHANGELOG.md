# Revision history for acl-hs

## 1.2.6.0 -- April 2025

- Added `AtCoder.Extra.Math` functions
  - `isPrime`
  - `primes`
  - `primeFactors`
- Added `AtCoderExtra.Math.Montgomery64`
- Added `AtCoderExtra.ModInt64`

## 1.2.5.0 -- April 2025

- Added `AtCoder.Extra.Mo`
- Added `AtCoder.Extra.SqrtDecomposition`

## 1.2.4.0 -- April 2025

- Added `AtCoder.Dsu.mergeMaybe`
- Added `AtCoder.Extra.Graph` functions
  - `rev`
  - `connectedComponents`
  - `bipartiteVertexColors`
  - BFS, Dijkstra, Bellman–Ford, Floyd–Warshall
  - path reconstruction functions
- Added `AtCoder.Extra.Tree` functions
  - `diameter`, `diameterPath`
  - `mst`, `mstBy`
- Added `AtCoder.Internal.Queue.newDeque`

## 1.2.3.0 -- March 2025

- Added `AtCoder.Extra.SegTree2d` and `Extra.SegTree2d.Dense`.

## 1.2.2.1 -- March 2025

- Reduced build time with `ST` monad and `INLINEABLE` pragmas.

## 1.2.2.0 -- Feb 2025

- Added `AtCoder.Extra.KdTree` and `AtCoder.Extra.LazyKdTree`.
- Added `clear` function to the dynamic segment tree family.
- Fixed `AtCoder.Extra.Hld.new` for a tree with a single vertex.

## 1.2.1.0 -- Feb 2025

- Added dynamic segment tree family.
- Added `AtCoder.Extra.Seq.Map`.
- Fixed `AtCoder.Extra.Pool.size`.
- `Handle` is moved from `AtCoder.Extra.Seq` to `AtCoder.Extra.Pool`.

## 1.2.0.0 -- Feb 2025

- Added `AtCoder.Extra.Seq`.
- Tweaked `INLINE` settings for less compile time.
- Breaking changes:
  - `Matrix.diag` now does not take length parameter.
  - `AtCoder.Extra.Math.primitiveRoot` is renamed to `primitiveRoot32`.
  - `Internal.Convolution` functions now use `ST` instead of `PrimMonad`.
  - `SegAct` implementation for `AtCoder.Extra.Monoid.RangeAdd` over `Max` and `Min` were fixed.

## 1.1.1.0 -- Jan 2025

- Added `AtCoder.Extra.Tree.Lct`.
- Added `blockCut`, `blockCutComponents` in `AtCoder.Extra.Graph`.
- Added `popBack_` in `AtCoder.Internal.Buffer`.
- Added `square`, `rank`, `inv`, `invRaw`, `detMod`, `detMint` in `AtCoder.Extra.Matrix`.

## 1.1.0.0 -- Jan 2025

- Removed `RangeSetId` and `RangeAddId` from `AtCoder.Extra.Monoid`.
- Implemented `SegAct` for `RangeSet`, `RangeAdd` and `Max`, `Min`.
- Added `segActWithLength` to `SegAct`.
- Added `build1` to `AtCoder.Internal.Csr`.
- Added a bunch of extra modules.

## 1.0.0.0 -- Dec 2024

- First version.
- Added ACL-compatible modules.
- Added `AtCoder.Extra.Math` (binary exponentiation) and `AtCoder.Extra.Monoid` (`SegAct` instances).

