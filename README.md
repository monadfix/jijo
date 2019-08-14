# jijo: Bidirectional JSON serialization

## Design Goals

* Explicitness – decouple types and encoders/decoders (unlike autoderived
  instances in Aeson).

* Bidirectionality – use the same definition for encoding and decoding to
  prevent mistakes when one side of the definition is updated and the other
  is not.

* Completeness – collect as many validation errors as possible, instead of
  stopping after the first error.

## Module Structure

**Core:**

* `JSON.Definition` – the core of the framework, includes combinators for
  defining complete JSON definitions, parsing primitives, objects, sums, and
  adding predicates to validate complex conditions.

* `JSON.Validation` – the validation machinery, complex enough to deserve
  its own module.

* `JSON.Path` – utilities for working with
  [JSONPath](https://goessner.net/articles/JsonPath/), which is used for
  error reporting.

**Records:**

* `RecordField.*` – helpers for generating record constructors that make it
  harder to mix up fields when decoding records from JSON.

## An Example

```
$ stack repl
```

Encoding:

```haskell
> uuid <- Data.UUID.V4.nextRandom

> encodeViaDefinition jUUID uuid
String "c7d63bec-517b-48d8-b77a-bc44d05f24af"
```

Decoding, happy path:

```haskell
> import Data.Aeson

> validateViaDefinition jUUID (String "c7d63bec-517b-48d8-b77a-bc44d05f24af")
Right c7d63bec-517b-48d8-b77a-bc44d05f24af
```

Decoding, type mismatch:

```haskell
> validateViaDefinition jUUID (Number 42)
Left [(JPath [],JTypeNotOneOf (fromList [JTyString]))]
```

Decoding, malformed UUID:

```haskell
> validateViaDefinition jUUID (String "invalid")
Left [(JPath [],JValidationFail InvalidUUID)]
```

The errors are returned as a flattened list of JSONPaths and
`JValidationError`s. They can include custom "business logic" errors.

## Implementation Details

### `JValidation`

`JValidation` is defined as follows:

```haskell
newtype JValidation e a = JValidation
  { runJValidation
      :: JPathBuilder
      -> ErrorList e
      -> (Maybe a, ErrorList e) }
```

The `ErrorList e -> (..., ErrorList e)` bit is writer monad, but with CPS
applied to avoid leaking.

`JValidationA` and `JValidationM` are isomorphic to `JValidation`, but are
defined using `Compose` and monad transformers respectively, which gives us
`Applicative` and `Monad` operations for free. Then we use `coerce` to get:

* an `Applicative` instance for `JValidation`
* `jValidationCompose` (aka `<=<` for `JValidation`)
* `jValidationLocal` (aka `local` for `JValidation`)

We don't want to have a `Monad` instance for `JValidation` because it would
violate the laws.

### `JDefinition`

`JDefinition` is a composition of a validator and an encoder:

```haskell
type JDefinition e = ArrPair (ValidationArr e) EncodingArr

data ArrPair p q j a = ArrPair (p j a) (q j a)

newtype ValidationArr e j a =
  ValidationArr (j -> JValidation e a)

newtype EncodingArr j a =
  EncodingArr (a -> j)
```

It has a `Category` instance that can be used for sequential/monadic
validation: any failed step of the pipeline aborts the pipeline. In most
cases, a `JDefinition` can be built by using the same recipe:

* narrow down the type using one of existing primitive combinators
* (`jString`, `jObject`, etc), parse (probably using `jObjectDefinition`),
* then add extra predicates using `jDefinition`.

### `JObjectDefinition`

`JObjectDefinition` is a `Product` of a validator and an encoder. It can be
converted into a `JDefinition`. It does not have a `Monad` instance but it
can be used for "parallel" applicative validation – all errors will be
reported in parallel.

### `makeRecBuilder`

`jField` uses explicit type applications so that fields would not be mixed
up; `makeRecBuilder` wraps constructors into something that takes explicitly
named fields.

## Future Work

* Use `-XDerivingVia` instead of `coerce` once GHC 8.10 is out (due to
  three-release policy).

* Better document how to use sum type validation. There are tests in
  `JSON.DefinitionSpec` but no docs yet.

* TODO: comment on `BadExponent`.

* Move the `Field` machinery into
  [`named`](http://hackage.haskell.org/package/named)?

* Use something like Barbies or
  [`higgledy`](https://github.com/i-am-tom/higgledy) to get rid of `Field`
  and move field names into types?
